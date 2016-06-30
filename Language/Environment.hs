{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances,TypeFamilies #-}
module Language.Environment where
import Language.Commons
import Language.Query
-- -- import Language.Element
-- -- import Language.Pose
-- import Language.Filter
import Language.Expression
-- import Language.Transition
import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Text.PrettyPrint
import Text.Printf
import Data.List
import System.Random
import Debug.Trace



data Action = EmptyAction
            | Create Object Address
            | Patch Object Address
            | Delete Object Address
            | Local PrimCommand
            deriving (Show)
            
data Event  = Click
            | Hover
            deriving (Eq,Show)
            

none :: Action
none = EmptyAction

click :: Event
click = Click

command :: Command Rel -> Action
command com = let com' = execState com EmptyCommand in Local com'

class Renderable a where
    directives      :: a -> InterpM [Doc]
    template        :: a -> InterpM Doc
    controller      :: a -> InterpM Doc
    references      :: a -> InterpM [Name]
    identifiers     :: a -> InterpM [Name]
    check           :: a -> InterpM Bool
    width           :: a -> Int
    classes         :: a -> [String]
    modules         :: a -> [String]
    toElementList   :: a -> [Element]
    schema          :: a -> InterpM Doc
    classes _ = []
    width _ = 1
    modules _ = []
    check _ = return True
    schema _ = return empty
   
class Presentable a where
    present :: a -> InterpM Doc

    
data Element = forall a. (Eq a, Show a, Renderable a, Presentable a, Filterable a) => Element a 

instance Eq Element where
    (Element a) == (Element b) = show a == show b
instance Show Element where
    show (Element a) = show a
    
    
type Filter = Element -> [Element]
class Filterable a where
    isNamed :: Name -> a -> [Element]
    parameters :: a -> [String]
    children :: a -> [Element]
    contains :: (Identifiable i) => i -> a -> Bool
    isNamed _ _ = []
    parameters _ = []
    children _  = []
paths :: Element -> [[Element]]
paths t@(Element e) 
    | null br = [[t]]
    | otherwise = [t:p | b <- br, p <- paths b]
    where br = children e
dfs :: Element -> [Element]
dfs t@(Element e) = t : concat (map dfs (children e))

support :: Element            -- subtree whose support is to be found
        -> Element            -- whole tree
        -> Maybe [Element]    -- path leading to subtree
support s = safeHead . filter (any (==s)) . paths

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

ident :: Filter 
ident a = [a]

(>>>) :: Filter -> Filter -> Filter
(f >>> g) t =  concat [g t' | t' <- f t]

(<++>) :: Filter -> Filter -> Filter
(f <++> g) t =  f t ++ g t

hasName :: String -> Filter
hasName n (Element e) = isNamed n e

hasParameters :: [String] -> Filter
hasParameters ps (Element e)  | ps == qs = [Element e]
                                    | otherwise = []
                                    where qs = parameters e

getChildren :: Filter
getChildren (Element e) = children e

orElse :: Filter -> Filter -> Filter
orElse f g t  | null res1 = g t
                    | otherwise = res1
                    where res1 = f t

both :: Filter -> Filter -> Filter
both f g t | null (f t) = []
                 | null (g t) = []
                 | otherwise = (f <++> g) t

deep :: Filter -> Filter
deep f  = f `orElse` (getChildren >>> deep f)

go :: String -> [String] ->  Filter
go s pars = deep $ hasName s >>> hasParameters pars

data Environment = Environment { getTransitions :: Map.Map Identifier [Transition]
                               , getPath        :: Path
                               , getContext     :: Context
                               , getClasses     :: Classes
                               }
-- data Style = Style Int Classes
type Classes = [String]

type InterpM = ReaderT Environment (ExceptT String Identity)

type ElementState = (Name,Int,Map.Map Identifier [Transition])

type SElement = State ElementState

nextIndex :: SElement Int
nextIndex = state $ \(n,i,f) -> (i,(n,i+1,f))

type Binding = (String,Expression String)
(<=>) :: Attribute -> Expression a -> (Attribute,Expression a)
a <=> expr = (a,expr)
data Void = Void deriving (Eq,Show)
data Chunk = Chunk Name [Binding] deriving (Eq,Show)
data Nest s p = Nest s p deriving (Eq,Show)
data Tuple p q = Tuple p q deriving (Eq,Show)
    
class (Eq p) => Posable p where
    generatePose :: p -> InterpM Doc
    toPoseList :: p -> [Pose]
    -- to :: p -> Filter
    toFilter :: p -> Filter
    poseRefs :: p -> InterpM [Name]
    pprint :: p -> InterpM Doc
    
data Pose = forall a. (Posable a) => Pose a

instance Posable Void where
    generatePose Void = return empty
    toPoseList Void = []
    -- toFilter Void = ident
    toFilter Void = ident
    poseRefs Void = return []
    pprint Void = return empty
 
instance Eq Pose where
    a == b = show a == show b
    
instance Show Pose where
    show (Pose pose) = let (Right pp) = runInterp (generatePose pose) (Environment Map.empty [] EmptyContext []) in render pp
    
instance Posable Chunk where
    generatePose (Chunk s pars) = do 
        pars' <- sequence (map (\(k,Expression e) ->
                    case e of 
                        (ConstExpr expr) -> do 
                            e' <- generateCtrlExpr e
                            return $ text k <> equals <> e'
                        _ -> do
                            e' <- generateCtrlExpr e
                            return (text k <> equals <> text "\"' + " <> e' <> text " + '\"")
                          ) pars)    
        case pars' of
            [] -> return $ text s
            _  -> return $ text s <> braces (hcat $ punctuate comma pars')
    toPoseList s = [Pose s]
    -- toFilter (Chunk s pars) = go s (map fst pars)
    toFilter (Chunk s pars) = go s (map fst pars)
    poseRefs (Chunk s pars) = 
        return $ concat (map (\(k,Expression e) ->
                        case e of 
                            (IdExpr id) -> [id]
                            _           -> []
                              ) pars)
    pprint (Chunk s pars) = return $ text s
    
instance (Posable s, Posable p) => Posable (Nest s p) where
    generatePose (Nest s p) = do 
        s' <- generatePose s 
        p' <- generatePose p
        return $ s' <> text "/" <> p'
    toPoseList p = [Pose p]
    -- toFilter (Nest s p) = toFilter s >>> toFilter p
    toFilter (Nest s p) = toFilter s >>> toFilter p
    poseRefs (Nest s p) = pure (++) <*> poseRefs s <*> poseRefs p
    pprint (Nest s p) = do
        s' <- pprint s
        p' <- pprint p
        return $ s' <> text "/" <> p'

    
instance (Posable p, Posable q) => Posable (Tuple p q) where
    generatePose (Tuple p q) = do 
        poses <- sequence (map (\(Pose a) -> generatePose a) (toPoseList p ++ toPoseList q))
        return $ parens (hcat $ punctuate comma poses)
    toPoseList (Tuple p q) = toPoseList p ++ toPoseList q
    -- toFilter (Tuple p q) = many (map (\(Pose a) -> Filter a) (toPoseList p ++ toPoseList q))
    -- toFilter (Tuple p q) = both (toFilter p) (toFilter q)
    toFilter (Tuple p q) = both (toFilter p) (toFilter q)
    poseRefs (Tuple p q) = pure (++) <*> poseRefs p <*> poseRefs q
    pprint (Tuple p q) = do 
        poses <- sequence (map (\(Pose a) -> pprint a) (toPoseList p ++ toPoseList q))
        return $ parens (hcat $ punctuate comma poses)
    
(</>) :: (Posable p) => Chunk -> p -> Nest Chunk p
s </> p = Nest s p

(<&>) :: (Posable p, Posable q) => p -> q -> Tuple p q
p <&> q = Tuple p q

class Parameterizable a where
    f :: String -> [(String,Expression String)] -> a

instance Parameterizable Chunk where
    f s pars = Chunk s pars
    
instance (Parameterizable a) => Parameterizable ((String,Expression String) -> a) where
    f s pars par = f s (par:pars)
    
seg' :: (Parameterizable a) => String -> a
seg' s = f s []

goto :: Chunk -> Chunk
goto = id
seg'' :: String -> Chunk
seg'' = goto . seg'
seg :: String -> [(String,Expression String)] -> Chunk
seg s pars = Chunk s pars

-- class C a where
    -- f :: String -> a
         
-- instance C x => C (Char -> x) where
    -- f a x = f (a ++ [x])

-- instance C x => C (Bool -> x) where
    -- f a x = f (a ++ show x)

-- instance C x => C (String -> x) where
    -- f a x = f (a ++ x)
-- -- data End = End
-- -- instance (x ~ String) => C (End -> x) where
    -- -- f a _ = a
    
-- instance (x ~ String) => C x where
    -- f a = a
    
data Transition = forall a. (Show a, Identifiable a) => Transition a Event Action Pose 
instance Show Transition where
    show (Transition el ev act pose) = "Transition " ++ show el ++ " " ++ show ev ++ " " ++ show act ++ " " ++ show pose
transition :: (Show a, Identifiable a, Posable p) => a -> Event -> Action -> p -> SElement Transition
transition el ev act pose = do
    (n,i,transMap) <- get
    let id = identifier el
    let trans = Transition el ev act (Pose pose)
    put (n,i,Map.insertWith (++) id [trans] transMap)
    return trans
    
runInterp :: InterpM Doc -> Environment -> Either String Doc
runInterp ev env = runIdentity (runExceptT (runReaderT ev env))

runInterpList :: InterpM [Doc] -> Environment -> Either String [Doc]
runInterpList ev env = runIdentity (runExceptT (runReaderT ev env))

-- data Context = Context { getList :: Maybe Name
                       -- , getForm :: Maybe Name
                       -- }
                -- deriving (Eq,Show)
data Context = EmptyContext
             | ListContext Name 
             | PlainFormContext Name 
             | QueryFormContext Name 
             deriving (Eq,Show)
             
-- getList :: Context -> Maybe Name
-- getList EmptyContext = Nothing
-- getList (ValidatorContext _ c) = getList c
-- getList (ListContext n _) = Just n
-- getList (PlainFormContext _ c) = getList c

-- getForm :: Context -> Maybe Name
-- getForm EmptyContext = Nothing
-- getForm (ValidatorContext _ c) = getForm c
-- getForm (PlainFormContext n _) = Just n
-- getForm (ListContext _ c) = getForm c

-- getValidator :: Context -> Maybe Validator
-- getValidator (ValidatorContext v _) = Just v
-- getValidator _ = Nothing
----------------------------------------------------------
-- generation functions
----------------------------------------------------------
-- first argument is true if expression is an angular expression
generateViewExpr :: Bool -> Expr -> InterpM Doc
generateViewExpr angExp (IdExpr expr) = do 
    ctx <- fmap getContext ask
    let suffix = case ctx of 
                    EmptyContext -> empty
                    ListContext id -> text "[$index]"
    let e = text expr <> suffix
    case angExp of 
        True -> return e  
        False -> return $ (braces . braces) e
        
generateViewExpr angExp (AttrExpr expr) = do
    let e = text "obj." <> text expr
    case angExp of 
        True -> return e
        False -> return $ (braces . braces) e

generateViewExpr angExp (ParExpr expr) = do 
    path <- fmap getPath ask
    let e = text (printf "get(\\'/%s\\',\\'%s\\')" (intercalate "/" path) expr)
    case angExp of 
        True -> return e
        False -> return $ (braces . braces) e
    
generateViewExpr angExp (ConstExpr expr) = do
    let e = text expr
    case angExp of 
        True -> return $ e
        False -> return $ e

generateViewExpr angExp (BinExpr op expr1 expr2)
    = do e1 <- generateViewExpr angExp expr1
         e2 <- generateViewExpr angExp expr2
         return $ parens e1 <+> text (show op) <+> parens e2
generateViewExpr angExp (UnExpr op expr)
    = do e <- generateViewExpr angExp expr
         return $ text (show op) <> parens e
         
generateCtrlExpr :: Expr -> InterpM Doc
generateCtrlExpr (IdExpr expr) = do
    ctx <- fmap getContext ask
    case ctx of 
        ListContext _ ->  return $ text "$scope." <> text expr <> text "[index]"
        _ -> return $ text "$scope." <> text expr

generateCtrlExpr (AttrExpr expr) = return $ text "obj." <> text expr
 -- do
    -- ctx <- fmap getContext ask
    -- traceShow ("ctx: " ++ show ctx ++ " expr: " ++ expr) $ case (getList ctx, getForm ctx) of 
        -- (Nothing,Just _) -> return $ text "obj." <> text expr
        -- (_, _) ->  return $ text "obj." <> text expr -- traceShow ("in generateCtrlExpr:" ++ show ctx ++ " expr: " ++ expr) $ 

generateCtrlExpr (ParExpr expr) = do 
    path <- fmap getPath ask
    return $ text (printf "$scope.get('/%s','%s')" (intercalate "/" path) expr)
generateCtrlExpr (ConstExpr expr) = return $ doubleQuotes $ text expr
-- generateCtrlExpr (AttrExpr expr) = return $ text "obj" <> brackets (text (show expr))-- return $ text $ show expr
generateCtrlExpr (BinExpr op expr1 expr2)
    = do e1 <- generateCtrlExpr expr1
         e2 <- generateCtrlExpr expr2
         return $ parens e1 <+> text (show op) <+> parens e2
generateCtrlExpr (UnExpr op expr)
    = do e <- generateCtrlExpr expr
         return $ text (show op) <> parens e




         
generateViewTransition :: Transition -> InterpM Doc
generateViewTransition (Transition el Click act pose) = do
    let id = identifier el
    ctx <- fmap getContext ask
    obj <- case ctx of 
            EmptyContext -> return empty
            ListContext _ -> return $ text "obj,$index" 
    return $ text (printf "ng-click=\"%sClick(" id)  <> obj <> text ")\""
    -- pose' <- generateViewpose pose
    -- return $ text "ng-click=\"go('" <> pose' <> text "');console.log('in generateViewTransition()');\""
    
    

        
generateCtrlTransition :: Transition -> InterpM Doc
generateCtrlTransition t@(Transition el Click act (Pose pose)) = do
    -- checkTransition t
    let id = identifier el
    pose' <- generatePose pose
    act' <- generateAction act
    ctx <- fmap getContext ask
    obj <- {-traceShow ctx $ -}case ctx of 
            ListContext _ -> return $ text "obj,index" 
            _  -> return empty
    return $ text (printf "$scope.%sClick = function(" id) <> obj <> text ") {"
          $$ nest 4 (act'
          $$ text "$scope.go('" <> pose' <> text "');")
          $$ text "};"

generateAction :: Action -> InterpM Doc
generateAction EmptyAction = return empty
generateAction (Create obj url) = -- traceShow (show obj) $
        do  let obj' = braces $ text $ intercalate "," ((map (\(k,v) -> show k ++ ": $scope." ++ v)) obj)
            url' <- generateAddress url
            return $ text "console.log('Creating ' +" 
             <+> obj' <> text ".toSource() +' to" <+> url' <> text "');"
              $$ text "$resource('" <> url' <> text "').save("<> obj' <> text ");"
 
generateAction (Patch obj url) = -- traceShow (show obj) $
        do  let obj' = braces $ text $ intercalate "," ((map (\(k,v) -> show k ++ ": $scope." ++ v)) obj)
            url' <- generateAddress url
            return $ text "console.log('Patch ' +" 
             <+> obj' <> text ".toSource() +' to" <+> url' <> text "');"
              $$ text "var obj = " <> obj' <> semi
              $$ text "obj.id.map(function(val,ind) {"
              $$ nest 4 (text "console.log(val + \" \" + ind);"
              $$ text "singleObj = " <> braces (text (intercalate "," ((map (\(k,v) -> show k ++ ": obj." ++ k ++ "[ind]")) obj))) <> semi
              $$ text "console.log(singleObj.toSource());"
              $$ text "$resource('" <> url' <> text "/' + val,{},{update: {method: 'PUT'}}).update(singleObj);}")
              $$ text ");"
              
generateAction (Local com) = {-traceShow com $ -} generateCommand com
              
-- generateAction (Delete obj url) = -- traceShow (show obj) $
        -- do  let obj' = braces $ text $ intercalate "," ((map (\(k,v) -> show k ++ ": $scope." ++ v)) obj)
            -- return $ text "console.log('Delete ' +" 
             -- <+> obj' <> text ".toSource() +' to" <+> url' <> text "');"
              -- $$ text "var obj = " <> obj' <> semi
              -- $$ text "obj.id.map(function(val,ind) {"
              -- $$ nest 4 (text "console.log(val + \" \" + ind);"
              -- $$ text "singleObj = " <> braces (text (intercalate "," ((map (\(k,v) -> show k ++ ": obj." ++ k ++ "[ind]")) obj))) <> semi
              -- $$ text "console.log(singleObj.toSource());"
              -- $$ text "$resource('" <> url' <> text "/' + val,{},{update: {method: 'PUT'}}).update(singleObj);}")
              -- $$ text ");"
-- $ text "+ ' " <> text (show obj) <> text ": ' + " <> text (show obj)

-- text "console.log('Createting ' +" <+> text (show obj) <> text ".toSource() +' to" <+> url' <> text "');"
-- $$ text "$resource('" <> url' <> text "').save("<> text (show obj) <> text ");"
    -- where submission (Singleton m) = text "+ ' " <> text m <> text ": ' + " <> text m
          -- submission (Array m) = error "array" -- submission m
          -- submission (Cat m n) = error "cat" -- submission m <> submission n
 
         
-- generateQueryOld :: PrimQuery -> InterpM Doc
-- generateQueryOld (BaseResource name url scheme) = return $ text "$resource('" <> url' <> text "').query({})"
-- generateQueryOld (BaseTable name scheme) = return $ text "$data." <> text name
-- generateQueryOld (Project assoc query) 
    -- = do query' <- generateQueryOld query
         -- return $ parens $ query'
            -- <> text ".map(function (obj) { return {" 
            -- <> catBy ($$) (text ",") (map (\(new,old) -> text (printf "\"%s\": obj[\"%s\"]" new old)) assoc)
            -- <> text "};})"
-- generateQueryOld (Restrict expr query) 
    -- = do query' <- generateQueryOld query
         -- expr' <- generateCtrlExpr expr
         -- return $ parens $ query'
            -- <> text ".filter(function (obj) { return " <> expr' <> text ";})"
            -- -- <> text ".map(function (obj) { console.log(obj);})"

getBases :: PrimQuery -> [PrimQuery]
getBases (EmptyQuery) = []
getBases b@(BaseTable _ _) = [b]
getBases b@(BaseResource _ _ _) = [b]
getBases (Project _ q) = getBases q
getBases (Restrict _ q) = getBases q
getBases (Binary _ q1 q2) = concat $ map getBases [q1,q2]
getBases (Computation _ _ q) = getBases q
getBases (Count q) = getBases q

generateTables :: PrimCommand -> InterpM Doc
generateTables (Insert r c) = do
    c' <- generateTables c
    r' <- generateRecord r
    return $ c' 
          $$ text ".insert" <> parens r'
generateTables (Into (Table name _)) = return $ text "this." <> text name <+> equals <+> text "[]"
generateTables (Remove _ _) = error "Remove not permitted in table initialization"
generateTables (Update _ _ _) = error "Update not permitted in table initialization"


generateCommand :: PrimCommand -> InterpM Doc
generateCommand (Into (Table name _)) = return $ text "$data." <> text name
generateCommand (Insert r c) = do
    c' <- generateCommand c
    r' <- generateRecord r
    return $ c' 
          $$ text ".insert" <> parens r'
generateCommand (Remove expr command) 
    = do command' <- generateCommand command
         expr' <- generateCtrlExpr expr
         return $ command'
               $$ text ".remove(function(obj) { return " <> expr' <> text ";})"
generateCommand (Update expr record command) 
    = do command' <- generateCommand command
         expr' <- generateCtrlExpr expr
         record' <- generateRecord record
         return $ command'
               $$ text ".update(function(obj) { return " <> expr' <> text ";}," <> record' <> text ")"
generateRecord :: Record -> InterpM Doc
generateRecord record = do record' <- sequence (map (\(k,e) -> do e' <- generateCtrlExpr e
                                                                  return $ text k <> colon <+> e')
                                                             record)
                           return $ braces $ hcat (punctuate comma record')

generateQuery :: String -> PrimQuery -> InterpM Doc
generateQuery lhs q = do
    let dict = zip (getBases q) ([1..] :: [Int]) -- build dictionary with query bases (tables, resources)
    let generateQueryInit ((BaseResource name url _),i) = do
            url' <- generateAddress url
            return $ text "$scope." <> text lhs <> text "tmp" <> text (show i)
                 <+> equals 
                 <+> text "$resource('" <> url' <> text "').query({})" <> semi
        generateQueryInit ((BaseTable name _),i) = do
            return $ text "$scope." <> text lhs <> text "tmp" <> text (show i)
                 <+> equals 
                 <+> text "$data." <> text name <> semi
        generateQueryWatcher (b,i) = do
            ctx <- fmap getContext ask
            query' <- generateQuery' lhs q dict
            case ctx of
                EmptyContext -> return $ text "$scope.$watchCollection('" <> text lhs <> text "tmp" <> text (show i) <> text "', function(newBase,oldBase){"
                                      $$ nest 4 (text "$scope." <> text lhs <+> equals <+> query' <> semi)
                                      $$ text "});"
                ListContext _ -> return $ text "$scope.$watchCollection('" <> text lhs <> text "tmp" <> text (show i) <> text "', function(newBase,oldBase){"
                                      $$ nest 4 (text "$scope." <> text lhs <+> equals <+> text "newBase.map(function (object,index) {"
                                      $$ nest 4 (text "return" <+> query' <> semi)
                                      $$ text "});")
                                      $$ text "});"
    watchers <- sequence (map generateQueryWatcher dict)
    inits <- sequence (map generateQueryInit dict)
    return $ vcat inits
          $$ vcat watchers
    where 

generateQuery' :: String -> PrimQuery -> [(PrimQuery,Int)] -> InterpM Doc
generateQuery' lhs (Project assoc query) dict = do
    query' <- generateQuery' lhs query dict
    return $ parens $ query'
                   <> text ".map(function (obj) { return {" 
                   <> catBy ($$) (text ",") (map (\(new,old) -> text (printf "\"%s\": obj[\"%s\"]" new old)) assoc)
                   <> text "};})"
generateQuery' lhs (Restrict expr query) dict = do
    query' <- generateQuery' lhs query dict
    expr' <- generateCtrlExpr expr
    return $ parens $ query'
                   <> text ".filter(function (obj) { return " <> expr' <> text ";})"

generateQuery' lhs (Computation Sum expr query) dict = do
    query' <- generateQuery' lhs query dict
    expr' <- generateCtrlExpr expr
    return $ parens $ query'
                   <> text ".map(function (obj) { return" <+> expr' <> text ";}).reduce(function(a,b) {return Number(a)+Number(b);},0)"

generateQuery' lhs (Count query) dict = do
    query' <- generateQuery' lhs query dict
    return $ parens $ query'
                   <> text ".length"

generateQuery' lhs (Binary Times q1 q2) dict = do
    q1' <- generateQuery' lhs q1 dict
    q2' <- generateQuery' lhs q2 dict
    return $ parens $ q1'<> text ".product(" <> q2' <> text ")"

generateQuery' lhs q dict = do -- only tables and resources left here
    case lookup q dict of
        Nothing -> error (show q)
        Just i -> return $ text "$scope." <> text lhs <> text "tmp" <> text (show i) -- text "newBase" 


generateValidator :: Name -> Validator -> (Doc,[Doc]) -- validator and list or error messages
generateValidator _ EmptyValidator = (empty,[])
generateValidator m Required = (text "ng-required=\"true\"",
        [text "+ '<div class=\"bg-danger\" ng-show=\"" <> text m <> text ".$dirty &&" <+> text m <> text ".$error.required\">mandatory field</div>'"])
generateValidator m (MinLen n) = (text (printf "ng-minlength=\"%d\"" n),
        [text "+ '<div class=\"bg-danger\" ng-show=\"" <> text m <> text ".$dirty &&" <+> text m <> text (printf ".$error.minlength\">minimum length: %d</div>'" n)])
generateValidator m (MaxLen n) = (text (printf "ng-maxlength=\"%d\"" n),
        [text "+ '<div class=\"bg-danger\" ng-show=\"" <> text m <> text ".$dirty &&" <+> text m <> text (printf ".$error.maxlength\">maximum length: %d</div>'" n)])
generateValidator m (Chain val1 val2)
    = let (v1,e1) = generateValidator m val1
          (v2,e2) = generateValidator m val2
      in  (v1 <+> v2, e1 ++ e2)
         


(//) :: Address -> Address -> Address
a // b = Concat a b
stat :: String -> Address 
stat s = Static s
dyn :: Expression a -> Address 
dyn (Expression e) = Dynamic e
testAdd = stat "http://localhost:8080/backend/customers" // dyn (string "1") // dyn (string "2")

generateAddress :: Address -> InterpM Doc
generateAddress (EmptyAddress) = return empty
generateAddress (Static string) = return $ text string
generateAddress (Dynamic expr) = generateCtrlExpr expr
generateAddress (Concat a b) = do
    a' <- generateAddress a
    b' <- generateAddress b
    return $ a' <> text "/" <> b'



-- type SRand = State StdGen
-- randomInt :: SRand Int
-- randomInt = state $ \g -> randomR (1,10) g 
-- randomBool :: SRand Bool
-- randomBool = state $ \g -> randomR (False,True) g 
-- randomString :: SRand String
-- randomString = do
    -- g <- get
    -- let (length,g') = randomR (10,20) g
    -- put g'
    -- s <- finiteRandomString length
    -- return s
    
-- finiteRandomString :: Int -> SRand String
-- finiteRandomString 0 = state $ \g -> ([],g)
-- finiteRandomString n = do 
    -- g <- get
    -- let (c,g') = randomR('a','z') g
    -- put g'
    -- cs <- finiteRandomString (n-1)
    -- return (c:cs)

-- generateModel :: Table -> Doc
-- generateModel (Table name scheme) = evalState (generateTable name scheme) (mkStdGen 3)

-- generateTable :: TableName -> Scheme -> SRand Doc
-- generateTable name scheme
    -- = do nrecords <- randomInt
         -- records <- sequence (take nrecords (repeat (generateRecord scheme)))
         -- return $ text "$data." <> text name <+> text "= [" <+> (catBy ($$) (text ",") records) <+> text "];"

-- generateRecord :: Scheme -> SRand Doc
-- generateRecord attrs
    -- = do fields <- sequence $ map generateField attrs
         -- return $ braces $ catBy (<+>) (text ",") fields

-- generateField :: Attribute -> SRand Doc 
-- generateField field
    -- = do val <- randomString
         -- return $ text (show field) <> text ":" <+> text (show val)
         
