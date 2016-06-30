module Language.Query where
import Language.Commons
import Language.Expression
import Control.Monad.State
import Data.List
import Debug.Trace

type TableName  = String
type ResourceName  = String
-- type ResourceUrl= String
type Value = String


type Scheme     = [Attribute]
type Assoc      = [(Attribute,Attribute)]
data Rel    = Rel Assoc deriving (Read,Show)
data Attr   = Attr Attribute deriving (Read,Show)
data Table  = Table TableName Scheme deriving (Eq,Read,Show)
data Resource = Resource ResourceName Address Scheme deriving (Eq, Show)
data Address = EmptyAddress
             | Static String
             | Dynamic Expr
             | Concat Address Address
             deriving (Eq,Show)

data PrimQuery  = BaseTable TableName Scheme
                | BaseResource ResourceName Address Scheme
                | Project Assoc PrimQuery
                | Restrict Expr PrimQuery
                | Computation AggrOp Expr PrimQuery
                | Count PrimQuery
                | Binary RelOp PrimQuery PrimQuery
                | EmptyQuery
                deriving (Eq,Show)

data RelOp      = Times
                | Union
                | Intersect
                | Divide
                | Difference
                deriving (Eq,Show)

data AggrOp = Sum 
            | Average
            deriving (Eq,Show)
            
type QueryState = (Int,PrimQuery)
type Query      = State QueryState

updatePrimQuery :: (PrimQuery -> PrimQuery) -> Query PrimQuery
updatePrimQuery f 
    = state $ \(i,qt) -> (qt,(i,f qt))

uniqueQuery :: Query Int
uniqueQuery 
    = state $ \(i,qt) -> (i,(i+1,qt))

times :: PrimQuery -> PrimQuery -> PrimQuery
times (EmptyQuery) query    = query
times query (EmptyQuery)    = query
times query1 query2         = Binary Times query1 query2

resource :: Resource -> Query Rel
resource (Resource name url scheme)
    = do updatePrimQuery (\q -> times q (BaseResource name url scheme))
         -- return (Rel (inverse assoc))
         return (Rel (inverse $ assocFromScheme scheme))
         
table :: Table -> Query Rel
table (Table name scheme)
    = do assoc <- uniqueAssoc scheme
         updatePrimQuery (\q -> times q (Project assoc (BaseTable name scheme)))
         -- updatePrimQuery (\q -> times q (BaseTable name scheme))
         return (Rel (inverse assoc))
         -- return (Rel (inverse $ assocFromScheme scheme))

uniqueAssoc :: Scheme -> Query Assoc
uniqueAssoc scheme 
    = do i <- uniqueQuery
         return (map (\attr -> (attr ++ show i, attr)) scheme)
         
inverse :: [(a,b)] -> [(b,a)]
inverse xs
    = map (\(a,b) -> (b,a)) xs
    
scheme :: PrimQuery -> Scheme
scheme EmptyQuery               = []
scheme (BaseTable nm attrs)     = attrs
scheme (BaseResource nm _ attrs)= attrs
scheme (Project assoc q)        = map fst assoc
-- scheme (Insert r q)             = scheme q
scheme (Restrict expr q)        = scheme q
scheme (Binary op q1 q2)        = case op of
                                    Times           -> attr1 ++ attr2
                                    Union           -> attr1
                                    Intersect       -> attr1 \\ attr2
                                    Divide          -> attr1
                                    Difference      -> attr1
                                  where attr1       = scheme q1
                                        attr2       = scheme q2
(!) :: Rel -> Name -> Expression String
(Rel assoc) ! attr
    = case lookup attr assoc of
        Just realname   -> fromAttribute realname
        Nothing         -> error ("unknown attribute " ++ attr ++ " in " ++ show assoc)
        
assocFromScheme :: Scheme -> Assoc
assocFromScheme scheme
    = map (\attr -> (attr,attr)) scheme

assocFromRecord :: Record -> Assoc
assocFromRecord record
    = map (\(attr,_) -> (attr,attr)) record
    
compose :: Assoc -> Assoc -> Assoc
compose assoc1 assoc2
    = map (\(a1,a2) -> (a1, case lookup a2 assoc1 of 
                                Just a3 -> a3
                                Nothing -> error ("partial compose " ++ a2 ++ " not found: assoc1 = " ++ show assoc1 ++ " assoc2 = " ++ show assoc2) )) assoc2
projectAttr :: Rel -> [String] -> Query Rel
projectAttr r attrs = 
    let assoc = map (\a -> (r ! a *@* a)) attrs
    in project assoc
            
project :: Assoc -> Query Rel
project assoc
    = do assocF <- uniqueAssoc (map fst assoc)
         -- updatePrimQuery (\q -> Project (assocFromScheme (scheme q) ++ compose assoc assocF) q)
         updatePrimQuery (\q -> Project (compose (assocFromScheme $ scheme q) (compose assoc assocF)) q)
         return (Rel (inverse assocF))

restrict :: Expression Bool -> Query Rel
restrict (Expression expr)
    = do q'<- updatePrimQuery (\q -> Restrict expr q)
         return (Rel (inverse (assocFromScheme $ scheme q')))

add :: Expression a -> Query ()
add (Expression expr)
    = do q'<- updatePrimQuery (\q -> Computation Sum expr q)
         -- return (Rel [("value","value")])
         return ()
         
count :: Query ()
count 
    = do q'<- updatePrimQuery (\q -> Count q)
         return ()
         
-- dirty check of attribute existence
checkAttributes :: [String] -> Scheme -> Bool
checkAttributes attrs scheme =
    foldl (&&) True $ map (\attr -> case elem attr scheme of
        False -> error ("attribute " ++ attr ++ " not found in " ++ show scheme)
        True -> True) attrs

type Record = [(Attribute,Expr)]
(*=*) :: Attribute -> Expression a -> (Attribute,Expr)
a *=* (Expression expr) = (a,expr)

(*@@*) :: Attribute -> Attribute -> (Attribute,Attribute)
a *@@* b = (b,a)
(*@*) ::  Expression String -> Attribute -> (Attribute,Attribute)
(Expression (AttrExpr a)) *@* b = (b,a)

data PrimCommand = EmptyCommand 
                 | Into Table
                 | Insert Record PrimCommand
                 | Remove Expr PrimCommand
                 | Update Expr Record PrimCommand
                 deriving (Eq,Show)

type CommandState = PrimCommand
type Command = State CommandState


updatePrimCommand :: (PrimCommand -> PrimCommand) -> Command PrimCommand
updatePrimCommand f = state $ \c -> (c,f c)

commandScheme :: PrimCommand -> Scheme

commandScheme EmptyCommand               = []
commandScheme (Into (Table nm attrs))     = attrs
commandScheme (Insert _ c)     = commandScheme c
commandScheme (Remove _ c)     = commandScheme c
commandScheme (Update _ _ c)     = commandScheme c

into :: Table -> Command Rel
into t@(Table name scheme)
    = do updatePrimCommand (\q -> Into t)
         return (Rel (inverse $ assocFromScheme scheme))
         
insert :: Record -> Command Rel
insert r = 
    do c <- get
       case checkAttributes (map fst r) (commandScheme c) of
            False -> error ("not reachable")
            True -> do put (Insert r c)
                       let assoc = compose (assocFromRecord r) (assocFromScheme $ commandScheme c)
                       return $ Rel (inverse assoc) 

remove :: Expression Bool -> Command Rel
remove (Expression expr)
    = do c'<- updatePrimCommand (\c -> Remove expr c)
         return (Rel (inverse (assocFromScheme $ commandScheme c')))
         

update :: Expression Bool -> Record -> Command Rel
update (Expression expr) record
    = do c'<- updatePrimCommand (\c -> Update expr record c)
         return (Rel (inverse (assocFromScheme $ commandScheme c')))
         
         
         