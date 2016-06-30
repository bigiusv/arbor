module Language.Elements.Input where
import Language.Commons
import Language.Expression
import Language.Query
-- import Language.Filter
-- import Language.Element
import Language.Environment
import Language.Transition
import Language.Utils

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Text.PrettyPrint
import Text.Printf
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map
import Debug.Trace

data FormInput a = FormInput Identifier String Name Validator (Expression a) deriving (Eq)
instance Show (FormInput a) where
    show (FormInput id _ _ _ _) = "(FormInput " ++ id ++ ")"
    
instance Identifiable (FormInput a) where
    identifier (FormInput id _ _ _ _) = id
instance Modifiable (FormInput a)
instance Alternable (FormInput a)
instance Submittable (FormInput a) where
    submission (FormInput id name _ _ _) = [(name,id)]
instance Filterable (FormInput a) where
    contains i j = identifier i == identifier j
formInput :: Name -> String -> Validator -> Expression a -> SElement (FormInput a)
formInput name lab val exp = do pref <- fmap first3 get
                                i <- nextIndex 
                                return $ FormInput (pref ++ "input" ++ show i) lab name val exp

instance Renderable (FormInput a) where
    references (FormInput id name lab val (Expression (IdExpr eid))) = return [eid]
    references (FormInput id name lab val (Expression _)) = return []
    
    identifiers (FormInput id name lab val _) = return [id]
    
    directives _ = return []
    
    template (FormInput id name lab val (Expression expr)) = do
        transMap <- fmap getTransitions ask
        ctx <- fmap getContext ask
        classes <- fmap getClasses ask
        let (formName,model) = 
                case ctx of
                    PlainFormContext formName -> (formName,text id)
                    QueryFormContext formName -> (formName,text id <> text "[$index]")
        let (val',errs) = generateValidator (formName ++ "." ++ id) val --  traceShow ("in let--------------id: " ++ id ++ ", ctx: " ++ show ctx ++ ", form : " ++ (show (getForm ctx)) ++ ", val: " ++ (show (getValidator ctx))) $  
        transitions' <- sequence $ map generateViewTransition (fromMaybe [] $ Map.lookup id transMap)
        return $ text "+ '<label for=\"" <> text id <> text"\">" <> text lab <> text "</label>'"
              $$ text "+ '<input id=\"" <> text id <> text "\" type=\"text\" class=\"form-control " <+> hsep (map text classes) <> text "\""
             <+> text "ng-model=\"" <> model <> text "\""
             <+> hsep transitions'
             <+> text "name=" <> doubleQuotes (text id)
             <+> {- traceShow ("val': " ++ show val') $ -} val'
              <> text "/>'"
              $$ vcat errs

    controller (FormInput id name lab val (Expression expr)) = -- return empty
        do  expr' <- generateCtrlExpr expr
            transMap <- fmap getTransitions ask
            ctx <- fmap getContext ask
            let init = case ctx of 
                            EmptyContext -> error ("no EmptyContext allowed for FormInput")
                            ListContext _-> error ("no ListContext allowed for FormInput")
                            PlainFormContext formName -> text "$scope." <> text id <+> equals <+> expr' <> semi 
                            QueryFormContext formName -> text "$scope.$watchCollection('" <> text formName <> text "', function(newContext,oldContext){"
                                         $$ nest 4 (text "$scope." <> text id <+> equals 
                                        <+> text "newContext.map(function(obj,ind) {return " <> expr' <> semi <> text"});")
                                         $$ text "});"
            return init
            -- return $ text "$scope." <> text id <+> equals <+> expr' <> semi
    toElementList e = [Element e]
-- instance Filterable (FormInput a) where
    -- item (FormInput id _ _ _) = ItemLeaf id
    
instance Presentable (FormInput a) where
    present (FormInput id name lab val (Expression expr)) = do
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"

data Input a = Input Identifier String (Expression a) deriving (Eq)
instance Show (Input a) where
    show (Input id _ _) = "(Input " ++ id ++ ")"
    
instance Modifiable (Input a)
instance Alternable (Input a)
instance Filterable (Input a) where
    contains i j = identifier i == identifier j
input :: String -> Expression a -> SElement (Input a)
input lab exp = do pref <- fmap first3 get
                   i <- nextIndex 
                   return $ Input (pref ++ "input" ++ show i) lab exp
-- instance Modellable (Input a) where
    -- model (Input m e) = m

instance Identifiable (Input a) where
    identifier (Input id lab e) = id
    

instance Renderable (Input a) where
    references (Input id lab (Expression (IdExpr eid))) = return [eid]
    references (Input id lab (Expression _)) = return []
    
    identifiers (Input id lab _) = return [id]
    
    directives _ = return []
    
    template (Input id lab (Expression expr)) = do
        transMap <- fmap getTransitions ask
        ctx <- fmap getContext ask
        classes <- fmap getClasses ask
        let suffix = case ctx of
                        EmptyContext -> empty
                        ListContext _ -> text "[$index]"
                        PlainFormContext _ -> error "PlainFormContext not allowed in input"
        transitions' <- sequence $ map generateViewTransition (fromMaybe [] $ Map.lookup id transMap)
        return $ text "+ '<label for=\"" <> text id <> text"\">" <> text lab <> text "</label>'"
              $$ text "+ '<input id=\"" <> text id <> text "\" type=\"text\" class=\"form-control" <+> hsep (map text classes) <> text "\""
             <+> text "ng-model=\"" <> text id <> suffix <> text "\""
             <+> hsep transitions'
             <+> text "name=" <> doubleQuotes (text id)
              <> text "/>'"

    controller (Input id lab (Expression expr)) = -- return empty
        do  expr' <- generateCtrlExpr expr
            transMap <- fmap getTransitions ask
            ctx <- fmap getContext ask
            let init = case ctx of 
                            EmptyContext -> text "$scope." <> text id <+> equals <+> expr' <> semi 
                            ListContext _-> text "$scope.$watchCollection('context', function(newContext,oldContext){"
                                   $$ nest 4 (text "$scope." <> text id <+> equals 
                                  <+> text "newContext.map(function(obj,ind) {return " <> expr' <> semi <> text"});")
                                   $$ text "});"
                            PlainFormContext _-> error ("no PlainFormContext allowed for Input")
            return init
    toElementList e = [Element e]
-- instance Filterable (Input a) where
    -- item (Input id lab _ ) = ItemLeaf id 
instance Presentable (Input a) where
    present (Input id lab e) = do
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
              