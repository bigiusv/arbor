module Language.Elements.Form where
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
import Language.Elements.Button
import Debug.Trace

data PlainForm a b = PlainForm Identifier a (Button b) deriving (Eq)
data QueryForm a b = QueryForm Identifier (Query Rel) (Rel -> SElement a) (Button b)
instance (Show a) => Show (QueryForm a b) where
    -- show (QueryForm id q f b) = "QueryForm " ++ " (" ++ show id ++ " " ++ show (evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)) ++ ")"
    show (QueryForm id q f b) = "(QueryForm " ++ id ++ ")"

instance (Show a) => Show (PlainForm a b) where
    show (PlainForm id e b) = "(PlainForm " ++ id ++ ")"
    
instance Eq (QueryForm a b) where
    (QueryForm id1 _ _ _) == (QueryForm id2 _ _ _) = id1 == id2
 
-- data Form a b = PlainForm (PlainForm a b)
              -- | QueryForm (QueryForm a b)
              
instance Alternable (PlainForm a b)
instance Alternable (QueryForm a b)

instance Submittable a => Submittable (PlainForm a b) where
    submission (PlainForm id e b) = submission e
    
instance Submittable a => Submittable (QueryForm a b) where
    submission (QueryForm id q f b) = submission $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
 
instance Renderable a => Renderable (PlainForm a b) where
    references  (PlainForm id e (Button _ (Expression expr))) = do
        e' <- references e
        let b' = case expr of
                    (IdExpr id) -> [id]
                    _ -> []
        return $ e' ++ b'
    identifiers (PlainForm id e (Button bid _)) = do
        e' <- identifiers e
        return $ e' ++ [bid]

    directives (PlainForm id e b) = local (\(Environment m p c s) -> Environment m p (PlainFormContext id) s) (directives e)
    template (PlainForm id e (Button _ (Expression expr))) 
        = do e' <- local (\(Environment m p c s) -> Environment m p (PlainFormContext id) s) (template e)
             expr' <- generateViewExpr True expr
             -- b' <- local (\(m,p,_) -> (m,p,PlainFormContext id EmptyValidator)) (template b)
             return $ text "+ '<form name=\"" <> text id <> text "\" ng-submit=\"" <> text id <> text "Submit()\">'"
                   $$ nest 4 (e'
                   $$ text "+ '<input type=\"submit\" class=\"btn btn-primary\" value=\"" <> expr' <> text "\"/>'")
                   $$ text "+ '</form>'"
    controller (PlainForm id e b)
        = do let update = \(Environment m p c s) -> Environment m p (PlainFormContext id) s
             e' <- local update (controller e)
             b' <- local update (controller b)
             return $ e' 
                 $$ b'
                 $$ text "$scope." <> text id <> text "Submit = function() {"
                 $$ nest 4 (text "if($scope." <> text id <> text ".$valid)"
                 $$ let id = identifier b in nest 4 (text "$scope." <> text id <> text "Click();")
                 $$ text "else"
                 $$ nest 4 (text "console.log('Unable to save. Validation error!');"))
                 $$ text "};"

    width (PlainForm mod e b) = width e
    modules (PlainForm mod e b) = modules e
    toElementList (PlainForm mod e b) = toElementList e
    
plainForm :: (Renderable a, Submittable a) => a -> (Button b) -> SElement (PlainForm a b)

plainForm e b = do pref <- fmap first3 get
                   i <- nextIndex
                   return $ PlainForm (pref ++ "form" ++ show i) e b
instance (Filterable a) => Filterable (PlainForm a b) where
    isNamed n (PlainForm id e b) = isNamed n e
    parameters (PlainForm id e b) = parameters e
    children (PlainForm id e b) = children e
    contains i (PlainForm id e b) = contains i e
    
-- instance (Filterable a) => Filterable (PlainForm a b) where
    -- item (PlainForm id e b) = ItemSequence (item e) (item b)
    
instance Presentable a => Presentable (PlainForm a b) where
    present (PlainForm id e b) = do
        e' <- present e
        b' <- present b
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
              $$ e'
              $$ b'
              
instance Renderable a => Renderable (QueryForm a b) where
    
    references (QueryForm id q f (Button bid (Expression expr))) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        let e = evalState (f rel) (id ++ "",0,Map.empty)
        e' <- references e
        let b' = case expr of
                    (IdExpr eid) -> [eid]
                    _ -> []
        return $ e' ++ b'
    identifiers (QueryForm id q f (Button bid (Expression expr))) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        let e = evalState (f rel) (id ++ "",0,Map.empty)
        e' <- identifiers e
        return $ e' ++ [bid]
        
    directives (QueryForm id q f b) = return []

    template (QueryForm id q f (Button bid (Expression expr))) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        let e = evalState (f rel) (id ++ "",0,Map.empty)
        let update = \(Environment m p c s) -> Environment m p (QueryFormContext (id++"Item")) s
        transMap <- fmap getTransitions ask
        e' <- local update (template e)
        expr' <- generateViewExpr True expr
        transitions' <- sequence $ map generateViewTransition (fromMaybe [] $ Map.lookup bid transMap)
        return $ text "+ '<form name=\"" <> text id <> text "\" ng-submit=\"" <> text id <> text "Submit()\">'"
               $$ nest 4 (text "+ '<ng-form name=\"" <> text id <> text "Item\" ng-repeat=\"obj in " <> text id <> text "\">'"
               $$ nest 4 e'
               $$ text "+ '</ng-form>'")
               $$ text "+ '<input type=\"submit\" class=\"btn btn-default\"" <+> hsep transitions' <+> text "value=\"" <> expr' <> text "\"/>'"
               $$ text "+ '</form>'"

    controller (QueryForm id q f b) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        let e = evalState (f rel) (id ++ "",0,Map.empty)
        let update = \(Environment m p c s) -> Environment m p (QueryFormContext id) s
        e' <- local update (controller e)
        b' <- local update (controller b)
        q' <-  generateQuery id pq
        ctx <- fmap getContext ask
        -- return $ text "$scope.obj =" <+> q' <> semi
        transMap <- fmap getTransitions ask
        return $ q' <> semi
              $$ e'
              $$ b'
        
    width (QueryForm id q f b) = width $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    toElementList (QueryForm id q f b) = toElementList $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    
queryForm :: (Alternable a) => Query Rel -> (Rel -> SElement a) -> (Button b) -> SElement (QueryForm a b)
queryForm q f b = do pref <- fmap first3 get
                     i <- nextIndex
                     return $ QueryForm (pref ++ "form" ++ show i) q f b

create :: (Submittable a) => a -> Address -> Action
create a url = Create (submission a) url

patch :: (Submittable a) => a -> Address -> Action
patch a url = Patch (submission a) url

instance (Filterable a) => Filterable (QueryForm a b) where
    isNamed n (QueryForm id q f b) = isNamed n $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    parameters (QueryForm id q f b) = parameters $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    children (QueryForm id q f b) = children $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    contains i (QueryForm id q f b) = contains i $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    
-- instance (Filterable a) => Filterable (QueryForm a b) where
    -- item (QueryForm id q f b) = ItemSequence (item $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)) (item b)
    
instance Presentable a => Presentable (QueryForm a b) where
    present (QueryForm id q f b) =  do
        e' <- present (evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty))
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
              $$ e'