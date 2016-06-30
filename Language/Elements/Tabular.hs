module Language.Elements.Tabular where
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

data Tabular a         = Tabular Identifier (Query Rel) (Rel -> SElement a)
instance (Show a) => Show (Tabular a) where
    -- show (Tabular id q f) = "Tabular " ++ " (" ++ show id ++ " " ++ show (evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)) ++ ")"
    show (Tabular id _ _) = "(Tabular " ++ show id ++ ")"
    
instance Eq (Tabular a) where
    (Tabular id1 _ _) == (Tabular id2 _ _) = id1 == id2
    
instance Alternable (Tabular a)

tabular :: (Alternable a) => Query Rel -> (Rel -> SElement a) -> SElement (Tabular a)
tabular q f = do pref <- fmap first3 get
                 i <- nextIndex
                 return $ Tabular (pref ++ "tabular" ++ show i) q f

instance (Submittable a) => Submittable (Tabular a) where
    submission (Tabular id q f) =
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
            elem = evalState (f rel) (id ++ "",0,Map.empty)
        in  submission elem
instance (Renderable a) => Renderable (Tabular a) where
                                        
    references (Tabular id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        let e = evalState (f rel) (id ++ "",0,Map.empty)
        references e
    
    identifiers (Tabular id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        let e = evalState (f rel) (id ++ "",0,Map.empty)
        identifiers e
    
    directives (Tabular id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        let elem = evalState (f rel) (id ++ "",0,Map.empty)
        let (elem,(_,_,transitions)) = runState (f rel) (id ++ "",0,Map.empty)
        let update = \(Environment m p c s) -> (Environment (Map.unionWith (++) m transitions) p (ListContext id) s)
        temp <- local update (template elem)
        ctrl <- local update (controller elem)
        ctx <- fmap getContext ask
        classes <- fmap getClasses ask
        let dir = text (printf "angular.module('%s', ['pose','data'])" id) -- traceShow ("in directive Tabular: " ++ show (model elem)) $ 
               $$ nest 4 (text (printf ".directive('dir%s', function() {" (capitalize id))
               $$ nest 4 (text "return {"
               $$ nest 4 (text "scope: {context: '='},"
               $$ text "template: '<table class=\"table table-striped" <+> hsep (map text classes) <> text "\">'"
               $$ nest 4 (text "+ '<tbody>'"
               $$ nest 4 (text "+ '<tr ng-repeat=\"obj in context\">'"
               $$ nest 4 temp
               $$ text "+ '</tr>'")
               $$ text "+ '</tbody>'")
               $$ text "+ '</table>'" <> comma
               $$ text "replace: true,"
               $$ text (printf "controller: '%sController'," id)
               $$ text "controllerAs: 'ctrl'")
               $$ text "};")
               $$ text "})"
               $$ text (printf ".controller('%sController', function($pose,$data,$scope,$resource) {" id)
               $$ nest 4 (text "$scope." <> text "go = function(pose) { $pose.go(pose);}"
               $$ text "$scope." <> text "match = function(url) {return $pose.match(url);};"
               $$ text "$scope." <> text "get = function(pose,key) { console.log(pose + \" \" + key + \" \" + $pose.get(pose,key)); return $pose.get(pose,key);}"
               $$ ctrl)
               $$ text "});")
        dirs <- local update (directives elem)
        return (dir:dirs)

    template (Tabular id q f) = do
        ctx <- fmap getContext ask
        case ctx of 
            EmptyContext -> return $ text "+ '<dir-" <> text id <+> text "context=\""<> text id <> text "\">" <> text "</dir-" <> text id <> text ">'"
            ListContext _ -> return $ text "+ '<dir-" <> text id <+> text "context=\""<> text id <> text "[$index]\">" <> text "</dir-" <> text id <> text ">'"
                                 
    controller (Tabular id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        q' <-  generateQuery id pq
        ctx <- fmap getContext ask
        case ctx of 
            EmptyContext -> return $ q' <> semi
            ListContext _ -> return $ text "$scope.$watchCollection('context', function(newContext,oldContext){"
                                   $$ nest 4 (q' <> semi)
                                   $$ text "});"
            -- ListContext _ -> return $ text "$scope.source = function (index) {"
                                   -- $$ nest 4 (text "return" <+> q' <> semi)
                                   -- $$ text "}"
    modules (Tabular id q f) = id : (modules $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty))
    toElementList (Tabular id q f) = toElementList $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
-- instance (Modellable a) => Modellable (Tabular a) where
    -- model (Tabular id q f) = 
        -- let (rel,(_,pq)) = runState q (0,EmptyQuery)
            -- elem = evalState (f rel) (0,Map.empty) in
        -- arrayfy $ model elem 
-- instance (Filterable a) => Filterable (Tabular a) where
    -- item (Tabular id q f) = item $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
instance (Filterable a) => Filterable (Tabular a) where
    isNamed n (Tabular id q f) = isNamed n $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    parameters (Tabular id q f) = parameters $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    children (Tabular id q f) = children $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    contains i (Tabular id q f) = contains i $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
instance (Presentable a) => Presentable (Tabular a) where
    present (Tabular id q f) = do
        e' <- present (evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty))
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
              $$ e'