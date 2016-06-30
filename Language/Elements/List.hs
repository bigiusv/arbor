module Language.Elements.List where
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

data List a         = List Identifier (Query Rel) (Rel -> SElement a)
instance (Show a) => Show (List a) where
    -- show (List id q f) = "List " ++ " (" ++ show id ++ " " ++ show (evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)) ++ ")"
    show (List id _ _) = "(List " ++ show id ++ ")"
    
instance Eq (List a) where
    (List id1 _ _) == (List id2 _ _) = id1 == id2
    
instance Alternable (List a)

list :: (Alternable a) => Query Rel -> (Rel -> SElement a) -> SElement (List a)
list q f = do pref <- fmap first3 get
              i <- nextIndex
              return $ List (pref ++ "list" ++ show i) q f

instance (Submittable a) => Submittable (List a) where
    submission (List id q f) =
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
            elem = evalState (f rel) (id ++ "",0,Map.empty)
        in  submission elem
instance (Renderable a) => Renderable (List a) where
                                        
    references (List id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        let e = evalState (f rel) (id ++ "",0,Map.empty)
        references e
    
    identifiers (List id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        let e = evalState (f rel) (id ++ "",0,Map.empty)
        identifiers e
    
    directives (List id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        let elem = evalState (f rel) (id ++ "",0,Map.empty)
        let (elem,(_,_,transitions)) = runState (f rel) (id ++ "",0,Map.empty)
        let update = \(Environment m p c s) -> (Environment (Map.unionWith (++) m transitions) p (ListContext id) s)
        temp <- local update (template elem)
        ctrl <- local update (controller elem)
        ctx <- fmap getContext ask
        classes <- fmap getClasses ask
        let dir = text (printf "angular.module('%s', ['pose','data'])" id) -- traceShow ("in directive list: " ++ show (model elem)) $ 
               $$ nest 4 (text (printf ".directive('dir%s', function() {" (capitalize id))
               $$ nest 4 (text "return {"
               $$ nest 4 (text "scope: {context: '='},"
               $$ text "template: '<ul class=\"list-group" <+> hsep (map text classes) <> text "\">'" 
               $$ nest 4 (text "+ '<li class=\"list-group-item\" ng-repeat=\"obj in context\">'"
               $$ nest 4 temp
               $$ text "+ '</li>'")
               $$ text "+ '</ul>'" <> comma
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

    template (List id q f) = do
        ctx <- fmap getContext ask
        case ctx of 
            EmptyContext -> return $ text "+ '<dir-" <> text id <+> text "context=\""<> text id <> text "\">" <> text "</dir-" <> text id <> text ">'"
            ListContext _ -> return $ text "+ '<dir-" <> text id <+> text "context=\""<> text id <> text "[$index]\">" <> text "</dir-" <> text id <> text ">'"
                                 
    controller (List id q f) = do
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
    modules (List id q f) = id : (modules $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty))
    toElementList (List id q f) = toElementList $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
-- instance (Modellable a) => Modellable (List a) where
    -- model (List id q f) = 
        -- let (rel,(_,pq)) = runState q (0,EmptyQuery)
            -- elem = evalState (f rel) (0,Map.empty) in
        -- arrayfy $ model elem 
-- instance (Filterable a) => Filterable (List a) where
    -- item (List id q f) = item $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
instance (Filterable a) => Filterable (List a) where
    isNamed n (List id q f) = isNamed n $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    parameters (List id q f) = parameters $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    children (List id q f) = children $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
    contains i (List id q f) = contains i $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty)
instance (Presentable a) => Presentable (List a) where
    present (List id q f) = do
        e' <- present (evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty))
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
              $$ e'