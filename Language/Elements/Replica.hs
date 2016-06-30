module Language.Elements.Replica where
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

data Replica a b        = Replica Identifier (Form a b)

instance (Show a) => Show (Replica a b) where
    show (Replica id f) = "Replica " ++ " (" ++ id ++ " " ++ show f ++ ")"

instance Alternable (Replica a b)

replica :: SElement (Form a b) -> SElement (Replica a b)
replica f = do pref <- fmap first3 get
               i <- nextIndex
               return $ Replica (pref ++ "replica" ++ show i) f

instance (Submittable a) => Submittable (Replica a b) where
    submission (Replica id f) = submission f 

instance (Renderable a) => Renderable (Replica a) where
    directives (Replica id q f) = do
        let update = \(m,p,c) -> case getForm c of 
                                    Nothing -> error "in directives Replica: nothing"
                                    Just formName -> (m,p,c{getForm = Just (formName ++ id)})
        temp <- local update (template elem)
        ctrl <- local update (controller elem)
        let dir = text (printf "angular.module('%s', ['pose','data'])" id) -- traceShow ("in directive replica: " ++ show (model elem)) $ 
               $$ nest 4 (text (printf ".directive('dir%s', function() {" (capitalize id))
               $$ nest 4 (text "return {"
               $$ nest 4 (text "scope: {context: '='},"
               $$ text "template: "+ '<div class=\"arbor-container\">'"
               $$ nest 4 (text (printf "+ '<ng-form name=\"%sItem\"ng-repeat=\"item in context\">'" id id)
               $$ nest 4 (temp
               $$ text (printf "+ '<button ng-show=\"$last\" ng-click=\"remove()\">-</button>'")
               $$ text (printf "+ '<button ng-show=\"$last\" ng-click=\"add()\">+</button>'"))
               $$ text "+ '</ng-form>'")
               $$ text "+ '</div>'" <> comma
               $$ text (printf "controller: '%sController'," id)
               $$ text "controllerAs: 'ctrl'")
               $$ text "};")
               $$ text "})"
               $$ text (printf ".controller('%sController', function($pose,$data,$scope,$resource) {" id)
               $$ nest 4 (text "$scope." <> text "go = function(pose) { $pose.go(pose);}"
               $$ text "$scope." <> text "match = function(url) {return $pose.match(url);};"
               $$ text "$scope." <> text "get = function(pose,key) { console.log(pose + \" \" + key + \" \" + $pose.get(pose,key)); return $pose.get(pose,key);}"
               $$ text (printf "$scope.add = function() {" )
               $$ nest 4 (text (printf "var currItem = [];")
               $$ text (printf "currItem.push({});" m))
               $$ text "};"
               $$ text ""
               $$ text (printf "$scope.remove = function() {")
               $$ nest 4 (text (printf "var currItem = %s;" (currItem prefix))
               $$ text (printf "var lastForm = currItem.%s.length-1;" m)
               $$ text (printf "currItem.%s.splice(lastForm);" m))
               $$ text "};")
               $$ ctrl)
               $$ text "});")
        dirs <- directives elem
        return (dir:dirs)

    template (Replica id q f) = do
        ctx <- fmap getContext ask
        case getForm ctx of
            Nothing -> error "in template Replica: nothing"
            Just formName -> return $ text "+ '<dir-" <> text id <+> text "context=\"" <> text id <> text "\">" <> text "</dir-" <> text id <> text ">'"
            
    controller (Replica id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        q' <-  generateQuery pq
        ctx <- fmap getContext ask
        case getContainer ctx  of 
            FormContainer _ _ -> return $ text "$scope." <> text id <+> text "=" <+> q' <> semi
            _ -> return $ empty
            
    modules (Replica id q f) = id : (modules $ evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty))
   
-- instance (Modellable a) => Modellable (Replica a) where
    -- model (Replica id q f) = 
        -- let (rel,(_,pq)) = runState q (0,EmptyQuery)
            -- elem = evalState (f rel) (0,Map.empty) in
        -- arrayfy $ model elem 
    
instance (Presentable a) => Presentable (Replica a) where
    present (Replica id q f) = do
        e' <- present (evalState (f (evalState q (0,EmptyQuery))) (id ++ "",0,Map.empty))
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
              $$ e'