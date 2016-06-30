{-# LANGUAGE ExistentialQuantification #-}
module Language.Alternative where
import Language.Commons
import Language.Expression
import Language.Query
import Language.Pose
import Language.Environment
import Language.Transition
import Language.Utils

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Text.PrettyPrint
import Text.Printf
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Debug.Trace

type Alt = (String,Element)

(<@>) :: (Eq a, Show a, Renderable a, Presentable a, Filterable a) => a -> String -> Alt
e <@> n = (n,Element e)


data Alternative    = Alternative [Alt] deriving (Eq)
instance Show Alternative where
    show (Alternative alts) = "(Alternative " ++ intercalate " " (map fst alts) ++ ")"
    -- show (Alternative alts) = "(Alternative " ++ intercalate " " (map (show. snd) alts) ++ ")"

alternative :: [Alt] -> SElement Alternative
alternative alts = return $ Alternative alts

generateAlternative :: [Alt] -> InterpM Doc
generateAlternative alts =
    do  alts' <- sequence (map generateDirective alts)
        return $ vcat alts'
            where generateDirective (name,Element elem) =
                    do temp <- local (\(Environment m p c s) -> (Environment m (p ++ [name]) c s)) (template elem)
                       ctrl <- local (\(Environment m p c s) -> (Environment m (p ++ [name]) c s)) (controller elem)
                       return $ text (printf "angular.module('%s', ['pose','data'])" name)
                             $$ nest 4 (text (printf ".directive('dir%s', function() {" (capitalize name))
                             $$ nest 4 (text "return {"
                             $$ nest 4 (text "scope: {},"
                             $$ text "template: '<div>'" 
                             $$ nest 4 temp 
                             $$ text " +'</div>'"<> comma
                             $$ text "replace: true,"
                             $$ text (printf "controller: '%sController'," name)
                             $$ text "controllerAs: 'ctrl'")
                             $$ text "};")
                             $$ text "})"
                             $$ text (printf ".controller('%sController', function($pose,$data,$scope,$resource) {" name)
                             $$ nest 4 (text "$scope." <> text "go = function(pose) { $pose.go(pose);}"
                             $$ text "$scope." <> text "match = function(url) {return $pose.match(url);};"
                             $$ text "$scope." <> text "get = function(pose,key) { console.log(pose + \" \" + key); return $pose.get(pose,key);}"
                             $$ ctrl)
                             $$ text "});")
                             


                                        
instance Renderable Alternative where
    -- poses (Alternative alts) =  -- traceShow ("in alternative: ") $ traceShow (map fst alts) $ 
                               -- concat $ map pose alts
                               -- where pose (name,Element e) = map (\(Pose p) -> (Pose (Nest (Chunk name) p))) (poses e)
                               -- -- let (Pose p') = p in (Pose (Nest (Segment name) p'))| p <- poses e]
    references (Alternative alts) = return []
    identifiers (Alternative alts) = return []
    check (Alternative alts) = foldl (\acc (name,Element e) -> do acc' <- acc
                                                                  part <- check e
                                                                  -- traceShow ("checked " ++ name ++ ": " ++ show part) $ 
                                                                  return $ acc' && part) (return True) alts
    directives (Alternative alts) =
        do dir <- generateAlternative alts 
           dirs <- sequence $ map (\(name,Element e) -> local (\(Environment m p c s) -> (Environment m (p ++ [name]) c s)) (directives e)) alts
           return (dir:concat dirs)
    template (Alternative alts) =
        do  alts' <- sequence (map generateDiv alts)
            return $ vcat alts'
                where generateDiv (name,Element elem) =
                        do path <- fmap getPath ask
                           elem' <- local (\(Environment m p c s) -> (Environment m (p ++ [name]) c s)) (template elem)
                           return $ text (printf "+ '<div class=\"container\" ng-if=\"match(\\'%s\\')\">'" ("/" ++ (intercalate "/" (path ++ [name]))))
                                 -- $$ nest 4 elem'
                                 $$ nest 4 (text "+ '<dir-" <> text name <> text ">" <> text "</dir-" <> text name <> text ">'")
                                 $$ text "+ '</div>'"
    controller (Alternative alts) = return empty

    modules (Alternative alts) = map fst alts ++ concatMap (\(Element e) -> modules e) (map snd alts)
    toElementList e = [Element e]
    schema (Alternative alts) = do
        elems <- sequence (map (\(Element e) -> schema e) (map snd alts))
        let temp = zip (map fst alts) elems
        return $ text "{"  <+> (vcommacat (map (\(name,doc) -> text name <> colon <> (text "[" <+> doc $$ text "]")) temp)) $$ text "}"
    

instance Filterable Alternative where
    isNamed n (Alternative alts) = case lookup n alts of
                                        Nothing -> [] -- error ("name " ++ n ++ " not found in " ++ (show alts))
                                        Just i -> [i]
    children (Alternative alts) = map snd alts
    contains _ _ = False
    -- subtrees (Alternative alts) = map (\(Element e) -> e) (map snd alts)
instance Presentable Alternative where
                    
    present (Alternative alts) = do
        ps <- sequence $ map presentAlternative alts
        return $ vcat ps 
            where presentAlternative (name, Element elem) = do
                    path <- fmap getPath ask
                    elem' <- local (\(Environment m p c s) -> (Environment m (p ++ [name]) c s)) (present elem)
                    let state = "/" ++ intercalate "/" (path ++ [name])
                    return $ text "State" <> (parens . doubleQuotes) (text state)
                          $$ text "Say" <> (parens. doubleQuotes) (text "Alternative " <> text name)
                          $$ nest 4 (text "; ----------------------------------------------------------"
                          $$ elem'
                          $$ text "; ----------------------------------------------------------")
        
        