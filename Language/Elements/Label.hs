module Language.Elements.Label where
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
data Label a = Label Identifier (Expression a) deriving (Eq)

instance Show (Label a) where
    show (Label id e) = "(Label " ++ id ++ ")"
    
instance Renderable (Label a) where
    references (Label _ (Expression (IdExpr id))) = return [id]
    references (Label _ (Expression _)) = return []
    
    identifiers (Label id _) = return [id]
    
    directives _ = return []
                               
    template (Label id (Expression expr)) = do
        transMap <- fmap getTransitions ask
        transitions' <- sequence $ map generateViewTransition (fromMaybe [] $ Map.lookup id transMap)
        ctx <- fmap getContext ask
        classes <- fmap getClasses ask
         -- traceShow ("in template label before generateViewExpr: expr=" ++ show expr ++ " ctx: " ++ show ctx) $ 
        expr' <- generateViewExpr False expr
        -- expr' <- generateViewExpr True expr

        let suffix = case ctx of
                        ListContext _ -> text "[$index]"
                        _ -> empty
        -- return $ text "+ '<input type=\"text\" ng-model=\"" <> text id <> text "\" class=\"arbor-label" <+> hsep (map text classes) <> text "\""
             -- <+> hsep transitions'
             -- -- <+> text "ng-bind=\"" <> expr' <> text "\"></label>'"
             -- <+> text "value=\"" <> expr' <> text "\"/>'"
        return $ text "+ '<span id=\"" <> text id <> text "\" class=\"" <+> hsep (map text classes) <> text "\""
             <+> hsep transitions'
             -- <> text ">" <> expr' <> text "</span>'"
             <> text ">{{" <> text id <> suffix <> text "}}</span>'"

    -- template (Label id (Expression expr)) = do
        -- transMap <- fmap getTransitions ask
        -- ctx <- fmap getContext ask
        -- classes <- fmap getClasses ask
        -- let suffix = case ctx of
                        -- EmptyContext -> empty
                        -- ListContext _ -> text "[$index]"
                        -- PlainFormContext _ -> error "PlainFormContext not allowed in input"
        -- transitions' <- sequence $ map generateViewTransition (fromMaybe [] $ Map.lookup id transMap)
        -- return $ text "+ '<span id=\"" <> text id <> text "\" class=\"" <+> hsep (map text classes) <> text "\""
             -- <+> hsep transitions'
             -- -- <+> text "ng-bind=\"" <> expr' <> text "\"></label>'"
             -- -- <> text ">" <> expr' <> text "</span>'"
             -- <> text ">{{" <> text id <> suffix <> text "}}</span>'"
                 
    -- controller (Label id (Expression expr)) = return empty
        -- -- do  ctx <- fmap getContext ask
            -- -- expr' <- generateCtrlExpr expr
            -- -- return $ text "$scope." <> text id <+> equals <+> expr' <> semi
            
    controller (Label id (Expression expr)) = -- return empty
        do  expr' <- generateCtrlExpr expr
            transMap <- {-traceShow expr $ traceShow expr' $ -}fmap getTransitions ask
            ctx <- fmap getContext ask
            let init = case ctx of 
                            EmptyContext -> text "$scope." <> text id <+> equals <+> expr' <> semi 
                            ListContext _-> text "$scope.$watchCollection('context', function(newContext,oldContext){"
                                   $$ nest 4 (text "$scope." <> text id <+> equals 
                                  <+> text "newContext.map(function(obj,ind) {return " <> expr' <> semi <> text"});")
                                   $$ text "});"
                            PlainFormContext _->  text "$scope." <> text id <+> equals <+> expr' <> semi 
            return init
            
    -- template (Label id (Expression expr)) = do
        -- transMap <- fmap getTransitions ask
        -- ctx <- fmap getContext ask
        -- classes <- fmap getClasses ask
        -- let suffix = case ctx of
                        -- EmptyContext -> empty
                        -- ListContext _ -> text "[$index]"
                        -- PlainFormContext _ -> error "PlainFormContext not allowed in input"
        -- transitions' <- sequence $ map generateViewTransition (fromMaybe [] $ Map.lookup id transMap)
        -- return $ text "+ '<input id=\"" <> text id <> text "\" type=\"text\" class=\"" <+> hsep (map text classes) <> text "\""
             -- <+> text "ng-model=\"" <> text id <> suffix <> text "\""
             -- <+> hsep transitions'
             -- <+> text "name=" <> doubleQuotes (text id)
             -- <+> text "style=\"outline:none;border-color:inherit;-webkit-box-shadow: none;box-shadow: none;\""
              -- <> text "/>'"

    -- controller (Label id (Expression expr)) = -- return empty
        -- do  expr' <- generateCtrlExpr expr
            -- transMap <- fmap getTransitions ask
            -- ctx <- fmap getContext ask
            -- let init = case ctx of 
                            -- EmptyContext -> text "$scope." <> text id <+> equals <+> expr' <> semi 
                            -- ListContext _-> text "$scope.$watchCollection('context', function(newContext,oldContext){"
                                   -- $$ nest 4 (text "$scope." <> text id <+> equals 
                                  -- <+> text "newContext.map(function(obj,ind) {return " <> expr' <> semi <> text"});")
                                   -- $$ text "});"
                            -- PlainFormContext _-> error ("no PlainFormContext allowed for Input")
            -- return init
    toElementList e = [Element e]
-- instance Modellable (Label a) where
    -- model (Label m e) = m
instance Alternable (Label a)
instance Filterable (Label a) where
    contains i j = identifier i == identifier j
instance Identifiable (Label a) where
    identifier (Label id e) = id

label :: Expression a -> SElement (Label a)
label exp =  do pref <- fmap first3 get
                i <- nextIndex 
                return $ Label (pref ++ "label" ++ show i) exp
-- instance Filterable (Label a) where
    -- item (Label id _) = ItemLeaf id
    
instance Presentable (Label a) where
    present (Label id e) = do
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
              