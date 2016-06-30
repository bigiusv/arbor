module Language.Elements.Button where
import Language.Commons
import Language.Expression
import Language.Query
-- -- import Language.Filter
-- -- import Language.Element
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
data Button a = Button Identifier (Expression a) deriving (Eq)

instance Show (Button a) where
    show (Button id e) = "(Button " ++ id ++ ")"

button :: Expression a -> SElement (Button a)
button exp = do pref <- fmap first3 get
                i <- nextIndex 
                return $ Button (pref ++ "button" ++ show i) exp

-- instance Modellable (Button a) where
    -- model (Button m e) = m
instance Alternable (Button a)
instance Identifiable (Button a) where
    identifier (Button id e) = id
    
instance Renderable (Button a) where
    references (Button bid (Expression (IdExpr eid))) = do
        transMap <- fmap getTransitions ask
        let trans = fromMaybe [] $ Map.lookup bid transMap
        transRefs <- sequence (map (\(Transition _ _ _ (Pose pose)) -> poseRefs pose) trans)
        -- traceShow ("in button: " ++ bid ++ " " ++ show ((concat transRefs) ++ [eid])) $ 
        return $ (concat transRefs) ++ [eid]
    references (Button bid (Expression _)) = do
        transMap <- fmap getTransitions ask
        let trans = fromMaybe [] $ Map.lookup bid transMap
        transRefs <- sequence (map (\(Transition _ _ _ (Pose pose)) -> poseRefs pose) trans)
        -- traceShow ("in button: " ++ bid ++ " trans: " ++ show trans ++ " " ++ show (concat transRefs)) $ 
        return $ (concat transRefs)
    
    identifiers (Button id _) = return [id]
    
    directives _ = return []
    
    template (Button id (Expression expr)) = do
        transMap <- fmap getTransitions ask
        classes <- fmap getClasses ask
        transitions' <- sequence $ map generateViewTransition (fromMaybe [] $ Map.lookup id transMap)
        expr' <- generateViewExpr False expr
        return $    text "+ '<button id=\"" <> text id <> text "\" class=\"btn btn-primary" <+> hsep (map text classes) <> text "\""  -- ng-controller=\"%sController\"
                <+> hsep transitions'
                 <> text ">" <> expr' <> text "</button>'"
                 
    controller (Button id (Expression expr)) = -- return empty
        do  transMap <- fmap getTransitions ask
            expr' <- generateCtrlExpr expr
            transitions' <- sequence $ map generateCtrlTransition (fromMaybe [] $ Map.lookup id transMap)
            return $ text "$scope." <> text id <+> equals <+> expr' <> semi
                  $$ vcat transitions'
    toElementList e = [Element e]
    
-- instance Filterable (Button a) where
    -- item (Button id e) = ItemLeaf id

instance Filterable (Button a) where
    contains i j = identifier i == identifier j

instance Presentable (Button a) where
    present (Button id e) = do
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"