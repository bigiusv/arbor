module Language.Elements.Image where
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

data Image a = Image Identifier (Expression a) deriving (Eq)
instance Show (Image a) where
    show (Image id e) = "(Image " ++ id ++ ")"
    
instance Alternable (Image a)
instance Identifiable (Image a) where
    identifier (Image id e) = id
    

image :: Expression a -> SElement (Image a)
image exp = do pref <- fmap first3 get
               i <- nextIndex 
               return $ Image (pref ++ "image" ++ show i) exp
instance Filterable (Image a) where
    contains i j = identifier i == identifier j

instance Renderable (Image a) where
    references (Image _ (Expression (IdExpr id))) = return [id]
    references (Image _ (Expression _)) = return []
    
    identifiers (Image id _) = return [id]
    
    directives _ = return []
    
    template (Image id (Expression expr)) = do
        transMap <- fmap getTransitions ask
        classes <- fmap getClasses ask
        transitions' <- sequence $ map generateViewTransition (fromMaybe [] $ Map.lookup id transMap)
        expr' <- generateViewExpr False expr
        return $    text "+ '<img src=\""<> expr' <> text"\" alt=\"" <> expr' <> text (printf "\" id=\"%s\"" id) <+> text "class=\"img-rounded" <+> hsep (map text classes) <> text "\""
                <+> hsep transitions'
                 <> text "/>'"
                 
    controller (Image id (Expression expr)) = return empty
        -- do  expr' <- generateCtrlExpr expr
            -- return $ text id <+> equals <+> expr' <> semi
    toElementList e = [Element e]
-- instance Filterable (Image a) where
    -- item (Image id e) = ItemLeaf id
     
instance Presentable (Image a) where
    present (Image id e) = do
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
              