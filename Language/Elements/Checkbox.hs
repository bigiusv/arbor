module Language.Elements.Checkbox where
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

data Checkbox = Checkbox Identifier deriving (Eq)
instance Show Checkbox where
    show (Checkbox id) = "(Checkbox " ++ id ++ ")"

instance Renderable Checkbox where
    references (Checkbox id) = return []
    
    identifiers(Checkbox id) = return [id]
    directives _ = return []
    
    template (Checkbox id) = do
        transMap <- fmap getTransitions ask
        classes <- fmap getClasses ask
        transitions' <- sequence $ map generateViewTransition (fromMaybe [] $ Map.lookup id transMap)
        return $ text "+ '<input type=\"checkbox\" class=\"" <+> hsep (map text classes) <> text (printf  "\" ng-model=\"%s\"/>'" id)
                 
                 
    controller (Checkbox id) = return empty
    toElementList e = [Element e]
instance Alternable Checkbox     
instance Identifiable Checkbox where
    identifier (Checkbox id) = id

checkbox :: SElement Checkbox
checkbox = do pref <- fmap first3 get
              i <- nextIndex 
              return $ Checkbox (pref ++ "checkbox" ++ show i)
-- instance Filterable Checkbox where
    -- item (Checkbox id) = ItemLeaf id
    
instance Filterable Checkbox where
    contains i j = identifier i == identifier j

instance Presentable Checkbox where
    present (Checkbox id) = do
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
              