module Language.Widget where
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
import Control.Monad.State
import Text.PrettyPrint
import Text.Printf
import Data.List
import Data.Maybe
import qualified Data.Map as Map

data Widget a = Widget Int a deriving (Eq)
instance (Show a) => Show (Widget a) where
    show (Widget n e) = "(Widget " ++ show n ++ " " ++ show e ++ ")"

(<#>) :: (Renderable a) => a -> Int -> Widget a
e <#> w = Widget w e


instance (Renderable a) => Renderable (Widget a) where
    references (Widget _ e) = references e
    identifiers (Widget _ e) = identifiers e
    check (Widget _ e) = check e
    directives (Widget _ e) = directives e 
    template (Widget _ e) = template e 
    controller (Widget _ e) = controller e
    width (Widget w _) = w
    modules (Widget _ e) = modules e
    toElementList (Widget _ e) = toElementList e
instance Alternable (Widget a)
instance (Filterable a) => Filterable (Widget a) where
    isNamed n (Widget _ e) = isNamed n e
    parameters (Widget _ e) = parameters e
    children (Widget _ e) = children e
    contains i (Widget _ e) = contains i e

-- instance (Filterable a) => Filterable (Widget a) where
    -- item (Widget _ e) = item e
    
instance (Presentable a) => Presentable (Widget a) where
    present (Widget _ e) = present e