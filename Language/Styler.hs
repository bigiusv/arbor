module Language.Styler where
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

data Styler a = Styler Classes a deriving (Eq)
instance (Show a) => Show (Styler a) where
    show (Styler cs e) = "(Styler " ++ show cs ++ " " ++ show e ++ ")"

(<:>) :: (Renderable a) => a -> Classes -> Styler a
e <:> cs = Styler cs e

instance (Renderable a) => Renderable (Styler a) where
    references (Styler _ e) = references e
    identifiers (Styler _ e) = identifiers e
    check (Styler _ e) = check e
    directives (Styler _ e) = directives e 
    template (Styler classes e) = do
         let update = \(Environment m p c s) -> Environment m p c classes
         local update (template e)
    controller (Styler classes e) = do
         let update = \(Environment m p c s) -> Environment m p c classes
         local update (controller e)
    width (Styler _ e) = width e
    modules (Styler _ e) = modules e
    toElementList (Styler _ e) = toElementList e
    classes (Styler cs _) = cs
    
instance Alternable (Styler a)
instance (Filterable a) => Filterable (Styler a) where
    isNamed n (Styler _ e) = isNamed n e
    parameters (Styler _ e) = parameters e
    children (Styler _ e) = children e
    contains i (Styler _ e) = contains i e

-- instance (Filterable a) => Filterable (Styler a) where
    -- item (Styler _ e) = item e
    
instance (Presentable a) => Presentable (Styler a) where
    present (Styler _ e) = present e