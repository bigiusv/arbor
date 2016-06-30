module Language.Abstraction where
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

data Abstraction a  = Abstraction Parameter a deriving (Eq)
instance (Show a) => Show (Abstraction a) where
    show (Abstraction n e) = "(Abstraction " ++ n ++ " " ++ show e ++ ")"

instance Alternable (Abstraction a)
infixr 4 <\>

(<\>) :: (Renderable a) => Parameter -> a -> Abstraction a
p <\> e = Abstraction p e

instance Renderable a => Renderable (Abstraction a) where
    references (Abstraction _ e) = references e
    identifiers (Abstraction _ e) = identifiers e
    check (Abstraction _ e) = check e
    directives (Abstraction _ e) = directives e 
    template (Abstraction _ e) = template e 
    controller (Abstraction _ e) = controller e
    width (Abstraction _ e) = width e
    modules (Abstraction _ e) = modules e
    toElementList (Abstraction _ e) = toElementList e

parameter :: Parameter -> SElement Parameter
parameter = return 

-- instance (Filterable a) => Filterable (Abstraction a) where
    -- item (Abstraction p e) = ItemParametric p (item e)

instance (Filterable a) => Filterable (Abstraction a) where
    children (Abstraction p e) = children e
    parameters (Abstraction p e) = p : parameters e
    contains i (Abstraction p e) = contains i e
    
instance (Presentable a) => Presentable (Abstraction a) where
    present (Abstraction _ e) = present e