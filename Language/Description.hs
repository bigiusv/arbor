module Language.Description where
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

data Description a = Description String a deriving (Eq)
instance (Show a) => Show (Description a) where
    show (Description n e) = "(Description " ++ n ++ " " ++ show e ++ ")"


(<?>) :: (Renderable a) => SElement a -> String -> SElement (Description a)
e <?> d = do e' <- e
             return $ Description d e'
             
             
instance (Renderable a) => Renderable (Description a) where
    references (Description _ e) = references e
    identifiers (Description _ e) = identifiers e
    check (Description _ e) = check e
    directives (Description _ e) = directives e 
    template (Description _ e) = template e 
    controller (Description _ e) = controller e
    width (Description _ e) = width e
    modules (Description _ e) = modules e
    toElementList (Description _ e) = toElementList e
instance Alternable (Description a)
instance (Filterable a) => Filterable (Description a) where
    isNamed n (Description _ e) = isNamed n e
    parameters (Description _ e) = parameters e
    children (Description _ e) = children e
    contains i (Description _ e) = contains i e

-- instance (Filterable a) => Filterable (Description a) where
    -- item (Description _ e) = item e
instance (Identifiable a) => Identifiable (Description a) where
    identifier (Description _ e) = identifier e
instance (Presentable a) => Presentable (Description a) where
    present (Description d e) = do
        e' <- present e
        return $ e' 
              $$ text "Say" <> (parens . doubleQuotes) (text d)
