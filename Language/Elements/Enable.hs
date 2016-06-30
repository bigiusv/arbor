{-# LANGUAGE ExistentialQuantification #-}
module Language.Elements.Enable where
import Language.Commons
import Language.Expression
import Language.Query
-- import Language.Element
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

data Enable = Enable (Expression Bool) Element deriving (Eq)
instance Show Enable where
    show (Enable _ (Element e)) = "(Enable " ++ show e ++ ")"

instance Alternable Enable
   

enable :: Expression Bool -> Element -> SElement Enable
enable exp e = do i <- nextIndex 
                  return $ Enable exp e

instance Renderable Enable where
    references  (Enable _ (Element e)) = references e
    identifiers (Enable _ (Element e)) = identifiers e

    directives (Enable _ (Element e)) = directives e
    
    template (Enable (Expression expr) (Element e)) = do
        expr' <- generateViewExpr True expr
        e' <- template e
        return $ text "+ '<div ng-if=\"" <> expr' <> text "\">'"
              $$ nest 4 e'
              $$ text "+ '</div>'"
                 
    controller (Enable (Expression expr) (Element e)) = controller e
    toElementList (Enable (Expression expr) (Element e)) = toElementList e
instance Filterable Enable where
    isNamed n (Enable _ (Element e)) = isNamed n e
    -- isParameterized ps (Enable _ (Element e)) = isParameterized ps e
    parameters (Enable _ (Element e)) = parameters e
    children (Enable _ (Element e)) = children e
    contains i (Enable _ (Element e)) = contains i e
-- instance Filterable Enable where
    -- item (Enable _ (Element e)) = item e
    
instance Presentable Enable where
    present (Enable exp (Element e)) = present e 
              