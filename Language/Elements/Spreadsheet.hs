module Language.Elements.Spreadsheet where
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
import Language.Elements.Button
import Debug.Trace

data Spreadsheet b = Spreadsheet Identifier Resource (Button b) deriving (Eq)
instance Show (Spreadsheet b) where
    show (Spreadsheet id res b) = "(Spreadsheet " ++ id ++ ")"
instance Alternable (Spreadsheet b)
  
    
instance Renderable (Spreadsheet b) where
    references (Spreadsheet id res b) = return []
    identifiers (Spreadsheet id res b) = return [id]
        
    directives (Spreadsheet id res b) = return []
    template (Spreadsheet id res@(Resource name url attrs) (Button _ (Expression expr))) 
        = do let q = resource res
             let (rel,(_,pq)) = runState q (0,EmptyQuery)
             let tableInput attr = do
                 let (Expression expr) = rel ! attr
                 expr' <- generateViewExpr True expr
                 return $ text "+ '<td><input type=\"text\" class=\"arbor-input arbor-border\" ng-model=\"" <> expr'
                  <> text "\"" <> text "</td>'" 
             expr' <- generateViewExpr True expr
             let headers = vcat $ map (\attr -> text "+ '<th>" <> text attr <> text "</th>'") attrs
             inputs <- sequence (map tableInput attrs)
             return $ text "+ '<form name=\"" <> text id <> text "\" ng-submit=\"" <> text id <> text "Submit()\">'"
                   $$ nest 4 (text "+ '<table>'"
                   $$ nest 4 (text "+ '<tr>'"
                   $$ nest 4 headers
                   $$ text "+ '</tr>'"
                   $$ text "+ '<tr ng-repeat=\"obj in" <+> text id <> text"Source\">'"
                   $$ nest 4 (vcat inputs)
                   $$ text "+ '</tr>'")
                   $$ text "+ '</table>'")
                   $$ text "+ '<input type=\"Submit\" class=\"arbor-btn\" value=\"" <> expr' <> text "\"/>'"
                   $$ text "+ '</form>'"
                   
    controller (Spreadsheet id res b)
        = do let q = resource res
             let (rel,(_,pq)) = runState q (0,EmptyQuery)
             q' <- generateQuery id pq
             return $ q' <> semi
                   $$ text "$scope." <> text id <> text "Submit = function() {"
                   $$ nest 4 (text "if($scope." <> text id <> text ".$valid)"
                   $$ let id = identifier b in nest 4 (text "$scope." <> text id <> text "Click();")
                   $$ text "else"
                   $$ nest 4 (text "console.log('Unable to save. Validation error!');"))
                   $$ text "};"
    
    toElementList e = [Element e]
spreadsheet :: Resource -> (Button b) -> SElement (Spreadsheet b)

spreadsheet r b = do i <- nextIndex
                     return $ Spreadsheet ("spreadsheet" ++ show i) r b


-- instance Filterable (Spreadsheet b) where
    -- item (Spreadsheet id res b) = ItemLeaf id
instance Filterable (Spreadsheet b) where
    contains i s = False
instance  Presentable (Spreadsheet b) where
    present (Spreadsheet id res b) = do
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
              
              
              
              