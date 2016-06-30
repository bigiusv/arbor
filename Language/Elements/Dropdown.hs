module Language.Elements.Dropdown where
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


data StaticDropdown a = StaticDropdown Identifier [Expression a] deriving (Eq)
instance Show (StaticDropdown a) where
    show (StaticDropdown id l) = "(StaticDropdown " ++ show id ++ ")"

instance Alternable (StaticDropdown a)
instance Filterable (StaticDropdown a) where
    contains i j = identifier i == identifier j
instance Identifiable (StaticDropdown a) where
    identifier (StaticDropdown id l) = id
staticDropdown :: [Expression a] -> SElement (StaticDropdown a)
staticDropdown l = do pref <- fmap first3 get
                      i <- nextIndex
                      return $ StaticDropdown (pref ++ "dropdown" ++ show i) l

-- instance Submittable (StaticDropdown a) where
    -- submission (StaticDropdown id l) =
        -- let (rel,(_,pq)) = runState q (0,EmptyQuery)
            -- elem = evalState (f rel) (id ++ "",0,Map.empty)
        -- in  submission elem
        
instance (Renderable (StaticDropdown a)) where

    references (StaticDropdown id l) = 
        return $ foldl (\acc (Expression e) -> 
            case e of 
                (IdExpr id) -> acc ++ [id]
                _           -> acc) [] l

    identifiers (StaticDropdown id l) = return [id]
    
    directives (StaticDropdown id l) = return []
        
    template (StaticDropdown id l) = do
        classes <- fmap getClasses ask
        return $ text "+ '<select class=\"dropdown" <+> hsep (map text classes) <> text "\" ng-model=\"" <> text id <> text "\""
             <+> text "ng-options=\"k as k for k in" <+> text id <> text "Source\">'"
              $$ text "+ '</select>'"
        -- return $ text "+ '<div class=\"dropdown\">'"
              -- $$ text "+ '<button class=\"btn btn-primary dropdown-toggle\" type=\"button\" data-toggle=\"dropdown\">Please select an option'"
              -- $$ nest 4 (text "+ '<span class=\"caret\"></span></button>'"
              -- $$ text "+ '<ul class=\"dropdown-menu\">'"
              -- $$ nest 4 (text "+ '<li ng-repeat=\"obj in" <+> text id <> text "Source\">'"
              -- $$ nest 4 (text "+ '{{obj}}'")
              -- $$ text "+ '</li>'")
              -- $$ text "+ '</ul>'")
              -- $$ text "+ '</div>'"
        
    controller (StaticDropdown id l) = do
        exprs <- sequence (map (\(Expression e) -> generateCtrlExpr e) l)
        return $ text "$scope." <> text id <> text "Source" <+> equals <+> brackets (catBy (<>) (text ",") exprs) <> semi
    modules (StaticDropdown id l) = []
    toElementList e = [Element e]
-- instance (Modellable a) => Modellable (StaticDropdown a) where
    -- model (StaticDropdown id l) = 
        -- let (rel,(_,pq)) = runState q (0,EmptyQuery)
            -- elem = evalState (f rel) (0,Map.empty) in
        -- arrayfy $ model elem 
    
-- instance Filterable (StaticDropdown a) where
    -- item  (StaticDropdown id l) = ItemLeaf id

instance Presentable (StaticDropdown a) where
    present (StaticDropdown id l) = do
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"

              
                  
data Dropdown a b = Dropdown Identifier (Query Rel) (Rel -> (Expression a, Expression b)) 
instance Eq (Dropdown a b) where
    (Dropdown id1 _ _) == (Dropdown id2 _ _) = id1 == id2
instance Show (Dropdown a b) where
    show (Dropdown id q f) = "(Dropdown " ++ show id ++ ")"

instance Alternable (Dropdown a b)
instance Identifiable (Dropdown a b) where
    identifier (Dropdown id q f) = id
dropdown :: Query Rel -> (Rel -> (Expression a, Expression b)) -> SElement (Dropdown a b)
dropdown q f = do pref <- fmap first3 get
                  i <- nextIndex
                  return $ Dropdown (pref ++ "dropdown" ++ show i) q f

-- instance Submittable (Dropdown a) where
    -- submission (Dropdown id q f) =
        -- let (rel,(_,pq)) = runState q (0,EmptyQuery)
            -- elem = evalState (f rel) (id ++ "",0,Map.empty)
        -- in  submission elem
instance (Renderable (Dropdown a b)) where
    references (Dropdown id q f) = 
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
            (Expression a,Expression b) = f rel
            a' = getId a
            b' = getId b
        in return $ a' ++ b'
        where getId (IdExpr id) = [id]
              getId _ = []
    identifiers (Dropdown id q f) = return [id]
    
    directives (Dropdown id q f) = return []
        
    template (Dropdown id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        ctx <- fmap getContext ask
        classes <- fmap getClasses ask
        (bind,display) <- mapTuple (generateViewExpr True) (f rel)
        index <- case ctx of 
                    EmptyContext -> return $ empty
                    ListContext _ -> return $ brackets $ text "$index"
        let suffix = case ctx of
                        EmptyContext -> empty
                        ListContext _ -> text "[$index]"
        return $ text "+ '<select class=\"dropdown" <+> hsep (map text classes) <> text "\" ng-model=\"" <> text id <> suffix <> text "\""
             <+> text "ng-options=\"" <>  bind <+> text "as" <+> display <+> text "for obj in" <+> text id <> text "Source" <> index <> text "\">'"
              $$ text "+ '</select>'"
        where mapTuple f (Expression a, Expression b) = do a' <- f a 
                                                           b' <- f b 
                                                           return (a',b')
         
    controller (Dropdown id q f) = do
        let (rel,(_,pq)) = runState q (0,EmptyQuery)
        q' <-  generateQuery (id ++ "Source") pq
        ctx <- fmap getContext ask
        case ctx of 
            EmptyContext -> return $ q' <> semi
            ListContext _ -> return $ text "$scope.$watchCollection('context', function(newContext,oldContext){"
                                   $$ nest 4 (q' <> semi)
                                   $$ text "});"
    modules (Dropdown id q f) = []
    toElementList e = [Element e]
-- instance (Modellable a) => Modellable (Dropdown a) where
    -- model (Dropdown id q f) = 
        -- let (rel,(_,pq)) = runState q (0,EmptyQuery)
            -- elem = evalState (f rel) (0,Map.empty) in
        -- arrayfy $ model elem 
-- instance Filterable (Dropdown a b) where
    -- item  (Dropdown id q f) = ItemLeaf id

instance Filterable (Dropdown a b) where
    contains i j = identifier i == identifier j

instance Presentable (Dropdown a b) where
    present (Dropdown id q f) = do
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
