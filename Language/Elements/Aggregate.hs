module Language.Elements.Aggregate where
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
data Aggregate = Aggregate Identifier (Query ()) 

instance Show Aggregate where
    show (Aggregate id e) = "(Aggregate " ++ id ++ ")"
instance Eq Aggregate where
    Aggregate id1 _ == Aggregate id2 _ = id1 == id2

instance Renderable Aggregate where
    references (Aggregate id q) = return []
    
    identifiers (Aggregate id _) = return [id]
    
    directives _ = return []
    
    template (Aggregate id q) = do
        transMap <- fmap getTransitions ask
        transitions' <- sequence $ map generateViewTransition (fromMaybe [] $ Map.lookup id transMap)
        ctx <- fmap getContext ask
        classes <- fmap getClasses ask
        let suffix = case ctx of
                        EmptyContext -> empty
                        ListContext _ -> text "[$index]"
        return $ text "+ '<span id=\"" <> text id <> text "\" class=\"" <+> hsep (map text classes) <> text "\""
             <+> hsep transitions'
             <> text ">{{" <> text id <> suffix <> text "}}</span>'"
                 
                 
    controller (Aggregate id q) = do
        let ((),(_,pq)) = runState q (0,EmptyQuery)
        q' <-  generateQuery id pq
        return $ q'
        
    toElementList e = [Element e]

instance Alternable Aggregate  
instance Filterable Aggregate where
    contains i j = identifier i == identifier j
instance Identifiable Aggregate where
    identifier (Aggregate id e) = id

aggregate :: Query () -> SElement Aggregate
aggregate q =  do pref <- fmap first3 get
                  i <- nextIndex 
                  return $ Aggregate (pref ++ "aggregate" ++ show i) q
-- instance Filterable Aggregate where
    -- item (Aggregate id _) = ItemLeaf id
    
instance Presentable Aggregate where
    present (Aggregate id e) = do
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
              