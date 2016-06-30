module Language.Elements.Link where
import Language.Commons
-- import Language.Expression
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
data Link = Link Identifier Name Pose deriving (Eq)

instance Show Link where
    show (Link id n _) = "(Link " ++ id ++ " " ++ n ++ ")"
    
link :: (Posable p) => Name -> p -> SElement Link
link name p = do (pref,i,transMap) <- get
                 let pose = Pose p
                 let id = pref ++ "link" ++ show i
                 let el = Link id name pose
                 let trans = Transition el Click EmptyAction pose
                 put (pref,i+1,Map.insertWith (++) id [trans] transMap)
                 return el

instance Identifiable Link where
    identifier (Link id _ _) = id

instance Alternable Link
instance Filterable (Link) where
    contains i j = identifier i == identifier j
instance Renderable Link where
    references (Link id _ _) = return []
    
    identifiers (Link id _ _) = return [id]
    
    directives _ = return []
    
    template (Link id name pose) = do
        transMap <- fmap getTransitions ask
        classes <- fmap getClasses ask
        transitions' <- sequence $ map generateViewTransition (fromMaybe [] $ Map.lookup id transMap)
        return $    text "+ '<a id=\"" <> text id <> text "\"class=\"" <+> hsep (map text classes) <> text "\""
                <+> hsep transitions'
                 <> text ">" <> text name <> text "</a>'"
    controller (Link id name pose) = -- return empty
        do  transMap <- fmap getTransitions ask
            transitions' <- {-traceShow (fromMaybe [] $ Map.lookup id transMap) $-} sequence $ map generateCtrlTransition (fromMaybe [] $ Map.lookup id transMap)
            return $ vcat transitions'
    toElementList e = [Element e]
-- instance Filterable Link where
    -- item (Link id _ _) = ItemLeaf id
    
instance Presentable Link where
    present (Link id name pose) = do
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text name)
              $$ text "; Sleep, 1000"