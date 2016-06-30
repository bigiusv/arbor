module Language.Sequence where
import Language.Commons
import Language.Expression
import Language.Query
-- import Language.Filter
-- import Language.Element
import Language.Environment
import Language.Transition
import Language.Utils
-- import Language.Element
-- import Language.Filter
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Text.PrettyPrint
import Text.Printf
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map
import Debug.Trace
data SequenceType   = Above 
                    | Beside
                    deriving (Eq,Show)

instance Alternable (Sequence a b)

-- instance (Modifiable a, Modifiable b) => Modifiable (Sequence a b)

-- instance (Validable a, Validable b) => Validable (Sequence a b)
instance (Submittable a, Submittable b) => Submittable (Sequence a b) where
    submission (Sequence _ a b) = submission a ++ submission b
 -- where
    -- validator (Sequence _ e f) = validator e ++ validator f
                    
data Sequence a b   = Sequence SequenceType a b deriving (Eq)
instance (Show a, Show b) => Show (Sequence a b) where
    show (Sequence _ a b) = "(Sequence " ++ show a ++ " " ++ show b ++ ")"


-- instance (Modellable a, Modellable b) => Modellable (Sequence a b) where
    -- model (Sequence _ ma mb) = Cat (model ma) (model mb)
    
infixl 6 <|>
infixl 5 <->

(<|>) :: (Renderable a, Renderable b) => a -> b -> Sequence a b
a <|> b = Sequence Beside a b

(<->) :: (Renderable a, Renderable b) => a -> b -> Sequence a b
a <-> b = Sequence Above a b
-- instance (Renderable a, Renderable b, Show a, Show b, Presentable a, Presentable b) => Filterable (Sequence a b) where
    -- getChildren (Sequence _ a b) =  [Element a, Element b]
-- instance (Filterable a, Filterable b) => Filterable (Sequence a b) where
    -- item (Sequence _ a b) = ItemSequence (item a) (item b)
    
instance (Renderable a, Renderable b, 
          Eq a, Eq b,
          Show a, Show b,
          Presentable a, Presentable b,
          Filterable a, Filterable b) => Filterable (Sequence a b) where
    children (Sequence _ a b) = [Element a,Element b]
    contains i (Sequence _ a b) = contains i a || contains i b

instance (Renderable a, Renderable b) => Renderable (Sequence a b) where
    references (Sequence _ a b) = pure (++) <*> (references a) <*> (references b)
    identifiers (Sequence _ a b) = pure (++) <*> (identifiers a) <*> (identifiers b)
    check s@(Sequence _ a b) = do 
        a' <- check a 
        b' <- check b
        ids <- identifiers s
        refs <- references s
        let test = all (\i -> i `elem` ids) refs
        return $ a' && b' && test

    directives (Sequence _ a b) = 
        do  dira <- directives a
            dirb <- directives b
            return $ dira ++ dirb
    
    template (Sequence Above f g)
        = do f' <- template f 
             g' <- template g
             return $ text "+ '<div class=\"\">'"
                   $$ nest 4 f'
                   $$ text "+ '</div>'"
                   $$ text "+ '<div class=\"\">'"
                   $$ nest 4 g'
                   $$ text "+ '</div>'"
    template (Sequence Beside f g)
        = do f' <- template f 
             g' <- template g
             let df = width f
             let dg = width g
             let d = df + dg
             return $ text "+ '<div class=\"row\">'"
                   -- $$ nest 4 (text (printf "+ '<div class=\"arbor-col\" style=\"width:%2.f%%;\">'" (100.0 * (fromIntegral df :: Double) / (fromIntegral d :: Double))) $$ nest 4 f' $$ text "+ '</div>'")
                   -- $$ nest 4 (text (printf "+ '<div class=\"arbor-col\" style=\"width:%2.f%%;\">'" (100.0 * (fromIntegral dg :: Double) / (fromIntegral d :: Double))) $$ nest 4 g' $$ text "+ '</div>'")
                   $$ nest 4 (text (printf "+ '<div class=\"col-md-%d\">'" (round (12.0 * (fromIntegral df :: Double) / (fromIntegral d :: Double)) :: Int)) $$ nest 4 f' $$ text "+ '</div>'")
                   $$ nest 4 (text (printf "+ '<div class=\"col-md-%d\">'" (round (12.0 * (fromIntegral dg :: Double) / (fromIntegral d :: Double)) :: Int)) $$ nest 4 g' $$ text "+ '</div>'")
                   $$ text "+ '</div>'"
                               
    controller (Sequence _ f g) = 
        do  f' <- controller f
            g' <- controller g
            return (f' $$ g')
            
    width (Sequence _ f g) = width f + width g
    modules (Sequence _ f g) =  modules f ++ modules g
    toElementList (Sequence _ f g) = toElementList f ++ toElementList g

    schema (Sequence _ f g) = do
        elems <- sequence (map (\(Element e) -> schema e) (toElementList f ++ toElementList g))
        return $ vcommacat elems

instance (Presentable a, Presentable b) => Presentable (Sequence a b) where
    present (Sequence _ a b) = do  
        pa <- present a
        pb <- present b
        return $ pa
              $$ pb