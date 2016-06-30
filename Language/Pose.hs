-- {-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Pose where
-- import Language.Commons
-- -- import Language.Filter
-- import Language.Expression
-- import Language.Environment
-- import Language.Utils
-- import Text.PrettyPrint

-- import Data.List
-- data Binding = Empty
-- data Void = Void deriving (Eq,Show)
-- data Chunk = Chunk Name [(String,Expression String)] deriving (Eq,Show)
-- data Nest s p = Nest s p deriving (Eq,Show)
-- data Tuple p q = Tuple p q deriving (Eq,Show)
    
-- class Posable p where
    -- generatePose :: p -> InterpM Doc 
    -- toList :: p -> [Pose]
    -- toFilter :: p -> Filter
    
-- data Pose = forall a. (Posable a) => Pose a
-- instance Posable Void where
    -- generatePose Void = return empty
    -- toList Void = []
    -- toFilter Void = ident
-- instance Posable Chunk where
    -- generatePose (Chunk s pars) = do 
        -- pars' <- sequence (map (\(k,Expression v) -> do v' <- generateCtrlExpr v
                                                        -- return $ text k <> equals <> v') pars)
        -- return $ text s <> braces (hcat $ punctuate comma pars')
    -- toList s = [Pose s]
    -- toFilter (Chunk s pars) = go s (map fst pars)
-- instance (Posable s, Posable p) => Posable (Nest s p) where
    -- generatePose (Nest s p) = do 
        -- s' <- generatePose s 
        -- p' <- generatePose p
        -- return $ s' <> text "/" <> p'
    -- toList p = [Pose p]
    -- toFilter (Nest s p) = toFilter s >>> toFilter p
-- instance (Posable p, Posable q) => Posable (Tuple p q) where
    -- generatePose (Tuple p q) = do 
        -- poses <- sequence (map (\(Pose a) -> generatePose a) (toList p ++ toList q))
        -- return $ parens (hcat $ punctuate comma poses)
    -- toList (Tuple p q) = toList p ++ toList q
    -- -- toFilter (Tuple p q) = many (map (\(Pose a) -> Filter a) (toList p ++ toList q))
    -- toFilter (Tuple p q) = both (toFilter p) (toFilter q)

-- (</>) :: (Posable p) => Chunk -> p -> Nest Chunk p
-- s </> p = Nest s p

-- (<&>) :: (Posable p, Posable q) => p -> q -> Tuple p q
-- p <&> q = Tuple p q

-- seg :: String -> [(String,Expression String)] -> Chunk
-- seg s pars = Chunk s pars
