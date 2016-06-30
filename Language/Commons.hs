{-# LANGUAGE ExistentialQuantification #-}
module Language.Commons where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Text.PrettyPrint
import Text.Printf
import Data.List
import System.FilePath
import System.Directory
import Data.Char

import qualified Data.Map as Map


data Model = Singleton Identifier
           | Array Model
           | Cat Model Model
           deriving (Eq, Ord, Show)
arrayfy :: Model -> Model
arrayfy (Cat m n) = Cat (arrayfy m) (arrayfy n)
arrayfy m = Array m


type Identifier = String
type Parameter = String
type Name = String
-- type Label = String
type Object = [(Name,Identifier)]
-- instance Show Object where
    -- show (Object obj) = "{" ++ intercalate "," (map (\(s,Singleton id) -> show s ++ " : " ++ id) obj) ++ "}"


type Attribute  = String

type Path = [Name]
-- type Value = Maybe String
-- type Environment = (Path,Validator)


capitalize :: String -> String
capitalize [] = []
capitalize (h:t) = toUpper h : t

upperCase :: String -> String
upperCase  = map toUpper

lowerCase :: String -> String
lowerCase  = map toLower

data OutputFile = OutputFile FilePath   -- file path
                             Doc        -- contained document 
    deriving (Eq,Show)
saveFile :: FilePath -> OutputFile -> IO ()
saveFile baseDirectory (OutputFile relativeFilePath sourceCode) = 
    let fileName = takeFileName relativeFilePath 
        targetDirectory = baseDirectory </> (takeDirectory relativeFilePath)
        absoluteFileName = targetDirectory </> fileName in
            do { createDirectoryIfMissing True targetDirectory
               ; writeFile absoluteFileName (render sourceCode)
               }

catBy :: (Doc -> Doc -> Doc) -> Doc -> [Doc] -> Doc
catBy _ _ [] = empty
catBy _ _ [d] = d
catBy op s (d:ds) = (d <> s) `op` (catBy op s ds)
               
               
vcatBy :: Doc -> [Doc] -> Doc
vcatBy _ [] = empty
vcatBy _ [d] = d
vcatBy s (d:ds) = d <> s 
               $$ vcatBy s ds
vsep :: Doc -> [Doc] -> Doc
vsep _ [] = empty
vsep s (d:ds) = d $$ s $$ vsep s ds

vcommacat :: [Doc] -> Doc
vcommacat [] = empty
vcommacat [d] = d
vcommacat (d:ds) 
    | isEmpty d = vcommacat ds
    | otherwise = d $$ nest (-2) (comma <+> vcommacat ds)

type Regex = String
data Validator = EmptyValidator
               | MinLen Int
               | MaxLen Int
               | Required
               | Pattern Regex
               | Chain Validator Validator
               deriving (Eq,Show)
(<%>) :: Validator -> Validator -> Validator
v <%> w = Chain v w

first3 :: (a, b, c) -> a  
first3 (x, _, _) = x  
  
second3 :: (a, b, c) -> b  
second3 (_, y, _) = y  
  
third3 :: (a, b, c) -> c  
third3 (_, _, z) = z


first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z

-- fourth :: (a, b, c) -> d
-- fourth (_, _, _, q) = q

class Alternable a

class Modifiable a
class Modifiable a => Validable a
-- class Validable a where
    -- validator :: a -> [Validator]

-- class Modellable a where
    -- model :: a -> Model
    
class Identifiable a where
    identifier :: a -> Identifier

class Submittable a where
    submission :: a -> Object
