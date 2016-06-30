module Server.Commons where
import Text.PrettyPrint
import Text.Printf
import Data.List
import System.FilePath
import System.Directory
import Data.Char

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