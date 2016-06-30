module Language.Elements.Navbar where
import Language.Commons
import Language.Elements.Link
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


data Navbar = Navbar Identifier [Link] deriving (Eq)
instance Show Navbar where
    show (Navbar id _) = "(Navbar " ++ id ++ ")"
instance Alternable Navbar
  
    
instance Renderable Navbar where
    references (Navbar id l) = return []
                                                                
    identifiers (Navbar id _) = return [id]
    
    directives _  = return []
    template (Navbar id bs)
        = do bs' <- sequence (map renderButton bs)
             classes <- fmap getClasses ask
             return $ text "+ '<nav id=\"" <> text id <> text "\" class=\"navbar navbar-inverse" <+> hsep (map text classes) <> text "\">'"
                   $$ text "+ '<div class=\"container-fluid\">'"
                   $$ nest 4 (text "+ '<ul class=\"nav navbar-nav\">'"
                   $$ nest 4 (vcat bs')
                   $$ text "+ '</ul>'")
                   $$ text "+ '</nav>'"
                   where renderButton b 
                            = do b' <- template b
                                 return $ text "+ '<li>'"
                                       $$ nest 4 b' 
                                       $$ text "+ '</li>'"
                                 -- return $ b' 
                                 -- return $ text "+ '<li>" <> b' <> text "</li>'"
    controller (Navbar id bs)
        = do bs' <- sequence (map controller bs)
             return $ vcat bs'
    toElementList e = [Element e]

navbar :: [Link] -> SElement Navbar 

navbar bs = do pref <- fmap first3 get
               i <- nextIndex
               return $ Navbar (pref ++ "navbar" ++ show i) bs

-- instance Filterable Navbar where
    -- item (Navbar id _) = ItemLeaf id
instance Filterable Navbar where
    contains i (Navbar _ bs) = any (contains i) bs

instance Presentable Navbar where
    present (Navbar id bs) = do
        bs' <- sequence (map present bs)
        return $ text "Goto" <> (parens . doubleQuotes) (text id)
              $$ text "Say" <> (parens . doubleQuotes) (text "this is " <> text id)
              $$ text "; Sleep, 1000"
              $$ vcat bs'
              
              
              
              