module Language.Elements.EmptyElement where
import Language.Commons
import Language.Environment
-- import Language.Filter
-- import Language.Element
import Text.PrettyPrint
import Text.Printf

data EmptyElement   = EmptyElement deriving (Eq,Show)
instance Alternable (EmptyElement)
    
instance Renderable EmptyElement where
    references  EmptyElement = return []
    identifiers EmptyElement = return []
    directives  EmptyElement = return []
    template    EmptyElement = return $ text "+ '<div>&nbsp;</div>'"
    controller  EmptyElement = return empty
    toElementList EmptyElement = []
-- instance Filterable EmptyElement where
    -- item EmptyElement = ItemLeaf ""
blank :: SElement EmptyElement    
blank = return EmptyElement

instance Filterable EmptyElement where
    contains _ _ = False 
    
instance Presentable EmptyElement where
    present EmptyElement = return empty
