module Server.Resource where
type Name = String

data Value = Empty
           | Bool   Name
           | Number Name
           | String Name
           | Array  Value
           | Object [(Name,Value)]
           deriving (Eq,Show)
          
data Relation = Relation Name Item deriving (Eq,Show)

data Item = Item Name [Value] [Relation] [Action] deriving (Eq,Show)
                
data Collection = Collection Name Item [Action] deriving (Eq,Show)


data Interface = Interface Name [Collection] deriving (Eq,Show)
collections :: Interface -> [Collection]
collections (Interface _ cols) = cols
item :: Collection -> Item
item (Collection _ item _) = item

data Action = Get
            | Put
            | Post
            | Delete
            deriving (Eq,Show)
infixr 2 .@. 
infixr 2 .<>. 
infixr 2 .<>@. 
-- infix 3 .+. 
infix 6 --> 

(.@.) :: Name -> ([Value],[Relation],[Action]) -> Item
n .@. (v,r,a) = Item n v r a

-- (.+.) :: [Value] -> [Relation] -> ([Value],[Relation])
-- vs .+. rs = (vs,rs)

(-->) :: Name -> Item -> Relation
n --> r = Relation n r

(.<>.) :: Name -> (Item,[Action]) -> Collection
n .<>. (r,a) = Collection n r a

(.<>@.) :: Name -> [Collection] -> Interface
n .<>@. c = Interface n c




