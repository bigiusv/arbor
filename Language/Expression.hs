module Language.Expression where
import Language.Commons

data Expr   = IdExpr Identifier
            | AttrExpr Attribute
            | ParExpr Parameter
            | ConstExpr String
            | BinExpr BinaryOp Expr Expr
            | UnExpr UnaryOp Expr
          deriving (Eq,Show)
data BinaryOp   = Equals | LessThan 
                | And | Or deriving (Eq)
instance Show BinaryOp where
    show Equals     = "=="
    show LessThan   = "<"
    show And        = "&&"
    show Or         = "||"
    
data UnaryOp = Not deriving (Eq)

instance Show UnaryOp where
    show Not        = "!"
data Expression a = Expression Expr
                    deriving (Eq,Show)

fromIdentifier  :: Identifier  -> Expression String
fromIdentifier m = Expression (IdExpr m)

fromAttribute :: Attribute -> Expression String
fromAttribute attr = Expression (AttrExpr attr)

param :: Parameter -> Expression String
param p = Expression (ParExpr p)
    
string :: (Show a) => a -> Expression String
string x = Expression (ConstExpr $ sq (show x))

value :: (Identifiable a) => a -> Expression String
value = fromIdentifier . identifier

unaryOp :: UnaryOp -> Expression a -> Expression b
unaryOp op (Expression p)
    = Expression (UnExpr op p)

binaryOp :: BinaryOp -> Expression a  -> Expression b -> Expression c
binaryOp op (Expression expr1) (Expression expr2)
    = Expression (BinExpr op expr1 expr2)
                
isNull :: Identifier -> Expression Bool
isNull m = Expression (UnExpr Not (IdExpr m))
isDirty :: Identifier -> Expression Bool
isDirty m = neg (isNull m)

(*==*) :: Eq a => Expression a -> Expression a -> Expression Bool
(*==*) (Expression x) (Expression y) 
    = Expression (BinExpr Equals x y)
(*<*) :: Eq a => Expression a -> Expression a -> Expression Bool
(*<*) (Expression x) (Expression y) 
    = Expression (BinExpr LessThan x y)

(*&&*):: Expression Bool -> Expression Bool -> Expression Bool
(*&&*) = binaryOp And
(*||*):: Expression Bool -> Expression Bool -> Expression Bool
(*||*) = binaryOp Or
neg:: Expression Bool -> Expression Bool
neg = unaryOp Not

sq :: String -> String
sq s@[c]                     = s
sq ('"':s)  | last s == '"'  = init s
            | otherwise      = s
sq ('\'':s) | last s == '\'' = init s
            | otherwise      = s
sq s                         = s
