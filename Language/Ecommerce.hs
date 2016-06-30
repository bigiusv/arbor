module Language.Ecommerce where
import Language
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Debug.Trace
productTable = Table "products" ["id","name","brand","price"]
cartTable = Table "cart" ["name","price"]
cartItems = do
    c <- table cartTable
    project ["name" *@old* "name","price" *@old* "price"]

searchProductTable :: Identifier -> Query Rel
searchProductTable c = do s <- table productTable
                          restrict (s ! "name" *==* (param c))
                          project ["id" *@old* "id","name" *@old* "name","brand" *@old* "brand","price" *@old* "price"]

initProductTable = do
    into productTable
    insert $ "id" *=* (string "1") : "name" *=* (string "p1") : "brand" *=* (string "b1") : "price" *=* (string "100") : []
    insert $ "id" *=* (string "2") : "name" *=* (string "p2") : "brand" *=* (string "b2") : "price" *=* (string "200") : []
    insert $ "id" *=* (string "3") : "name" *=* (string "p3") : "brand" *=* (string "b3") : "price" *=* (string "300") : []
    insert $ "id" *=* (string "4") : "name" *=* (string "p4") : "brand" *=* (string "b4") : "price" *=* (string "400") : []

initCartTable = do
    into cartTable

-- center :: (Renderable a, Renderable b) => a -> Int -> b
-- center :: (Renderable a) => a -> Int -> Sequence (Sequence (Widget EmptyElement) (Widget a)) (Widget EmptyElement)
center el w = 
    let pad = round $ (fromIntegral (100 - w) / 2.0)
        fill = EmptyElement
    in (fill <#> pad) <|> (el <#> w) <|> (fill <#> pad)
    
model = [initProductTable,initCartTable]
interface = do
    l <- login -- <?> "Here the user inputs his email address"
    h <- home
    a <- alternative [ (center l 50) <@> "login"
                     , h  <@> "home"
                     ]
    return a
login = do 
    userid  <- input (string "user id") -- <?> "Here the user inputs his email address"
    passwd  <- input (string "password") -- <?> "Here the user inputs his password"
    ok      <- button (string "login") -- <?> "Pressing this button brings the user to the home page"
    transition ok click none (seg "home" [] </> (seg "navbarstd" [] <&> seg "welcome" []))
    return $    userid
            <-> passwd
            <-> (ok <:> ["arbor-red","arbor-round-xxlarge"])
home = do 
    ns <- navbarstd
    ne <- navbarstd
    w <- welcome
    p <- results
    c <- cart
    a1 <- alternative [ ns <@> "navbarstd"
                      , ne <@> "navbarexp"]
    a2 <- alternative [ w <@> "welcome"
                      , p <@> "results"
                      , c <@> "cart"]
    
    return $ (a1 <#> 1 <:> ["arbor-red"]) <|> (a2 <:> ["arbor-green"] <#> 3)
    
navbarstd = do
    wb <- link "welcome" (seg "home" [] </> (seg "navbarstd" [] <&> seg "welcome" []))
    cb <- link "cart" (seg "home" [] </> (seg "navbarstd" [] <&> seg "cart" []))
    lb <- link "logout" (seg "login" [])
    nb <- navbar [wb,cb,lb]
    return nb

welcome = do 
    lab <- label (string "Filter product")
    inp <- input (string "")
    btn <- button (string "go")
    transition btn click none (seg "home" [] </> (seg "navbarstd" [] <&> seg "results" [("search",value inp)]))
    return $    lab <|> inp
            <-> btn
           
results = do
    par <- parameter "search"
    lab <- label (string "Filter results")
    let showProduct prod = do
            ln <- input (prod ! "name")
            lb <- input (prod ! "brand")
            lp <- input (prod ! "price")
            b  <- button (string "buy it")
            let addToCart = do
                into cartTable
                insert $ ("name" *=* value ln) : ("price" *=* value lp) : []
            transition b click (command addToCart) (seg "home" [] </> (seg "navbarstd" [] <&> seg "cart" []))
            return ( ln <|> lb <|> lp <|> b )
    prodList <- list (searchProductTable par) showProduct
    return $ par <\> lab
                 <-> prodList
                                
cart = do
    lab <- label (string "your cart")
    let showItem prod = do
            ln <- label (prod ! "name")
            lp <- label (prod ! "price")
            return $ ln <|> lp
    items <- list cartItems showItem
    let sumPrices = do
        c <- table cartTable
        add (c!"price")
    disp <- aggregate sumPrices
    total <- label (string "total")
    return $ lab
         <-> items
         <-> total <|> disp
