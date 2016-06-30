module Language.Demo where
import Language
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Debug.Trace
sailorsTable = Table "sailors" ["sid","sname","rating","age"]
reservesTable = Table "reserves" ["sid","bid","date"]


-- productTable = Table "products" ["id","name","brand","price"]
-- cartTable = Table "cart" ["name","price"]
-- cartItems = do
    -- c <- table cartTable
    -- project ["name" *@old* "name","price" *@old* "price"]

searchSailorsTable :: Identifier -> Query Rel
searchSailorsTable sname = do s <- table sailorsTable
                              r <- table reservesTable
                              -- restrict ((s ! "sname") *==* (param sname))
                              -- traceShow r $ traceShow s 
                              q <- restrict (((s ! "sid") *==* (r ! "sid")) *&&* ((r ! "sid") *==* (param sname)))
                              state <- get
                              return q
                              project [ (s ! "sid") *@* "sid"
                                      , (s ! "sname") *@* "name"
                                      , (r ! "bid") *@* "boat"
                                      , (s ! "age") *@* "age"
                                      , (r ! "date") *@*"date"
                                      , (s ! "rating") *@*"rating"
                                      ]
                              -- project ["sid" *@old* "id","sname" *@old* "name","boat" *@old* "bid","rating" *@old* "rating"]

initSailorsTable = do
    into sailorsTable
    insert $ "sid" *=* (string "22") : "sname" *=* (string "Dustin") : "rating" *=* (string "7") : "age" *=* (string "45") : []
    insert $ "sid" *=* (string "31") : "sname" *=* (string "Lubber") : "rating" *=* (string "8") : "age" *=* (string "55") : []
    insert $ "sid" *=* (string "58") : "sname" *=* (string "Rusty") : "rating" *=* (string "10") : "age" *=* (string "35") : []
initReservesTable = do
    into reservesTable
    insert $ "sid" *=* (string "22") : "bid" *=* (string "101") : "date" *=* (string "1") : []
    insert $ "sid" *=* (string "58") : "bid" *=* (string "103") : "date" *=* (string "2") : []
    
model = [initSailorsTable,initReservesTable]
interface = do
    l <- blank -- <?> "Here the user inputs his email address"
    h <- home
    a <- alternative [ l <@> "empty"
                     , h <@> "home"
                     ]
    return a
    
empty = do
    blank
    
home = do 
    ns <- navbarstd
    ne <- navbarstd
    w <- welcome
    p <- results
    a1 <- alternative [ ns <@> "navbarstd"
                      , ne <@> "navbarexp"
                      ]
    a2 <- alternative [ w <@> "welcome"
                      , p <@> "results"
                      ]
    
    return $ (a1 <#> 1) <|> (a2 <#> 3)
    
navbarstd = do
    wb <- link "welcome" (seg "home" [] </> (seg "navbarstd" [] <&> seg "welcome" []))
    cb <- link "empty" (seg "empty" [])
    nb <- navbar [wb,cb]
    return nb

welcome = do 
    lab <- label (string "Filter sailors")
    inp <- input (string "")
    btn <- button (string "go")
    transition btn click none (seg "home" [] </> (seg "navbarstd" [] <&> seg "results" [("search",value inp)]))
    return $    lab <|> inp
            <-> btn
           
results = do
    par <- parameter "search"
    lab <- label (string "Filter results")
    let showSailor s = do
            li <- input (s ! "sid")
            ln <- input (s ! "name")
            lr <- input (s ! "rating")
            la <- input (s ! "age")
            lb <- input (s ! "boat")
            ld <- input (s ! "date")
            return ( li <|> ln <|> lr <|> la <|> lb <|> ld)
    sList <- list (searchSailorsTable par) showSailor
    return $ par <\> lab
                 <-> sList
