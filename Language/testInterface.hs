module Language.TestInterface where
import Language
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

----------------------------------------------------------
-- test
----------------------------------------------------------


countries = Table "countries" ["id","name"]
states = Table "states" ["id","name","countryId"]
cities = Table "cities" ["id","name","stateId"]
sindata = Table "sin" ["x","y"]

initCountries = do
    c <- into countries 
    insert $ "id" *=* (string "c1") : "name" *=* (string "country1") : []
    insert $ "id" *=* (string "c2") : "name" *=* (string "country2") : []
    insert $ "id" *=* (string "c3") : "name" *=* (string "country3") : []
    insert $ "id" *=* (string "c4") : "name" *=* (string "country4") : []
    -- remove (c ! "id" *==* (string "c3"))
    
initStates = do
    into states
    insert $ "id" *=* (string "s11") : "name" *=* (string "state11") : "countryId" *=* (string "c1") : []
    insert $ "id" *=* (string "s12") : "name" *=* (string "state12") : "countryId" *=* (string "c1") : []
    insert $ "id" *=* (string "s13") : "name" *=* (string "state13") : "countryId" *=* (string "c1") : []
    insert $ "id" *=* (string "s21") : "name" *=* (string "state21") : "countryId" *=* (string "c2") : []
    insert $ "id" *=* (string "s22") : "name" *=* (string "state22") : "countryId" *=* (string "c2") : []
    insert $ "id" *=* (string "s23") : "name" *=* (string "state23") : "countryId" *=* (string "c2") : []
    insert $ "id" *=* (string "s31") : "name" *=* (string "state31") : "countryId" *=* (string "c3") : []
    insert $ "id" *=* (string "s32") : "name" *=* (string "state32") : "countryId" *=* (string "c3") : []
    insert $ "id" *=* (string "s33") : "name" *=* (string "state33") : "countryId" *=* (string "c3") : []
    insert $ "id" *=* (string "s41") : "name" *=* (string "state41") : "countryId" *=* (string "c4") : []
    insert $ "id" *=* (string "s42") : "name" *=* (string "state42") : "countryId" *=* (string "c4") : []
    insert $ "id" *=* (string "s43") : "name" *=* (string "state43") : "countryId" *=* (string "c4") : []
                    
initCities = do
    into cities
    insert $ "id" *=* (string "y111") : "name" *=* (string "city111") : "stateId" *=* (string "s11") : []
    insert $ "id" *=* (string "y112") : "name" *=* (string "city112") : "stateId" *=* (string "s11") : []
    insert $ "id" *=* (string "y113") : "name" *=* (string "city113") : "stateId" *=* (string "s11") : []
    insert $ "id" *=* (string "y114") : "name" *=* (string "city114") : "stateId" *=* (string "s11") : []
    insert $ "id" *=* (string "y121") : "name" *=* (string "city121") : "stateId" *=* (string "s12") : []
    insert $ "id" *=* (string "y122") : "name" *=* (string "city122") : "stateId" *=* (string "s12") : []
    insert $ "id" *=* (string "y123") : "name" *=* (string "city123") : "stateId" *=* (string "s12") : []
    insert $ "id" *=* (string "y124") : "name" *=* (string "city124") : "stateId" *=* (string "s12") : []
                    
                    
allCountries :: Query Rel
allCountries = do c <- table countries
                  project [ (c ! "name") *@* "countryName"
                          , (c ! "id")   *@* "countryId"
                          ]
someCountries :: Query Rel
someCountries = do c <- table countries
                   restrict (c ! "id" *==* (string "c3"))
                   project [ (c ! "name") *@* "countryName"
                           , (c ! "id")   *@* "countryId"
                           ]
                   
-- statesFromCountry :: Identifier -> Query Rel
statesFromCountry c = do s <- table states
                         restrict (s ! "countryId" *==* (c))
                         project [ (s ! "name") *@* "stateName"
                                 , (s ! "id")   *@* "stateId"
                                 ]
-- citiesFromState :: Identifier -> Query Rel
citiesFromState s = do y <- table cities
                       restrict (y ! "stateId" *==* (s))
                       project [ (y ! "name") *@* "cityName"
                               , (y ! "id")   *@* "cityId"
                               ]

customers = Resource "customers" (stat "http://localhost:8080/backend/customers") ["id","name","surname"]
testCustomers :: Query Rel
testCustomers = do x <- resource customers
                   return x
customersFromId id = do c <- resource customers
                        restrict (c ! "id" *<* (param id))
login = do 
    a <- parameter "ip"
    b <- parameter "parb"
    iml  <- image (string "This is the login screen")
    im  <- image (param a) -- "This is the login screen. You are logging from "
    userid  <- image (string "userId")
    passwd  <- image (string "password")
    ok      <- button (string "login")
    fail    <- button (string "fail")
    failureTrans <- transition fail Click EmptyAction (seg "failure" [])
    homeTrans <- transition ok Click EmptyAction (seg "home" [] </> (seg "navbar1" [] <&> seg "welcome" ["p" <=> string "hello"]))
    return $ a <\>  b <\>  iml
                     <-> im
                     <-> userid
                     <-> passwd
                     <-> Widget 1 ok <|> Widget 10 fail 

failure = do 
    im  <- image (string "This is the failure screen.")
    lb  <- button (string "go back to login")
    loginTrans <- transition lb Click EmptyAction (seg "login" ["ip" <=> string "123123123","parb" <=> string "qwerty"] )
    return $ im
         <-> lb

welcome = do
    p <- parameter "p"
    let showCountries rel = do 
        par <- image (param p)
        mod <- input "" (rel ! "countryId")
        name <- input "" (rel ! "countryName")
        let showStates rr = do 
            pp <- image (param p)
            mm <- input "" (rr ! "stateId")
            nn <- input "" (rr ! "stateName")
            let bindCities rrr = (rrr ! "cityId",rrr ! "cityName")
            let bindCustomers rrr = (rrr ! "id",rrr ! "name")
            let showCities rrr = do
                ppp <- image(param p)
                mmm <- input "" (rrr ! "cityId")
                nnn <- input "" (rrr ! "cityName")
                return  $ ppp <-> mmm <-> nnn
            lll <- list (citiesFromState (value mm)) showCities
            ddd <- dropdown (citiesFromState (value mm)) bindCities
            dddl <- label (value ddd)
            -- ddc <- dropdown testCustomers bindCustomers
            -- ddcl <- label (value ddc)
            -- return $ mm <|> nn <|> lll
            return $ mm  --- <|> nn
                 <-> (ddd <|> dddl) 
                 -- <-> (ddc <|> ddcl) 
        ll <- list (statesFromCountry (value mod)) showStates
        return $ {-Widget 1 par <|>-} Widget 1 mod <|> Widget 2 name <|> Widget 10 ll -- traceShow (show par ++ "      " ++ show mod ++ "         " ++ show name) $ 
    im  <- image (string "This is the welcome screen.")
    l   <- list allCountries showCountries
    lab <- label (string "input:")
    inp <- input "" (string "hhh")
    sd  <- staticDropdown [string "c1",string "c2",string "c3",string "c4"]
    db  <- button (string "Submit and goto dash/nb1 screen")
    lb  <- button (string "logout")
    dashTrans <- transition db Click EmptyAction (seg "home" [] </> (seg "navbar1" [] <&> seg "dashboard" []))
    logoutTrans <- transition lb Click EmptyAction (seg "login" ["ip" <=> value inp,"parb" <=> string "qwerty"])
    let view =  im         
            <-> l
            <-> lab <|> inp
            <-> db
            <-> sd
            <-> lb
    return $ p <\> view
-- evalState (showNames (evalState allCountries (0,EmptyQuery))) (0,Map.empty)

            
          
dashboard = do
    im  <- image (string "This is the dashboard screen.")
    wb  <- button (string "goto welc/nb1 screen")
    lab <- label (string "customer name: ")
    custName <- input "" (string "")
    sub <- button (string "create")
    let showCustomers rel = do idc <- image (rel ! "id")
                               namec <- image (rel ! "name")
                               surnamec <- image (rel ! "surname")
                               go <- button (string "modify")
                               goTrans <- transition go Click EmptyAction (seg "home" [] </> (seg "navbar1" [] <&> seg "register" []))
                               return $ idc <|> namec <|> surnamec <|> go
    l <- list testCustomers showCustomers
    -- testTrans <- transition sub Click (create custName http://localhost:8080/backend/customers") (seg "home" [] </> (seg "navbar1" [] <&> seg "dashboard" []))
    welcTrans <- transition wb Click EmptyAction (seg "home" [] </> (seg "navbar1" [] <&> seg "welcome" ["p" <=> string "hello"]))
    let view = im
           <-> wb
           <-> lab <|> custName <|> sub
           <-> l
    return view
navbar1 = do
    im  <- image (string "This is the navbar1 screen.")
    wb <- link "goto welcome" (seg "home" [] </> (seg "navbar1" [] <&> seg "welcome" ["p" <=> string "hello"]))
    db <- link "goto dashboard" (seg "home" [] </> (seg "navbar1" [] <&> seg "dashboard" []))
    rb <- link "goto register" (seg "home" [] </> (seg "navbar1" [] <&> seg "register" []))
    sb <- link "goto spreadsheet" (seg "home" [] </> (seg "navbar1" [] <&> seg "spreadsheet" []))
    mb <- link "goto testput" (seg "home" [] </> (seg "navbar1" [] <&> seg "testput" ["id" <=> string "4"]))
    tb <- link "goto tref/nb1" (seg "home" [] </> (seg "navbar1" [] <&> (seg "testref" [] </> seg "l1" [])))
    -- cb <- link "goto tref/nb1" (seg "home" [] </> (seg "navbar1" [] <&> seg "testchart" []))
    cb <- link "goto testcommand" (seg "home" [] </> (seg "navbar1" [] <&> seg "testcommand" []))
    n2 <- link "goto navbar2" (seg "home" [] </> seg "navbar2" [])
    nb <- navbar [wb,db,rb,sb,mb,tb,cb,n2]
    let view = im
           <-> nb
    return view
navbar2 = do
    im  <- image (string "This is the navbar2 screen.")
    n1 <- link "goto navbar1" (seg "home" [] </> seg "navbar1" [])
    nb <- navbar [n1]
    let view = im
           <-> nb
    return view
   
registerBody = do
    i1 <- formInput "id" (MinLen 2) (string "id")
    i2 <- formInput "name" (MinLen 2) (string "forminput")
    i3 <- formInput "surname" (MinLen 2) (string "surname") 
    return $ i2 <|> i3

register = do
    let drop rel = (rel ! "countryId", rel ! "countryName")
    dd <- dropdown allCountries drop
    l <- label (fromIdentifier (identifier dd))
    lcb <- label (string "check me to enable form")
    cb <- checkbox
    fb <- registerBody
    sub <- button (string "create")
    f <- plainForm fb sub
    w <- enable ((value cb) *==* (string "true")) (Element f)
    subTrans <- transition sub Click (create f (stat "http://localhost:8080/backend/customers")) (seg "home" [] </> (seg "navbar1" [] <&> seg "register" []))
    return $ dd <|> l
         <-> lcb <|> cb 
         <-> f

spread = do
    but <- button (string "upload")
    -- sb <- spreadsheet customers but
    return $ but

testput = do
    id <- parameter "id"
    let modbody rel = do iid <-      formInput "id" EmptyValidator (rel ! "id")
                         iname <-    formInput "name" (MinLen 2) (rel ! "name")
                         isurname <- formInput "surname" (MinLen 2) (rel ! "surname")
                         lid <- label (string "id")
                         lname <- label (string "name")
                         lsurname <- label (string "surname")
                         return $ iid <|> iname <|> isurname
    
    sub <- button (string "patch")
    f <- queryForm (customersFromId id) modbody sub
    subTrans <- transition sub Click (patch f (stat"http://localhost:8080/backend/customers")) (seg "home" [] </> (seg "navbar1" [] <&> seg "testput" ["id" <=> string "4"]))
    return $ id <\> f
    
testref = do
    inp <- input "" (string "1")
    b1 <- button (string "view l1")
    b2 <- button (string "view l2")
    l <- label (string "label")
    l1 <- label (value inp)
    l2 <- label (value inp)
    trans1 <- transition b1 Click EmptyAction (seg "home" [] </> (seg "navbar1" [] <&> (seg "testref" [] </> seg "l1" [])))
    trans2 <- transition b2 Click EmptyAction (seg "home" [] </> (seg "navbar1" [] <&> (seg "testref" [] </> seg "l2" [])))
    let alt = Alternative [ ("l1",Element (l <|> inp <|> l1))
                           , ("l2",Element (l <|> inp <|> l2))
                           ]
    return $ (b1 <-> b2) <|> (inp <-> alt)

-- getSin = do table sindata
-- testchart = do
    -- let plotSin r = (r ! "x",r ! "y") 
    -- ch <- chart getSin plotSin
    -- return ch
    
testcommand = do
    
    let showCountries rel = do idc <- input "" (rel ! "countryId")
                               namec <- input "" (rel ! "countryName")
                               go <- button (string "modify")
                               let insertCountry = do
                                    c <- into countries
                                    update (c!"id" *==* (value idc)) ("id" *=* (value idc) : "name" *=* (value namec) : [])
                               goTrans <- transition go click (command insertCountry) (seg "home" [] </> (seg "navbar1" [] <&> seg "testcommand" []))
                               return $ idc <|> namec <|> go
    l <- list allCountries showCountries
    return l

 
home = do 
    im  <- image (string "This is the home screen.")
    n1 <- navbar1
    n2 <- navbar2
    n <- alternative [ n1 <@> "navbar1"
                     , n2 <@> "navbar2"]
    w <- welcome
    d <- dashboard
    f <- register
    s <- spread
    t <- testref
    m <- testput
    -- c <- testchart
    c <- testcommand
    v <- alternative [ w <@> "welcome"
                     , d <@> "dashboard"
                     , f <@> "register"
                     , s <@> "spreadsheet"
                     , t <@> "testref"
                     , c <@> "testcommand"
                     , m <@> "testput"]
                     -- , c <@> "testchart"]
    let view = im 
           <-> n 
           <-> v
    return view

model = [initCountries,initStates,initCities]
interface = do
    im  <- image (string "This is the interface.")
    l <- login
    f <- failure
    h <- home
    v <- alternative [ l <@> "login"
                     , f <@> "failure"
                     , h <@> "home"]
    return v
    