module Language.Insoluti where
import Language
--------------------- Data ---------------------
checks = Table "assegni" ["id","abi","cab","data","luogo","divisa","pid"]
persons = Table "soggetti" ["id","nome","cognome","datanasc","luogonasc","codfisc"]
requests = Table "richieste" ["id","sid","aid"]
states = Table "stati" ["id"]

allStates = do
    s <- table states
    projectAttr s ["id"]

allPersons = do
    p <- table persons
    projectAttr p ["id" ,"nome","cognome","datanasc","luogonasc","codfisc"]

allChecks = do
    c <- table checks 
    projectAttr c ["id","abi","cab","data","luogo","divisa"]
    
reqbyStatus st = do
    r <- table requests 
    restrict (r ! "sid" *==* st)
    projectAttr r ["id","sid","aid"]
           
reqByCheck c = do
    r <- table requests 
    restrict (r ! "aid" *==* c)
    projectAttr r ["id","sid","aid"]
            
getChecks id = do
    c <- table checks 
    restrict (c ! "id" *==* id)
    projectAttr c ["id","cab","abi","data","luogo","divisa"]



--------------------- Elements ---------------------
interface = do
    nav <- navbarstd
    hom <- home
    det  <- details
    sea <- search
    res <- results
    lts <- lists
    frm <- form
    a <- alternative [ hom <@> "home"
                     , det <@> "details"
                     , sea <@> "search"
                     , res <@> "results"
                     , lts <@> "lists"
                     , frm <@> "form"
                     ]
    return $ nav 
         <-> a

navbarstd = do
    hb <- link "Home" (seg "home" [])
    db <- link "Ricerca" (seg "search" [])
    lb <- link "Liste" (seg "lists" [] </> seg "checks" [])
    fb <- link "Form" (seg "form" [])
    nb <- navbar [hb,db,lb,fb]
    return nb
    
home = do
    lid <- label (string "Stato richiesta")
    lstate <- label (string "Numero")
    b <- blank
    rlist <- list allStates showState
    return $ lid <|> lstate <|> b
         <-> rlist

showState state = do
    lstate <- label (state ! "id")
    let countRequests = do
        r <- reqbyStatus (value lstate)
        count
    lnum <- aggregate countRequests
    but <- button (string "Dettagli")
    transition but click none (seg "details" ["rid" <=> value lstate])
    return $ lstate <|> lnum <|> but
    
details = do
    id <- parameter "rid"
    lid <- label (string "ID richiesta")
    lstate <- label (string "Stato")
    lch <- label (string "Assegno")
    rlist <- list (reqbyStatus (param id)) showRequest
    return $ lid <|> lstate <|> lch
         <-> rlist

search = do
    inp <- input "ID assegno:" (string "")
    but <- button (string "Cerca")
    transition but click none (seg "results" ["aid" <=> (value inp)])
    return $ inp 
         <-> but

         
results = do 
    aid <- parameter "aid"
    clist <- list (getChecks (param aid)) showCheck
    rlist <- list (reqByCheck (param aid)) showRequest
    return $ aid <\> clist
                 <-> rlist
    
showCheck c = do
    lid <- doubleLabel "ID assegno" c "id"
    labi <- doubleLabel "ABI" c "abi"
    lcab <- doubleLabel "CAB" c "cab"
    ldat <- doubleLabel "Data" c "data"
    lplc <- doubleLabel "Luogo" c "luogo"
    lcur <- doubleLabel "Divisa" c "divisa"
    return $ lid <-> labi <-> lcab <-> ldat <-> lplc <-> lcur

showRequest r = do
    lid <- label (r ! "id")
    lsid <- label (r ! "sid")
    laid <- label (r ! "aid")
    return $ lid <|> lsid <|> laid
    
lists = do
    nb <- navbarLists
    pl <- personList
    cl <- checkList
    a <- alternative [ pl <@> "persons"
                     , cl <@> "checks"
                     ]
    return $ nb <-> a


navbarLists = do
    pl <- link "Soggetti" (seg "lists" [] </> seg "persons" [])
    cl <- link "Assegni" (seg "lists" [] </> seg "checks" [])
    navbar [pl,cl]

checkList = do
    clist <- list allChecks showExtendedCheck
    return $ clist 

showExtendedCheck c = do
    hid <- label (string "id") ; lid <- label (c! "id")
    habi <- label (string "abi") ; labi <-label (c! "abi")
    hcab <- label (string "cab") ; lcab <-label (c! "cab")
    hdat <- label (string "data") ; ldat <-label (c! "data")
    hplc <- label (string "luogo") ; lplc <-label (c! "luogo")
    hcur <- label (string "divisa") ; lcur <-label (c! "divisa")
    rlist <- list (reqByCheck (value lid)) showRequest
    return $ (hid <|> habi <|> hcab <|> hdat <|> hplc <|> hcur ) 
         <-> (lid <|> labi <|> lcab <|> ldat <|> lplc <|> lcur ) 
         <-> rlist 
    
personList = do
    plist <- list allPersons showPerson
    return $ plist 


showPerson p = do
    ln <- doubleLabel "Nome" p "nome"
    lc <- doubleLabel "Cognome" p "cognome"
    ld <- doubleLabel "Data di nascita" p "datanasc"
    ll <- doubleLabel "Luogo di nascita" p "luogonasc"
    lf <- doubleLabel "Codice fiscale" p "codfisc"
    b <- blank
    return $ ln <|> lc 
         <-> ld <|> ll 
         <-> lf <|> (b <#> 2)

doubleLabel l1 c l2 = do
    l1' <- label (string l1)
    l2' <- label (c ! l2)
    return $ l1' <|> l2'

form = do
    n <- formInput  "Nome:"             "name"           Required (string "")
    c <- formInput  "Cognome:"          "surname"        Required (string "")
    d <- formInput  "Data di nascita:"  "birthdate"      Required (string "")
    l <- formInput  "Luogo di nascita:" "birthplace"     Required (string "")
    cf <- formInput "Codice fiscale:"   "fcode"         (Required <%> MinLen 16 <%> MaxLen 16) (string "")
    b <- button (string "Invio")
    f <- plainForm (    n <|> c 
                    <-> d <|> l 
                    <-> cf) b 
    let addToPersons = do
                into persons
                insert $ ("nome" *=* value n) : ("cognome" *=* value c) : ("datanasc" *=* value d) : ("luogonasc" *=* value l) : ("codfisc" *=* value cf) : []
    transition b click (command addToPersons) (seg "home" [])
    return f

    
    
    
    


    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
initStates = do
    into states
    insert $ "id" *=* (string "attesa") : []
    insert $ "id" *=* (string "lavorazione") : []
    insert $ "id" *=* (string "conclusa") : []
initRequests = do
    into requests
    insert $ "id" *=* (string "01") : "aid" *=* (string "41") : "sid" *=* (string "attesa")      : []
    insert $ "id" *=* (string "02") : "aid" *=* (string "05") : "sid" *=* (string "attesa")      : []
    insert $ "id" *=* (string "03") : "aid" *=* (string "04") : "sid" *=* (string "attesa")      : []
    insert $ "id" *=* (string "04") : "aid" *=* (string "07") : "sid" *=* (string "lavorazione") : []
    insert $ "id" *=* (string "05") : "aid" *=* (string "10") : "sid" *=* (string "lavorazione") : []
    insert $ "id" *=* (string "06") : "aid" *=* (string "06") : "sid" *=* (string "lavorazione") : []
    insert $ "id" *=* (string "07") : "aid" *=* (string "05") : "sid" *=* (string "conclusa")    : []
    insert $ "id" *=* (string "08") : "aid" *=* (string "08") : "sid" *=* (string "conclusa")    : []
    insert $ "id" *=* (string "09") : "aid" *=* (string "10") : "sid" *=* (string "conclusa")    : []
    insert $ "id" *=* (string "10") : "aid" *=* (string "10") : "sid" *=* (string "conclusa")    : []
    insert $ "id" *=* (string "11") : "aid" *=* (string "10") : "sid" *=* (string "attesa")      : []
    insert $ "id" *=* (string "12") : "aid" *=* (string "05") : "sid" *=* (string "attesa")      : []
    insert $ "id" *=* (string "13") : "aid" *=* (string "09") : "sid" *=* (string "attesa")      : []
    insert $ "id" *=* (string "14") : "aid" *=* (string "06") : "sid" *=* (string "lavorazione") : []
    insert $ "id" *=* (string "15") : "aid" *=* (string "07") : "sid" *=* (string "lavorazione") : []
    insert $ "id" *=* (string "16") : "aid" *=* (string "05") : "sid" *=* (string "lavorazione") : []
    insert $ "id" *=* (string "17") : "aid" *=* (string "06") : "sid" *=* (string "conclusa")    : []
    insert $ "id" *=* (string "18") : "aid" *=* (string "03") : "sid" *=* (string "conclusa")    : []
    insert $ "id" *=* (string "19") : "aid" *=* (string "09") : "sid" *=* (string "conclusa")    : []
    insert $ "id" *=* (string "20") : "aid" *=* (string "05") : "sid" *=* (string "conclusa")    : []
    insert $ "id" *=* (string "21") : "aid" *=* (string "10") : "sid" *=* (string "attesa")      : []
    insert $ "id" *=* (string "22") : "aid" *=* (string "01") : "sid" *=* (string "attesa")      : []
    insert $ "id" *=* (string "23") : "aid" *=* (string "02") : "sid" *=* (string "attesa")      : []
    insert $ "id" *=* (string "24") : "aid" *=* (string "06") : "sid" *=* (string "lavorazione") : []
    insert $ "id" *=* (string "25") : "aid" *=* (string "01") : "sid" *=* (string "lavorazione") : []
    insert $ "id" *=* (string "26") : "aid" *=* (string "03") : "sid" *=* (string "lavorazione") : []
    insert $ "id" *=* (string "27") : "aid" *=* (string "05") : "sid" *=* (string "conclusa")    : []
    insert $ "id" *=* (string "28") : "aid" *=* (string "04") : "sid" *=* (string "conclusa")    : []
    insert $ "id" *=* (string "29") : "aid" *=* (string "04") : "sid" *=* (string "conclusa")    : []
    insert $ "id" *=* (string "30") : "aid" *=* (string "02") : "sid" *=* (string "conclusa")    : []
initChecks = do                                          
    into checks                
    insert $ "id" *=* (string "01") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "02") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "03") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    insert $ "id" *=* (string "04") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "05") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "06") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "07") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "08") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    insert $ "id" *=* (string "09") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "10") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "11") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "12") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "13") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    insert $ "id" *=* (string "14") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "15") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "16") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "17") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "18") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    insert $ "id" *=* (string "19") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "20") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "21") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "22") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "23") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    insert $ "id" *=* (string "24") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "25") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "26") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "27") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "28") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    insert $ "id" *=* (string "29") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "30") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "31") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "32") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "33") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    insert $ "id" *=* (string "34") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "35") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "36") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "37") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "38") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    insert $ "id" *=* (string "39") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "40") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "41") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "42") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "43") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    insert $ "id" *=* (string "44") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "45") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "46") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "47") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    insert $ "id" *=* (string "48") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    insert $ "id" *=* (string "49") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    insert $ "id" *=* (string "50") : "abi" *=* (string "00100") :  "cab" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
initPersons = do                                          
    into persons
    insert $ "id" *=* (string "1") : "nome" *=* (string "nome1") : "cognome" *=* (string "cognome1") : "datanasc" *=* (string "01/01/2000") : "luogonasc" *=* (string "Roma") : "codfisc" *=* (string "DJFEIKA78L34Y4784C") : []
    insert $ "id" *=* (string "2") : "nome" *=* (string "nome2") : "cognome" *=* (string "cognome2") : "datanasc" *=* (string "01/01/2000") : "luogonasc" *=* (string "Roma") : "codfisc" *=* (string "DJFEIKA38L34Y4784C") : []
    insert $ "id" *=* (string "3") : "nome" *=* (string "nome3") : "cognome" *=* (string "cognome3") : "datanasc" *=* (string "01/01/2000") : "luogonasc" *=* (string "Roma") : "codfisc" *=* (string "DJFEIKA78L34Y4744C") : []
model = [initStates,initRequests,initChecks,initPersons]