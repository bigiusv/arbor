module Language.Abaco where
import Language

    
-- banks = Resource "banks" (stat "http://localhost:8080/backend/banks") ["id","name"]
-- branches = Resource "branches" (stat "http://localhost:8080/backend/branches") ["id","name","bankid"]

banks = Table "banks" ["id","name"]
branches = Table "branches" ["id","name","bid"]
collaterals = Table "collaterals" ["id","amount","bid","cid"]
customers = Table "customers" ["id","name","surname"]

--------------------- Queries ---------------------
allBanks = do
    b <- table banks
    project [ b ! "id" *@* "bankId"
            , b ! "name" *@* "bankName"
            ]
getBranches bank = do
    b <- table branches 
    restrict (b ! "bid" *==* bank)
    project [ b ! "id" *@* "branchId"
            , b ! "name" *@* "branchName"
            ]
getCollaterals branch = do
    c <- table collaterals 
    restrict (c ! "bid" *==* branch)
    project [ c ! "id" *@* "collId"
            , c ! "amount" *@* "collAmount"
            ]
getCustomers coll = do
    co <- table collaterals
    cu <- table customers
    restrict (((co ! "cid") *==* (cu ! "id")) *&&* ((co ! "id") *==* coll))
    project [ cu ! "id" *@* "custId"
            , cu ! "name" *@* "custName"
            , cu ! "surname" *@* "custSurname"
            ]

--------------------- Elements ---------------------
welcome = do
    drop <- dropdown allBanks bindBanks
    but  <- button (string "go")
    transition but click none (seg "dashboard" [("bank",value drop)])
    return $ drop <|> but
    
bindBanks scheme = (scheme ! "bankId", scheme ! "bankName")

showBranch branch = do
    lid <- input (branch ! "branchId")
    lname <- input (branch ! "branchName")
    return $ lid <|> lname
    
showBranchNest branch = do
    id <- input (string "Branch ID")
    lid <- input (branch ! "branchId")
    name <- input (string "Branch denomination")
    lname <- input (branch ! "branchName")
    clist <- list (getCollaterals (value lid)) showCollateral
    cid <- input (string "collateral ID")
    camount <- input (string "amount")
    tot <- input (string "total")
    let sumAmounts = do
        c <- getCollaterals (value lid)
        add (c ! "collAmount")
    total <- aggregate sumAmounts
    return $ (    id <|> name
             <-> lid <|> lname)
         <-> (   cid <|> camount
             <-> clist
             <-> tot <|> total)
    

showCollateral coll = do
    cid <- input (coll ! "collId")
    camount <- input (coll ! "collAmount")
    but <- button (string "details")
    transition but click none (seg "details" [("id",value cid)])
    return $ cid <#> 2 <|> camount <#> 2 <|> but <#> 1

dashboard = do
    bank <- parameter "bank"
    bdata <- list (getBranches (param bank)) showBranchNest
    return $ bank <\> bdata

showCustomer cust = do
    lid <- input (cust ! "custId")
    ln <- input (cust ! "custName")
    ls <- input (cust ! "custSurname")
    return $ lid <|> ln <|> ls
    
details = do
    id <- parameter "id"
    lab <- label (string "details")
    det <- list (getCustomers (param id)) showCustomer
    return $ id <\> lab
                <-> det

-- navbarstd = do
    -- wb <- link "Visualize" (seg "visualize" [])
    -- cb <- link "Update" (seg "update" [])
    -- nb <- navbar [wb,cb]
    -- return nb

model = [initBanks,initBranches,initCollaterals,initCustomers]
interface = do
    -- nb <- navbarstd
    welc <- welcome
    dash <- dashboard
    det  <- details
    a <- alternative [ welc <@> "welcome"
                     , dash <@> "dashboard"
                     , det <@> "details"
                     ]
    return $ a
    
    
    
    


initBanks = do
    into banks
    insert $ "id" *=* (string "1") : "name" *=* (string "Bank1") : []
    insert $ "id" *=* (string "2") : "name" *=* (string "Bank2") : []
    insert $ "id" *=* (string "3") : "name" *=* (string "Bank3") : []
initBranches = do
    into branches
    insert $ "id" *=* (string "1") : "name" *=* (string "Branch1") : "bid" *=* (string "1") : []
    insert $ "id" *=* (string "2") : "name" *=* (string "Branch2") : "bid" *=* (string "2") : []
    insert $ "id" *=* (string "3") : "name" *=* (string "Branch3") : "bid" *=* (string "3") : []
    insert $ "id" *=* (string "4") : "name" *=* (string "Branch4") : "bid" *=* (string "1") : []
    insert $ "id" *=* (string "5") : "name" *=* (string "Branch5") : "bid" *=* (string "2") : []
    insert $ "id" *=* (string "6") : "name" *=* (string "Branch6") : "bid" *=* (string "3") : []
    insert $ "id" *=* (string "7") : "name" *=* (string "Branch7") : "bid" *=* (string "1") : []
    insert $ "id" *=* (string "8") : "name" *=* (string "Branch8") : "bid" *=* (string "2") : []
    insert $ "id" *=* (string "9") : "name" *=* (string "Branch9") : "bid" *=* (string "3") : []
initCollaterals = do
    into collaterals
    insert $ "id" *=* (string "1") : "amount" *=* (string "10000") : "bid" *=* (string "1") : "cid" *=* (string "1") : []
    insert $ "id" *=* (string "2") : "amount" *=* (string "20000") : "bid" *=* (string "2") : "cid" *=* (string "2") : []
    insert $ "id" *=* (string "3") : "amount" *=* (string "30000") : "bid" *=* (string "3") : "cid" *=* (string "3") : []
    insert $ "id" *=* (string "4") : "amount" *=* (string "40000") : "bid" *=* (string "1") : "cid" *=* (string "1") : []
    insert $ "id" *=* (string "5") : "amount" *=* (string "50000") : "bid" *=* (string "2") : "cid" *=* (string "2") : []
    insert $ "id" *=* (string "6") : "amount" *=* (string "60000") : "bid" *=* (string "3") : "cid" *=* (string "3") : []
    insert $ "id" *=* (string "7") : "amount" *=* (string "70000") : "bid" *=* (string "1") : "cid" *=* (string "1") : []
    insert $ "id" *=* (string "8") : "amount" *=* (string "80000") : "bid" *=* (string "2") : "cid" *=* (string "2") : []
    insert $ "id" *=* (string "9") : "amount" *=* (string "90000") : "bid" *=* (string "3") : "cid" *=* (string "3") : []
    insert $ "id" *=* (string "10") : "amount" *=* (string "10000") : "bid" *=* (string "1") : "cid" *=* (string "4") : []
    insert $ "id" *=* (string "11") : "amount" *=* (string "20000") : "bid" *=* (string "2") : "cid" *=* (string "5") : []
    insert $ "id" *=* (string "12") : "amount" *=* (string "30000") : "bid" *=* (string "3") : "cid" *=* (string "6") : []
    insert $ "id" *=* (string "13") : "amount" *=* (string "40000") : "bid" *=* (string "1") : "cid" *=* (string "4") : []
    insert $ "id" *=* (string "14") : "amount" *=* (string "50000") : "bid" *=* (string "2") : "cid" *=* (string "5") : []
    insert $ "id" *=* (string "15") : "amount" *=* (string "60000") : "bid" *=* (string "3") : "cid" *=* (string "6") : []
    insert $ "id" *=* (string "16") : "amount" *=* (string "70000") : "bid" *=* (string "1") : "cid" *=* (string "4") : []
    insert $ "id" *=* (string "17") : "amount" *=* (string "80000") : "bid" *=* (string "2") : "cid" *=* (string "5") : []
    insert $ "id" *=* (string "18") : "amount" *=* (string "90000") : "bid" *=* (string "3") : "cid" *=* (string "6") : []
initCustomers = do
    into customers
    insert $ "id" *=* (string "1") : "name" *=* (string "name1") : "surname" *=* (string "surname1") : []
    insert $ "id" *=* (string "2") : "name" *=* (string "name2") : "surname" *=* (string "surname2") : []
    insert $ "id" *=* (string "3") : "name" *=* (string "name3") : "surname" *=* (string "surname3") : []
    insert $ "id" *=* (string "4") : "name" *=* (string "name4") : "surname" *=* (string "surname4") : []
    insert $ "id" *=* (string "5") : "name" *=* (string "name5") : "surname" *=* (string "surname5") : []
    insert $ "id" *=* (string "6") : "name" *=* (string "name6") : "surname" *=* (string "surname6") : []
    insert $ "id" *=* (string "7") : "name" *=* (string "name7") : "surname" *=* (string "surname7") : []
    insert $ "id" *=* (string "8") : "name" *=* (string "name8") : "surname" *=* (string "surname8") : []
    insert $ "id" *=* (string "9") : "name" *=* (string "name9") : "surname" *=* (string "surname9") : []