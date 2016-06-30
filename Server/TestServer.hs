module Server.TestServer where
import Server.Resource
import Server.Commons
import Server.Generator
import Text.PrettyPrint
customer :: Item
customer = "customer" .@. ( Number "id" : String "name" : String "surname" : []
                          , "books" --> book : []
                          , Get : []
                          )
               
           
customers :: Collection
customers = "customers" .<>. ( customer
                             , Get : Post : []
                             )
                   

book :: Item
book = let values = Number "id" : String "title" : []
           in  "book" .@. (values
                          , "customer" --> customer : []
                          , Get : []
                          )
           
books :: Collection
books = "books" .<>. ( book
                     , Get : Post : []
                     )
interface :: Interface
interface = "shop" .<>@. customers : books : []


main :: IO ()
main = 
    let target = "D:/Dati/Profili/M026980/workspace/backend/src/main/java"
        (Interface int colls) = interface
        app = generateApplication interface
        res = map (generateCollection int) $ colls
        dom = map (generateItem int) $ (map item colls)
        outputFiles = app : res ++ dom
        
    in do -- putStrLn $ render doc
          mapM_ (saveFile target) outputFiles