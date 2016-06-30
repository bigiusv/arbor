module Server.AbacoServer where
import Server.Resource
import Server.Commons
import Server.Generator
import Text.PrettyPrint
bank :: Item
bank = "bank" .@. ( Number "id" : String "name" : []
                          , []
                          , Get : []
                          )
               
           
banks :: Collection
banks = "banks" .<>. ( bank
                     , Get : Post : []
                     )
                   
interface :: Interface
interface = "abaco" .<>@. banks : []


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