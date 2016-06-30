import Language
import Language.Insoluti
-- import Language.TestInterface
-- import Language.Ecommerce
-- import Language.Bootstrap
-- import Language.Demo
-- import Language.Abaco
import Language.Pose

import System.Environment
import System.IO
import Text.PrettyPrint
-- import Text.Printf
-- import System.FilePath
import System.Directory
import Control.Monad.State
import qualified Data.Map as Map

main :: IO ()
main = 
    -- putStrLn $ show $ runState welcome (0,Map.empty)
    let appName = "insoluti"
        appFile = "app.js"
        posFile = "pose.js"
        datFile = "data.js"
        ahkFile = "presentation.ahk"
        indexFile = appName ++ ".html"
        -- init = Pose $ (seg "login" [])
        init = Pose $ (seg "home" [])
        -- init = Pose $ (seg "home" [] </> (seg "navbarstd" [] <&> seg "welcome" []))
        target = "D:/Dati/Profili/M026980/Documents/programmi/nucleo/bi-mvc/sandeep/angular-seed-master/app"
        jsdir = target ++ "/js"
        ahkdir = target ++ "/ahk"
        -- (element,(_,_,transitions)) = runState interface ("",0,Map.empty)
        (element,transitions) = parse interface
        -- init = Pose $ seg "login" [("ip",string ""),("parb",string "")]

        -- init = Pose $ seg "home" [] </> ((seg "navbar1" [] <&> seg "welcome" [("p",string "hello")]))
        -- model = [countries,states,cities]
        in
        do { -- putStrLn $ show $ checkPose (seg "home" [] </> ((seg "navbar1" [("pi",string "hello")] <&> seg "welcome" [("p",string "hello")]))) (item element)
             -- let pos = (seg "home" [] </> ((seg "navbar1" [] <&> seg "welcome" [("p",string "hello")])))
           -- ; let pths = paths (Element element)
           -- ; let pos = seg "navbar1" []
           -- ; let (Just supp) = support (head (toFilter pos (Element element))) (Element element)
           -- ; mapM_ putStrLn (map (\e -> show e++ "\n") supp)
           
           -- ; putStrLn $ show $ head pths
             -- (seg "login" [("parb",string "hello"),("ip",string "hello")])
           -- ; putStrLn $ show $ toFilter pos (item element)
           -- ; putStrLn $ show $ toFilter pos (Element element)
           -- ; putStrLn $ show $ length $ toFilter pos (Element element)
           -- ; let (Right sch) = runInterp (schema element) (Map.empty,[],EmptyContext)
           -- ; putStrLn $ render $ sch
           -- ; let net = graph (concat $ Map.elems transitions) element init
           -- ; printGraph $ net
           -- ; putStrLn $ show $ visit net init
           -- ; mapM_ (\e -> putStrLn $ show e ++ "\n") $ toFilter pos (Element element)
           
           -- ; let (Right pp) = runInterp (generatePose pos) (Map.empty,[],EmptyContext)
           -- ; putStrLn $ render pp
             -- putStrLn $ show $ item element
             -- mapM_ (putStrLn . (\(Pose p) -> pprint p)) (poses element)
           ; -- putStrLn (show transitions)
           ; let
                 -- app = runInterpList (directives element) (Map.empty,[])
                 ind = generateIndex appName element transitions
                 app = generateApplication appName element transitions init
                 pos = generatePoseService appName element
                 dat = generateDataService model
                 ahk = generateAhk appName element transitions init
             in
                case sequence [ind,app,dat,pos,ahk] of 
                    Left errors -> putStrLn ("error:" ++ errors)
                    Right docs@[indexDoc,appDoc,dataDoc,poseDoc,ahkDoc] ->
                        sequence_ [ -- mapM_ putStrLn (map render docs)
                                  -- , putStrLn $ show $ length docs
                                    saveFile target (OutputFile indexFile indexDoc)
                                  , saveFile jsdir (OutputFile appFile appDoc)
                                  , saveFile jsdir (OutputFile posFile poseDoc)
                                  , saveFile jsdir (OutputFile datFile dataDoc)
                                  , saveFile ahkdir (OutputFile ahkFile ahkDoc)
                                  ]
           }