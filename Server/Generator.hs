module Server.Generator where
import Server.Commons
import Server.Resource
import Text.PrettyPrint
import Text.Printf
import Data.List
import System.FilePath
generateApplication :: Interface -> OutputFile
generateApplication (Interface int cols) = (OutputFile fileName doc) where 
    fileName = addExtension ("it" </> "bancaditalia" </> int </> "services" </> capInt) "java"
    capInt = capitalize int
    doc = text (printf "package it.bancaditalia.%s.services;" int)
        $$ text ""
        $$ text (printf "//import it.bancaditalia.%s.services.*;" int)
        $$ text ""
        $$ text "import javax.ws.rs.ApplicationPath;"
        $$ text "import javax.ws.rs.core.Application;"
        $$ text "import java.util.HashSet;"
        $$ text "import java.util.Set;"
        $$ text "@ApplicationPath(\"/\")"
        $$ text (printf "public class %s extends Application {" capInt)
        $$ nest 4 (text "private Set<Object> singletons = new HashSet<Object>();"
        $$ text "private Set<Class<?>> empty = new HashSet<Class<?>>();"
        $$ text (printf "public %s() {" capInt)
        $$ nest 4 (vcat (map generateAdds (cols)))
        $$ text "}"
        $$ text "@Override"
        $$ text "public Set<Class<?>> getClasses() {"
        $$ nest 4 (text "return empty;")
        $$ text "}"
        $$ text "@Override"
        $$ text "public Set<Object> getSingletons() {"
        $$ nest 4 (text "return singletons;")
        $$ text "}")
        $$ text "}"
        where generateAdds (Collection n _ _) = text (printf "singletons.add(new %s());" (capitalize n))
     
generateCollection :: Name -> Collection -> OutputFile
generateCollection int (Collection coll (Item item vals rels itemActs) collActs) = (OutputFile fileName doc) where 
    fileName = addExtension ("it" </> "bancaditalia" </> int </> "services" </> capColl) "java"
    capColl = capitalize coll
    capItem = capitalize item-- generateCollectionAction :: Action -> Doc
    doc = text (printf "package it.bancaditalia.%s.services;" int)
        $$ text ""
        $$ text "import java.net.URI;"
        $$ text "import java.util.ArrayList;"
        $$ text "import java.util.Map;"
        $$ text "import java.util.concurrent.ConcurrentHashMap;"
        $$ text "import java.util.concurrent.atomic.AtomicInteger;"
        $$ text ""
        $$ text "import javax.ws.rs.*;"
        $$ text "import javax.ws.rs.core.Response;"
        $$ text ""
        $$ text (printf "import it.bancaditalia.%s.domain.%s;" int capItem)
        $$ text ""
        $$ text (printf "@Path(\"/%s\")" coll)
        $$ text (printf "public class %s {" capColl)
        $$ nest 4 (text (printf "private Map<Integer, %s> %sDB = new ConcurrentHashMap<Integer, %s>();" capItem item capItem)
        $$ text "private AtomicInteger idCounter = new AtomicInteger();"
        $$ text ""
        $$ text "@OPTIONS"
        $$ text "public Response options() {"
        $$ nest 4 (text "return Response" <>  (text ".ok(\"\")"
                                            $$ text ".header(\"Access-Control-Allow-Origin\", \"*\")"
                                            $$ text ".header(\"Access-Control-Allow-Headers\", \"origin, content-type, accept, authorization\")"
                                            $$ text ".header(\"Access-Control-Allow-Credentials\", \"true\")"
                                            $$ text ".header(\"Access-Control-Allow-Methods\", \"" <> text (intercalate ", " (map (upperCase . show) collActs)) <> text "\")"
                                            $$ text ".header(\"Access-Control-Max-Age\", \"1209600\")"
                                            $$ text ".build();"))
        $$ text "}"
        $$ vcat (map generateCollectionAction collActs)
        $$ text "@OPTIONS"
        $$ text "@Path(\"{id}\")"
        $$ text "public Response options(@PathParam(\"id\") int id) {"
        $$ nest 4 (text "return Response" <>  (text ".ok(\"\")"
                                            $$ text ".header(\"Access-Control-Allow-Origin\", \"*\")"
                                            $$ text ".header(\"Access-Control-Allow-Headers\", \"origin, content-type, accept, authorization\")"
                                            $$ text ".header(\"Access-Control-Allow-Credentials\", \"true\")"
                                            $$ text ".header(\"Access-Control-Allow-Methods\", \"" <> text (intercalate ", " (map (upperCase . show) collActs)) <> text "\")"
                                            $$ text ".header(\"Access-Control-Max-Age\", \"1209600\")"
                                            $$ text ".build();"))
        $$ text "}"
        $$ vcat (map generateItemAction itemActs))
        $$ text "}"
        where  
            generateCollectionAction Get = 
                   text "@GET"
                $$ text "@Produces(\"application/json\")"
                $$ text (printf "public Response get%s() {" capColl)
                $$ nest 4 (text (printf "ArrayList<%s> %s = new ArrayList<%s>(%sDB.values());" capItem coll capItem item)
                $$ text "return Response" <>  (text ".status(200)"
                                            $$ text ".header(\"Access-Control-Allow-Origin\", \"*\")"
                                            $$ text ".header(\"Access-Control-Allow-Headers\", \"origin, content-type, accept, authorization\")"
                                            $$ text ".header(\"Access-Control-Allow-Credentials\", \"true\")"
                                            $$ text ".header(\"Access-Control-Allow-Methods\", \"" <> text (intercalate ", " (map (upperCase . show) collActs)) <> text "\")"
                                            $$ text ".header(\"Access-Control-Max-Age\", \"1209600\")"
                                            $$ text (printf ".entity(%s)" coll)
                                            $$ text ".build();"))
                $$ text "}"
            generateCollectionAction Post = 
                   text "@POST"
                $$ text "@Consumes(\"application/json\")"
                $$ text (printf "public Response create%s(%s %s) {" capItem capItem item)
                $$ nest 4 (text (printf "%s.setId(idCounter.incrementAndGet());" item)
                $$ text (printf "%sDB.put(%s.getId(), %s);" item item item)
                $$ text (printf "System.out.println(\"Created %s \" + %s);" item item)
                $$ text (printf "System.out.println(\"--------------------------db size: \" + %sDB.size() + \"---------------------------\\n\");" item)
                $$ text "return Response" <> (text (printf ".created(URI.create(\"/%s/\"+ %s.getId()))" coll item)
                                            $$ text ".header(\"Access-Control-Allow-Origin\", \"*\")"
                                            $$ text ".header(\"Access-Control-Allow-Headers\", \"origin, content-type, accept, authorization\")"
                                            $$ text ".header(\"Access-Control-Allow-Credentials\", \"true\")"
                                            $$ text ".header(\"Access-Control-Allow-Methods\", \"" <> text (intercalate ", " (map (upperCase . show) collActs)) <> text "\")"
                                            $$ text ".header(\"Access-Control-Max-Age\", \"1209600\")"
                                            $$ text ".build();"))
                $$ text "}"
            generateCollectionAction a = error ("not supported: " ++ show a)
            
            generateItemAction Get = 
                   text "@GET"
                $$ text "@Path(\"{id}\")"
                $$ text "@Produces(\"application/json\")"
                $$ text (printf "public Response get%s(@PathParam(\"id\") int id) {" capItem)
                $$ nest 4 (text (printf "final %s %s = %sDB.get(id);" capItem item item)
                $$ text (printf "if (%s == null) {" item)
                $$ nest 4 (text "throw new WebApplicationException(Response.Status.NOT_FOUND);")
                $$ text "}"            
                $$ text "return Response" <>  (text ".status(200)"
                                            $$ text ".header(\"Access-Control-Allow-Origin\", \"*\")"
                                            $$ text ".header(\"Access-Control-Allow-Headers\", \"origin, content-type, accept, authorization\")"
                                            $$ text ".header(\"Access-Control-Allow-Credentials\", \"true\")"
                                            $$ text ".header(\"Access-Control-Allow-Methods\", \"" <> text (intercalate ", " (map (upperCase . show) itemActs)) <> text "\")"
                                            $$ text ".header(\"Access-Control-Max-Age\", \"1209600\")"
                                            $$ text (printf ".entity(%s)" item)
                                            $$ text ".build();"))
                $$ text "}"
            generateItemAction a = error ("not supported: " ++ show a)

generateItem :: Name -> Item -> OutputFile
generateItem int (Item item vals _ _) = (OutputFile fileName doc) where 
    fileName = addExtension ("it" </> "bancaditalia" </> int </> "domain" </> capItem) "java"
    capItem = capitalize item
    doc =  text (printf "package it.bancaditalia.%s.domain;" int)
        $$ text (printf "public class %s {" capItem)
        $$ nest 4 (vcat (map generateDeclaration vals)
        $$ text ""
        $$ vcat (map generateGetter vals)
        $$ text ""
        $$ vcat (map generateSetter vals)
        $$ text ""
     
        $$ text "public String toString() {"
        $$ nest 4 (text "return " <> text (intercalate " + \": \" + " (map toString vals)) <> semi)
        $$ text "}")
        $$ text "}"
        where 
              -- generateDeclaration :: Value -> Doc
              generateDeclaration Empty = empty
              generateDeclaration (Bool name) = text "Bool" <+> text name <> semi
              generateDeclaration (Number name) = text "int" <+> text name <> semi
              generateDeclaration (String name) = text "String" <+> text name <> semi
              generateDeclaration (Array value) = error ("not supported generateDeclaration: " ++ show value) -- text "ArrayList<" <> text name <> semi
              generateDeclaration (Object o) = error ("not supported generateDeclaration: " ++ show o) 
              
              -- generateGetter :: Value -> Doc
              generateGetter Empty = empty
              generateGetter (Bool name) = text (printf "public Bool get%s() { return %s; }" (capitalize name) name)
              generateGetter (Number name) = text (printf "public int get%s() { return %s; }" (capitalize name) name)
              generateGetter (String name) = text (printf "public String get%s() { return %s; }" (capitalize name) name)
              generateGetter (Array value) = error ("not supported generateGetter: " ++ show value)
              generateGetter (Object o) = error ("not supported generateGetter: " ++ show o) 
              
              -- generateGetter :: Value -> Doc
              generateSetter Empty = empty
              generateSetter (Bool name) = text (printf "public void set%s(Bool %s) { this.%s = %s; }" (capitalize name) name name name)
              generateSetter (Number name) = text (printf "public void set%s(int %s) { this.%s = %s; }" (capitalize name) name name name)
              generateSetter (String name) = text (printf "public void set%s(String %s) { this.%s = %s; }" (capitalize name) name name name)
              generateSetter (Array value) = error ("not supported generateSetter: " ++ show value)
              generateSetter (Object o) = error ("not supported generateSetter: " ++ show o) 
              
              -- toString :: Value -> String
              toString Empty = ""
              toString (Bool name) = printf "String.valueOf(%s)" name
              toString (Number name) = printf "String.valueOf(%s)" name
              toString (String name) = name
              toString (Array value) = error ("not supported toString: " ++ show value)
              toString (Object o) = error ("not supported toString: " ++ show o) 