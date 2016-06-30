-- {-#LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
module Language.Utils where
import Language.Commons
-- import Language.Expression
import Language.Query
-- import Language.Filter
-- -- import Language.Element
import Language.Environment
-- import Language.Transition

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Text.PrettyPrint
import Text.Printf
import Control.Monad.State
-- import System.Random
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Debug.Trace
-- parse :: (Renderable a) => SElement a -> (b,Map.Map Identifier [Transition])
parse interface = 
    let (element,(_,_,transitions)) = runState interface ("",0,Map.empty)
        transitions' = Map.map (\tlist -> map (checkTransition (Element element)) tlist) transitions
    in (element,transitions')
          
checkTransition :: Element -> Transition -> Transition
checkTransition top t@(Transition el Click act (Pose pose)) =
    let (Right pp) = runInterp (generatePose pose) (Environment Map.empty [] EmptyContext [])
    in case (toFilter pose) top of 
        [] -> error (render pp ++ " is not valid in transition " ++ show t) --wrong segment name
        elems -> case (any (\(Element e) -> isInfixOf "Alternative" (show e)) elems) of
            True -> error (render pp ++ " is not complete in " ++ show t) --wrong segment name
            False -> t
        
absolutify :: (Filterable a) => a -> Transition -> Transition
absolutify interface (Transition el ev act pose) = 
    let pose' = pose
    in (Transition el Click act pose')
    
    

----------------------------------------------------------
-- top level generation functions
----------------------------------------------------------
generateIndex :: (Renderable a) => Name -> a -> (Map.Map Identifier [Transition]) -> Either String Doc
generateIndex name elem trans = runInterp (generateIndex' name elem) (Environment trans [] EmptyContext [])

generateIndex' :: (Renderable a) => Name -> a -> InterpM Doc
generateIndex' name element = do
    return $ text "<!DOCTYPE html>"
         $$ text (printf "<html ng-app=\"%s\">" name)
         $$ nest 4 (text "<head>"
         $$ nest 4 (text "<meta charset=\"utf-8\" />"
         $$ text (printf "<title>%s</title>" name)
         -- $$ text "<link rel=\"stylesheet\" href=\"http://www.w3schools.com/lib/w3.css\">")
         -- $$ text "<link rel=\"stylesheet\" href=\"css/arbor.css\">")
         $$ text "<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css\" integrity=\"sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7\" crossorigin=\"anonymous\">")
         $$ text "</head>")
         -- $$ nest 4 (text "<script src=\"lib/angular/angular.js\"></script>"
         $$ nest 4 (text "<script data-require=\"angular.js@1.3.0\" data-semver=\"1.3.0\" src=\"//code.angularjs.org/1.3.0/angular.js\"></script>"
         $$ text "<script src=\"lib/angular/angular-resource.js\"></script>"
         $$ text "<script src=\"js/app.js\"></script>"
         -- $$ text "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.17/d3.js\"></script>"
         $$ text "<script src=\"js/data.js\"></script>"
         $$ text "<script src=\"js/pose.js\"></script>"
         $$ text "<body id=\"body\">"
         $$ nest 4 (text (printf "<h1>%s</h1>" name))
         $$ nest 4 (text "<dir-interface></dir-interface>")
         $$ text "</body>")
         $$ text "</html>"
 
generateApplication :: (Renderable a) => Name -> a -> (Map.Map Identifier [Transition]) -> Pose -> Either String Doc
generateApplication name elem trans pose
    = case runInterp (generateApplication' name elem pose) (Environment trans [] EmptyContext []) of
            Left e -> Left e
            Right doc -> Right doc

generateApplication' :: (Renderable a) => Name -> a -> Pose -> InterpM Doc
generateApplication' name elem (Pose pose) = 
    do  b <- check elem
        let mods = {- traceShow b $ -} modules elem
        dirs <- directives elem
        temp <- template elem
        ctrl <- controller elem
        pose' <- generatePose pose
        return $ text "angular.module('" <> text name <> text "', ['interface','" <> text (intercalate "','" mods) <> text "']);"
              $$ text "angular.module('interface', ['pose','data'])" -- 
              $$ nest 4 (text ".directive('dirInterface', function() {"
              $$ nest 4 (text "return {"
              $$ nest 4 (text "scope: {},"
              $$ text "template: '<div>'" 
              $$ nest 4 (text "+ '<button class=\"btn btn-info\" type=\"button\" ng-click=\"backButtonClick()\"> <span class=\"glyphicon glyphicon-arrow-left\"></span> </button>'"
              $$ text "+ '<button class=\"btn btn-info\" type=\"button\" ng-click=\"fwdButtonClick()\"> <span class=\"glyphicon glyphicon-arrow-right\"></span> </button>'"
              $$ temp)
              $$ text " +'</div>'"<> comma
              $$ text "replace: true,"
              $$ text "controller: 'interfaceController'," 
              $$ text "controllerAs: 'ctrl'")
              $$ text "};")
              $$ text "})"
              $$ text ".controller('interfaceController', function($pose,$data,$scope) {"
              $$ nest 4 (text "$scope." <> text "go = function(pose) { $pose.go(pose);}"
              $$ text "$scope.match = function(url) {return $pose.match(url);};"
              $$ text "$scope.backButtonClick = function() {return $pose.back();};"
              $$ text "$scope.fwdButtonClick = function() {return $pose.forward();};"
              $$ text "$scope.get = function(pose,key) { console.log(pose + \" \" + key + \" \" + $pose.get(pose,key)); return $pose.get(pose,key);}"
              $$ text "$scope.go('" <> pose' <> text "');"
              $$ ctrl)
              $$ text "});")
              $$ vcat dirs

generateDataService :: [Command Rel] -> Either String Doc
generateDataService tables
    = case runInterp (generateDataService' tables) (Environment Map.empty [] EmptyContext []) of
            Left e -> Left e
            Right doc -> Right doc

generateDataService' :: [Command Rel] -> InterpM Doc
generateDataService' cmds = do
    let cmds' = map (\c -> let (rel,pc) = runState c EmptyCommand
                                    in {-traceShow pc $-} pc) cmds
    cmds'' <- sequence (map generateTables cmds')
    return $ text "var app = angular.module('data', []);"
      $$ text "app.service('$data', function() {"
      $$ nest 4 (text "Array.prototype.insert = function (obj) {"
      $$ text "    this.push(obj);"
      $$ text "    return this;"
      $$ text "}"
      $$ text "Array.prototype.remove = function(condition) {"
      $$ text "    for(var i = this.length - 1; i >= 0; i--) {"
      $$ text "        if(condition(this[i])) {"
      $$ text "           this.splice(i, 1);"
      $$ text "        }"
      $$ text "    }"
      $$ text "    return this;"
      $$ text "}"
      $$ text "Array.prototype.update = function(condition,value) {"
      $$ text "    for(var i = this.length - 1; i >= 0; i--) {"
      $$ text "        if(condition(this[i])) {"
      $$ text "           this[i] = value;"
      $$ text "        }"
      $$ text "    }"
      $$ text "    return this;"
      $$ text "}"
      $$ text ""
      $$ text "Array.prototype.product = function(rest) {"
      $$ text " return this.reduce(function (acc, x) {"
      $$ text "    return acc.concat(rest.map(function (y) {"
      $$ text "        return Object.assign({}, x, y);;"
      $$ text "    }));},[]);"
      $$ text "}"
      $$ vcat cmds'')
      $$ text "});"

generatePoseService :: (Renderable a) => Name -> a -> Either String Doc
generatePoseService appName elem
    = case runInterp (generatePoseService' elem) (Environment Map.empty [] EmptyContext []) of
            Left e -> Left e
            Right doc -> Right doc

generatePoseService' ::  (Renderable a) => a -> InterpM Doc
generatePoseService' elem = do
    sch <- schema elem
    return $ text "'use strict';"
        $$ text "var app = angular.module('pose', ['ngResource']);"
        $$ text ""
        $$ text "app.service('$pose', function() {"
        $$ text "    this.schema = " <> brackets (sch) <> semi
        $$ text "    this.states = [];"
        $$ text "    this.state = [];"
        $$ text "    this.index = -1;"
        $$ text "    // console.log('in $pose.init: index=' + this.index);"
        $$ text "    // console.log('in $pose.init: schema=' + this.schema.toSource());"
        $$ text "    "
        $$ text "    this.seq = function(name) {"
        $$ text "        var seqArray = function(name,array) {"
        $$ text "            var res = -1;"
        $$ text "            Array.prototype.max = function() {"
        $$ text "                return Math.max.apply(null, this);"
        $$ text "            };"
        $$ text "            if (array.length == 0)"
        $$ text "                return -1;"
        $$ text "            else {"
        $$ text "                // console.log(\"array:\" + array.toSource());"
        $$ text "                return array.map(function(obj,ind) {"
        $$ text "                                if (obj.hasOwnProperty(name))"
        $$ text "                                    return ind;"
        $$ text "                                else"
        $$ text "                                    return seqObj(name,obj);"
        $$ text "                            }).max();"
        $$ text "            }"
        $$ text "        }"
        $$ text "        var seqObj = function(name,obj) {"
        $$ text "            var values = function(obj) {"
        $$ text "                var vals = [];"
        $$ text "                var i = 0;"
        $$ text "                for (var key in obj) {"
        $$ text "                    vals[i] = obj[key];"
        $$ text "                    i = i+1;"
        $$ text "                }"
        $$ text "                // console.log(\"vals: \" + vals.toSource());"
        $$ text "                return vals;"
        $$ text "            }"
        $$ text "            // console.log(\"obj:\" + obj.toSource());"
        $$ text "            return values(obj).map(function(item,ind){"
        $$ text "                                    return seqArray(name,item);"
        $$ text "                                  }).max();"
        $$ text "        }"
        $$ text "        return seqArray(name,this.schema);"
        $$ text "    }"
        $$ text "    "
        $$ text "    "
        $$ text "    this.compare = function(pose,path) {"
        $$ text "        var expansion = pose.substr(1).split(\"/\");"
        $$ text "        // console.log(\"expansion: \" + expansion.toSource());"
        $$ text "        // console.log(\"path: \" + path.toSource());"
        $$ text "        if (expansion.length != path.length)"
        $$ text "            return false;"
        $$ text "        else {"
        $$ text "            for (var i = 0; i < expansion.length; i++) {"
        $$ text "                if (expansion[i] !== path[i].name)"
        $$ text "                    return false;"
        $$ text "            }"
        $$ text "            return true;"
        $$ text "        }"
        $$ text "    }"
        $$ text "    "
        $$ text "    this.match = function(pose) {"
        $$ text "                    // var expansion = this.parse(pose);"
        $$ text "                    //var expansion = pose.substr(1).split(\"/\");"
        $$ text "                    // console.log('in $pose.match: expansion=' + expansion.toSource());"
        $$ text "                    self = this;"
        $$ text "                    return (this.state.filter(function(g) {"
        $$ text "                                                "
        $$ text "                                                return (self.compare(pose,g.path));"
        $$ text "                                               }"
        $$ text "                            ).length != 0);"
        $$ text "                 }"
        $$ text ""
        $$ text "    this.merge = function(oldstate,newstate) {"
        $$ text "        var pathcompare= function(newpath,oldpath) { "
        $$ text "            // returns 0 if paths are the same"
        $$ text "            //         1 if paths are compatible (can coexist in a state)"
        $$ text "            //        -1 if paths are not compatible"
        $$ text "            if (newpath.length == 0 && oldpath.length == 0)"
        $$ text "                return 0;  //paths are trivially the same"
        $$ text "            else if (newpath.length == 0 || oldpath.length == 0)"
        $$ text "                return 1;  //if only one of them is empty, paths are compatible"
        $$ text "            else { //at this point none of newpath, oldpath are of zero length"
        $$ text "                if (newpath[0].ind == oldpath[0].ind) {"
        $$ text "                    if (newpath[0].name === oldpath[0].name)"
        $$ text "                        //first elements are the same, continue with remaining elements"
        $$ text "                        return pathcompare(newpath.slice(1),oldpath.slice(1)); "
        $$ text "                    else"
        $$ text "                        //same index in sequence, but different names: paths are not compatible"
        $$ text "                        return -1;"
        $$ text "                } else {"
        $$ text "                    //different index in sequence, paths are compatible"
        $$ text "                    return 1;"
        $$ text "                }"
        $$ text "            }"
        $$ text "        }"
        $$ text "        "
        $$ text "        var iscompatible = function(oldst) {"
        $$ text "            return newstate.every(function(newst){"
        $$ text "                if (pathcompare(newst.path,oldst.path) <= 0)"
        $$ text "                    //if comp==0 we have same paths, but params could be different, oldst must not be added"
        $$ text "                    return false;"
        $$ text "                else"
        $$ text "                    return true;"
        $$ text "            });"
        $$ text "        }"
        $$ text "        return newstate.concat(oldstate.filter(iscompatible));"
        $$ text "    }"
        $$ text "    this.go = function(pose) {"
        $$ text "        this.states = this.states.slice(0,this.index+1);"
        $$ text "        var oldstate = this.state;"
        $$ text "        var newstate = this.parse(pose);"
        $$ text "        this.states.push(this.merge(oldstate,newstate));"
        $$ text "        this.index = this.index + 1;"
        $$ text "        this.state = this.states[this.index];"
        $$ text "        // console.log('in $pose.go: states = ' + this.states.toSource() + ' index=' + this.index + ' state=' + this.state);"
        $$ text "        console.log('in $pose.go: state=' + this.state.toSource());"
        $$ text "    }"
        $$ text "    this.back = function() {"
        $$ text "        if (this.index != 0)"
        $$ text "            this.index = this.index - 1;"
        $$ text "        this.state = this.states[this.index];"
        $$ text "    }"
        $$ text "    this.forward = function() {"
        $$ text "        if (this.index != this.states.length -1)"
        $$ text "            this.index = this.index + 1;"
        $$ text "        this.state = this.states[this.index];"
        $$ text "    }"
        $$ text "    "
        $$ text "    this.get = function(pose,key) {"
        $$ text "        self = this;"
        $$ text "        var res = this.state.filter(function(elem) {"
        $$ text "            if (self.compare(pose,elem.path)) {"
        $$ text "                return true;"
        $$ text "            } else {"
        $$ text "                return false;"
        $$ text "            }"
        $$ text "        });"
        $$ text "        if (res.length > 0) {"
        $$ text "            //console.log('found ' + pose + ',' + key + ': ' + res[0].params[key]);"
        $$ text "            return res[0].params[key];"
        $$ text "        } else {"
        $$ text "            //console.log('not found ' + pose + ',' + key);"
        $$ text "            return undefined;"
        $$ text "        }"
        $$ text "    }"
        $$ text "    var self = this;"
        $$ text "    this.parse =    function(string) {"
        $$ text "                        var index = 0;"
        $$ text "                        var lookahead;"
        $$ text "                        if (string.length == 0) {"
        $$ text "                            lookahead = '$';"
        $$ text "                        } else {"
        $$ text "                            lookahead = string[index];"
        $$ text "                        }"
        $$ text "                        var pose = function() {"
        $$ text "                            if(lookahead == '$'){"
        $$ text "                                throw(\"syntax error in pose: found $\");"
        $$ text "                            } else if(lookahead == '/') {"
        $$ text "                                //console.log(lookahead);"
        $$ text "                                match('/');"
        $$ text "                                var av = absolute();"
        $$ text "                                //console.log(\"synthesized attribute in pose (lookahead == '/'): \" + av.toSource());"
        $$ text "                                return av;"
        $$ text "                            // } else if(isCharacter(lookahead)) {"
        $$ text "                            } else {"
        $$ text "                                var rv = relative();"
        $$ text "                                //console.log(\"synthesized attribute in pose (else): \" + rv.toSource());"
        $$ text "                                return rv;"
        $$ text "                            // } else {"
        $$ text "                                // throw(\"syntax error in pose: found \" + lookahead);"
        $$ text "                            }"
        $$ text "                        }"
        $$ text "                        var absolute = function() {"
        $$ text "                            if(lookahead == '$') {"
        $$ text "                                av = [];"
        $$ text "                                //console.log(\"synthesized attribute in absolute (lookahead == '$'): \" + av.toSource());"
        $$ text "                                return av;"
        $$ text "                            // else if(lookahead == '/') {"
        $$ text "                            } else {"
        $$ text "                                var rv = relative();"
        $$ text "                                //console.log(\"synthesized attribute in absolute (else): \" + rv.toSource());"
        $$ text "                                return rv;"
        $$ text "                            // } else {"
        $$ text "                                // throw(\"syntax error in absolute: expected \"/\", found \" + lookahead);"
        $$ text "                            }"
        $$ text "                        }"
        $$ text ""
        $$ text "                        var relative = function() {"
        $$ text "                            if(lookahead == '(') {"
        $$ text "                                //console.log(lookahead);"
        $$ text "                                match('(');"
        $$ text "                                var rv = relative();"
        $$ text "                                var tv = restRelativeTuple();"
        $$ text "                                //console.log(lookahead);"
        $$ text "                                match(')');"
        $$ text "                                //console.log(\"synthesized attribute in relative (lookahead == '('): \" + rv.concat(tv));"
        $$ text "                                return rv.concat(tv);"
        $$ text "                            } else if(isCharacter(lookahead)) {"
        $$ text "                                var sv = segment();"
        $$ text "                                var pv = parameterList();"
        $$ text "                                // console.log(\"before restRelative \" + restRelative);"
        $$ text "                                var rv = restRelative();"
        $$ text "                                // console.log(\"after restRelative \" + sv);"
        $$ text "                                var part = {path: [{ind: self.seq(sv),name: sv}], params: pv};"
        $$ text "                                var res = [part].concat(cat(part,rv));"
        $$ text "                                //console.log(\"synthesized attribute in relative (isCharacter(lookahead)): \" + res);"
        $$ text "                                return res;"
        $$ text "                            } else {"
        $$ text "                                throw(\"syntax error in relative: expected \\\"(\\\" or character, found \" + lookahead);"
        $$ text "                            }"
        $$ text "                        }"
        $$ text ""
        $$ text "                        var parameterList = function () {"
        $$ text "                        if(lookahead == '{') {"
        $$ text "                                //console.log(lookahead);"
        $$ text "                                match('{');"
        $$ text "                                var pv = parameter();"
        $$ text "                                var tv = restParameterTuple();"
        $$ text "                                //console.log(lookahead);"
        $$ text "                                match('}');"
        $$ text "                                //console.log(\"synthesized attribute in parameterList (lookahead == '{'): \" + Object.assign(pv,tv).toSource());"
        $$ text "                                return Object.assign(pv,tv);"
        $$ text "                            } else {"
        $$ text "                                //console.log(\"synthesized attribute in parameterList (else): []\");"
        $$ text "                                return {};"
        $$ text "                            }"
        $$ text "                        }"
        $$ text "                        var parameter = function() {"
        $$ text "                            if(isCharacter(lookahead)) {"
        $$ text "                                var kv = segment();"
        $$ text "                                //console.log(lookahead);"
        $$ text "                                match('=');"
        $$ text "                                match('\"');"
        $$ text "                                var vv = segment();"
        $$ text "                                match('\"');"
        $$ text "                                var obj = {};"
        $$ text "                                obj[kv] = vv;"
        $$ text "                                //console.log(\"synthesized attribute in parameter (lookahead == '{'): \" + obj.toSource());"
        $$ text "                                return obj;"
        $$ text "                            }"
        $$ text "                        }"
        $$ text "                        var restParameterTuple = function() {"
        $$ text "                            if(lookahead == ',') {"
        $$ text "                                //console.log(lookahead);"
        $$ text "                                match(',');"
        $$ text "                                var pv = parameter();"
        $$ text "                                var tv = restParameterTuple();"
        $$ text "                                //console.log(\"synthesized attribute in restParameterTuple (lookahead == ','): \" + Object.assign(pv,tv).toSource());"
        $$ text "                                return Object.assign(pv,tv);"
        $$ text "                            } else {"
        $$ text "                                return [];"
        $$ text "                            }"
        $$ text "                        }"
        $$ text "                        var restRelative = function() {"
        $$ text "                            if(lookahead == '/') {"
        $$ text "                                //console.log(lookahead);"
        $$ text "                                match('/');"
        $$ text "                                var rv = relative();"
        $$ text "                                //console.log(\"synthesized attribute in restRelative (lookahead == '/'): \" + rv);"
        $$ text "                                return rv;"
        $$ text "                            } else {"
        $$ text "                                //console.log(\"synthesized attribute in restRelative (else): []\");"
        $$ text "                                return [];  "
        $$ text "                            }"
        $$ text "                        }"
        $$ text ""
        $$ text "                        var restRelativeTuple = function() {"
        $$ text "                            if(lookahead == ',') {"
        $$ text "                                //console.log(lookahead);"
        $$ text "                                match(',');"
        $$ text "                                var rv = relative();"
        $$ text "                                var tv = restRelativeTuple();"
        $$ text "                                //console.log(\"synthesized attribute in restRelativeTuple (lookahead == ','): \" + rv.concat(tv));"
        $$ text "                                return rv.concat(tv);"
        $$ text "                            } else {"
        $$ text "                                return [];"
        $$ text "                            }"
        $$ text "                        }"
        $$ text "                        var segment = function() {"
        $$ text "                            var cv = character();"
        $$ text "                            // console.log(\"**************** in segment cv = \" + cv);"
        $$ text "                            var rv = restSegment();"
        $$ text "                            // console.log(\"**************** in segment rv = ******\" + rv + \"##########\");"
        $$ text "                            //console.log(\"synthesized attribute in segment: \" + cv+rv);"
        $$ text "                            return cv+rv;"
        $$ text "                        }"
        $$ text "                        var character = function(){"
        $$ text "                            if (isCharacter(lookahead)) {"
        $$ text "                                //console.log(lookahead);"
        $$ text "                                var c = match(lookahead);"
        $$ text "                                //console.log(\"synthesized attribute in character: \" + c);"
        $$ text "                                return c;"
        $$ text "                            } else {"
        $$ text "                                throw(\"unknown character \" + lookahead);"
        $$ text "                            }"
        $$ text "                        }"
        $$ text "                        var restSegment = function() {"
        $$ text "                            if (lookahead == '$') {"
        $$ text "                                //console.log(\"end of string in restSegment\");"
        $$ text "                                //console.log(\"synthesized attribute in restSegment (lookahead == '$'): \"\"\");"
        $$ text "                                return \"\";"
        $$ text "                            } else if (isCharacter(lookahead)) {"
        $$ text "                                // console.log(lookahead);"
        $$ text "                                var cv = character();"
        $$ text "                                // console.log(\"**************** in restSegment before cv = \" + cv);"
        $$ text "                                var rv = restSegment();"
        $$ text "                                // console.log(\"**************** in restSegment after cv = \" + cv);"
        $$ text "                                // console.log(\"**************** in restSegment rv = ******\" + rv + \"##########\");"
        $$ text "                                //console.log(\"synthesized attribute in restSegment (isCharacter(lookahead)): \" + cv+rv);"
        $$ text "                                return cv+rv;"
        $$ text "                            } else {"
        $$ text "                                //console.log(\"synthesized attribute in restSegment (else): \"\"\");"
        $$ text "                                return \"\";"
        $$ text "                            }"
        $$ text "                        }"
        $$ text "                        var isCharacter = function(character) {"
        $$ text "                            return \"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\".indexOf(character) != -1;"
        $$ text "                        }"
        $$ text "                        var match = function(character) {"
        $$ text "                            if (character == lookahead) {"
        $$ text "                                index += 1;"
        $$ text "                                if (index < string.length) {"
        $$ text "                                    lookahead = string[index];"
        $$ text "                                } else {"
        $$ text "                                    lookahead = '$';"
        $$ text "                                }"
        $$ text "                                return character;"
        $$ text "                            } else {"
        $$ text "                                throw(\"syntax error in match: expected: \" + character + \", found: \" + lookahead);"
        $$ text "                            }"
        $$ text "                        }"
        $$ text "                        // var cat = function(prefix,list) {"
        $$ text "                            // if (list.length == 0) {"
        $$ text "                                // console.log(\"in cat length = 0: \" + [prefix]);"
        $$ text "                                // return [prefix];"
        $$ text "                            // } else {"
        $$ text "                                // console.log(\"in cat length > 0: \" + list.map(function(element) {return prefix + \"/\" + element;}));"
        $$ text "                                // return list.map(function(element) {return prefix + \"/\" + element;});"
        $$ text "                            // }                            "
        $$ text "                        // }"
        $$ text "                        var cat = function(prefix,list) {"
        $$ text "                            if (list.length == 0) {"
        $$ text "                                //console.log(\"in cat length = 0: \" + [prefix]);"
        $$ text "                                return [prefix];"
        $$ text "                            } else {"
        $$ text "                                var res = list.map(function(element) {"
        $$ text "                                                        return {path: prefix.path.concat(element.path),"
        $$ text "                                                                params: element.params};"
        $$ text "                                                    });"
        $$ text "                                //console.log(\"in cat length > 0: \" + res);"
        $$ text "                                return res;"
        $$ text "                            }                            "
        $$ text "                        }"
        $$ text "                        "
        $$ text "                        var unique = function (value, index, self) {"
        $$ text "                            //console.log(self.indexOf(value) + \" \" + index);"
        $$ text "                            "
        $$ text "                            return self.indexOf(value) === index;"
        $$ text "                        }"
        $$ text "                        "
        $$ text "                        var expansion = pose().filter(unique);"
        $$ text "                        return expansion;"
        $$ text "                    }"
        $$ text "});"

 
type Graph = [(Pose,[Pose])]
-- neighbors :: (Filterable a) => [Transition] -> a -> [Pose]
-- neighbors trans element =
    -- concat $ map (\(Transition a _ _ pose) -> if contains a element then [pose] else []) trans
visit :: Graph -> Pose -> [Pose]
visit graph init = dfs [init] []
    where   dfs [] vis = vis
            dfs (c:cs) vis
                | elem c vis = dfs cs vis
                | otherwise  = dfs ((adjacent graph c) ++ cs) (vis ++ [c])

adjacent :: Graph -> Pose -> [Pose]
adjacent graph pose = fromMaybe [] $ lookup pose graph

printGraph :: Graph -> IO ()
printGraph = 
    mapM_ printRow 
    where printRow (pose,neighbors) = do
            putStrLn $ show pose
            mapM_ (\n -> putStrLn ("    " ++ show n)) neighbors
            putStrLn "\n"
            return ()
graph :: (Renderable a, Filterable a, Eq a, Show a, Presentable a) => [Transition] -> a -> Pose -> Graph
graph [] _ _ = []
graph trans element (Pose init) =
    let start = (toFilter init) (Element element) :: [Element]
        (now, later) = partition (\(Transition a _ _ _) -> any (\(Element e) -> contains a e) start) trans :: ([Transition],[Transition])
        neighbors = map (\(Transition a _ _ pose) -> pose) now :: [Pose]
    -- in traceShow ("init: " ++ show (Pose init)) $
       -- traceShow ("element: " ++ show element) $
       -- traceShow ("start: " ++ show start) $
       -- traceShow ("now: " ++ show now) $
       -- traceShow ("later: " ++ show later) $
       -- traceShow ("neighbors: " ++ show neighbors) $
    in ((Pose init),neighbors) : (concat $ map (graph later element) neighbors) :: Graph

-- instance Presentable Graph where
    -- present g = return empty
    
generateAhk ::  (Presentable a, Renderable a, Eq a, Show a, Filterable a) => Name -> a -> (Map.Map Identifier [Transition]) -> Pose -> Either String Doc
generateAhk name elem trans pose
    = case runInterp (generateAhk' name elem pose )(Environment trans [] EmptyContext []) of
            Left e -> Left e
            Right doc -> Right doc
generateAhk' :: (Presentable a, Renderable a, Eq a, Show a, Filterable a) => Name -> a -> Pose -> InterpM Doc
generateAhk' name element init = do
    do  transitions <- fmap getTransitions ask
        let net = graph (concat $ Map.elems transitions) element init
        let poses = visit net init :: [Pose]
        docs <- sequence (map describe poses)
        return $ text "#Include ahklib.ahk"
              $$ text "#w::"
              $$ nest 4 (vsep (text "; -----------------") docs)
              $$ text "Return"
        where describe (Pose pose) = do 
                pp <- pprint pose
                let elems = (toFilter pose) (Element element) :: [Element]
                elems' <- sequence (map (\(Element e) -> present e) elems)
                return  $ text "State" <> (parens . doubleQuotes) pp
                       $$ vcat elems'
