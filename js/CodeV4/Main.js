;; var {head, fst, snd, stringToCharList, readFile, writeFile, replaceExtensionToJs} = require('./Externals.js'); var {parse, progP} = require('./Parser.js'); var {toJs} = require('./CodeGenerator.js'); var main = function (progArgs) { var txt = (readFile ((fst (progArgs)))); var progAst = (fst ((head (((parse (progP)) ((stringToCharList (txt)))))))); return ((writeFile ((replaceExtensionToJs ((snd (progArgs)))))) ((toJs (progAst)))) }; exports.main = main; 