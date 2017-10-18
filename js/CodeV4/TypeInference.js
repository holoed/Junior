;; var {foldLeft, fmap} = require('./Base.js'); var {mkTuple2, fst, snd, empty, containsKey, findValue, error, singletonSet, union, emptySet, not, setContains, emptyDict, addEntry, isEmpty, elit, evar, elam, eapp} = require('./Externals.js'); var {mkTyCon, mkTyVar, mkTyLam, TyVar, TyLam, isInt, isFloat, isString, isChar, isTyCon, isTyLam, isTyVar, extractTyLam, extractTyVar, extractTyCon} = require('./TypesExternals.js'); var {cataExp} = require('./CodeGenerator.js'); var bind = function (m) {  return function (f) {  return function (r) {  return function (s) { var p = ((m (r)) (s)); var s2 = (fst (p)); var x = (snd (p)); return (((f (x)) (r)) (s2)) } } } }; var unit = function (x) {  return function (r) {  return function (s) {  return ((mkTuple2 (s)) (x)) } } }; var ask = function (r) {  return function (s) {  return ((mkTuple2 (s)) (r)) } }; var local = function (f) {  return function (m) {  return function (r) {  return function (s) {  return ((m ((f (r)))) (s)) } } } }; var put = function (s2) {  return function (r) {  return function (s) {  return ((mkTuple2 (s2)) ("()")) } } }; var get = function (r) {  return function (s) {  return ((mkTuple2 (s)) (s)) } }; var runReaderState = function (m) {  return function (r) {  return function (s) {  return ((m (r)) (s)) } } }; var freshName = ((bind (get)) (function (state) { var index = (snd (state)); return ((bind ((put (((mkTuple2 ((fst (state)))) ((index + (1)))))))) (function (i2) {  return (unit ((mkTyVar (("T" + (index)))))) })) })); var prettyType = function (t) {  return function() { if ((isTyCon (t))) { var p = (extractTyCon (t)); return (((fst (p)) + (" ")) + ((((foldLeft (function (acc) {  return function (x) {  return (acc + ((prettyType (x)))) } })) ("")) ((snd (p)))))) } else { return function() { if ((isTyVar (t))) { return (extractTyVar (t)) } else { return function() { if ((isTyLam (t))) { var p = (extractTyLam (t)); return (((("(" + ((prettyType ((fst (p)))))) + (" -> ")) + ((prettyType ((snd (p)))))) + (")")) } else { return (error ("This should not happen")) } }() } }() } }() }; var integerCon = ((mkTyCon ("Int")) (empty)); var floatCon = ((mkTyCon ("Float")) (empty)); var charCon = ((mkTyCon ("Char")) (empty)); var stringCon = ((mkTyCon ("String")) (empty)); var literalToType = function (lit) {  return function() { if ((isInt (lit))) { return integerCon } else { return function() { if ((isFloat (lit))) { return floatCon } else { return function() { if ((isChar (lit))) { return charCon } else { return function() { if ((isString (lit))) { return stringCon } else { return (error ("This should not happen")) } }() } }() } }() } }() }; var lookup = function (k) {  return function (dict) {  return function() { if (((containsKey (k)) (dict))) { return ((findValue (k)) (dict)) } else { return (mkTyVar (k)) } }() } }; var equalType = function (t1) {  return function (t2) {  return (((isTyVar (t1)) && ((isTyVar (t2)))) && (((extractTyVar (t1)) == ((extractTyVar (t2)))))) } }; var subst = function (t) {  return function (s) {  return function() { if ((isTyVar (t))) { var n = (extractTyVar (t)); var t2 = ((lookup (n)) (s)); return function() { if (((equalType (t)) (t2))) { return t2 } else { return ((subst (t2)) (s)) } }() } else { return function() { if ((isTyLam (t))) { var p = (extractTyLam (t)); return ((mkTyLam (((subst ((fst (p)))) (s)))) (((subst ((snd (p)))) (s)))) } else { return function() { if ((isTyCon (t))) { var p = (extractTyCon (t)); return ((mkTyCon ((fst (p)))) (((fmap (function (x) {  return ((subst (x)) (s)) })) ((snd (p)))))) } else { return (error ("This should not happen")) } }() } }() } }() } }; var getTVarsOfType = function (t) {  return function() { if ((isTyVar (t))) { return (singletonSet ((extractTyVar (t)))) } else { return function() { if ((isTyLam (t))) { var p = (extractTyLam (t)); return ((union ((getTVarsOfType ((fst (p)))))) ((getTVarsOfType ((snd (p)))))) } else { return function() { if ((isTyCon (t))) { var p = (extractTyCon (t)); return (((foldLeft (function (acc) {  return function (x) {  return ((union (acc)) ((getTVarsOfType (x)))) } })) (emptySet)) ((snd (p)))) } else { return (error ("This should not happen")) } }() } }() } }() }; var fold2 = function (f) {  return function (acc) {  return function (xs) {  return function (ys) {  return function() { if (((isEmpty (xs)) && ((isEmpty (ys))))) { return acc } else { return ((((fold2 (f)) ((((f (acc)) ((head (xs)))) ((head (ys)))))) ((tail (xs)))) ((tail (ys)))) } }() } } } }; var mgu = function (a) {  return function (b) {  return function (s) { var p = ((mkTuple2 (((subst (a)) (s)))) (((subst (b)) (s)))); return function() { if ((((isTyVar ((fst (p)))) && ((isTyVar ((snd (p)))))) && (((extractTyVar ((fst (p)))) == ((extractTyVar ((snd (p))))))))) { return s } else { return function() { if (((isTyVar ((fst (p)))) && ((not (((setContains ((extractTyVar ((fst (p)))))) ((getTVarsOfType ((snd (p))))))))))) { return (((addEntry ((extractTyVar ((fst (p)))))) (b)) (s)) } else { return function() { if ((isTyVar ((snd (p))))) { return (((mgu (b)) (a)) (s)) } else { return function() { if (((isTyLam ((fst (p)))) && ((isTyLam ((snd (p))))))) { var p1 = (extractTyLam ((fst (p)))); var p2 = (extractTyLam ((snd (p)))); return (((mgu ((fst (p1)))) ((fst (p2)))) ((((mgu ((snd (p1)))) ((snd (p2)))) (s)))) } else { return function() { if ((((isTyCon ((fst (p)))) && ((isTyCon ((snd (p)))))) && (((fst ((extractTyCon ((fst (p)))))) == ((fst ((extractTyCon ((snd (p))))))))))) { return ((((fold2 (function (subst) {  return function (t1) {  return function (t2) {  return (((mgu (t1)) (t2)) (subst)) } } })) (s)) ((snd ((extractTyCon ((fst (p)))))))) ((snd ((extractTyCon ((snd (p)))))))) } else { return (error ("This should not happen")) } }() } }() } }() } }() } }() } } }; var tp = function (e) {  return (((((((cataExp (e)) (function (x) {  return ((bind (ask)) (function (env) {  return ((bind (get)) (function (state) { var baseType = (snd (env)); var subs = (fst (state)); var index = (snd (state)); var newSubs = (((mgu ((literalToType (x)))) (baseType)) (subs)); return ((bind ((put (((mkTuple2 (newSubs)) (index)))))) (function (ig) {  return (unit ((elit (x)))) })) })) })) })) (function (s) {  return ((bind (ask)) (function (env) { var ty = ((findValue (s)) ((fst (env)))); return ((bind (get)) (function (state) { var baseType = (snd (env)); var subs = (fst (state)); var index = (snd (state)); var newSubs = (((mgu (ty)) (baseType)) (subs)); return ((bind ((put (((mkTuple2 (newSubs)) (index)))))) (function (ig) {  return (unit ((evar (s)))) })) })) })) })) (function (s) {  return function (e) {  return ((bind (ask)) (function (env) {  return ((bind (freshName)) (function (t1) {  return ((bind (freshName)) (function (t2) {  return ((bind (get)) (function (state) { var baseType = (snd (env)); var subs = (fst (state)); var index = (snd (state)); var newSubs = (((mgu (baseType)) (((mkTyLam (t1)) (t2)))) (subs)); return ((bind ((put (((mkTuple2 (newSubs)) (index)))))) (function (ig) {  return ((bind (((local (function (pair) {  return ((mkTuple2 ((((addEntry (s)) (t1)) ((fst (pair)))))) (t2)) })) (e)))) (function (e2) {  return (unit (((elam (s)) (e2)))) })) })) })) })) })) })) } })) (function (e1) {  return function (e2) {  return ((bind (ask)) (function (env) {  return ((bind (freshName)) (function (t1) {  return ((bind (((local (function (pair) {  return ((mkTuple2 ((fst (pair)))) (((mkTyLam (t1)) ((snd (pair)))))) })) (e1)))) (function (e1p) {  return ((bind (((local (function (pair) {  return ((mkTuple2 ((fst (pair)))) (t1)) })) (e2)))) (function (e2p) {  return (unit (((eapp (e1p)) (e2p)))) })) })) })) })) } })) (function (s) {  return function (e1) {  return function (e2) {  return (error ("")) } } })) (function (p) {  return function (e1) {  return function (e2) {  return (error ("")) } } })) }; var predefTypeEnv = (((addEntry ("+")) (((mkTyLam (integerCon)) (((mkTyLam (integerCon)) (integerCon)))))) ((((addEntry ("-")) (((mkTyLam (integerCon)) (((mkTyLam (integerCon)) (integerCon)))))) (emptyDict)))); var typeOf = function (e) { var baseType = (mkTyVar ("TBase")); var ret = (((runReaderState ((tp (e)))) (((mkTuple2 (predefTypeEnv)) (baseType)))) (((mkTuple2 (emptyDict)) (0)))); return ((subst (baseType)) ((fst ((fst (ret)))))) }; exports.bind = bind; exports.unit = unit; exports.ask = ask; exports.local = local; exports.put = put; exports.get = get; exports.runReaderState = runReaderState; exports.freshName = freshName; exports.prettyType = prettyType; exports.integerCon = integerCon; exports.floatCon = floatCon; exports.charCon = charCon; exports.stringCon = stringCon; exports.literalToType = literalToType; exports.lookup = lookup; exports.equalType = equalType; exports.subst = subst; exports.getTVarsOfType = getTVarsOfType; exports.fold2 = fold2; exports.mgu = mgu; exports.tp = tp; exports.predefTypeEnv = predefTypeEnv; exports.typeOf = typeOf; 