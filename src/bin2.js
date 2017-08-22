var fs = require('fs')

var Cons = function(head, tail) {
  this.head = head;
  this.tail = tail;
};

Cons.prototype.isEmpty = false;

var Nil = {
  isEmpty: true,

  get head() {
    throw new Error('Accessing head on empty list.');
  },

  get tail() {
    throw new Error('Accessing tail on empty list.');
  }
};

var nil = Nil;

var cons = function (x) {
  return function (xs) {
    return new Cons(x, xs);
  }
}

var isEmpty = function (xs) { return xs.isEmpty; }

var head = function (xs) { return xs.head; }

var tail = function (xs) { return xs.tail; }

var isSpace = function (ch) {
  return " \t\n\r\v".indexOf(ch) != -1;
}

var isLower = function (str) {
  return str.length === 1 && str.match(/[a-z]/i);
}

var isUpper = function (str) {
  return str.length === 1 && str.match(/[A-Z]/i);
}

var empty = Nil;

var mkTuple2 = function (x) {
  return function(y) {
    return { fst: x, snd: y };
  };
};

var fst = function (p) { return p.fst; };

var snd = function (p) { return p.snd; };

var stringToCharList = function (s) {
   if (s == "") {
    return empty;
   } else {
   return cons (s.charAt(0)) (stringToCharList (s.slice(1)));
 };
};

var charListToString = function (xs) {
   if (isEmpty(xs)) {
     return "";
   } else {
     var x = head(xs);
     return x + charListToString(tail(xs));
   }
};

var stringToInt = function (s) {
  return parseInt(s);
};

var size = function (x) {
  var ret = Math.pow(10, (x.toString().length));
  return ret;
};

var notElem = function (x) {
  return function (ys) {
    if (x == head(ys)) {
      return false;
    } else {
      var ys2 = tail(ys);
      return isEmpty(ys2) || notElem(x)(ys2);
    }
  };
};

var evar = function (s) {
  return { tag: "Var", args: s };
}

var elitS = function (s) {
  return { tag: "Lit", args: s }
}

var elit = function (s) {
  return { tag: "Lit", args: s }
}

var eapp = function (e1) {
  return function (e2) {
    return { tag: "App", args: { left: e1, right: e2 } }
  }
}

var elet = function (s) {
  return function (e1) {
    return function (e2) {
      return { tag: "Let", args: { name: s, value: e1, body: e2  } }
    }
  }
}

var eIfThenElse = function (p) {
  return function (e1) {
    return function (e2) {
      return { tag: "IfThenElse", args: { predExp: p, thenExp: e1, elseExp: e2 } }
    }
  }
}

var elam = function (s) {
  return function (e) {
    return { tag: "Lam", args: { name: s, body: e } }
  }
}

var isLit = function (e) {
  return e.tag == "Lit"
}

var isVar = function (e) {
  return e.tag == "Var"
}

var isLam = function (e) {
  return e.tag == "Lam"
}

var isApp = function (e) {
  return e.tag == "App"
}

var isLet = function (e) {
  return e.tag == "Let"
}

var isIfThenElse = function (e) {
  return e.tag == "IfThenElse"
}

var error = function (s) {
  console.log(s)
  return null
}

var extractLet = function (e) {
  return { fst: { fst: e.args.name, snd: e.args.value }, snd: e.args.body }
}

var extractLam = function (e) {
  return { fst: e.args.name, snd: e.args.body }
}

var extractIfThenElse = function (e) {
  return { fst: e.args.predExp, snd: { fst: e.args.thenExp, snd: e.args.elseExp } }
}

var extractApp = function (e) {
  return { fst: e.args.left, snd: e.args.right }
}

var extractVar = function (e) {
  return e.args;
}

var extractLit = function (e) {
  return e.args;
}

function addslashes (str) {
  return (str + '')
    .replace(/[\\"']/g, '\\$&')
    .replace(/\u0000/g, '\\0')
}

var show = function (e) {
  if (typeof e == "string")
    return "\"" + addslashes(e) + "\"";
  else return e.toString();
}

var startsWith = function (s1) {
  return function (s2) {
    return s2.startsWith(s1);
  }
}

var readFile = function (path) {
  return fs.readFileSync(path, 'UTF8');
}

var writeFile = function (path) {
  return function (content) {
    fs.writeFileSync(path, content)
    console.log(content)
    console.log(path + " file saved.")
  }
}

var replaceExtensionToJs = function (s) {
  return s.substr(0, s.lastIndexOf(".")) + ".js";
}
; var main = function(inputFilePath){ var compose = function (f) {  return function (g) {  return function (x) {  return (f ((g (x)))) } } }; var foldLeft = function (f) {  return function (v) {  return function (xs) {  return function() { if ((isEmpty (xs))) { return v } else { return (((foldLeft (f)) (((f (v)) ((head (xs)))))) ((tail (xs)))) } }() } } }; var id = function (x) {  return x }; var foldRight = function (f) {  return function (v) {  return function (xs) {  return ((((foldLeft (function (g) {  return function (b) {  return function (x) {  return (g (((f (b)) (x)))) } } })) (id)) (xs)) (v)) } } }; var append = function (xs) {  return function (ys) {  return function() { if ((isEmpty (xs))) { return ys } else { return ((cons ((head (xs)))) (((append ((tail (xs)))) (ys)))) } }() } }; var concat = function (xss) {  return function() { if ((isEmpty (xss))) { return nil } else { return ((append ((head (xss)))) ((concat ((tail (xss)))))) } }() }; var range = function (startIndex) {  return function (endIndex) { var range2 = function (acc) {  return function (endIndex) {  return function() { if ((startIndex > (endIndex))) { return acc } else { return ((range2 (((cons (endIndex)) (acc)))) ((endIndex - (1)))) } }() } }; return ((range2 (nil)) (endIndex)) } }; var fmap = function (f) {  return function (xs) {  return (((foldRight (function (x) {  return function (acc) {  return ((cons ((f (x)))) (acc)) } })) (nil)) (xs)) } }; var flatMap = function (f) {  return ((compose (concat)) ((fmap (f)))) }; var isDigit = function (ch) {  return ((((((((((ch == ("0")) || ((ch == ("1")))) || ((ch == ("2")))) || ((ch == ("3")))) || ((ch == ("4")))) || ((ch == ("5")))) || ((ch == ("6")))) || ((ch == ("7")))) || ((ch == ("8")))) || ((ch == ("9")))) }; var bind = function (m) {  return function (f) {  return function (cs) { var xs = (m (cs)); return function() { if ((isEmpty (xs))) { return empty } else { var n = (head (xs)); return ((f ((fst (n)))) ((snd (n)))) } }() } } }; var unit = function (x) {  return function (cs) {  return ((cons (((mkTuple2 (x)) (cs)))) (empty)) } }; var fail = function (cs) {  return empty }; var item = function (cs) {  return function() { if ((isEmpty (cs))) { return empty } else { var ch = (head (cs)); return ((cons (((mkTuple2 (ch)) ((tail (cs)))))) (empty)) } }() }; var sep = function (p) {  return function (q) {  return ((bind (p)) (function (x) {  return ((bind (q)) (function (y) {  return (unit (((mkTuple2 (x)) (y)))) })) })) } }; var sat = function (p) {  return ((bind (item)) (function (ch) {  return function() { if ((p (ch))) { return (unit (ch)) } else { return fail } }() })) }; var chr = function (x) {  return (sat (function (ch) {  return (ch == (x)) })) }; var choice = function (p) {  return function (q) {  return function (cs) { var xs = (p (cs)); return function() { if ((isEmpty (xs))) { return (q (cs)) } else { return xs } }() } } }; var many = function (p) {  return ((choice (((bind (p)) (function (x) {  return ((bind ((many (p)))) (function (xs) {  return (unit (((cons (x)) (xs)))) })) })))) ((unit (empty)))) }; var digit = (sat (isDigit)); var lower = (sat (isLower)); var upper = (sat (isUpper)); var letter = ((choice (lower)) (upper)); var alphanum = ((choice (letter)) (digit)); var word = (many (letter)); var space = ((bind ((sat (isSpace)))) (function (x) {  return (unit ("")) })); var spaces = ((bind ((many (space)))) (function (x) {  return (unit ("")) })); var str = function (txt) { var xs = (stringToCharList (txt)); var str2 = function (xs) {  return function() { if ((isEmpty (xs))) { return (unit (empty)) } else { var x = (head (xs)); var xs2 = (tail (xs)); return ((bind ((chr (x)))) (function (a) {  return ((bind ((str2 (xs2)))) (function (b) {  return (unit (((cons (x)) (xs2)))) })) })) } }() }; return ((bind ((str2 (xs)))) (function (ys) {  return (unit ((charListToString (ys)))) })) }; var junk = ((bind ((many (space)))) (function (a) {  return (unit ("")) })); var token = function (p) {  return ((bind (p)) (function (x) {  return ((bind (junk)) (function (a) {  return (unit (x)) })) })) }; var parse = function (p) {  return ((bind (junk)) (function (a) {  return p })) }; var symbol = function (txt) {  return (token ((str (txt)))) }; var ident = ((bind (lower)) (function (x) {  return ((bind ((many (alphanum)))) (function (xs) {  return (unit ((charListToString (((cons (x)) (xs)))))) })) })); var many1 = function (p) {  return ((bind (p)) (function (x) {  return ((bind ((many (p)))) (function (xs) {  return (unit (((cons (x)) (xs)))) })) })) }; var nat = ((bind ((many1 (digit)))) (function (xs) {  return (unit ((stringToInt ((charListToString (xs)))))) })); var integer = function () { var op = ((choice (((bind ((chr ("-")))) (function (x) {  return (unit (neg)) })))) ((unit (function (x) {  return x })))); return ((bind (op)) (function (f) {  return ((bind (nat)) (function (n) {  return (unit ((f (n)))) })) })) }();; var makeFloat = function (m) {  return function (n) {  return function() { if ((m > (0))) { return (m + ((n * ((1 / ((size (n)))))))) } else { return (m - ((n * ((1 / ((size (n)))))))) } }() } }; var float = ((bind (integer)) (function (m) {  return ((bind ((symbol (".")))) (function (x) {  return ((bind (nat)) (function (n) {  return (unit (((makeFloat (m)) (n)))) })) })) })); var identifier = function (ks) {  return ((bind ((token (ident)))) (function (x) {  return function() { if (((notElem (x)) (ks))) { return (unit (x)) } else { return fail } }() })) }; var variable = (identifier (((cons ("let")) (((cons ("in")) (((cons ("if")) (((cons ("then")) (((cons ("else")) (empty)))))))))))); var vaR = ((bind (variable)) (function (s) {  return (unit ((evar (s)))) })); var noneOf = function (cs) {  return (sat (function (c) {  return ((notElem (c)) (cs)) })) }; var escapedQuotes = ((bind ((chr ("\\")))) (function (x) {  return ((bind ((chr ("\"")))) (function (y) {  return (unit ("\"")) })) })); var escapedBackslash = ((bind ((chr ("\\")))) (function (x) {  return ((bind ((chr ("\\")))) (function (y) {  return (unit ("\\")) })) })); var escaped = ((choice (escapedQuotes)) (escapedBackslash)); var chars = ((choice (escaped)) ((noneOf ((stringToCharList ("\\\"")))))); var quotedString = ((bind ((chr ("\"")))) (function (x) {  return ((bind ((many (chars)))) (function (xs) {  return ((bind ((chr ("\"")))) (function (z) {  return (unit ((charListToString (xs)))) })) })) })); var lit = ((choice (((bind ((token (integer)))) (function (n) {  return (unit ((elit (n)))) })))) (((bind ((token (quotedString)))) (function (s) {  return (unit ((elitS (s)))) })))); var chainl1 = function (p) {  return function (op) { var rest = function (x) {  return ((choice (((bind (op)) (function (f) {  return ((bind (p)) (function (y) {  return (rest (((f (x)) (y)))) })) })))) ((unit (x)))) }; return ((bind (p)) (rest)) } }; var chainr1 = function (p) {  return function (op) { var rest = function (x) {  return ((choice (((bind (op)) (function (f) {  return ((bind (((chainr1 (p)) (op)))) (function (y) {  return (unit (((f (x)) (y)))) })) })))) ((unit (x)))) }; return ((bind (p)) (rest)) } }; var infixOp = function (s) {  return function (x) {  return function (y) {  return ((eapp (((eapp ((evar (s)))) (x)))) (y)) } } }; var addOp = ((choice (((bind ((symbol ("+")))) (function (x) {  return (unit ((infixOp ("+")))) })))) (((bind ((symbol ("-")))) (function (x) {  return (unit ((infixOp ("-")))) })))); var mulOp = ((choice (((bind ((symbol ("*")))) (function (x) {  return (unit ((infixOp ("*")))) })))) (((bind ((symbol ("/")))) (function (x) {  return (unit ((infixOp ("/")))) })))); var cmpOp = ((choice (((choice (((choice (((bind ((symbol (">")))) (function (x) {  return (unit ((infixOp (">")))) })))) (((bind ((symbol ("<")))) (function (x) {  return (unit ((infixOp ("<")))) })))))) (((bind ((symbol ("==")))) (function (x) {  return (unit ((infixOp ("==")))) })))))) (((bind ((symbol ("||")))) (function (x) {  return (unit ((infixOp ("||")))) })))); var expr = function () { var parens = ((bind ((symbol ("(")))) (function (i1) {  return ((bind (expr)) (function (e) {  return ((bind ((symbol (")")))) (function (i2) {  return (unit (e)) })) })) })); var lam = ((bind ((symbol ("\\")))) (function (i1) {  return ((bind (variable)) (function (s) {  return ((bind ((symbol ("->")))) (function (i2) {  return ((bind (expr)) (function (e) {  return (unit (((elam (s)) (e)))) })) })) })) })); var local = ((bind ((symbol ("let")))) (function (i1) {  return ((bind (variable)) (function (v) {  return ((bind ((symbol ("=")))) (function (i2) {  return ((bind (expr)) (function (e1) {  return ((bind ((symbol ("in")))) (function (i3) {  return ((bind (expr)) (function (e2) {  return (unit ((((elet (v)) (e1)) (e2)))) })) })) })) })) })) })); var ifThenElse = ((bind ((symbol ("if")))) (function (i1) {  return ((bind (expr)) (function (p) {  return ((bind ((symbol ("then")))) (function (i2) {  return ((bind (expr)) (function (e1) {  return ((bind ((symbol ("else")))) (function (i3) {  return ((bind (expr)) (function (e2) {  return (unit ((((eIfThenElse (p)) (e1)) (e2)))) })) })) })) })) })) })); var atom = ((choice (((choice (((choice (((choice (((choice (ifThenElse)) (lam)))) (local)))) (vaR)))) (lit)))) (parens)); return (((foldLeft (chainl1)) (atom)) (((cons (mulOp)) (((cons (addOp)) (((cons (cmpOp)) (((cons ((unit (eapp)))) (empty)))))))))) }();; var cataExp = function (e) {  return function (litF) {  return function (varF) {  return function (lamF) {  return function (appF) {  return function (letF) {  return function (ifThenElseF) { var cata = function (e) {  return function() { if ((isLit (e))) { return (litF ((extractLit (e)))) } else { return function() { if ((isVar (e))) { return (varF ((extractVar (e)))) } else { return function() { if ((isLam (e))) { var pair = (extractLam (e)); var n = (fst (pair)); var b = (snd (pair)); return ((lamF (n)) ((cata (b)))) } else { return function() { if ((isApp (e))) { var pair = (extractApp (e)); var e1 = (fst (pair)); var e2 = (snd (pair)); return ((appF ((cata (e1)))) ((cata (e2)))) } else { return function() { if ((isLet (e))) { var args = (extractLet (e)); var n = (fst ((fst (args)))); var v = (snd ((fst (args)))); var b = (snd (args)); return (((letF (n)) ((cata (v)))) ((cata (b)))) } else { return function() { if ((isIfThenElse (e))) { var args = (extractIfThenElse (e)); var p = (fst (args)); var t = (fst ((snd (args)))); var h = (snd ((snd (args)))); return (((ifThenElseF ((cata (p)))) ((cata (t)))) ((cata (h)))) } else { return (error ("This should not happen")) } }() } }() } }() } }() } }() } }() }; return (cata (e)) } } } } } } }; var pretty = function (e) { var isOp = function (c) {  return (((c == ("-")) || ((c == ("==")))) || ((c == ("*")))) }; return (((((((cataExp (e)) (function (x) {  return (show (x)) })) (function (s) {  return s })) (function (s) {  return function (e) {  return ((("\\" + (s)) + (" -> ")) + (e)) } })) (function (e1) {  return function (e2) {  return function() { if ((isOp (e1))) { return ((e2 + (" ")) + (e1)) } else { return (((("(" + (e1)) + (" ")) + (e2)) + (")")) } }() } })) (function (s) {  return function (e1) {  return function (e2) {  return ((((("let " + (s)) + (" = ")) + (e1)) + (" in ")) + (e2)) } } })) (function (p) {  return function (e1) {  return function (e2) {  return ((((("if " + (p)) + (" then ")) + (e1)) + (" else ")) + (e2)) } } })) }; var toJs = function (e) { var isOp = function (c) {  return (((((((((c == ("+")) || ((c == ("*")))) || ((c == ("==")))) || ((c == ("||")))) || ((c == ("&&")))) || ((c == ("-")))) || ((c == ("/")))) || ((c == (">")))) || ((c == ("<")))) }; return (((((((cataExp (e)) (function (x) {  return (show (x)) })) (function (s) {  return s })) (function (s) {  return function (e) {  return ((((("function (" + (s)) + (") { ")) + (function() { if ((((startsWith ("if")) (e)) || (((startsWith ("var")) (e))))) { return "" } else { return " return " } }())) + (e)) + (" }")) } })) (function (e1) {  return function (e2) {  return function() { if ((isOp (e1))) { return ((e2 + (" ")) + (e1)) } else { return (((("(" + (e1)) + (" (")) + (e2)) + ("))")) } }() } })) (function (s) {  return function (e1) {  return function (e2) {  return ((((("var " + (s)) + (" = ")) + (function() { if (((startsWith ("var")) (e1))) { return (("function () { " + (e1)) + (" }();")) } else { return e1 } }())) + (function() { if ((((startsWith ("if")) (e2)) || (((startsWith ("var")) (e2))))) { return "; " } else { return "; return " } }())) + (e2)) } } })) (function (p) {  return function (e1) {  return function (e2) {  return (((((("function() { if (" + (p)) + (") { ")) + (function() { if ((((startsWith ("if")) (e1)) || (((startsWith ("var")) (e1))))) { return e1 } else { return ("return " + (e1)) } }())) + (" } else { ")) + (function() { if ((((startsWith ("if")) (e2)) || (((startsWith ("var")) (e2))))) { return e2 } else { return ("return " + (e2)) } }())) + (" } }()")) } } })) }; var txt = (readFile (inputFilePath)); var sample = (fst ((head (((parse (expr)) ((stringToCharList (txt)))))))); var externals = (readFile ("Externals.js")); return ((writeFile ((replaceExtensionToJs (inputFilePath)))) ((((externals + ("; var main = function(){ ")) + ((toJs (sample)))) + ("}"))))}