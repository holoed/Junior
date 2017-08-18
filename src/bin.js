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

var isLower = function (x) { return x.toLowerCase() == x; }

var isUpper = function (x) { return x.toUpperCase() == x; }

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
var main = function(){ var compose = function (f) {  return function (g) {  return function (x) {  return (f ((g (x)))) } } }; var foldLeft = function (f) {  return function (v) {  return function (xs) { if ((isEmpty (xs))) { return v } else { return (((foldLeft (f)) (((f (v)) ((head (xs)))))) ((tail (xs)))) } } } }; var id = function (x) {  return x }; var foldRight = function (f) {  return function (v) {  return function (xs) {  return ((((foldLeft (function (g) {  return function (b) {  return function (x) {  return (g (((f (b)) (x)))) } } })) (id)) (xs)) (v)) } } }; var append = function (xs) {  return function (ys) { if ((isEmpty (xs))) { return ys } else { return ((cons ((head (xs)))) (((append ((tail (xs)))) (ys)))) } } }; var concat = function (xss) { if ((isEmpty (xss))) { return nil } else { return ((append ((head (xss)))) ((concat ((tail (xss)))))) } }; var range = function (startIndex) {  return function (endIndex) { var range2 = function (acc) {  return function (endIndex) { if ((startIndex > (endIndex))) { return acc } else { return ((range2 (((cons (endIndex)) (acc)))) ((endIndex - (1)))) } } }; return ((range2 (nil)) (endIndex)) } }; var fmap = function (f) {  return function (xs) {  return (((foldRight (function (x) {  return function (acc) {  return ((cons ((f (x)))) (acc)) } })) (nil)) (xs)) } }; var flatMap = function (f) {  return ((compose (concat)) ((fmap (f)))) }; var isDigit = function (ch) {  return ((((((((((ch == ("0")) || ((ch == ("1")))) || ((ch == ("2")))) || ((ch == ("3")))) || ((ch == ("4")))) || ((ch == ("5")))) || ((ch == ("6")))) || ((ch == ("7")))) || ((ch == ("8")))) || ((ch == ("9")))) }; var bind = function (m) {  return function (f) {  return function (cs) { var xs = (m (cs)); if ((isEmpty (xs))) { return empty } else { var n = (head (xs)); return ((f ((fst (n)))) ((snd (n)))) } } } }; var unit = function (x) {  return function (cs) {  return ((cons (((mkTuple2 (x)) (cs)))) (empty)) } }; var fail = function (cs) {  return empty }; var item = function (cs) { if ((isEmpty (cs))) { return empty } else { var ch = (head (cs)); return ((cons (((mkTuple2 (ch)) ((tail (cs)))))) (empty)) } }; var sep = function (p) {  return function (q) {  return ((bind (p)) (function (x) {  return ((bind (q)) (function (y) {  return (unit (((mkTuple2 (x)) (y)))) })) })) } }; var sat = function (p) {  return ((bind (item)) (function (ch) { if ((p (ch))) { return (unit (ch)) } else { return fail } })) }; var chr = function (x) {  return (sat (function (ch) {  return (ch == (x)) })) }; var choice = function (p) {  return function (q) {  return function (cs) { var xs = (p (cs)); if ((isEmpty (xs))) { return (q (cs)) } else { return xs } } } }; var many = function (p) {  return ((choice (((bind (p)) (function (x) {  return ((bind ((many (p)))) (function (xs) {  return (unit (((cons (x)) (xs)))) })) })))) ((unit (empty)))) }; var digit = (sat (isDigit)); var lower = (sat (isLower)); var upper = (sat (isUpper)); var letter = ((choice (lower)) (upper)); var alphanum = ((choice (letter)) (digit)); var word = (many (letter)); var isSpace = function (ch) {  return (ch == (" ")) }; var space = ((bind ((sat (isSpace)))) (function (x) {  return (unit ("")) })); var spaces = ((bind ((many (space)))) (function (x) {  return (unit ("")) })); var str = function (txt) { var xs = (stringToCharList (txt)); var str2 = function (xs) { if ((isEmpty (xs))) { return (unit (empty)) } else { var x = (head (xs)); var xs2 = (tail (xs)); return ((bind ((chr (x)))) (function (a) {  return ((bind ((str2 (xs2)))) (function (b) {  return (unit (((cons (x)) (xs2)))) })) })) } }; return ((bind ((str2 (xs)))) (function (ys) {  return (unit ((charListToString (ys)))) })) }; var junk = ((bind ((many (space)))) (function (a) {  return (unit ("")) })); var token = function (p) {  return ((bind (p)) (function (x) {  return ((bind (junk)) (function (a) {  return (unit (x)) })) })) }; var parse = function (p) {  return ((bind (junk)) (function (a) {  return p })) }; var symbol = function (txt) {  return (token ((str (txt)))) }; var ident = ((bind (lower)) (function (x) {  return ((bind ((many (alphanum)))) (function (xs) {  return (unit ((charListToString (((cons (x)) (xs)))))) })) })); var many1 = function (p) {  return ((bind (p)) (function (x) {  return ((bind ((many (p)))) (function (xs) {  return (unit (((cons (x)) (xs)))) })) })) }; var nat = ((bind ((many1 (digit)))) (function (xs) {  return (unit ((stringToInt ((charListToString (xs)))))) })); var integer = function () { var op = ((choice (((bind ((chr ("-")))) (function (x) {  return (unit (neg)) })))) ((unit (function (x) {  return x })))); return ((bind (op)) (function (f) {  return ((bind (nat)) (function (n) {  return (unit ((f (n)))) })) })) }();; var makeFloat = function (m) {  return function (n) { if ((m > (0))) { return (m + ((n * ((1 / ((size (n)))))))) } else { return (m - ((n * ((1 / ((size (n)))))))) } } }; var float = ((bind (integer)) (function (m) {  return ((bind ((symbol (".")))) (function (x) {  return ((bind (nat)) (function (n) {  return (unit (((makeFloat (m)) (n)))) })) })) })); return ((parse (float)) ((stringToCharList ("3.14"))))}