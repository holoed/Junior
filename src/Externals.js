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
  return { tag: "Var", value: s };
}

var elitS = function (s) {
  return { tag: "Lit", value: s }
}
