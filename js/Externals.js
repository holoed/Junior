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

var elitVoid = { tag: "Lit", args: "()" }

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
