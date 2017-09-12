var TyCon = (function () {
    function TyCon(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TyCon.create = function (value0, value1) {
        return new TyCon(value0, value1);
    };
    return TyCon;
})();

var TyVar = (function () {
    function TyVar(value0) {
        this.value0 = value0;
    };
    TyVar.create = function (value0) {
        return new TyVar(value0);
    };
    return TyVar;
})();

var TyLam = (function () {
    function TyLam(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TyLam.create = function (value0, value1) {
        return new TyLam(value0, value1);
    };
    return TyLam;
})();

var isTyCon = function (x) {
  return (x instanceof TyCon);
};

var extractTyCon = function (x) {
  return { fst: x.value0, snd: x.value1 }
}

var isTyVar = function (x) {
  return (x instanceof TyVar);
}

var extractTyVar = function (x) {
  return x.value0;
}

var isTyLam = function (x) {
  return (x instanceof TyLam);
}

var extractTyLam = function (x) {
  return { fst: x.value0, snd: x.value1 }
}

var mkTyCon = function (x) {
  return function (y) {
    return TyCon.create(x, y);
  }
}

var isFloat = function(n) {
  return n === +n && n !== (n|0);
}

var isInt = function(n) {
  return n === +n && n === (n|0);
}

var isString = function(x) {
  return (typeof x === 'string' || x instanceof String);
}

var isChar = function(x) {
  return isString(x) && x.length == 1;
}

exports.TyCon = TyCon;
exports.TyVar = TyVar;
exports.TyLam = TyLam;
exports.isTyCon = isTyCon;
exports.isTyVar = isTyVar;
exports.isTyLam = isTyLam;
exports.extractTyCon = extractTyCon;
exports.extractTyVar = extractTyVar;
exports.extractTyLam = extractTyLam;
exports.mkTyCon = mkTyCon;
exports.isFloat = isFloat;
exports.isInt = isInt;
exports.isString = isString;
exports.isChar = isChar;
