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
    function TyVar(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TyVar.create = function (value0, value1) {
        return new TyVar(value0, value1);
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

exports.TyCon = TyCon;
exports.TyVar = TyVar;
exports.TyLam = TyLam;
exports.isTyCon = isTyCon;
exports.extractTyCon = extractTyCon;
