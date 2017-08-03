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
var main = function(){ var compose = function (f) {  return function (g) {  return function (x) {  return (f ((g (x)))) } } }; var foldLeft = function (f) {  return function (v) {  return function (xs) { if ((isEmpty (xs))) { return v } else { return (((foldLeft (f)) (((f (v)) ((head (xs)))))) ((tail (xs)))) } } } }; var id = function (x) {  return x }; var foldRight = function (f) {  return function (v) {  return function (xs) {  return ((((foldLeft (function (g) {  return function (b) {  return function (x) {  return (g (((f (b)) (x)))) } } })) (id)) (xs)) (v)) } } }; var append = function (xs) {  return function (ys) { if ((isEmpty (xs))) { return ys } else { return ((cons ((head (xs)))) (((append ((tail (xs)))) (ys)))) } } }; var concat = function (xss) { if ((isEmpty (xss))) { return nil } else { return ((append ((head (xss)))) ((concat ((tail (xss)))))) } }; var range = function (startIndex) {  return function (endIndex) { var range2 = function (acc) {  return function (endIndex) { if ((startIndex > (endIndex))) { return acc } else { return ((range2 (((cons (endIndex)) (acc)))) ((endIndex - (1)))) } } }; return ((range2 (nil)) (endIndex)) } }; var fmap = function (f) {  return function (xs) {  return (((foldRight (function (x) {  return function (acc) {  return ((cons ((f (x)))) (acc)) } })) (nil)) (xs)) } }; var flatMap = function (f) {  return ((compose (concat)) ((fmap (f)))) }; var fac = function (n) {  return (((foldRight (function (x) {  return function (y) {  return (x * (y)) } })) (1)) (((range (1)) (n)))) }; var xs = (concat (((append (((cons (((cons ((fac (5)))) (nil)))) (nil)))) (((cons (((cons ((fac (6)))) (nil)))) (nil)))))); return ((flatMap (function (x) {  return ((cons ((x * (2)))) (((cons (x)) (nil)))) })) (xs))}