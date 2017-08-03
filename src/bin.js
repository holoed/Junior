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
var main = function(){ var compose = function (f) {  return function (g) {  return function (x) {  return (f ((g (x)))) } } }; var foldLeft = function (f) {  return function (v) {  return function (xs) { if ((isEmpty (xs))) { return v } else { return (((foldLeft (f)) (((f (v)) ((head (xs)))))) ((tail (xs)))) } } } }; var range = function (startIndex) {  return function (endIndex) { var range2 = function (acc) {  return function (endIndex) { if ((startIndex > (endIndex))) { return acc } else { return ((range2 (((cons (endIndex)) (acc)))) ((endIndex - (1)))) } } }; return ((range2 (nil)) (endIndex)) } }; var fac = function (n) {  return (((foldLeft (function (x) {  return function (y) {  return (x * (y)) } })) (1)) (((range (1)) (n)))) }; return (fac (5))}