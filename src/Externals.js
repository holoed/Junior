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

var cons = function (x) {
  return function (xs) {
    return new Cons(x, xs)
  }
}

var nil = Nil
