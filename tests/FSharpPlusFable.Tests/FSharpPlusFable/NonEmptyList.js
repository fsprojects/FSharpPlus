"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.NonEmptyList$00601$reflection = NonEmptyList$00601$reflection;
exports.NonEmptyList$00601$$get_head = NonEmptyList$00601$$get_head;
exports.NonEmptyList$00601$$get_tail = NonEmptyList$00601$$get_tail;
exports.NonEmptyList$00601$$get_Item = NonEmptyList$00601$$get_Item;
exports.NonEmptyList$00601$$get_GetSlice = NonEmptyList$00601$$get_GetSlice;
exports.NonEmptyList$00601$$get_Length = NonEmptyList$00601$$get_Length;
exports.NonEmptyList$$$create = NonEmptyList$$$create;
exports.NonEmptyList$$$singleton = NonEmptyList$$$singleton;
exports.NonEmptyList$$$toList = NonEmptyList$$$toList;
exports.NonEmptyList$$$toSeq = NonEmptyList$$$toSeq;
exports.NonEmptyList$$$map = NonEmptyList$$$map;
exports.NonEmptyList$$$cons = NonEmptyList$$$cons;
exports.NonEmptyList$$$tails = NonEmptyList$$$tails;
exports.NonEmptyList$00601$$$Map$$3AE9B50E = NonEmptyList$00601$$$Map$$3AE9B50E;
exports.NonEmptyList$00601$$$op_GreaterGreaterEquals$$631FF593 = NonEmptyList$00601$$$op_GreaterGreaterEquals$$631FF593;
exports.NonEmptyList$00601$$$Return$$1505 = NonEmptyList$00601$$$Return$$1505;
exports.NonEmptyList$00601$$$op_LessMultiplyGreater$$558831F5 = NonEmptyList$00601$$$op_LessMultiplyGreater$$558831F5;
exports.NonEmptyList$00601$$$Extract$$7A32E36E = NonEmptyList$00601$$$Extract$$7A32E36E;
exports.NonEmptyList$00601$$$Duplicate$$3E7DA489 = NonEmptyList$00601$$$Duplicate$$3E7DA489;
exports.NonEmptyList$00601$$$op_EqualsGreaterGreater$$Z79925CAD = NonEmptyList$00601$$$op_EqualsGreaterGreater$$Z79925CAD;
exports.NonEmptyList$00601$$$op_Addition$$38E04500 = NonEmptyList$00601$$$op_Addition$$38E04500;
exports.NonEmptyList$00601$$$FoldBack$$35CE0A78 = NonEmptyList$00601$$$FoldBack$$35CE0A78;
exports.NonEmptyList$00601$$$ToList$$Z39731399 = NonEmptyList$00601$$$ToList$$Z39731399;
exports.NonEmptyList$00601$$$ToSeq$$Z395C369E = NonEmptyList$00601$$$ToSeq$$Z395C369E;
exports.NonEmptyList$00601$$$Replace$$Z4DD1F56E = NonEmptyList$00601$$$Replace$$Z4DD1F56E;
exports.NonEmptyList$00601 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

var _List = require("./fable-library.2.3.24/List");

var _Seq = require("./fable-library.2.3.24/Seq");

var _Functor = require("./Functor");

var _Extensions = require("./Extensions");

const NonEmptyList$00601 = (0, _Types.declare)(function FSharpPlus_Data_NonEmptyList(arg1, arg2) {
  this.Head = arg1;
  this.Tail = arg2;
}, _Types.Record);
exports.NonEmptyList$00601 = NonEmptyList$00601;

function NonEmptyList$00601$reflection($gen$$2) {
  return (0, _Reflection.record)("FSharpPlus.Data.NonEmptyList`1", [$gen$$2], NonEmptyList$00601, () => [["Head", $gen$$2], ["Tail", (0, _Reflection.list)($gen$$2)]]);
}

function NonEmptyList$00601$$get_head(this$) {
  return this$.Head;
}

function NonEmptyList$00601$$get_tail(this$$$1) {
  return this$$$1.Tail;
}

function NonEmptyList$00601$$get_Item(this$$$2) {
  return function (_arg1) {
    return _arg1 === 0 ? NonEmptyList$00601$$get_head(this$$$2) : (0, _List.item)(_arg1 - 1, NonEmptyList$00601$$get_tail(this$$$2));
  };
}

function NonEmptyList$00601$$get_GetSlice(this$$$3) {
  return function (_arg2) {
    var b, a$$3, b$$1, a$$2;
    return _arg2[0] != null ? _arg2[0] === 0 ? _arg2[1] != null ? _arg2[1] === 0 ? this$$$3 : (b = _arg2[1] | 0, new NonEmptyList$00601(this$$$3.Head, (0, _List.slice)(null, b - 1, this$$$3.Tail))) : this$$$3 : _arg2[1] != null ? (a$$3 = _arg2[0] | 0, b$$1 = _arg2[1] | 0, new NonEmptyList$00601((0, _List.item)(a$$3 - 1, this$$$3.Tail), (0, _List.slice)(a$$3, b$$1 - 1, this$$$3.Tail))) : (a$$2 = _arg2[0] | 0, new NonEmptyList$00601((0, _List.item)(a$$2 - 1, this$$$3.Tail), (0, _List.slice)(a$$2, null, this$$$3.Tail))) : _arg2[1] != null ? (b = _arg2[1] | 0, new NonEmptyList$00601(this$$$3.Head, (0, _List.slice)(null, b - 1, this$$$3.Tail))) : this$$$3;
  };
}

function NonEmptyList$00601$$get_Length(this$$$4) {
  return 1 + (0, _List.length)(this$$$4.Tail);
}

NonEmptyList$00601.prototype[Symbol.iterator] = function () {
  const x$$1 = this;
  return (0, _Seq.toIterator)((0, _Seq.getEnumerator)(new _Types.List(x$$1.Head, x$$1.Tail)));
};

Object.defineProperty(NonEmptyList$00601.prototype, "Count", {
  "get": function () {
    const s = this;
    return 1 + (0, _List.length)(s.Tail) | 0;
  }
});

NonEmptyList$00601.prototype.Item = function (index) {
  const s$$1 = this;
  return NonEmptyList$00601$$get_Item(s$$1)(index);
};

function NonEmptyList$$$create(x$$3, xs$$4) {
  return new NonEmptyList$00601(x$$3, xs$$4);
}

function NonEmptyList$$$singleton(x$$4) {
  return new NonEmptyList$00601(x$$4, new _Types.List());
}

function NonEmptyList$$$toList(_arg1$$1) {
  return new _Types.List(_arg1$$1.Head, _arg1$$1.Tail);
}

function NonEmptyList$$$toSeq(_arg1$$2) {
  return (0, _Seq.delay)(function () {
    return (0, _Seq.append)((0, _Seq.singleton)(_arg1$$2.Head), (0, _Seq.delay)(function () {
      return _arg1$$2.Tail;
    }));
  });
}

function NonEmptyList$$$map(f, _arg1$$3) {
  return new NonEmptyList$00601(f(_arg1$$3.Head), (0, _List.map)(f, _arg1$$3.Tail));
}

function NonEmptyList$$$cons(e, _arg1$$4) {
  return new NonEmptyList$00601(e, new _Types.List(_arg1$$4.Head, _arg1$$4.Tail));
}

function NonEmptyList$$$tails(s$$2) {
  if (s$$2.Tail.tail != null) {
    return NonEmptyList$$$cons(s$$2, NonEmptyList$$$tails(new NonEmptyList$00601(s$$2.Tail.head, s$$2.Tail.tail)));
  } else {
    return new NonEmptyList$00601(s$$2, new _Types.List());
  }
}

function NonEmptyList$00601$$$Map$$3AE9B50E(x$$9, f$$1) {
  return NonEmptyList$$$map(f$$1, x$$9);
}

function NonEmptyList$00601$$$op_GreaterGreaterEquals$$631FF593(_arg1$$5, f$$2) {
  const patternInput = f$$2(_arg1$$5.Head);
  const ys$0027 = (0, _List.collect)(function ($arg$$1) {
    const arg00$0040 = f$$2($arg$$1);
    return NonEmptyList$$$toList(arg00$0040);
  }, _arg1$$5.Tail);
  return new NonEmptyList$00601(patternInput.Head, (0, _List.append)(patternInput.Tail, ys$0027));
}

function NonEmptyList$00601$$$Return$$1505(x$$11) {
  return new NonEmptyList$00601(x$$11, new _Types.List());
}

function NonEmptyList$00601$$$op_LessMultiplyGreater$$558831F5(f$$3, x$$12) {
  let r;
  const x$$13 = NonEmptyList$$$toList(f$$3);
  const y$$1 = NonEmptyList$$$toList(x$$12);
  r = (0, _Functor.Control$002EApply$$$$003C$002A$003E$$ZAE52212)(x$$13, y$$1, null, null);
  return new NonEmptyList$00601((0, _List.head)(r), (0, _List.tail)(r));
}

function NonEmptyList$00601$$$Extract$$7A32E36E(_arg2$$1) {
  return _arg2$$1.Head;
}

function NonEmptyList$00601$$$Duplicate$$3E7DA489(s$$3, _impl) {
  return NonEmptyList$$$tails(s$$3);
}

function NonEmptyList$00601$$$op_EqualsGreaterGreater$$Z79925CAD(s$$4, g) {
  return NonEmptyList$$$map(g, NonEmptyList$$$tails(s$$4));
}

function NonEmptyList$00601$$$op_Addition$$38E04500(_arg3, x$$15) {
  return new NonEmptyList$00601(_arg3.Head, (0, _List.append)(_arg3.Tail, NonEmptyList$$$toList(x$$15)));
}

function NonEmptyList$00601$$$FoldBack$$35CE0A78(_arg4, f$$5, z) {
  return (0, _List.foldBack)(f$$5, new _Types.List(_arg4.Head, _arg4.Tail), z);
}

function NonEmptyList$00601$$$ToList$$Z39731399(s$$5, _impl$$1) {
  return NonEmptyList$$$toList(s$$5);
}

function NonEmptyList$00601$$$ToSeq$$Z395C369E(s$$6, _impl$$2) {
  const list = NonEmptyList$$$toList(s$$6);
  return list;
}

function NonEmptyList$00601$$$Replace$$Z4DD1F56E(source, oldValue, newValue, _impl$$3) {
  let lst;
  let source$$2;
  let source$$1;
  source$$1 = NonEmptyList$$$toSeq(source);
  source$$2 = (0, _Extensions.Seq$$$replace)(oldValue, newValue, source$$1);
  lst = (0, _List.ofSeq)(source$$2);
  return new NonEmptyList$00601((0, _List.head)(lst), (0, _List.tail)(lst));
}