"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.DListData$00601$reflection = DListData$00601$reflection;
exports.DList$00601$reflection = DList$00601$reflection;
exports.DList$00601$$$$002Ector$$Z2E394EE4 = DList$00601$$$$002Ector$$Z2E394EE4;
exports.DList$00601$$get_dc = DList$00601$$get_dc;
exports.DList$00601$$$ofSeq$$BB573A = DList$00601$$$ofSeq$$BB573A;
exports.DList$00601$$get_Length = DList$00601$$get_Length;
exports.DList$00601$$$foldBack = DList$00601$$$foldBack;
exports.DList$00601$$$fold = DList$00601$$$fold;
exports.DList$00601$$$append$$Z5A626040 = DList$00601$$$append$$Z5A626040;
exports.DList$00601$$$appendLists$$Z6B15200 = DList$00601$$$appendLists$$Z6B15200;
exports.DList$00601$$$head$$Z2B1B5B4A = DList$00601$$$head$$Z2B1B5B4A;
exports.DList$00601$$$tryHead$$Z2B1B5B4A = DList$00601$$$tryHead$$Z2B1B5B4A;
exports.DList$00601$$Cons$$2B595 = DList$00601$$Cons$$2B595;
exports.DList$00601$$get_Head = DList$00601$$get_Head;
exports.DList$00601$$get_TryHead = DList$00601$$get_TryHead;
exports.DList$00601$$get_IsEmpty = DList$00601$$get_IsEmpty;
exports.DList$00601$$Add$$2B595 = DList$00601$$Add$$2B595;
exports.DList$00601$$get_Tail = DList$00601$$get_Tail;
exports.DList$00601$$get_TryTail = DList$00601$$get_TryTail;
exports.DList$00601$$get_Uncons = DList$00601$$get_Uncons;
exports.DList$00601$$get_TryUncons = DList$00601$$get_TryUncons;
exports.DList$00601$$get_Item$$Z524259A4 = DList$00601$$get_Item$$Z524259A4;
exports.DList$00601$$toSeq = DList$00601$$toSeq;
exports.DListModule$$$append = DListModule$$$append;
exports.DListModule$$$cons = DListModule$$$cons;
exports.DListModule$$$empty = DListModule$$$empty;
exports.DListModule$$$foldBack = DListModule$$$foldBack;
exports.DListModule$$$fold = DListModule$$$fold;
exports.DListModule$$$singleton = DListModule$$$singleton;
exports.DListModule$$$ofSeq = DListModule$$$ofSeq;
exports.DListModule$$$concat = DListModule$$$concat;
exports.DList$00601$$$get_get_Zero = DList$00601$$$get_get_Zero;
exports.DList$00601$$$op_Addition$$6E8B3740 = DList$00601$$$op_Addition$$6E8B3740;
exports.DList$00601$$$get_get_Empty = DList$00601$$$get_get_Empty;
exports.DList$00601$$$op_LessBarGreater$$6E8B3740 = DList$00601$$$op_LessBarGreater$$6E8B3740;
exports.DList$00601$$$ToSeq$$38EBA746 = DList$00601$$$ToSeq$$38EBA746;
exports.DList$00601$$$ToList$$38EBA746 = DList$00601$$$ToList$$38EBA746;
exports.DList$00601$$$OfSeq$$404FCA0C = DList$00601$$$OfSeq$$404FCA0C;
exports.DList$00601$$$Fold$$138425E6 = DList$00601$$$Fold$$138425E6;
exports.DList$00601$$$Return$$1505 = DList$00601$$$Return$$1505;
exports.DList$00601$$$Map$$2A2B9DD0 = DList$00601$$$Map$$2A2B9DD0;
exports.DList$00601$$$op_LessMultiplyGreater$$315A85B5 = DList$00601$$$op_LessMultiplyGreater$$315A85B5;
exports.DList$00601$$$op_GreaterGreaterEquals$$247C51B3 = DList$00601$$$op_GreaterGreaterEquals$$247C51B3;
exports.DList$00601 = exports.DListData$00601 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

var _Seq = require("./fable-library.2.3.24/Seq");

var _Util = require("./fable-library.2.3.24/Util");

var _Option = require("./fable-library.2.3.24/Option");

var _Extensions = require("./Extensions");

const DListData$00601 = (0, _Types.declare)(function FSharpPlus_Data_DListData(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.DListData$00601 = DListData$00601;

function DListData$00601$reflection($gen$$6) {
  return (0, _Reflection.union)("FSharpPlus.Data.DListData`1", [$gen$$6], DListData$00601, () => ["Nil", ["Unit", [$gen$$6]], ["Join", [DListData$00601$reflection($gen$$6), DListData$00601$reflection($gen$$6)]]]);
}

const DList$00601 = (0, _Types.declare)(function FSharpPlus_Data_DList(length, data) {
  const $this$$1 = this;
  $this$$1.length = length;
  $this$$1.data = data;
  $this$$1.hashCode = null;
});
exports.DList$00601 = DList$00601;

function DList$00601$reflection($gen$$7) {
  return (0, _Reflection.type)("FSharpPlus.Data.DList`1", [$gen$$7]);
}

function DList$00601$$$$002Ector$$Z2E394EE4(length, data) {
  return this instanceof DList$00601 ? DList$00601.call(this, length, data) : new DList$00601(length, data);
}

function DList$00601$$get_dc(this$) {
  return this$.data;
}

function DList$00601$$$ofSeq$$BB573A(s) {
  const tupledArg = (0, _Seq.fold)(function (tupledArg$$1, x) {
    return [tupledArg$$1[0] + 1, tupledArg$$1[1].tag === 1 ? new DListData$00601(2, "Join", tupledArg$$1[1], new DListData$00601(1, "Unit", x)) : tupledArg$$1[1].tag === 2 ? new DListData$00601(2, "Join", tupledArg$$1[1], new DListData$00601(1, "Unit", x)) : new DListData$00601(1, "Unit", x)];
  }, [0, new DListData$00601(0, "Nil")], s);
  return DList$00601$$$$002Ector$$Z2E394EE4(tupledArg[0], tupledArg[1]);
}

DList$00601.prototype.GetHashCode = function () {
  const this$$$1 = this;
  const matchValue = this$$$1.hashCode;

  if (matchValue != null) {
    const hash$$1 = matchValue | 0;
    return hash$$1 | 0;
  } else {
    let hash = 1;
    (0, _Seq.iterate)(function (x$$1) {
      hash = 31 * hash + (0, _Util.structuralHash)(x$$1);
    }, this$$$1);
    this$$$1.hashCode = hash;
    return hash | 0;
  }
};

DList$00601.prototype.Equals = function (other) {
  const this$$$2 = this;
  return other instanceof DList$00601 ? DList$00601$$get_Length(this$$$2) !== DList$00601$$get_Length(other) ? false : (0, _Util.identityHash)(this$$$2) !== (0, _Util.identityHash)(other) ? false : (0, _Seq.forAll2)(_Util.equals, this$$$2, other) : false;
};

function DList$00601$$get_Length(__) {
  return __.length;
}

function DList$00601$$$foldBack(f, l, state$$1) {
  const walk = function walk(lefts) {
    return function (l$$1) {
      return function (xs) {
        return l$$1.tag === 1 ? finish(lefts)(f(l$$1.fields[0], xs)) : l$$1.tag === 2 ? walk(new _Types.List(l$$1.fields[0], lefts))(l$$1.fields[1])(xs) : finish(lefts)(xs);
      };
    };
  };

  const finish = function finish(lefts$$1) {
    return function (xs$$1) {
      return lefts$$1.tail != null ? walk(lefts$$1.tail)(lefts$$1.head)(xs$$1) : xs$$1;
    };
  };

  return walk(new _Types.List())(DList$00601$$get_dc(l))(state$$1);
}

function DList$00601$$$fold(f$$1, state$$2, l$$2) {
  const walk$$1 = function walk$$1(rights) {
    return function (l$$3) {
      return function (xs$$2) {
        return l$$3.tag === 1 ? finish$$1(rights)((0, _Util.curry)(2, f$$1)(xs$$2, l$$3.fields[0])) : l$$3.tag === 2 ? walk$$1(new _Types.List(l$$3.fields[1], rights))(l$$3.fields[0])(xs$$2) : finish$$1(rights)(xs$$2);
      };
    };
  };

  const finish$$1 = function finish$$1(rights$$1) {
    return function (xs$$3) {
      return rights$$1.tail != null ? walk$$1(rights$$1.tail)(rights$$1.head)(xs$$3) : xs$$3;
    };
  };

  return walk$$1(new _Types.List())(DList$00601$$get_dc(l$$2))(state$$2);
}

function DList$00601$$$tryFindi(f$$3, l$$4) {
  const walk$$2 = function walk$$2(rights$$2) {
    return function (l$$5) {
      return function (i$$1) {
        return l$$5.tag === 1 ? (0, _Util.curry)(2, f$$3)(i$$1, l$$5.fields[0]) ? (0, _Option.some)(l$$5.fields[0]) : finish$$2(rights$$2)(i$$1 + 1) : l$$5.tag === 2 ? walk$$2(new _Types.List(l$$5.fields[1], rights$$2))(l$$5.fields[0])(i$$1) : finish$$2(rights$$2)(i$$1);
      };
    };
  };

  const finish$$2 = function finish$$2(rights$$3) {
    return function (xs$$4) {
      return rights$$3.tail != null ? walk$$2(rights$$3.tail)(rights$$3.head)(xs$$4) : null;
    };
  };

  return walk$$2(new _Types.List())(DList$00601$$get_dc(l$$4))(0);
}

function DList$00601$$$findi(f$$5, l$$6) {
  let matchValue$$1;
  matchValue$$1 = DList$00601$$$tryFindi(f$$5, l$$6);

  if (matchValue$$1 == null) {
    throw new Error();
  } else {
    const v = (0, _Option.value)(matchValue$$1);
    return v;
  }
}

function DList$00601$$$append$$Z5A626040(left, right) {
  if (left.tag === 0) {
    return right;
  } else if (right.tag === 0) {
    return left;
  } else {
    return new DListData$00601(2, "Join", left, right);
  }
}

function DList$00601$$$appendLists$$Z6B15200(left$$1, right$$1) {
  return DList$00601$$$$002Ector$$Z2E394EE4(DList$00601$$get_Length(left$$1) + DList$00601$$get_Length(right$$1), DList$00601$$$append$$Z5A626040(DList$00601$$get_dc(left$$1), DList$00601$$get_dc(right$$1)));
}

function DList$00601$$$head$$Z2B1B5B4A($data$$1$$31) {
  DList$00601$$$head$$Z2B1B5B4A: while (true) {
    const data$$1 = $data$$1$$31;

    switch (data$$1.tag) {
      case 1:
        {
          return data$$1.fields[0];
        }

      case 2:
        {
          $data$$1$$31 = data$$1.fields[0];
          continue DList$00601$$$head$$Z2B1B5B4A;
        }

      default:
        {
          throw new Error("DList.head: empty DList");
        }
    }

    break;
  }
}

function DList$00601$$$tryHead$$Z2B1B5B4A($data$$2$$32) {
  DList$00601$$$tryHead$$Z2B1B5B4A: while (true) {
    const data$$2 = $data$$2$$32;

    switch (data$$2.tag) {
      case 1:
        {
          return (0, _Option.some)(data$$2.fields[0]);
        }

      case 2:
        {
          $data$$2$$32 = data$$2.fields[0];
          continue DList$00601$$$tryHead$$Z2B1B5B4A;
        }

      default:
        {
          return null;
        }
    }

    break;
  }
}

function DList$00601$$Cons$$2B595(__$$1, hd) {
  if (__$$1.data.tag === 0) {
    return DList$00601$$$$002Ector$$Z2E394EE4(1, new DListData$00601(1, "Unit", hd));
  } else {
    return DList$00601$$$$002Ector$$Z2E394EE4(__$$1.length + 1, new DListData$00601(2, "Join", new DListData$00601(1, "Unit", hd), __$$1.data));
  }
}

function DList$00601$$get_Head(__$$2) {
  return DList$00601$$$head$$Z2B1B5B4A(__$$2.data);
}

function DList$00601$$get_TryHead(__$$3) {
  return DList$00601$$$tryHead$$Z2B1B5B4A(__$$3.data);
}

function DList$00601$$get_IsEmpty(__$$4) {
  if (__$$4.data.tag === 0) {
    return true;
  } else {
    return false;
  }
}

function DList$00601$$Add$$2B595(__$$5, x$$8) {
  return DList$00601$$$$002Ector$$Z2E394EE4(__$$5.length + 1, DList$00601$$$append$$Z5A626040(__$$5.data, new DListData$00601(1, "Unit", x$$8)));
}

function DList$00601$$get_Tail(this$$$3) {
  const step = function step($xs$$5$$41, $acc$$42) {
    step: while (true) {
      const xs$$5 = $xs$$5$$41,
            acc = $acc$$42;

      switch (xs$$5.tag) {
        case 1:
          {
            return acc;
          }

        case 2:
          {
            $xs$$5$$41 = xs$$5.fields[0];
            $acc$$42 = DList$00601$$$append$$Z5A626040(xs$$5.fields[1], acc);
            continue step;
          }

        default:
          {
            return acc;
          }
      }

      break;
    }
  };

  if (DList$00601$$get_IsEmpty(this$$$3)) {
    throw new Error("DList.tail: empty DList");
  } else {
    return DList$00601$$$$002Ector$$Z2E394EE4(this$$$3.length - 1, step(this$$$3.data, new DListData$00601(0, "Nil")));
  }
}

function DList$00601$$get_TryTail(this$$$4) {
  const step$$1 = function step$$1($xs$$6$$44, $acc$$1$$45) {
    step$$1: while (true) {
      const xs$$6 = $xs$$6$$44,
            acc$$1 = $acc$$1$$45;

      switch (xs$$6.tag) {
        case 1:
          {
            return acc$$1;
          }

        case 2:
          {
            $xs$$6$$44 = xs$$6.fields[0];
            $acc$$1$$45 = DList$00601$$$append$$Z5A626040(xs$$6.fields[1], acc$$1);
            continue step$$1;
          }

        default:
          {
            return acc$$1;
          }
      }

      break;
    }
  };

  if (DList$00601$$get_IsEmpty(this$$$4)) {
    return null;
  } else {
    return DList$00601$$$$002Ector$$Z2E394EE4(this$$$4.length - 1, step$$1(this$$$4.data, new DListData$00601(0, "Nil")));
  }
}

function DList$00601$$get_Uncons(this$$$5) {
  return [DList$00601$$$head$$Z2B1B5B4A(this$$$5.data), DList$00601$$get_Tail(this$$$5)];
}

function DList$00601$$get_TryUncons(this$$$6) {
  const matchValue$$5 = DList$00601$$$tryHead$$Z2B1B5B4A(this$$$6.data);

  if (matchValue$$5 == null) {
    return null;
  } else {
    const x$$11 = (0, _Option.value)(matchValue$$5);
    return [x$$11, DList$00601$$get_Tail(this$$$6)];
  }
}

function DList$00601$$get_Item$$Z524259A4(s$$1, index) {
  if (index < 0 ? true : index >= DList$00601$$get_Length(s$$1)) {
    throw new Error();
  }

  return DList$00601$$$findi(function withIndex(i$$2, _arg1) {
    return i$$2 === index;
  }, s$$1);
}

function DList$00601$$toSeq(__$$6) {
  const walk$$3 = function walk$$3(rights$$4, l$$7) {
    return (0, _Seq.delay)(function () {
      switch (l$$7.tag) {
        case 1:
          {
            return (0, _Seq.append)((0, _Seq.singleton)(l$$7.fields[0]), (0, _Seq.delay)(function () {
              if (rights$$4.tail != null) {
                return walk$$3(rights$$4.tail, rights$$4.head);
              } else {
                return (0, _Seq.empty)();
              }
            }));
          }

        case 2:
          {
            return walk$$3(new _Types.List(l$$7.fields[1], rights$$4), l$$7.fields[0]);
          }

        default:
          {
            if (rights$$4.tail != null) {
              return walk$$3(rights$$4.tail, rights$$4.head);
            } else {
              return (0, _Seq.empty)();
            }
          }
      }
    });
  };

  return (0, _Seq.getEnumerator)(walk$$3(new _Types.List(), __$$6.data));
}

DList$00601.prototype[Symbol.iterator] = function () {
  const s$$2 = this;
  return (0, _Seq.toIterator)(DList$00601$$toSeq(s$$2));
};

Object.defineProperty(DList$00601.prototype, "Count", {
  "get": function () {
    const s$$3 = this;
    return DList$00601$$get_Length(s$$3) | 0;
  }
});

DList$00601.prototype.Item = function (index$$1) {
  const s$$4 = this;
  return DList$00601$$get_Item$$Z524259A4(s$$4, index$$1);
};

function DListModule$$$append(left$$2, right$$2) {
  return DList$00601$$$appendLists$$Z6B15200(left$$2, right$$2);
}

function DListModule$$$cons(hd$$1, l$$8) {
  const matchValue$$6 = DList$00601$$get_Length(l$$8) | 0;

  if (matchValue$$6 === 0) {
    return DList$00601$$$$002Ector$$Z2E394EE4(1, new DListData$00601(1, "Unit", hd$$1));
  } else {
    return DList$00601$$$$002Ector$$Z2E394EE4(DList$00601$$get_Length(l$$8) + 1, new DListData$00601(2, "Join", new DListData$00601(1, "Unit", hd$$1), DList$00601$$get_dc(l$$8)));
  }
}

function DListModule$$$empty() {
  return DList$00601$$$$002Ector$$Z2E394EE4(0, new DListData$00601(0, "Nil"));
}

function DListModule$$$foldBack(f$$6, l$$9, state$$3) {
  return DList$00601$$$foldBack(f$$6, l$$9, state$$3);
}

function DListModule$$$fold(f$$7, state$$4, l$$10) {
  return DList$00601$$$fold(f$$7, state$$4, l$$10);
}

function DListModule$$$singleton(x$$14) {
  return DList$00601$$$$002Ector$$Z2E394EE4(1, new DListData$00601(1, "Unit", x$$14));
}

function DListModule$$$ofSeq(s$$5) {
  return DList$00601$$$ofSeq$$BB573A(s$$5);
}

function DListModule$$$concat(x$$15) {
  const arg10$$4 = DListModule$$$empty();
  return DList$00601$$$fold(DListModule$$$append, arg10$$4, x$$15);
}

function DList$00601$$$get_get_Zero() {
  return DList$00601$$$$002Ector$$Z2E394EE4(0, new DListData$00601(0, "Nil"));
}

function DList$00601$$$op_Addition$$6E8B3740(x$$16, y$$7) {
  return DListModule$$$append(x$$16, y$$7);
}

function DList$00601$$$get_get_Empty() {
  return DList$00601$$$$002Ector$$Z2E394EE4(0, new DListData$00601(0, "Nil"));
}

function DList$00601$$$op_LessBarGreater$$6E8B3740(x$$17, y$$8) {
  return DListModule$$$append(x$$17, y$$8);
}

function DList$00601$$$ToSeq$$38EBA746(x$$18) {
  return x$$18;
}

function DList$00601$$$ToList$$38EBA746(x$$19) {
  return DListModule$$$foldBack(_Extensions.List$$$cons, x$$19, new _Types.List());
}

function DList$00601$$$OfSeq$$404FCA0C(x$$21) {
  return DListModule$$$ofSeq(x$$21);
}

function DList$00601$$$Fold$$138425E6(x$$22, f$$8, z) {
  return DListModule$$$fold(f$$8, x$$22, z);
}

function DList$00601$$$Return$$1505(x$$23) {
  return DListModule$$$singleton(x$$23);
}

function DList$00601$$$Map$$2A2B9DD0(x$$24, f$$9) {
  const arg20$$3 = DListModule$$$empty();
  return DList$00601$$$foldBack((0, _Util.uncurry)(2, function ($arg$$2) {
    const hd$$2 = f$$9($arg$$2);
    return function (l$$13) {
      return DListModule$$$cons(hd$$2, l$$13);
    };
  }), x$$24, arg20$$3);
}

function DList$00601$$$op_LessMultiplyGreater$$315A85B5(f$$11, x$$26) {
  let x$$28;
  const arg20$$5 = DListModule$$$empty();
  x$$28 = DList$00601$$$foldBack((0, _Util.uncurry)(2, function ($arg$$4) {
    let hd$$4;
    const arg20$$4 = DListModule$$$empty();
    hd$$4 = DList$00601$$$foldBack((0, _Util.uncurry)(2, function ($arg$$3) {
      let hd$$3;
      hd$$3 = $arg$$3($arg$$4);
      return function (l$$14) {
        return DListModule$$$cons(hd$$3, l$$14);
      };
    }), f$$11, arg20$$4);
    return function (l$$15) {
      return DListModule$$$cons(hd$$4, l$$15);
    };
  }), x$$26, arg20$$5);
  return DListModule$$$concat(x$$28);
}

function DList$00601$$$op_GreaterGreaterEquals$$247C51B3(x$$31, f$$15) {
  const arg10$$8 = DListModule$$$empty();
  return DList$00601$$$foldBack((0, _Util.uncurry)(2, function ($arg$$5) {
    const left$$4 = f$$15($arg$$5);
    return function (right$$4) {
      return DListModule$$$append(left$$4, right$$4);
    };
  }), arg10$$8, x$$31);
}