"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.head = head;
exports.tryHead = tryHead;
exports.tail = tail;
exports.last = last;
exports.tryLast = tryLast;
exports.compareWith = compareWith;
exports.foldIndexedAux = foldIndexedAux;
exports.foldIndexed = foldIndexed;
exports.fold = fold;
exports.reverse = reverse;
exports.foldBack = foldBack;
exports.toSeq = toSeq;
exports.ofSeq = ofSeq;
exports.concat = concat;
exports.foldIndexed2Aux = foldIndexed2Aux;
exports.foldIndexed2 = foldIndexed2;
exports.fold2 = fold2;
exports.foldBack2 = foldBack2;
exports.unfold = unfold;
exports.foldIndexed3Aux = foldIndexed3Aux;
exports.foldIndexed3 = foldIndexed3;
exports.fold3 = fold3;
exports.scan = scan;
exports.scanBack = scanBack;
exports.length = length;
exports.append = append;
exports.collect = collect;
exports.map = map;
exports.mapIndexed = mapIndexed;
exports.indexed = indexed;
exports.map2 = map2;
exports.mapIndexed2 = mapIndexed2;
exports.map3 = map3;
exports.mapIndexed3 = mapIndexed3;
exports.mapFold = mapFold;
exports.mapFoldBack = mapFoldBack;
exports.iterate = iterate;
exports.iterate2 = iterate2;
exports.iterateIndexed = iterateIndexed;
exports.iterateIndexed2 = iterateIndexed2;
exports.ofArray = ofArray;
exports.empty = empty;
exports.isEmpty = isEmpty;
exports.tryPickIndexedAux = tryPickIndexedAux;
exports.tryPickIndexed = tryPickIndexed;
exports.tryPick = tryPick;
exports.pick = pick;
exports.tryFindIndexed = tryFindIndexed;
exports.tryFind = tryFind;
exports.findIndexed = findIndexed;
exports.find = find;
exports.findBack = findBack;
exports.tryFindBack = tryFindBack;
exports.tryFindIndex = tryFindIndex;
exports.tryFindIndexBack = tryFindIndexBack;
exports.findIndex = findIndex;
exports.findIndexBack = findIndexBack;
exports.item = item;
exports.tryItem = tryItem;
exports.filter = filter;
exports.partition = partition;
exports.choose = choose;
exports.contains = contains;
exports.except = except;
exports.initialize = initialize;
exports.replicate = replicate;
exports.reduce = reduce;
exports.reduceBack = reduceBack;
exports.forAll = forAll;
exports.forAll2 = forAll2;
exports.exists = exists;
exports.exists2 = exists2;
exports.unzip = unzip;
exports.unzip3 = unzip3;
exports.zip = zip;
exports.zip3 = zip3;
exports.sort = sort;
exports.sortBy = sortBy;
exports.sortDescending = sortDescending;
exports.sortByDescending = sortByDescending;
exports.sortWith = sortWith;
exports.sum = sum;
exports.sumBy = sumBy;
exports.maxBy = maxBy;
exports.max = max;
exports.minBy = minBy;
exports.min = min;
exports.average = average;
exports.averageBy = averageBy;
exports.permute = permute;
exports.skip = skip;
exports.skipWhile = skipWhile;
exports.takeSplitAux = takeSplitAux;
exports.take = take;
exports.takeWhile = takeWhile;
exports.truncate = truncate;
exports.splitAt = splitAt;
exports.outOfRange = outOfRange;
exports.slice = slice;
exports.distinctBy = distinctBy;
exports.distinct = distinct;
exports.exactlyOne = exactlyOne;
exports.groupBy = groupBy;
exports.countBy = countBy;
exports.where = where;
exports.pairwise = pairwise;
exports.windowed = windowed;

var _Option = require("./Option");

var _Types = require("./Types");

var _Seq = require("./Seq");

var _Util = require("./Util");

var _Array = require("./Array");

var _Set = require("./Set");

var _Map = require("./Map");

function head(_arg1) {
  if (_arg1.tail != null) {
    return _arg1.head;
  } else {
    throw new Error("List was empty");
  }
}

function tryHead(_arg1$$1) {
  if (_arg1$$1.tail != null) {
    return (0, _Option.some)(_arg1$$1.head);
  } else {
    return null;
  }
}

function tail(_arg1$$2) {
  if (_arg1$$2.tail != null) {
    return _arg1$$2.tail;
  } else {
    throw new Error("List was empty");
  }
}

function last($_arg1$$3$$5) {
  last: while (true) {
    const _arg1$$3 = $_arg1$$3$$5;

    if (_arg1$$3.tail != null) {
      if (_arg1$$3.tail.tail == null) {
        return _arg1$$3.head;
      } else {
        $_arg1$$3$$5 = _arg1$$3.tail;
        continue last;
      }
    } else {
      throw new Error("List was empty");
    }

    break;
  }
}

function tryLast($_arg1$$4$$6) {
  tryLast: while (true) {
    const _arg1$$4 = $_arg1$$4$$6;

    if (_arg1$$4.tail != null) {
      if (_arg1$$4.tail.tail == null) {
        return (0, _Option.some)(_arg1$$4.head);
      } else {
        $_arg1$$4$$6 = _arg1$$4.tail;
        continue tryLast;
      }
    } else {
      return null;
    }

    break;
  }
}

function compareWith(comparer, xs$$3, ys) {
  if (xs$$3 === ys) {
    return 0;
  } else {
    const loop = function loop($xs$$4$$10, $ys$$1$$11) {
      loop: while (true) {
        const xs$$4 = $xs$$4$$10,
              ys$$1 = $ys$$1$$11;

        if (xs$$4.tail != null) {
          if (ys$$1.tail != null) {
            const matchValue$$1 = comparer(xs$$4.head, ys$$1.head) | 0;

            if (matchValue$$1 === 0) {
              $xs$$4$$10 = xs$$4.tail;
              $ys$$1$$11 = ys$$1.tail;
              continue loop;
            } else {
              return matchValue$$1 | 0;
            }
          } else {
            return 1;
          }
        } else if (ys$$1.tail == null) {
          return 0;
        } else {
          return -1 | 0;
        }

        break;
      }
    };

    return loop(xs$$3, ys) | 0;
  }
}

function foldIndexedAux($f$$12, $i$$13, $acc$$14, $_arg1$$5$$15) {
  foldIndexedAux: while (true) {
    const f = $f$$12,
          i = $i$$13,
          acc = $acc$$14,
          _arg1$$5 = $_arg1$$5$$15;

    if (_arg1$$5.tail != null) {
      $f$$12 = f;
      $i$$13 = i + 1;
      $acc$$14 = f(i, acc, _arg1$$5.head);
      $_arg1$$5$$15 = _arg1$$5.tail;
      continue foldIndexedAux;
    } else {
      return acc;
    }

    break;
  }
}

function foldIndexed(f$$1, state, xs$$7) {
  return foldIndexedAux(f$$1, 0, state, xs$$7);
}

function fold($f$$2$$19, $state$$1$$20, $xs$$8$$21) {
  fold: while (true) {
    const f$$2 = $f$$2$$19,
          state$$1 = $state$$1$$20,
          xs$$8 = $xs$$8$$21;

    if (xs$$8.tail != null) {
      $f$$2$$19 = f$$2;
      $state$$1$$20 = f$$2(state$$1, xs$$8.head);
      $xs$$8$$21 = xs$$8.tail;
      continue fold;
    } else {
      return state$$1;
    }

    break;
  }
}

function reverse(xs$$9) {
  return fold(function (acc$$1, x$$6) {
    return new _Types.List(x$$6, acc$$1);
  }, new _Types.List(), xs$$9);
}

function foldBack(f$$3, xs$$10, state$$2) {
  return fold(function (acc$$2, x$$7) {
    return f$$3(x$$7, acc$$2);
  }, state$$2, reverse(xs$$10));
}

function toSeq(xs$$11) {
  return (0, _Seq.map)(function (x$$8) {
    return x$$8;
  }, xs$$11);
}

function ofSeq(xs$$12) {
  const xs$$13 = (0, _Seq.fold)(function (acc$$3, x$$9) {
    return new _Types.List(x$$9, acc$$3);
  }, new _Types.List(), xs$$12);
  return reverse(xs$$13);
}

function concat(lists) {
  const xs$$15 = (0, _Seq.fold)(function (state$$3, xs$$14) {
    return fold(function f$$4(acc$$4, x$$10) {
      return new _Types.List(x$$10, acc$$4);
    }, state$$3, xs$$14);
  }, new _Types.List(), lists);
  return reverse(xs$$15);
}

function foldIndexed2Aux($f$$5$$31, $i$$1$$32, $acc$$5$$33, $bs$$34, $cs$$35) {
  foldIndexed2Aux: while (true) {
    const f$$5 = $f$$5$$31,
          i$$1 = $i$$1$$32,
          acc$$5 = $acc$$5$$33,
          bs = $bs$$34,
          cs = $cs$$35;
    var $target$$36, x$$11, xs$$16, y$$1, ys$$3;

    if (bs.tail != null) {
      if (cs.tail != null) {
        $target$$36 = 1;
        x$$11 = bs.head;
        xs$$16 = bs.tail;
        y$$1 = cs.head;
        ys$$3 = cs.tail;
      } else {
        $target$$36 = 2;
      }
    } else if (cs.tail == null) {
      $target$$36 = 0;
    } else {
      $target$$36 = 2;
    }

    switch ($target$$36) {
      case 0:
        {
          return acc$$5;
        }

      case 1:
        {
          $f$$5$$31 = f$$5;
          $i$$1$$32 = i$$1 + 1;
          $acc$$5$$33 = f$$5(i$$1, acc$$5, x$$11, y$$1);
          $bs$$34 = xs$$16;
          $cs$$35 = ys$$3;
          continue foldIndexed2Aux;
        }

      case 2:
        {
          throw new Error("Lists had different lengths");
        }
    }

    break;
  }
}

function foldIndexed2(f$$6, state$$4, xs$$17, ys$$4) {
  return foldIndexed2Aux(f$$6, 0, state$$4, xs$$17, ys$$4);
}

function fold2(f$$7, state$$5, xs$$18, ys$$5) {
  return (0, _Seq.fold2)(f$$7, state$$5, xs$$18, ys$$5);
}

function foldBack2(f$$8, xs$$19, ys$$6, state$$6) {
  return (0, _Seq.foldBack2)(f$$8, xs$$19, ys$$6, state$$6);
}

function unfold(f$$9, state$$7) {
  const unfoldInner = function unfoldInner($acc$$6$$51, $state$$8$$52) {
    unfoldInner: while (true) {
      const acc$$6 = $acc$$6$$51,
            state$$8 = $state$$8$$52;
      const matchValue$$3 = f$$9(state$$8);

      if (matchValue$$3 != null) {
        const x$$12 = matchValue$$3[0];
        const state$$9 = matchValue$$3[1];
        $acc$$6$$51 = new _Types.List(x$$12, acc$$6);
        $state$$8$$52 = state$$9;
        continue unfoldInner;
      } else {
        return reverse(acc$$6);
      }

      break;
    }
  };

  return unfoldInner(new _Types.List(), state$$7);
}

function foldIndexed3Aux($f$$10$$53, $i$$2$$54, $acc$$7$$55, $bs$$1$$56, $cs$$1$$57, $ds$$58) {
  foldIndexed3Aux: while (true) {
    const f$$10 = $f$$10$$53,
          i$$2 = $i$$2$$54,
          acc$$7 = $acc$$7$$55,
          bs$$1 = $bs$$1$$56,
          cs$$1 = $cs$$1$$57,
          ds = $ds$$58;
    var $target$$59, x$$13, xs$$20, y$$2, ys$$7, z, zs;

    if (bs$$1.tail != null) {
      if (cs$$1.tail != null) {
        if (ds.tail != null) {
          $target$$59 = 1;
          x$$13 = bs$$1.head;
          xs$$20 = bs$$1.tail;
          y$$2 = cs$$1.head;
          ys$$7 = cs$$1.tail;
          z = ds.head;
          zs = ds.tail;
        } else {
          $target$$59 = 2;
        }
      } else {
        $target$$59 = 2;
      }
    } else if (cs$$1.tail == null) {
      if (ds.tail == null) {
        $target$$59 = 0;
      } else {
        $target$$59 = 2;
      }
    } else {
      $target$$59 = 2;
    }

    switch ($target$$59) {
      case 0:
        {
          return acc$$7;
        }

      case 1:
        {
          $f$$10$$53 = f$$10;
          $i$$2$$54 = i$$2 + 1;
          $acc$$7$$55 = f$$10(i$$2, acc$$7, x$$13, y$$2, z);
          $bs$$1$$56 = xs$$20;
          $cs$$1$$57 = ys$$7;
          $ds$$58 = zs;
          continue foldIndexed3Aux;
        }

      case 2:
        {
          throw new Error("Lists had different lengths");
        }
    }

    break;
  }
}

function foldIndexed3(f$$11, seed, xs$$21, ys$$8, zs$$1) {
  return foldIndexed3Aux(f$$11, 0, seed, xs$$21, ys$$8, zs$$1);
}

function fold3(f$$12, state$$10, xs$$22, ys$$9, zs$$2) {
  return foldIndexed3(function (_arg1$$6, acc$$8, x$$14, y$$3, z$$1) {
    return f$$12(acc$$8, x$$14, y$$3, z$$1);
  }, state$$10, xs$$22, ys$$9, zs$$2);
}

function scan(f$$13, state$$11, xs$$23) {
  const xs$$24 = (0, _Seq.scan)(f$$13, state$$11, xs$$23);
  return ofSeq(xs$$24);
}

function scanBack(f$$14, xs$$25, state$$12) {
  const xs$$26 = (0, _Seq.scanBack)(f$$14, xs$$25, state$$12);
  return ofSeq(xs$$26);
}

function length(xs$$27) {
  return fold(function (acc$$9, _arg1$$7) {
    return acc$$9 + 1;
  }, 0, xs$$27);
}

function append(xs$$28, ys$$10) {
  return fold(function (acc$$10, x$$15) {
    return new _Types.List(x$$15, acc$$10);
  }, ys$$10, reverse(xs$$28));
}

function collect(f$$15, xs$$29) {
  const xs$$30 = (0, _Seq.collect)(f$$15, xs$$29);
  return ofSeq(xs$$30);
}

function map(f$$16, xs$$31) {
  const xs$$32 = fold(function (acc$$11, x$$16) {
    return new _Types.List(f$$16(x$$16), acc$$11);
  }, new _Types.List(), xs$$31);
  return reverse(xs$$32);
}

function mapIndexed(f$$17, xs$$33) {
  const xs$$34 = foldIndexed(function (i$$3, acc$$12, x$$17) {
    return new _Types.List(f$$17(i$$3, x$$17), acc$$12);
  }, new _Types.List(), xs$$33);
  return reverse(xs$$34);
}

function indexed(xs$$35) {
  return mapIndexed(function (i$$4, x$$18) {
    return [i$$4, x$$18];
  }, xs$$35);
}

function map2(f$$18, xs$$36, ys$$11) {
  const xs$$37 = fold2(function (acc$$13, x$$19, y$$4) {
    return new _Types.List(f$$18(x$$19, y$$4), acc$$13);
  }, new _Types.List(), xs$$36, ys$$11);
  return reverse(xs$$37);
}

function mapIndexed2(f$$19, xs$$38, ys$$12) {
  const xs$$39 = foldIndexed2(function (i$$5, acc$$14, x$$20, y$$5) {
    return new _Types.List(f$$19(i$$5, x$$20, y$$5), acc$$14);
  }, new _Types.List(), xs$$38, ys$$12);
  return reverse(xs$$39);
}

function map3(f$$20, xs$$40, ys$$13, zs$$3) {
  const xs$$41 = fold3(function (acc$$15, x$$21, y$$6, z$$2) {
    return new _Types.List(f$$20(x$$21, y$$6, z$$2), acc$$15);
  }, new _Types.List(), xs$$40, ys$$13, zs$$3);
  return reverse(xs$$41);
}

function mapIndexed3(f$$21, xs$$42, ys$$14, zs$$4) {
  const xs$$43 = foldIndexed3(function (i$$6, acc$$16, x$$22, y$$7, z$$3) {
    return new _Types.List(f$$21(i$$6, x$$22, y$$7, z$$3), acc$$16);
  }, new _Types.List(), xs$$42, ys$$14, zs$$4);
  return reverse(xs$$43);
}

function mapFold(f$$22, s, xs$$44) {
  const patternInput$$1 = fold(function foldFn(tupledArg, x$$23) {
    const patternInput = f$$22(tupledArg[1], x$$23);
    return [new _Types.List(patternInput[0], tupledArg[0]), patternInput[1]];
  }, [new _Types.List(), s], xs$$44);
  return [reverse(patternInput$$1[0]), patternInput$$1[1]];
}

function mapFoldBack(f$$23, xs$$45, s$$2) {
  return mapFold(function (s$$3, v) {
    return f$$23(v, s$$3);
  }, s$$2, reverse(xs$$45));
}

function iterate(f$$24, xs$$46) {
  fold(function (unitVar0, x$$24) {
    f$$24(x$$24);
  }, null, xs$$46);
}

function iterate2(f$$25, xs$$47, ys$$15) {
  fold2(function (unitVar0$$1, x$$25, y$$8) {
    f$$25(x$$25, y$$8);
  }, null, xs$$47, ys$$15);
}

function iterateIndexed(f$$26, xs$$48) {
  foldIndexed(function (i$$7, unitVar1, x$$26) {
    f$$26(i$$7, x$$26);
  }, null, xs$$48);
}

function iterateIndexed2(f$$27, xs$$49, ys$$16) {
  foldIndexed2(function (i$$8, unitVar1$$1, x$$27, y$$9) {
    f$$27(i$$8, x$$27, y$$9);
  }, null, xs$$49, ys$$16);
}

function ofArray(xs$$50) {
  let res$$1 = new _Types.List();

  for (let i$$9 = (0, _Util.count)(xs$$50) - 1; i$$9 >= 0; i$$9--) {
    res$$1 = new _Types.List(xs$$50[i$$9], res$$1);
  }

  return res$$1;
}

function empty() {
  return new _Types.List();
}

function isEmpty(_arg1$$8) {
  if (_arg1$$8.tail == null) {
    return true;
  } else {
    return false;
  }
}

function tryPickIndexedAux($f$$28$$120, $i$$10$$121, $_arg1$$9$$122) {
  tryPickIndexedAux: while (true) {
    const f$$28 = $f$$28$$120,
          i$$10 = $i$$10$$121,
          _arg1$$9 = $_arg1$$9$$122;

    if (_arg1$$9.tail != null) {
      const result = f$$28(i$$10, _arg1$$9.head);

      if (result == null) {
        $f$$28$$120 = f$$28;
        $i$$10$$121 = i$$10 + 1;
        $_arg1$$9$$122 = _arg1$$9.tail;
        continue tryPickIndexedAux;
      } else {
        return result;
      }
    } else {
      return null;
    }

    break;
  }
}

function tryPickIndexed(f$$29, xs$$52) {
  return tryPickIndexedAux(f$$29, 0, xs$$52);
}

function tryPick(f$$30, xs$$53) {
  return tryPickIndexed(function (_arg1$$10, x$$29) {
    return f$$30(x$$29);
  }, xs$$53);
}

function pick(f$$31, xs$$54) {
  const matchValue$$5 = tryPick(f$$31, xs$$54);

  if (matchValue$$5 != null) {
    const x$$30 = (0, _Option.value)(matchValue$$5);
    return x$$30;
  } else {
    throw new Error("List did not contain any matching elements");
  }
}

function tryFindIndexed(f$$32, xs$$55) {
  return tryPickIndexed(function (i$$11, x$$31) {
    return f$$32(i$$11, x$$31) ? (0, _Option.some)(x$$31) : null;
  }, xs$$55);
}

function tryFind(f$$33, xs$$56) {
  return tryPickIndexed(function (_arg1$$11, x$$32) {
    return f$$33(x$$32) ? (0, _Option.some)(x$$32) : null;
  }, xs$$56);
}

function findIndexed(f$$34, xs$$57) {
  const matchValue$$6 = tryFindIndexed(f$$34, xs$$57);

  if (matchValue$$6 != null) {
    const x$$33 = (0, _Option.value)(matchValue$$6);
    return x$$33;
  } else {
    throw new Error("List did not contain any matching elements");
  }
}

function find(f$$35, xs$$58) {
  return findIndexed(function (_arg1$$12, x$$34) {
    return f$$35(x$$34);
  }, xs$$58);
}

function findBack(f$$36, xs$$59) {
  let xs$$61;
  xs$$61 = reverse(xs$$59);
  return find(f$$36, xs$$61);
}

function tryFindBack(f$$37, xs$$62) {
  let xs$$64;
  xs$$64 = reverse(xs$$62);
  return tryFind(f$$37, xs$$64);
}

function tryFindIndex(f$$38, xs$$65) {
  return tryPickIndexed(function (i$$12, x$$35) {
    return f$$38(x$$35) ? i$$12 : null;
  }, xs$$65);
}

function tryFindIndexBack(f$$39, xs$$66) {
  const array = (0, _Array.ofList)(xs$$66, Array);
  return (0, _Array.tryFindIndexBack)(f$$39, array);
}

function findIndex(f$$40, xs$$67) {
  const matchValue$$7 = tryFindIndex(f$$40, xs$$67);

  if (matchValue$$7 != null) {
    const x$$36 = matchValue$$7 | 0;
    return x$$36 | 0;
  } else {
    throw new Error("List did not contain any matching elements");
  }
}

function findIndexBack(f$$41, xs$$68) {
  const array$$1 = (0, _Array.ofList)(xs$$68, Array);
  return (0, _Array.findIndexBack)(f$$41, array$$1) | 0;
}

function item(n, xs$$69) {
  return findIndexed(function (i$$13, _arg1$$13) {
    return n === i$$13;
  }, xs$$69);
}

function tryItem(n$$1, xs$$70) {
  return tryFindIndexed(function (i$$14, _arg1$$14) {
    return n$$1 === i$$14;
  }, xs$$70);
}

function filter(f$$42, xs$$71) {
  const xs$$72 = fold(function (acc$$17, x$$37) {
    return f$$42(x$$37) ? new _Types.List(x$$37, acc$$17) : acc$$17;
  }, new _Types.List(), xs$$71);
  return reverse(xs$$72);
}

function partition(f$$43, xs$$73) {
  return fold(function (tupledArg$$1, x$$38) {
    return f$$43(x$$38) ? [new _Types.List(x$$38, tupledArg$$1[0]), tupledArg$$1[1]] : [tupledArg$$1[0], new _Types.List(x$$38, tupledArg$$1[1])];
  }, [new _Types.List(), new _Types.List()], reverse(xs$$73));
}

function choose(f$$44, xs$$74) {
  const xs$$75 = fold(function (acc$$18, x$$39) {
    const matchValue$$8 = f$$44(x$$39);

    if (matchValue$$8 == null) {
      return acc$$18;
    } else {
      const y$$10 = (0, _Option.value)(matchValue$$8);
      return new _Types.List(y$$10, acc$$18);
    }
  }, new _Types.List(), xs$$74);
  return reverse(xs$$75);
}

function contains(value, list, eq) {
  const loop$$1 = function loop$$1($xs$$76$$162) {
    loop$$1: while (true) {
      const xs$$76 = $xs$$76$$162;

      if (xs$$76.tail != null) {
        if (eq.Equals(value, xs$$76.head)) {
          return true;
        } else {
          $xs$$76$$162 = xs$$76.tail;
          continue loop$$1;
        }
      } else {
        return false;
      }

      break;
    }
  };

  return loop$$1(list);
}

function except(itemsToExclude, array$$2, eq$$1) {
  if (isEmpty(array$$2)) {
    return array$$2;
  } else {
    const cached = (0, _Set.createMutable)(itemsToExclude, eq$$1);
    return filter(function f$$45(arg00) {
      return (0, _Util.addToSet)(arg00, cached);
    }, array$$2);
  }
}

function initialize(n$$2, f$$46) {
  let xs$$78 = new _Types.List();

  for (let i$$15 = 1; i$$15 <= n$$2; i$$15++) {
    xs$$78 = new _Types.List(f$$46(n$$2 - i$$15), xs$$78);
  }

  return xs$$78;
}

function replicate(n$$3, x$$40) {
  return initialize(n$$3, function (_arg1$$15) {
    return x$$40;
  });
}

function reduce(f$$47, _arg1$$16) {
  if (_arg1$$16.tail != null) {
    return fold(f$$47, _arg1$$16.head, _arg1$$16.tail);
  } else {
    throw new Error("List was empty");
  }
}

function reduceBack(f$$48, _arg1$$17) {
  if (_arg1$$17.tail != null) {
    return foldBack(f$$48, _arg1$$17.tail, _arg1$$17.head);
  } else {
    throw new Error("List was empty");
  }
}

function forAll(f$$49, xs$$79) {
  return fold(function (acc$$19, x$$41) {
    return acc$$19 ? f$$49(x$$41) : false;
  }, true, xs$$79);
}

function forAll2(f$$50, xs$$80, ys$$17) {
  return fold2(function (acc$$20, x$$42, y$$11) {
    return acc$$20 ? f$$50(x$$42, y$$11) : false;
  }, true, xs$$80, ys$$17);
}

function exists($f$$51$$180, $_arg1$$18$$181) {
  exists: while (true) {
    const f$$51 = $f$$51$$180,
          _arg1$$18 = $_arg1$$18$$181;

    if (_arg1$$18.tail != null) {
      if (f$$51(_arg1$$18.head)) {
        return true;
      } else {
        $f$$51$$180 = f$$51;
        $_arg1$$18$$181 = _arg1$$18.tail;
        continue exists;
      }
    } else {
      return false;
    }

    break;
  }
}

function exists2($f$$52$$182, $bs$$2$$183, $cs$$2$$184) {
  exists2: while (true) {
    const f$$52 = $f$$52$$182,
          bs$$2 = $bs$$2$$183,
          cs$$2 = $cs$$2$$184;
    var $target$$185, x$$44, xs$$82, y$$12, ys$$18;

    if (bs$$2.tail != null) {
      if (cs$$2.tail != null) {
        $target$$185 = 1;
        x$$44 = bs$$2.head;
        xs$$82 = bs$$2.tail;
        y$$12 = cs$$2.head;
        ys$$18 = cs$$2.tail;
      } else {
        $target$$185 = 2;
      }
    } else if (cs$$2.tail == null) {
      $target$$185 = 0;
    } else {
      $target$$185 = 2;
    }

    switch ($target$$185) {
      case 0:
        {
          return false;
        }

      case 1:
        {
          if (f$$52(x$$44, y$$12)) {
            return true;
          } else {
            $f$$52$$182 = f$$52;
            $bs$$2$$183 = xs$$82;
            $cs$$2$$184 = ys$$18;
            continue exists2;
          }
        }

      case 2:
        {
          throw new Error("Lists had different lengths");
        }
    }

    break;
  }
}

function unzip(xs$$83) {
  return foldBack(function (tupledArg$$2, tupledArg$$3) {
    return [new _Types.List(tupledArg$$2[0], tupledArg$$3[0]), new _Types.List(tupledArg$$2[1], tupledArg$$3[1])];
  }, xs$$83, [new _Types.List(), new _Types.List()]);
}

function unzip3(xs$$84) {
  return foldBack(function (tupledArg$$4, tupledArg$$5) {
    return [new _Types.List(tupledArg$$4[0], tupledArg$$5[0]), new _Types.List(tupledArg$$4[1], tupledArg$$5[1]), new _Types.List(tupledArg$$4[2], tupledArg$$5[2])];
  }, xs$$84, [new _Types.List(), new _Types.List(), new _Types.List()]);
}

function zip(xs$$85, ys$$19) {
  return map2(function (x$$47, y$$15) {
    return [x$$47, y$$15];
  }, xs$$85, ys$$19);
}

function zip3(xs$$86, ys$$20, zs$$5) {
  return map3(function (x$$48, y$$16, z$$5) {
    return [x$$48, y$$16, z$$5];
  }, xs$$86, ys$$20, zs$$5);
}

function sort(xs$$87, comparer$$1) {
  let xs$$89;
  const xs$$88 = (0, _Array.ofList)(xs$$87, Array);
  xs$$88.sort(function comparer$$2(x$$49, y$$17) {
    return comparer$$1.Compare(x$$49, y$$17);
  });
  xs$$89 = xs$$88;
  return ofArray(xs$$89);
}

function sortBy(projection, xs$$90, comparer$$3) {
  let xs$$92;
  const xs$$91 = (0, _Array.ofList)(xs$$90, Array);
  xs$$91.sort(function comparer$$4(x$$50, y$$18) {
    return comparer$$3.Compare(projection(x$$50), projection(y$$18));
  });
  xs$$92 = xs$$91;
  return ofArray(xs$$92);
}

function sortDescending(xs$$93, comparer$$5) {
  let xs$$95;
  const xs$$94 = (0, _Array.ofList)(xs$$93, Array);
  xs$$94.sort(function comparer$$6(x$$51, y$$19) {
    return comparer$$5.Compare(x$$51, y$$19) * -1;
  });
  xs$$95 = xs$$94;
  return ofArray(xs$$95);
}

function sortByDescending(projection$$1, xs$$96, comparer$$7) {
  let xs$$98;
  const xs$$97 = (0, _Array.ofList)(xs$$96, Array);
  xs$$97.sort(function comparer$$8(x$$52, y$$20) {
    return comparer$$7.Compare(projection$$1(x$$52), projection$$1(y$$20)) * -1;
  });
  xs$$98 = xs$$97;
  return ofArray(xs$$98);
}

function sortWith(comparer$$9, xs$$99) {
  let xs$$101;
  const xs$$100 = (0, _Array.ofList)(xs$$99, Array);
  xs$$100.sort(comparer$$9);
  xs$$101 = xs$$100;
  return ofArray(xs$$101);
}

function sum(xs$$102, adder) {
  return fold(function (acc$$21, x$$53) {
    return adder.Add(acc$$21, x$$53);
  }, adder.GetZero(), xs$$102);
}

function sumBy(f$$53, xs$$103, adder$$1) {
  return fold(function (acc$$22, x$$54) {
    return adder$$1.Add(acc$$22, f$$53(x$$54));
  }, adder$$1.GetZero(), xs$$103);
}

function maxBy(projection$$2, xs$$104, comparer$$11) {
  return reduce(function (x$$55, y$$21) {
    return comparer$$11.Compare(projection$$2(y$$21), projection$$2(x$$55)) > 0 ? y$$21 : x$$55;
  }, xs$$104);
}

function max(li, comparer$$12) {
  return reduce(function (x$$56, y$$22) {
    return comparer$$12.Compare(y$$22, x$$56) > 0 ? y$$22 : x$$56;
  }, li);
}

function minBy(projection$$3, xs$$105, comparer$$13) {
  return reduce(function (x$$57, y$$23) {
    return comparer$$13.Compare(projection$$3(y$$23), projection$$3(x$$57)) > 0 ? x$$57 : y$$23;
  }, xs$$105);
}

function min(xs$$106, comparer$$14) {
  return reduce(function (x$$58, y$$24) {
    return comparer$$14.Compare(y$$24, x$$58) > 0 ? x$$58 : y$$24;
  }, xs$$106);
}

function average(xs$$107, averager) {
  const total = fold(function (acc$$23, x$$59) {
    return averager.Add(acc$$23, x$$59);
  }, averager.GetZero(), xs$$107);
  return averager.DivideByInt(total, length(xs$$107));
}

function averageBy(f$$54, xs$$108, averager$$1) {
  const total$$1 = fold(function (acc$$24, x$$60) {
    return averager$$1.Add(acc$$24, f$$54(x$$60));
  }, averager$$1.GetZero(), xs$$108);
  return averager$$1.DivideByInt(total$$1, length(xs$$108));
}

function permute(f$$55, xs$$109) {
  let xs$$110;
  let array$$3;
  array$$3 = (0, _Array.ofList)(xs$$109, Array);
  xs$$110 = (0, _Array.permute)(f$$55, array$$3);
  return ofArray(xs$$110);
}

function skip(i$$16, xs$$111) {
  const skipInner = function skipInner($i$$17$$237, $xs$$112$$238) {
    skipInner: while (true) {
      const i$$17 = $i$$17$$237,
            xs$$112 = $xs$$112$$238;

      if (i$$17 === 0) {
        return xs$$112;
      } else if (xs$$112.tail != null) {
        $i$$17$$237 = i$$17 - 1;
        $xs$$112$$238 = xs$$112.tail;
        continue skipInner;
      } else {
        throw new Error("The input sequence has an insufficient number of elements.");
      }

      break;
    }
  };

  if (i$$16 < 0) {
    throw new Error("The input must be non-negative.");
  } else {
    var $target$$239, i$$20, xs$$115;

    if (i$$16 === 0) {
      $target$$239 = 0;
    } else if (i$$16 === 1) {
      if (xs$$111.tail != null) {
        $target$$239 = 1;
      } else {
        $target$$239 = 2;
        i$$20 = i$$16;
        xs$$115 = xs$$111;
      }
    } else {
      $target$$239 = 2;
      i$$20 = i$$16;
      xs$$115 = xs$$111;
    }

    switch ($target$$239) {
      case 0:
        {
          return xs$$111;
        }

      case 1:
        {
          return xs$$111.tail;
        }

      case 2:
        {
          return skipInner(i$$20, xs$$115);
        }
    }
  }
}

function skipWhile($predicate$$240, $xs$$116$$241) {
  skipWhile: while (true) {
    const predicate = $predicate$$240,
          xs$$116 = $xs$$116$$241;
    var $target$$242, h$$4, t$$4;

    if (xs$$116.tail != null) {
      if (predicate(xs$$116.head)) {
        $target$$242 = 0;
        h$$4 = xs$$116.head;
        t$$4 = xs$$116.tail;
      } else {
        $target$$242 = 1;
      }
    } else {
      $target$$242 = 1;
    }

    switch ($target$$242) {
      case 0:
        {
          $predicate$$240 = predicate;
          $xs$$116$$241 = t$$4;
          continue skipWhile;
        }

      case 1:
        {
          return xs$$116;
        }
    }

    break;
  }
}

function takeSplitAux($error$$243, $i$$21$$244, $acc$$25$$245, $xs$$117$$246) {
  takeSplitAux: while (true) {
    const error = $error$$243,
          i$$21 = $i$$21$$244,
          acc$$25 = $acc$$25$$245,
          xs$$117 = $xs$$117$$246;

    if (i$$21 === 0) {
      return [reverse(acc$$25), xs$$117];
    } else if (xs$$117.tail != null) {
      $error$$243 = error;
      $i$$21$$244 = i$$21 - 1;
      $acc$$25$$245 = new _Types.List(xs$$117.head, acc$$25);
      $xs$$117$$246 = xs$$117.tail;
      continue takeSplitAux;
    } else {
      if (error) {
        throw new Error("The input sequence has an insufficient number of elements.");
      } else {
        return [reverse(acc$$25), xs$$117];
      }
    }

    break;
  }
}

function take(i$$22, xs$$119) {
  if (i$$22 < 0) {
    throw new Error("The input must be non-negative.");
  } else {
    var $target$$249, i$$25, xs$$120;

    if (i$$22 === 0) {
      $target$$249 = 0;
    } else if (i$$22 === 1) {
      if (xs$$119.tail != null) {
        $target$$249 = 1;
      } else {
        $target$$249 = 2;
        i$$25 = i$$22;
        xs$$120 = xs$$119;
      }
    } else {
      $target$$249 = 2;
      i$$25 = i$$22;
      xs$$120 = xs$$119;
    }

    switch ($target$$249) {
      case 0:
        {
          return new _Types.List();
        }

      case 1:
        {
          return new _Types.List(xs$$119.head, new _Types.List());
        }

      case 2:
        {
          const tuple = takeSplitAux(true, i$$25, new _Types.List(), xs$$120);
          return tuple[0];
        }
    }
  }
}

function takeWhile(predicate$$1, xs$$121) {
  if (xs$$121.tail != null) {
    if (xs$$121.tail.tail == null) {
      if (predicate$$1(xs$$121.head)) {
        return xs$$121;
      } else {
        return xs$$121.tail;
      }
    } else {
      if (!predicate$$1(xs$$121.head)) {
        return new _Types.List();
      } else {
        return new _Types.List(xs$$121.head, takeWhile(predicate$$1, xs$$121.tail));
      }
    }
  } else {
    return xs$$121;
  }
}

function truncate(i$$26, xs$$123) {
  if (i$$26 < 0) {
    throw new Error("The input must be non-negative.");
  } else {
    var $target$$254, i$$29, xs$$124;

    if (i$$26 === 0) {
      $target$$254 = 0;
    } else if (i$$26 === 1) {
      if (xs$$123.tail != null) {
        $target$$254 = 1;
      } else {
        $target$$254 = 2;
        i$$29 = i$$26;
        xs$$124 = xs$$123;
      }
    } else {
      $target$$254 = 2;
      i$$29 = i$$26;
      xs$$124 = xs$$123;
    }

    switch ($target$$254) {
      case 0:
        {
          return new _Types.List();
        }

      case 1:
        {
          return new _Types.List(xs$$123.head, new _Types.List());
        }

      case 2:
        {
          const tuple$$1 = takeSplitAux(false, i$$29, new _Types.List(), xs$$124);
          return tuple$$1[0];
        }
    }
  }
}

function splitAt(i$$30, xs$$125) {
  if (i$$30 < 0) {
    throw new Error("The input must be non-negative.");
  } else {
    var $target$$257, i$$33, xs$$127;

    if (i$$30 === 0) {
      $target$$257 = 0;
    } else if (i$$30 === 1) {
      if (xs$$125.tail != null) {
        $target$$257 = 1;
      } else {
        $target$$257 = 2;
        i$$33 = i$$30;
        xs$$127 = xs$$125;
      }
    } else {
      $target$$257 = 2;
      i$$33 = i$$30;
      xs$$127 = xs$$125;
    }

    switch ($target$$257) {
      case 0:
        {
          return [new _Types.List(), xs$$125];
        }

      case 1:
        {
          return [new _Types.List(xs$$125.head, new _Types.List()), xs$$125.tail];
        }

      case 2:
        {
          return takeSplitAux(true, i$$33, new _Types.List(), xs$$127);
        }
    }
  }
}

function outOfRange() {
  throw new Error("Index out of range");
}

function slice(lower, upper, xs$$128) {
  const lower$$1 = (0, _Option.defaultArg)(lower, 0) | 0;
  const hasUpper = upper != null;

  if (lower$$1 < 0) {
    return outOfRange();
  } else if (hasUpper ? upper < lower$$1 : false) {
    return new _Types.List();
  } else {
    let lastIndex = -1 | 0;
    let res$$2;
    const state$$13 = new _Types.List();
    res$$2 = foldIndexed(function f$$56(i$$34, acc$$26, x$$67) {
      lastIndex = i$$34;

      if (lower$$1 <= i$$34 ? !hasUpper ? true : i$$34 <= upper : false) {
        return new _Types.List(x$$67, acc$$26);
      } else {
        return acc$$26;
      }
    }, state$$13, xs$$128);

    if (lower$$1 > lastIndex + 1 ? true : hasUpper ? upper > lastIndex : false) {
      outOfRange();
    }

    return reverse(res$$2);
  }
}

function distinctBy(projection$$4, xs$$130, eq$$2) {
  const hashSet = (0, _Set.createMutable)([], eq$$2);
  return filter(function f$$57($arg$$1) {
    const arg00$$1 = projection$$4($arg$$1);
    return (0, _Util.addToSet)(arg00$$1, hashSet);
  }, xs$$130);
}

function distinct(xs$$132, eq$$3) {
  return distinctBy(function (x$$68) {
    return x$$68;
  }, xs$$132, eq$$3);
}

function exactlyOne(xs$$133) {
  if (xs$$133.tail != null) {
    if (xs$$133.tail.tail != null) {
      throw new Error("Input list too long\\nParameter name: list");
    } else {
      return xs$$133.head;
    }
  } else {
    throw new Error("The input sequence was empty\\nParameter name: list");
  }
}

function groupBy(projection$$5, xs$$135, eq$$4) {
  const dict = (0, _Map.createMutable)([], eq$$4);
  let keys = new _Types.List();
  iterate(function f$$58(v$$2) {
    const key = projection$$5(v$$2);
    const matchValue$$16 = (0, _Util.tryGetValue)(dict, key, null);

    if (matchValue$$16[0]) {
      dict.set(key, new _Types.List(v$$2, matchValue$$16[1]));
    } else {
      (0, _Util.addToDict)(dict, key, new _Types.List(v$$2, new _Types.List()));
      keys = new _Types.List(key, keys);
    }
  }, xs$$135);
  let result$$1 = new _Types.List();
  const xs$$137 = keys;
  iterate(function f$$59(key$$1) {
    result$$1 = new _Types.List([key$$1, reverse((0, _Util.getItemFromDict)(dict, key$$1))], result$$1);
  }, xs$$137);
  return result$$1;
}

function countBy(projection$$6, xs$$138, eq$$5) {
  const dict$$1 = (0, _Map.createMutable)([], eq$$5);
  let keys$$1 = new _Types.List();
  iterate(function f$$60(v$$3) {
    const key$$2 = projection$$6(v$$3);
    const matchValue$$17 = (0, _Util.tryGetValue)(dict$$1, key$$2, 0);

    if (matchValue$$17[0]) {
      dict$$1.set(key$$2, matchValue$$17[1] + 1);
    } else {
      dict$$1.set(key$$2, 1);
      keys$$1 = new _Types.List(key$$2, keys$$1);
    }
  }, xs$$138);
  let result$$2 = new _Types.List();
  const xs$$140 = keys$$1;
  iterate(function f$$61(key$$3) {
    result$$2 = new _Types.List([key$$3, (0, _Util.getItemFromDict)(dict$$1, key$$3)], result$$2);
  }, xs$$140);
  return result$$2;
}

function where(predicate$$2, xs$$141) {
  return filter(predicate$$2, xs$$141);
}

function pairwise(xs$$142) {
  const inner = function inner($xs$$143$$284, $acc$$27$$285, $x1$$1$$286) {
    inner: while (true) {
      const xs$$143 = $xs$$143$$284,
            acc$$27 = $acc$$27$$285,
            x1$$1 = $x1$$1$$286;

      if (xs$$143.tail != null) {
        let copyOfStruct = acc$$27;
        copyOfStruct.push([x1$$1, xs$$143.head]);
        $xs$$143$$284 = xs$$143.tail;
        $acc$$27$$285 = acc$$27;
        $x1$$1$$286 = xs$$143.head;
        continue inner;
      } else {
        return ofArray(acc$$27);
      }

      break;
    }
  };

  var $target$$287, x1$$2, x2$$2, xs$$145;

  if (xs$$142.tail != null) {
    if (xs$$142.tail.tail != null) {
      $target$$287 = 1;
      x1$$2 = xs$$142.head;
      x2$$2 = xs$$142.tail.head;
      xs$$145 = xs$$142.tail.tail;
    } else {
      $target$$287 = 0;
    }
  } else {
    $target$$287 = 0;
  }

  switch ($target$$287) {
    case 0:
      {
        return new _Types.List();
      }

    case 1:
      {
        const acc$$28 = [];
        acc$$28.push([x1$$2, x2$$2]);
        const clo1 = (0, _Util.partialApply)(2, inner, [xs$$145]);
        const clo2 = clo1(acc$$28);
        return clo2(x2$$2);
      }
  }
}

function windowed(windowSize, source) {
  if (windowSize <= 0) {
    throw new Error("windowSize must be positive");
  }

  let res$$3 = new _Types.List();

  for (let i$$35 = length(source); i$$35 >= windowSize; i$$35--) {
    res$$3 = new _Types.List(slice(i$$35 - windowSize, i$$35 - 1, source), res$$3);
  }

  return res$$3;
}