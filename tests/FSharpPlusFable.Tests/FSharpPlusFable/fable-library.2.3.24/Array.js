"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.append = append;
exports.filter = filter;
exports.fill = fill;
exports.getSubArray = getSubArray;
exports.last = last;
exports.tryLast = tryLast;
exports.mapIndexed = mapIndexed;
exports.map = map;
exports.mapIndexed2 = mapIndexed2;
exports.map2 = map2;
exports.mapIndexed3 = mapIndexed3;
exports.map3 = map3;
exports.mapFold = mapFold;
exports.mapFoldBack = mapFoldBack;
exports.indexed = indexed;
exports.truncate = truncate;
exports.concat = concat;
exports.collect = collect;
exports.countBy = countBy;
exports.distinctBy = distinctBy;
exports.distinct = distinct;
exports.where = where;
exports.contains = contains;
exports.except = except;
exports.groupBy = groupBy;
exports.empty = empty;
exports.singleton = singleton;
exports.initialize = initialize;
exports.pairwise = pairwise;
exports.replicate = replicate;
exports.copy = copy;
exports.reverse = reverse;
exports.scan = scan;
exports.scanBack = scanBack;
exports.skip = skip;
exports.skipWhile = skipWhile;
exports.take = take;
exports.takeWhile = takeWhile;
exports.addRangeInPlace = addRangeInPlace;
exports.removeInPlace = removeInPlace;
exports.removeAllInPlace = removeAllInPlace;
exports.copyTo = copyTo;
exports.partition = partition;
exports.find = find;
exports.tryFind = tryFind;
exports.findIndex = findIndex;
exports.tryFindIndex = tryFindIndex;
exports.pick = pick;
exports.tryPick = tryPick;
exports.findBack = findBack;
exports.tryFindBack = tryFindBack;
exports.findLastIndex = findLastIndex;
exports.findIndexBack = findIndexBack;
exports.tryFindIndexBack = tryFindIndexBack;
exports.choose = choose;
exports.foldIndexed = foldIndexed;
exports.fold = fold;
exports.iterate = iterate;
exports.iterateIndexed = iterateIndexed;
exports.iterate2 = iterate2;
exports.iterateIndexed2 = iterateIndexed2;
exports.isEmpty = isEmpty;
exports.forAll = forAll;
exports.permute = permute;
exports.setSlice = setSlice;
exports.sortInPlaceBy = sortInPlaceBy;
exports.sortInPlace = sortInPlace;
exports.sort = sort;
exports.sortBy = sortBy;
exports.sortDescending = sortDescending;
exports.sortByDescending = sortByDescending;
exports.sortWith = sortWith;
exports.unfold = unfold;
exports.unzip = unzip;
exports.unzip3 = unzip3;
exports.zip = zip;
exports.zip3 = zip3;
exports.chunkBySize = chunkBySize;
exports.splitAt = splitAt;
exports.compareWith = compareWith;
exports.equalsWith = equalsWith;
exports.exactlyOne = exactlyOne;
exports.head = head;
exports.tryHead = tryHead;
exports.tail = tail;
exports.item = item;
exports.tryItem = tryItem;
exports.foldBackIndexed = foldBackIndexed;
exports.foldBack = foldBack;
exports.foldIndexed2 = foldIndexed2;
exports.fold2 = fold2;
exports.foldBackIndexed2 = foldBackIndexed2;
exports.foldBack2 = foldBack2;
exports.reduce = reduce;
exports.reduceBack = reduceBack;
exports.forAll2 = forAll2;
exports.existsOffset = existsOffset;
exports.exists = exists;
exports.existsOffset2 = existsOffset2;
exports.exists2 = exists2;
exports.sum = sum;
exports.sumBy = sumBy;
exports.maxBy = maxBy;
exports.max = max;
exports.minBy = minBy;
exports.min = min;
exports.average = average;
exports.averageBy = averageBy;
exports.ofSeq = ofSeq;
exports.ofList = ofList;
exports.toList = toList;
exports.windowed = windowed;

var _Option = require("./Option");

var _Util = require("./Util");

var _Map = require("./Map");

var _Set = require("./Set");

var _Types = require("./Types");

var _Seq = require("./Seq");

function indexNotFound() {
  throw new Error("An index satisfying the predicate was not found in the collection.");
}

function append(array1, array2, cons) {
  if (ArrayBuffer.isView(array1)) {
    const len1 = array1.length | 0;
    const len2 = array2.length | 0;
    const newArray = new cons(len1 + len2);

    for (let i = 0; i <= len1 - 1; i++) {
      newArray[i] = array1[i];
    }

    for (let i$$1 = 0; i$$1 <= len2 - 1; i$$1++) {
      newArray[i$$1 + len1] = array2[i$$1];
    }

    return newArray;
  } else {
    return array1.concat(array2);
  }
}

function filter(predicate, array) {
  return array.filter(predicate);
}

function fill(target, targetIndex, count, value) {
  target.fill(value, targetIndex, targetIndex + count);
  return target;
}

function getSubArray(array$$3, start$$1, count$$2) {
  return array$$3.slice(start$$1, start$$1 + count$$2);
}

function last(array$$5) {
  if (array$$5.length === 0) {
    throw new Error("The input array was empty\\nParameter name: array");
  }

  return array$$5[array$$5.length - 1];
}

function tryLast(array$$6) {
  if (array$$6.length === 0) {
    return null;
  } else {
    return (0, _Option.some)(array$$6[array$$6.length - 1]);
  }
}

function mapIndexed(f, source, cons$$1) {
  if (ArrayBuffer.isView(source)) {
    const len = source.length | 0;
    const target$$1 = new cons$$1(len);

    for (let i$$2 = 0; i$$2 <= len - 1; i$$2++) {
      target$$1[i$$2] = f(i$$2, source[i$$2]);
    }

    return target$$1;
  } else {
    return source.map(function (delegateArg0, delegateArg1) {
      return f(delegateArg1, delegateArg0);
    });
  }
}

function map(f$$1, source$$1, cons$$2) {
  if (ArrayBuffer.isView(source$$1)) {
    const len$$1 = source$$1.length | 0;
    const target$$2 = new cons$$2(len$$1);

    for (let i$$4 = 0; i$$4 <= len$$1 - 1; i$$4++) {
      target$$2[i$$4] = f$$1(source$$1[i$$4]);
    }

    return target$$2;
  } else {
    return source$$1.map(function mapping$$1(x$$1) {
      return f$$1(x$$1);
    });
  }
}

function mapIndexed2(f$$2, source1, source2, cons$$3) {
  if (source1.length !== source2.length) {
    throw new Error("Arrays had different lengths");
  }

  const result = new cons$$3(source1.length);

  for (let i$$5 = 0; i$$5 <= source1.length - 1; i$$5++) {
    result[i$$5] = f$$2(i$$5, source1[i$$5], source2[i$$5]);
  }

  return result;
}

function map2(f$$3, source1$$1, source2$$1, cons$$4) {
  if (source1$$1.length !== source2$$1.length) {
    throw new Error("Arrays had different lengths");
  }

  const result$$1 = new cons$$4(source1$$1.length);

  for (let i$$6 = 0; i$$6 <= source1$$1.length - 1; i$$6++) {
    result$$1[i$$6] = f$$3(source1$$1[i$$6], source2$$1[i$$6]);
  }

  return result$$1;
}

function mapIndexed3(f$$4, source1$$2, source2$$2, source3, cons$$5) {
  if (source1$$2.length !== source2$$2.length ? true : source2$$2.length !== source3.length) {
    throw new Error("Arrays had different lengths");
  }

  const result$$2 = new cons$$5(source1$$2.length);

  for (let i$$7 = 0; i$$7 <= source1$$2.length - 1; i$$7++) {
    result$$2[i$$7] = f$$4(i$$7, source1$$2[i$$7], source2$$2[i$$7], source3[i$$7]);
  }

  return result$$2;
}

function map3(f$$5, source1$$3, source2$$3, source3$$1, cons$$6) {
  if (source1$$3.length !== source2$$3.length ? true : source2$$3.length !== source3$$1.length) {
    throw new Error("Arrays had different lengths");
  }

  const result$$3 = new cons$$6(source1$$3.length);

  for (let i$$8 = 0; i$$8 <= source1$$3.length - 1; i$$8++) {
    result$$3[i$$8] = f$$5(source1$$3[i$$8], source2$$3[i$$8], source3$$1[i$$8]);
  }

  return result$$3;
}

function mapFold(mapping$$2, state, array$$9, cons$$7) {
  const matchValue = array$$9.length | 0;

  if (matchValue === 0) {
    return [[], state];
  } else {
    let acc = state;
    const res = new cons$$7(matchValue);

    for (let i$$9 = 0; i$$9 <= array$$9.length - 1; i$$9++) {
      const patternInput = mapping$$2(acc, array$$9[i$$9]);
      res[i$$9] = patternInput[0];
      acc = patternInput[1];
    }

    return [res, acc];
  }
}

function mapFoldBack(mapping$$3, array$$10, state$$1, cons$$8) {
  const matchValue$$1 = array$$10.length | 0;

  if (matchValue$$1 === 0) {
    return [[], state$$1];
  } else {
    let acc$$1 = state$$1;
    const res$$1 = new cons$$8(matchValue$$1);

    for (let i$$10 = array$$10.length - 1; i$$10 >= 0; i$$10--) {
      const patternInput$$1 = mapping$$3(array$$10[i$$10], acc$$1);
      res$$1[i$$10] = patternInput$$1[0];
      acc$$1 = patternInput$$1[1];
    }

    return [res$$1, acc$$1];
  }
}

function indexed(source$$2) {
  const len$$4 = source$$2.length | 0;
  const target$$3 = new Array(len$$4);

  for (let i$$11 = 0; i$$11 <= len$$4 - 1; i$$11++) {
    target$$3[i$$11] = [i$$11, source$$2[i$$11]];
  }

  return target$$3;
}

function truncate(count$$4, array$$11) {
  const count$$5 = (0, _Util.max)(_Util.comparePrimitives, 0, count$$4) | 0;
  return array$$11.slice(0, 0 + count$$5);
}

function concat(arrays, cons$$9) {
  var arr$$3;
  const arrays$$1 = Array.from(arrays);
  const matchValue$$2 = arrays$$1.length | 0;

  switch (matchValue$$2) {
    case 0:
      {
        return new cons$$9(0);
      }

    case 1:
      {
        return arrays$$1[0];
      }

    default:
      {
        if (arr$$3 = arrays$$1[0], ArrayBuffer.isView(arr$$3)) {
          let totalIdx = 0;
          let totalLength = 0;

          for (let idx = 0; idx <= arrays$$1.length - 1; idx++) {
            const arr$$4 = arrays$$1[idx];
            totalLength = totalLength + arr$$4.length;
          }

          const result$$4 = new cons$$9(totalLength);

          for (let idx$$1 = 0; idx$$1 <= arrays$$1.length - 1; idx$$1++) {
            const arr$$5 = arrays$$1[idx$$1];

            for (let j = 0; j <= arr$$5.length - 1; j++) {
              result$$4[totalIdx] = arr$$5[j];
              totalIdx = totalIdx + 1;
            }
          }

          return result$$4;
        } else {
          return arrays$$1[0].concat(...arrays$$1.slice(1));
        }
      }
  }
}

function collect(mapping$$4, array$$14, cons$$10) {
  const mapped = map(mapping$$4, array$$14, Array);
  return concat(mapped, cons$$10);
}

function countBy(projection, array$$15, eq) {
  const dict = (0, _Map.createMutable)([], eq);
  const keys = [];

  for (let idx$$2 = 0; idx$$2 <= array$$15.length - 1; idx$$2++) {
    const value$$2 = array$$15[idx$$2];
    const key = projection(value$$2);
    const matchValue$$3 = (0, _Util.tryGetValue)(dict, key, 0);

    if (matchValue$$3[0]) {
      dict.set(key, matchValue$$3[1] + 1);
    } else {
      dict.set(key, 1);
      const value$$3 = keys.push(key);
      value$$3, null;
    }
  }

  const result$$5 = map(function (key$$1) {
    return [key$$1, (0, _Util.getItemFromDict)(dict, key$$1)];
  }, keys, Array);
  return result$$5;
}

function distinctBy(projection$$1, array$$17, eq$$1) {
  const hashSet = (0, _Set.createMutable)([], eq$$1);
  return filter(function predicate$$2($arg$$3) {
    const arg00 = projection$$1($arg$$3);
    return (0, _Util.addToSet)(arg00, hashSet);
  }, array$$17);
}

function distinct(array$$19, eq$$2) {
  return distinctBy(function (x$$2) {
    return x$$2;
  }, array$$19, eq$$2);
}

function where(predicate$$3, array$$20) {
  return array$$20.filter(predicate$$3);
}

function contains(value$$4, array$$22, eq$$3) {
  const loop = function loop($i$$12$$75) {
    loop: while (true) {
      const i$$12 = $i$$12$$75;

      if (i$$12 >= array$$22.length) {
        return false;
      } else if (eq$$3.Equals(value$$4, array$$22[i$$12])) {
        return true;
      } else {
        $i$$12$$75 = i$$12 + 1;
        continue loop;
      }

      break;
    }
  };

  return loop(0);
}

function except(itemsToExclude, array$$23, eq$$4) {
  if (array$$23.length === 0) {
    return array$$23;
  } else {
    const cached = (0, _Set.createMutable)(itemsToExclude, eq$$4);
    return array$$23.filter(function predicate$$5(arg00$$1) {
      return (0, _Util.addToSet)(arg00$$1, cached);
    });
  }
}

function groupBy(projection$$2, array$$26, cons$$11, eq$$5) {
  const dict$$1 = (0, _Map.createMutable)([], eq$$5);
  const keys$$1 = [];

  for (let idx$$3 = 0; idx$$3 <= array$$26.length - 1; idx$$3++) {
    const v = array$$26[idx$$3];
    const key$$2 = projection$$2(v);
    const matchValue$$4 = (0, _Util.tryGetValue)(dict$$1, key$$2, null);

    if (matchValue$$4[0]) {
      dict$$1.set(key$$2, new _Types.List(v, matchValue$$4[1]));
    } else {
      (0, _Util.addToDict)(dict$$1, key$$2, new _Types.List(v, new _Types.List()));
      const value$$5 = keys$$1.push(key$$2);
      value$$5, null;
    }
  }

  const result$$6 = map(function (key$$3) {
    var array$$28;
    return [key$$3, (array$$28 = cons$$11.from((0, _Util.getItemFromDict)(dict$$1, key$$3)), array$$28.reverse())];
  }, keys$$1, Array);
  return result$$6;
}

function empty(cons$$12) {
  return new cons$$12(0);
}

function singleton(value$$6, cons$$14) {
  const ar = new cons$$14(1);
  ar[0] = value$$6;
  return ar;
}

function initialize(count$$8, initializer, cons$$15) {
  if (count$$8 < 0) {
    throw new Error("The input must be non-negative\\nParameter name: count");
  }

  const result$$7 = new cons$$15(count$$8);

  for (let i$$13 = 0; i$$13 <= count$$8 - 1; i$$13++) {
    result$$7[i$$13] = initializer(i$$13);
  }

  return result$$7;
}

function pairwise(array$$30) {
  if (array$$30.length < 2) {
    return [];
  } else {
    const count$$9 = array$$30.length - 1 | 0;
    const result$$8 = new Array(count$$9);

    for (let i$$14 = 0; i$$14 <= count$$9 - 1; i$$14++) {
      result$$8[i$$14] = [array$$30[i$$14], array$$30[i$$14 + 1]];
    }

    return result$$8;
  }
}

function replicate(count$$10, initial, cons$$16) {
  if (count$$10 < 0) {
    throw new Error("The input must be non-negative\\nParameter name: count");
  }

  const result$$9 = new cons$$16(count$$10);

  for (let i$$15 = 0; i$$15 <= result$$9.length - 1; i$$15++) {
    result$$9[i$$15] = initial;
  }

  return result$$9;
}

function copy(array$$31, cons$$17) {
  return array$$31.slice();
}

function reverse(array$$33, cons$$18) {
  const array$$35 = array$$33.slice();
  return array$$35.reverse();
}

function scan(folder, state$$2, array$$37, cons$$19) {
  const res$$2 = new cons$$19(array$$37.length + 1);
  res$$2[0] = state$$2;

  for (let i$$16 = 0; i$$16 <= array$$37.length - 1; i$$16++) {
    res$$2[i$$16 + 1] = folder(res$$2[i$$16], array$$37[i$$16]);
  }

  return res$$2;
}

function scanBack(folder$$1, array$$38, state$$3, cons$$20) {
  const res$$3 = new cons$$20(array$$38.length + 1);
  res$$3[array$$38.length] = state$$3;

  for (let i$$17 = array$$38.length - 1; i$$17 >= 0; i$$17--) {
    res$$3[i$$17] = folder$$1(array$$38[i$$17], res$$3[i$$17 + 1]);
  }

  return res$$3;
}

function skip(count$$11, array$$39, cons$$21) {
  if (count$$11 > array$$39.length) {
    throw new Error("count is greater than array length\\nParameter name: count");
  }

  if (count$$11 === array$$39.length) {
    return new cons$$21(0);
  } else {
    const count$$12 = (count$$11 < 0 ? 0 : count$$11) | 0;
    return array$$39.slice(count$$12);
  }
}

function skipWhile(predicate$$7, array$$41, cons$$23) {
  let count$$14 = 0;

  while (count$$14 < array$$41.length ? predicate$$7(array$$41[count$$14]) : false) {
    count$$14 = count$$14 + 1;
  }

  if (count$$14 === array$$41.length) {
    return new cons$$23(0);
  } else {
    const count$$15 = count$$14 | 0;
    return array$$41.slice(count$$15);
  }
}

function take(count$$16, array$$43, cons$$25) {
  if (count$$16 < 0) {
    throw new Error("The input must be non-negative\\nParameter name: count");
  }

  if (count$$16 > array$$43.length) {
    throw new Error("count is greater than array length\\nParameter name: count");
  }

  if (count$$16 === 0) {
    return new cons$$25(0);
  } else {
    return array$$43.slice(0, 0 + count$$16);
  }
}

function takeWhile(predicate$$8, array$$45, cons$$27) {
  let count$$18 = 0;

  while (count$$18 < array$$45.length ? predicate$$8(array$$45[count$$18]) : false) {
    count$$18 = count$$18 + 1;
  }

  if (count$$18 === 0) {
    return new cons$$27(0);
  } else {
    const count$$19 = count$$18 | 0;
    return array$$45.slice(0, 0 + count$$19);
  }
}

function addRangeInPlace(range, array$$47) {
  (0, _Seq.iterate)(function (x$$3) {
    const value$$7 = array$$47.push(x$$3);
    value$$7, null;
  }, range);
}

function removeInPlace(item$$4, array$$49) {
  const i$$18 = array$$49.indexOf(item$$4);

  if (i$$18 > -1) {
    const value$$8 = array$$49.splice(i$$18, 1);
    value$$8, null;
    return true;
  } else {
    return false;
  }
}

function removeAllInPlace(predicate$$9, array$$52) {
  const countRemoveAll = function countRemoveAll(count$$20) {
    const i$$19 = array$$52.findIndex(predicate$$9);

    if (i$$19 > -1) {
      const value$$9 = array$$52.splice(i$$19, 1);
      value$$9, null;
      return countRemoveAll(count$$20) + 1 | 0;
    } else {
      return count$$20 | 0;
    }
  };

  return countRemoveAll(0) | 0;
}

function copyTo(source$$3, sourceIndex, target$$4, targetIndex$$1, count$$21) {
  const diff = targetIndex$$1 - sourceIndex | 0;

  for (let i$$20 = sourceIndex; i$$20 <= sourceIndex + count$$21 - 1; i$$20++) {
    target$$4[i$$20 + diff] = source$$3[i$$20];
  }
}

function partition(f$$6, source$$4, cons$$29) {
  const len$$7 = source$$4.length | 0;
  const res1 = new cons$$29(len$$7);
  const res2 = new cons$$29(len$$7);
  let iTrue = 0;
  let iFalse = 0;

  for (let i$$21 = 0; i$$21 <= len$$7 - 1; i$$21++) {
    if (f$$6(source$$4[i$$21])) {
      res1[iTrue] = source$$4[i$$21];
      iTrue = iTrue + 1;
    } else {
      res2[iFalse] = source$$4[i$$21];
      iFalse = iFalse + 1;
    }
  }

  return [truncate(iTrue, res1), truncate(iFalse, res2)];
}

function find(predicate$$11, array$$57) {
  const matchValue$$5 = array$$57.find(predicate$$11);

  if (matchValue$$5 == null) {
    return indexNotFound();
  } else {
    const res$$4 = (0, _Option.value)(matchValue$$5);
    return res$$4;
  }
}

function tryFind(predicate$$13, array$$59) {
  return array$$59.find(predicate$$13);
}

function findIndex(predicate$$15, array$$61) {
  const matchValue$$6 = array$$61.findIndex(predicate$$15);

  if (matchValue$$6 > -1) {
    return matchValue$$6 | 0;
  } else {
    return indexNotFound() | 0;
  }
}

function tryFindIndex(predicate$$17, array$$63) {
  const matchValue$$7 = array$$63.findIndex(predicate$$17);

  if (matchValue$$7 > -1) {
    return matchValue$$7;
  } else {
    return null;
  }
}

function pick(chooser, array$$65) {
  const loop$$1 = function loop$$1($i$$22$$143) {
    loop$$1: while (true) {
      const i$$22 = $i$$22$$143;

      if (i$$22 >= array$$65.length) {
        return indexNotFound();
      } else {
        const matchValue$$8 = chooser(array$$65[i$$22]);

        if (matchValue$$8 != null) {
          const res$$5 = (0, _Option.value)(matchValue$$8);
          return res$$5;
        } else {
          $i$$22$$143 = i$$22 + 1;
          continue loop$$1;
        }
      }

      break;
    }
  };

  return loop$$1(0);
}

function tryPick(chooser$$1, array$$66) {
  const loop$$2 = function loop$$2($i$$23$$146) {
    loop$$2: while (true) {
      const i$$23 = $i$$23$$146;

      if (i$$23 >= array$$66.length) {
        return null;
      } else {
        const matchValue$$9 = chooser$$1(array$$66[i$$23]);

        if (matchValue$$9 == null) {
          $i$$23$$146 = i$$23 + 1;
          continue loop$$2;
        } else {
          return matchValue$$9;
        }
      }

      break;
    }
  };

  return loop$$2(0);
}

function findBack(predicate$$19, array$$67) {
  const loop$$3 = function loop$$3($i$$24$$149) {
    loop$$3: while (true) {
      const i$$24 = $i$$24$$149;

      if (i$$24 < 0) {
        return indexNotFound();
      } else if (predicate$$19(array$$67[i$$24])) {
        return array$$67[i$$24];
      } else {
        $i$$24$$149 = i$$24 - 1;
        continue loop$$3;
      }

      break;
    }
  };

  return loop$$3(array$$67.length - 1);
}

function tryFindBack(predicate$$20, array$$68) {
  const loop$$4 = function loop$$4($i$$25$$152) {
    loop$$4: while (true) {
      const i$$25 = $i$$25$$152;

      if (i$$25 < 0) {
        return null;
      } else if (predicate$$20(array$$68[i$$25])) {
        return (0, _Option.some)(array$$68[i$$25]);
      } else {
        $i$$25$$152 = i$$25 - 1;
        continue loop$$4;
      }

      break;
    }
  };

  return loop$$4(array$$68.length - 1);
}

function findLastIndex(predicate$$21, array$$69) {
  const loop$$5 = function loop$$5($i$$26$$155) {
    loop$$5: while (true) {
      const i$$26 = $i$$26$$155;

      if (i$$26 < 0) {
        return -1 | 0;
      } else if (predicate$$21(array$$69[i$$26])) {
        return i$$26 | 0;
      } else {
        $i$$26$$155 = i$$26 - 1;
        continue loop$$5;
      }

      break;
    }
  };

  return loop$$5(array$$69.length - 1) | 0;
}

function findIndexBack(predicate$$22, array$$70) {
  const loop$$6 = function loop$$6($i$$27$$158) {
    loop$$6: while (true) {
      const i$$27 = $i$$27$$158;

      if (i$$27 < 0) {
        return indexNotFound() | 0;
      } else if (predicate$$22(array$$70[i$$27])) {
        return i$$27 | 0;
      } else {
        $i$$27$$158 = i$$27 - 1;
        continue loop$$6;
      }

      break;
    }
  };

  return loop$$6(array$$70.length - 1) | 0;
}

function tryFindIndexBack(predicate$$23, array$$71) {
  const loop$$7 = function loop$$7($i$$28$$161) {
    loop$$7: while (true) {
      const i$$28 = $i$$28$$161;

      if (i$$28 < 0) {
        return null;
      } else if (predicate$$23(array$$71[i$$28])) {
        return i$$28;
      } else {
        $i$$28$$161 = i$$28 - 1;
        continue loop$$7;
      }

      break;
    }
  };

  return loop$$7(array$$71.length - 1);
}

function choose(chooser$$2, array$$72, cons$$30) {
  const arr$$6 = array$$72.filter(function f$$7(x$$4) {
    const option = chooser$$2(x$$4);
    return option != null;
  });
  return map(function g(x$$5) {
    const option$$1 = chooser$$2(x$$5);
    return (0, _Option.value)(option$$1);
  }, arr$$6, cons$$30);
}

function foldIndexed(folder$$2, state$$4, array$$74) {
  return array$$74.reduce(function (delegateArg0$$1, delegateArg1$$1, delegateArg2) {
    return folder$$2(delegateArg2, delegateArg0$$1, delegateArg1$$1);
  }, state$$4);
}

function fold(folder$$4, state$$6, array$$76) {
  return array$$76.reduce(function (delegateArg0$$2, delegateArg1$$2) {
    return folder$$4(delegateArg0$$2, delegateArg1$$2);
  }, state$$6);
}

function iterate(action, array$$78) {
  for (let i$$30 = 0; i$$30 <= array$$78.length - 1; i$$30++) {
    action(array$$78[i$$30]);
  }
}

function iterateIndexed(action$$1, array$$79) {
  for (let i$$31 = 0; i$$31 <= array$$79.length - 1; i$$31++) {
    action$$1(i$$31, array$$79[i$$31]);
  }
}

function iterate2(action$$2, array1$$2, array2$$2) {
  if (array1$$2.length !== array2$$2.length) {
    throw new Error("Arrays had different lengths");
  }

  for (let i$$32 = 0; i$$32 <= array1$$2.length - 1; i$$32++) {
    action$$2(array1$$2[i$$32], array2$$2[i$$32]);
  }
}

function iterateIndexed2(action$$3, array1$$3, array2$$3) {
  if (array1$$3.length !== array2$$3.length) {
    throw new Error("Arrays had different lengths");
  }

  for (let i$$33 = 0; i$$33 <= array1$$3.length - 1; i$$33++) {
    action$$3(i$$33, array1$$3[i$$33], array2$$3[i$$33]);
  }
}

function isEmpty(array$$80) {
  return array$$80.length === 0;
}

function forAll(predicate$$25, array$$81) {
  return array$$81.every(predicate$$25);
}

function permute(f$$8, array$$83) {
  const size = array$$83.length | 0;
  const res$$7 = new array$$83.constructor(array$$83.length);
  const checkFlags = new Array(size);
  iterateIndexed(function (i$$34, x$$8) {
    const j$$1 = f$$8(i$$34) | 0;

    if (j$$1 < 0 ? true : j$$1 >= size) {
      throw new Error("Not a valid permutation");
    }

    res$$7[j$$1] = x$$8;
    checkFlags[j$$1] = 1;
  }, array$$83);
  const isValid = forAll(function (y) {
    return 1 === y;
  }, checkFlags);

  if (!isValid) {
    throw new Error("Not a valid permutation");
  }

  return res$$7;
}

function setSlice(target$$5, lower, upper, source$$5) {
  const lower$$1 = (0, _Option.defaultArg)(lower, 0) | 0;
  const upper$$1 = (0, _Option.defaultArg)(upper, 0) | 0;
  const length = (upper$$1 > 0 ? upper$$1 : target$$5.length - 1) - lower$$1 | 0;

  if (ArrayBuffer.isView(target$$5) ? source$$5.length <= length : false) {
    const target$$6 = target$$5;
    const source$$6 = source$$5;
    return target$$6.set(source$$6, lower$$1);
  } else {
    for (let i$$35 = 0; i$$35 <= length; i$$35++) {
      target$$5[i$$35 + lower$$1] = source$$5[i$$35];
    }
  }
}

function sortInPlaceBy(projection$$3, xs, comparer) {
  xs.sort(function (x$$10, y$$1) {
    return comparer.Compare(projection$$3(x$$10), projection$$3(y$$1));
  });
}

function sortInPlace(xs$$1, comparer$$1) {
  xs$$1.sort(function (x$$11, y$$2) {
    return comparer$$1.Compare(x$$11, y$$2);
  });
}

function copyArray(array$$84) {
  const result$$10 = new array$$84.constructor(array$$84.length);

  for (let i$$36 = 0; i$$36 <= array$$84.length - 1; i$$36++) {
    result$$10[i$$36] = array$$84[i$$36];
  }

  return result$$10;
}

function sort(xs$$2, comparer$$2) {
  const xs$$3 = copyArray(xs$$2);
  xs$$3.sort(function comparer$$3(x$$12, y$$3) {
    return comparer$$2.Compare(x$$12, y$$3);
  });
  return xs$$3;
}

function sortBy(projection$$4, xs$$4, comparer$$4) {
  const xs$$5 = copyArray(xs$$4);
  xs$$5.sort(function comparer$$5(x$$13, y$$4) {
    return comparer$$4.Compare(projection$$4(x$$13), projection$$4(y$$4));
  });
  return xs$$5;
}

function sortDescending(xs$$6, comparer$$6) {
  const xs$$7 = copyArray(xs$$6);
  xs$$7.sort(function comparer$$7(x$$14, y$$5) {
    return comparer$$6.Compare(x$$14, y$$5) * -1;
  });
  return xs$$7;
}

function sortByDescending(projection$$5, xs$$8, comparer$$8) {
  const xs$$9 = copyArray(xs$$8);
  xs$$9.sort(function comparer$$9(x$$15, y$$6) {
    return comparer$$8.Compare(projection$$5(x$$15), projection$$5(y$$6)) * -1;
  });
  return xs$$9;
}

function sortWith(comparer$$10, xs$$10) {
  const xs$$11 = copyArray(xs$$10);
  xs$$11.sort(comparer$$10);
  return xs$$11;
}

function unfold(generator, state$$8) {
  const res$$8 = [];

  const loop$$8 = function loop$$8($state$$9$$220) {
    loop$$8: while (true) {
      const state$$9 = $state$$9$$220;
      const matchValue$$10 = generator(state$$9);

      if (matchValue$$10 != null) {
        const x$$16 = matchValue$$10[0];
        const s$0027$$2 = matchValue$$10[1];
        const value$$10 = res$$8.push(x$$16);
        value$$10, null;
        $state$$9$$220 = s$0027$$2;
        continue loop$$8;
      }

      break;
    }
  };

  loop$$8(state$$8);
  return res$$8;
}

function unzip(array$$86) {
  const len$$9 = array$$86.length | 0;
  const res1$$1 = new Array(len$$9);
  const res2$$1 = new Array(len$$9);
  iterateIndexed(function (i$$37, tupledArg) {
    res1$$1[i$$37] = tupledArg[0];
    res2$$1[i$$37] = tupledArg[1];
  }, array$$86);
  return [res1$$1, res2$$1];
}

function unzip3(array$$87) {
  const len$$12 = array$$87.length | 0;
  const res1$$2 = new Array(len$$12);
  const res2$$2 = new Array(len$$12);
  const res3 = new Array(len$$12);
  iterateIndexed(function (i$$38, tupledArg$$1) {
    res1$$2[i$$38] = tupledArg$$1[0];
    res2$$2[i$$38] = tupledArg$$1[1];
    res3[i$$38] = tupledArg$$1[2];
  }, array$$87);
  return [res1$$2, res2$$2, res3];
}

function zip(array1$$4, array2$$4) {
  if (array1$$4.length !== array2$$4.length) {
    throw new Error("Arrays had different lengths");
  }

  let result$$11;
  const len$$16 = array1$$4.length | 0;
  result$$11 = new Array(len$$16);

  for (let i$$39 = 0; i$$39 <= array1$$4.length - 1; i$$39++) {
    result$$11[i$$39] = [array1$$4[i$$39], array2$$4[i$$39]];
  }

  return result$$11;
}

function zip3(array1$$5, array2$$5, array3) {
  if (array1$$5.length !== array2$$5.length ? true : array2$$5.length !== array3.length) {
    throw new Error("Arrays had different lengths");
  }

  let result$$12;
  const len$$17 = array1$$5.length | 0;
  result$$12 = new Array(len$$17);

  for (let i$$40 = 0; i$$40 <= array1$$5.length - 1; i$$40++) {
    result$$12[i$$40] = [array1$$5[i$$40], array2$$5[i$$40], array3[i$$40]];
  }

  return result$$12;
}

function chunkBySize(chunkSize, array$$88) {
  if (chunkSize < 1) {
    throw new Error("The input must be positive.\\nParameter name: size");
  }

  if (array$$88.length === 0) {
    return [[]];
  } else {
    const result$$13 = [];

    for (let x$$17 = 0; x$$17 <= ~~Math.ceil(array$$88.length / chunkSize) - 1; x$$17++) {
      const start$$8 = x$$17 * chunkSize | 0;
      const slice = array$$88.slice(start$$8, start$$8 + chunkSize);
      const value$$11 = result$$13.push(slice);
      value$$11, null;
    }

    return result$$13;
  }
}

function splitAt(index$$4, array$$91) {
  if (index$$4 < 0) {
    throw new Error("The input must be non-negative\\nParameter name: index");
  }

  if (index$$4 > array$$91.length) {
    throw new Error("The input sequence has an insufficient number of elements.\\nParameter name: index");
  }

  return [array$$91.slice(0, 0 + index$$4), array$$91.slice(index$$4)];
}

function compareWith(comparer$$12, array1$$6, array2$$6) {
  if (array1$$6 == null) {
    if (array2$$6 == null) {
      return 0;
    } else {
      return -1 | 0;
    }
  } else if (array2$$6 == null) {
    return 1;
  } else {
    let i$$41 = 0;
    let result$$14 = 0;
    const length1 = array1$$6.length | 0;
    const length2 = array2$$6.length | 0;

    if (length1 > length2) {
      return 1;
    } else if (length1 < length2) {
      return -1 | 0;
    } else {
      while (i$$41 < length1 ? result$$14 === 0 : false) {
        result$$14 = comparer$$12(array1$$6[i$$41], array2$$6[i$$41]);
        i$$41 = i$$41 + 1;
      }

      return result$$14 | 0;
    }
  }
}

function equalsWith(comparer$$13, array1$$7, array2$$7) {
  return compareWith(_Util.compare, array1$$7, array2$$7) === 0;
}

function exactlyOne(array$$94) {
  if (array$$94.length === 1) {
    return array$$94[0];
  } else if (array$$94.length === 0) {
    throw new Error("The input sequence was empty\\nParameter name: array");
  } else {
    throw new Error("Input array too long\\nParameter name: array");
  }
}

function head(array$$95) {
  if (array$$95.length === 0) {
    throw new Error("The input array was empty\\nParameter name: array");
  } else {
    return array$$95[0];
  }
}

function tryHead(array$$96) {
  if (array$$96.length === 0) {
    return null;
  } else {
    return (0, _Option.some)(array$$96[0]);
  }
}

function tail(array$$97) {
  if (array$$97.length === 0) {
    throw new Error("Not enough elements\\nParameter name: array");
  }

  return array$$97.slice(1);
}

function item(index$$5, array$$99) {
  return array$$99[index$$5];
}

function tryItem(index$$6, array$$100) {
  if (index$$6 < 0 ? true : index$$6 >= array$$100.length) {
    return null;
  } else {
    return (0, _Option.some)(array$$100[index$$6]);
  }
}

function foldBackIndexed(folder$$6, array$$101, state$$10) {
  return array$$101.reduceRight(function (delegateArg0$$3, delegateArg1$$3, delegateArg2$$1) {
    return folder$$6(delegateArg2$$1, delegateArg1$$3, delegateArg0$$3);
  }, state$$10);
}

function foldBack(folder$$8, array$$103, state$$12) {
  return array$$103.reduceRight(function (delegateArg0$$4, delegateArg1$$4) {
    return folder$$8(delegateArg1$$4, delegateArg0$$4);
  }, state$$12);
}

function foldIndexed2(folder$$10, state$$14, array1$$8, array2$$8) {
  let acc$$6 = state$$14;

  if (array1$$8.length !== array2$$8.length) {
    throw new Error("Arrays have different lengths");
  }

  for (let i$$43 = 0; i$$43 <= array1$$8.length - 1; i$$43++) {
    acc$$6 = folder$$10(i$$43, acc$$6, array1$$8[i$$43], array2$$8[i$$43]);
  }

  return acc$$6;
}

function fold2(folder$$11, state$$15, array1$$9, array2$$9) {
  return foldIndexed2(function (_arg1, acc$$7, x$$20, y$$7) {
    return folder$$11(acc$$7, x$$20, y$$7);
  }, state$$15, array1$$9, array2$$9);
}

function foldBackIndexed2(folder$$12, array1$$10, array2$$10, state$$16) {
  let acc$$8 = state$$16;

  if (array1$$10.length !== array2$$10.length) {
    throw new Error("Arrays had different lengths");
  }

  const size$$1 = array1$$10.length | 0;

  for (let i$$44 = 1; i$$44 <= size$$1; i$$44++) {
    acc$$8 = folder$$12(i$$44 - 1, array1$$10[size$$1 - i$$44], array2$$10[size$$1 - i$$44], acc$$8);
  }

  return acc$$8;
}

function foldBack2(f$$9, array1$$11, array2$$11, state$$17) {
  return foldBackIndexed2(function (_arg1$$1, x$$21, y$$8, acc$$9) {
    return f$$9(x$$21, y$$8, acc$$9);
  }, array1$$11, array2$$11, state$$17);
}

function reduce(reduction, array$$105) {
  if (array$$105.length === 0) {
    throw new Error("The input array was empty");
  }

  return array$$105.reduce(reduction);
}

function reduceBack(reduction$$2, array$$107) {
  if (array$$107.length === 0) {
    throw new Error("The input array was empty");
  }

  return array$$107.reduceRight(reduction$$2);
}

function forAll2(predicate$$27, array1$$12, array2$$12) {
  return fold2(function (acc$$10, x$$22, y$$9) {
    return acc$$10 ? predicate$$27(x$$22, y$$9) : false;
  }, true, array1$$12, array2$$12);
}

function existsOffset($predicate$$28$$275, $array$$109$$276, $index$$7$$277) {
  existsOffset: while (true) {
    const predicate$$28 = $predicate$$28$$275,
          array$$109 = $array$$109$$276,
          index$$7 = $index$$7$$277;

    if (index$$7 === array$$109.length) {
      return false;
    } else if (predicate$$28(array$$109[index$$7])) {
      return true;
    } else {
      $predicate$$28$$275 = predicate$$28;
      $array$$109$$276 = array$$109;
      $index$$7$$277 = index$$7 + 1;
      continue existsOffset;
    }

    break;
  }
}

function exists(predicate$$29, array$$110) {
  return existsOffset(predicate$$29, array$$110, 0);
}

function existsOffset2($predicate$$30$$280, $array1$$13$$281, $array2$$13$$282, $index$$8$$283) {
  existsOffset2: while (true) {
    const predicate$$30 = $predicate$$30$$280,
          array1$$13 = $array1$$13$$281,
          array2$$13 = $array2$$13$$282,
          index$$8 = $index$$8$$283;

    if (index$$8 === array1$$13.length) {
      return false;
    } else if (predicate$$30(array1$$13[index$$8], array2$$13[index$$8])) {
      return true;
    } else {
      $predicate$$30$$280 = predicate$$30;
      $array1$$13$$281 = array1$$13;
      $array2$$13$$282 = array2$$13;
      $index$$8$$283 = index$$8 + 1;
      continue existsOffset2;
    }

    break;
  }
}

function exists2(predicate$$31, array1$$14, array2$$14) {
  if (array1$$14.length !== array2$$14.length) {
    throw new Error("Arrays had different lengths");
  }

  return existsOffset2(predicate$$31, array1$$14, array2$$14, 0);
}

function sum(array$$111, adder) {
  let acc$$11 = adder.GetZero();

  for (let i$$45 = 0; i$$45 <= array$$111.length - 1; i$$45++) {
    acc$$11 = adder.Add(acc$$11, array$$111[i$$45]);
  }

  return acc$$11;
}

function sumBy(projection$$6, array$$112, adder$$1) {
  let acc$$12 = adder$$1.GetZero();

  for (let i$$46 = 0; i$$46 <= array$$112.length - 1; i$$46++) {
    acc$$12 = adder$$1.Add(acc$$12, projection$$6(array$$112[i$$46]));
  }

  return acc$$12;
}

function maxBy(projection$$7, xs$$12, comparer$$14) {
  return reduce(function (x$$23, y$$10) {
    return comparer$$14.Compare(projection$$7(y$$10), projection$$7(x$$23)) > 0 ? y$$10 : x$$23;
  }, xs$$12);
}

function max(xs$$13, comparer$$15) {
  return reduce(function (x$$24, y$$11) {
    return comparer$$15.Compare(y$$11, x$$24) > 0 ? y$$11 : x$$24;
  }, xs$$13);
}

function minBy(projection$$8, xs$$14, comparer$$16) {
  return reduce(function (x$$25, y$$12) {
    return comparer$$16.Compare(projection$$8(y$$12), projection$$8(x$$25)) > 0 ? x$$25 : y$$12;
  }, xs$$14);
}

function min(xs$$15, comparer$$17) {
  return reduce(function (x$$26, y$$13) {
    return comparer$$17.Compare(y$$13, x$$26) > 0 ? x$$26 : y$$13;
  }, xs$$15);
}

function average(array$$113, averager) {
  if (array$$113.length === 0) {
    throw new Error("The input array was empty\\nParameter name: array");
  }

  let total = averager.GetZero();

  for (let i$$47 = 0; i$$47 <= array$$113.length - 1; i$$47++) {
    total = averager.Add(total, array$$113[i$$47]);
  }

  return averager.DivideByInt(total, array$$113.length);
}

function averageBy(projection$$9, array$$114, averager$$1) {
  if (array$$114.length === 0) {
    throw new Error("The input array was empty\\nParameter name: array");
  }

  let total$$1 = averager$$1.GetZero();

  for (let i$$48 = 0; i$$48 <= array$$114.length - 1; i$$48++) {
    total$$1 = averager$$1.Add(total$$1, projection$$9(array$$114[i$$48]));
  }

  return averager$$1.DivideByInt(total$$1, array$$114.length);
}

function ofSeq(source$$7, cons$$31) {
  return cons$$31.from(source$$7);
}

function ofList(source$$8, cons$$32) {
  return cons$$32.from(source$$8);
}

function toList(source$$9) {
  const len$$18 = source$$9.length | 0;
  let target$$7 = new _Types.List();

  for (let i$$49 = len$$18 - 1; i$$49 >= 0; i$$49--) {
    target$$7 = new _Types.List(source$$9[i$$49], target$$7);
  }

  return target$$7;
}

function windowed(windowSize, source$$10) {
  if (windowSize <= 0) {
    throw new Error("windowSize must be positive");
  }

  let res$$9;
  const len$$19 = (0, _Util.max)(_Util.comparePrimitives, 0, source$$10.length - windowSize) | 0;
  res$$9 = new Array(len$$19);

  for (let i$$50 = windowSize; i$$50 <= source$$10.length; i$$50++) {
    res$$9[i$$50 - windowSize] = source$$10.slice(i$$50 - windowSize, i$$50 - 1 + 1);
  }

  return res$$9;
}