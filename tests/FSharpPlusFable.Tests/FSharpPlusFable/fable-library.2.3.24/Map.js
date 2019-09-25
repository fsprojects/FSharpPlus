"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.MapTree$00602$reflection = MapTree$00602$reflection;
exports.MapTreeModule$$$sizeAux = MapTreeModule$$$sizeAux;
exports.MapTreeModule$$$size = MapTreeModule$$$size;
exports.MapTreeModule$$$empty = MapTreeModule$$$empty;
exports.MapTreeModule$$$height = MapTreeModule$$$height;
exports.MapTreeModule$$$isEmpty = MapTreeModule$$$isEmpty;
exports.MapTreeModule$$$mk = MapTreeModule$$$mk;
exports.MapTreeModule$$$rebalance = MapTreeModule$$$rebalance;
exports.MapTreeModule$$$add = MapTreeModule$$$add;
exports.MapTreeModule$$$find = MapTreeModule$$$find;
exports.MapTreeModule$$$tryFind = MapTreeModule$$$tryFind;
exports.MapTreeModule$$$partition1 = MapTreeModule$$$partition1;
exports.MapTreeModule$$$partitionAux = MapTreeModule$$$partitionAux;
exports.MapTreeModule$$$partition = MapTreeModule$$$partition;
exports.MapTreeModule$$$filter1 = MapTreeModule$$$filter1;
exports.MapTreeModule$$$filterAux = MapTreeModule$$$filterAux;
exports.MapTreeModule$$$filter = MapTreeModule$$$filter;
exports.MapTreeModule$$$spliceOutSuccessor = MapTreeModule$$$spliceOutSuccessor;
exports.MapTreeModule$$$remove = MapTreeModule$$$remove;
exports.MapTreeModule$$$mem = MapTreeModule$$$mem;
exports.MapTreeModule$$$iter = MapTreeModule$$$iter;
exports.MapTreeModule$$$tryPick = MapTreeModule$$$tryPick;
exports.MapTreeModule$$$exists = MapTreeModule$$$exists;
exports.MapTreeModule$$$forall = MapTreeModule$$$forall;
exports.MapTreeModule$$$map = MapTreeModule$$$map;
exports.MapTreeModule$$$mapi = MapTreeModule$$$mapi;
exports.MapTreeModule$$$foldBack = MapTreeModule$$$foldBack;
exports.MapTreeModule$$$fold = MapTreeModule$$$fold;
exports.MapTreeModule$$$foldFromTo = MapTreeModule$$$foldFromTo;
exports.MapTreeModule$$$foldSection = MapTreeModule$$$foldSection;
exports.MapTreeModule$$$loop = MapTreeModule$$$loop;
exports.MapTreeModule$$$toList = MapTreeModule$$$toList;
exports.MapTreeModule$$$ofList = MapTreeModule$$$ofList;
exports.MapTreeModule$$$mkFromEnumerator = MapTreeModule$$$mkFromEnumerator;
exports.MapTreeModule$$$ofArray = MapTreeModule$$$ofArray;
exports.MapTreeModule$$$ofSeq = MapTreeModule$$$ofSeq;
exports.MapTreeModule$$$copyToArray = MapTreeModule$$$copyToArray;
exports.MapTreeModule$002EMapIterator$00602$reflection = MapTreeModule$002EMapIterator$00602$reflection;
exports.MapTreeModule$$$collapseLHS = MapTreeModule$$$collapseLHS;
exports.MapTreeModule$$$mkIterator = MapTreeModule$$$mkIterator;
exports.MapTreeModule$$$notStarted = MapTreeModule$$$notStarted;
exports.MapTreeModule$$$alreadyFinished = MapTreeModule$$$alreadyFinished;
exports.MapTreeModule$$$current = MapTreeModule$$$current;
exports.MapTreeModule$$$moveNext = MapTreeModule$$$moveNext;
exports.MapTreeModule$002EmkIEnumerator$0027$00602$reflection = MapTreeModule$002EmkIEnumerator$0027$00602$reflection;
exports.MapTreeModule$002EmkIEnumerator$0027$00602$$$$002Ector$$Z26BC498C = MapTreeModule$002EmkIEnumerator$0027$00602$$$$002Ector$$Z26BC498C;
exports.MapTreeModule$$$mkIEnumerator = MapTreeModule$$$mkIEnumerator;
exports.MapTreeModule$$$toSeq = MapTreeModule$$$toSeq;
exports.FSharpMap$reflection = FSharpMap$reflection;
exports.FSharpMap$$$$002Ector$$58ADD115 = FSharpMap$$$$002Ector$$58ADD115;
exports.FSharpMap$$get_Comparer = FSharpMap$$get_Comparer;
exports.FSharpMap$$get_Tree = FSharpMap$$get_Tree;
exports.FSharpMap$$Add$$5BDDA1 = FSharpMap$$Add$$5BDDA1;
exports.FSharpMap$$get_IsEmpty = FSharpMap$$get_IsEmpty;
exports.FSharpMap$$get_Item$$2B595 = FSharpMap$$get_Item$$2B595;
exports.FSharpMap$$TryGetValue$$5BDDA1 = FSharpMap$$TryGetValue$$5BDDA1;
exports.FSharpMap$$TryPick$$72321DD7 = FSharpMap$$TryPick$$72321DD7;
exports.FSharpMap$$Exists$$Z395DDC35 = FSharpMap$$Exists$$Z395DDC35;
exports.FSharpMap$$Filter$$Z395DDC35 = FSharpMap$$Filter$$Z395DDC35;
exports.FSharpMap$$ForAll$$Z395DDC35 = FSharpMap$$ForAll$$Z395DDC35;
exports.FSharpMap$$Fold = FSharpMap$$Fold;
exports.FSharpMap$$FoldSection = FSharpMap$$FoldSection;
exports.FSharpMap$$Iterate$$1DCFB91D = FSharpMap$$Iterate$$1DCFB91D;
exports.FSharpMap$$MapRange$$6DC7247 = FSharpMap$$MapRange$$6DC7247;
exports.FSharpMap$$Map$$Z6F6B671C = FSharpMap$$Map$$Z6F6B671C;
exports.FSharpMap$$Partition$$Z395DDC35 = FSharpMap$$Partition$$Z395DDC35;
exports.FSharpMap$$get_Count = FSharpMap$$get_Count;
exports.FSharpMap$$ContainsKey$$2B595 = FSharpMap$$ContainsKey$$2B595;
exports.FSharpMap$$Remove$$2B595 = FSharpMap$$Remove$$2B595;
exports.FSharpMap$$TryFind$$2B595 = FSharpMap$$TryFind$$2B595;
exports.FSharpMap$$ToList = FSharpMap$$ToList;
exports.isEmpty = isEmpty;
exports.add = add;
exports.find = find;
exports.tryFind = tryFind;
exports.remove = remove;
exports.containsKey = containsKey;
exports.iterate = iterate;
exports.tryPick = tryPick;
exports.pick = pick;
exports.exists = exists;
exports.filter = filter;
exports.partition = partition;
exports.forAll = forAll;
exports.mapRange = mapRange;
exports.map = map;
exports.fold = fold;
exports.foldBack = foldBack;
exports.toSeq = toSeq;
exports.findKey = findKey;
exports.tryFindKey = tryFindKey;
exports.ofList = ofList;
exports.ofSeq = ofSeq;
exports.ofArray = ofArray;
exports.toList = toList;
exports.toArray = toArray;
exports.empty = empty;
exports.createMutable = createMutable;
exports.groupBy = groupBy;
exports.countBy = countBy;
exports.count = count;
exports.FSharpMap = exports.MapTreeModule$002EmkIEnumerator$0027$00602 = exports.MapTreeModule$002EMapIterator$00602 = exports.MapTree$00602 = void 0;

var _Types = require("./Types");

var _Reflection = require("./Reflection");

var _Option = require("./Option");

var _Seq = require("./Seq");

var _Util = require("./Util");

var _String = require("./String");

var _MutableMap = require("./MutableMap");

const MapTree$00602 = (0, _Types.declare)(function Map_MapTree(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.MapTree$00602 = MapTree$00602;

function MapTree$00602$reflection($gen$$3, $gen$$4) {
  return (0, _Reflection.union)("Map.MapTree`2", [$gen$$3, $gen$$4], MapTree$00602, () => ["MapEmpty", ["MapOne", [$gen$$3, $gen$$4]], ["MapNode", [$gen$$3, $gen$$4, MapTree$00602$reflection($gen$$3, $gen$$4), MapTree$00602$reflection($gen$$3, $gen$$4), _Reflection.int32]]]);
}

function MapTreeModule$$$sizeAux($acc$$5, $m$$6) {
  MapTreeModule$$$sizeAux: while (true) {
    const acc = $acc$$5,
          m = $m$$6;

    switch (m.tag) {
      case 1:
        {
          return acc + 1 | 0;
        }

      case 2:
        {
          $acc$$5 = MapTreeModule$$$sizeAux(acc + 1, m.fields[2]);
          $m$$6 = m.fields[3];
          continue MapTreeModule$$$sizeAux;
        }

      default:
        {
          return acc | 0;
        }
    }

    break;
  }
}

function MapTreeModule$$$size(x) {
  return MapTreeModule$$$sizeAux(0, x);
}

function MapTreeModule$$$empty() {
  return new MapTree$00602(0, "MapEmpty");
}

function MapTreeModule$$$height(_arg1) {
  switch (_arg1.tag) {
    case 1:
      {
        return 1;
      }

    case 2:
      {
        return _arg1.fields[4] | 0;
      }

    default:
      {
        return 0;
      }
  }
}

function MapTreeModule$$$isEmpty(m$$1) {
  if (m$$1.tag === 0) {
    return true;
  } else {
    return false;
  }
}

function MapTreeModule$$$mk(l$$1, k, v, r$$1) {
  var $target$$14;

  if (l$$1.tag === 0) {
    if (r$$1.tag === 0) {
      $target$$14 = 0;
    } else {
      $target$$14 = 1;
    }
  } else {
    $target$$14 = 1;
  }

  switch ($target$$14) {
    case 0:
      {
        return new MapTree$00602(1, "MapOne", k, v);
      }

    case 1:
      {
        const hl = MapTreeModule$$$height(l$$1) | 0;
        const hr = MapTreeModule$$$height(r$$1) | 0;
        const m$$2 = (hl < hr ? hr : hl) | 0;
        return new MapTree$00602(2, "MapNode", k, v, l$$1, r$$1, m$$2 + 1);
      }
  }
}

function MapTreeModule$$$rebalance(t1, k$$1, v$$1, t2) {
  const t1h = MapTreeModule$$$height(t1) | 0;
  const t2h = MapTreeModule$$$height(t2) | 0;

  if (t2h > t1h + 2) {
    if (t2.tag === 2) {
      if (MapTreeModule$$$height(t2.fields[2]) > t1h + 1) {
        if (t2.fields[2].tag === 2) {
          return MapTreeModule$$$mk(MapTreeModule$$$mk(t1, k$$1, v$$1, t2.fields[2].fields[2]), t2.fields[2].fields[0], t2.fields[2].fields[1], MapTreeModule$$$mk(t2.fields[2].fields[3], t2.fields[0], t2.fields[1], t2.fields[3]));
        } else {
          throw new Error("rebalance");
        }
      } else {
        return MapTreeModule$$$mk(MapTreeModule$$$mk(t1, k$$1, v$$1, t2.fields[2]), t2.fields[0], t2.fields[1], t2.fields[3]);
      }
    } else {
      throw new Error("rebalance");
    }
  } else if (t1h > t2h + 2) {
    if (t1.tag === 2) {
      if (MapTreeModule$$$height(t1.fields[3]) > t2h + 1) {
        if (t1.fields[3].tag === 2) {
          return MapTreeModule$$$mk(MapTreeModule$$$mk(t1.fields[2], t1.fields[0], t1.fields[1], t1.fields[3].fields[2]), t1.fields[3].fields[0], t1.fields[3].fields[1], MapTreeModule$$$mk(t1.fields[3].fields[3], k$$1, v$$1, t2));
        } else {
          throw new Error("re  balance");
        }
      } else {
        return MapTreeModule$$$mk(t1.fields[2], t1.fields[0], t1.fields[1], MapTreeModule$$$mk(t1.fields[3], k$$1, v$$1, t2));
      }
    } else {
      throw new Error("rebalance");
    }
  } else {
    return MapTreeModule$$$mk(t1, k$$1, v$$1, t2);
  }
}

function MapTreeModule$$$add(comparer, k$$2, v$$2, m$$3) {
  switch (m$$3.tag) {
    case 1:
      {
        const c = comparer.Compare(k$$2, m$$3.fields[0]) | 0;

        if (c < 0) {
          return new MapTree$00602(2, "MapNode", k$$2, v$$2, new MapTree$00602(0, "MapEmpty"), m$$3, 2);
        } else if (c === 0) {
          return new MapTree$00602(1, "MapOne", k$$2, v$$2);
        } else {
          return new MapTree$00602(2, "MapNode", k$$2, v$$2, m$$3, new MapTree$00602(0, "MapEmpty"), 2);
        }
      }

    case 2:
      {
        const c$$1 = comparer.Compare(k$$2, m$$3.fields[0]) | 0;

        if (c$$1 < 0) {
          return MapTreeModule$$$rebalance(MapTreeModule$$$add(comparer, k$$2, v$$2, m$$3.fields[2]), m$$3.fields[0], m$$3.fields[1], m$$3.fields[3]);
        } else if (c$$1 === 0) {
          return new MapTree$00602(2, "MapNode", k$$2, v$$2, m$$3.fields[2], m$$3.fields[3], m$$3.fields[4]);
        } else {
          return MapTreeModule$$$rebalance(m$$3.fields[2], m$$3.fields[0], m$$3.fields[1], MapTreeModule$$$add(comparer, k$$2, v$$2, m$$3.fields[3]));
        }
      }

    default:
      {
        return new MapTree$00602(1, "MapOne", k$$2, v$$2);
      }
  }
}

function MapTreeModule$$$find($comparer$$1$$23, $k$$3$$24, $m$$4$$25) {
  MapTreeModule$$$find: while (true) {
    const comparer$$1 = $comparer$$1$$23,
          k$$3 = $k$$3$$24,
          m$$4 = $m$$4$$25;

    switch (m$$4.tag) {
      case 1:
        {
          const c$$2 = comparer$$1.Compare(k$$3, m$$4.fields[0]) | 0;

          if (c$$2 === 0) {
            return m$$4.fields[1];
          } else {
            throw new Error("key not found");
          }
        }

      case 2:
        {
          const c$$3 = comparer$$1.Compare(k$$3, m$$4.fields[0]) | 0;

          if (c$$3 < 0) {
            $comparer$$1$$23 = comparer$$1;
            $k$$3$$24 = k$$3;
            $m$$4$$25 = m$$4.fields[2];
            continue MapTreeModule$$$find;
          } else if (c$$3 === 0) {
            return m$$4.fields[1];
          } else {
            $comparer$$1$$23 = comparer$$1;
            $k$$3$$24 = k$$3;
            $m$$4$$25 = m$$4.fields[3];
            continue MapTreeModule$$$find;
          }
        }

      default:
        {
          throw new Error("key not found");
        }
    }

    break;
  }
}

function MapTreeModule$$$tryFind($comparer$$2$$26, $k$$4$$27, $m$$5$$28) {
  MapTreeModule$$$tryFind: while (true) {
    const comparer$$2 = $comparer$$2$$26,
          k$$4 = $k$$4$$27,
          m$$5 = $m$$5$$28;

    switch (m$$5.tag) {
      case 1:
        {
          const c$$4 = comparer$$2.Compare(k$$4, m$$5.fields[0]) | 0;

          if (c$$4 === 0) {
            return (0, _Option.some)(m$$5.fields[1]);
          } else {
            return null;
          }
        }

      case 2:
        {
          const c$$5 = comparer$$2.Compare(k$$4, m$$5.fields[0]) | 0;

          if (c$$5 < 0) {
            $comparer$$2$$26 = comparer$$2;
            $k$$4$$27 = k$$4;
            $m$$5$$28 = m$$5.fields[2];
            continue MapTreeModule$$$tryFind;
          } else if (c$$5 === 0) {
            return (0, _Option.some)(m$$5.fields[1]);
          } else {
            $comparer$$2$$26 = comparer$$2;
            $k$$4$$27 = k$$4;
            $m$$5$$28 = m$$5.fields[3];
            continue MapTreeModule$$$tryFind;
          }
        }

      default:
        {
          return null;
        }
    }

    break;
  }
}

function MapTreeModule$$$partition1(comparer$$3, f, k$$5, v$$3, acc1, acc2) {
  if (f(k$$5, v$$3)) {
    return [MapTreeModule$$$add(comparer$$3, k$$5, v$$3, acc1), acc2];
  } else {
    return [acc1, MapTreeModule$$$add(comparer$$3, k$$5, v$$3, acc2)];
  }
}

function MapTreeModule$$$partitionAux($comparer$$4$$35, $f$$1$$36, $s$$37, $acc_0$$38, $acc_1$$39) {
  MapTreeModule$$$partitionAux: while (true) {
    const comparer$$4 = $comparer$$4$$35,
          f$$1 = $f$$1$$36,
          s = $s$$37,
          acc_0 = $acc_0$$38,
          acc_1 = $acc_1$$39;

    switch (s.tag) {
      case 1:
        {
          return MapTreeModule$$$partition1(comparer$$4, f$$1, s.fields[0], s.fields[1], acc_0, acc_1);
        }

      case 2:
        {
          const acc$$2 = MapTreeModule$$$partitionAux(comparer$$4, f$$1, s.fields[3], acc_0, acc_1);
          const acc$$3 = MapTreeModule$$$partition1(comparer$$4, f$$1, s.fields[0], s.fields[1], acc$$2[0], acc$$2[1]);
          $comparer$$4$$35 = comparer$$4;
          $f$$1$$36 = f$$1;
          $s$$37 = s.fields[2];
          $acc_0$$38 = acc$$3[0];
          $acc_1$$39 = acc$$3[1];
          continue MapTreeModule$$$partitionAux;
        }

      default:
        {
          return [acc_0, acc_1];
        }
    }

    break;
  }
}

function MapTreeModule$$$partition(comparer$$5, f$$2, s$$1) {
  return MapTreeModule$$$partitionAux(comparer$$5, f$$2, s$$1, MapTreeModule$$$empty(), MapTreeModule$$$empty());
}

function MapTreeModule$$$filter1(comparer$$6, f$$3, k$$8, v$$6, acc$$4) {
  if (f$$3(k$$8, v$$6)) {
    return MapTreeModule$$$add(comparer$$6, k$$8, v$$6, acc$$4);
  } else {
    return acc$$4;
  }
}

function MapTreeModule$$$filterAux($comparer$$7$$48, $f$$4$$49, $s$$2$$50, $acc$$5$$51) {
  MapTreeModule$$$filterAux: while (true) {
    const comparer$$7 = $comparer$$7$$48,
          f$$4 = $f$$4$$49,
          s$$2 = $s$$2$$50,
          acc$$5 = $acc$$5$$51;

    switch (s$$2.tag) {
      case 1:
        {
          return MapTreeModule$$$filter1(comparer$$7, f$$4, s$$2.fields[0], s$$2.fields[1], acc$$5);
        }

      case 2:
        {
          const acc$$6 = MapTreeModule$$$filterAux(comparer$$7, f$$4, s$$2.fields[2], acc$$5);
          const acc$$7 = MapTreeModule$$$filter1(comparer$$7, f$$4, s$$2.fields[0], s$$2.fields[1], acc$$6);
          $comparer$$7$$48 = comparer$$7;
          $f$$4$$49 = f$$4;
          $s$$2$$50 = s$$2.fields[3];
          $acc$$5$$51 = acc$$7;
          continue MapTreeModule$$$filterAux;
        }

      default:
        {
          return acc$$5;
        }
    }

    break;
  }
}

function MapTreeModule$$$filter(comparer$$8, f$$5, s$$3) {
  return MapTreeModule$$$filterAux(comparer$$8, f$$5, s$$3, MapTreeModule$$$empty());
}

function MapTreeModule$$$spliceOutSuccessor(m$$6) {
  switch (m$$6.tag) {
    case 1:
      {
        return [m$$6.fields[0], m$$6.fields[1], new MapTree$00602(0, "MapEmpty")];
      }

    case 2:
      {
        if (m$$6.fields[2].tag === 0) {
          return [m$$6.fields[0], m$$6.fields[1], m$$6.fields[3]];
        } else {
          const patternInput = MapTreeModule$$$spliceOutSuccessor(m$$6.fields[2]);
          return [patternInput[0], patternInput[1], MapTreeModule$$$mk(patternInput[2], m$$6.fields[0], m$$6.fields[1], m$$6.fields[3])];
        }
      }

    default:
      {
        throw new Error("internal error: Map.spliceOutSuccessor");
      }
  }
}

function MapTreeModule$$$remove(comparer$$9, k$$11, m$$7) {
  switch (m$$7.tag) {
    case 1:
      {
        const c$$6 = comparer$$9.Compare(k$$11, m$$7.fields[0]) | 0;

        if (c$$6 === 0) {
          return new MapTree$00602(0, "MapEmpty");
        } else {
          return m$$7;
        }
      }

    case 2:
      {
        const c$$7 = comparer$$9.Compare(k$$11, m$$7.fields[0]) | 0;

        if (c$$7 < 0) {
          return MapTreeModule$$$rebalance(MapTreeModule$$$remove(comparer$$9, k$$11, m$$7.fields[2]), m$$7.fields[0], m$$7.fields[1], m$$7.fields[3]);
        } else if (c$$7 === 0) {
          if (m$$7.fields[2].tag === 0) {
            return m$$7.fields[3];
          } else if (m$$7.fields[3].tag === 0) {
            return m$$7.fields[2];
          } else {
            const patternInput$$1 = MapTreeModule$$$spliceOutSuccessor(m$$7.fields[3]);
            return MapTreeModule$$$mk(m$$7.fields[2], patternInput$$1[0], patternInput$$1[1], patternInput$$1[2]);
          }
        } else {
          return MapTreeModule$$$rebalance(m$$7.fields[2], m$$7.fields[0], m$$7.fields[1], MapTreeModule$$$remove(comparer$$9, k$$11, m$$7.fields[3]));
        }
      }

    default:
      {
        return MapTreeModule$$$empty();
      }
  }
}

function MapTreeModule$$$mem($comparer$$10$$59, $k$$12$$60, $m$$8$$61) {
  MapTreeModule$$$mem: while (true) {
    const comparer$$10 = $comparer$$10$$59,
          k$$12 = $k$$12$$60,
          m$$8 = $m$$8$$61;

    switch (m$$8.tag) {
      case 1:
        {
          return comparer$$10.Compare(k$$12, m$$8.fields[0]) === 0;
        }

      case 2:
        {
          const c$$8 = comparer$$10.Compare(k$$12, m$$8.fields[0]) | 0;

          if (c$$8 < 0) {
            $comparer$$10$$59 = comparer$$10;
            $k$$12$$60 = k$$12;
            $m$$8$$61 = m$$8.fields[2];
            continue MapTreeModule$$$mem;
          } else if (c$$8 === 0) {
            return true;
          } else {
            $comparer$$10$$59 = comparer$$10;
            $k$$12$$60 = k$$12;
            $m$$8$$61 = m$$8.fields[3];
            continue MapTreeModule$$$mem;
          }
        }

      default:
        {
          return false;
        }
    }

    break;
  }
}

function MapTreeModule$$$iter($f$$6$$62, $m$$9$$63) {
  MapTreeModule$$$iter: while (true) {
    const f$$6 = $f$$6$$62,
          m$$9 = $m$$9$$63;

    switch (m$$9.tag) {
      case 1:
        {
          f$$6(m$$9.fields[0], m$$9.fields[1]);
          break;
        }

      case 2:
        {
          MapTreeModule$$$iter(f$$6, m$$9.fields[2]);
          f$$6(m$$9.fields[0], m$$9.fields[1]);
          $f$$6$$62 = f$$6;
          $m$$9$$63 = m$$9.fields[3];
          continue MapTreeModule$$$iter;
          break;
        }

      default:
        {}
    }

    break;
  }
}

function MapTreeModule$$$tryPick($f$$7$$64, $m$$10$$65) {
  MapTreeModule$$$tryPick: while (true) {
    const f$$7 = $f$$7$$64,
          m$$10 = $m$$10$$65;

    switch (m$$10.tag) {
      case 1:
        {
          return f$$7(m$$10.fields[0], m$$10.fields[1]);
        }

      case 2:
        {
          const matchValue$$2 = MapTreeModule$$$tryPick(f$$7, m$$10.fields[2]);

          if (matchValue$$2 == null) {
            const matchValue$$3 = f$$7(m$$10.fields[0], m$$10.fields[1]);

            if (matchValue$$3 == null) {
              $f$$7$$64 = f$$7;
              $m$$10$$65 = m$$10.fields[3];
              continue MapTreeModule$$$tryPick;
            } else {
              return matchValue$$3;
            }
          } else {
            return matchValue$$2;
          }
        }

      default:
        {
          return null;
        }
    }

    break;
  }
}

function MapTreeModule$$$exists($f$$8$$66, $m$$11$$67) {
  MapTreeModule$$$exists: while (true) {
    const f$$8 = $f$$8$$66,
          m$$11 = $m$$11$$67;

    switch (m$$11.tag) {
      case 1:
        {
          return f$$8(m$$11.fields[0], m$$11.fields[1]);
        }

      case 2:
        {
          if (MapTreeModule$$$exists(f$$8, m$$11.fields[2]) ? true : f$$8(m$$11.fields[0], m$$11.fields[1])) {
            return true;
          } else {
            $f$$8$$66 = f$$8;
            $m$$11$$67 = m$$11.fields[3];
            continue MapTreeModule$$$exists;
          }
        }

      default:
        {
          return false;
        }
    }

    break;
  }
}

function MapTreeModule$$$forall($f$$9$$68, $m$$12$$69) {
  MapTreeModule$$$forall: while (true) {
    const f$$9 = $f$$9$$68,
          m$$12 = $m$$12$$69;

    switch (m$$12.tag) {
      case 1:
        {
          return f$$9(m$$12.fields[0], m$$12.fields[1]);
        }

      case 2:
        {
          if (MapTreeModule$$$forall(f$$9, m$$12.fields[2]) ? f$$9(m$$12.fields[0], m$$12.fields[1]) : false) {
            $f$$9$$68 = f$$9;
            $m$$12$$69 = m$$12.fields[3];
            continue MapTreeModule$$$forall;
          } else {
            return false;
          }
        }

      default:
        {
          return true;
        }
    }

    break;
  }
}

function MapTreeModule$$$map(f$$10, m$$13) {
  switch (m$$13.tag) {
    case 1:
      {
        return new MapTree$00602(1, "MapOne", m$$13.fields[0], f$$10(m$$13.fields[1]));
      }

    case 2:
      {
        const l2 = MapTreeModule$$$map(f$$10, m$$13.fields[2]);
        const v2$$16 = f$$10(m$$13.fields[1]);
        const r2 = MapTreeModule$$$map(f$$10, m$$13.fields[3]);
        return new MapTree$00602(2, "MapNode", m$$13.fields[0], v2$$16, l2, r2, m$$13.fields[4]);
      }

    default:
      {
        return MapTreeModule$$$empty();
      }
  }
}

function MapTreeModule$$$mapi(f$$11, m$$14) {
  switch (m$$14.tag) {
    case 1:
      {
        return new MapTree$00602(1, "MapOne", m$$14.fields[0], f$$11(m$$14.fields[0], m$$14.fields[1]));
      }

    case 2:
      {
        const l2$$1 = MapTreeModule$$$mapi(f$$11, m$$14.fields[2]);
        const v2$$17 = f$$11(m$$14.fields[0], m$$14.fields[1]);
        const r2$$1 = MapTreeModule$$$mapi(f$$11, m$$14.fields[3]);
        return new MapTree$00602(2, "MapNode", m$$14.fields[0], v2$$17, l2$$1, r2$$1, m$$14.fields[4]);
      }

    default:
      {
        return MapTreeModule$$$empty();
      }
  }
}

function MapTreeModule$$$foldBack($f$$12$$74, $m$$15$$75, $x$$1$$76) {
  MapTreeModule$$$foldBack: while (true) {
    const f$$12 = $f$$12$$74,
          m$$15 = $m$$15$$75,
          x$$1 = $x$$1$$76;

    switch (m$$15.tag) {
      case 1:
        {
          return f$$12(m$$15.fields[0], m$$15.fields[1], x$$1);
        }

      case 2:
        {
          const x$$2 = MapTreeModule$$$foldBack(f$$12, m$$15.fields[3], x$$1);
          const x$$3 = f$$12(m$$15.fields[0], m$$15.fields[1], x$$2);
          $f$$12$$74 = f$$12;
          $m$$15$$75 = m$$15.fields[2];
          $x$$1$$76 = x$$3;
          continue MapTreeModule$$$foldBack;
        }

      default:
        {
          return x$$1;
        }
    }

    break;
  }
}

function MapTreeModule$$$fold($f$$13$$77, $x$$4$$78, $m$$16$$79) {
  MapTreeModule$$$fold: while (true) {
    const f$$13 = $f$$13$$77,
          x$$4 = $x$$4$$78,
          m$$16 = $m$$16$$79;

    switch (m$$16.tag) {
      case 1:
        {
          return f$$13(x$$4, m$$16.fields[0], m$$16.fields[1]);
        }

      case 2:
        {
          const x$$5 = MapTreeModule$$$fold(f$$13, x$$4, m$$16.fields[2]);
          const x$$6 = f$$13(x$$5, m$$16.fields[0], m$$16.fields[1]);
          $f$$13$$77 = f$$13;
          $x$$4$$78 = x$$6;
          $m$$16$$79 = m$$16.fields[3];
          continue MapTreeModule$$$fold;
        }

      default:
        {
          return x$$4;
        }
    }

    break;
  }
}

function MapTreeModule$$$foldFromTo(comparer$$11, lo, hi, f$$14, m$$17, x$$7) {
  switch (m$$17.tag) {
    case 1:
      {
        const cLoKey = comparer$$11.Compare(lo, m$$17.fields[0]) | 0;
        const cKeyHi = comparer$$11.Compare(m$$17.fields[0], hi) | 0;
        const x$$8 = (cLoKey <= 0 ? cKeyHi <= 0 : false) ? f$$14(m$$17.fields[0], m$$17.fields[1], x$$7) : x$$7;
        return x$$8;
      }

    case 2:
      {
        const cLoKey$$1 = comparer$$11.Compare(lo, m$$17.fields[0]) | 0;
        const cKeyHi$$1 = comparer$$11.Compare(m$$17.fields[0], hi) | 0;
        const x$$9 = cLoKey$$1 < 0 ? MapTreeModule$$$foldFromTo(comparer$$11, lo, hi, f$$14, m$$17.fields[2], x$$7) : x$$7;
        const x$$10 = (cLoKey$$1 <= 0 ? cKeyHi$$1 <= 0 : false) ? f$$14(m$$17.fields[0], m$$17.fields[1], x$$9) : x$$9;
        const x$$11 = cKeyHi$$1 < 0 ? MapTreeModule$$$foldFromTo(comparer$$11, lo, hi, f$$14, m$$17.fields[3], x$$10) : x$$10;
        return x$$11;
      }

    default:
      {
        return x$$7;
      }
  }
}

function MapTreeModule$$$foldSection(comparer$$12, lo$$1, hi$$1, f$$15, m$$18, x$$12) {
  if (comparer$$12.Compare(lo$$1, hi$$1) === 1) {
    return x$$12;
  } else {
    return MapTreeModule$$$foldFromTo(comparer$$12, lo$$1, hi$$1, f$$15, m$$18, x$$12);
  }
}

function MapTreeModule$$$loop($m$$19$$92, $acc$$8$$93) {
  MapTreeModule$$$loop: while (true) {
    const m$$19 = $m$$19$$92,
          acc$$8 = $acc$$8$$93;

    switch (m$$19.tag) {
      case 1:
        {
          return new _Types.List([m$$19.fields[0], m$$19.fields[1]], acc$$8);
        }

      case 2:
        {
          $m$$19$$92 = m$$19.fields[2];
          $acc$$8$$93 = new _Types.List([m$$19.fields[0], m$$19.fields[1]], MapTreeModule$$$loop(m$$19.fields[3], acc$$8));
          continue MapTreeModule$$$loop;
        }

      default:
        {
          return acc$$8;
        }
    }

    break;
  }
}

function MapTreeModule$$$toList(m$$20) {
  return MapTreeModule$$$loop(m$$20, new _Types.List());
}

function MapTreeModule$$$ofList(comparer$$13, l$$20) {
  return (0, _Seq.fold)(function (acc$$9, tupledArg) {
    return MapTreeModule$$$add(comparer$$13, tupledArg[0], tupledArg[1], acc$$9);
  }, MapTreeModule$$$empty(), l$$20);
}

function MapTreeModule$$$mkFromEnumerator($comparer$$14$$97, $acc$$10$$98, $e$$99) {
  MapTreeModule$$$mkFromEnumerator: while (true) {
    const comparer$$14 = $comparer$$14$$97,
          acc$$10 = $acc$$10$$98,
          e = $e$$99;

    if (e.MoveNext()) {
      const patternInput$$2 = e.Current;
      $comparer$$14$$97 = comparer$$14;
      $acc$$10$$98 = MapTreeModule$$$add(comparer$$14, patternInput$$2[0], patternInput$$2[1], acc$$10);
      $e$$99 = e;
      continue MapTreeModule$$$mkFromEnumerator;
    } else {
      return acc$$10;
    }

    break;
  }
}

function MapTreeModule$$$ofArray(comparer$$15, arr) {
  let res$$2 = MapTreeModule$$$empty();

  for (let i = 0; i <= arr.length - 1; i++) {
    const patternInput$$3 = arr[i];
    res$$2 = MapTreeModule$$$add(comparer$$15, patternInput$$3[0], patternInput$$3[1], res$$2);
  }

  return res$$2;
}

function MapTreeModule$$$ofSeq(comparer$$16, c$$9) {
  const ie = (0, _Seq.getEnumerator)(c$$9);

  try {
    return MapTreeModule$$$mkFromEnumerator(comparer$$16, MapTreeModule$$$empty(), ie);
  } finally {
    if ((0, _Util.isDisposable)(ie)) {
      ie.Dispose();
    }
  }
}

function MapTreeModule$$$copyToArray(s$$4, arr$$1, i$$1) {
  let j = i$$1 | 0;
  MapTreeModule$$$iter(function f$$16(x$$15, y$$2) {
    arr$$1[j] = [x$$15, y$$2];
    j = j + 1;
  }, s$$4);
}

const MapTreeModule$002EMapIterator$00602 = (0, _Types.declare)(function Map_MapTreeModule_MapIterator(arg1, arg2) {
  this.stack = arg1;
  this.started = arg2;
}, _Types.Record);
exports.MapTreeModule$002EMapIterator$00602 = MapTreeModule$002EMapIterator$00602;

function MapTreeModule$002EMapIterator$00602$reflection($gen$$109, $gen$$110) {
  return (0, _Reflection.record)("Map.MapTreeModule.MapIterator`2", [$gen$$109, $gen$$110], MapTreeModule$002EMapIterator$00602, () => [["stack", (0, _Reflection.list)(MapTree$00602$reflection($gen$$109, $gen$$110))], ["started", _Reflection.bool]]);
}

function MapTreeModule$$$collapseLHS($stack$$111) {
  MapTreeModule$$$collapseLHS: while (true) {
    const stack = $stack$$111;

    if (stack.tail != null) {
      if (stack.head.tag === 1) {
        return stack;
      } else if (stack.head.tag === 2) {
        $stack$$111 = new _Types.List(stack.head.fields[2], new _Types.List(new MapTree$00602(1, "MapOne", stack.head.fields[0], stack.head.fields[1]), new _Types.List(stack.head.fields[3], stack.tail)));
        continue MapTreeModule$$$collapseLHS;
      } else {
        $stack$$111 = stack.tail;
        continue MapTreeModule$$$collapseLHS;
      }
    } else {
      return new _Types.List();
    }

    break;
  }
}

function MapTreeModule$$$mkIterator(s$$5) {
  return new MapTreeModule$002EMapIterator$00602(MapTreeModule$$$collapseLHS(new _Types.List(s$$5, new _Types.List())), false);
}

function MapTreeModule$$$notStarted() {
  throw new Error("enumeration not started");
}

function MapTreeModule$$$alreadyFinished() {
  throw new Error("enumeration already finished");
}

function MapTreeModule$$$current(i$$2) {
  if (i$$2.started) {
    const matchValue$$4 = i$$2.stack;

    if (matchValue$$4.tail == null) {
      return MapTreeModule$$$alreadyFinished();
    } else if (matchValue$$4.head.tag === 1) {
      return [matchValue$$4.head.fields[0], matchValue$$4.head.fields[1]];
    } else {
      throw new Error("Please report error: Map iterator, unexpected stack for current");
    }
  } else {
    return MapTreeModule$$$notStarted();
  }
}

function MapTreeModule$$$moveNext(i$$3) {
  if (i$$3.started) {
    const matchValue$$5 = i$$3.stack;

    if (matchValue$$5.tail == null) {
      return false;
    } else if (matchValue$$5.head.tag === 1) {
      i$$3.stack = MapTreeModule$$$collapseLHS(matchValue$$5.tail);
      return !(i$$3.stack.tail == null);
    } else {
      throw new Error("Please report error: Map iterator, unexpected stack for moveNext");
    }
  } else {
    i$$3.started = true;
    return !(i$$3.stack.tail == null);
  }
}

const MapTreeModule$002EmkIEnumerator$0027$00602 = (0, _Types.declare)(function Map_MapTreeModule_mkIEnumerator_(s$$6) {
  const $this$$1 = this;
  $this$$1.s = s$$6;
  $this$$1.i = MapTreeModule$$$mkIterator($this$$1.s);
});
exports.MapTreeModule$002EmkIEnumerator$0027$00602 = MapTreeModule$002EmkIEnumerator$0027$00602;

function MapTreeModule$002EmkIEnumerator$0027$00602$reflection($gen$$115, $gen$$116) {
  return (0, _Reflection.type)("Map.MapTreeModule.mkIEnumerator'`2", [$gen$$115, $gen$$116]);
}

function MapTreeModule$002EmkIEnumerator$0027$00602$$$$002Ector$$Z26BC498C(s$$6) {
  return this instanceof MapTreeModule$002EmkIEnumerator$0027$00602 ? MapTreeModule$002EmkIEnumerator$0027$00602.call(this, s$$6) : new MapTreeModule$002EmkIEnumerator$0027$00602(s$$6);
}

Object.defineProperty(MapTreeModule$002EmkIEnumerator$0027$00602.prototype, "Current", {
  "get": function () {
    const __ = this;

    return MapTreeModule$$$current(__.i);
  }
});

MapTreeModule$002EmkIEnumerator$0027$00602.prototype.MoveNext = function () {
  const __$$1 = this;

  return MapTreeModule$$$moveNext(__$$1.i);
};

MapTreeModule$002EmkIEnumerator$0027$00602.prototype.Reset = function () {
  const __$$2 = this;

  __$$2.i = MapTreeModule$$$mkIterator(__$$2.s);
};

MapTreeModule$002EmkIEnumerator$0027$00602.prototype.Dispose = function () {};

function MapTreeModule$$$mkIEnumerator(s$$7) {
  return MapTreeModule$002EmkIEnumerator$0027$00602$$$$002Ector$$Z26BC498C(s$$7);
}

function MapTreeModule$$$toSeq(s$$8) {
  const en = MapTreeModule$$$mkIEnumerator(s$$8);
  return (0, _Seq.unfold)(function generator(en$$1) {
    if (en$$1.MoveNext()) {
      return [en$$1.Current, en$$1];
    } else {
      return null;
    }
  }, en);
}

const FSharpMap = (0, _Types.declare)(function Map_Map(comparer$$17, tree) {
  const $this$$2 = this;
  $this$$2.comparer = comparer$$17;
  $this$$2.tree = tree;
});
exports.FSharpMap = FSharpMap;

function FSharpMap$reflection($gen$$120, $gen$$121) {
  return (0, _Reflection.type)("Map.FSharpMap", [$gen$$120, $gen$$121]);
}

function FSharpMap$$$$002Ector$$58ADD115(comparer$$17, tree) {
  return this instanceof FSharpMap ? FSharpMap.call(this, comparer$$17, tree) : new FSharpMap(comparer$$17, tree);
}

function FSharpMap$$get_Comparer(__$$4) {
  return __$$4.comparer;
}

function FSharpMap$$get_Tree(__$$5) {
  return __$$5.tree;
}

function FSharpMap$$Add$$5BDDA1(__$$6, k$$28, v$$24) {
  return FSharpMap$$$$002Ector$$58ADD115(__$$6.comparer, MapTreeModule$$$add(__$$6.comparer, k$$28, v$$24, __$$6.tree));
}

function FSharpMap$$get_IsEmpty(__$$7) {
  return MapTreeModule$$$isEmpty(__$$7.tree);
}

function FSharpMap$$get_Item$$2B595(__$$8, k$$29) {
  return MapTreeModule$$$find(__$$8.comparer, k$$29, __$$8.tree);
}

function FSharpMap$$TryGetValue$$5BDDA1(__$$9, k$$30, defValue) {
  const matchValue$$6 = MapTreeModule$$$tryFind(__$$9.comparer, k$$30, __$$9.tree);

  if (matchValue$$6 == null) {
    return [false, defValue];
  } else {
    const v$$25 = (0, _Option.value)(matchValue$$6);
    return [true, v$$25];
  }
}

function FSharpMap$$TryPick$$72321DD7(__$$10, f$$17) {
  return MapTreeModule$$$tryPick(f$$17, __$$10.tree);
}

function FSharpMap$$Exists$$Z395DDC35(__$$11, f$$18) {
  return MapTreeModule$$$exists(f$$18, __$$11.tree);
}

function FSharpMap$$Filter$$Z395DDC35(__$$12, f$$19) {
  return FSharpMap$$$$002Ector$$58ADD115(__$$12.comparer, MapTreeModule$$$filter(__$$12.comparer, f$$19, __$$12.tree));
}

function FSharpMap$$ForAll$$Z395DDC35(__$$13, f$$20) {
  return MapTreeModule$$$forall(f$$20, __$$13.tree);
}

function FSharpMap$$Fold(__$$14, f$$21, acc$$11) {
  return MapTreeModule$$$foldBack(f$$21, __$$14.tree, acc$$11);
}

function FSharpMap$$FoldSection(__$$15, lo$$2, hi$$2, f$$22, acc$$12) {
  return MapTreeModule$$$foldSection(__$$15.comparer, lo$$2, hi$$2, f$$22, __$$15.tree, acc$$12);
}

function FSharpMap$$Iterate$$1DCFB91D(__$$16, f$$23) {
  MapTreeModule$$$iter(f$$23, __$$16.tree);
}

function FSharpMap$$MapRange$$6DC7247(__$$17, f$$24) {
  return FSharpMap$$$$002Ector$$58ADD115(__$$17.comparer, MapTreeModule$$$map(f$$24, __$$17.tree));
}

function FSharpMap$$Map$$Z6F6B671C(__$$18, f$$25) {
  return FSharpMap$$$$002Ector$$58ADD115(__$$18.comparer, MapTreeModule$$$mapi(f$$25, __$$18.tree));
}

function FSharpMap$$Partition$$Z395DDC35(__$$19, f$$26) {
  const patternInput$$4 = MapTreeModule$$$partition(__$$19.comparer, f$$26, __$$19.tree);
  return [FSharpMap$$$$002Ector$$58ADD115(__$$19.comparer, patternInput$$4[0]), FSharpMap$$$$002Ector$$58ADD115(__$$19.comparer, patternInput$$4[1])];
}

function FSharpMap$$get_Count(__$$20) {
  return MapTreeModule$$$size(__$$20.tree);
}

function FSharpMap$$ContainsKey$$2B595(__$$21, k$$31) {
  return MapTreeModule$$$mem(__$$21.comparer, k$$31, __$$21.tree);
}

function FSharpMap$$Remove$$2B595(__$$22, k$$32) {
  return FSharpMap$$$$002Ector$$58ADD115(__$$22.comparer, MapTreeModule$$$remove(__$$22.comparer, k$$32, __$$22.tree));
}

function FSharpMap$$TryFind$$2B595(__$$23, k$$33) {
  return MapTreeModule$$$tryFind(__$$23.comparer, k$$33, __$$23.tree);
}

function FSharpMap$$ToList(__$$24) {
  return MapTreeModule$$$toList(__$$24.tree);
}

FSharpMap.prototype.toString = function () {
  var strings;
  const this$ = this;
  return "map [" + (strings = (0, _Seq.map)(function mapping(kv) {
    const clo1 = (0, _String.toText)((0, _String.printf)("(%A, %A)"));
    const clo2 = clo1(kv[0]);
    return clo2(kv[1]);
  }, this$), (0, _String.join)("; ", ...strings)) + "]";
};

FSharpMap.prototype.GetHashCode = function () {
  const this$$$1 = this;

  const combineHash = function combineHash(x$$16, y$$3) {
    return (x$$16 << 1) + y$$3 + 631;
  };

  let res$$3 = 0;
  const e$$1 = MapTreeModule$$$mkIEnumerator(FSharpMap$$get_Tree(this$$$1));

  while (e$$1.MoveNext()) {
    const patternInput$$5 = e$$1.Current;
    const activePatternResult3539 = patternInput$$5;
    res$$3 = combineHash(res$$3, (0, _Util.structuralHash)(activePatternResult3539[0]));
    res$$3 = combineHash(res$$3, (0, _Util.structuralHash)(activePatternResult3539[1]));
  }

  return Math.abs(res$$3) | 0;
};

FSharpMap.prototype.Equals = function (that) {
  const this$$$2 = this;
  return this$$$2.CompareTo(that) === 0;
};

FSharpMap.prototype[Symbol.iterator] = function () {
  const __$$25 = this;

  return (0, _Seq.toIterator)(MapTreeModule$$$mkIEnumerator(__$$25.tree));
};

FSharpMap.prototype.CompareTo = function (obj) {
  const m$$22 = this;
  let res$$4 = 0;
  let finished = false;
  const e1 = MapTreeModule$$$mkIEnumerator(FSharpMap$$get_Tree(m$$22));

  try {
    const e2 = MapTreeModule$$$mkIEnumerator(FSharpMap$$get_Tree(obj));

    try {
      while (!finished ? res$$4 === 0 : false) {
        const matchValue$$7 = [e1.MoveNext(), e2.MoveNext()];

        if (matchValue$$7[0]) {
          if (matchValue$$7[1]) {
            const kvp1 = e1.Current;
            const kvp2 = e2.Current;
            const c$$10 = m$$22.comparer.Compare(kvp1[0], kvp2[0]) | 0;
            res$$4 = c$$10 !== 0 ? c$$10 : (0, _Util.compare)(kvp1[1], kvp2[1]);
          } else {
            res$$4 = 1;
          }
        } else if (matchValue$$7[1]) {
          res$$4 = -1;
        } else {
          finished = true;
        }
      }

      return res$$4 | 0;
    } finally {
      if ((0, _Util.isDisposable)(e2)) {
        e2.Dispose();
      }
    }
  } finally {
    if ((0, _Util.isDisposable)(e1)) {
      e1.Dispose();
    }
  }
};

Object.defineProperty(FSharpMap.prototype, "size", {
  "get": function () {
    const this$$$3 = this;
    return FSharpMap$$get_Count(this$$$3) | 0;
  }
});

FSharpMap.prototype.clear = function () {
  throw new Error("Map cannot be mutated");
};

FSharpMap.prototype.delete = function (_arg1$$1) {
  throw new Error("Map cannot be mutated");
};

FSharpMap.prototype.entries = function () {
  const this$$$4 = this;
  return MapTreeModule$$$toSeq(FSharpMap$$get_Tree(this$$$4));
};

FSharpMap.prototype.get = function (k$$34) {
  const this$$$5 = this;
  return FSharpMap$$get_Item$$2B595(this$$$5, k$$34);
};

FSharpMap.prototype.has = function (k$$35) {
  const this$$$6 = this;
  return FSharpMap$$ContainsKey$$2B595(this$$$6, k$$35);
};

FSharpMap.prototype.keys = function () {
  const this$$$7 = this;
  const source$$1 = MapTreeModule$$$toSeq(FSharpMap$$get_Tree(this$$$7));
  return (0, _Seq.map)(function mapping$$1(kv$$1) {
    return kv$$1[0];
  }, source$$1);
};

FSharpMap.prototype.set = function (k$$36, v$$26) {
  throw new Error("Map cannot be mutated");
};

FSharpMap.prototype.values = function () {
  const this$$$8 = this;
  const source$$2 = MapTreeModule$$$toSeq(FSharpMap$$get_Tree(this$$$8));
  return (0, _Seq.map)(function mapping$$2(kv$$2) {
    return kv$$2[1];
  }, source$$2);
};

function isEmpty(m$$23) {
  return FSharpMap$$get_IsEmpty(m$$23);
}

function add(k$$37, v$$27, m$$24) {
  return FSharpMap$$Add$$5BDDA1(m$$24, k$$37, v$$27);
}

function find(k$$38, m$$25) {
  return FSharpMap$$get_Item$$2B595(m$$25, k$$38);
}

function tryFind(k$$39, m$$26) {
  return FSharpMap$$TryFind$$2B595(m$$26, k$$39);
}

function remove(k$$40, m$$27) {
  return FSharpMap$$Remove$$2B595(m$$27, k$$40);
}

function containsKey(k$$41, m$$28) {
  return FSharpMap$$ContainsKey$$2B595(m$$28, k$$41);
}

function iterate(f$$27, m$$29) {
  FSharpMap$$Iterate$$1DCFB91D(m$$29, f$$27);
}

function tryPick(f$$28, m$$30) {
  return FSharpMap$$TryPick$$72321DD7(m$$30, f$$28);
}

function pick(f$$29, m$$31) {
  const matchValue$$8 = tryPick(f$$29, m$$31);

  if (matchValue$$8 != null) {
    const res$$5 = (0, _Option.value)(matchValue$$8);
    return res$$5;
  } else {
    throw new Error("key not found");
  }
}

function exists(f$$30, m$$32) {
  return FSharpMap$$Exists$$Z395DDC35(m$$32, f$$30);
}

function filter(f$$31, m$$33) {
  return FSharpMap$$Filter$$Z395DDC35(m$$33, f$$31);
}

function partition(f$$32, m$$34) {
  return FSharpMap$$Partition$$Z395DDC35(m$$34, f$$32);
}

function forAll(f$$33, m$$35) {
  return FSharpMap$$ForAll$$Z395DDC35(m$$35, f$$33);
}

function mapRange(f$$34, m$$36) {
  return FSharpMap$$MapRange$$6DC7247(m$$36, f$$34);
}

function map(f$$35, m$$37) {
  return FSharpMap$$Map$$Z6F6B671C(m$$37, f$$35);
}

function fold(f$$36, z, m$$38) {
  return MapTreeModule$$$fold(f$$36, z, FSharpMap$$get_Tree(m$$38));
}

function foldBack(f$$37, m$$39, z$$1) {
  return MapTreeModule$$$foldBack(f$$37, FSharpMap$$get_Tree(m$$39), z$$1);
}

function toSeq(m$$40) {
  return MapTreeModule$$$toSeq(FSharpMap$$get_Tree(m$$40));
}

function findKey(f$$38, m$$41) {
  let _arg1$$2;

  const m$$42 = FSharpMap$$get_Tree(m$$41);
  _arg1$$2 = MapTreeModule$$$tryPick(function f$$39(k$$42, v$$28) {
    if (f$$38(k$$42, v$$28)) {
      return (0, _Option.some)(k$$42);
    } else {
      return null;
    }
  }, m$$42);

  if (_arg1$$2 == null) {
    throw new Error("Key not found");
  } else {
    const k$$43 = (0, _Option.value)(_arg1$$2);
    return k$$43;
  }
}

function tryFindKey(f$$40, m$$43) {
  const m$$44 = FSharpMap$$get_Tree(m$$43);
  return MapTreeModule$$$tryPick(function f$$41(k$$44, v$$29) {
    if (f$$40(k$$44, v$$29)) {
      return (0, _Option.some)(k$$44);
    } else {
      return null;
    }
  }, m$$44);
}

function ofList(l$$22, comparer$$18) {
  return FSharpMap$$$$002Ector$$58ADD115(comparer$$18, MapTreeModule$$$ofList(comparer$$18, l$$22));
}

function ofSeq(l$$23, comparer$$19) {
  return FSharpMap$$$$002Ector$$58ADD115(comparer$$19, MapTreeModule$$$ofSeq(comparer$$19, l$$23));
}

function ofArray(array, comparer$$20) {
  return FSharpMap$$$$002Ector$$58ADD115(comparer$$20, MapTreeModule$$$ofArray(comparer$$20, array));
}

function toList(m$$45) {
  return FSharpMap$$ToList(m$$45);
}

function toArray(m$$46) {
  let res$$6;
  const len = FSharpMap$$get_Count(m$$46) | 0;
  res$$6 = new Array(len);
  MapTreeModule$$$copyToArray(FSharpMap$$get_Tree(m$$46), res$$6, 0);
  return res$$6;
}

function empty(comparer$$21) {
  return FSharpMap$$$$002Ector$$58ADD115(comparer$$21, new MapTree$00602(0, "MapEmpty"));
}

function createMutablePrivate(comparer$$22, tree$0027) {
  let tree$$1 = tree$0027;
  return {
    get size() {
      return MapTreeModule$$$size(tree$$1);
    },

    clear() {
      tree$$1 = new MapTree$00602(0, "MapEmpty");
    },

    delete(x$$18) {
      if (MapTreeModule$$$mem(comparer$$22, x$$18, tree$$1)) {
        tree$$1 = MapTreeModule$$$remove(comparer$$22, x$$18, tree$$1);
        return true;
      } else {
        return false;
      }
    },

    entries() {
      return MapTreeModule$$$toSeq(tree$$1);
    },

    get(k$$45) {
      return MapTreeModule$$$find(comparer$$22, k$$45, tree$$1);
    },

    has(x$$19) {
      return MapTreeModule$$$mem(comparer$$22, x$$19, tree$$1);
    },

    keys() {
      const source$$3 = MapTreeModule$$$toSeq(tree$$1);
      return (0, _Seq.map)(function mapping$$3(kv$$3) {
        return kv$$3[0];
      }, source$$3);
    },

    set(k$$46, v$$30) {
      const this$$$9 = this;
      tree$$1 = MapTreeModule$$$add(comparer$$22, k$$46, v$$30, tree$$1);
      return this$$$9;
    },

    values() {
      const source$$4 = MapTreeModule$$$toSeq(tree$$1);
      return (0, _Seq.map)(function mapping$$4(kv$$4) {
        return kv$$4[1];
      }, source$$4);
    },

    [Symbol.iterator]() {
      return (0, _Seq.toIterator)(MapTreeModule$$$mkIEnumerator(tree$$1));
    },

    GetEnumerator() {
      return MapTreeModule$$$mkIEnumerator(tree$$1);
    }

  };
}

function createMutable(source$$5, comparer$$23) {
  const map$$1 = (0, _MutableMap.MutableMap$00602$$$$002Ector$$Z79760D57)(comparer$$23);
  (0, _Seq.iterate)(function (forLoopVar) {
    (0, _MutableMap.MutableMap$00602$$Add$$5BDDA1)(map$$1, forLoopVar[0], forLoopVar[1]);
  }, source$$5);
  return map$$1;
}

function groupBy(projection, xs, comparer$$24) {
  const dict = createMutable((0, _Seq.empty)(), comparer$$24);
  (0, _Seq.iterate)(function (v$$31) {
    const key$$1 = projection(v$$31);

    if (dict.has(key$$1)) {
      dict.get(key$$1).push(v$$31);
    } else {
      const value$$1 = dict.set(key$$1, [v$$31]);
      value$$1, null;
    }
  }, xs);
  return (0, _Seq.map)(function mapping$$5(kv$$5) {
    return [kv$$5[0], kv$$5[1]];
  }, dict);
}

function countBy(projection$$1, xs$$1, comparer$$25) {
  const dict$$1 = createMutable((0, _Seq.empty)(), comparer$$25);
  (0, _Seq.iterate)(function (value$$2) {
    const key$$2 = projection$$1(value$$2);
    const value$$3 = dict$$1.has(key$$2) ? dict$$1.set(key$$2, dict$$1.get(key$$2) + 1) : dict$$1.set(key$$2, 1);
    value$$3, null;
  }, xs$$1);
  return (0, _Seq.map)(function mapping$$6(kv$$6) {
    return [kv$$6[0], kv$$6[1]];
  }, dict$$1);
}

function count(m$$47) {
  return FSharpMap$$get_Count(m$$47);
}