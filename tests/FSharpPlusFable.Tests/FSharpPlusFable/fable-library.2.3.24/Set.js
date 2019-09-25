"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.SetTree$00601$reflection = SetTree$00601$reflection;
exports.SetTreeModule$$$countAux = SetTreeModule$$$countAux;
exports.SetTreeModule$$$count = SetTreeModule$$$count;
exports.SetTreeModule$$$SetOne = SetTreeModule$$$SetOne;
exports.SetTreeModule$$$SetNode = SetTreeModule$$$SetNode;
exports.SetTreeModule$$$height = SetTreeModule$$$height;
exports.SetTreeModule$$$mk = SetTreeModule$$$mk;
exports.SetTreeModule$$$rebalance = SetTreeModule$$$rebalance;
exports.SetTreeModule$$$add = SetTreeModule$$$add;
exports.SetTreeModule$$$balance = SetTreeModule$$$balance;
exports.SetTreeModule$$$split = SetTreeModule$$$split;
exports.SetTreeModule$$$spliceOutSuccessor = SetTreeModule$$$spliceOutSuccessor;
exports.SetTreeModule$$$remove = SetTreeModule$$$remove;
exports.SetTreeModule$$$mem = SetTreeModule$$$mem;
exports.SetTreeModule$$$iter = SetTreeModule$$$iter;
exports.SetTreeModule$$$foldBack = SetTreeModule$$$foldBack;
exports.SetTreeModule$$$fold = SetTreeModule$$$fold;
exports.SetTreeModule$$$forall = SetTreeModule$$$forall;
exports.SetTreeModule$$$exists = SetTreeModule$$$exists;
exports.SetTreeModule$$$isEmpty = SetTreeModule$$$isEmpty;
exports.SetTreeModule$$$subset = SetTreeModule$$$subset;
exports.SetTreeModule$$$psubset = SetTreeModule$$$psubset;
exports.SetTreeModule$$$filterAux = SetTreeModule$$$filterAux;
exports.SetTreeModule$$$filter = SetTreeModule$$$filter;
exports.SetTreeModule$$$diffAux = SetTreeModule$$$diffAux;
exports.SetTreeModule$$$diff = SetTreeModule$$$diff;
exports.SetTreeModule$$$union = SetTreeModule$$$union;
exports.SetTreeModule$$$intersectionAux = SetTreeModule$$$intersectionAux;
exports.SetTreeModule$$$intersection = SetTreeModule$$$intersection;
exports.SetTreeModule$$$partition1 = SetTreeModule$$$partition1;
exports.SetTreeModule$$$partitionAux = SetTreeModule$$$partitionAux;
exports.SetTreeModule$$$partition = SetTreeModule$$$partition;
exports.SetTreeModule$$$$007CMatchSetNode$007CMatchSetEmpty$007C = SetTreeModule$$$$007CMatchSetNode$007CMatchSetEmpty$007C;
exports.SetTreeModule$$$minimumElementAux = SetTreeModule$$$minimumElementAux;
exports.SetTreeModule$$$minimumElementOpt = SetTreeModule$$$minimumElementOpt;
exports.SetTreeModule$$$maximumElementAux = SetTreeModule$$$maximumElementAux;
exports.SetTreeModule$$$maximumElementOpt = SetTreeModule$$$maximumElementOpt;
exports.SetTreeModule$$$minimumElement = SetTreeModule$$$minimumElement;
exports.SetTreeModule$$$maximumElement = SetTreeModule$$$maximumElement;
exports.SetTreeModule$002ESetIterator$00601$reflection = SetTreeModule$002ESetIterator$00601$reflection;
exports.SetTreeModule$$$collapseLHS = SetTreeModule$$$collapseLHS;
exports.SetTreeModule$$$mkIterator = SetTreeModule$$$mkIterator;
exports.SetTreeModule$$$notStarted = SetTreeModule$$$notStarted;
exports.SetTreeModule$$$alreadyFinished = SetTreeModule$$$alreadyFinished;
exports.SetTreeModule$$$current = SetTreeModule$$$current;
exports.SetTreeModule$$$moveNext = SetTreeModule$$$moveNext;
exports.SetTreeModule$002EmkIEnumerator$00601$reflection = SetTreeModule$002EmkIEnumerator$00601$reflection;
exports.SetTreeModule$002EmkIEnumerator$00601$$$$002Ector$$Z5B395D56 = SetTreeModule$002EmkIEnumerator$00601$$$$002Ector$$Z5B395D56;
exports.SetTreeModule$$$mkIEnumerator = SetTreeModule$$$mkIEnumerator;
exports.SetTreeModule$$$toSeq = SetTreeModule$$$toSeq;
exports.SetTreeModule$$$compareStacks = SetTreeModule$$$compareStacks;
exports.SetTreeModule$$$compare = SetTreeModule$$$compare;
exports.SetTreeModule$$$choose = SetTreeModule$$$choose;
exports.SetTreeModule$$$loop = SetTreeModule$$$loop;
exports.SetTreeModule$$$toList = SetTreeModule$$$toList;
exports.SetTreeModule$$$copyToArray = SetTreeModule$$$copyToArray;
exports.SetTreeModule$$$mkFromEnumerator = SetTreeModule$$$mkFromEnumerator;
exports.SetTreeModule$$$ofSeq = SetTreeModule$$$ofSeq;
exports.SetTreeModule$$$ofArray = SetTreeModule$$$ofArray;
exports.FSharpSet$reflection = FSharpSet$reflection;
exports.FSharpSet$$$$002Ector$$2528C5CB = FSharpSet$$$$002Ector$$2528C5CB;
exports.FSharpSet$$get_Comparer = FSharpSet$$get_Comparer;
exports.FSharpSet$$get_Tree = FSharpSet$$get_Tree;
exports.FSharpSet$$Add$$2B595 = FSharpSet$$Add$$2B595;
exports.FSharpSet$$Remove$$2B595 = FSharpSet$$Remove$$2B595;
exports.FSharpSet$$get_Count = FSharpSet$$get_Count;
exports.FSharpSet$$Contains$$2B595 = FSharpSet$$Contains$$2B595;
exports.FSharpSet$$Iterate$$5028453F = FSharpSet$$Iterate$$5028453F;
exports.FSharpSet$$Fold = FSharpSet$$Fold;
exports.FSharpSet$$get_IsEmpty = FSharpSet$$get_IsEmpty;
exports.FSharpSet$$Partition$$Z1D55A0D7 = FSharpSet$$Partition$$Z1D55A0D7;
exports.FSharpSet$$Filter$$Z1D55A0D7 = FSharpSet$$Filter$$Z1D55A0D7;
exports.FSharpSet$$Map$$38806891 = FSharpSet$$Map$$38806891;
exports.FSharpSet$$Exists$$Z1D55A0D7 = FSharpSet$$Exists$$Z1D55A0D7;
exports.FSharpSet$$ForAll$$Z1D55A0D7 = FSharpSet$$ForAll$$Z1D55A0D7;
exports.FSharpSet$$$op_Subtraction = FSharpSet$$$op_Subtraction;
exports.FSharpSet$$$op_Addition = FSharpSet$$$op_Addition;
exports.FSharpSet$$$Intersection$$Z3BE9BFE0 = FSharpSet$$$Intersection$$Z3BE9BFE0;
exports.FSharpSet$$$IntersectionMany$$Z15B59630 = FSharpSet$$$IntersectionMany$$Z15B59630;
exports.FSharpSet$$$Equality$$Z3BE9BFE0 = FSharpSet$$$Equality$$Z3BE9BFE0;
exports.FSharpSet$$$Compare$$Z3BE9BFE0 = FSharpSet$$$Compare$$Z3BE9BFE0;
exports.FSharpSet$$get_Choose = FSharpSet$$get_Choose;
exports.FSharpSet$$get_MinimumElement = FSharpSet$$get_MinimumElement;
exports.FSharpSet$$get_MaximumElement = FSharpSet$$get_MaximumElement;
exports.FSharpSet$$IsSubsetOf$$6A20B1FF = FSharpSet$$IsSubsetOf$$6A20B1FF;
exports.FSharpSet$$IsSupersetOf$$6A20B1FF = FSharpSet$$IsSupersetOf$$6A20B1FF;
exports.FSharpSet$$IsProperSubsetOf$$6A20B1FF = FSharpSet$$IsProperSubsetOf$$6A20B1FF;
exports.FSharpSet$$IsProperSupersetOf$$6A20B1FF = FSharpSet$$IsProperSupersetOf$$6A20B1FF;
exports.isEmpty = isEmpty;
exports.contains = contains;
exports.add = add;
exports.singleton = singleton;
exports.remove = remove;
exports.union = union;
exports.unionMany = unionMany;
exports.intersect = intersect;
exports.intersectMany = intersectMany;
exports.iterate = iterate;
exports.empty = empty;
exports.forAll = forAll;
exports.exists = exists;
exports.filter = filter;
exports.partition = partition;
exports.fold = fold;
exports.foldBack = foldBack;
exports.map = map;
exports.count = count;
exports.minimumElement = minimumElement;
exports.maximumElement = maximumElement;
exports.ofList = ofList;
exports.ofArray = ofArray;
exports.toList = toList;
exports.toArray = toArray;
exports.toSeq = toSeq;
exports.ofSeq = ofSeq;
exports.difference = difference;
exports.isSubset = isSubset;
exports.isSuperset = isSuperset;
exports.isProperSubset = isProperSubset;
exports.isProperSuperset = isProperSuperset;
exports.minElement = minElement;
exports.maxElement = maxElement;
exports.createMutable = createMutable;
exports.distinct = distinct;
exports.distinctBy = distinctBy;
exports.unionWith = unionWith;
exports.intersectWith = intersectWith;
exports.exceptWith = exceptWith;
exports.isSubsetOf = isSubsetOf;
exports.isSupersetOf = isSupersetOf;
exports.isProperSubsetOf = isProperSubsetOf;
exports.isProperSupersetOf = isProperSupersetOf;
exports.FSharpSet = exports.SetTreeModule$002EmkIEnumerator$00601 = exports.SetTreeModule$002ESetIterator$00601 = exports.SetTreeModule$$$tolerance = exports.SetTree$00601 = void 0;

var _Types = require("./Types");

var _Reflection = require("./Reflection");

var _Option = require("./Option");

var _Seq = require("./Seq");

var _Util = require("./Util");

var _String = require("./String");

var _MutableSet = require("./MutableSet");

const SetTree$00601 = (0, _Types.declare)(function Set_SetTree(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.SetTree$00601 = SetTree$00601;

function SetTree$00601$reflection($gen$$3) {
  return (0, _Reflection.union)("Set.SetTree`1", [$gen$$3], SetTree$00601, () => ["SetEmpty", ["SetNode", [$gen$$3, SetTree$00601$reflection($gen$$3), SetTree$00601$reflection($gen$$3), _Reflection.int32]], ["SetOne", [$gen$$3]]]);
}

function SetTreeModule$$$countAux($s$$4, $acc$$5) {
  SetTreeModule$$$countAux: while (true) {
    const s = $s$$4,
          acc = $acc$$5;

    switch (s.tag) {
      case 2:
        {
          return acc + 1 | 0;
        }

      case 0:
        {
          return acc | 0;
        }

      default:
        {
          $s$$4 = s.fields[1];
          $acc$$5 = SetTreeModule$$$countAux(s.fields[2], acc + 1);
          continue SetTreeModule$$$countAux;
        }
    }

    break;
  }
}

function SetTreeModule$$$count(s$$1) {
  return SetTreeModule$$$countAux(s$$1, 0);
}

function SetTreeModule$$$SetOne(n) {
  return new SetTree$00601(2, "SetOne", n);
}

function SetTreeModule$$$SetNode(x, l$$1, r$$1, h) {
  return new SetTree$00601(1, "SetNode", x, l$$1, r$$1, h);
}

function SetTreeModule$$$height(t) {
  switch (t.tag) {
    case 2:
      {
        return 1;
      }

    case 1:
      {
        return t.fields[3] | 0;
      }

    default:
      {
        return 0;
      }
  }
}

const SetTreeModule$$$tolerance = 2;
exports.SetTreeModule$$$tolerance = SetTreeModule$$$tolerance;

function SetTreeModule$$$mk(l$$2, k, r$$2) {
  var $target$$16;

  if (l$$2.tag === 0) {
    if (r$$2.tag === 0) {
      $target$$16 = 0;
    } else {
      $target$$16 = 1;
    }
  } else {
    $target$$16 = 1;
  }

  switch ($target$$16) {
    case 0:
      {
        return SetTreeModule$$$SetOne(k);
      }

    case 1:
      {
        const hl = SetTreeModule$$$height(l$$2) | 0;
        const hr = SetTreeModule$$$height(r$$2) | 0;
        const m = (hl < hr ? hr : hl) | 0;
        return SetTreeModule$$$SetNode(k, l$$2, r$$2, m + 1);
      }
  }
}

function SetTreeModule$$$rebalance(t1, k$$1, t2) {
  const t1h = SetTreeModule$$$height(t1) | 0;
  const t2h = SetTreeModule$$$height(t2) | 0;

  if (t2h > t1h + SetTreeModule$$$tolerance) {
    if (t2.tag === 1) {
      if (SetTreeModule$$$height(t2.fields[1]) > t1h + 1) {
        if (t2.fields[1].tag === 1) {
          return SetTreeModule$$$mk(SetTreeModule$$$mk(t1, k$$1, t2.fields[1].fields[1]), t2.fields[1].fields[0], SetTreeModule$$$mk(t2.fields[1].fields[2], t2.fields[0], t2.fields[2]));
        } else {
          throw new Error("rebalance");
        }
      } else {
        return SetTreeModule$$$mk(SetTreeModule$$$mk(t1, k$$1, t2.fields[1]), t2.fields[0], t2.fields[2]);
      }
    } else {
      throw new Error("rebalance");
    }
  } else if (t1h > t2h + SetTreeModule$$$tolerance) {
    if (t1.tag === 1) {
      if (SetTreeModule$$$height(t1.fields[2]) > t2h + 1) {
        if (t1.fields[2].tag === 1) {
          return SetTreeModule$$$mk(SetTreeModule$$$mk(t1.fields[1], t1.fields[0], t1.fields[2].fields[1]), t1.fields[2].fields[0], SetTreeModule$$$mk(t1.fields[2].fields[2], k$$1, t2));
        } else {
          throw new Error("rebalance");
        }
      } else {
        return SetTreeModule$$$mk(t1.fields[1], t1.fields[0], SetTreeModule$$$mk(t1.fields[2], k$$1, t2));
      }
    } else {
      throw new Error("rebalance");
    }
  } else {
    return SetTreeModule$$$mk(t1, k$$1, t2);
  }
}

function SetTreeModule$$$add(comparer, k$$2, t$$1) {
  switch (t$$1.tag) {
    case 2:
      {
        const c$$1 = comparer.Compare(k$$2, t$$1.fields[0]) | 0;

        if (c$$1 < 0) {
          return SetTreeModule$$$SetNode(k$$2, new SetTree$00601(0, "SetEmpty"), t$$1, 2);
        } else if (c$$1 === 0) {
          return t$$1;
        } else {
          return SetTreeModule$$$SetNode(k$$2, t$$1, new SetTree$00601(0, "SetEmpty"), 2);
        }
      }

    case 0:
      {
        return SetTreeModule$$$SetOne(k$$2);
      }

    default:
      {
        const c = comparer.Compare(k$$2, t$$1.fields[0]) | 0;

        if (c < 0) {
          return SetTreeModule$$$rebalance(SetTreeModule$$$add(comparer, k$$2, t$$1.fields[1]), t$$1.fields[0], t$$1.fields[2]);
        } else if (c === 0) {
          return t$$1;
        } else {
          return SetTreeModule$$$rebalance(t$$1.fields[1], t$$1.fields[0], SetTreeModule$$$add(comparer, k$$2, t$$1.fields[2]));
        }
      }
  }
}

function SetTreeModule$$$balance(comparer$$1, t1$$1, k$$3, t2$$1) {
  var $target$$27, t2$$2, t1$$2, k1, t2$$3, k2$$2, t1$$3, h1, h2, k1$$1, k2$$3, t11, t12, t21, t22;

  if (t1$$1.tag === 2) {
    if (t2$$1.tag === 0) {
      $target$$27 = 1;
      t1$$2 = t1$$1;
    } else if (t2$$1.tag === 2) {
      $target$$27 = 2;
      k1 = t1$$1.fields[0];
      t2$$3 = t2$$1;
    } else {
      $target$$27 = 2;
      k1 = t1$$1.fields[0];
      t2$$3 = t2$$1;
    }
  } else if (t1$$1.tag === 1) {
    if (t2$$1.tag === 2) {
      $target$$27 = 3;
      k2$$2 = t2$$1.fields[0];
      t1$$3 = t1$$1;
    } else if (t2$$1.tag === 1) {
      $target$$27 = 4;
      h1 = t1$$1.fields[3];
      h2 = t2$$1.fields[3];
      k1$$1 = t1$$1.fields[0];
      k2$$3 = t2$$1.fields[0];
      t11 = t1$$1.fields[1];
      t12 = t1$$1.fields[2];
      t21 = t2$$1.fields[1];
      t22 = t2$$1.fields[2];
    } else {
      $target$$27 = 1;
      t1$$2 = t1$$1;
    }
  } else {
    $target$$27 = 0;
    t2$$2 = t2$$1;
  }

  switch ($target$$27) {
    case 0:
      {
        return SetTreeModule$$$add(comparer$$1, k$$3, t2$$2);
      }

    case 1:
      {
        return SetTreeModule$$$add(comparer$$1, k$$3, t1$$2);
      }

    case 2:
      {
        return SetTreeModule$$$add(comparer$$1, k$$3, SetTreeModule$$$add(comparer$$1, k1, t2$$3));
      }

    case 3:
      {
        return SetTreeModule$$$add(comparer$$1, k$$3, SetTreeModule$$$add(comparer$$1, k2$$2, t1$$3));
      }

    case 4:
      {
        if (h1 + SetTreeModule$$$tolerance < h2) {
          return SetTreeModule$$$rebalance(SetTreeModule$$$balance(comparer$$1, t1$$1, k$$3, t21), k2$$3, t22);
        } else if (h2 + SetTreeModule$$$tolerance < h1) {
          return SetTreeModule$$$rebalance(t11, k1$$1, SetTreeModule$$$balance(comparer$$1, t12, k$$3, t2$$1));
        } else {
          return SetTreeModule$$$mk(t1$$1, k$$3, t2$$1);
        }
      }
  }
}

function SetTreeModule$$$split(comparer$$2, pivot, t$$2) {
  switch (t$$2.tag) {
    case 2:
      {
        const c$$3 = comparer$$2.Compare(t$$2.fields[0], pivot) | 0;

        if (c$$3 < 0) {
          return [t$$2, false, new SetTree$00601(0, "SetEmpty")];
        } else if (c$$3 === 0) {
          return [new SetTree$00601(0, "SetEmpty"), true, new SetTree$00601(0, "SetEmpty")];
        } else {
          return [new SetTree$00601(0, "SetEmpty"), false, t$$2];
        }
      }

    case 0:
      {
        return [new SetTree$00601(0, "SetEmpty"), false, new SetTree$00601(0, "SetEmpty")];
      }

    default:
      {
        const c$$2 = comparer$$2.Compare(pivot, t$$2.fields[0]) | 0;

        if (c$$2 < 0) {
          const patternInput = SetTreeModule$$$split(comparer$$2, pivot, t$$2.fields[1]);
          return [patternInput[0], patternInput[1], SetTreeModule$$$balance(comparer$$2, patternInput[2], t$$2.fields[0], t$$2.fields[2])];
        } else if (c$$2 === 0) {
          return [t$$2.fields[1], true, t$$2.fields[2]];
        } else {
          const patternInput$$1 = SetTreeModule$$$split(comparer$$2, pivot, t$$2.fields[2]);
          return [SetTreeModule$$$balance(comparer$$2, t$$2.fields[1], t$$2.fields[0], patternInput$$1[0]), patternInput$$1[1], patternInput$$1[2]];
        }
      }
  }
}

function SetTreeModule$$$spliceOutSuccessor(t$$3) {
  switch (t$$3.tag) {
    case 2:
      {
        return [t$$3.fields[0], new SetTree$00601(0, "SetEmpty")];
      }

    case 1:
      {
        if (t$$3.fields[1].tag === 0) {
          return [t$$3.fields[0], t$$3.fields[2]];
        } else {
          const patternInput$$2 = SetTreeModule$$$spliceOutSuccessor(t$$3.fields[1]);
          return [patternInput$$2[0], SetTreeModule$$$mk(patternInput$$2[1], t$$3.fields[0], t$$3.fields[2])];
        }
      }

    default:
      {
        throw new Error("internal error: Set.spliceOutSuccessor");
      }
  }
}

function SetTreeModule$$$remove(comparer$$3, k$$4, t$$4) {
  switch (t$$4.tag) {
    case 2:
      {
        const c$$4 = comparer$$3.Compare(k$$4, t$$4.fields[0]) | 0;

        if (c$$4 === 0) {
          return new SetTree$00601(0, "SetEmpty");
        } else {
          return t$$4;
        }
      }

    case 1:
      {
        const c$$5 = comparer$$3.Compare(k$$4, t$$4.fields[0]) | 0;

        if (c$$5 < 0) {
          return SetTreeModule$$$rebalance(SetTreeModule$$$remove(comparer$$3, k$$4, t$$4.fields[1]), t$$4.fields[0], t$$4.fields[2]);
        } else if (c$$5 === 0) {
          if (t$$4.fields[1].tag === 0) {
            return t$$4.fields[2];
          } else if (t$$4.fields[2].tag === 0) {
            return t$$4.fields[1];
          } else {
            const patternInput$$3 = SetTreeModule$$$spliceOutSuccessor(t$$4.fields[2]);
            return SetTreeModule$$$mk(t$$4.fields[1], patternInput$$3[0], patternInput$$3[1]);
          }
        } else {
          return SetTreeModule$$$rebalance(t$$4.fields[1], t$$4.fields[0], SetTreeModule$$$remove(comparer$$3, k$$4, t$$4.fields[2]));
        }
      }

    default:
      {
        return t$$4;
      }
  }
}

function SetTreeModule$$$mem($comparer$$4$$35, $k$$5$$36, $t$$5$$37) {
  SetTreeModule$$$mem: while (true) {
    const comparer$$4 = $comparer$$4$$35,
          k$$5 = $k$$5$$36,
          t$$5 = $t$$5$$37;

    switch (t$$5.tag) {
      case 2:
        {
          return comparer$$4.Compare(k$$5, t$$5.fields[0]) === 0;
        }

      case 0:
        {
          return false;
        }

      default:
        {
          const c$$6 = comparer$$4.Compare(k$$5, t$$5.fields[0]) | 0;

          if (c$$6 < 0) {
            $comparer$$4$$35 = comparer$$4;
            $k$$5$$36 = k$$5;
            $t$$5$$37 = t$$5.fields[1];
            continue SetTreeModule$$$mem;
          } else if (c$$6 === 0) {
            return true;
          } else {
            $comparer$$4$$35 = comparer$$4;
            $k$$5$$36 = k$$5;
            $t$$5$$37 = t$$5.fields[2];
            continue SetTreeModule$$$mem;
          }
        }
    }

    break;
  }
}

function SetTreeModule$$$iter($f$$38, $t$$6$$39) {
  SetTreeModule$$$iter: while (true) {
    const f = $f$$38,
          t$$6 = $t$$6$$39;

    switch (t$$6.tag) {
      case 2:
        {
          f(t$$6.fields[0]);
          break;
        }

      case 0:
        {
          break;
        }

      default:
        {
          SetTreeModule$$$iter(f, t$$6.fields[1]);
          f(t$$6.fields[0]);
          $f$$38 = f;
          $t$$6$$39 = t$$6.fields[2];
          continue SetTreeModule$$$iter;
        }
    }

    break;
  }
}

function SetTreeModule$$$foldBack($f$$1$$40, $m$$1$$41, $x$$1$$42) {
  SetTreeModule$$$foldBack: while (true) {
    const f$$1 = $f$$1$$40,
          m$$1 = $m$$1$$41,
          x$$1 = $x$$1$$42;

    switch (m$$1.tag) {
      case 2:
        {
          return f$$1(m$$1.fields[0], x$$1);
        }

      case 0:
        {
          return x$$1;
        }

      default:
        {
          $f$$1$$40 = f$$1;
          $m$$1$$41 = m$$1.fields[1];
          $x$$1$$42 = f$$1(m$$1.fields[0], SetTreeModule$$$foldBack(f$$1, m$$1.fields[2], x$$1));
          continue SetTreeModule$$$foldBack;
        }
    }

    break;
  }
}

function SetTreeModule$$$fold($f$$2$$43, $x$$2$$44, $m$$2$$45) {
  SetTreeModule$$$fold: while (true) {
    const f$$2 = $f$$2$$43,
          x$$2 = $x$$2$$44,
          m$$2 = $m$$2$$45;

    switch (m$$2.tag) {
      case 2:
        {
          return f$$2(x$$2, m$$2.fields[0]);
        }

      case 0:
        {
          return x$$2;
        }

      default:
        {
          const x$$3 = SetTreeModule$$$fold(f$$2, x$$2, m$$2.fields[1]);
          const x$$4 = f$$2(x$$3, m$$2.fields[0]);
          $f$$2$$43 = f$$2;
          $x$$2$$44 = x$$4;
          $m$$2$$45 = m$$2.fields[2];
          continue SetTreeModule$$$fold;
        }
    }

    break;
  }
}

function SetTreeModule$$$forall($f$$3$$46, $m$$3$$47) {
  SetTreeModule$$$forall: while (true) {
    const f$$3 = $f$$3$$46,
          m$$3 = $m$$3$$47;

    switch (m$$3.tag) {
      case 2:
        {
          return f$$3(m$$3.fields[0]);
        }

      case 0:
        {
          return true;
        }

      default:
        {
          if (f$$3(m$$3.fields[0]) ? SetTreeModule$$$forall(f$$3, m$$3.fields[1]) : false) {
            $f$$3$$46 = f$$3;
            $m$$3$$47 = m$$3.fields[2];
            continue SetTreeModule$$$forall;
          } else {
            return false;
          }
        }
    }

    break;
  }
}

function SetTreeModule$$$exists($f$$4$$48, $m$$4$$49) {
  SetTreeModule$$$exists: while (true) {
    const f$$4 = $f$$4$$48,
          m$$4 = $m$$4$$49;

    switch (m$$4.tag) {
      case 2:
        {
          return f$$4(m$$4.fields[0]);
        }

      case 0:
        {
          return false;
        }

      default:
        {
          if (f$$4(m$$4.fields[0]) ? true : SetTreeModule$$$exists(f$$4, m$$4.fields[1])) {
            return true;
          } else {
            $f$$4$$48 = f$$4;
            $m$$4$$49 = m$$4.fields[2];
            continue SetTreeModule$$$exists;
          }
        }
    }

    break;
  }
}

function SetTreeModule$$$isEmpty(m$$5) {
  if (m$$5.tag === 0) {
    return true;
  } else {
    return false;
  }
}

function SetTreeModule$$$subset(comparer$$5, a, b) {
  return SetTreeModule$$$forall(function (x$$5) {
    return SetTreeModule$$$mem(comparer$$5, x$$5, b);
  }, a);
}

function SetTreeModule$$$psubset(comparer$$6, a$$1, b$$1) {
  if (SetTreeModule$$$forall(function (x$$6) {
    return SetTreeModule$$$mem(comparer$$6, x$$6, b$$1);
  }, a$$1)) {
    return SetTreeModule$$$exists(function (x$$7) {
      return !SetTreeModule$$$mem(comparer$$6, x$$7, a$$1);
    }, b$$1);
  } else {
    return false;
  }
}

function SetTreeModule$$$filterAux($comparer$$7$$57, $f$$5$$58, $s$$2$$59, $acc$$1$$60) {
  SetTreeModule$$$filterAux: while (true) {
    const comparer$$7 = $comparer$$7$$57,
          f$$5 = $f$$5$$58,
          s$$2 = $s$$2$$59,
          acc$$1 = $acc$$1$$60;

    switch (s$$2.tag) {
      case 2:
        {
          if (f$$5(s$$2.fields[0])) {
            return SetTreeModule$$$add(comparer$$7, s$$2.fields[0], acc$$1);
          } else {
            return acc$$1;
          }
        }

      case 0:
        {
          return acc$$1;
        }

      default:
        {
          const acc$$2 = f$$5(s$$2.fields[0]) ? SetTreeModule$$$add(comparer$$7, s$$2.fields[0], acc$$1) : acc$$1;
          $comparer$$7$$57 = comparer$$7;
          $f$$5$$58 = f$$5;
          $s$$2$$59 = s$$2.fields[1];
          $acc$$1$$60 = SetTreeModule$$$filterAux(comparer$$7, f$$5, s$$2.fields[2], acc$$2);
          continue SetTreeModule$$$filterAux;
        }
    }

    break;
  }
}

function SetTreeModule$$$filter(comparer$$8, f$$6, s$$3) {
  return SetTreeModule$$$filterAux(comparer$$8, f$$6, s$$3, new SetTree$00601(0, "SetEmpty"));
}

function SetTreeModule$$$diffAux($comparer$$9$$64, $m$$6$$65, $acc$$3$$66) {
  SetTreeModule$$$diffAux: while (true) {
    const comparer$$9 = $comparer$$9$$64,
          m$$6 = $m$$6$$65,
          acc$$3 = $acc$$3$$66;

    switch (m$$6.tag) {
      case 2:
        {
          return SetTreeModule$$$remove(comparer$$9, m$$6.fields[0], acc$$3);
        }

      case 0:
        {
          return acc$$3;
        }

      default:
        {
          $comparer$$9$$64 = comparer$$9;
          $m$$6$$65 = m$$6.fields[1];
          $acc$$3$$66 = SetTreeModule$$$diffAux(comparer$$9, m$$6.fields[2], SetTreeModule$$$remove(comparer$$9, m$$6.fields[0], acc$$3));
          continue SetTreeModule$$$diffAux;
        }
    }

    break;
  }
}

function SetTreeModule$$$diff(comparer$$10, a$$2, b$$2) {
  return SetTreeModule$$$diffAux(comparer$$10, b$$2, a$$2);
}

function SetTreeModule$$$union(comparer$$11, t1$$4, t2$$4) {
  var $target$$73, h1$$1, h2$$1, k1$$4, k2$$16, t11$$2, t12$$2, t21$$1, t22$$1, t$$7, t$$8, k1$$5, t2$$5, k2$$17, t1$$5;

  if (t1$$4.tag === 0) {
    $target$$73 = 1;
    t$$7 = t2$$4;
  } else if (t1$$4.tag === 2) {
    if (t2$$4.tag === 0) {
      $target$$73 = 2;
      t$$8 = t1$$4;
    } else if (t2$$4.tag === 2) {
      $target$$73 = 3;
      k1$$5 = t1$$4.fields[0];
      t2$$5 = t2$$4;
    } else {
      $target$$73 = 3;
      k1$$5 = t1$$4.fields[0];
      t2$$5 = t2$$4;
    }
  } else if (t2$$4.tag === 0) {
    $target$$73 = 2;
    t$$8 = t1$$4;
  } else if (t2$$4.tag === 2) {
    $target$$73 = 4;
    k2$$17 = t2$$4.fields[0];
    t1$$5 = t1$$4;
  } else {
    $target$$73 = 0;
    h1$$1 = t1$$4.fields[3];
    h2$$1 = t2$$4.fields[3];
    k1$$4 = t1$$4.fields[0];
    k2$$16 = t2$$4.fields[0];
    t11$$2 = t1$$4.fields[1];
    t12$$2 = t1$$4.fields[2];
    t21$$1 = t2$$4.fields[1];
    t22$$1 = t2$$4.fields[2];
  }

  switch ($target$$73) {
    case 0:
      {
        if (h1$$1 > h2$$1) {
          const patternInput$$4 = SetTreeModule$$$split(comparer$$11, k1$$4, t2$$4);
          return SetTreeModule$$$balance(comparer$$11, SetTreeModule$$$union(comparer$$11, t11$$2, patternInput$$4[0]), k1$$4, SetTreeModule$$$union(comparer$$11, t12$$2, patternInput$$4[2]));
        } else {
          const patternInput$$5 = SetTreeModule$$$split(comparer$$11, k2$$16, t1$$4);
          return SetTreeModule$$$balance(comparer$$11, SetTreeModule$$$union(comparer$$11, t21$$1, patternInput$$5[0]), k2$$16, SetTreeModule$$$union(comparer$$11, t22$$1, patternInput$$5[2]));
        }
      }

    case 1:
      {
        return t$$7;
      }

    case 2:
      {
        return t$$8;
      }

    case 3:
      {
        return SetTreeModule$$$add(comparer$$11, k1$$5, t2$$5);
      }

    case 4:
      {
        return SetTreeModule$$$add(comparer$$11, k2$$17, t1$$5);
      }
  }
}

function SetTreeModule$$$intersectionAux($comparer$$12$$74, $b$$3$$75, $m$$7$$76, $acc$$4$$77) {
  SetTreeModule$$$intersectionAux: while (true) {
    const comparer$$12 = $comparer$$12$$74,
          b$$3 = $b$$3$$75,
          m$$7 = $m$$7$$76,
          acc$$4 = $acc$$4$$77;

    switch (m$$7.tag) {
      case 2:
        {
          if (SetTreeModule$$$mem(comparer$$12, m$$7.fields[0], b$$3)) {
            return SetTreeModule$$$add(comparer$$12, m$$7.fields[0], acc$$4);
          } else {
            return acc$$4;
          }
        }

      case 0:
        {
          return acc$$4;
        }

      default:
        {
          const acc$$5 = SetTreeModule$$$intersectionAux(comparer$$12, b$$3, m$$7.fields[2], acc$$4);
          const acc$$6 = SetTreeModule$$$mem(comparer$$12, m$$7.fields[0], b$$3) ? SetTreeModule$$$add(comparer$$12, m$$7.fields[0], acc$$5) : acc$$5;
          $comparer$$12$$74 = comparer$$12;
          $b$$3$$75 = b$$3;
          $m$$7$$76 = m$$7.fields[1];
          $acc$$4$$77 = acc$$6;
          continue SetTreeModule$$$intersectionAux;
        }
    }

    break;
  }
}

function SetTreeModule$$$intersection(comparer$$13, a$$3, b$$4) {
  return SetTreeModule$$$intersectionAux(comparer$$13, b$$4, a$$3, new SetTree$00601(0, "SetEmpty"));
}

function SetTreeModule$$$partition1(comparer$$14, f$$7, k$$16, acc1, acc2) {
  if (f$$7(k$$16)) {
    return [SetTreeModule$$$add(comparer$$14, k$$16, acc1), acc2];
  } else {
    return [acc1, SetTreeModule$$$add(comparer$$14, k$$16, acc2)];
  }
}

function SetTreeModule$$$partitionAux($comparer$$15$$86, $f$$8$$87, $s$$4$$88, $acc_0$$89, $acc_1$$90) {
  SetTreeModule$$$partitionAux: while (true) {
    const comparer$$15 = $comparer$$15$$86,
          f$$8 = $f$$8$$87,
          s$$4 = $s$$4$$88,
          acc_0 = $acc_0$$89,
          acc_1 = $acc_1$$90;

    switch (s$$4.tag) {
      case 2:
        {
          return SetTreeModule$$$partition1(comparer$$15, f$$8, s$$4.fields[0], acc_0, acc_1);
        }

      case 0:
        {
          return [acc_0, acc_1];
        }

      default:
        {
          const acc$$8 = SetTreeModule$$$partitionAux(comparer$$15, f$$8, s$$4.fields[2], acc_0, acc_1);
          const acc$$9 = SetTreeModule$$$partition1(comparer$$15, f$$8, s$$4.fields[0], acc$$8[0], acc$$8[1]);
          $comparer$$15$$86 = comparer$$15;
          $f$$8$$87 = f$$8;
          $s$$4$$88 = s$$4.fields[1];
          $acc_0$$89 = acc$$9[0];
          $acc_1$$90 = acc$$9[1];
          continue SetTreeModule$$$partitionAux;
        }
    }

    break;
  }
}

function SetTreeModule$$$partition(comparer$$16, f$$9, s$$5) {
  const seed = [new SetTree$00601(0, "SetEmpty"), new SetTree$00601(0, "SetEmpty")];
  return SetTreeModule$$$partitionAux(comparer$$16, f$$9, s$$5, seed[0], seed[1]);
}

function SetTreeModule$$$$007CMatchSetNode$007CMatchSetEmpty$007C(s$$6) {
  switch (s$$6.tag) {
    case 2:
      {
        return new _Option.Choice(0, "Choice1Of2", [s$$6.fields[0], new SetTree$00601(0, "SetEmpty"), new SetTree$00601(0, "SetEmpty")]);
      }

    case 0:
      {
        return new _Option.Choice(1, "Choice2Of2", null);
      }

    default:
      {
        return new _Option.Choice(0, "Choice1Of2", [s$$6.fields[0], s$$6.fields[1], s$$6.fields[2]]);
      }
  }
}

function SetTreeModule$$$minimumElementAux($s$$7$$95, $n$$1$$96) {
  SetTreeModule$$$minimumElementAux: while (true) {
    const s$$7 = $s$$7$$95,
          n$$1 = $n$$1$$96;

    switch (s$$7.tag) {
      case 2:
        {
          return s$$7.fields[0];
        }

      case 0:
        {
          return n$$1;
        }

      default:
        {
          $s$$7$$95 = s$$7.fields[1];
          $n$$1$$96 = s$$7.fields[0];
          continue SetTreeModule$$$minimumElementAux;
        }
    }

    break;
  }
}

function SetTreeModule$$$minimumElementOpt(s$$8) {
  switch (s$$8.tag) {
    case 2:
      {
        return (0, _Option.some)(s$$8.fields[0]);
      }

    case 0:
      {
        return null;
      }

    default:
      {
        return (0, _Option.some)(SetTreeModule$$$minimumElementAux(s$$8.fields[1], s$$8.fields[0]));
      }
  }
}

function SetTreeModule$$$maximumElementAux($s$$9$$98, $n$$2$$99) {
  SetTreeModule$$$maximumElementAux: while (true) {
    const s$$9 = $s$$9$$98,
          n$$2 = $n$$2$$99;

    switch (s$$9.tag) {
      case 2:
        {
          return s$$9.fields[0];
        }

      case 0:
        {
          return n$$2;
        }

      default:
        {
          $s$$9$$98 = s$$9.fields[2];
          $n$$2$$99 = s$$9.fields[0];
          continue SetTreeModule$$$maximumElementAux;
        }
    }

    break;
  }
}

function SetTreeModule$$$maximumElementOpt(s$$10) {
  switch (s$$10.tag) {
    case 2:
      {
        return (0, _Option.some)(s$$10.fields[0]);
      }

    case 0:
      {
        return null;
      }

    default:
      {
        return (0, _Option.some)(SetTreeModule$$$maximumElementAux(s$$10.fields[2], s$$10.fields[0]));
      }
  }
}

function SetTreeModule$$$minimumElement(s$$11) {
  const matchValue$$4 = SetTreeModule$$$minimumElementOpt(s$$11);

  if (matchValue$$4 == null) {
    throw new Error("Set contains no elements");
  } else {
    const k$$27 = (0, _Option.value)(matchValue$$4);
    return k$$27;
  }
}

function SetTreeModule$$$maximumElement(s$$12) {
  const matchValue$$5 = SetTreeModule$$$maximumElementOpt(s$$12);

  if (matchValue$$5 == null) {
    throw new Error("Set contains no elements");
  } else {
    const k$$28 = (0, _Option.value)(matchValue$$5);
    return k$$28;
  }
}

const SetTreeModule$002ESetIterator$00601 = (0, _Types.declare)(function Set_SetTreeModule_SetIterator(arg1, arg2) {
  this.stack = arg1;
  this.started = arg2;
}, _Types.Record);
exports.SetTreeModule$002ESetIterator$00601 = SetTreeModule$002ESetIterator$00601;

function SetTreeModule$002ESetIterator$00601$reflection($gen$$103) {
  return (0, _Reflection.record)("Set.SetTreeModule.SetIterator`1", [$gen$$103], SetTreeModule$002ESetIterator$00601, () => [["stack", (0, _Reflection.list)(SetTree$00601$reflection($gen$$103))], ["started", _Reflection.bool]]);
}

function SetTreeModule$$$collapseLHS($stack$$104) {
  SetTreeModule$$$collapseLHS: while (true) {
    const stack = $stack$$104;

    if (stack.tail != null) {
      if (stack.head.tag === 2) {
        return stack;
      } else if (stack.head.tag === 1) {
        $stack$$104 = new _Types.List(stack.head.fields[1], new _Types.List(SetTreeModule$$$SetOne(stack.head.fields[0]), new _Types.List(stack.head.fields[2], stack.tail)));
        continue SetTreeModule$$$collapseLHS;
      } else {
        $stack$$104 = stack.tail;
        continue SetTreeModule$$$collapseLHS;
      }
    } else {
      return new _Types.List();
    }

    break;
  }
}

function SetTreeModule$$$mkIterator(s$$13) {
  return new SetTreeModule$002ESetIterator$00601(SetTreeModule$$$collapseLHS(new _Types.List(s$$13, new _Types.List())), false);
}

function SetTreeModule$$$notStarted() {
  throw new Error("Enumeration not started");
}

function SetTreeModule$$$alreadyFinished() {
  throw new Error("Enumeration already started");
}

function SetTreeModule$$$current(i) {
  if (i.started) {
    const matchValue$$6 = i.stack;

    if (matchValue$$6.tail == null) {
      return SetTreeModule$$$alreadyFinished();
    } else if (matchValue$$6.head.tag === 2) {
      return matchValue$$6.head.fields[0];
    } else {
      throw new Error("Please report error: Set iterator, unexpected stack for current");
    }
  } else {
    return SetTreeModule$$$notStarted();
  }
}

function SetTreeModule$$$moveNext(i$$1) {
  if (i$$1.started) {
    const matchValue$$7 = i$$1.stack;

    if (matchValue$$7.tail == null) {
      return false;
    } else if (matchValue$$7.head.tag === 2) {
      i$$1.stack = SetTreeModule$$$collapseLHS(matchValue$$7.tail);
      return !(i$$1.stack.tail == null);
    } else {
      throw new Error("Please report error: Set iterator, unexpected stack for moveNext");
    }
  } else {
    i$$1.started = true;
    return !(i$$1.stack.tail == null);
  }
}

const SetTreeModule$002EmkIEnumerator$00601 = (0, _Types.declare)(function Set_SetTreeModule_mkIEnumerator(s$$14) {
  const $this$$1 = this;
  $this$$1.s = s$$14;
  $this$$1.i = SetTreeModule$$$mkIterator($this$$1.s);
});
exports.SetTreeModule$002EmkIEnumerator$00601 = SetTreeModule$002EmkIEnumerator$00601;

function SetTreeModule$002EmkIEnumerator$00601$reflection($gen$$108) {
  return (0, _Reflection.type)("Set.SetTreeModule.mkIEnumerator`1", [$gen$$108]);
}

function SetTreeModule$002EmkIEnumerator$00601$$$$002Ector$$Z5B395D56(s$$14) {
  return this instanceof SetTreeModule$002EmkIEnumerator$00601 ? SetTreeModule$002EmkIEnumerator$00601.call(this, s$$14) : new SetTreeModule$002EmkIEnumerator$00601(s$$14);
}

Object.defineProperty(SetTreeModule$002EmkIEnumerator$00601.prototype, "Current", {
  "get": function () {
    const __ = this;

    return SetTreeModule$$$current(__.i);
  }
});

SetTreeModule$002EmkIEnumerator$00601.prototype.MoveNext = function () {
  const __$$1 = this;

  return SetTreeModule$$$moveNext(__$$1.i);
};

SetTreeModule$002EmkIEnumerator$00601.prototype.Reset = function () {
  const __$$2 = this;

  __$$2.i = SetTreeModule$$$mkIterator(__$$2.s);
};

SetTreeModule$002EmkIEnumerator$00601.prototype.Dispose = function () {};

function SetTreeModule$$$mkIEnumerator(s$$15) {
  return SetTreeModule$002EmkIEnumerator$00601$$$$002Ector$$Z5B395D56(s$$15);
}

function SetTreeModule$$$toSeq(s$$16) {
  const en = SetTreeModule$$$mkIEnumerator(s$$16);
  return (0, _Seq.unfold)(function generator(en$$1) {
    if (en$$1.MoveNext()) {
      return [en$$1.Current, en$$1];
    } else {
      return null;
    }
  }, en);
}

function SetTreeModule$$$compareStacks($comparer$$17$$112, $l1$$113, $l2$$114) {
  SetTreeModule$$$compareStacks: while (true) {
    const comparer$$17 = $comparer$$17$$112,
          l1 = $l1$$113,
          l2 = $l2$$114;
    var $target$$115, t1$$6, t2$$6, n1k, n2k, t1$$7, t2$$7, n1k$$1, n2k$$1, n2r, t1$$8, t2$$8, emp, n1k$$2, n1r, n2k$$2, t1$$9, t2$$9, n1k$$3, n1r$$1, n2k$$3, n2r$$1, t1$$10, t2$$10, n1k$$4, t1$$11, n1k$$5, n1l, n1r$$2, t1$$12, n2k$$4, t2$$11, n2k$$5, n2l, n2r$$2, t2$$12;

    if (l1.tail != null) {
      if (l2.tail != null) {
        if (l2.head.tag === 2) {
          if (l1.head.tag === 2) {
            $target$$115 = 4;
            n1k = l1.head.fields[0];
            n2k = l2.head.fields[0];
            t1$$7 = l1.tail;
            t2$$7 = l2.tail;
          } else if (l1.head.tag === 1) {
            if (l1.head.fields[1].tag === 0) {
              $target$$115 = 6;
              emp = l1.head.fields[1];
              n1k$$2 = l1.head.fields[0];
              n1r = l1.head.fields[2];
              n2k$$2 = l2.head.fields[0];
              t1$$9 = l1.tail;
              t2$$9 = l2.tail;
            } else {
              $target$$115 = 9;
              n1k$$5 = l1.head.fields[0];
              n1l = l1.head.fields[1];
              n1r$$2 = l1.head.fields[2];
              t1$$12 = l1.tail;
            }
          } else {
            $target$$115 = 10;
            n2k$$4 = l2.head.fields[0];
            t2$$11 = l2.tail;
          }
        } else if (l2.head.tag === 1) {
          if (l2.head.fields[1].tag === 0) {
            if (l1.head.tag === 2) {
              $target$$115 = 5;
              n1k$$1 = l1.head.fields[0];
              n2k$$1 = l2.head.fields[0];
              n2r = l2.head.fields[2];
              t1$$8 = l1.tail;
              t2$$8 = l2.tail;
            } else if (l1.head.tag === 1) {
              if (l1.head.fields[1].tag === 0) {
                $target$$115 = 7;
                n1k$$3 = l1.head.fields[0];
                n1r$$1 = l1.head.fields[2];
                n2k$$3 = l2.head.fields[0];
                n2r$$1 = l2.head.fields[2];
                t1$$10 = l1.tail;
                t2$$10 = l2.tail;
              } else {
                $target$$115 = 9;
                n1k$$5 = l1.head.fields[0];
                n1l = l1.head.fields[1];
                n1r$$2 = l1.head.fields[2];
                t1$$12 = l1.tail;
              }
            } else {
              $target$$115 = 11;
              n2k$$5 = l2.head.fields[0];
              n2l = l2.head.fields[1];
              n2r$$2 = l2.head.fields[2];
              t2$$12 = l2.tail;
            }
          } else if (l1.head.tag === 2) {
            $target$$115 = 8;
            n1k$$4 = l1.head.fields[0];
            t1$$11 = l1.tail;
          } else if (l1.head.tag === 1) {
            $target$$115 = 9;
            n1k$$5 = l1.head.fields[0];
            n1l = l1.head.fields[1];
            n1r$$2 = l1.head.fields[2];
            t1$$12 = l1.tail;
          } else {
            $target$$115 = 11;
            n2k$$5 = l2.head.fields[0];
            n2l = l2.head.fields[1];
            n2r$$2 = l2.head.fields[2];
            t2$$12 = l2.tail;
          }
        } else if (l1.head.tag === 2) {
          $target$$115 = 8;
          n1k$$4 = l1.head.fields[0];
          t1$$11 = l1.tail;
        } else if (l1.head.tag === 1) {
          $target$$115 = 9;
          n1k$$5 = l1.head.fields[0];
          n1l = l1.head.fields[1];
          n1r$$2 = l1.head.fields[2];
          t1$$12 = l1.tail;
        } else {
          $target$$115 = 3;
          t1$$6 = l1.tail;
          t2$$6 = l2.tail;
        }
      } else {
        $target$$115 = 2;
      }
    } else if (l2.tail != null) {
      $target$$115 = 1;
    } else {
      $target$$115 = 0;
    }

    switch ($target$$115) {
      case 0:
        {
          return 0;
        }

      case 1:
        {
          return -1 | 0;
        }

      case 2:
        {
          return 1;
        }

      case 3:
        {
          $comparer$$17$$112 = comparer$$17;
          $l1$$113 = t1$$6;
          $l2$$114 = t2$$6;
          continue SetTreeModule$$$compareStacks;
        }

      case 4:
        {
          const c$$7 = comparer$$17.Compare(n1k, n2k) | 0;

          if (c$$7 !== 0) {
            return c$$7 | 0;
          } else {
            $comparer$$17$$112 = comparer$$17;
            $l1$$113 = t1$$7;
            $l2$$114 = t2$$7;
            continue SetTreeModule$$$compareStacks;
          }
        }

      case 5:
        {
          const c$$8 = comparer$$17.Compare(n1k$$1, n2k$$1) | 0;

          if (c$$8 !== 0) {
            return c$$8 | 0;
          } else {
            $comparer$$17$$112 = comparer$$17;
            $l1$$113 = new _Types.List(new SetTree$00601(0, "SetEmpty"), t1$$8);
            $l2$$114 = new _Types.List(n2r, t2$$8);
            continue SetTreeModule$$$compareStacks;
          }
        }

      case 6:
        {
          const c$$9 = comparer$$17.Compare(n1k$$2, n2k$$2) | 0;

          if (c$$9 !== 0) {
            return c$$9 | 0;
          } else {
            $comparer$$17$$112 = comparer$$17;
            $l1$$113 = new _Types.List(n1r, t1$$9);
            $l2$$114 = new _Types.List(emp, t2$$9);
            continue SetTreeModule$$$compareStacks;
          }
        }

      case 7:
        {
          const c$$10 = comparer$$17.Compare(n1k$$3, n2k$$3) | 0;

          if (c$$10 !== 0) {
            return c$$10 | 0;
          } else {
            $comparer$$17$$112 = comparer$$17;
            $l1$$113 = new _Types.List(n1r$$1, t1$$10);
            $l2$$114 = new _Types.List(n2r$$1, t2$$10);
            continue SetTreeModule$$$compareStacks;
          }
        }

      case 8:
        {
          $comparer$$17$$112 = comparer$$17;
          $l1$$113 = new _Types.List(new SetTree$00601(0, "SetEmpty"), new _Types.List(SetTreeModule$$$SetOne(n1k$$4), t1$$11));
          $l2$$114 = l2;
          continue SetTreeModule$$$compareStacks;
        }

      case 9:
        {
          $comparer$$17$$112 = comparer$$17;
          $l1$$113 = new _Types.List(n1l, new _Types.List(SetTreeModule$$$SetNode(n1k$$5, new SetTree$00601(0, "SetEmpty"), n1r$$2, 0), t1$$12));
          $l2$$114 = l2;
          continue SetTreeModule$$$compareStacks;
        }

      case 10:
        {
          $comparer$$17$$112 = comparer$$17;
          $l1$$113 = l1;
          $l2$$114 = new _Types.List(new SetTree$00601(0, "SetEmpty"), new _Types.List(SetTreeModule$$$SetOne(n2k$$4), t2$$11));
          continue SetTreeModule$$$compareStacks;
        }

      case 11:
        {
          $comparer$$17$$112 = comparer$$17;
          $l1$$113 = l1;
          $l2$$114 = new _Types.List(n2l, new _Types.List(SetTreeModule$$$SetNode(n2k$$5, new SetTree$00601(0, "SetEmpty"), n2r$$2, 0), t2$$12));
          continue SetTreeModule$$$compareStacks;
        }
    }

    break;
  }
}

function SetTreeModule$$$compare(comparer$$18, s1, s2) {
  if (s1.tag === 0) {
    if (s2.tag === 0) {
      return 0;
    } else {
      return -1 | 0;
    }
  } else if (s2.tag === 0) {
    return 1;
  } else {
    return SetTreeModule$$$compareStacks(comparer$$18, new _Types.List(s1, new _Types.List()), new _Types.List(s2, new _Types.List())) | 0;
  }
}

function SetTreeModule$$$choose(s$$17) {
  return SetTreeModule$$$minimumElement(s$$17);
}

function SetTreeModule$$$loop($m$$8$$120, $acc$$10$$121) {
  SetTreeModule$$$loop: while (true) {
    const m$$8 = $m$$8$$120,
          acc$$10 = $acc$$10$$121;

    switch (m$$8.tag) {
      case 2:
        {
          return new _Types.List(m$$8.fields[0], acc$$10);
        }

      case 0:
        {
          return acc$$10;
        }

      default:
        {
          $m$$8$$120 = m$$8.fields[1];
          $acc$$10$$121 = new _Types.List(m$$8.fields[0], SetTreeModule$$$loop(m$$8.fields[2], acc$$10));
          continue SetTreeModule$$$loop;
        }
    }

    break;
  }
}

function SetTreeModule$$$toList(s$$18) {
  return SetTreeModule$$$loop(s$$18, new _Types.List());
}

function SetTreeModule$$$copyToArray(s$$19, arr, i$$2) {
  let j = i$$2 | 0;
  SetTreeModule$$$iter(function (x$$8) {
    arr[j] = x$$8;
    j = j + 1;
  }, s$$19);
}

function SetTreeModule$$$mkFromEnumerator($comparer$$19$$126, $acc$$11$$127, $e$$128) {
  SetTreeModule$$$mkFromEnumerator: while (true) {
    const comparer$$19 = $comparer$$19$$126,
          acc$$11 = $acc$$11$$127,
          e = $e$$128;

    if (e.MoveNext()) {
      $comparer$$19$$126 = comparer$$19;
      $acc$$11$$127 = SetTreeModule$$$add(comparer$$19, e.Current, acc$$11);
      $e$$128 = e;
      continue SetTreeModule$$$mkFromEnumerator;
    } else {
      return acc$$11;
    }

    break;
  }
}

function SetTreeModule$$$ofSeq(comparer$$20, c$$11) {
  const ie = (0, _Seq.getEnumerator)(c$$11);

  try {
    return SetTreeModule$$$mkFromEnumerator(comparer$$20, new SetTree$00601(0, "SetEmpty"), ie);
  } finally {
    if ((0, _Util.isDisposable)(ie)) {
      ie.Dispose();
    }
  }
}

function SetTreeModule$$$ofArray(comparer$$21, arr$$1) {
  let acc$$12 = new SetTree$00601(0, "SetEmpty");

  for (let i$$3 = 0; i$$3 <= arr$$1.length - 1; i$$3++) {
    acc$$12 = SetTreeModule$$$add(comparer$$21, arr$$1[i$$3], acc$$12);
  }

  return acc$$12;
}

const FSharpSet = (0, _Types.declare)(function Set_Set(comparer$$22, tree) {
  const $this$$2 = this;
  $this$$2.comparer = comparer$$22;
  $this$$2.tree = tree;
});
exports.FSharpSet = FSharpSet;

function FSharpSet$reflection($gen$$133) {
  return (0, _Reflection.type)("Set.FSharpSet", [$gen$$133]);
}

function FSharpSet$$$$002Ector$$2528C5CB(comparer$$22, tree) {
  return this instanceof FSharpSet ? FSharpSet.call(this, comparer$$22, tree) : new FSharpSet(comparer$$22, tree);
}

function FSharpSet$$get_Comparer(__$$4) {
  return __$$4.comparer;
}

function FSharpSet$$get_Tree(__$$5) {
  return __$$5.tree;
}

function FSharpSet$$Add$$2B595(s$$20, x$$9) {
  return FSharpSet$$$$002Ector$$2528C5CB(FSharpSet$$get_Comparer(s$$20), SetTreeModule$$$add(FSharpSet$$get_Comparer(s$$20), x$$9, FSharpSet$$get_Tree(s$$20)));
}

function FSharpSet$$Remove$$2B595(s$$21, x$$10) {
  return FSharpSet$$$$002Ector$$2528C5CB(FSharpSet$$get_Comparer(s$$21), SetTreeModule$$$remove(FSharpSet$$get_Comparer(s$$21), x$$10, FSharpSet$$get_Tree(s$$21)));
}

function FSharpSet$$get_Count(s$$22) {
  return SetTreeModule$$$count(FSharpSet$$get_Tree(s$$22));
}

function FSharpSet$$Contains$$2B595(s$$23, x$$11) {
  return SetTreeModule$$$mem(FSharpSet$$get_Comparer(s$$23), x$$11, FSharpSet$$get_Tree(s$$23));
}

function FSharpSet$$Iterate$$5028453F(s$$24, x$$12) {
  SetTreeModule$$$iter(x$$12, FSharpSet$$get_Tree(s$$24));
}

function FSharpSet$$Fold(s$$25, f$$10, z) {
  return SetTreeModule$$$fold(function (x$$13, z$$1) {
    return f$$10(z$$1, x$$13);
  }, z, FSharpSet$$get_Tree(s$$25));
}

function FSharpSet$$get_IsEmpty(s$$26) {
  return SetTreeModule$$$isEmpty(FSharpSet$$get_Tree(s$$26));
}

function FSharpSet$$Partition$$Z1D55A0D7(s$$27, f$$11) {
  const matchValue$$10 = FSharpSet$$get_Tree(s$$27);

  if (matchValue$$10.tag === 0) {
    return [s$$27, s$$27];
  } else {
    const patternInput$$6 = SetTreeModule$$$partition(FSharpSet$$get_Comparer(s$$27), f$$11, FSharpSet$$get_Tree(s$$27));
    return [FSharpSet$$$$002Ector$$2528C5CB(FSharpSet$$get_Comparer(s$$27), patternInput$$6[0]), FSharpSet$$$$002Ector$$2528C5CB(FSharpSet$$get_Comparer(s$$27), patternInput$$6[1])];
  }
}

function FSharpSet$$Filter$$Z1D55A0D7(s$$28, f$$12) {
  const matchValue$$11 = FSharpSet$$get_Tree(s$$28);

  if (matchValue$$11.tag === 0) {
    return s$$28;
  } else {
    return FSharpSet$$$$002Ector$$2528C5CB(FSharpSet$$get_Comparer(s$$28), SetTreeModule$$$filter(FSharpSet$$get_Comparer(s$$28), f$$12, FSharpSet$$get_Tree(s$$28)));
  }
}

function FSharpSet$$Map$$38806891(s$$29, f$$13, comparer$$23) {
  return FSharpSet$$$$002Ector$$2528C5CB(comparer$$23, SetTreeModule$$$fold(function (acc$$13, k$$33) {
    return SetTreeModule$$$add(comparer$$23, f$$13(k$$33), acc$$13);
  }, new SetTree$00601(0, "SetEmpty"), FSharpSet$$get_Tree(s$$29)));
}

function FSharpSet$$Exists$$Z1D55A0D7(s$$30, f$$14) {
  return SetTreeModule$$$exists(f$$14, FSharpSet$$get_Tree(s$$30));
}

function FSharpSet$$ForAll$$Z1D55A0D7(s$$31, f$$15) {
  return SetTreeModule$$$forall(f$$15, FSharpSet$$get_Tree(s$$31));
}

function FSharpSet$$$op_Subtraction(a$$4, b$$5) {
  const matchValue$$12 = FSharpSet$$get_Tree(a$$4);

  if (matchValue$$12.tag === 0) {
    return a$$4;
  } else {
    const matchValue$$13 = FSharpSet$$get_Tree(b$$5);

    if (matchValue$$13.tag === 0) {
      return a$$4;
    } else {
      return FSharpSet$$$$002Ector$$2528C5CB(FSharpSet$$get_Comparer(a$$4), SetTreeModule$$$diff(FSharpSet$$get_Comparer(a$$4), FSharpSet$$get_Tree(a$$4), FSharpSet$$get_Tree(b$$5)));
    }
  }
}

function FSharpSet$$$op_Addition(a$$5, b$$6) {
  const matchValue$$14 = FSharpSet$$get_Tree(b$$6);

  if (matchValue$$14.tag === 0) {
    return a$$5;
  } else {
    const matchValue$$15 = FSharpSet$$get_Tree(a$$5);

    if (matchValue$$15.tag === 0) {
      return b$$6;
    } else {
      return FSharpSet$$$$002Ector$$2528C5CB(FSharpSet$$get_Comparer(a$$5), SetTreeModule$$$union(FSharpSet$$get_Comparer(a$$5), FSharpSet$$get_Tree(a$$5), FSharpSet$$get_Tree(b$$6)));
    }
  }
}

function FSharpSet$$$Intersection$$Z3BE9BFE0(a$$6, b$$7) {
  const matchValue$$16 = FSharpSet$$get_Tree(b$$7);

  if (matchValue$$16.tag === 0) {
    return b$$7;
  } else {
    const matchValue$$17 = FSharpSet$$get_Tree(a$$6);

    if (matchValue$$17.tag === 0) {
      return a$$6;
    } else {
      return FSharpSet$$$$002Ector$$2528C5CB(FSharpSet$$get_Comparer(a$$6), SetTreeModule$$$intersection(FSharpSet$$get_Comparer(a$$6), FSharpSet$$get_Tree(a$$6), FSharpSet$$get_Tree(b$$7)));
    }
  }
}

function FSharpSet$$$IntersectionMany$$Z15B59630(sets) {
  return (0, _Seq.reduce)(FSharpSet$$$Intersection$$Z3BE9BFE0, sets);
}

function FSharpSet$$$Equality$$Z3BE9BFE0(a$$7, b$$8) {
  return SetTreeModule$$$compare(FSharpSet$$get_Comparer(a$$7), FSharpSet$$get_Tree(a$$7), FSharpSet$$get_Tree(b$$8)) === 0;
}

function FSharpSet$$$Compare$$Z3BE9BFE0(a$$8, b$$9) {
  return SetTreeModule$$$compare(FSharpSet$$get_Comparer(a$$8), FSharpSet$$get_Tree(a$$8), FSharpSet$$get_Tree(b$$9));
}

function FSharpSet$$get_Choose(x$$14) {
  return SetTreeModule$$$choose(FSharpSet$$get_Tree(x$$14));
}

function FSharpSet$$get_MinimumElement(x$$15) {
  return SetTreeModule$$$minimumElement(FSharpSet$$get_Tree(x$$15));
}

function FSharpSet$$get_MaximumElement(x$$16) {
  return SetTreeModule$$$maximumElement(FSharpSet$$get_Tree(x$$16));
}

function FSharpSet$$IsSubsetOf$$6A20B1FF(x$$17, y) {
  return SetTreeModule$$$subset(FSharpSet$$get_Comparer(x$$17), FSharpSet$$get_Tree(x$$17), FSharpSet$$get_Tree(y));
}

function FSharpSet$$IsSupersetOf$$6A20B1FF(x$$18, y$$1) {
  return SetTreeModule$$$subset(FSharpSet$$get_Comparer(x$$18), FSharpSet$$get_Tree(y$$1), FSharpSet$$get_Tree(x$$18));
}

function FSharpSet$$IsProperSubsetOf$$6A20B1FF(x$$19, y$$2) {
  return SetTreeModule$$$psubset(FSharpSet$$get_Comparer(x$$19), FSharpSet$$get_Tree(x$$19), FSharpSet$$get_Tree(y$$2));
}

function FSharpSet$$IsProperSupersetOf$$6A20B1FF(x$$20, y$$3) {
  return SetTreeModule$$$psubset(FSharpSet$$get_Comparer(x$$20), FSharpSet$$get_Tree(y$$3), FSharpSet$$get_Tree(x$$20));
}

FSharpSet.prototype.toString = function () {
  var strings;
  const this$ = this;
  return "set [" + (strings = (0, _Seq.map)(function (x$$21) {
    let copyOfStruct = x$$21;
    return String(copyOfStruct);
  }, this$), (0, _String.join)("; ", ...strings)) + "]";
};

FSharpSet.prototype.GetHashCode = function () {
  const this$$$1 = this;
  let res = 0;
  const e$$1 = SetTreeModule$$$mkIEnumerator(FSharpSet$$get_Tree(this$$$1));

  while (e$$1.MoveNext()) {
    const x$$22 = res | 0;
    const y$$4 = (0, _Util.structuralHash)(e$$1.Current) | 0;
    res = (x$$22 << 1) + y$$4 + 631;
  }

  return Math.abs(res) | 0;
};

FSharpSet.prototype.Equals = function (that) {
  const this$$$2 = this;
  return SetTreeModule$$$compare(FSharpSet$$get_Comparer(this$$$2), FSharpSet$$get_Tree(this$$$2), FSharpSet$$get_Tree(that)) === 0;
};

FSharpSet.prototype.CompareTo = function (that$$1) {
  const this$$$3 = this;
  return SetTreeModule$$$compare(FSharpSet$$get_Comparer(this$$$3), FSharpSet$$get_Tree(this$$$3), FSharpSet$$get_Tree(that$$1)) | 0;
};

FSharpSet.prototype[Symbol.iterator] = function () {
  const s$$32 = this;
  return (0, _Seq.toIterator)(SetTreeModule$$$mkIEnumerator(FSharpSet$$get_Tree(s$$32)));
};

function isEmpty(s$$33) {
  return FSharpSet$$get_IsEmpty(s$$33);
}

function contains(x$$23, s$$34) {
  return FSharpSet$$Contains$$2B595(s$$34, x$$23);
}

function add(x$$24, s$$35) {
  return FSharpSet$$Add$$2B595(s$$35, x$$24);
}

function singleton(x$$25, comparer$$24) {
  return FSharpSet$$$$002Ector$$2528C5CB(comparer$$24, new SetTree$00601(2, "SetOne", x$$25));
}

function remove(x$$26, s$$36) {
  return FSharpSet$$Remove$$2B595(s$$36, x$$26);
}

function union(s1$$2, s2$$2) {
  return FSharpSet$$$op_Addition(s1$$2, s2$$2);
}

function unionMany(sets$$1, comparer$$25) {
  return (0, _Seq.fold)(FSharpSet$$$op_Addition, FSharpSet$$$$002Ector$$2528C5CB(comparer$$25, new SetTree$00601(0, "SetEmpty")), sets$$1);
}

function intersect(s1$$3, s2$$3) {
  return FSharpSet$$$Intersection$$Z3BE9BFE0(s1$$3, s2$$3);
}

function intersectMany(sets$$2) {
  return FSharpSet$$$IntersectionMany$$Z15B59630(sets$$2);
}

function iterate(f$$16, s$$37) {
  FSharpSet$$Iterate$$5028453F(s$$37, f$$16);
}

function empty(comparer$$26) {
  return FSharpSet$$$$002Ector$$2528C5CB(comparer$$26, new SetTree$00601(0, "SetEmpty"));
}

function forAll(f$$17, s$$38) {
  return FSharpSet$$ForAll$$Z1D55A0D7(s$$38, f$$17);
}

function exists(f$$18, s$$39) {
  return FSharpSet$$Exists$$Z1D55A0D7(s$$39, f$$18);
}

function filter(f$$19, s$$40) {
  return FSharpSet$$Filter$$Z1D55A0D7(s$$40, f$$19);
}

function partition(f$$20, s$$41) {
  return FSharpSet$$Partition$$Z1D55A0D7(s$$41, f$$20);
}

function fold(f$$21, z$$2, s$$42) {
  return SetTreeModule$$$fold(f$$21, z$$2, FSharpSet$$get_Tree(s$$42));
}

function foldBack(f$$22, s$$43, z$$3) {
  return SetTreeModule$$$foldBack(f$$22, FSharpSet$$get_Tree(s$$43), z$$3);
}

function map(f$$23, s$$44, comparer$$27) {
  return FSharpSet$$Map$$38806891(s$$44, f$$23, comparer$$27);
}

function count(s$$45) {
  return FSharpSet$$get_Count(s$$45);
}

function minimumElement(s$$46) {
  return FSharpSet$$get_MinimumElement(s$$46);
}

function maximumElement(s$$47) {
  return FSharpSet$$get_MaximumElement(s$$47);
}

function ofList(li, comparer$$28) {
  return FSharpSet$$$$002Ector$$2528C5CB(comparer$$28, SetTreeModule$$$ofSeq(comparer$$28, li));
}

function ofArray(arr$$2, comparer$$29) {
  return FSharpSet$$$$002Ector$$2528C5CB(comparer$$29, SetTreeModule$$$ofArray(comparer$$29, arr$$2));
}

function toList(s$$48) {
  return SetTreeModule$$$toList(FSharpSet$$get_Tree(s$$48));
}

function toArray(s$$49, cons) {
  const n$$3 = count(s$$49) | 0;
  const res$$1 = new cons(n$$3);
  SetTreeModule$$$copyToArray(FSharpSet$$get_Tree(s$$49), res$$1, 0);
  return res$$1;
}

function toSeq(s$$50) {
  return SetTreeModule$$$toSeq(FSharpSet$$get_Tree(s$$50));
}

function ofSeq(elements, comparer$$30) {
  return FSharpSet$$$$002Ector$$2528C5CB(comparer$$30, SetTreeModule$$$ofSeq(comparer$$30, elements));
}

function difference(x$$28, y$$6) {
  return FSharpSet$$$op_Subtraction(x$$28, y$$6);
}

function isSubset(x$$29, y$$7) {
  return FSharpSet$$IsSubsetOf$$6A20B1FF(x$$29, y$$7);
}

function isSuperset(x$$30, y$$8) {
  return FSharpSet$$IsSupersetOf$$6A20B1FF(x$$30, y$$8);
}

function isProperSubset(x$$31, y$$9) {
  return FSharpSet$$IsProperSubsetOf$$6A20B1FF(x$$31, y$$9);
}

function isProperSuperset(x$$32, y$$10) {
  return FSharpSet$$IsProperSupersetOf$$6A20B1FF(x$$32, y$$10);
}

function minElement(s$$51) {
  return FSharpSet$$get_MinimumElement(s$$51);
}

function maxElement(s$$52) {
  return FSharpSet$$get_MaximumElement(s$$52);
}

function createMutablePrivate(comparer$$31, tree$0027) {
  let tree$$1 = tree$0027;
  return {
    get size() {
      return SetTreeModule$$$count(tree$$1);
    },

    add(x$$33) {
      const this$$$4 = this;
      tree$$1 = SetTreeModule$$$add(comparer$$31, x$$33, tree$$1);
      return this$$$4;
    },

    add_(x$$34) {
      if (SetTreeModule$$$mem(comparer$$31, x$$34, tree$$1)) {
        return false;
      } else {
        tree$$1 = SetTreeModule$$$add(comparer$$31, x$$34, tree$$1);
        return true;
      }
    },

    clear() {
      tree$$1 = new SetTree$00601(0, "SetEmpty");
    },

    delete(x$$35) {
      if (SetTreeModule$$$mem(comparer$$31, x$$35, tree$$1)) {
        tree$$1 = SetTreeModule$$$remove(comparer$$31, x$$35, tree$$1);
        return true;
      } else {
        return false;
      }
    },

    has(x$$36) {
      return SetTreeModule$$$mem(comparer$$31, x$$36, tree$$1);
    },

    values() {
      return SetTreeModule$$$toSeq(tree$$1);
    },

    [Symbol.iterator]() {
      return (0, _Seq.toIterator)(SetTreeModule$$$mkIEnumerator(tree$$1));
    },

    GetEnumerator() {
      return SetTreeModule$$$mkIEnumerator(tree$$1);
    }

  };
}

function createMutable(source, comparer$$32) {
  const set = (0, _MutableSet.MutableSet$00601$$$$002Ector$$Z79760D57)(comparer$$32);
  (0, _Seq.iterate)(function (item) {
    const value = (0, _MutableSet.MutableSet$00601$$Add$$2B595)(set, item);
    value, null;
  }, source);
  return set;
}

function distinct(xs, comparer$$33) {
  return (0, _Seq.delay)(function () {
    const set$$1 = (0, _MutableSet.MutableSet$00601$$$$002Ector$$Z79760D57)(comparer$$33);
    return (0, _Seq.collect)(function (x$$37) {
      return (0, _MutableSet.MutableSet$00601$$Add$$2B595)(set$$1, x$$37) ? (0, _Seq.singleton)(x$$37) : (0, _Seq.empty)();
    }, xs);
  });
}

function distinctBy(projection, xs$$1, comparer$$34) {
  return (0, _Seq.delay)(function () {
    const set$$2 = (0, _MutableSet.MutableSet$00601$$$$002Ector$$Z79760D57)(comparer$$34);
    return (0, _Seq.collect)(function (x$$38) {
      return (0, _MutableSet.MutableSet$00601$$Add$$2B595)(set$$2, projection(x$$38)) ? (0, _Seq.singleton)(x$$38) : (0, _Seq.empty)();
    }, xs$$1);
  });
}

function unionWith(s1$$4, s2$$4) {
  return (0, _Seq.fold)(function folder(acc$$14, x$$39) {
    return acc$$14.add(x$$39);
  }, s1$$4, s2$$4);
}

function intersectWith(s1$$5, s2$$5, comparer$$35) {
  const s2$$6 = ofSeq(s2$$5, comparer$$35);
  (0, _Seq.iterate)(function (x$$40) {
    if (!FSharpSet$$Contains$$2B595(s2$$6, x$$40)) {
      const value$$1 = s1$$5.delete(x$$40);
      value$$1, null;
    }
  }, s1$$5);
}

function exceptWith(s1$$6, s2$$7) {
  (0, _Seq.iterate)(function (x$$41) {
    const value$$2 = s1$$6.delete(x$$41);
    value$$2, null;
  }, s2$$7);
}

function isSubsetOf(s1$$7, s2$$8, comparer$$36) {
  return isSubset(ofSeq(s1$$7, comparer$$36), ofSeq(s2$$8, comparer$$36));
}

function isSupersetOf(s1$$8, s2$$9, comparer$$37) {
  return isSuperset(ofSeq(s1$$8, comparer$$37), ofSeq(s2$$9, comparer$$37));
}

function isProperSubsetOf(s1$$9, s2$$10, comparer$$38) {
  return isProperSubset(ofSeq(s1$$9, comparer$$38), ofSeq(s2$$10, comparer$$38));
}

function isProperSupersetOf(s1$$10, s2$$11, comparer$$39) {
  return isProperSuperset(ofSeq(s1$$10, comparer$$39), ofSeq(s2$$11, comparer$$39));
}