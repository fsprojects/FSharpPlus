"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Option$$$apply = Option$$$apply;
exports.Option$$$unzip = Option$$$unzip;
exports.Option$$$zip = Option$$$zip;
exports.Option$$$toResult = Option$$$toResult;
exports.Option$$$toResultWith = Option$$$toResultWith;
exports.Option$$$ofResult = Option$$$ofResult;
exports.Result$$$result = Result$$$result;
exports.Result$$$throw = Result$$$throw;
exports.Result$$$apply = Result$$$apply;
exports.Result$$$flatten = Result$$$flatten;
exports.Choice$$$result = Choice$$$result;
exports.Choice$$$throw = Choice$$$throw;
exports.Choice$$$apply = Choice$$$apply;
exports.Choice$$$map = Choice$$$map;
exports.Choice$$$flatten = Choice$$$flatten;
exports.Choice$$$bind = Choice$$$bind;
exports.Seq$$$bind = Seq$$$bind;
exports.Seq$$$apply = Seq$$$apply;
exports.Seq$$$foldBack = Seq$$$foldBack;
exports.Seq$$$chunkBy = Seq$$$chunkBy;
exports.Seq$$$intersperse = Seq$$$intersperse;
exports.Seq$$$intercalate = Seq$$$intercalate;
exports.Seq$$$split = Seq$$$split;
exports.Seq$$$replace = Seq$$$replace;
exports.Seq$$$drop = Seq$$$drop;
exports.List$$$singleton = List$$$singleton;
exports.List$$$cons = List$$$cons;
exports.List$$$apply = List$$$apply;
exports.List$$$tails = List$$$tails;
exports.List$$$take = List$$$take;
exports.List$$$skip = List$$$skip;
exports.List$$$drop = List$$$drop;
exports.List$$$intercalate = List$$$intercalate;
exports.List$$$intersperse = List$$$intersperse;
exports.List$$$split = List$$$split;
exports.List$$$replace = List$$$replace;
exports.List$$$toIReadOnlyList = List$$$toIReadOnlyList;
exports.Array$$$intercalate = Array$$$intercalate;
exports.Array$$$intersperse = Array$$$intersperse;
exports.Array$$$split = Array$$$split;
exports.Array$$$replace = Array$$$replace;
exports.String$$$intercalate = String$$$intercalate;
exports.String$$$intersperse = String$$$intersperse;
exports.String$$$split = String$$$split;
exports.String$$$replace = String$$$replace;
exports.String$$$isSubString = String$$$isSubString;
exports.String$$$endsWith = String$$$endsWith;
exports.String$$$contains = String$$$contains;
exports.String$$$toUpper = String$$$toUpper;
exports.String$$$toLower = String$$$toLower;
exports.String$$$trimWhiteSpaces = String$$$trimWhiteSpaces;
exports.String$$$padLeft = String$$$padLeft;
exports.String$$$padLeftWith = String$$$padLeftWith;
exports.String$$$padRight = String$$$padRight;
exports.String$$$padRightWith = String$$$padRightWith;
exports.String$$$trim = String$$$trim;
exports.String$$$trimStart = String$$$trimStart;
exports.String$$$trimEnd = String$$$trimEnd;
exports.String$$$toArray = String$$$toArray;
exports.String$$$ofArray = String$$$ofArray;
exports.String$$$toList = String$$$toList;
exports.String$$$ofList = String$$$ofList;
exports.String$$$toSeq = String$$$toSeq;
exports.String$$$ofSeq = String$$$ofSeq;
exports.String$$$item = String$$$item;
exports.String$$$tryItem = String$$$tryItem;
exports.String$$$rev = String$$$rev;
exports.String$$$take = String$$$take;
exports.String$$$skip = String$$$skip;
exports.String$$$takeWhile = String$$$takeWhile;
exports.String$$$skipWhile = String$$$skipWhile;
exports.String$$$truncate = String$$$truncate;
exports.String$$$drop = String$$$drop;
exports.String$$$findIndex = String$$$findIndex;
exports.String$$$tryFindIndex = String$$$tryFindIndex;
exports.String$$$findSliceIndex = String$$$findSliceIndex;
exports.String$$$tryFindSliceIndex = String$$$tryFindSliceIndex;
exports.String$$$getBytes = String$$$getBytes;
exports.IReadOnlyCollection$$$ofArray = IReadOnlyCollection$$$ofArray;
exports.IReadOnlyCollection$$$ofList = IReadOnlyCollection$$$ofList;
exports.IReadOnlyCollection$$$ofSeq = IReadOnlyCollection$$$ofSeq;
exports.IReadOnlyCollection$$$map = IReadOnlyCollection$$$map;
exports.IReadOnlyList$$$toArray = IReadOnlyList$$$toArray;
exports.IReadOnlyList$$$tryItem = IReadOnlyList$$$tryItem;
exports.Map$$$keys = Map$$$keys;
exports.Map$$$values = Map$$$values;
exports.Map$$$mapValues = Map$$$mapValues;
exports.Map$$$mapValues2 = Map$$$mapValues2;
exports.Map$$$zip = Map$$$zip;
exports.Map$$$unzip = Map$$$unzip;
exports.Map$$$unionWith = Map$$$unionWith;
exports.Map$$$union = Map$$$union;
exports.Dict$$$tryGetValue = Dict$$$tryGetValue;
exports.Dict$$$containsKey = Dict$$$containsKey;
exports.Dict$$$keys = Dict$$$keys;
exports.Dict$$$values = Dict$$$values;
exports.Dict$$$map = Dict$$$map;
exports.Dict$$$map2 = Dict$$$map2;
exports.Dict$$$zip = Dict$$$zip;
exports.Dict$$$unzip = Dict$$$unzip;
exports.Dict$$$unionWith = Dict$$$unionWith;
exports.IReadOnlyDictionary$$$add = IReadOnlyDictionary$$$add;
exports.IReadOnlyDictionary$$$tryGetValue = IReadOnlyDictionary$$$tryGetValue;
exports.IReadOnlyDictionary$$$containsKey = IReadOnlyDictionary$$$containsKey;
exports.IReadOnlyDictionary$$$keys = IReadOnlyDictionary$$$keys;
exports.IReadOnlyDictionary$$$values = IReadOnlyDictionary$$$values;
exports.IReadOnlyDictionary$$$map = IReadOnlyDictionary$$$map;
exports.IReadOnlyDictionary$$$map2 = IReadOnlyDictionary$$$map2;
exports.IReadOnlyDictionary$$$zip = IReadOnlyDictionary$$$zip;
exports.IReadOnlyDictionary$$$unzip = IReadOnlyDictionary$$$unzip;
exports.IReadOnlyDictionary$$$unionWith = IReadOnlyDictionary$$$unionWith;
exports.Async$$$map = Async$$$map;
exports.Async$$$map2 = Async$$$map2;
exports.Async$$$zip = Async$$$zip;
exports.Async$$$join = Async$$$join;
exports.Async$$$apply = Async$$$apply;
exports.Async$$$raise = Async$$$raise;
exports.System$002ECollections$002EGeneric$002EIEnumerable$00601$$IEnumerable$00601$002Eget_GetSlice = System$002ECollections$002EGeneric$002EIEnumerable$00601$$IEnumerable$00601$002Eget_GetSlice;
exports.Microsoft$002EFSharp$002ECollections$002EFSharpList$00601$$List$00601$002Eget_GetSlice = Microsoft$002EFSharp$002ECollections$002EFSharpList$00601$$List$00601$002Eget_GetSlice;
exports.Microsoft$002EFSharp$002EControl$002EFSharpAsync$00601$$Async$00601$002ESequence$002EStatic$$65E6EDE0 = Microsoft$002EFSharp$002EControl$002EFSharpAsync$00601$$Async$00601$002ESequence$002EStatic$$65E6EDE0;
exports.Microsoft$002EFSharp$002EControl$002EFSharpAsync$00601$$Async$00601$002ESequence$002EStatic$$Z3B031D5B = Microsoft$002EFSharp$002EControl$002EFSharpAsync$00601$$Async$00601$002ESequence$002EStatic$$Z3B031D5B;
exports.Microsoft$002EFSharp$002EControl$002EFSharpAsync$00601$$Async$00601$002ESequence$002EStatic$$75C1C4A9 = Microsoft$002EFSharp$002EControl$002EFSharpAsync$00601$$Async$00601$002ESequence$002EStatic$$75C1C4A9;
exports.Microsoft$002EFSharp$002ECore$002EFSharpOption$00601$$Option$00601$002ESequence$002EStatic$$19140039 = Microsoft$002EFSharp$002ECore$002EFSharpOption$00601$$Option$00601$002ESequence$002EStatic$$19140039;

var _Option = require("./fable-library.2.3.24/Option");

var _Seq = require("./fable-library.2.3.24/Seq");

var _Array = require("./fable-library.2.3.24/Array");

var _Util = require("./fable-library.2.3.24/Util");

var _List = require("./fable-library.2.3.24/List");

var _Types = require("./fable-library.2.3.24/Types");

var _String = require("./fable-library.2.3.24/String");

var _Map = require("./fable-library.2.3.24/Map");

var _AsyncBuilder = require("./fable-library.2.3.24/AsyncBuilder");

var _Async = require("./fable-library.2.3.24/Async");

function Option$$$apply(f, x) {
  var $target$$36, f$$1, x$$1;

  if (f != null) {
    if (x != null) {
      $target$$36 = 0;
      f$$1 = f;
      x$$1 = (0, _Option.value)(x);
    } else {
      $target$$36 = 1;
    }
  } else {
    $target$$36 = 1;
  }

  switch ($target$$36) {
    case 0:
      {
        return (0, _Option.some)(f$$1(x$$1));
      }

    case 1:
      {
        return null;
      }
  }
}

function Option$$$unzip(v) {
  if (v != null) {
    const y = v[1];
    const x$$2 = v[0];
    return [(0, _Option.some)(x$$2), (0, _Option.some)(y)];
  } else {
    return [null, null];
  }
}

function Option$$$zip(x$$3, y$$1) {
  var $target$$40, x$$4, y$$2;

  if (x$$3 != null) {
    if (y$$1 != null) {
      $target$$40 = 0;
      x$$4 = (0, _Option.value)(x$$3);
      y$$2 = (0, _Option.value)(y$$1);
    } else {
      $target$$40 = 1;
    }
  } else {
    $target$$40 = 1;
  }

  switch ($target$$40) {
    case 0:
      {
        return [x$$4, y$$2];
      }

    case 1:
      {
        return null;
      }
  }
}

function Option$$$toResult(source) {
  if (source == null) {
    return new _Option.Result(1, "Error", null);
  } else {
    const x$$5 = (0, _Option.value)(source);
    return new _Option.Result(0, "Ok", x$$5);
  }
}

function Option$$$toResultWith(errorValue, source$$1) {
  if (source$$1 == null) {
    return new _Option.Result(1, "Error", errorValue);
  } else {
    const x$$6 = (0, _Option.value)(source$$1);
    return new _Option.Result(0, "Ok", x$$6);
  }
}

function Option$$$ofResult(source$$2) {
  if (source$$2.tag === 1) {
    return null;
  } else {
    return (0, _Option.some)(source$$2.fields[0]);
  }
}

function Result$$$result(x$$8) {
  return new _Option.Result(0, "Ok", x$$8);
}

function Result$$$throw(x$$9) {
  return new _Option.Result(1, "Error", x$$9);
}

function Result$$$apply(f$$2, x$$10) {
  var $target$$49, a, b, e;

  if (f$$2.tag === 1) {
    $target$$49 = 1;
    e = f$$2.fields[0];
  } else if (x$$10.tag === 1) {
    $target$$49 = 1;
    e = x$$10.fields[0];
  } else {
    $target$$49 = 0;
    a = f$$2.fields[0];
    b = x$$10.fields[0];
  }

  switch ($target$$49) {
    case 0:
      {
        return new _Option.Result(0, "Ok", a(b));
      }

    case 1:
      {
        return new _Option.Result(1, "Error", e);
      }
  }
}

function Result$$$flatten(_arg1) {
  var $target$$51, v$$1, e$$1;

  if (_arg1.tag === 1) {
    $target$$51 = 1;
    e$$1 = _arg1.fields[0];
  } else if (_arg1.fields[0].tag === 1) {
    $target$$51 = 1;
    e$$1 = _arg1.fields[0].fields[0];
  } else {
    $target$$51 = 0;
    v$$1 = _arg1.fields[0].fields[0];
  }

  switch ($target$$51) {
    case 0:
      {
        return new _Option.Result(0, "Ok", v$$1);
      }

    case 1:
      {
        return new _Option.Result(1, "Error", e$$1);
      }
  }
}

function Choice$$$result(x$$11) {
  return new _Option.Choice(0, "Choice1Of2", x$$11);
}

function Choice$$$throw(x$$12) {
  return new _Option.Choice(1, "Choice2Of2", x$$12);
}

function Choice$$$apply(f$$3, x$$13) {
  var $target$$56, a$$1, b$$1, e$$2;

  if (f$$3.tag === 1) {
    $target$$56 = 1;
    e$$2 = f$$3.fields[0];
  } else if (x$$13.tag === 1) {
    $target$$56 = 1;
    e$$2 = x$$13.fields[0];
  } else {
    $target$$56 = 0;
    a$$1 = f$$3.fields[0];
    b$$1 = x$$13.fields[0];
  }

  switch ($target$$56) {
    case 0:
      {
        return new _Option.Choice(0, "Choice1Of2", a$$1(b$$1));
      }

    case 1:
      {
        return new _Option.Choice(1, "Choice2Of2", e$$2);
      }
  }
}

function Choice$$$map(f$$4, _arg1$$1) {
  if (_arg1$$1.tag === 1) {
    return new _Option.Choice(1, "Choice2Of2", _arg1$$1.fields[0]);
  } else {
    return new _Option.Choice(0, "Choice1Of2", f$$4(_arg1$$1.fields[0]));
  }
}

function Choice$$$flatten(_arg1$$2) {
  var $target$$60, v$$3, e$$4;

  if (_arg1$$2.tag === 1) {
    $target$$60 = 1;
    e$$4 = _arg1$$2.fields[0];
  } else if (_arg1$$2.fields[0].tag === 1) {
    $target$$60 = 1;
    e$$4 = _arg1$$2.fields[0].fields[0];
  } else {
    $target$$60 = 0;
    v$$3 = _arg1$$2.fields[0].fields[0];
  }

  switch ($target$$60) {
    case 0:
      {
        return new _Option.Choice(0, "Choice1Of2", v$$3);
      }

    case 1:
      {
        return new _Option.Choice(1, "Choice2Of2", e$$4);
      }
  }
}

function Choice$$$bind(f$$5, _arg1$$3) {
  if (_arg1$$3.tag === 1) {
    return new _Option.Choice(1, "Choice2Of2", _arg1$$3.fields[0]);
  } else {
    return f$$5(_arg1$$3.fields[0]);
  }
}

function Seq$$$bind(f$$6, x$$14) {
  return (0, _Seq.collect)(f$$6, x$$14);
}

function Seq$$$apply(f$$7, x$$15) {
  return Seq$$$bind(function (f$$8) {
    return (0, _Seq.map)(function (arg1) {
      return f$$8(arg1);
    }, x$$15);
  }, f$$7);
}

function Seq$$$foldBack(f$$9, x$$16, z) {
  return (0, _Array.foldBack)(f$$9, (0, _Array.ofSeq)(x$$16, Array), z);
}

function Seq$$$chunkBy(projection, source$$3) {
  return (0, _Seq.delay)(function () {
    return (0, _Seq.enumerateUsing)((0, _Seq.getEnumerator)(source$$3), function (e$$6) {
      if (e$$6.MoveNext()) {
        let g = projection(e$$6.Current);
        let members = [];
        members.push(e$$6.Current);
        return (0, _Seq.append)((0, _Seq.enumerateWhile)(function () {
          return e$$6.MoveNext();
        }, (0, _Seq.delay)(function () {
          const key = projection(e$$6.Current);

          if ((0, _Util.equals)(g, key)) {
            members.push(e$$6.Current);
            return (0, _Seq.empty)();
          } else {
            return (0, _Seq.append)((0, _Seq.singleton)([g, members]), (0, _Seq.delay)(function () {
              g = key;
              members = [];
              members.push(e$$6.Current);
              return (0, _Seq.empty)();
            }));
          }
        })), (0, _Seq.delay)(function () {
          return (0, _Seq.singleton)([g, members]);
        }));
      } else {
        return (0, _Seq.empty)();
      }
    });
  });
}

function Seq$$$intersperse(sep, list) {
  return (0, _Seq.delay)(function () {
    let notFirst = false;
    return (0, _Seq.collect)(function (element) {
      return (0, _Seq.append)(notFirst ? (0, _Seq.singleton)(sep) : (0, _Seq.empty)(), (0, _Seq.delay)(function () {
        return (0, _Seq.append)((0, _Seq.singleton)(element), (0, _Seq.delay)(function () {
          notFirst = true;
          return (0, _Seq.empty)();
        }));
      }));
    }, list);
  });
}

function Seq$$$intercalate(separator, source$$4) {
  return (0, _Seq.delay)(function () {
    let notFirst$$1 = false;
    return (0, _Seq.collect)(function (element$$1) {
      return (0, _Seq.append)(notFirst$$1 ? separator : (0, _Seq.empty)(), (0, _Seq.delay)(function () {
        return (0, _Seq.append)(element$$1, (0, _Seq.delay)(function () {
          notFirst$$1 = true;
          return (0, _Seq.empty)();
        }));
      }));
    }, source$$4);
  });
}

function Seq$$$split(separators, source$$5) {
  return (0, _Seq.delay)(function () {
    let matchValue$$4;
    let source$$8;
    source$$8 = (0, _Seq.map)(function mapping(source$$6) {
      return (0, _List.ofSeq)(source$$6);
    }, separators);
    matchValue$$4 = (0, _List.ofSeq)(source$$8);

    if (matchValue$$4.tail == null) {
      return (0, _Seq.singleton)(source$$5);
    } else {
      const buffer = [];
      let candidate;
      let arg00;
      let list$$3;
      list$$3 = (0, _List.map)(_List.length, matchValue$$4);
      arg00 = (0, _List.max)(list$$3, {
        Compare: _Util.comparePrimitives
      });
      candidate = [];
      let i = 0;
      return (0, _Seq.append)((0, _Seq.collect)(function (item) {
        candidate.push(item);
        let matchValue$$5;
        matchValue$$5 = (0, _List.filter)(function predicate(sep$$1) {
          if ((0, _List.length)(sep$$1) > i) {
            return (0, _Util.equals)(item, (0, _List.item)(i, sep$$1));
          } else {
            return false;
          }
        }, matchValue$$4);

        if (matchValue$$5.tail == null) {
          i = 0;
          (0, _Array.addRangeInPlace)(candidate, buffer);
          (0, _Util.clear)(candidate);
          return (0, _Seq.empty)();
        } else if ((0, _List.exists)(function predicate$$1(sep$$2) {
          return (0, _List.length)(sep$$2) === i + 1;
        }, matchValue$$5)) {
          i = 0;
          return (0, _Seq.append)((0 === 0 ? true : buffer.length > 0) ? (0, _Seq.singleton)(buffer.slice()) : (0, _Seq.empty)(), (0, _Seq.delay)(function () {
            (0, _Util.clear)(buffer);
            (0, _Util.clear)(candidate);
            return (0, _Seq.empty)();
          }));
        } else {
          i = i + 1;
          return (0, _Seq.empty)();
        }
      }, source$$5), (0, _Seq.delay)(function () {
        return (0, _Seq.append)(candidate.length > 0 ? ((0, _Array.addRangeInPlace)(candidate, buffer), (0, _Seq.empty)()) : (0, _Seq.empty)(), (0, _Seq.delay)(function () {
          return (0 === 0 ? true : buffer.length > 0) ? (0, _Seq.singleton)(buffer) : (0, _Seq.empty)();
        }));
      }));
    }
  });
}

function Seq$$$replace(oldValue, newValue, source$$9) {
  return (0, _Seq.delay)(function () {
    let old;
    old = (0, _List.ofSeq)(oldValue);

    if ((0, _List.length)(old) === 0) {
      return source$$9;
    } else {
      const candidate$$1 = [];
      let sindex = 0;
      return (0, _Seq.append)((0, _Seq.collect)(function (item$$1) {
        candidate$$1.push(item$$1);

        if ((0, _Util.equals)(item$$1, (0, _List.item)(sindex, old))) {
          sindex = sindex + 1;

          if (sindex >= (0, _List.length)(old)) {
            sindex = 0;
            return (0, _Seq.append)(newValue, (0, _Seq.delay)(function () {
              (0, _Util.clear)(candidate$$1);
              return (0, _Seq.empty)();
            }));
          } else {
            return (0, _Seq.empty)();
          }
        } else {
          sindex = 0;
          return (0, _Seq.append)(candidate$$1, (0, _Seq.delay)(function () {
            (0, _Util.clear)(candidate$$1);
            return (0, _Seq.empty)();
          }));
        }
      }, source$$9), (0, _Seq.delay)(function () {
        return candidate$$1;
      }));
    }
  });
}

function Seq$$$drop(i$$1, source$$11) {
  let count = i$$1 | 0;
  const e$$7 = (0, _Seq.getEnumerator)(source$$11);

  try {
    while (count > 0 ? e$$7.MoveNext() : false) {
      count = count - 1;
    }

    return (0, _Seq.delay)(function () {
      return (0, _Seq.enumerateWhile)(function () {
        return e$$7.MoveNext();
      }, (0, _Seq.delay)(function () {
        return (0, _Seq.singleton)(e$$7.Current);
      }));
    });
  } finally {
    if ((0, _Util.isDisposable)(e$$7)) {
      e$$7.Dispose();
    }
  }
}

function List$$$singleton(x$$17) {
  return new _Types.List(x$$17, new _Types.List());
}

function List$$$cons(x$$18, y$$3) {
  return new _Types.List(x$$18, y$$3);
}

function List$$$apply(f$$10, x$$19) {
  return (0, _List.collect)(function (f$$11) {
    return (0, _List.map)(function (arg1$$1) {
      return f$$11(arg1$$1);
    }, x$$19);
  }, f$$10);
}

function List$$$tails(x$$20) {
  const loop = function loop(_arg1$$4) {
    if (_arg1$$4.tail != null) {
      return new _Types.List(_arg1$$4, loop(_arg1$$4.tail));
    } else {
      return new _Types.List();
    }
  };

  return loop(x$$20);
}

function List$$$take(i$$2, list$$6) {
  const source$$12 = (0, _Seq.take)(i$$2, list$$6);
  return (0, _List.ofSeq)(source$$12);
}

function List$$$skip(i$$3, list$$7) {
  const listSkip = function listSkip($lst$$97, $_arg1$$5$$98) {
    listSkip: while (true) {
      const lst = $lst$$97,
            _arg1$$5 = $_arg1$$5$$98;

      if (_arg1$$5 === 0) {
        return lst;
      } else {
        $lst$$97 = (0, _List.tail)(lst);
        $_arg1$$5$$98 = _arg1$$5 - 1;
        continue listSkip;
      }

      break;
    }
  };

  return listSkip(list$$7, i$$3);
}

function List$$$drop(i$$4, list$$8) {
  const loop$$1 = function loop$$1($i$$5$$101, $lst$$1$$102) {
    loop$$1: while (true) {
      const i$$5 = $i$$5$$101,
            lst$$1 = $lst$$1$$102;
      var $target$$103, x$$21;

      if (lst$$1.tail == null) {
        $target$$103 = 0;
        x$$21 = lst$$1;
      } else if (i$$5 === 0) {
        $target$$103 = 0;
        x$$21 = lst$$1;
      } else {
        $target$$103 = 1;
      }

      switch ($target$$103) {
        case 0:
          {
            return x$$21;
          }

        case 1:
          {
            $i$$5$$101 = i$$5 - 1;
            $lst$$1$$102 = (0, _List.tail)(lst$$1);
            continue loop$$1;
          }
      }

      break;
    }
  };

  if (i$$4 > 0) {
    return loop$$1(i$$4, list$$8);
  } else {
    return list$$8;
  }
}

function List$$$intercalate(separator$$1, source$$13) {
  let source$$15;
  source$$15 = Seq$$$intercalate(separator$$1, source$$13);
  return (0, _List.ofSeq)(source$$15);
}

function List$$$intersperse(element$$2, source$$16) {
  let source$$17;
  let list$$10;
  list$$10 = source$$16;
  source$$17 = Seq$$$intersperse(element$$2, list$$10);
  return (0, _List.ofSeq)(source$$17);
}

function List$$$split(separators$$2, source$$18) {
  let source$$21;
  let source$$19;
  source$$19 = source$$18;
  source$$21 = Seq$$$split(separators$$2, source$$19);
  return (0, _Seq.map)(_List.ofSeq, source$$21);
}

function List$$$replace(oldValue$$1, newValue$$1, source$$22) {
  let source$$24;
  let source$$23;
  source$$23 = source$$22;
  source$$24 = Seq$$$replace(oldValue$$1, newValue$$1, source$$23);
  return (0, _List.ofSeq)(source$$24);
}

function List$$$toIReadOnlyList(source$$25) {
  return {
    get Count() {
      return (0, _List.length)(source$$25);
    },

    get_Item(index) {
      return (0, _List.item)(index, source$$25);
    },

    [Symbol.iterator]() {
      return (0, _Seq.toIterator)((0, _Seq.getEnumerator)(source$$25));
    },

    GetEnumerator() {
      return (0, _Seq.getEnumerator)(source$$25);
    }

  };
}

function Array$$$intercalate(separator$$2, source$$26) {
  let source$$28;
  source$$28 = Seq$$$intercalate(separator$$2, source$$26);
  return (0, _Array.ofSeq)(source$$28, Array);
}

function Array$$$intersperse(element$$3, source$$29) {
  let source$$30;
  let list$$13;
  list$$13 = source$$29;
  source$$30 = Seq$$$intersperse(element$$3, list$$13);
  return (0, _Array.ofSeq)(source$$30, Array);
}

function Array$$$split(separators$$3, source$$31) {
  let source$$34;
  let source$$32;
  source$$32 = source$$31;
  source$$34 = Seq$$$split(separators$$3, source$$32);
  return (0, _Seq.map)(function mapping$$3(source$$33) {
    return (0, _Array.ofSeq)(source$$33, Array);
  }, source$$34);
}

function Array$$$replace(oldValue$$2, newValue$$2, source$$35) {
  let source$$37;
  let source$$36;
  source$$36 = source$$35;
  source$$37 = Seq$$$replace(oldValue$$2, newValue$$2, source$$36);
  return (0, _Array.ofSeq)(source$$37, Array);
}

function String$$$intercalate(separator$$3, source$$38) {
  return (0, _String.join)(separator$$3, ...source$$38);
}

function String$$$intersperse(element$$4, source$$39) {
  return (0, _String.join)("", ...(0, _Array.ofSeq)((Seq$$$intersperse(element$$4, source$$39.split(""))), Array));
}

function String$$$split(separators$$4, source$$40) {
  return (0, _String.split)(source$$40, (0, _Array.ofSeq)(separators$$4, Array), null, 0);
}

function String$$$replace(oldValue$$3, newValue$$3, source$$41) {
  if (oldValue$$3.length === 0) {
    return source$$41;
  } else {
    return (0, _String.replace)(source$$41, oldValue$$3, newValue$$3);
  }
}

function String$$$isSubString(subString, source$$42) {
  return source$$42.indexOf(subString) >= 0;
}

function String$$$endsWith(subString$$1, source$$43) {
  return (0, _String.endsWith)(source$$43, subString$$1, false, {});
}

function String$$$contains(char$, source$$44) {
  return (0, _Seq.contains)(char$, source$$44.split(""));
}

function String$$$toUpper(source$$45) {
  if (source$$45 == null) {
    return source$$45;
  } else {
    return source$$45.toUpperCase();
  }
}

function String$$$toLower(source$$46) {
  if (source$$46 == null) {
    return source$$46;
  } else {
    return source$$46.toLowerCase();
  }
}

function String$$$trimWhiteSpaces(source$$47) {
  return source$$47.trim();
}

function String$$$padLeft(totalLength, source$$48) {
  return (0, _String.padLeft)(source$$48, totalLength);
}

function String$$$padLeftWith(totalLength$$1, paddingChar, source$$49) {
  return (0, _String.padLeft)(source$$49, totalLength$$1, paddingChar);
}

function String$$$padRight(totalLength$$2, source$$50) {
  return (0, _String.padRight)(source$$50, totalLength$$2);
}

function String$$$padRightWith(totalLength$$3, paddingChar$$1, source$$51) {
  return (0, _String.padRight)(source$$51, totalLength$$3, paddingChar$$1);
}

function String$$$trim(trimChars, source$$52) {
  return (0, _String.trim)(source$$52, ...(0, _Array.ofSeq)(trimChars, Array));
}

function String$$$trimStart(trimChars$$1, source$$53) {
  return (0, _String.trimStart)(source$$53, ...(0, _Array.ofSeq)(trimChars$$1, Array));
}

function String$$$trimEnd(trimChars$$2, source$$54) {
  return (0, _String.trimEnd)(source$$54, ...(0, _Array.ofSeq)(trimChars$$2, Array));
}

function String$$$toArray(source$$55) {
  return source$$55.split("");
}

function String$$$ofArray(source$$56) {
  return source$$56.join("");
}

function String$$$toList(source$$57) {
  const array$$3 = String$$$toArray(source$$57);
  return (0, _List.ofArray)(array$$3);
}

function String$$$ofList(source$$58) {
  return ((0, _Array.ofList)(source$$58, Array)).join("");
}

function String$$$toSeq(source$$59) {
  return source$$59.split("");
}

function String$$$ofSeq(source$$60) {
  return (0, _String.join)("", ...source$$60);
}

function String$$$item(index$$1, source$$61) {
  return source$$61[index$$1];
}

function String$$$tryItem(index$$2, source$$62) {
  if (index$$2 >= 0 ? index$$2 < source$$62.length : false) {
    return source$$62[index$$2];
  } else {
    return null;
  }
}

function String$$$rev(source$$63) {
  var array$$4;
  return (array$$4 = source$$63.split(""), ((0, _Array.reverse)(array$$4, Array))).join("");
}

function String$$$take(count$$1, source$$64) {
  return source$$64.slice(null, count$$1 - 1 + 1);
}

function String$$$skip(count$$2, source$$65) {
  return source$$65.slice(count$$2, source$$65.length);
}

function String$$$takeWhile(predicate$$2, source$$66) {
  if ((0, _String.isNullOrEmpty)(source$$66)) {
    return "";
  } else {
    let i$$6 = 0;
    const length = source$$66.length | 0;

    while (i$$6 < length ? predicate$$2(source$$66[i$$6]) : false) {
      i$$6 = i$$6 + 1;
    }

    if (i$$6 === 0) {
      return "";
    } else {
      return String$$$take(i$$6, source$$66);
    }
  }
}

function String$$$skipWhile(predicate$$3, source$$68) {
  if ((0, _String.isNullOrEmpty)(source$$68)) {
    return "";
  } else {
    let i$$7 = 0;
    const length$$1 = source$$68.length | 0;

    while (i$$7 < length$$1 ? predicate$$3(source$$68[i$$7]) : false) {
      i$$7 = i$$7 + 1;
    }

    if (i$$7 === 0) {
      return "";
    } else {
      return String$$$skip(i$$7, source$$68);
    }
  }
}

function String$$$truncate(count$$3, source$$70) {
  if (count$$3 < 1) {
    return "";
  } else if (source$$70.length <= count$$3) {
    return source$$70;
  } else {
    return String$$$take(count$$3, source$$70);
  }
}

function String$$$drop(count$$4, source$$71) {
  if (count$$4 < 1) {
    return source$$71;
  } else if (source$$71.length >= count$$4) {
    return "";
  } else {
    return String$$$skip(count$$4, source$$71);
  }
}

function String$$$findIndex(predicate$$4, source$$72) {
  const go = function go($index$$3$$183) {
    go: while (true) {
      const index$$3 = $index$$3$$183;

      if (index$$3 >= source$$72.length) {
        const exn = new Error("An index satisfying the predicate was not found in the string.");
        throw exn;
      } else if (predicate$$4(source$$72[index$$3])) {
        return index$$3 | 0;
      } else {
        $index$$3$$183 = index$$3 + 1;
        continue go;
      }

      break;
    }
  };

  return go(0) | 0;
}

function String$$$tryFindIndex(predicate$$5, source$$73) {
  const go$$1 = function go$$1($index$$4$$186) {
    go$$1: while (true) {
      const index$$4 = $index$$4$$186;

      if (index$$4 >= source$$73.length) {
        return null;
      } else if (predicate$$5(source$$73[index$$4])) {
        return index$$4;
      } else {
        $index$$4$$186 = index$$4 + 1;
        continue go$$1;
      }

      break;
    }
  };

  return go$$1(0);
}

function String$$$findSliceIndex(slice, source$$74) {
  const index$$5 = source$$74.indexOf(slice) | 0;

  if (index$$5 === -1) {
    const exn$$1 = new Error("The specified substring was not found in the string.");
    throw exn$$1;
  } else {
    return index$$5 | 0;
  }
}

function String$$$tryFindSliceIndex(slice$$1, source$$75) {
  const index$$6 = source$$75.indexOf(slice$$1) | 0;

  if (index$$6 === -1) {
    return null;
  } else {
    return index$$6;
  }
}

function String$$$getBytes(encoding, source$$76) {
  return encoding.getBytes(source$$76);
}

function IReadOnlyCollection$$$ofArray(source$$77) {
  return source$$77;
}

function IReadOnlyCollection$$$ofList(source$$78) {
  return source$$78;
}

function IReadOnlyCollection$$$ofSeq(source$$79) {
  return (0, _Array.ofSeq)(source$$79, Array);
}

function IReadOnlyCollection$$$map(mapping$$4, source$$81) {
  var source$$82;
  return source$$82 = (0, _Seq.map)(mapping$$4, source$$81), ((0, _Array.ofSeq)(source$$82, Array));
}

function IReadOnlyList$$$toArray(source$$83) {
  return (0, _Array.ofSeq)(source$$83, Array);
}

function IReadOnlyList$$$tryItem(i$$8, source$$84) {
  if (0 <= i$$8 ? i$$8 < source$$84.Count : false) {
    return (0, _Option.some)(source$$84.Item(i$$8));
  } else {
    return null;
  }
}

function Map$$$keys(source$$85) {
  return (0, _Seq.map)(function (_arg1$$6) {
    const activePatternResult425 = _arg1$$6;
    return activePatternResult425[0];
  }, source$$85);
}

function Map$$$values(source$$86) {
  return (0, _Seq.map)(function (_arg1$$7) {
    const activePatternResult429 = _arg1$$7;
    return activePatternResult429[1];
  }, source$$86);
}

function Map$$$mapValues(f$$12, x$$23) {
  return (0, _Map.map)((0, _Util.uncurry)(2, function (_arg1$$8) {
    return f$$12;
  }), x$$23);
}

function Map$$$mapValues2(f$$13, x$$24, y$$4) {
  const arg00$$1 = (0, _Seq.delay)(function () {
    return (0, _Seq.collect)(function (matchValue$$7) {
      const activePatternResult438 = matchValue$$7;
      const matchValue$$8 = (0, _Map.tryFind)(activePatternResult438[0], y$$4);

      if (matchValue$$8 == null) {
        return (0, _Seq.empty)();
      } else {
        const vy = (0, _Option.value)(matchValue$$8);
        return (0, _Seq.singleton)([activePatternResult438[0], (0, _Util.curry)(2, f$$13)(activePatternResult438[1], vy)]);
      }
    }, x$$24);
  });
  return (0, _Map.ofSeq)(arg00$$1, {
    Compare: _Util.compare
  });
}

function Map$$$zip(x$$25, y$$5) {
  const arg00$$2 = (0, _Seq.delay)(function () {
    return (0, _Seq.collect)(function (matchValue$$9) {
      const activePatternResult446 = matchValue$$9;
      const matchValue$$10 = (0, _Map.tryFind)(activePatternResult446[0], y$$5);

      if (matchValue$$10 == null) {
        return (0, _Seq.empty)();
      } else {
        const vy$$1 = (0, _Option.value)(matchValue$$10);
        return (0, _Seq.singleton)([activePatternResult446[0], [activePatternResult446[1], vy$$1]]);
      }
    }, x$$25);
  });
  return (0, _Map.ofSeq)(arg00$$2, {
    Compare: _Util.compare
  });
}

function Map$$$unzip(source$$87) {
  return [Map$$$mapValues(function (tuple) {
    return tuple[0];
  }, source$$87), Map$$$mapValues(function (tuple$$1) {
    return tuple$$1[1];
  }, source$$87)];
}

function Map$$$unionWith(combiner, source1, source2) {
  return (0, _Map.fold)(function (m, k$$3, v$0027) {
    var matchValue$$11, v$$6;
    return (0, _Map.add)(k$$3, (matchValue$$11 = (0, _Map.tryFind)(k$$3, m), matchValue$$11 == null ? v$0027 : (v$$6 = (0, _Option.value)(matchValue$$11), combiner(v$$6, v$0027))), m);
  }, source1, source2);
}

function Map$$$union(source$$88, altSource) {
  return Map$$$unionWith(function (x$$26, _arg1$$9) {
    return x$$26;
  }, source$$88, altSource);
}

function Dict$$$tryGetValue(k$$4, dct) {
  const matchValue$$12 = (0, _Util.tryGetValue)(dct, k$$4, null);

  if (matchValue$$12[0]) {
    return (0, _Option.some)(matchValue$$12[1]);
  } else {
    return null;
  }
}

function Dict$$$containsKey(k$$5, dct$$1) {
  return dct$$1.has(k$$5);
}

function Dict$$$keys(source$$89) {
  return (0, _Seq.map)(function (_arg1$$10) {
    const activePatternResult472 = _arg1$$10;
    return activePatternResult472[0];
  }, source$$89);
}

function Dict$$$values(source$$90) {
  return (0, _Seq.map)(function (_arg1$$11) {
    const activePatternResult476 = _arg1$$11;
    return activePatternResult476[1];
  }, source$$90);
}

function Dict$$$map(f$$15, x$$27) {
  const dct$$2 = (0, _Map.createMutable)([], {
    Equals: _Util.equals,
    GetHashCode: _Util.structuralHash
  });
  (0, _Seq.iterate)(function (forLoopVar) {
    const activePatternResult480 = forLoopVar;
    (0, _Util.addToDict)(dct$$2, activePatternResult480[0], f$$15(activePatternResult480[1]));
  }, x$$27);
  return dct$$2;
}

function Dict$$$map2(f$$16, x$$28, y$$6) {
  const dct$$3 = (0, _Map.createMutable)([], {
    Equals: _Util.equals,
    GetHashCode: _Util.structuralHash
  });
  (0, _Seq.iterate)(function (forLoopVar$$1) {
    const activePatternResult488 = forLoopVar$$1;
    const matchValue$$13 = Dict$$$tryGetValue(activePatternResult488[0], y$$6);

    if (matchValue$$13 == null) {} else {
      const vy$$2 = (0, _Option.value)(matchValue$$13);
      (0, _Util.addToDict)(dct$$3, activePatternResult488[0], (0, _Util.curry)(2, f$$16)(activePatternResult488[1], vy$$2));
    }
  }, x$$28);
  return dct$$3;
}

function Dict$$$zip(x$$29, y$$7) {
  const dct$$4 = (0, _Map.createMutable)([], {
    Equals: _Util.equals,
    GetHashCode: _Util.structuralHash
  });
  (0, _Seq.iterate)(function (forLoopVar$$2) {
    const activePatternResult497 = forLoopVar$$2;
    const matchValue$$14 = Dict$$$tryGetValue(activePatternResult497[0], y$$7);

    if (matchValue$$14 == null) {} else {
      const vy$$3 = (0, _Option.value)(matchValue$$14);
      (0, _Util.addToDict)(dct$$4, activePatternResult497[0], [activePatternResult497[1], vy$$3]);
    }
  }, x$$29);
  return dct$$4;
}

function Dict$$$unzip(source$$91) {
  const dct1 = (0, _Map.createMutable)([], {
    Equals: _Util.equals,
    GetHashCode: _Util.structuralHash
  });
  const dct2 = (0, _Map.createMutable)([], {
    Equals: _Util.equals,
    GetHashCode: _Util.structuralHash
  });
  (0, _Seq.iterate)(function (forLoopVar$$3) {
    const activePatternResult505 = forLoopVar$$3;
    (0, _Util.addToDict)(dct1, activePatternResult505[0], activePatternResult505[1][0]);
    (0, _Util.addToDict)(dct2, activePatternResult505[0], activePatternResult505[1][1]);
  }, source$$91);
  return [dct1, dct2];
}

function Dict$$$unionWith(combiner$$1, source1$$1, source2$$1) {
  const d = (0, _Map.createMutable)([], {
    Equals: _Util.equals,
    GetHashCode: _Util.structuralHash
  });
  (0, _Seq.iterate)(function (forLoopVar$$4) {
    const activePatternResult512 = forLoopVar$$4;
    d.set(activePatternResult512[0], activePatternResult512[1]);
  }, source1$$1);
  (0, _Seq.iterate)(function (forLoopVar$$5) {
    var matchValue$$15;
    const activePatternResult517 = forLoopVar$$5;
    d.set(activePatternResult517[0], (matchValue$$15 = (0, _Util.tryGetValue)(d, activePatternResult517[0], null), matchValue$$15[0] ? (0, _Util.curry)(2, combiner$$1)(matchValue$$15[1], activePatternResult517[1]) : activePatternResult517[1]));
  }, source2$$1);
  return d;
}

function IReadOnlyDictionary$$$add(key$$1, value, table) {
  var table$$1, arg00$$3;
  return table$$1 = (arg00$$3 = ((0, _Seq.map)(function mapping$$5(keyValuePair) {
    return keyValuePair;
  }, table)), ((0, _Map.ofSeq)(arg00$$3, {
    Compare: _Util.compare
  }))), ((0, _Map.add)(key$$1, value, table$$1));
}

function IReadOnlyDictionary$$$tryGetValue(k$$13, dct$$5) {
  const matchValue$$16 = dct$$5.TryGetValue(k$$13, null);

  if (matchValue$$16[0]) {
    return (0, _Option.some)(matchValue$$16[1]);
  } else {
    return null;
  }
}

function IReadOnlyDictionary$$$containsKey(k$$14, dct$$6) {
  return dct$$6.ContainsKey(k$$14);
}

function IReadOnlyDictionary$$$keys(source$$93) {
  return (0, _Seq.map)(function (_arg1$$12) {
    const activePatternResult537 = _arg1$$12;
    return activePatternResult537[0];
  }, source$$93);
}

function IReadOnlyDictionary$$$values(source$$94) {
  return (0, _Seq.map)(function (_arg1$$13) {
    const activePatternResult541 = _arg1$$13;
    return activePatternResult541[1];
  }, source$$94);
}

function IReadOnlyDictionary$$$map(f$$19, x$$30) {
  const dct$$7 = (0, _Map.createMutable)([], {
    Equals: _Util.equals,
    GetHashCode: _Util.structuralHash
  });
  (0, _Seq.iterate)(function (forLoopVar$$6) {
    const activePatternResult545 = forLoopVar$$6;
    (0, _Util.addToDict)(dct$$7, activePatternResult545[0], f$$19(activePatternResult545[1]));
  }, x$$30);
  return dct$$7;
}

function IReadOnlyDictionary$$$map2(f$$20, x$$31, y$$8) {
  const dct$$8 = (0, _Map.createMutable)([], {
    Equals: _Util.equals,
    GetHashCode: _Util.structuralHash
  });
  (0, _Seq.iterate)(function (forLoopVar$$7) {
    const activePatternResult553 = forLoopVar$$7;
    const matchValue$$17 = IReadOnlyDictionary$$$tryGetValue(activePatternResult553[0], y$$8);

    if (matchValue$$17 == null) {} else {
      const vy$$5 = (0, _Option.value)(matchValue$$17);
      (0, _Util.addToDict)(dct$$8, activePatternResult553[0], (0, _Util.curry)(2, f$$20)(activePatternResult553[1], vy$$5));
    }
  }, x$$31);
  return dct$$8;
}

function IReadOnlyDictionary$$$zip(x$$32, y$$9) {
  const dct$$9 = (0, _Map.createMutable)([], {
    Equals: _Util.equals,
    GetHashCode: _Util.structuralHash
  });
  (0, _Seq.iterate)(function (forLoopVar$$8) {
    const activePatternResult562 = forLoopVar$$8;
    const matchValue$$18 = IReadOnlyDictionary$$$tryGetValue(activePatternResult562[0], y$$9);

    if (matchValue$$18 == null) {} else {
      const vy$$6 = (0, _Option.value)(matchValue$$18);
      (0, _Util.addToDict)(dct$$9, activePatternResult562[0], [activePatternResult562[1], vy$$6]);
    }
  }, x$$32);
  return dct$$9;
}

function IReadOnlyDictionary$$$unzip(source$$95) {
  const dct1$$1 = (0, _Map.createMutable)([], {
    Equals: _Util.equals,
    GetHashCode: _Util.structuralHash
  });
  const dct2$$1 = (0, _Map.createMutable)([], {
    Equals: _Util.equals,
    GetHashCode: _Util.structuralHash
  });
  (0, _Seq.iterate)(function (forLoopVar$$9) {
    const activePatternResult570 = forLoopVar$$9;
    (0, _Util.addToDict)(dct1$$1, activePatternResult570[0], activePatternResult570[1][0]);
    (0, _Util.addToDict)(dct2$$1, activePatternResult570[0], activePatternResult570[1][1]);
  }, source$$95);
  return [dct1$$1, dct2$$1];
}

function IReadOnlyDictionary$$$unionWith(combiner$$2, source1$$2, source2$$2) {
  const d$$1 = (0, _Map.createMutable)([], {
    Equals: _Util.equals,
    GetHashCode: _Util.structuralHash
  });
  (0, _Seq.iterate)(function (forLoopVar$$10) {
    const activePatternResult577 = forLoopVar$$10;
    d$$1.set(activePatternResult577[0], activePatternResult577[1]);
  }, source1$$2);
  (0, _Seq.iterate)(function (forLoopVar$$11) {
    var matchValue$$19;
    const activePatternResult582 = forLoopVar$$11;
    d$$1.set(activePatternResult582[0], (matchValue$$19 = (0, _Util.tryGetValue)(d$$1, activePatternResult582[0], null), matchValue$$19[0] ? (0, _Util.curry)(2, combiner$$2)(matchValue$$19[1], activePatternResult582[1]) : activePatternResult582[1]));
  }, source2$$2);
  return d$$1;
}

function Async$$$map(f$$23, x$$33) {
  return _AsyncBuilder.singleton.Bind(x$$33, function ($arg$$33) {
    const arg00$$4 = f$$23($arg$$33);
    return _AsyncBuilder.singleton.Return(arg00$$4);
  });
}

function Async$$$map2(f$$24, x$$34, y$$10) {
  return _AsyncBuilder.singleton.Delay(function () {
    return _AsyncBuilder.singleton.Bind(x$$34, function (_arg1$$14) {
      return _AsyncBuilder.singleton.Bind(y$$10, function (_arg2) {
        return _AsyncBuilder.singleton.Return(f$$24(_arg1$$14, _arg2));
      });
    });
  });
}

function Async$$$zip(x$$35, y$$11) {
  return _AsyncBuilder.singleton.Delay(function () {
    return _AsyncBuilder.singleton.Bind(x$$35, function (_arg1$$15) {
      return _AsyncBuilder.singleton.Bind(y$$11, function (_arg2$$1) {
        return _AsyncBuilder.singleton.Return([_arg1$$15, _arg2$$1]);
      });
    });
  });
}

function Async$$$join(x$$36) {
  return _AsyncBuilder.singleton.Bind(x$$36, function (x$$37) {
    return x$$37;
  });
}

function Async$$$apply(f$$25, x$$38) {
  return _AsyncBuilder.singleton.Bind(f$$25, function (x1) {
    return _AsyncBuilder.singleton.Bind(x$$38, function (x2) {
      return _AsyncBuilder.singleton.Delay(function () {
        return _AsyncBuilder.singleton.Return(x1(x2));
      });
    });
  });
}

function Async$$$raise(ex) {
  return (0, _Async.fromContinuations)(function (tupledArg) {
    tupledArg[1](ex);
  });
}

function System$002ECollections$002EGeneric$002EIEnumerable$00601$$IEnumerable$00601$002Eget_GetSlice(this$) {
  return function (_arg1$$17) {
    if (_arg1$$17[0] != null) {
      if (_arg1$$17[1] != null) {
        const a$$5 = _arg1$$17[0] | 0;
        const b$$5 = _arg1$$17[1] | 0;
        let source$$99;
        source$$99 = (0, _Seq.skip)(a$$5, this$);
        const count$$5 = b$$5 - a$$5 + 1 | 0;
        return (0, _Seq.take)(count$$5, source$$99);
      } else {
        const a$$4 = _arg1$$17[0] | 0;
        return (0, _Seq.skip)(a$$4, this$);
      }
    } else if (_arg1$$17[1] != null) {
      const b$$4 = _arg1$$17[1] | 0;
      return (0, _Seq.take)(b$$4, this$);
    } else {
      return this$;
    }
  };
}

function Microsoft$002EFSharp$002ECollections$002EFSharpList$00601$$List$00601$002Eget_GetSlice(this$$$1) {
  return function (_arg1$$18) {
    var b$$8, a$$9, b$$6, a$$6;
    var $target$$267, a$$7;

    if (_arg1$$18[0] != null) {
      if (_arg1$$18[1] == null) {
        if (a$$6 = _arg1$$18[0] | 0, a$$6 < 0) {
          $target$$267 = 1;
          a$$7 = _arg1$$18[0];
        } else {
          $target$$267 = 2;
        }
      } else {
        $target$$267 = 2;
      }
    } else if (_arg1$$18[1] == null) {
      $target$$267 = 0;
    } else {
      $target$$267 = 2;
    }

    switch ($target$$267) {
      case 0:
        {
          return this$$$1;
        }

      case 1:
        {
          const i$$9 = (0, _List.length)(this$$$1) + a$$7 | 0;
          return List$$$skip(i$$9, this$$$1);
        }

      case 2:
        {
          var $target$$268, a$$8, b$$7;

          if (_arg1$$18[0] == null) {
            if (_arg1$$18[1] != null) {
              if (b$$6 = _arg1$$18[1] | 0, b$$6 < 0) {
                $target$$268 = 1;
                b$$7 = _arg1$$18[1];
              } else {
                $target$$268 = 2;
              }
            } else {
              $target$$268 = 2;
            }
          } else if (_arg1$$18[1] == null) {
            $target$$268 = 0;
            a$$8 = _arg1$$18[0];
          } else {
            $target$$268 = 2;
          }

          switch ($target$$268) {
            case 0:
              {
                return List$$$skip(a$$8, this$$$1);
              }

            case 1:
              {
                const i$$10 = (0, _List.length)(this$$$1) + b$$7 | 0;
                return List$$$take(i$$10, this$$$1);
              }

            case 2:
              {
                var $target$$269, b$$9, a$$10, b$$10;

                if (_arg1$$18[0] != null) {
                  if (_arg1$$18[1] != null) {
                    if (b$$8 = _arg1$$18[1] | 0, (a$$9 = _arg1$$18[0] | 0, a$$9 >= 0 ? b$$8 >= 0 : false)) {
                      $target$$269 = 1;
                      a$$10 = _arg1$$18[0];
                      b$$10 = _arg1$$18[1];
                    } else {
                      $target$$269 = 2;
                    }
                  } else {
                    $target$$269 = 2;
                  }
                } else if (_arg1$$18[1] != null) {
                  $target$$269 = 0;
                  b$$9 = _arg1$$18[1];
                } else {
                  $target$$269 = 2;
                }

                switch ($target$$269) {
                  case 0:
                    {
                      return List$$$take(b$$9, this$$$1);
                    }

                  case 1:
                    {
                      let list$$21;
                      list$$21 = List$$$skip(a$$10, this$$$1);
                      return List$$$take(b$$10, list$$21);
                    }

                  case 2:
                    {
                      var $target$$270, a$$11, b$$11;

                      if (_arg1$$18[0] != null) {
                        if (_arg1$$18[1] != null) {
                          $target$$270 = 0;
                          a$$11 = _arg1$$18[0];
                          b$$11 = _arg1$$18[1];
                        } else {
                          $target$$270 = 1;
                        }
                      } else {
                        $target$$270 = 1;
                      }

                      switch ($target$$270) {
                        case 0:
                          {
                            const l = (0, _List.length)(this$$$1) | 0;

                            const f$$26 = function f$$26(i$$11) {
                              if (i$$11 < 0) {
                                return l + i$$11 | 0;
                              } else {
                                return i$$11 | 0;
                              }
                            };

                            const a$$12 = f$$26(a$$11) | 0;
                            let list$$23;
                            list$$23 = List$$$skip(a$$12, this$$$1);
                            const i$$12 = f$$26(b$$11) - a$$12 + 1 | 0;
                            return List$$$take(i$$12, list$$23);
                          }

                        case 1:
                          {
                            throw new Error("The match cases were incomplete against type of 'Option' at /Users/jorgebotto/github/FSharpPlus/src/FSharpPlus/Extensions.fs");
                          }
                      }
                    }
                }
              }
          }
        }
    }
  };
}

function Microsoft$002EFSharp$002EControl$002EFSharpAsync$00601$$Async$00601$002ESequence$002EStatic$$65E6EDE0(t) {
  return _AsyncBuilder.singleton.Delay(function () {
    return _AsyncBuilder.singleton.Bind((0, _Async.cancellationToken)(), function (_arg1$$19) {
      return _AsyncBuilder.singleton.Return((0, _Seq.delay)(function () {
        return (0, _Seq.enumerateUsing)((0, _Seq.getEnumerator)(t), function (enum$) {
          return (0, _Seq.enumerateWhile)(function () {
            return enum$.MoveNext();
          }, (0, _Seq.delay)(function () {
            return (0, _Seq.singleton)((0, _Async.runSynchronously)(enum$.Current, null, _arg1$$19));
          }));
        });
      }));
    });
  });
}

function Microsoft$002EFSharp$002EControl$002EFSharpAsync$00601$$Async$00601$002ESequence$002EStatic$$Z3B031D5B(t$$1) {
  const loop$$2 = function loop$$2(acc, _arg2$$3) {
    if (_arg2$$3.tail != null) {
      return _AsyncBuilder.singleton.Bind(_arg2$$3.head, function (x$$40) {
        return loop$$2(new _Types.List(x$$40, acc), _arg2$$3.tail);
      });
    } else {
      return _AsyncBuilder.singleton.Return((0, _List.reverse)(acc));
    }
  };

  return loop$$2(new _Types.List(), t$$1);
}

function Microsoft$002EFSharp$002EControl$002EFSharpAsync$00601$$Async$00601$002ESequence$002EStatic$$75C1C4A9(t$$2) {
  return _AsyncBuilder.singleton.Delay(function () {
    const siz = t$$2.length | 0;
    const arr = (0, _Array.fill)(new Array(siz), 0, siz, null);
    return _AsyncBuilder.singleton.Combine(_AsyncBuilder.singleton.For((0, _Seq.rangeNumber)(0, 1, siz - 1), function (_arg3) {
      return _AsyncBuilder.singleton.Bind(t$$2[_arg3], function (_arg4) {
        arr[_arg3] = _arg4;
        return _AsyncBuilder.singleton.Zero();
      });
    }), _AsyncBuilder.singleton.Delay(function () {
      return _AsyncBuilder.singleton.Return(arr);
    }));
  });
}

function Microsoft$002EFSharp$002ECore$002EFSharpOption$00601$$Option$00601$002ESequence$002EStatic$$19140039(t$$3) {
  let ok = true;
  const res = (0, _Array.ofSeq)((0, _Seq.delay)(function () {
    return (0, _Seq.enumerateUsing)((0, _Seq.getEnumerator)(t$$3), function (e$$8) {
      return (0, _Seq.enumerateWhile)(function () {
        return e$$8.MoveNext() ? ok : false;
      }, (0, _Seq.delay)(function () {
        const matchValue$$21 = e$$8.Current;

        if (matchValue$$21 == null) {
          ok = false;
          return (0, _Seq.empty)();
        } else {
          const v$$18 = (0, _Option.value)(matchValue$$21);
          return (0, _Seq.singleton)(v$$18);
        }
      }));
    });
  }), Array);

  if (ok) {
    return res;
  } else {
    return null;
  }
}