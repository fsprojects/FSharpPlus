"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.MutableMap$00602$reflection = MutableMap$00602$reflection;
exports.MutableMap$00602$$$$002Ector$$Z79760D57 = MutableMap$00602$$$$002Ector$$Z79760D57;
exports.MutableMap$00602$$$$002Ector = MutableMap$00602$$$$002Ector;
exports.MutableMap$00602$$TryFind$$2B595 = MutableMap$00602$$TryFind$$2B595;
exports.MutableMap$00602$$get_Comparer = MutableMap$00602$$get_Comparer;
exports.MutableMap$00602$$Clear = MutableMap$00602$$Clear;
exports.MutableMap$00602$$get_Count = MutableMap$00602$$get_Count;
exports.MutableMap$00602$$get_Item$$2B595 = MutableMap$00602$$get_Item$$2B595;
exports.MutableMap$00602$$set_Item$$5BDDA1 = MutableMap$00602$$set_Item$$5BDDA1;
exports.MutableMap$00602$$Add$$5BDDA1 = MutableMap$00602$$Add$$5BDDA1;
exports.MutableMap$00602$$ContainsKey$$2B595 = MutableMap$00602$$ContainsKey$$2B595;
exports.MutableMap$00602$$Remove$$2B595 = MutableMap$00602$$Remove$$2B595;
exports.MutableMap$00602 = void 0;

var _Types = require("./Types");

var _Reflection = require("./Reflection");

var _SystemCollections = require("./System.Collections.Generic");

var _Util = require("./Util");

var _Seq = require("./Seq");

var _String = require("./String");

const MutableMap$00602 = (0, _Types.declare)(function Fable_Collections_MutableMap(comparer) {
  const $this$$1 = this;
  $this$$1.comparer = comparer;
  $this$$1["entries@23"] = new Map([]);
});
exports.MutableMap$00602 = MutableMap$00602;

function MutableMap$00602$reflection($gen$$4, $gen$$5) {
  return (0, _Reflection.type)("Fable.Collections.MutableMap`2", [$gen$$4, $gen$$5]);
}

function MutableMap$00602$$$$002Ector$$Z79760D57(comparer) {
  return this instanceof MutableMap$00602 ? MutableMap$00602.call(this, comparer) : new MutableMap$00602(comparer);
}

function MutableMap$00602$$$$002Ector() {
  return MutableMap$00602$$$$002Ector$$Z79760D57.call(this, (0, _SystemCollections.EqualityComparer$00601$$$get_Default)());
}

function MutableMap$00602$$TryFindIndex$$2B595(this$, k) {
  const h = this$.comparer.GetHashCode(k) | 0;
  const matchValue = (0, _Util.tryGetValue)(this$["entries@23"], h, null);

  if (matchValue[0]) {
    return [true, h, matchValue[1].findIndex(function (pair) {
      return this$.comparer.Equals(k, pair[0]);
    })];
  } else {
    return [false, h, -1];
  }
}

function MutableMap$00602$$TryFind$$2B595(this$$$1, k$$1) {
  const matchValue$$1 = MutableMap$00602$$TryFindIndex$$2B595(this$$$1, k$$1);
  var $target$$10;

  if (matchValue$$1[0]) {
    if (matchValue$$1[2] > -1) {
      $target$$10 = 0;
    } else {
      $target$$10 = 1;
    }
  } else {
    $target$$10 = 1;
  }

  switch ($target$$10) {
    case 0:
      {
        return (0, _Util.getItemFromDict)(this$$$1["entries@23"], matchValue$$1[1])[matchValue$$1[2]];
      }

    case 1:
      {
        return null;
      }
  }
}

function MutableMap$00602$$get_Comparer(this$$$2) {
  return this$$$2.comparer;
}

function MutableMap$00602$$Clear(this$$$3) {
  this$$$3["entries@23"].clear();
}

function MutableMap$00602$$get_Count(this$$$4) {
  const source = this$$$4["entries@23"].values();
  return (0, _Seq.sumBy)(function projection(pairs$$1) {
    return pairs$$1.length;
  }, source, {
    GetZero() {
      return 0;
    },

    Add($x$$2, $y$$3) {
      return $x$$2 + $y$$3;
    }

  }) | 0;
}

function MutableMap$00602$$get_Item$$2B595(this$$$5, k$$2) {
  const matchValue$$2 = MutableMap$00602$$TryFind$$2B595(this$$$5, k$$2);

  if (matchValue$$2 != null) {
    const pair$$1 = matchValue$$2;
    return pair$$1[1];
  } else {
    throw new Error("The item was not found in collection");
  }
}

function MutableMap$00602$$set_Item$$5BDDA1(this$$$6, k$$3, v) {
  const matchValue$$3 = MutableMap$00602$$TryFindIndex$$2B595(this$$$6, k$$3);
  var $target$$20;

  if (matchValue$$3[0]) {
    if (matchValue$$3[2] > -1) {
      $target$$20 = 0;
    } else {
      $target$$20 = 1;
    }
  } else {
    $target$$20 = 1;
  }

  switch ($target$$20) {
    case 0:
      {
        (0, _Util.getItemFromDict)(this$$$6["entries@23"], matchValue$$3[1])[matchValue$$3[2]] = [k$$3, v];
        break;
      }

    case 1:
      {
        if (matchValue$$3[0]) {
          const value = (0, _Util.getItemFromDict)(this$$$6["entries@23"], matchValue$$3[1]).push([k$$3, v]);
          null, null;
        } else {
          this$$$6["entries@23"].set(matchValue$$3[1], [[k$$3, v]]);
        }

        break;
      }
  }
}

function MutableMap$00602$$Add$$5BDDA1(this$$$7, k$$4, v$$1) {
  const matchValue$$4 = MutableMap$00602$$TryFindIndex$$2B595(this$$$7, k$$4);
  var $target$$24;

  if (matchValue$$4[0]) {
    if (matchValue$$4[2] > -1) {
      $target$$24 = 0;
    } else {
      $target$$24 = 1;
    }
  } else {
    $target$$24 = 1;
  }

  switch ($target$$24) {
    case 0:
      {
        let msg;
        const clo1 = (0, _String.toText)((0, _String.printf)("An item with the same key has already been added. Key: %A"));
        msg = clo1(k$$4);
        throw new Error(msg);
        break;
      }

    case 1:
      {
        if (matchValue$$4[0]) {
          const value$$1 = (0, _Util.getItemFromDict)(this$$$7["entries@23"], matchValue$$4[1]).push([k$$4, v$$1]);
          null, null;
        } else {
          this$$$7["entries@23"].set(matchValue$$4[1], [[k$$4, v$$1]]);
        }

        break;
      }
  }
}

function MutableMap$00602$$ContainsKey$$2B595(this$$$8, k$$5) {
  const matchValue$$5 = MutableMap$00602$$TryFindIndex$$2B595(this$$$8, k$$5);
  var $target$$27;

  if (matchValue$$5[0]) {
    if (matchValue$$5[2] > -1) {
      $target$$27 = 0;
    } else {
      $target$$27 = 1;
    }
  } else {
    $target$$27 = 1;
  }

  switch ($target$$27) {
    case 0:
      {
        return true;
      }

    case 1:
      {
        return false;
      }
  }
}

function MutableMap$00602$$Remove$$2B595(this$$$9, k$$6) {
  const matchValue$$6 = MutableMap$00602$$TryFindIndex$$2B595(this$$$9, k$$6);
  var $target$$30;

  if (matchValue$$6[0]) {
    if (matchValue$$6[2] > -1) {
      $target$$30 = 0;
    } else {
      $target$$30 = 1;
    }
  } else {
    $target$$30 = 1;
  }

  switch ($target$$30) {
    case 0:
      {
        (0, _Util.getItemFromDict)(this$$$9["entries@23"], matchValue$$6[1]).splice(matchValue$$6[2], 1);
        return true;
      }

    case 1:
      {
        return false;
      }
  }
}

MutableMap$00602.prototype[Symbol.iterator] = function () {
  var elems;
  const this$$$10 = this;
  return (0, _Seq.toIterator)((elems = (0, _Seq.delay)(function () {
    return (0, _Seq.collect)(function (pairs$$2) {
      return (0, _Seq.map)(function (pair$$2) {
        return pair$$2;
      }, pairs$$2);
    }, this$$$10["entries@23"].values());
  }), (0, _Seq.getEnumerator)(elems)));
};

MutableMap$00602.prototype.Add = function (item) {
  const this$$$11 = this;
  MutableMap$00602$$Add$$5BDDA1(this$$$11, item[0], item[1]);
};

MutableMap$00602.prototype.Clear = function () {
  const this$$$12 = this;
  MutableMap$00602$$Clear(this$$$12);
};

MutableMap$00602.prototype.Contains = function (item$$1) {
  var p;
  const this$$$13 = this;
  const matchValue$$7 = MutableMap$00602$$TryFind$$2B595(this$$$13, item$$1[0]);
  var $target$$31;

  if (matchValue$$7 != null) {
    if (p = matchValue$$7, (0, _Util.equals)(p[1], item$$1[1])) {
      $target$$31 = 0;
    } else {
      $target$$31 = 1;
    }
  } else {
    $target$$31 = 1;
  }

  switch ($target$$31) {
    case 0:
      {
        return true;
      }

    case 1:
      {
        return false;
      }
  }
};

MutableMap$00602.prototype.CopyTo = function (array, arrayIndex) {
  const this$$$14 = this;
  (0, _Seq.iterateIndexed)(function action(i$$10, e) {
    array[arrayIndex + i$$10] = e;
  }, this$$$14);
};

Object.defineProperty(MutableMap$00602.prototype, "Count", {
  "get": function () {
    const this$$$15 = this;
    return MutableMap$00602$$get_Count(this$$$15) | 0;
  }
});
Object.defineProperty(MutableMap$00602.prototype, "IsReadOnly", {
  "get": function () {
    return false;
  }
});

MutableMap$00602.prototype.Remove = function (item$$2) {
  const this$$$17 = this;
  const matchValue$$8 = MutableMap$00602$$TryFind$$2B595(this$$$17, item$$2[0]);

  if (matchValue$$8 != null) {
    const pair$$3 = matchValue$$8;

    if ((0, _Util.equals)(pair$$3[1], item$$2[1])) {
      const value$$2 = MutableMap$00602$$Remove$$2B595(this$$$17, item$$2[0]);
      value$$2, null;
    }

    return true;
  } else {
    return false;
  }
};

Object.defineProperty(MutableMap$00602.prototype, "size", {
  "get": function () {
    const this$$$18 = this;
    return MutableMap$00602$$get_Count(this$$$18) | 0;
  }
});

MutableMap$00602.prototype.clear = function () {
  const this$$$19 = this;
  MutableMap$00602$$Clear(this$$$19);
};

MutableMap$00602.prototype.delete = function (k$$7) {
  const this$$$20 = this;
  return MutableMap$00602$$Remove$$2B595(this$$$20, k$$7);
};

MutableMap$00602.prototype.entries = function () {
  const this$$$21 = this;
  return (0, _Seq.map)(function mapping(x) {
    return x;
  }, this$$$21);
};

MutableMap$00602.prototype.get = function (k$$8) {
  const this$$$22 = this;
  return MutableMap$00602$$get_Item$$2B595(this$$$22, k$$8);
};

MutableMap$00602.prototype.has = function (k$$9) {
  const this$$$23 = this;
  return MutableMap$00602$$ContainsKey$$2B595(this$$$23, k$$9);
};

MutableMap$00602.prototype.keys = function () {
  const this$$$24 = this;
  return (0, _Seq.map)(function mapping$$1(pair$$4) {
    return pair$$4[0];
  }, this$$$24);
};

MutableMap$00602.prototype.set = function (k$$10, v$$2) {
  const this$$$25 = this;
  MutableMap$00602$$set_Item$$5BDDA1(this$$$25, k$$10, v$$2);
  return this$$$25;
};

MutableMap$00602.prototype.values = function () {
  const this$$$26 = this;
  return (0, _Seq.map)(function mapping$$2(pair$$5) {
    return pair$$5[1];
  }, this$$$26);
};