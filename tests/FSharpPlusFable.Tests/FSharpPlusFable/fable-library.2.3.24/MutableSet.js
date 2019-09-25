"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.MutableSet$00601$reflection = MutableSet$00601$reflection;
exports.MutableSet$00601$$$$002Ector$$Z79760D57 = MutableSet$00601$$$$002Ector$$Z79760D57;
exports.MutableSet$00601$$$$002Ector = MutableSet$00601$$$$002Ector;
exports.MutableSet$00601$$get_Comparer = MutableSet$00601$$get_Comparer;
exports.MutableSet$00601$$Clear = MutableSet$00601$$Clear;
exports.MutableSet$00601$$get_Count = MutableSet$00601$$get_Count;
exports.MutableSet$00601$$Add$$2B595 = MutableSet$00601$$Add$$2B595;
exports.MutableSet$00601$$Contains$$2B595 = MutableSet$00601$$Contains$$2B595;
exports.MutableSet$00601$$Remove$$2B595 = MutableSet$00601$$Remove$$2B595;
exports.MutableSet$00601 = void 0;

var _Types = require("./Types");

var _Reflection = require("./Reflection");

var _SystemCollections = require("./System.Collections.Generic");

var _Util = require("./Util");

var _Option = require("./Option");

var _Seq = require("./Seq");

const MutableSet$00601 = (0, _Types.declare)(function Fable_Collections_MutableSet(comparer) {
  const $this$$1 = this;
  $this$$1.comparer = comparer;
  $this$$1.entries = new Map([]);
});
exports.MutableSet$00601 = MutableSet$00601;

function MutableSet$00601$reflection($gen$$4) {
  return (0, _Reflection.type)("Fable.Collections.MutableSet`1", [$gen$$4]);
}

function MutableSet$00601$$$$002Ector$$Z79760D57(comparer) {
  return this instanceof MutableSet$00601 ? MutableSet$00601.call(this, comparer) : new MutableSet$00601(comparer);
}

function MutableSet$00601$$$$002Ector() {
  return MutableSet$00601$$$$002Ector$$Z79760D57.call(this, (0, _SystemCollections.EqualityComparer$00601$$$get_Default)());
}

function MutableSet$00601$$TryFindIndex$$2B595(this$, k) {
  const h = this$.comparer.GetHashCode(k) | 0;
  const matchValue = (0, _Util.tryGetValue)(this$.entries, h, null);

  if (matchValue[0]) {
    return [true, h, matchValue[1].findIndex(function (v) {
      return this$.comparer.Equals(k, v);
    })];
  } else {
    return [false, h, -1];
  }
}

function MutableSet$00601$$TryFind$$2B595(this$$$1, k$$1) {
  const matchValue$$1 = MutableSet$00601$$TryFindIndex$$2B595(this$$$1, k$$1);
  var $target$$9;

  if (matchValue$$1[0]) {
    if (matchValue$$1[2] > -1) {
      $target$$9 = 0;
    } else {
      $target$$9 = 1;
    }
  } else {
    $target$$9 = 1;
  }

  switch ($target$$9) {
    case 0:
      {
        return (0, _Option.some)((0, _Util.getItemFromDict)(this$$$1.entries, matchValue$$1[1])[matchValue$$1[2]]);
      }

    case 1:
      {
        return null;
      }
  }
}

function MutableSet$00601$$get_Comparer(this$$$2) {
  return this$$$2.comparer;
}

function MutableSet$00601$$Clear(this$$$3) {
  this$$$3.entries.clear();
}

function MutableSet$00601$$get_Count(this$$$4) {
  const source = this$$$4.entries.values();
  return (0, _Seq.sumBy)(function projection(pairs) {
    return pairs.length;
  }, source, {
    GetZero() {
      return 0;
    },

    Add($x$$2, $y$$3) {
      return $x$$2 + $y$$3;
    }

  }) | 0;
}

function MutableSet$00601$$Add$$2B595(this$$$5, k$$2) {
  const matchValue$$2 = MutableSet$00601$$TryFindIndex$$2B595(this$$$5, k$$2);
  var $target$$16;

  if (matchValue$$2[0]) {
    if (matchValue$$2[2] > -1) {
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
        return false;
      }

    case 1:
      {
        if (matchValue$$2[0]) {
          const value = (0, _Util.getItemFromDict)(this$$$5.entries, matchValue$$2[1]).push(k$$2);
          null, null;
          return true;
        } else {
          this$$$5.entries.set(matchValue$$2[1], [k$$2]);
          return true;
        }
      }
  }
}

function MutableSet$00601$$Contains$$2B595(this$$$6, k$$3) {
  const matchValue$$3 = MutableSet$00601$$TryFindIndex$$2B595(this$$$6, k$$3);
  var $target$$19;

  if (matchValue$$3[0]) {
    if (matchValue$$3[2] > -1) {
      $target$$19 = 0;
    } else {
      $target$$19 = 1;
    }
  } else {
    $target$$19 = 1;
  }

  switch ($target$$19) {
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

function MutableSet$00601$$Remove$$2B595(this$$$7, k$$4) {
  const matchValue$$4 = MutableSet$00601$$TryFindIndex$$2B595(this$$$7, k$$4);
  var $target$$22;

  if (matchValue$$4[0]) {
    if (matchValue$$4[2] > -1) {
      $target$$22 = 0;
    } else {
      $target$$22 = 1;
    }
  } else {
    $target$$22 = 1;
  }

  switch ($target$$22) {
    case 0:
      {
        (0, _Util.getItemFromDict)(this$$$7.entries, matchValue$$4[1]).splice(matchValue$$4[2], 1);
        return true;
      }

    case 1:
      {
        return false;
      }
  }
}

MutableSet$00601.prototype[Symbol.iterator] = function () {
  var elems;
  const this$$$8 = this;
  return (0, _Seq.toIterator)((elems = (0, _Seq.delay)(function () {
    return (0, _Seq.collect)(function (values$$1) {
      return (0, _Seq.map)(function (value$$1) {
        return value$$1;
      }, values$$1);
    }, this$$$8.entries.values());
  }), (0, _Seq.getEnumerator)(elems)));
};

MutableSet$00601.prototype.Add = function (item) {
  const this$$$9 = this;
  const value$$2 = MutableSet$00601$$Add$$2B595(this$$$9, item);
  value$$2, null;
};

MutableSet$00601.prototype.Clear = function () {
  const this$$$10 = this;
  MutableSet$00601$$Clear(this$$$10);
};

MutableSet$00601.prototype.Contains = function (item$$1) {
  const this$$$11 = this;
  return MutableSet$00601$$Contains$$2B595(this$$$11, item$$1);
};

MutableSet$00601.prototype.CopyTo = function (array, arrayIndex) {
  const this$$$12 = this;
  (0, _Seq.iterateIndexed)(function action(i$$8, e) {
    array[arrayIndex + i$$8] = e;
  }, this$$$12);
};

Object.defineProperty(MutableSet$00601.prototype, "Count", {
  "get": function () {
    const this$$$13 = this;
    return MutableSet$00601$$get_Count(this$$$13) | 0;
  }
});
Object.defineProperty(MutableSet$00601.prototype, "IsReadOnly", {
  "get": function () {
    return false;
  }
});

MutableSet$00601.prototype.Remove = function (item$$2) {
  const this$$$15 = this;
  return MutableSet$00601$$Remove$$2B595(this$$$15, item$$2);
};

Object.defineProperty(MutableSet$00601.prototype, "size", {
  "get": function () {
    const this$$$16 = this;
    return MutableSet$00601$$get_Count(this$$$16) | 0;
  }
});

MutableSet$00601.prototype.add = function (k$$5) {
  const this$$$17 = this;
  const value$$3 = MutableSet$00601$$Add$$2B595(this$$$17, k$$5);
  value$$3, null;
  return this$$$17;
};

MutableSet$00601.prototype.add_ = function (k$$6) {
  const this$$$18 = this;
  return MutableSet$00601$$Add$$2B595(this$$$18, k$$6);
};

MutableSet$00601.prototype.clear = function () {
  const this$$$19 = this;
  MutableSet$00601$$Clear(this$$$19);
};

MutableSet$00601.prototype.delete = function (k$$7) {
  const this$$$20 = this;
  return MutableSet$00601$$Remove$$2B595(this$$$20, k$$7);
};

MutableSet$00601.prototype.has = function (k$$8) {
  const this$$$21 = this;
  return MutableSet$00601$$Contains$$2B595(this$$$21, k$$8);
};

MutableSet$00601.prototype.values = function () {
  const this$$$22 = this;
  return (0, _Seq.map)(function mapping(x) {
    return x;
  }, this$$$22);
};