"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Internals$$$dimap$0027 = Internals$$$dimap$0027;
exports.Internals$$$getAny = Internals$$$getAny;
exports.Internals$$$getAll = Internals$$$getAll;
exports.Internals$002EExchange$00604$reflection = Internals$002EExchange$00604$reflection;
exports.Internals$002EExchange$00604$$$Dimap$$20DF162A = Internals$002EExchange$00604$$$Dimap$$20DF162A;
exports.setl = setl;
exports.over = over;
exports.view = view;
exports.preview = preview;
exports.foldMapOf = foldMapOf;
exports.foldOf = foldOf;
exports.foldrOf = foldrOf;
exports.foldlOf = foldlOf;
exports.toListOf = toListOf;
exports.maximumOf = maximumOf;
exports.minimumOf = minimumOf;
exports.anyOf = anyOf;
exports.allOf = allOf;
exports.elemOf = elemOf;
exports.op_HatDot = op_HatDot;
exports.op_DotMinusGreater = op_DotMinusGreater;
exports.op_PercentMinusGreater = op_PercentMinusGreater;
exports.op_HatQmark = op_HatQmark;
exports.op_HatDotDot = op_HatDotDot;
exports.Internals$002EExchange$00604 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

var _Identity = require("./Identity");

var _Monoids = require("./Monoids");

var _Option = require("./fable-library.2.3.24/Option");

var _Util = require("./fable-library.2.3.24/Util");

function Internals$$$dimap$0027(ab, cd, p) {
  return function ($arg$$2) {
    return cd((p(ab($arg$$2))));
  };
}

function Internals$$$getAny(_arg1) {
  return _arg1.fields[0];
}

function Internals$$$getAll(_arg1$$1) {
  return _arg1$$1.fields[0];
}

const Internals$002EExchange$00604 = (0, _Types.declare)(function FSharpPlus_Lens_Internals_Exchange(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Internals$002EExchange$00604 = Internals$002EExchange$00604;

function Internals$002EExchange$00604$reflection($gen$$29, $gen$$30, $gen$$31, $gen$$32) {
  return (0, _Reflection.union)("FSharpPlus.Lens.Internals.Exchange`4", [$gen$$29, $gen$$30, $gen$$31, $gen$$32], Internals$002EExchange$00604, () => [["Exchange", [(0, _Reflection.lambda)($gen$$31, $gen$$29), (0, _Reflection.lambda)($gen$$30, $gen$$32)]]]);
}

function Internals$002EExchange$00604$$$Dimap$$20DF162A(_arg1$$2, f, g) {
  return new Internals$002EExchange$00604(0, "Exchange", function ($arg$$3) {
    return _arg1$$2.fields[0](f($arg$$3));
  }, function ($arg$$4) {
    return g(_arg1$$2.fields[1]($arg$$4));
  });
}

function setl(optic, value, source) {
  return (0, _Identity.Identity$$$run)(optic(function (_arg1$$3) {
    return new _Identity.Identity$00601(0, "Identity", value);
  }, source));
}

function over(optic$$1, updater, source$$1) {
  return (0, _Identity.Identity$$$run)(optic$$1(function ($arg$$5) {
    const arg0 = updater($arg$$5);
    return new _Identity.Identity$00601(0, "Identity", arg0);
  }, source$$1));
}

function view(optic$$2, source$$2) {
  return (0, _Monoids.Const$$$run)(optic$$2(function (arg0$$1) {
    return new _Monoids.Const$00602(0, "Const", arg0$$1);
  }, source$$2));
}

function preview(optic$$3, source$$3) {
  let arg00;
  const arg00$0040 = optic$$3(function (x) {
    return new _Monoids.Const$00602(0, "Const", new _Monoids.First$00601(0, "First", (0, _Option.some)(x)));
  }, source$$3);
  arg00 = (0, _Monoids.Const$$$run)(arg00$0040);
  return (0, _Monoids.First$00601$$$run$$Z73EA2E14)(arg00);
}

function foldMapOf(l, f$$1) {
  return function ($arg$$7) {
    const arg00$0040$$1 = l(function ($arg$$6) {
      const arg0$$2 = f$$1($arg$$6);
      return new _Monoids.Const$00602(0, "Const", arg0$$2);
    }, $arg$$7);
    return (0, _Monoids.Const$$$run)(arg00$0040$$1);
  };
}

function foldOf(l$$1) {
  return function ($arg$$8) {
    const arg00$0040$$2 = l$$1(function (arg0$$3) {
      return new _Monoids.Const$00602(0, "Const", arg0$$3);
    }, $arg$$8);
    return (0, _Monoids.Const$$$run)(arg00$0040$$2);
  };
}

function foldrOf(l$$2, f$$2, z) {
  return function ($arg$$10) {
    const y = foldMapOf(l$$2, function ($arg$$9) {
      const arg0$$4 = (0, _Util.partialApply)(1, f$$2, [$arg$$9]);
      return new _Monoids.Endo$00601(0, "Endo", arg0$$4);
    })($arg$$10);
    return (0, _Monoids.Endo$$$run)(y)(z);
  };
}

function foldlOf(l$$3, f$$5, z$$1) {
  return function ($arg$$14) {
    const $arg$$11 = foldMapOf(l$$3, function ($arg$$13) {
      let arg0$$5;
      arg0$$5 = new _Monoids.Endo$00601(0, "Endo", function (y$$4) {
        return f$$5(y$$4, $arg$$13);
      });
      return new _Monoids.Dual$00601(0, "Dual", arg0$$5);
    })($arg$$14);
    let y$$2;
    y$$2 = (0, _Monoids.Dual$$$run)($arg$$11);
    return (0, _Monoids.Endo$$$run)(y$$2)(z$$1);
  };
}

function toListOf(l$$4) {
  return foldrOf(l$$4, function cons(x$$5, y$$6) {
    return new _Types.List(x$$5, y$$6);
  }, new _Types.List());
}

function maximumOf(l$$5) {
  return foldlOf(l$$5, function mf(o, y$$7) {
    if (o == null) {
      return (0, _Option.some)(y$$7);
    } else {
      const x$$6 = (0, _Option.value)(o);
      return (0, _Option.some)((0, _Util.max)(_Util.compare, x$$6, y$$7));
    }
  }, null);
}

function minimumOf(l$$6) {
  return foldlOf(l$$6, function mf$$1(o$$1, y$$8) {
    if (o$$1 == null) {
      return (0, _Option.some)(y$$8);
    } else {
      const x$$7 = (0, _Option.value)(o$$1);
      return (0, _Option.some)((0, _Util.min)(_Util.compare, x$$7, y$$8));
    }
  }, null);
}

function anyOf(l$$7, f$$9) {
  return function ($arg$$20) {
    const arg00$0040$$6 = foldMapOf(l$$7, function ($arg$$19) {
      const arg0$$7 = f$$9($arg$$19);
      return new _Monoids.Any(0, "Any", arg0$$7);
    })($arg$$20);
    return Internals$$$getAny(arg00$0040$$6);
  };
}

function allOf(l$$8, f$$10) {
  return function ($arg$$22) {
    const arg00$0040$$7 = foldMapOf(l$$8, function ($arg$$21) {
      const arg0$$8 = f$$10($arg$$21);
      return new _Monoids.All(0, "All", arg0$$8);
    })($arg$$22);
    return Internals$$$getAll(arg00$0040$$7);
  };
}

function elemOf(l$$9) {
  return function ($arg$$23) {
    return anyOf(l$$9, function (y$$9) {
      return (0, _Util.equals)($arg$$23, y$$9);
    });
  };
}

function op_HatDot(source$$4, lens$$1) {
  return view(lens$$1, source$$4);
}

function op_DotMinusGreater(lens$$2, value$$1) {
  return function (source$$5) {
    return setl(lens$$2, value$$1, source$$5);
  };
}

function op_PercentMinusGreater(lens$$3, updater$$1) {
  return function (source$$6) {
    return over(lens$$3, updater$$1, source$$6);
  };
}

function op_HatQmark(source$$7, prism$$1) {
  return preview(prism$$1, source$$7);
}

function op_HatDotDot(s, l$$10) {
  return toListOf(l$$10)(s);
}