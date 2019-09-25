"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Cont$00602$reflection = Cont$00602$reflection;
exports.Cont$$$run = Cont$$$run;
exports.Cont$$$callCC = Cont$$$callCC;
exports.Cont$$$map = Cont$$$map;
exports.Cont$$$bind = Cont$$$bind;
exports.Cont$$$apply = Cont$$$apply;
exports.Cont$00602$$$Return$$1505 = Cont$00602$$$Return$$1505;
exports.Cont$00602$$$Map$$2D351509 = Cont$00602$$$Map$$2D351509;
exports.Cont$00602$$$op_LessMultiplyGreater$$1FA4F473 = Cont$00602$$$op_LessMultiplyGreater$$1FA4F473;
exports.Cont$00602$$$op_GreaterGreaterEquals$$ZBCE796D = Cont$00602$$$op_GreaterGreaterEquals$$ZBCE796D;
exports.Cont$00602$$$Delay$$22F21795 = Cont$00602$$$Delay$$22F21795;
exports.Cont$00602$$$TryWith$$Z67A993D1 = Cont$00602$$$TryWith$$Z67A993D1;
exports.Cont$00602$$$TryFinally$$6B257C89 = Cont$00602$$$TryFinally$$6B257C89;
exports.Cont$00602$$$Using$$Z5AB2D417 = Cont$00602$$$Using$$Z5AB2D417;
exports.Cont$00602$$$CallCC$$Z2A07D25B = Cont$00602$$$CallCC$$Z2A07D25B;
exports.ContT$$$run = ContT$$$run;
exports.Cont$00602 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

var _Operators = require("./Operators");

const Cont$00602 = (0, _Types.declare)(function FSharpPlus_Data_Cont(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Cont$00602 = Cont$00602;

function Cont$00602$reflection($gen$$3, $gen$$4) {
  return (0, _Reflection.union)("FSharpPlus.Data.Cont`2", [$gen$$3, $gen$$4], Cont$00602, () => [["Cont", [(0, _Reflection.lambda)((0, _Reflection.lambda)($gen$$4, $gen$$3), $gen$$3)]]]);
}

function Cont$$$run(_arg1) {
  return _arg1.fields[0];
}

function Cont$$$callCC(f) {
  return new Cont$00602(0, "Cont", function (k) {
    return Cont$$$run(f(function (a) {
      return new Cont$00602(0, "Cont", function (_arg1$$1) {
        return k(a);
      });
    }))(k);
  });
}

function Cont$$$map(f$$1, _arg1$$2) {
  return new Cont$00602(0, "Cont", function (c) {
    return _arg1$$2.fields[0](function ($arg$$1) {
      return c(f$$1($arg$$1));
    });
  });
}

function Cont$$$bind(f$$2, _arg1$$3) {
  return new Cont$00602(0, "Cont", function (k$$1) {
    return _arg1$$3.fields[0](function (a$$1) {
      return Cont$$$run(f$$2(a$$1))(k$$1);
    });
  });
}

function Cont$$$apply(_arg2, _arg1$$4) {
  return new Cont$00602(0, "Cont", function (k$$2) {
    return _arg2.fields[0](function (f$0027) {
      return _arg1$$4.fields[0](function ($arg$$2) {
        return k$$2(f$0027($arg$$2));
      });
    });
  });
}

function Cont$00602$$$Return$$1505(n) {
  return new Cont$00602(0, "Cont", function (k$$3) {
    return k$$3(n);
  });
}

function Cont$00602$$$Map$$2D351509(x$$4, f$$4) {
  return Cont$$$map(f$$4, x$$4);
}

function Cont$00602$$$op_LessMultiplyGreater$$1FA4F473(f$$5, x$$5) {
  return Cont$$$apply(f$$5, x$$5);
}

function Cont$00602$$$op_GreaterGreaterEquals$$ZBCE796D(x$$6, f$$6) {
  return Cont$$$bind(f$$6, x$$6);
}

function Cont$00602$$$Delay$$22F21795(f$$7) {
  return new Cont$00602(0, "Cont", function (k$$4) {
    return Cont$$$run(f$$7())(k$$4);
  });
}

function Cont$00602$$$TryWith$$Z67A993D1(_arg1$$5, h) {
  return new Cont$00602(0, "Cont", function (k$$5) {
    try {
      return _arg1$$5.fields[0](k$$5);
    } catch (e) {
      return Cont$$$run(h(e))(k$$5);
    }
  });
}

function Cont$00602$$$TryFinally$$6B257C89(_arg2$$1, h$$1) {
  return new Cont$00602(0, "Cont", function (k$$6) {
    try {
      return _arg2$$1.fields[0](k$$6);
    } finally {
      h$$1();
    }
  });
}

function Cont$00602$$$Using$$Z5AB2D417(resource, f$$8) {
  return Cont$00602$$$TryFinally$$6B257C89(f$$8(resource), function () {
    (0, _Operators.Operators$$$dispose)(resource);
  });
}

function Cont$00602$$$CallCC$$Z2A07D25B(f$$9) {
  return Cont$$$callCC(f$$9);
}

function ContT$$$run(_arg1$$6) {
  return _arg1$$6.fields[0];
}