"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Dual$00601$reflection = Dual$00601$reflection;
exports.Dual$$$run = Dual$$$run;
exports.Endo$00601$reflection = Endo$00601$reflection;
exports.Endo$00601$$$get_Zero = Endo$00601$$$get_Zero;
exports.Endo$00601$$$op_Addition$$7D05C00 = Endo$00601$$$op_Addition$$7D05C00;
exports.Endo$$$run = Endo$$$run;
exports.All$reflection = All$reflection;
exports.All$$$get_Zero = All$$$get_Zero;
exports.All$$$op_Addition$$Z137E6A00 = All$$$op_Addition$$Z137E6A00;
exports.Any$reflection = Any$reflection;
exports.Any$$$get_Zero = Any$$$get_Zero;
exports.Any$$$op_Addition$$Z137E76A0 = Any$$$op_Addition$$Z137E76A0;
exports.Const$00602$reflection = Const$00602$reflection;
exports.Const$00602$$$Map$$3AB1BEFA = Const$00602$$$Map$$3AB1BEFA;
exports.Const$00602$$$Contramap$$3AB1BEFA = Const$00602$$$Contramap$$3AB1BEFA;
exports.Const$00602$$$Bimap$$Z135D6C14 = Const$00602$$$Bimap$$Z135D6C14;
exports.Const$00602$$$First$$3AB1BEFA = Const$00602$$$First$$3AB1BEFA;
exports.Const$$$run = Const$$$run;
exports.First$00601$reflection = First$00601$reflection;
exports.First$00601$$$get_Zero = First$00601$$$get_Zero;
exports.First$00601$$$op_Addition$$Z7D3A2180 = First$00601$$$op_Addition$$Z7D3A2180;
exports.First$00601$$$run$$Z73EA2E14 = First$00601$$$run$$Z73EA2E14;
exports.Last$00601$reflection = Last$00601$reflection;
exports.Last$00601$$$get_Zero = Last$00601$$$get_Zero;
exports.Last$00601$$$op_Addition$$7A934480 = Last$00601$$$op_Addition$$7A934480;
exports.Last$00601$$$run$$Z12AC9A24 = Last$00601$$$run$$Z12AC9A24;
exports.Mult$00601$reflection = Mult$00601$reflection;
exports.Compose$00601$reflection = Compose$00601$reflection;
exports.Compose$$$run = Compose$$$run;
exports.Compose$00601 = exports.Mult$00601 = exports.Last$00601 = exports.First$00601 = exports.Const$00602 = exports.Any = exports.All = exports.Endo$00601 = exports.Dual$00601 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

const Dual$00601 = (0, _Types.declare)(function FSharpPlus_Data_Dual(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Dual$00601 = Dual$00601;

function Dual$00601$reflection($gen$$2) {
  return (0, _Reflection.union)("FSharpPlus.Data.Dual`1", [$gen$$2], Dual$00601, () => [["Dual", [$gen$$2]]]);
}

function Dual$$$run(_arg1) {
  return _arg1.fields[0];
}

const Endo$00601 = (0, _Types.declare)(function FSharpPlus_Data_Endo(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Endo$00601 = Endo$00601;

function Endo$00601$reflection($gen$$4) {
  return (0, _Reflection.union)("FSharpPlus.Data.Endo`1", [$gen$$4], Endo$00601, () => [["Endo", [(0, _Reflection.lambda)($gen$$4, $gen$$4)]]]);
}

function Endo$00601$$$get_Zero() {
  return new Endo$00601(0, "Endo", function (x$$1) {
    return x$$1;
  });
}

function Endo$00601$$$op_Addition$$7D05C00(_arg1$$1, _arg2) {
  return new Endo$00601(0, "Endo", function ($arg$$1) {
    return _arg1$$1.fields[0](_arg2.fields[0]($arg$$1));
  });
}

function Endo$$$run(_arg1$$2) {
  return _arg1$$2.fields[0];
}

const All = (0, _Types.declare)(function FSharpPlus_Data_All(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.All = All;

function All$reflection() {
  return (0, _Reflection.union)("FSharpPlus.Data.All", [], All, () => [["All", [_Reflection.bool]]]);
}

function All$$$get_Zero() {
  return new All(0, "All", true);
}

function All$$$op_Addition$$Z137E6A00(_arg1$$3, _arg2$$1) {
  return new All(0, "All", _arg1$$3.fields[0] ? _arg2$$1.fields[0] : false);
}

const Any = (0, _Types.declare)(function FSharpPlus_Data_Any(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Any = Any;

function Any$reflection() {
  return (0, _Reflection.union)("FSharpPlus.Data.Any", [], Any, () => [["Any", [_Reflection.bool]]]);
}

function Any$$$get_Zero() {
  return new Any(0, "Any", false);
}

function Any$$$op_Addition$$Z137E76A0(_arg1$$4, _arg2$$2) {
  return new Any(0, "Any", _arg1$$4.fields[0] ? true : _arg2$$2.fields[0]);
}

const Const$00602 = (0, _Types.declare)(function FSharpPlus_Data_Const(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Const$00602 = Const$00602;

function Const$00602$reflection($gen$$12, $gen$$13) {
  return (0, _Reflection.union)("FSharpPlus.Data.Const`2", [$gen$$12, $gen$$13], Const$00602, () => [["Const", [$gen$$12]]]);
}

function Const$00602$$$Map$$3AB1BEFA(_arg3, _arg4) {
  return new Const$00602(0, "Const", _arg3.fields[0]);
}

function Const$00602$$$Contramap$$3AB1BEFA(_arg8, _arg9) {
  return new Const$00602(0, "Const", _arg8.fields[0]);
}

function Const$00602$$$Bimap$$Z135D6C14(_arg10, f$$2, _arg11) {
  return new Const$00602(0, "Const", f$$2(_arg10.fields[0]));
}

function Const$00602$$$First$$3AB1BEFA(_arg12, f$$3) {
  return new Const$00602(0, "Const", f$$3(_arg12.fields[0]));
}

function Const$$$run(_arg1$$5) {
  return _arg1$$5.fields[0];
}

const First$00601 = (0, _Types.declare)(function FSharpPlus_Data_First(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.First$00601 = First$00601;

function First$00601$reflection($gen$$24) {
  return (0, _Reflection.union)("FSharpPlus.Data.First`1", [$gen$$24], First$00601, () => [["First", [(0, _Reflection.option)($gen$$24)]]]);
}

function First$00601$$$get_Zero() {
  return new First$00601(0, "First", null);
}

function First$00601$$$op_Addition$$Z7D3A2180(x$$8, y$$2) {
  if (x$$8.fields[0] == null) {
    return y$$2;
  } else {
    return x$$8;
  }
}

function First$00601$$$run$$Z73EA2E14(_arg1$$6) {
  return _arg1$$6.fields[0];
}

const Last$00601 = (0, _Types.declare)(function FSharpPlus_Data_Last(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Last$00601 = Last$00601;

function Last$00601$reflection($gen$$28) {
  return (0, _Reflection.union)("FSharpPlus.Data.Last`1", [$gen$$28], Last$00601, () => [["Last", [(0, _Reflection.option)($gen$$28)]]]);
}

function Last$00601$$$get_Zero() {
  return new Last$00601(0, "Last", null);
}

function Last$00601$$$op_Addition$$7A934480(x$$9, y$$3) {
  if (y$$3.fields[0] == null) {
    return x$$9;
  } else {
    return y$$3;
  }
}

function Last$00601$$$run$$Z12AC9A24(_arg1$$7) {
  return _arg1$$7.fields[0];
}

const Mult$00601 = (0, _Types.declare)(function FSharpPlus_Data_Mult(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Mult$00601 = Mult$00601;

function Mult$00601$reflection($gen$$32) {
  return (0, _Reflection.union)("FSharpPlus.Data.Mult`1", [$gen$$32], Mult$00601, () => [["Mult", [$gen$$32]]]);
}

const Compose$00601 = (0, _Types.declare)(function FSharpPlus_Data_Compose(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Compose$00601 = Compose$00601;

function Compose$00601$reflection($gen$$33) {
  return (0, _Reflection.union)("FSharpPlus.Data.Compose`1", [$gen$$33], Compose$00601, () => [["Compose", [$gen$$33]]]);
}

function Compose$$$run(_arg1$$8) {
  return _arg1$$8.fields[0];
}