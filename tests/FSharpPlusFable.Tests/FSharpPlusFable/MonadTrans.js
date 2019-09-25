"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Lift$reflection = Lift$reflection;
exports.LiftAsync$reflection = LiftAsync$reflection;
exports.LiftAsync$$$LiftAsync$$ZD4A93B1 = LiftAsync$$$LiftAsync$$ZD4A93B1;
exports.Throw$reflection = Throw$reflection;
exports.Throw$$$Throw$$Z7E028290 = Throw$$$Throw$$Z7E028290;
exports.Throw$$$Throw$$704F8E32 = Throw$$$Throw$$704F8E32;
exports.Catch$reflection = Catch$reflection;
exports.Catch$$$Catch$$764BA1D3 = Catch$$$Catch$$764BA1D3;
exports.Catch$$$Catch$$Z19386EED = Catch$$$Catch$$Z19386EED;
exports.CallCC$reflection = CallCC$reflection;
exports.Get$reflection = Get$reflection;
exports.Put$reflection = Put$reflection;
exports.Ask$reflection = Ask$reflection;
exports.Local$reflection = Local$reflection;
exports.Tell$reflection = Tell$reflection;
exports.Listen$reflection = Listen$reflection;
exports.Pass$reflection = Pass$reflection;
exports.Pass = exports.Listen = exports.Tell = exports.Local = exports.Ask = exports.Put = exports.Get = exports.CallCC = exports.Catch = exports.Throw = exports.LiftAsync = exports.Lift = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

var _Option = require("./fable-library.2.3.24/Option");

const Lift = (0, _Types.declare)(function FSharpPlus_Control_Lift() {});
exports.Lift = Lift;

function Lift$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Lift");
}

const LiftAsync = (0, _Types.declare)(function FSharpPlus_Control_LiftAsync() {});
exports.LiftAsync = LiftAsync;

function LiftAsync$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.LiftAsync");
}

function LiftAsync$$$LiftAsync$$ZD4A93B1(_arg2) {
  return function (x) {
    return x;
  };
}

const Throw = (0, _Types.declare)(function FSharpPlus_Control_Throw() {});
exports.Throw = Throw;

function Throw$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Throw");
}

function Throw$$$Throw$$Z7E028290(_arg1, x$$1) {
  return new _Option.Result(1, "Error", x$$1);
}

function Throw$$$Throw$$704F8E32(_arg2$$1, x$$2) {
  return new _Option.Choice(1, "Choice2Of2", x$$2);
}

const Catch = (0, _Types.declare)(function FSharpPlus_Control_Catch() {});
exports.Catch = Catch;

function Catch$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Catch");
}

function Catch$$$Catch$$764BA1D3(x$$3, k) {
  if (x$$3.tag === 1) {
    return k(x$$3.fields[0]);
  } else {
    return new _Option.Result(0, "Ok", x$$3.fields[0]);
  }
}

function Catch$$$Catch$$Z19386EED(x$$4, k$$1) {
  if (x$$4.tag === 1) {
    return k$$1(x$$4.fields[0]);
  } else {
    return new _Option.Choice(0, "Choice1Of2", x$$4.fields[0]);
  }
}

const CallCC = (0, _Types.declare)(function FSharpPlus_Control_CallCC() {});
exports.CallCC = CallCC;

function CallCC$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.CallCC");
}

const Get = (0, _Types.declare)(function FSharpPlus_Control_Get() {});
exports.Get = Get;

function Get$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Get");
}

const Put = (0, _Types.declare)(function FSharpPlus_Control_Put() {});
exports.Put = Put;

function Put$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Put");
}

const Ask = (0, _Types.declare)(function FSharpPlus_Control_Ask() {});
exports.Ask = Ask;

function Ask$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Ask");
}

const Local = (0, _Types.declare)(function FSharpPlus_Control_Local() {});
exports.Local = Local;

function Local$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Local");
}

const Tell = (0, _Types.declare)(function FSharpPlus_Control_Tell() {});
exports.Tell = Tell;

function Tell$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Tell");
}

const Listen = (0, _Types.declare)(function FSharpPlus_Control_Listen() {});
exports.Listen = Listen;

function Listen$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Listen");
}

const Pass = (0, _Types.declare)(function FSharpPlus_Control_Pass() {});
exports.Pass = Pass;

function Pass$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Pass");
}