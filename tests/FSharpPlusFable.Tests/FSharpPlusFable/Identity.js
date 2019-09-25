"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Identity$00601$reflection = Identity$00601$reflection;
exports.Identity$00601$$$Return$$1505 = Identity$00601$$$Return$$1505;
exports.Identity$00601$$$op_GreaterGreaterEquals$$Z408F5B6D = Identity$00601$$$op_GreaterGreaterEquals$$Z408F5B6D;
exports.Identity$00601$$$op_LessMultiplyGreater$$22194EB5 = Identity$00601$$$op_LessMultiplyGreater$$22194EB5;
exports.Identity$00601$$$Map$$Z7F07DB80 = Identity$00601$$$Map$$Z7F07DB80;
exports.Identity$$$run = Identity$$$run;
exports.Identity$00601 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

const Identity$00601 = (0, _Types.declare)(function FSharpPlus_Data_Identity(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Identity$00601 = Identity$00601;

function Identity$00601$reflection($gen$$1) {
  return (0, _Reflection.union)("FSharpPlus.Data.Identity`1", [$gen$$1], Identity$00601, () => [["Identity", [$gen$$1]]]);
}

function Identity$00601$$$Return$$1505(x) {
  return new Identity$00601(0, "Identity", x);
}

function Identity$00601$$$op_GreaterGreaterEquals$$Z408F5B6D(_arg1, f) {
  return f(_arg1.fields[0]);
}

function Identity$00601$$$op_LessMultiplyGreater$$22194EB5(_arg2, _arg3) {
  return new Identity$00601(0, "Identity", _arg2.fields[0](_arg3.fields[0]));
}

function Identity$00601$$$Map$$Z7F07DB80(_arg4, f$$2) {
  return new Identity$00601(0, "Identity", f$$2(_arg4.fields[0]));
}

function Identity$$$run(_arg1$$1) {
  return _arg1$$1.fields[0];
}