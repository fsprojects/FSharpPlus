"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.OptionT$00601$reflection = OptionT$00601$reflection;
exports.OptionT$$$run = OptionT$$$run;
exports.OptionT$00601 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

const OptionT$00601 = (0, _Types.declare)(function FSharpPlus_Data_OptionT(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.OptionT$00601 = OptionT$00601;

function OptionT$00601$reflection($gen$$1) {
  return (0, _Reflection.union)("FSharpPlus.Data.OptionT`1", [$gen$$1], OptionT$00601, () => [["OptionT", [$gen$$1]]]);
}

function OptionT$$$run(_arg1) {
  return _arg1.fields[0];
}