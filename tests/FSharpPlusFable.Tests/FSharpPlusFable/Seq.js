"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.SeqT$00601$reflection = SeqT$00601$reflection;
exports.SeqT$$$run = SeqT$$$run;
exports.SeqT$00601 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

const SeqT$00601 = (0, _Types.declare)(function FSharpPlus_Data_SeqT(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.SeqT$00601 = SeqT$00601;

function SeqT$00601$reflection($gen$$1) {
  return (0, _Reflection.union)("FSharpPlus.Data.SeqT`1", [$gen$$1], SeqT$00601, () => [["SeqT", [$gen$$1]]]);
}

function SeqT$$$run(_arg1) {
  return _arg1.fields[0];
}