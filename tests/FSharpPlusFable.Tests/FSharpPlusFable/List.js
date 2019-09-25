"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ListT$00601$reflection = ListT$00601$reflection;
exports.ListT$$$run = ListT$$$run;
exports.ListT$00601 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

const ListT$00601 = (0, _Types.declare)(function FSharpPlus_Data_ListT(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.ListT$00601 = ListT$00601;

function ListT$00601$reflection($gen$$1) {
  return (0, _Reflection.union)("FSharpPlus.Data.ListT`1", [$gen$$1], ListT$00601, () => [["ListT", [$gen$$1]]]);
}

function ListT$$$run(_arg1) {
  return _arg1.fields[0];
}