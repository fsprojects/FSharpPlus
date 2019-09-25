"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ParallelArray$00601$reflection = ParallelArray$00601$reflection;
exports.ParallelArray$$$run = ParallelArray$$$run;
exports.ParallelArrayOperators$$$parray = ParallelArrayOperators$$$parray;
exports.ParallelArray$00601$$$Return$$1505 = ParallelArray$00601$$$Return$$1505;
exports.ParallelArray$00601 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

const ParallelArray$00601 = (0, _Types.declare)(function FSharpPlus_Data_ParallelArray(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.ParallelArray$00601 = ParallelArray$00601;

function ParallelArray$00601$reflection($gen$$1) {
  return (0, _Reflection.union)("FSharpPlus.Data.ParallelArray`1", [$gen$$1], ParallelArray$00601, () => [["Infinite", [$gen$$1]], ["Bounded", [(0, _Reflection.array)($gen$$1)]]]);
}

function ParallelArray$$$run(_arg1) {
  if (_arg1.tag === 1) {
    return _arg1.fields[0];
  } else {
    throw new Error("Resulting array would be infinite.");
  }
}

function ParallelArrayOperators$$$parray(s) {
  return new ParallelArray$00601(1, "Bounded", s);
}

function ParallelArray$00601$$$Return$$1505(x) {
  return new ParallelArray$00601(0, "Infinite", x);
}