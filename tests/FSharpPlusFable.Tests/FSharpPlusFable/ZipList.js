"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ZipList$00601$reflection = ZipList$00601$reflection;
exports.ZipList$00601$$Item$$Z524259A4 = ZipList$00601$$Item$$Z524259A4;
exports.ZipList$$$run = ZipList$$$run;
exports.ZipList$$$map = ZipList$$$map;
exports.ZipList$$$singleton = ZipList$$$singleton;
exports.ZipList$00601$$$Map$$1935EA17 = ZipList$00601$$$Map$$1935EA17;
exports.ZipList$00601$$$Return$$1505 = ZipList$00601$$$Return$$1505;
exports.ZipList$00601$$$op_LessMultiplyGreater$$181AD695 = ZipList$00601$$$op_LessMultiplyGreater$$181AD695;
exports.ZipList$00601$$$ToSeq$$777073A1 = ZipList$00601$$$ToSeq$$777073A1;
exports.ZipList$00601 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

var _Seq = require("./fable-library.2.3.24/Seq");

const ZipList$00601 = (0, _Types.declare)(function FSharpPlus_Data_ZipList(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.ZipList$00601 = ZipList$00601;

function ZipList$00601$reflection($gen$$1) {
  return (0, _Reflection.union)("FSharpPlus.Data.ZipList`1", [$gen$$1], ZipList$00601, () => [["ZipList", [(0, _Reflection.type)("System.Collections.Generic.IEnumerable`1", [$gen$$1])]]]);
}

function ZipList$00601$$Item$$Z524259A4(this$, n) {
  return (0, _Seq.item)(n, this$.fields[0]);
}

function ZipList$$$run(_arg1) {
  return _arg1.fields[0];
}

function ZipList$$$map(f, _arg1$$1) {
  return new ZipList$00601(0, "ZipList", (0, _Seq.map)(f, _arg1$$1.fields[0]));
}

function ZipList$$$singleton(x$$2) {
  return new ZipList$00601(0, "ZipList", (0, _Seq.singleton)(x$$2));
}

function ZipList$00601$$$Map$$1935EA17(_arg1$$2, f$$1) {
  return new ZipList$00601(0, "ZipList", (0, _Seq.map)(f$$1, _arg1$$2.fields[0]));
}

function ZipList$00601$$$Return$$1505(x$$4) {
  return new ZipList$00601(0, "ZipList", (0, _Seq.initializeInfinite)(function (arg10$0040) {
    return x$$4;
  }));
}

function ZipList$00601$$$op_LessMultiplyGreater$$181AD695(_arg2, _arg3) {
  var source;
  return new ZipList$00601(0, "ZipList", (source = (0, _Seq.zip)(_arg2.fields[0], _arg3.fields[0]), ((0, _Seq.map)(function mapping(tupledArg) {
    return tupledArg[0](tupledArg[1]);
  }, source))));
}

function ZipList$00601$$$ToSeq$$777073A1(_arg4) {
  return _arg4.fields[0];
}