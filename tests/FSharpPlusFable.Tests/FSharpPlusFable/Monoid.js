"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Plus$reflection = Plus$reflection;
exports.Plus$$$$002B$$Z7600AE84 = Plus$$$$002B$$Z7600AE84;
exports.Plus$$$$002B$$Z69633004 = Plus$$$$002B$$Z69633004;
exports.Plus$$$$002B$$27088CDC = Plus$$$$002B$$27088CDC;
exports.Plus$$$$002B$$Z60D22C4 = Plus$$$$002B$$Z60D22C4;
exports.Plus$$$$002B$$65A1517C = Plus$$$$002B$$65A1517C;
exports.Plus$$$$002B$$Z62D666A4 = Plus$$$$002B$$Z62D666A4;
exports.Plus$$$$002B$$Z62E20AC4 = Plus$$$$002B$$Z62E20AC4;
exports.Plus$$$$002B$$16E05F2D = Plus$$$$002B$$16E05F2D;
exports.Plus$$$$002B$$694F2E0D = Plus$$$$002B$$694F2E0D;
exports.Sum$reflection = Sum$reflection;
exports.Sum$$$Sum$$ZBD8955E = Sum$$$Sum$$ZBD8955E;
exports.Sum$$$Sum$$Z5FC659DE = Sum$$$Sum$$Z5FC659DE;
exports.Sum$$$Sum$$Z102BC59E = Sum$$$Sum$$Z102BC59E;
exports.Sum$$$Sum$$2E3B5D22 = Sum$$$Sum$$2E3B5D22;
exports.Sum = exports.Plus = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

var _List = require("./fable-library.2.3.24/List");

var _Array = require("./fable-library.2.3.24/Array");

var _Set = require("./fable-library.2.3.24/Set");

var _System = require("./fable-library.2.3.24/System.Text");

var _Internals = require("./Internals");

var _Seq = require("./fable-library.2.3.24/Seq");

var _Observable = require("./fable-library.2.3.24/Observable");

var _String = require("./fable-library.2.3.24/String");

const Plus = (0, _Types.declare)(function FSharpPlus_Control_Plus() {});
exports.Plus = Plus;

function Plus$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Plus");
}

function Plus$$$$002B$$Z7600AE84(x, y, _mthd) {
  return (0, _List.append)(x, y);
}

function Plus$$$$002B$$Z69633004(x$$1, y$$1, _mthd$$1) {
  return (0, _Array.append)(x$$1, y$$1, Array);
}

function Plus$$$$002B$$27088CDC(_arg1, _arg2, _mthd$$2) {}

function Plus$$$$002B$$Z60D22C4(x$$2, y$$2, _mthd$$3) {
  return (0, _Set.union)(x$$2, y$$2);
}

function Plus$$$$002B$$65A1517C(x$$3, y$$3, _mthd$$4) {
  return (0, _System.StringBuilder$$Append$$4E60E31B)((0, _System.StringBuilder$$Append$$4E60E31B)((0, _System.StringBuilder$$$$002Ector)(), x$$3), y$$3);
}

function Plus$$$$002B$$Z62D666A4(_arg3, _arg4, _mthd$$5) {
  return (0, _Internals.Id0$$$$002Ector$$Z721C83C5)("");
}

function Plus$$$$002B$$Z62E20AC4(x$$4, y$$4, _mthd$$6) {
  return Array.from((0, _Seq.append)(x$$4, y$$4));
}

function Plus$$$$002B$$16E05F2D(x$$5, y$$5, _mthd$$7) {
  return (0, _Observable.merge)(x$$5, y$$5);
}

function Plus$$$$002B$$694F2E0D(x$$6, y$$6, _mthd$$8) {
  return (0, _Seq.append)(x$$6, y$$6);
}

const Sum = (0, _Types.declare)(function FSharpPlus_Control_Sum() {});
exports.Sum = Sum;

function Sum$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Sum");
}

function Sum$$$Sum$$ZBD8955E(x$$7, _output, _impl) {
  return (0, _List.concat)(x$$7);
}

function Sum$$$Sum$$Z5FC659DE(x$$8, _output$$1, _impl$$1) {
  return (0, _Array.concat)(x$$8, Array);
}

function Sum$$$Sum$$Z102BC59E(x$$9, _output$$2, _impl$$2) {
  return (0, _String.join)("", ...x$$9);
}

function Sum$$$Sum$$2E3B5D22(x$$10, _output$$3, _impl$$3) {
  const state = (0, _System.StringBuilder$$$$002Ector)();
  return (0, _Seq.fold)(function folder(x$$11, arg00) {
    return (0, _System.StringBuilder$$Append$$4E60E31B)(x$$11, arg00);
  }, state, x$$10);
}