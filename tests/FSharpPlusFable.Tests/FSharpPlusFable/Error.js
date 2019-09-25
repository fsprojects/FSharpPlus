"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ResultOrException$$$IsResult = ResultOrException$$$IsResult;
exports.ResultOrException$$$IsException = ResultOrException$$$IsException;
exports.ResultOrException$$$Result = ResultOrException$$$Result;
exports.ResultOrException$$$Exception = ResultOrException$$$Exception;
exports.ResultT$00601$reflection = ResultT$00601$reflection;
exports.ResultT$$$run = ResultT$$$run;
exports.ChoiceT$00601$reflection = ChoiceT$00601$reflection;
exports.ChoiceT$$$run = ChoiceT$$$run;
exports.ChoiceT$00601 = exports.ResultT$00601 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

function ResultOrException$$$IsResult(_arg1) {
  if (_arg1.tag === 0) {
    return true;
  } else {
    return false;
  }
}

function ResultOrException$$$IsException(_arg1$$1) {
  if (_arg1$$1.tag === 1) {
    return true;
  } else {
    return false;
  }
}

function ResultOrException$$$Result(_arg1$$2) {
  if (_arg1$$2.tag === 1) {
    throw _arg1$$2.fields[0];
  } else {
    return _arg1$$2.fields[0];
  }
}

function ResultOrException$$$Exception(_arg1$$3) {
  if (_arg1$$3.tag === 1) {
    return _arg1$$3.fields[0];
  } else {
    return new Error();
  }
}

const ResultT$00601 = (0, _Types.declare)(function FSharpPlus_Data_ResultT(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.ResultT$00601 = ResultT$00601;

function ResultT$00601$reflection($gen$$5) {
  return (0, _Reflection.union)("FSharpPlus.Data.ResultT`1", [$gen$$5], ResultT$00601, () => [["ResultT", [$gen$$5]]]);
}

function ResultT$$$run(_arg1$$4) {
  return _arg1$$4.fields[0];
}

const ChoiceT$00601 = (0, _Types.declare)(function FSharpPlus_Data_ChoiceT(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.ChoiceT$00601 = ChoiceT$00601;

function ChoiceT$00601$reflection($gen$$7) {
  return (0, _Reflection.union)("FSharpPlus.Data.ChoiceT`1", [$gen$$7], ChoiceT$00601, () => [["ChoiceT", [$gen$$7]]]);
}

function ChoiceT$$$run(_arg1$$5) {
  return _arg1$$5.fields[0];
}