"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.State$00602$reflection = State$00602$reflection;
exports.State$$$run = State$$$run;
exports.State$$$map = State$$$map;
exports.State$$$bind = State$$$bind;
exports.State$$$apply = State$$$apply;
exports.State$$$eval = State$$$eval;
exports.State$$$exec = State$$$exec;
exports.State$$$get = State$$$get;
exports.State$$$put = State$$$put;
exports.State$00602$$$Map$$Z17288698 = State$00602$$$Map$$Z17288698;
exports.State$00602$$$Return$$1505 = State$00602$$$Return$$1505;
exports.State$00602$$$op_GreaterGreaterEquals$$Z5DD85EAD = State$00602$$$op_GreaterGreaterEquals$$Z5DD85EAD;
exports.State$00602$$$op_LessMultiplyGreater$$2AB19493 = State$00602$$$op_LessMultiplyGreater$$2AB19493;
exports.State$00602$$$get_Get = State$00602$$$get_Get;
exports.State$00602$$$Put$$1505 = State$00602$$$Put$$1505;
exports.StateT$00602$reflection = StateT$00602$reflection;
exports.StateT$$$run = StateT$$$run;
exports.StateT$00602 = exports.State$00602 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

const State$00602 = (0, _Types.declare)(function FSharpPlus_Data_State(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.State$00602 = State$00602;

function State$00602$reflection($gen$$1, $gen$$2) {
  return (0, _Reflection.union)("FSharpPlus.Data.State`2", [$gen$$1, $gen$$2], State$00602, () => [["State", [(0, _Reflection.lambda)($gen$$1, (0, _Reflection.tuple)($gen$$2, $gen$$1))]]]);
}

function State$$$run(_arg1) {
  return _arg1.fields[0];
}

function State$$$map(f, _arg1$$1) {
  return new State$00602(0, "State", function (s) {
    const patternInput = _arg1$$1.fields[0](s);

    return [f(patternInput[0]), patternInput[1]];
  });
}

function State$$$bind(f$$1, _arg1$$2) {
  return new State$00602(0, "State", function (s$$1) {
    const patternInput$$1 = _arg1$$2.fields[0](s$$1);

    return State$$$run(f$$1(patternInput$$1[0]))(patternInput$$1[1]);
  });
}

function State$$$apply(_arg2, _arg1$$3) {
  return new State$00602(0, "State", function (s$$2) {
    const patternInput$$2 = _arg2.fields[0](s$$2);

    const patternInput$$3 = _arg1$$3.fields[0](patternInput$$2[1]);

    return [patternInput$$2[0](patternInput$$3[0]), patternInput$$3[1]];
  });
}

function State$$$eval(_arg1$$4, s$$3) {
  return _arg1$$4.fields[0](s$$3)[0];
}

function State$$$exec(_arg1$$5, s$$4) {
  return _arg1$$5.fields[0](s$$4)[1];
}

function State$$$get() {
  return new State$00602(0, "State", function (s$$5) {
    return [s$$5, s$$5];
  });
}

function State$$$put(x$$2) {
  return new State$00602(0, "State", function (_arg1$$6) {
    return [null, x$$2];
  });
}

function State$00602$$$Map$$Z17288698(x$$3, f$$3) {
  return State$$$map(f$$3, x$$3);
}

function State$00602$$$Return$$1505(a$$2) {
  return new State$00602(0, "State", function (s$$6) {
    return [a$$2, s$$6];
  });
}

function State$00602$$$op_GreaterGreaterEquals$$Z5DD85EAD(x$$4, f$$4) {
  return State$$$bind(f$$4, x$$4);
}

function State$00602$$$op_LessMultiplyGreater$$2AB19493(f$$5, x$$5) {
  return State$$$apply(f$$5, x$$5);
}

function State$00602$$$get_Get() {
  return State$$$get();
}

function State$00602$$$Put$$1505(x$$6) {
  return State$$$put(x$$6);
}

const StateT$00602 = (0, _Types.declare)(function FSharpPlus_Data_StateT(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.StateT$00602 = StateT$00602;

function StateT$00602$reflection($gen$$23, $gen$$24) {
  return (0, _Reflection.union)("FSharpPlus.Data.StateT`2", [$gen$$23, $gen$$24], StateT$00602, () => [["StateT", [(0, _Reflection.lambda)($gen$$23, $gen$$24)]]]);
}

function StateT$$$run(_arg1$$7) {
  return _arg1$$7.fields[0];
}