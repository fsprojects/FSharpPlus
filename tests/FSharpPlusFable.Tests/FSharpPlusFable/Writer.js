"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Writer$00602$reflection = Writer$00602$reflection;
exports.Writer$$$run = Writer$$$run;
exports.Writer$$$map = Writer$$$map;
exports.Writer$$$exec = Writer$$$exec;
exports.Writer$$$tell = Writer$$$tell;
exports.Writer$$$listen = Writer$$$listen;
exports.Writer$$$pass = Writer$$$pass;
exports.Writer$00602$$$Map$$Z48653D30 = Writer$00602$$$Map$$Z48653D30;
exports.Writer$00602$$$Tell$$1505 = Writer$00602$$$Tell$$1505;
exports.Writer$00602$$$Listen$$15B10646 = Writer$00602$$$Listen$$15B10646;
exports.Writer$00602$$$Pass$$Z72C5ABC1 = Writer$00602$$$Pass$$Z72C5ABC1;
exports.Writer$00602$$$Extract$$15B10646 = Writer$00602$$$Extract$$15B10646;
exports.Writer$00602$$$op_EqualsGreaterGreater$$3C160093 = Writer$00602$$$op_EqualsGreaterGreater$$3C160093;
exports.WriterT$00601$reflection = WriterT$00601$reflection;
exports.WriterT$$$run = WriterT$$$run;
exports.WriterT$00601 = exports.Writer$00602 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

const Writer$00602 = (0, _Types.declare)(function FSharpPlus_Data_Writer(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Writer$00602 = Writer$00602;

function Writer$00602$reflection($gen$$1, $gen$$2) {
  return (0, _Reflection.union)("FSharpPlus.Data.Writer`2", [$gen$$1, $gen$$2], Writer$00602, () => [["Writer", [(0, _Reflection.tuple)($gen$$2, $gen$$1)]]]);
}

function Writer$$$run(_arg1) {
  return _arg1.fields[0];
}

function Writer$$$map(f, _arg1$$1) {
  return new Writer$00602(0, "Writer", [f(_arg1$$1.fields[0][0]), _arg1$$1.fields[0][1]]);
}

function Writer$$$exec(_arg1$$2) {
  return _arg1$$2.fields[0][1];
}

function Writer$$$tell(w$$1) {
  return new Writer$00602(0, "Writer", [null, w$$1]);
}

function Writer$$$listen(m$$1) {
  return new Writer$00602(0, "Writer", [[m$$1.fields[0][0], m$$1.fields[0][1]], m$$1.fields[0][1]]);
}

function Writer$$$pass(m$$2) {
  return new Writer$00602(0, "Writer", [m$$2.fields[0][0][0], m$$2.fields[0][0][1](m$$2.fields[0][1])]);
}

function Writer$00602$$$Map$$Z48653D30(x$$1, f$$2) {
  return Writer$$$map(f$$2, x$$1);
}

function Writer$00602$$$Tell$$1505(w$$4) {
  return Writer$$$tell(w$$4);
}

function Writer$00602$$$Listen$$15B10646(m$$3) {
  return Writer$$$listen(m$$3);
}

function Writer$00602$$$Pass$$Z72C5ABC1(m$$4) {
  return Writer$$$pass(m$$4);
}

function Writer$00602$$$Extract$$15B10646(_arg1$$3) {
  return _arg1$$3.fields[0][1];
}

function Writer$00602$$$op_EqualsGreaterGreater$$3C160093(_arg2, f$$3) {
  return new Writer$00602(0, "Writer", [_arg2.fields[0][0], f$$3(_arg2)]);
}

const WriterT$00601 = (0, _Types.declare)(function FSharpPlus_Data_WriterT(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.WriterT$00601 = WriterT$00601;

function WriterT$00601$reflection($gen$$18) {
  return (0, _Reflection.union)("FSharpPlus.Data.WriterT`1", [$gen$$18], WriterT$00601, () => [["WriterT", [$gen$$18]]]);
}

function WriterT$$$run(_arg1$$4) {
  return _arg1$$4.fields[0];
}