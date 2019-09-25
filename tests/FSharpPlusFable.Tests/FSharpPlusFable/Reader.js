"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Reader$00602$reflection = Reader$00602$reflection;
exports.Reader$$$run = Reader$$$run;
exports.Reader$$$map = Reader$$$map;
exports.Reader$$$bind = Reader$$$bind;
exports.Reader$$$apply = Reader$$$apply;
exports.Reader$$$ask = Reader$$$ask;
exports.Reader$$$local = Reader$$$local;
exports.Reader$00602$$$Map$$Z6244AF86 = Reader$00602$$$Map$$Z6244AF86;
exports.Reader$00602$$$Return$$1505 = Reader$00602$$$Return$$1505;
exports.Reader$00602$$$op_GreaterGreaterEquals$$Z2A846D4D = Reader$00602$$$op_GreaterGreaterEquals$$Z2A846D4D;
exports.Reader$00602$$$op_LessMultiplyGreater$$ZB6E4A2D = Reader$00602$$$op_LessMultiplyGreater$$ZB6E4A2D;
exports.Reader$00602$$$get_Ask = Reader$00602$$$get_Ask;
exports.Reader$00602$$$Local$$Z6244AF86 = Reader$00602$$$Local$$Z6244AF86;
exports.ReaderT$00602$reflection = ReaderT$00602$reflection;
exports.ReaderT$$$run = ReaderT$$$run;
exports.ReaderT$00602$$$Lift$$1505 = ReaderT$00602$$$Lift$$1505;
exports.ReaderT$00602$$$Local$$Z15280BD2 = ReaderT$00602$$$Local$$Z15280BD2;
exports.ReaderT$00602 = exports.Reader$00602 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

const Reader$00602 = (0, _Types.declare)(function FSharpPlus_Data_Reader(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Reader$00602 = Reader$00602;

function Reader$00602$reflection($gen$$3, $gen$$4) {
  return (0, _Reflection.union)("FSharpPlus.Data.Reader`2", [$gen$$3, $gen$$4], Reader$00602, () => [["Reader", [(0, _Reflection.lambda)($gen$$3, $gen$$4)]]]);
}

function Reader$$$run(_arg1) {
  return _arg1.fields[0];
}

function Reader$$$map(f, _arg1$$1) {
  return new Reader$00602(0, "Reader", function ($arg$$1) {
    return f(_arg1$$1.fields[0]($arg$$1));
  });
}

function Reader$$$bind(f$$1, _arg1$$2) {
  return new Reader$00602(0, "Reader", function (r) {
    return Reader$$$run(f$$1(_arg1$$2.fields[0](r)))(r);
  });
}

function Reader$$$apply(_arg2, _arg1$$3) {
  return new Reader$00602(0, "Reader", function (a) {
    return _arg2.fields[0](a)(_arg1$$3.fields[0](a));
  });
}

function Reader$$$ask() {
  return new Reader$00602(0, "Reader", function (x$$2) {
    return x$$2;
  });
}

function Reader$$$local(f$$3, m$$2) {
  return new Reader$00602(0, "Reader", function ($arg$$2) {
    return m$$2.fields[0](f$$3($arg$$2));
  });
}

function Reader$00602$$$Map$$Z6244AF86(x$$3, f$$4) {
  return Reader$$$map(f$$4, x$$3);
}

function Reader$00602$$$Return$$1505(x$$4) {
  return new Reader$00602(0, "Reader", function (_arg1$$4) {
    return x$$4;
  });
}

function Reader$00602$$$op_GreaterGreaterEquals$$Z2A846D4D(x$$5, f$$5) {
  return Reader$$$bind(f$$5, x$$5);
}

function Reader$00602$$$op_LessMultiplyGreater$$ZB6E4A2D(f$$6, x$$6) {
  return Reader$$$apply(f$$6, x$$6);
}

function Reader$00602$$$get_Ask() {
  return Reader$$$ask();
}

function Reader$00602$$$Local$$Z6244AF86(m$$4, f$$7) {
  return Reader$$$local(f$$7, m$$4);
}

const ReaderT$00602 = (0, _Types.declare)(function FSharpPlus_Data_ReaderT(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.ReaderT$00602 = ReaderT$00602;

function ReaderT$00602$reflection($gen$$23, $gen$$24) {
  return (0, _Reflection.union)("FSharpPlus.Data.ReaderT`2", [$gen$$23, $gen$$24], ReaderT$00602, () => [["ReaderT", [(0, _Reflection.lambda)($gen$$23, $gen$$24)]]]);
}

function ReaderT$$$run(_arg1$$5) {
  return _arg1$$5.fields[0];
}

function ReaderT$00602$$$Lift$$1505(m$$5) {
  return new ReaderT$00602(0, "ReaderT", function (_arg3) {
    return m$$5;
  });
}

function ReaderT$00602$$$Local$$Z15280BD2(_arg3$$1, f$$8) {
  return new ReaderT$00602(0, "ReaderT", function (r$$1) {
    return _arg3$$1.fields[0](f$$8(r$$1));
  });
}