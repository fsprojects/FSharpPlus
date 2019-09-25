"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Explicit$reflection = Explicit$reflection;
exports.OfBytes$reflection = OfBytes$reflection;
exports.OfBytes$$$OfBytes$$15C6B29C = OfBytes$$$OfBytes$$15C6B29C;
exports.OfBytes$$$OfBytes$$Z48C3DED3 = OfBytes$$$OfBytes$$Z48C3DED3;
exports.ToBytes$reflection = ToBytes$reflection;
exports.ToBytes$$$ToBytes$$7AF173CB = ToBytes$$$ToBytes$$7AF173CB;
exports.ToBytes$$$ToBytes$$139EB29A = ToBytes$$$ToBytes$$139EB29A;
exports.TryParse$reflection = TryParse$reflection;
exports.TryParse$$$TryParse$$Z3D50DB7 = TryParse$$$TryParse$$Z3D50DB7;
exports.TryParse$$$TryParse$$1037BF58 = TryParse$$$TryParse$$1037BF58;
exports.TryParse$$$TryParse$$52BF4537 = TryParse$$$TryParse$$52BF4537;
exports.TryParse$$$TryParse$$Z3A7A819D = TryParse$$$TryParse$$Z3A7A819D;
exports.TryParse$$$TryParse$$Z3A7A891B = TryParse$$$TryParse$$Z3A7A891B;
exports.TryParse$$$TryParse$$Z3A7AB5DA = TryParse$$$TryParse$$Z3A7AB5DA;
exports.TryParse$$$TryParse$$12F5C2F6 = TryParse$$$TryParse$$12F5C2F6;
exports.TryParse$$$TryParse$$12F5DA70 = TryParse$$$TryParse$$12F5DA70;
exports.TryParse$$$TryParse$$12F5C5B3 = TryParse$$$TryParse$$12F5C5B3;
exports.TryParse$$$TryParse$$3DD4A837 = TryParse$$$TryParse$$3DD4A837;
exports.TryParse$$$TryParse$$Z3009897B = TryParse$$$TryParse$$Z3009897B;
exports.TryParse$$$TryParse$$4DE1303 = TryParse$$$TryParse$$4DE1303;
exports.TryParse$$$TryParse$$43B2208E = TryParse$$$TryParse$$43B2208E;
exports.Parse$reflection = Parse$reflection;
exports.Parse$$$Parse$$2D9EB305 = Parse$$$Parse$$2D9EB305;
exports.Parse$$$Parse$$Z35EE58F8 = Parse$$$Parse$$Z35EE58F8;
exports.Parse$$$Parse$$383379BA = Parse$$$Parse$$383379BA;
exports.Parse = exports.TryParse = exports.ToBytes = exports.OfBytes = exports.Explicit = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

var _BitConverter = require("./fable-library.2.3.24/BitConverter");

var _Internals = require("./Internals");

var _Array = require("./fable-library.2.3.24/Array");

var _Decimal = require("./fable-library.2.3.24/Decimal");

var _Option = require("./fable-library.2.3.24/Option");

var _Double = require("./fable-library.2.3.24/Double");

var _Int = require("./fable-library.2.3.24/Int32");

var _Long = require("./fable-library.2.3.24/Long");

var _System = require("./fable-library.2.3.24/System.Text");

var _Date = require("./fable-library.2.3.24/Date");

var _DateOffset = require("./fable-library.2.3.24/DateOffset");

var _Char = require("./fable-library.2.3.24/Char");

const Explicit = (0, _Types.declare)(function FSharpPlus_Control_Explicit() {});
exports.Explicit = Explicit;

function Explicit$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Explicit");
}

const OfBytes = (0, _Types.declare)(function FSharpPlus_Control_OfBytes() {});
exports.OfBytes = OfBytes;

function OfBytes$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.OfBytes");
}

function OfBytes$$$OfBytes$$15C6B29C(_arg1, _arg2) {
  return function (tupledArg) {
    return (0, _BitConverter.toBoolean)(tupledArg[0], tupledArg[1]);
  };
}

function OfBytes$$$OfBytes$$Z48C3DED3(_arg3, _arg4) {
  return function (tupledArg$$1) {
    return (0, _Internals.BitConverter$$$ToString$$52ECB83E)(tupledArg$$1[0], tupledArg$$1[1]);
  };
}

const ToBytes = (0, _Types.declare)(function FSharpPlus_Control_ToBytes() {});
exports.ToBytes = ToBytes;

function ToBytes$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.ToBytes");
}

function ToBytes$$$ToBytes$$7AF173CB(x$$2, _arg1$$2, _arg2$$2) {
  return (0, _Internals.BitConverter$$$GetBytes$$Z1FBCCD16)(x$$2);
}

function ToBytes$$$ToBytes$$139EB29A(x$$3, _arg3$$1, _arg4$$1) {
  return (0, _Array.map)(function (value) {
    return value.charCodeAt(0);
  }, x$$3.split(""), Uint8Array);
}

const TryParse = (0, _Types.declare)(function FSharpPlus_Control_TryParse() {});
exports.TryParse = TryParse;

function TryParse$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.TryParse");
}

function TryParse$$$TryParse$$Z3D50DB7(_arg1$$3, _arg2$$3) {
  return function (x$$4) {
    const tupledArg$$2 = (0, _Decimal.tryParse)(x$$4);
    return tupledArg$$2[0] ? (0, _Option.some)(tupledArg$$2[1]) : null;
  };
}

function TryParse$$$TryParse$$1037BF58(_arg3$$2, _arg4$$2) {
  return function (x$$6) {
    const tupledArg$$3 = (0, _Double.tryParse)(x$$6);
    return tupledArg$$3[0] ? (0, _Option.some)(tupledArg$$3[1]) : null;
  };
}

function TryParse$$$TryParse$$52BF4537(_arg5, _arg6) {
  return function (x$$8) {
    const tupledArg$$4 = (0, _Double.tryParse)(x$$8);
    return tupledArg$$4[0] ? (0, _Option.some)(tupledArg$$4[1]) : null;
  };
}

function TryParse$$$TryParse$$Z3A7A819D(_arg7, _arg8) {
  return function (x$$10) {
    const tupledArg$$5 = (0, _Int.tryParse)(x$$10, 511, true, 16);
    return tupledArg$$5[0] ? (0, _Option.some)(tupledArg$$5[1]) : null;
  };
}

function TryParse$$$TryParse$$Z3A7A891B(_arg9, _arg10) {
  return function (x$$12) {
    const tupledArg$$6 = (0, _Int.tryParse)(x$$12, 511, true, 32);
    return tupledArg$$6[0] ? (0, _Option.some)(tupledArg$$6[1]) : null;
  };
}

function TryParse$$$TryParse$$Z3A7AB5DA(_arg11, _arg12) {
  return function (x$$14) {
    const tupledArg$$7 = (0, _Long.tryParse)(x$$14, 511, true, 64);
    return tupledArg$$7[0] ? (0, _Option.some)(tupledArg$$7[1]) : null;
  };
}

function TryParse$$$TryParse$$12F5C2F6(_arg13, _arg14) {
  return function (x$$16) {
    const tupledArg$$8 = (0, _Int.tryParse)(x$$16, 511, false, 16);
    return tupledArg$$8[0] ? (0, _Option.some)(tupledArg$$8[1]) : null;
  };
}

function TryParse$$$TryParse$$12F5DA70(_arg15, _arg16) {
  return function (x$$18) {
    const tupledArg$$9 = (0, _Int.tryParse)(x$$18, 511, false, 32);
    return tupledArg$$9[0] ? (0, _Option.some)(tupledArg$$9[1]) : null;
  };
}

function TryParse$$$TryParse$$12F5C5B3(_arg17, _arg18) {
  return function (x$$20) {
    const tupledArg$$10 = (0, _Long.tryParse)(x$$20, 511, false, 64);
    return tupledArg$$10[0] ? (0, _Option.some)(tupledArg$$10[1]) : null;
  };
}

function TryParse$$$TryParse$$3DD4A837(_arg19, _arg20) {
  return function (x$$22) {
    return x$$22;
  };
}

function TryParse$$$TryParse$$Z3009897B(_arg21, _arg22) {
  return function (x$$23) {
    return (0, _System.StringBuilder$$$$002Ector$$Z721C83C5)(x$$23);
  };
}

function TryParse$$$TryParse$$4DE1303(_arg23, _arg24) {
  return function (x$$24) {
    const tupledArg$$11 = (0, _Date.tryParseExact)(x$$24, ["yyyy-MM-ddTHH:mm:ss.fffZ", "yyyy-MM-ddTHH:mm:ssZ"], null, 128, (0, _Date.minValue)());
    return tupledArg$$11[0] ? (0, _Option.some)(tupledArg$$11[1]) : null;
  };
}

function TryParse$$$TryParse$$43B2208E(_arg25, _arg26) {
  return function (x$$26) {
    const tupledArg$$12 = (0, _DateOffset.tryParseExact)(x$$26, ["yyyy-MM-ddTHH:mm:ss.fffK", "yyyy-MM-ddTHH:mm:ssK"], null, 128, (0, _DateOffset.minValue)());
    return tupledArg$$12[0] ? (0, _Option.some)(tupledArg$$12[1]) : null;
  };
}

const Parse = (0, _Types.declare)(function FSharpPlus_Control_Parse() {});
exports.Parse = Parse;

function Parse$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Parse");
}

function Parse$$$Parse$$2D9EB305(_arg5$$1, _arg6$$1) {
  return _Char.parse;
}

function Parse$$$Parse$$Z35EE58F8(_arg7$$1, _arg8$$1) {
  return function (x$$29) {
    return x$$29;
  };
}

function Parse$$$Parse$$383379BA(_arg9$$1, _arg10$$1) {
  return _System.StringBuilder$$$$002Ector$$Z721C83C5;
}