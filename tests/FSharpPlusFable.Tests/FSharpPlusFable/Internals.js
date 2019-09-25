"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Default6$reflection = Default6$reflection;
exports.Default5$reflection = Default5$reflection;
exports.Default4$reflection = Default4$reflection;
exports.Default3$reflection = Default3$reflection;
exports.Default2$reflection = Default2$reflection;
exports.Default1$reflection = Default1$reflection;
exports.BigInteger$$$trySqrtRem = BigInteger$$$trySqrtRem;
exports.Id$00601$reflection = Id$00601$reflection;
exports.Id$00601$$$$002Ector$$2B595 = Id$00601$$$$002Ector$$2B595;
exports.Id$00601$$get_getValue = Id$00601$$get_getValue;
exports.Id$$$run = Id$$$run;
exports.Id$$$map = Id$$$map;
exports.Id$$$create = Id$$$create;
exports.Id0$reflection = Id0$reflection;
exports.Id0$$$$002Ector$$Z721C83C5 = Id0$$$$002Ector$$Z721C83C5;
exports.Id0$$get_getValue = Id0$$get_getValue;
exports.Either$00602$reflection = Either$00602$reflection;
exports.DmStruct$reflection = DmStruct$reflection;
exports.BitConverter$reflection = BitConverter$reflection;
exports.BitConverter$$$GetBytes$$Z1FBCCD16 = BitConverter$$$GetBytes$$Z1FBCCD16;
exports.BitConverter$$$ToString$$1D3E19A2 = BitConverter$$$ToString$$1D3E19A2;
exports.BitConverter$$$ToString$$6C95DA22 = BitConverter$$$ToString$$6C95DA22;
exports.BitConverter$$$ToString$$52ECB83E = BitConverter$$$ToString$$52ECB83E;
exports.BitConverter = exports.DmStruct = exports.Either$00602 = exports.Id0 = exports.Id$00601 = exports.Errors$$$exnNoSubtraction = exports.Errors$$$exnNoSqrt = exports.Errors$$$exnSqrtOfNegative = exports.Errors$$$exnNoDivision = exports.Errors$$$exnDivByZero = exports.Default1 = exports.Default2 = exports.Default3 = exports.Default4 = exports.Default5 = exports.Default6 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

var _BigInt = require("./fable-library.2.3.24/BigInt");

var _Util = require("./fable-library.2.3.24/Util");

var _Option = require("./fable-library.2.3.24/Option");

var _Array = require("./fable-library.2.3.24/Array");

var _FSharp = require("./fable-library.2.3.24/FSharp.Core");

var _Seq = require("./fable-library.2.3.24/Seq");

const Default6 = (0, _Types.declare)(function FSharpPlus_Internals_Default6() {});
exports.Default6 = Default6;

function Default6$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Internals.Default6");
}

const Default5 = (0, _Types.declare)(function FSharpPlus_Internals_Default5() {});
exports.Default5 = Default5;

function Default5$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Internals.Default5");
}

const Default4 = (0, _Types.declare)(function FSharpPlus_Internals_Default4() {});
exports.Default4 = Default4;

function Default4$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Internals.Default4");
}

const Default3 = (0, _Types.declare)(function FSharpPlus_Internals_Default3() {});
exports.Default3 = Default3;

function Default3$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Internals.Default3");
}

const Default2 = (0, _Types.declare)(function FSharpPlus_Internals_Default2() {});
exports.Default2 = Default2;

function Default2$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Internals.Default2");
}

const Default1 = (0, _Types.declare)(function FSharpPlus_Internals_Default1() {});
exports.Default1 = Default1;

function Default1$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Internals.Default1");
}

const Errors$$$exnDivByZero = new Error();
exports.Errors$$$exnDivByZero = Errors$$$exnDivByZero;
const Errors$$$exnNoDivision = new Error("These numbers are not divisible in this domain.");
exports.Errors$$$exnNoDivision = Errors$$$exnNoDivision;
const Errors$$$exnSqrtOfNegative = new Error("Cannot calculate square root of a negative number");
exports.Errors$$$exnSqrtOfNegative = Errors$$$exnSqrtOfNegative;
const Errors$$$exnNoSqrt = new Error("No square root defined for this value in this domain.");
exports.Errors$$$exnNoSqrt = Errors$$$exnNoSqrt;
const Errors$$$exnNoSubtraction = new Error("No subtraction defined for these values in this domain.");
exports.Errors$$$exnNoSubtraction = Errors$$$exnNoSubtraction;

function BigInteger$$$trySqrtRem(x) {
  if ((0, _Util.sign)((0, _BigInt.toInt32)(x)) === -1) {
    return new _Option.Result(1, "Error", Errors$$$exnSqrtOfNegative);
  } else {
    const loop = function loop($previous$$4) {
      loop: while (true) {
        const previous = $previous$$4;
        const current = (0, _BigInt.op_RightShift)((0, _BigInt.op_Addition)(previous, (0, _BigInt.op_Division)(x, previous)), 1);

        if ((0, _BigInt.compare)((0, _BigInt.abs)((0, _BigInt.op_Subtraction)(previous, current)), (0, _BigInt.fromInt32)(2)) < 0) {
          return current;
        } else {
          $previous$$4 = current;
          continue loop;
        }

        break;
      }
    };

    const guess = Math.pow((0, _BigInt.fromInt32)(10), ~~(0, _BigInt.log10)((0, _BigInt.op_Addition)(x, (0, _BigInt.fromOne)())) + 1 >> 1);
    const r = loop(guess);
    const r2 = (0, _BigInt.op_Multiply)(r, r);
    const matchValue = (0, _BigInt.compare)(r2, x) | 0;

    switch (matchValue) {
      case 0:
        {
          return new _Option.Result(0, "Ok", [r, (0, _BigInt.fromZero)()]);
        }

      case 1:
        {
          const root = (0, _BigInt.op_Subtraction)(r, (0, _BigInt.fromOne)());
          return new _Option.Result(0, "Ok", [root, (0, _BigInt.op_Subtraction)(x, (0, _BigInt.op_Multiply)(root, root))]);
        }

      default:
        {
          return new _Option.Result(0, "Ok", [r, (0, _BigInt.op_Subtraction)(x, r2)]);
        }
    }
  }
}

const Id$00601 = (0, _Types.declare)(function FSharpPlus_Internals_Id(v) {
  const $this$$1 = this;
  $this$$1.value = v;
});
exports.Id$00601 = Id$00601;

function Id$00601$reflection($gen$$5) {
  return (0, _Reflection.type)("FSharpPlus.Internals.Id`1", [$gen$$5]);
}

function Id$00601$$$$002Ector$$2B595(v) {
  return this instanceof Id$00601 ? Id$00601.call(this, v) : new Id$00601(v);
}

function Id$00601$$get_getValue(__) {
  return __.value;
}

function Id$$$run(x$$1) {
  return Id$00601$$get_getValue(x$$1);
}

function Id$$$map(f, x$$2) {
  return Id$00601$$$$002Ector$$2B595(f(Id$00601$$get_getValue(x$$2)));
}

function Id$$$create(x$$3) {
  return Id$00601$$$$002Ector$$2B595(x$$3);
}

const Id0 = (0, _Types.declare)(function FSharpPlus_Internals_Id0(v$$1) {
  const $this$$2 = this;
  $this$$2.value = v$$1;
});
exports.Id0 = Id0;

function Id0$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Internals.Id0");
}

function Id0$$$$002Ector$$Z721C83C5(v$$1) {
  return this instanceof Id0 ? Id0.call(this, v$$1) : new Id0(v$$1);
}

function Id0$$get_getValue(__$$1) {
  return __$$1.value;
}

const Either$00602 = (0, _Types.declare)(function FSharpPlus_Internals_Either(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Either$00602 = Either$00602;

function Either$00602$reflection($gen$$12, $gen$$13) {
  return (0, _Reflection.union)("FSharpPlus.Internals.Either`2", [$gen$$12, $gen$$13], Either$00602, () => [["Left", [$gen$$12]], ["Right", [$gen$$13]]]);
}

const DmStruct = (0, _Types.declare)(function FSharpPlus_Internals_DmStruct() {}, _Types.Record);
exports.DmStruct = DmStruct;

function DmStruct$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Internals.DmStruct");
}

const BitConverter = (0, _Types.declare)(function FSharpPlus_Internals_BitConverter() {});
exports.BitConverter = BitConverter;

function BitConverter$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Internals.BitConverter");
}

function BitConverter$$$GetBytes$$Z1FBCCD16(value) {
  return (0, _Array.singleton)(value ? 1 : 0, Uint8Array);
}

function BitConverter$$$GetHexValue$$Z524259A4(i) {
  if (i < 10) {
    return String.fromCharCode(i) + "0";
  } else {
    return String.fromCharCode(i - 10) + "A";
  }
}

function BitConverter$$$ToString$$1D3E19A2(value$$1, startIndex, length) {
  if (value$$1 == null) {
    (0, _FSharp.Operators$$$NullArg)("value");
  }

  const arrayLen = value$$1.length | 0;

  if (startIndex >= value$$1.length) {
    const exn = new Error("startIndex", "ArgumentOutOfRange_StartIndex");
    throw exn;
  }

  if (length < 0) {
    const exn$$1 = new Error("length", "ArgumentOutOfRange_GenericPositive");
    throw exn$$1;
  }

  if (startIndex > arrayLen - length) {
    const exn$$2 = new Error("Arg_ArrayPlusOffTooSmall");
    throw exn$$2;
  }

  if (length === 0) {
    return "";
  } else {
    const chArray = (0, _Array.fill)(new Array(length * 3), 0, length * 3, "");
    let index = startIndex | 0;
    const inputSequence = (0, _Seq.rangeNumber)(0, 3, 3 * length - 1);
    (0, _Seq.iterate)(function (i$$1) {
      const b = ~~value$$1[index] | 0;
      index = index + 1;
      chArray[i$$1] = BitConverter$$$GetHexValue$$Z524259A4(~~(b / 16));
      chArray[i$$1 + 1] = BitConverter$$$GetHexValue$$Z524259A4(b % 16);
      chArray[i$$1 + 2] = "-";
    }, inputSequence);
    return chArray.join("").substr(0, chArray.length - 1);
  }
}

function BitConverter$$$ToString$$6C95DA22(value$$2) {
  if (value$$2 == null) {
    (0, _FSharp.Operators$$$NullArg)("value");
  }

  return BitConverter$$$ToString$$1D3E19A2(value$$2, 0, value$$2.length);
}

function BitConverter$$$ToString$$52ECB83E(value$$3, startIndex$$1) {
  if (value$$3 == null) {
    (0, _FSharp.Operators$$$NullArg)("value");
  }

  return BitConverter$$$ToString$$1D3E19A2(value$$3, startIndex$$1, value$$3.length - startIndex$$1);
}