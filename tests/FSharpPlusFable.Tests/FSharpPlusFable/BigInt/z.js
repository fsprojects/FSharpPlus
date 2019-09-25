"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.BigInteger$reflection = BigInteger$reflection;
exports.BigInteger$$$$002Ector$$Z2BE94A1 = BigInteger$$$$002Ector$$Z2BE94A1;
exports.BigInteger$$$nat$$Z67CCE57D = BigInteger$$$nat$$Z67CCE57D;
exports.BigInteger$$$create$$Z2BE94A1 = BigInteger$$$create$$Z2BE94A1;
exports.BigInteger$$$posn$$Z67CCE57D = BigInteger$$$posn$$Z67CCE57D;
exports.BigInteger$$$negn$$Z67CCE57D = BigInteger$$$negn$$Z67CCE57D;
exports.BigInteger$$get_Sign = BigInteger$$get_Sign;
exports.BigInteger$$get_SignInt = BigInteger$$get_SignInt;
exports.BigInteger$$get_V = BigInteger$$get_V;
exports.BigInteger$$$op_Equality$$56F059C0 = BigInteger$$$op_Equality$$56F059C0;
exports.BigInteger$$$op_Inequality$$56F059C0 = BigInteger$$$op_Inequality$$56F059C0;
exports.BigInteger$$$op_LessThan$$56F059C0 = BigInteger$$$op_LessThan$$56F059C0;
exports.BigInteger$$$op_GreaterThan$$56F059C0 = BigInteger$$$op_GreaterThan$$56F059C0;
exports.BigInteger$$$compare$$56F059C0 = BigInteger$$$compare$$56F059C0;
exports.BigInteger$$$hash$$Z665282C2 = BigInteger$$$hash$$Z665282C2;
exports.BigInteger$$get_StructuredDisplayString = BigInteger$$get_StructuredDisplayString;
exports.BigInteger$$$$002Ector$$Z524259A4 = BigInteger$$$$002Ector$$Z524259A4;
exports.BigInteger$$$$002Ector$$Z524259C1 = BigInteger$$$$002Ector$$Z524259C1;
exports.BigInteger$$$get_One = BigInteger$$$get_One;
exports.BigInteger$$$get_Two = BigInteger$$$get_Two;
exports.BigInteger$$$get_Zero = BigInteger$$$get_Zero;
exports.BigInteger$$$op_UnaryNegation$$Z665282C2 = BigInteger$$$op_UnaryNegation$$Z665282C2;
exports.BigInteger$$$Scale$$Z320F31E = BigInteger$$$Scale$$Z320F31E;
exports.BigInteger$$$subnn$$6A57060 = BigInteger$$$subnn$$6A57060;
exports.BigInteger$$$addnn$$6A57060 = BigInteger$$$addnn$$6A57060;
exports.BigInteger$$get_IsZero = BigInteger$$get_IsZero;
exports.BigInteger$$get_IsOne = BigInteger$$get_IsOne;
exports.BigInteger$$$op_Addition$$56F059C0 = BigInteger$$$op_Addition$$56F059C0;
exports.BigInteger$$$op_Subtraction$$56F059C0 = BigInteger$$$op_Subtraction$$56F059C0;
exports.BigInteger$$$op_Multiply$$56F059C0 = BigInteger$$$op_Multiply$$56F059C0;
exports.BigInteger$$$DivRem$$56F059C0 = BigInteger$$$DivRem$$56F059C0;
exports.BigInteger$$$op_Division$$56F059C0 = BigInteger$$$op_Division$$56F059C0;
exports.BigInteger$$$op_Modulus$$56F059C0 = BigInteger$$$op_Modulus$$56F059C0;
exports.BigInteger$$$op_RightShift$$62E082A2 = BigInteger$$$op_RightShift$$62E082A2;
exports.BigInteger$$$op_LeftShift$$62E082A2 = BigInteger$$$op_LeftShift$$62E082A2;
exports.BigInteger$$$op_BitwiseAnd$$56F059C0 = BigInteger$$$op_BitwiseAnd$$56F059C0;
exports.BigInteger$$$op_BitwiseOr$$56F059C0 = BigInteger$$$op_BitwiseOr$$56F059C0;
exports.BigInteger$$$op_ExclusiveOr$$56F059C0 = BigInteger$$$op_ExclusiveOr$$56F059C0;
exports.BigInteger$$$GreatestCommonDivisor$$56F059C0 = BigInteger$$$GreatestCommonDivisor$$56F059C0;
exports.BigInteger$$get_IsNegative = BigInteger$$get_IsNegative;
exports.BigInteger$$get_IsPositive = BigInteger$$get_IsPositive;
exports.BigInteger$$$Abs$$Z665282C2 = BigInteger$$$Abs$$Z665282C2;
exports.BigInteger$$$op_LessThanOrEqual$$56F059C0 = BigInteger$$$op_LessThanOrEqual$$56F059C0;
exports.BigInteger$$$op_GreaterThanOrEqual$$56F059C0 = BigInteger$$$op_GreaterThanOrEqual$$56F059C0;
exports.BigInteger$$$Pow$$62E082A2 = BigInteger$$$Pow$$62E082A2;
exports.BigInteger$$get_ToInt32 = BigInteger$$get_ToInt32;
exports.BigInteger$$get_ToUInt32 = BigInteger$$get_ToUInt32;
exports.BigInteger$$get_ToInt64 = BigInteger$$get_ToInt64;
exports.BigInteger$$get_ToUInt64 = BigInteger$$get_ToUInt64;
exports.BigInteger$$get_ToDouble = BigInteger$$get_ToDouble;
exports.BigInteger$$get_ToSByte = BigInteger$$get_ToSByte;
exports.BigInteger$$get_ToByte = BigInteger$$get_ToByte;
exports.BigInteger$$get_ToInt16 = BigInteger$$get_ToInt16;
exports.BigInteger$$get_ToUInt16 = BigInteger$$get_ToUInt16;
exports.BigInteger$$get_ToSingle = BigInteger$$get_ToSingle;
exports.BigInteger$$get_ToDecimal = BigInteger$$get_ToDecimal;
exports.BigInteger$$$Parse$$Z721C83C5 = BigInteger$$$Parse$$Z721C83C5;
exports.BigInteger$$get_IsSmall = BigInteger$$get_IsSmall;
exports.BigInteger$$$Factorial$$Z665282C2 = BigInteger$$$Factorial$$Z665282C2;
exports.BigInteger$$$op_UnaryPlus$$Z665282C2 = BigInteger$$$op_UnaryPlus$$Z665282C2;
exports.BigInteger$$$FromInt64$$Z524259C1 = BigInteger$$$FromInt64$$Z524259C1;
exports.BigInteger$$$FromInt32$$Z524259A4 = BigInteger$$$FromInt32$$Z524259A4;
exports.BigInteger = void 0;

var _Types = require("../fable-library.2.3.24/Types");

var _Reflection = require("../fable-library.2.3.24/Reflection");

var _n = require("./n");

var _Array = require("../fable-library.2.3.24/Array");

var _Long = require("../fable-library.2.3.24/Long");

var _Int = require("../fable-library.2.3.24/Int32");

var _Decimal = _interopRequireDefault(require("../fable-library.2.3.24/Decimal"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

const BigInteger = (0, _Types.declare)(function BigInt_BigInteger(signInt, v) {
  const $this$$1 = this;
  $this$$1.signInt = signInt;
  $this$$1.v = v;
}, _Types.Record);
exports.BigInteger = BigInteger;

function BigInteger$reflection() {
  return (0, _Reflection.type)("BigInt.BigInteger");
}

function BigInteger$$$$002Ector$$Z2BE94A1(signInt, v) {
  return this instanceof BigInteger ? BigInteger.call(this, signInt, v) : new BigInteger(signInt, v);
}

(function BigInteger$$$$002Ecctor() {
  BigInteger.smallLim = 4096;
  BigInteger.smallPosTab = (0, _Array.initialize)(BigInteger.smallLim, _n.BigNatModule$$$ofInt32, Array);
  BigInteger.one = BigInteger$$$$002Ector$$Z524259A4(1);
  BigInteger.two = BigInteger$$$$002Ector$$Z524259A4(2);
  BigInteger.zero = BigInteger$$$$002Ector$$Z524259A4(0);
})();

function BigInteger$$$nat$$Z67CCE57D(n$$1) {
  if ((0, _n.BigNatModule$$$isSmall)(n$$1) ? (0, _n.BigNatModule$$$getSmall)(n$$1) < BigInteger.smallLim : false) {
    return BigInteger.smallPosTab[(0, _n.BigNatModule$$$getSmall)(n$$1)];
  } else {
    return n$$1;
  }
}

function BigInteger$$$create$$Z2BE94A1(s, n$$2) {
  return BigInteger$$$$002Ector$$Z2BE94A1(s, BigInteger$$$nat$$Z67CCE57D(n$$2));
}

function BigInteger$$$posn$$Z67CCE57D(n$$3) {
  return BigInteger$$$$002Ector$$Z2BE94A1(1, BigInteger$$$nat$$Z67CCE57D(n$$3));
}

function BigInteger$$$negn$$Z67CCE57D(n$$4) {
  return BigInteger$$$$002Ector$$Z2BE94A1(-1, BigInteger$$$nat$$Z67CCE57D(n$$4));
}

function BigInteger$$get_Sign(x) {
  if (BigInteger$$get_IsZero(x)) {
    return 0;
  } else {
    return x.signInt | 0;
  }
}

function BigInteger$$get_SignInt(x$$1) {
  return x$$1.signInt;
}

function BigInteger$$get_V(x$$2) {
  return x$$2.v;
}

function BigInteger$$$op_Equality$$56F059C0(x$$3, y) {
  const matchValue = [BigInteger$$get_SignInt(x$$3), BigInteger$$get_SignInt(y)];
  var $target$$12;

  if (matchValue[0] === -1) {
    if (matchValue[1] === -1) {
      $target$$12 = 1;
    } else if (matchValue[1] === 0) {
      $target$$12 = 8;
    } else if (matchValue[1] === 1) {
      $target$$12 = 3;
    } else {
      $target$$12 = 9;
    }
  } else if (matchValue[0] === 0) {
    if (matchValue[1] === -1) {
      $target$$12 = 6;
    } else if (matchValue[1] === 0) {
      $target$$12 = 4;
    } else if (matchValue[1] === 1) {
      $target$$12 = 5;
    } else {
      $target$$12 = 9;
    }
  } else if (matchValue[0] === 1) {
    if (matchValue[1] === -1) {
      $target$$12 = 2;
    } else if (matchValue[1] === 0) {
      $target$$12 = 7;
    } else if (matchValue[1] === 1) {
      $target$$12 = 0;
    } else {
      $target$$12 = 9;
    }
  } else {
    $target$$12 = 9;
  }

  switch ($target$$12) {
    case 0:
      {
        return (0, _n.BigNatModule$$$equal)(BigInteger$$get_V(x$$3), BigInteger$$get_V(y));
      }

    case 1:
      {
        return (0, _n.BigNatModule$$$equal)(BigInteger$$get_V(x$$3), BigInteger$$get_V(y));
      }

    case 2:
      {
        if ((0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(x$$3))) {
          return (0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(y));
        } else {
          return false;
        }
      }

    case 3:
      {
        if ((0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(x$$3))) {
          return (0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(y));
        } else {
          return false;
        }
      }

    case 4:
      {
        return true;
      }

    case 5:
      {
        return (0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(y));
      }

    case 6:
      {
        return (0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(y));
      }

    case 7:
      {
        return (0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(x$$3));
      }

    case 8:
      {
        return (0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(x$$3));
      }

    case 9:
      {
        throw new Error("signs should be +/- 1 or 0\\nParameter name: x");
      }
  }
}

function BigInteger$$$op_Inequality$$56F059C0(x$$4, y$$1) {
  return !BigInteger$$$op_Equality$$56F059C0(x$$4, y$$1);
}

function BigInteger$$$op_LessThan$$56F059C0(x$$5, y$$2) {
  const matchValue$$1 = [BigInteger$$get_SignInt(x$$5), BigInteger$$get_SignInt(y$$2)];
  var $target$$17;

  if (matchValue$$1[0] === -1) {
    if (matchValue$$1[1] === -1) {
      $target$$17 = 1;
    } else if (matchValue$$1[1] === 0) {
      $target$$17 = 8;
    } else if (matchValue$$1[1] === 1) {
      $target$$17 = 3;
    } else {
      $target$$17 = 9;
    }
  } else if (matchValue$$1[0] === 0) {
    if (matchValue$$1[1] === -1) {
      $target$$17 = 6;
    } else if (matchValue$$1[1] === 0) {
      $target$$17 = 4;
    } else if (matchValue$$1[1] === 1) {
      $target$$17 = 5;
    } else {
      $target$$17 = 9;
    }
  } else if (matchValue$$1[0] === 1) {
    if (matchValue$$1[1] === -1) {
      $target$$17 = 2;
    } else if (matchValue$$1[1] === 0) {
      $target$$17 = 7;
    } else if (matchValue$$1[1] === 1) {
      $target$$17 = 0;
    } else {
      $target$$17 = 9;
    }
  } else {
    $target$$17 = 9;
  }

  switch ($target$$17) {
    case 0:
      {
        return (0, _n.BigNatModule$$$lt)(BigInteger$$get_V(x$$5), BigInteger$$get_V(y$$2));
      }

    case 1:
      {
        return (0, _n.BigNatModule$$$lt)(BigInteger$$get_V(y$$2), BigInteger$$get_V(x$$5));
      }

    case 2:
      {
        return false;
      }

    case 3:
      {
        if (!(0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(x$$5))) {
          return true;
        } else {
          return !(0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(y$$2));
        }
      }

    case 4:
      {
        return false;
      }

    case 5:
      {
        return !(0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(y$$2));
      }

    case 6:
      {
        return false;
      }

    case 7:
      {
        return false;
      }

    case 8:
      {
        return !(0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(x$$5));
      }

    case 9:
      {
        throw new Error("signs should be +/- 1 or 0\\nParameter name: x");
      }
  }
}

function BigInteger$$$op_GreaterThan$$56F059C0(x$$6, y$$3) {
  const matchValue$$2 = [BigInteger$$get_SignInt(x$$6), BigInteger$$get_SignInt(y$$3)];
  var $target$$20;

  if (matchValue$$2[0] === -1) {
    if (matchValue$$2[1] === -1) {
      $target$$20 = 1;
    } else if (matchValue$$2[1] === 0) {
      $target$$20 = 8;
    } else if (matchValue$$2[1] === 1) {
      $target$$20 = 3;
    } else {
      $target$$20 = 9;
    }
  } else if (matchValue$$2[0] === 0) {
    if (matchValue$$2[1] === -1) {
      $target$$20 = 6;
    } else if (matchValue$$2[1] === 0) {
      $target$$20 = 4;
    } else if (matchValue$$2[1] === 1) {
      $target$$20 = 5;
    } else {
      $target$$20 = 9;
    }
  } else if (matchValue$$2[0] === 1) {
    if (matchValue$$2[1] === -1) {
      $target$$20 = 2;
    } else if (matchValue$$2[1] === 0) {
      $target$$20 = 7;
    } else if (matchValue$$2[1] === 1) {
      $target$$20 = 0;
    } else {
      $target$$20 = 9;
    }
  } else {
    $target$$20 = 9;
  }

  switch ($target$$20) {
    case 0:
      {
        return (0, _n.BigNatModule$$$gt)(BigInteger$$get_V(x$$6), BigInteger$$get_V(y$$3));
      }

    case 1:
      {
        return (0, _n.BigNatModule$$$gt)(BigInteger$$get_V(y$$3), BigInteger$$get_V(x$$6));
      }

    case 2:
      {
        if (!(0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(x$$6))) {
          return true;
        } else {
          return !(0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(y$$3));
        }
      }

    case 3:
      {
        return false;
      }

    case 4:
      {
        return false;
      }

    case 5:
      {
        return false;
      }

    case 6:
      {
        return !(0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(y$$3));
      }

    case 7:
      {
        return !(0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(x$$6));
      }

    case 8:
      {
        return false;
      }

    case 9:
      {
        throw new Error("signs should be +/- 1 or 0\\nParameter name: x");
      }
  }
}

function BigInteger$$$compare$$56F059C0(n$$5, nn) {
  if (BigInteger$$$op_LessThan$$56F059C0(n$$5, nn)) {
    return -1 | 0;
  } else if (BigInteger$$$op_Equality$$56F059C0(n$$5, nn)) {
    return 0;
  } else {
    return 1;
  }
}

function BigInteger$$$hash$$Z665282C2(z) {
  if (BigInteger$$get_SignInt(z) === 0) {
    return 1;
  } else {
    return BigInteger$$get_SignInt(z) + (0, _n.BigNatModule$$$hash)(BigInteger$$get_V(z)) | 0;
  }
}

BigInteger.prototype.toString = function () {
  const x$$7 = this;
  const matchValue$$3 = BigInteger$$get_SignInt(x$$7) | 0;

  switch (matchValue$$3) {
    case -1:
      {
        return (0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(x$$7)) ? "0" : "-" + (0, _n.BigNatModule$$$toString)(BigInteger$$get_V(x$$7));
      }

    case 0:
      {
        return "0";
      }

    case 1:
      {
        return (0, _n.BigNatModule$$$toString)(BigInteger$$get_V(x$$7));
      }

    default:
      {
        throw new Error("signs should be +/- 1 or 0");
      }
  }
};

function BigInteger$$get_StructuredDisplayString(x$$8) {
  return String(x$$8);
}

BigInteger.prototype.Equals = function (obj) {
  const this$ = this;
  return obj instanceof BigInteger ? BigInteger$$$op_Equality$$56F059C0(this$, obj) : false;
};

BigInteger.prototype.GetHashCode = function () {
  const x$$9 = this;
  return BigInteger$$$hash$$Z665282C2(x$$9) | 0;
};

function BigInteger$$$$002Ector$$Z524259A4(n$$6) {
  if (n$$6 >= 0) {
    return BigInteger$$$$002Ector$$Z2BE94A1.call(this, 1, BigInteger$$$nat$$Z67CCE57D((0, _n.BigNatModule$$$ofInt32)(n$$6)));
  } else if (n$$6 === -2147483648) {
    return BigInteger$$$$002Ector$$Z2BE94A1.call(this, -1, BigInteger$$$nat$$Z67CCE57D((0, _n.BigNatModule$$$ofInt64)((0, _Long.op_UnaryNegation)((0, _Long.fromInteger)(n$$6, false, 2)))));
  } else {
    return BigInteger$$$$002Ector$$Z2BE94A1.call(this, -1, BigInteger$$$nat$$Z67CCE57D((0, _n.BigNatModule$$$ofInt32)((0, _Int.op_UnaryNegation_Int32)(n$$6))));
  }
}

function BigInteger$$$$002Ector$$Z524259C1(n$$7) {
  if ((0, _Long.compare)(n$$7, (0, _Long.fromBits)(0, 0, false)) >= 0) {
    return BigInteger$$$$002Ector$$Z2BE94A1.call(this, 1, BigInteger$$$nat$$Z67CCE57D((0, _n.BigNatModule$$$ofInt64)(n$$7)));
  } else if ((0, _Long.equals)(n$$7, (0, _Long.fromBits)(0, 2147483648, false))) {
    return BigInteger$$$$002Ector$$Z2BE94A1.call(this, -1, BigInteger$$$nat$$Z67CCE57D((0, _n.BigNatModule$$$add)((0, _n.BigNatModule$$$ofInt64)((0, _Long.fromBits)(4294967295, 2147483647, false)), _n.BigNatModule$$$one)));
  } else {
    return BigInteger$$$$002Ector$$Z2BE94A1.call(this, -1, BigInteger$$$nat$$Z67CCE57D((0, _n.BigNatModule$$$ofInt64)((0, _Long.op_UnaryNegation)(n$$7))));
  }
}

function BigInteger$$$get_One() {
  return BigInteger.one;
}

function BigInteger$$$get_Two() {
  return BigInteger.two;
}

function BigInteger$$$get_Zero() {
  return BigInteger.zero;
}

function BigInteger$$$op_UnaryNegation$$Z665282C2(z$$1) {
  const matchValue$$4 = BigInteger$$get_SignInt(z$$1) | 0;

  if (matchValue$$4 === 0) {
    return BigInteger$$$get_Zero();
  } else {
    return BigInteger$$$create$$Z2BE94A1((0, _Int.op_UnaryNegation_Int32)(matchValue$$4), BigInteger$$get_V(z$$1));
  }
}

function BigInteger$$$Scale$$Z320F31E(k, z$$2) {
  if (BigInteger$$get_SignInt(z$$2) === 0) {
    return BigInteger$$$get_Zero();
  } else if (k < 0) {
    return BigInteger$$$create$$Z2BE94A1((0, _Int.op_UnaryNegation_Int32)(BigInteger$$get_SignInt(z$$2)), (0, _n.BigNatModule$$$scale)((0, _Int.op_UnaryNegation_Int32)(k), BigInteger$$get_V(z$$2)));
  } else {
    return BigInteger$$$create$$Z2BE94A1(BigInteger$$get_SignInt(z$$2), (0, _n.BigNatModule$$$scale)(k, BigInteger$$get_V(z$$2)));
  }
}

function BigInteger$$$subnn$$6A57060(nx, ny) {
  if ((0, _n.BigNatModule$$$gte)(nx, ny)) {
    return BigInteger$$$posn$$Z67CCE57D((0, _n.BigNatModule$$$sub)(nx, ny));
  } else {
    return BigInteger$$$negn$$Z67CCE57D((0, _n.BigNatModule$$$sub)(ny, nx));
  }
}

function BigInteger$$$addnn$$6A57060(nx$$1, ny$$1) {
  return BigInteger$$$posn$$Z67CCE57D((0, _n.BigNatModule$$$add)(nx$$1, ny$$1));
}

function BigInteger$$get_IsZero(x$$10) {
  if (BigInteger$$get_SignInt(x$$10) === 0) {
    return true;
  } else {
    return (0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(x$$10));
  }
}

function BigInteger$$get_IsOne(x$$11) {
  if (BigInteger$$get_SignInt(x$$11) === 1) {
    return (0, _n.BigNatModule$$$isOne)(BigInteger$$get_V(x$$11));
  } else {
    return false;
  }
}

function BigInteger$$$op_Addition$$56F059C0(x$$12, y$$4) {
  if (BigInteger$$get_IsZero(y$$4)) {
    return x$$12;
  } else if (BigInteger$$get_IsZero(x$$12)) {
    return y$$4;
  } else {
    const matchValue$$5 = [BigInteger$$get_SignInt(x$$12), BigInteger$$get_SignInt(y$$4)];
    var $target$$38;

    if (matchValue$$5[0] === -1) {
      if (matchValue$$5[1] === -1) {
        $target$$38 = 1;
      } else if (matchValue$$5[1] === 1) {
        $target$$38 = 3;
      } else {
        $target$$38 = 4;
      }
    } else if (matchValue$$5[0] === 1) {
      if (matchValue$$5[1] === -1) {
        $target$$38 = 2;
      } else if (matchValue$$5[1] === 1) {
        $target$$38 = 0;
      } else {
        $target$$38 = 4;
      }
    } else {
      $target$$38 = 4;
    }

    switch ($target$$38) {
      case 0:
        {
          return BigInteger$$$addnn$$6A57060(BigInteger$$get_V(x$$12), BigInteger$$get_V(y$$4));
        }

      case 1:
        {
          return BigInteger$$$op_UnaryNegation$$Z665282C2(BigInteger$$$addnn$$6A57060(BigInteger$$get_V(x$$12), BigInteger$$get_V(y$$4)));
        }

      case 2:
        {
          return BigInteger$$$subnn$$6A57060(BigInteger$$get_V(x$$12), BigInteger$$get_V(y$$4));
        }

      case 3:
        {
          return BigInteger$$$subnn$$6A57060(BigInteger$$get_V(y$$4), BigInteger$$get_V(x$$12));
        }

      case 4:
        {
          throw new Error("signs should be +/- 1\\nParameter name: x");
        }
    }
  }
}

function BigInteger$$$op_Subtraction$$56F059C0(x$$13, y$$5) {
  if (BigInteger$$get_IsZero(y$$5)) {
    return x$$13;
  } else if (BigInteger$$get_IsZero(x$$13)) {
    return BigInteger$$$op_UnaryNegation$$Z665282C2(y$$5);
  } else {
    const matchValue$$6 = [BigInteger$$get_SignInt(x$$13), BigInteger$$get_SignInt(y$$5)];
    var $target$$41;

    if (matchValue$$6[0] === -1) {
      if (matchValue$$6[1] === -1) {
        $target$$41 = 1;
      } else if (matchValue$$6[1] === 1) {
        $target$$41 = 3;
      } else {
        $target$$41 = 4;
      }
    } else if (matchValue$$6[0] === 1) {
      if (matchValue$$6[1] === -1) {
        $target$$41 = 2;
      } else if (matchValue$$6[1] === 1) {
        $target$$41 = 0;
      } else {
        $target$$41 = 4;
      }
    } else {
      $target$$41 = 4;
    }

    switch ($target$$41) {
      case 0:
        {
          return BigInteger$$$subnn$$6A57060(BigInteger$$get_V(x$$13), BigInteger$$get_V(y$$5));
        }

      case 1:
        {
          return BigInteger$$$subnn$$6A57060(BigInteger$$get_V(y$$5), BigInteger$$get_V(x$$13));
        }

      case 2:
        {
          return BigInteger$$$addnn$$6A57060(BigInteger$$get_V(x$$13), BigInteger$$get_V(y$$5));
        }

      case 3:
        {
          return BigInteger$$$op_UnaryNegation$$Z665282C2(BigInteger$$$addnn$$6A57060(BigInteger$$get_V(x$$13), BigInteger$$get_V(y$$5)));
        }

      case 4:
        {
          throw new Error("signs should be +/- 1\\nParameter name: x");
        }
    }
  }
}

function BigInteger$$$op_Multiply$$56F059C0(x$$14, y$$6) {
  if (BigInteger$$get_IsZero(x$$14)) {
    return x$$14;
  } else if (BigInteger$$get_IsZero(y$$6)) {
    return y$$6;
  } else if (BigInteger$$get_IsOne(x$$14)) {
    return y$$6;
  } else if (BigInteger$$get_IsOne(y$$6)) {
    return x$$14;
  } else {
    const m = (0, _n.BigNatModule$$$mul)(BigInteger$$get_V(x$$14), BigInteger$$get_V(y$$6));
    return BigInteger$$$create$$Z2BE94A1(BigInteger$$get_SignInt(x$$14) * BigInteger$$get_SignInt(y$$6), m);
  }
}

function BigInteger$$$DivRem$$56F059C0(x$$15, y$$7) {
  if (BigInteger$$get_IsZero(y$$7)) {
    throw new Error();
  }

  if (BigInteger$$get_IsZero(x$$15)) {
    return [BigInteger$$$get_Zero(), BigInteger$$$get_Zero()];
  } else {
    const patternInput = (0, _n.BigNatModule$$$divmod)(BigInteger$$get_V(x$$15), BigInteger$$get_V(y$$7));
    const matchValue$$7 = [BigInteger$$get_SignInt(x$$15), BigInteger$$get_SignInt(y$$7)];
    var $target$$46;

    if (matchValue$$7[0] === -1) {
      if (matchValue$$7[1] === -1) {
        $target$$46 = 1;
      } else if (matchValue$$7[1] === 1) {
        $target$$46 = 3;
      } else {
        $target$$46 = 4;
      }
    } else if (matchValue$$7[0] === 1) {
      if (matchValue$$7[1] === -1) {
        $target$$46 = 2;
      } else if (matchValue$$7[1] === 1) {
        $target$$46 = 0;
      } else {
        $target$$46 = 4;
      }
    } else {
      $target$$46 = 4;
    }

    switch ($target$$46) {
      case 0:
        {
          return [BigInteger$$$posn$$Z67CCE57D(patternInput[0]), BigInteger$$$posn$$Z67CCE57D(patternInput[1])];
        }

      case 1:
        {
          return [BigInteger$$$posn$$Z67CCE57D(patternInput[0]), BigInteger$$$negn$$Z67CCE57D(patternInput[1])];
        }

      case 2:
        {
          return [BigInteger$$$negn$$Z67CCE57D(patternInput[0]), BigInteger$$$posn$$Z67CCE57D(patternInput[1])];
        }

      case 3:
        {
          return [BigInteger$$$negn$$Z67CCE57D(patternInput[0]), BigInteger$$$negn$$Z67CCE57D(patternInput[1])];
        }

      case 4:
        {
          throw new Error("signs should be +/- 1\\nParameter name: x");
        }
    }
  }
}

function BigInteger$$$op_Division$$56F059C0(x$$16, y$$8) {
  const tuple = BigInteger$$$DivRem$$56F059C0(x$$16, y$$8);
  return tuple[0];
}

function BigInteger$$$op_Modulus$$56F059C0(x$$17, y$$9) {
  const tuple$$1 = BigInteger$$$DivRem$$56F059C0(x$$17, y$$9);
  return tuple$$1[1];
}

function BigInteger$$$op_RightShift$$62E082A2(x$$18, y$$10) {
  return BigInteger$$$op_Division$$56F059C0(x$$18, BigInteger$$$Pow$$62E082A2(BigInteger$$$get_Two(), y$$10));
}

function BigInteger$$$op_LeftShift$$62E082A2(x$$19, y$$11) {
  return BigInteger$$$op_Multiply$$56F059C0(x$$19, BigInteger$$$Pow$$62E082A2(BigInteger$$$get_Two(), y$$11));
}

function BigInteger$$$op_BitwiseAnd$$56F059C0(x$$20, y$$12) {
  return BigInteger$$$posn$$Z67CCE57D((0, _n.BigNatModule$$$bitAnd)(BigInteger$$get_V(x$$20), BigInteger$$get_V(y$$12)));
}

function BigInteger$$$op_BitwiseOr$$56F059C0(x$$21, y$$13) {
  return BigInteger$$$posn$$Z67CCE57D((0, _n.BigNatModule$$$bitOr)(BigInteger$$get_V(x$$21), BigInteger$$get_V(y$$13)));
}

function BigInteger$$$op_ExclusiveOr$$56F059C0(x$$22, y$$14) {
  return BigInteger$$$posn$$Z67CCE57D((0, _n.BigNatModule$$$bitXor)(BigInteger$$get_V(x$$22), BigInteger$$get_V(y$$14)));
}

function BigInteger$$$GreatestCommonDivisor$$56F059C0(x$$23, y$$15) {
  const matchValue$$8 = [BigInteger$$get_SignInt(x$$23), BigInteger$$get_SignInt(y$$15)];

  if (matchValue$$8[0] === 0) {
    if (matchValue$$8[1] === 0) {
      return BigInteger$$$get_Zero();
    } else {
      return BigInteger$$$posn$$Z67CCE57D(BigInteger$$get_V(y$$15));
    }
  } else if (matchValue$$8[1] === 0) {
    return BigInteger$$$posn$$Z67CCE57D(BigInteger$$get_V(x$$23));
  } else {
    return BigInteger$$$posn$$Z67CCE57D((0, _n.BigNatModule$$$hcf)(BigInteger$$get_V(x$$23), BigInteger$$get_V(y$$15)));
  }
}

function BigInteger$$get_IsNegative(x$$24) {
  if (BigInteger$$get_SignInt(x$$24) === -1) {
    return !BigInteger$$get_IsZero(x$$24);
  } else {
    return false;
  }
}

function BigInteger$$get_IsPositive(x$$25) {
  if (BigInteger$$get_SignInt(x$$25) === 1) {
    return !BigInteger$$get_IsZero(x$$25);
  } else {
    return false;
  }
}

function BigInteger$$$Abs$$Z665282C2(x$$26) {
  if (BigInteger$$get_SignInt(x$$26) === -1) {
    return BigInteger$$$op_UnaryNegation$$Z665282C2(x$$26);
  } else {
    return x$$26;
  }
}

function BigInteger$$$op_LessThanOrEqual$$56F059C0(x$$27, y$$16) {
  const matchValue$$9 = [BigInteger$$get_SignInt(x$$27), BigInteger$$get_SignInt(y$$16)];
  var $target$$68;

  if (matchValue$$9[0] === -1) {
    if (matchValue$$9[1] === -1) {
      $target$$68 = 1;
    } else if (matchValue$$9[1] === 0) {
      $target$$68 = 6;
    } else if (matchValue$$9[1] === 1) {
      $target$$68 = 3;
    } else {
      $target$$68 = 9;
    }
  } else if (matchValue$$9[0] === 0) {
    if (matchValue$$9[1] === -1) {
      $target$$68 = 8;
    } else if (matchValue$$9[1] === 0) {
      $target$$68 = 4;
    } else if (matchValue$$9[1] === 1) {
      $target$$68 = 7;
    } else {
      $target$$68 = 9;
    }
  } else if (matchValue$$9[0] === 1) {
    if (matchValue$$9[1] === -1) {
      $target$$68 = 2;
    } else if (matchValue$$9[1] === 0) {
      $target$$68 = 5;
    } else if (matchValue$$9[1] === 1) {
      $target$$68 = 0;
    } else {
      $target$$68 = 9;
    }
  } else {
    $target$$68 = 9;
  }

  switch ($target$$68) {
    case 0:
      {
        return (0, _n.BigNatModule$$$lte)(BigInteger$$get_V(x$$27), BigInteger$$get_V(y$$16));
      }

    case 1:
      {
        return (0, _n.BigNatModule$$$lte)(BigInteger$$get_V(y$$16), BigInteger$$get_V(x$$27));
      }

    case 2:
      {
        if ((0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(x$$27))) {
          return (0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(y$$16));
        } else {
          return false;
        }
      }

    case 3:
      {
        return true;
      }

    case 4:
      {
        return true;
      }

    case 5:
      {
        return (0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(x$$27));
      }

    case 6:
      {
        return true;
      }

    case 7:
      {
        return true;
      }

    case 8:
      {
        return (0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(y$$16));
      }

    case 9:
      {
        throw new Error("signs should be +/- 1 or 0\\nParameter name: x");
      }
  }
}

function BigInteger$$$op_GreaterThanOrEqual$$56F059C0(x$$28, y$$17) {
  const matchValue$$10 = [BigInteger$$get_SignInt(x$$28), BigInteger$$get_SignInt(y$$17)];
  var $target$$71;

  if (matchValue$$10[0] === -1) {
    if (matchValue$$10[1] === -1) {
      $target$$71 = 1;
    } else if (matchValue$$10[1] === 0) {
      $target$$71 = 6;
    } else if (matchValue$$10[1] === 1) {
      $target$$71 = 3;
    } else {
      $target$$71 = 9;
    }
  } else if (matchValue$$10[0] === 0) {
    if (matchValue$$10[1] === -1) {
      $target$$71 = 8;
    } else if (matchValue$$10[1] === 0) {
      $target$$71 = 4;
    } else if (matchValue$$10[1] === 1) {
      $target$$71 = 7;
    } else {
      $target$$71 = 9;
    }
  } else if (matchValue$$10[0] === 1) {
    if (matchValue$$10[1] === -1) {
      $target$$71 = 2;
    } else if (matchValue$$10[1] === 0) {
      $target$$71 = 5;
    } else if (matchValue$$10[1] === 1) {
      $target$$71 = 0;
    } else {
      $target$$71 = 9;
    }
  } else {
    $target$$71 = 9;
  }

  switch ($target$$71) {
    case 0:
      {
        return (0, _n.BigNatModule$$$gte)(BigInteger$$get_V(x$$28), BigInteger$$get_V(y$$17));
      }

    case 1:
      {
        return (0, _n.BigNatModule$$$gte)(BigInteger$$get_V(y$$17), BigInteger$$get_V(x$$28));
      }

    case 2:
      {
        return true;
      }

    case 3:
      {
        if ((0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(x$$28))) {
          return (0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(y$$17));
        } else {
          return false;
        }
      }

    case 4:
      {
        return true;
      }

    case 5:
      {
        return true;
      }

    case 6:
      {
        return (0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(x$$28));
      }

    case 7:
      {
        return (0, _n.BigNatModule$$$isZero)(BigInteger$$get_V(y$$17));
      }

    case 8:
      {
        return true;
      }

    case 9:
      {
        throw new Error("signs should be +/- 1 or 0\\nParameter name: x");
      }
  }
}

function BigInteger$$$Pow$$62E082A2(x$$29, y$$18) {
  if (y$$18 < 0) {
    throw new Error("y");
  }

  const matchValue$$11 = [BigInteger$$get_IsZero(x$$29), y$$18];

  if (matchValue$$11[0]) {
    if (matchValue$$11[1] === 0) {
      return BigInteger$$$get_One();
    } else {
      return BigInteger$$$get_Zero();
    }
  } else {
    const yval = BigInteger$$$$002Ector$$Z524259A4(y$$18);
    return BigInteger$$$create$$Z2BE94A1((0, _n.BigNatModule$$$isZero)((0, _n.BigNatModule$$$rem)(BigInteger$$get_V(yval), _n.BigNatModule$$$two)) ? 1 : BigInteger$$get_SignInt(x$$29), (0, _n.BigNatModule$$$pow)(BigInteger$$get_V(x$$29), BigInteger$$get_V(yval)));
  }
}

function BigInteger$$get_ToInt32(x$$30) {
  if (BigInteger$$get_IsZero(x$$30)) {
    return 0;
  } else {
    const u = (0, _n.BigNatModule$$$toUInt32)(BigInteger$$get_V(x$$30));

    if (u <= 2147483647 >>> 0) {
      return BigInteger$$get_SignInt(x$$30) * ~~u | 0;
    } else if (BigInteger$$get_SignInt(x$$30) === -1 ? u === 2147483647 + 1 >>> 0 : false) {
      return -2147483648 | 0;
    } else {
      throw new Error();
    }
  }
}

function BigInteger$$get_ToUInt32(x$$31) {
  if (BigInteger$$get_IsZero(x$$31)) {
    return 0;
  } else {
    return (0, _n.BigNatModule$$$toUInt32)(BigInteger$$get_V(x$$31));
  }
}

function BigInteger$$get_ToInt64(x$$32) {
  if (BigInteger$$get_IsZero(x$$32)) {
    return (0, _Long.fromBits)(0, 0, false);
  } else {
    const u$$1 = (0, _n.BigNatModule$$$toUInt64)(BigInteger$$get_V(x$$32));

    if ((0, _Long.compare)(u$$1, (0, _Long.fromValue)((0, _Long.fromBits)(4294967295, 2147483647, false), true)) <= 0) {
      return (0, _Long.op_Multiply)((0, _Long.fromInteger)(BigInteger$$get_SignInt(x$$32), false, 2), (0, _Long.fromValue)(u$$1, false));
    } else if (BigInteger$$get_SignInt(x$$32) === -1 ? (0, _Long.equals)(u$$1, (0, _Long.fromValue)((0, _Long.op_Addition)((0, _Long.fromBits)(4294967295, 2147483647, false), (0, _Long.fromBits)(1, 0, false)), true)) : false) {
      return (0, _Long.fromBits)(0, 2147483648, false);
    } else {
      throw new Error();
    }
  }
}

function BigInteger$$get_ToUInt64(x$$33) {
  if (BigInteger$$get_IsZero(x$$33)) {
    return (0, _Long.fromBits)(0, 0, true);
  } else {
    return (0, _n.BigNatModule$$$toUInt64)(BigInteger$$get_V(x$$33));
  }
}

function BigInteger$$get_ToDouble(x$$34) {
  const matchValue$$12 = BigInteger$$get_SignInt(x$$34) | 0;

  switch (matchValue$$12) {
    case -1:
      {
        return -(0, _n.BigNatModule$$$toFloat)(BigInteger$$get_V(x$$34));
      }

    case 0:
      {
        return 0;
      }

    case 1:
      {
        return (0, _n.BigNatModule$$$toFloat)(BigInteger$$get_V(x$$34));
      }

    default:
      {
        throw new Error("signs should be +/- 1 or 0\\nParameter name: x");
      }
  }
}

function BigInteger$$get_ToSByte(x$$35) {
  return (BigInteger$$get_ToInt32(x$$35) + 0x80 & 0xFF) - 0x80;
}

function BigInteger$$get_ToByte(x$$36) {
  return BigInteger$$get_ToUInt32(x$$36) & 0xFF;
}

function BigInteger$$get_ToInt16(x$$37) {
  return (BigInteger$$get_ToInt32(x$$37) + 0x8000 & 0xFFFF) - 0x8000;
}

function BigInteger$$get_ToUInt16(x$$38) {
  return BigInteger$$get_ToUInt32(x$$38) & 0xFFFF;
}

function BigInteger$$get_ToSingle(x$$39) {
  return BigInteger$$get_ToDouble(x$$39);
}

function BigInteger$$get_ToDecimal(x$$40) {
  return new _Decimal.default(BigInteger$$get_ToDouble(x$$40));
}

function BigInteger$$$Parse$$Z721C83C5(text) {
  if (text == null) {
    throw new Error("text");
  }

  const text$$1 = text.trim();
  const len = text$$1.length | 0;

  if (len === 0) {
    throw new Error();
  }

  const matchValue$$13 = [text$$1[0], len];

  if (matchValue$$13[0] === "+") {
    if (matchValue$$13[1] === 1) {
      throw new Error();
    } else {
      return BigInteger$$$posn$$Z67CCE57D((0, _n.BigNatModule$$$ofString)(text$$1.slice(1, len - 1 + 1)));
    }
  } else if (matchValue$$13[0] === "-") {
    if (matchValue$$13[1] === 1) {
      throw new Error();
    } else {
      return BigInteger$$$negn$$Z67CCE57D((0, _n.BigNatModule$$$ofString)(text$$1.slice(1, len - 1 + 1)));
    }
  } else {
    return BigInteger$$$posn$$Z67CCE57D((0, _n.BigNatModule$$$ofString)(text$$1));
  }
}

function BigInteger$$get_IsSmall(x$$41) {
  if (BigInteger$$get_IsZero(x$$41)) {
    return true;
  } else {
    return (0, _n.BigNatModule$$$isSmall)(BigInteger$$get_V(x$$41));
  }
}

function BigInteger$$$Factorial$$Z665282C2(x$$42) {
  if (BigInteger$$get_IsNegative(x$$42)) {
    throw new Error("mustBeNonNegative\\nParameter name: x");
  }

  if (BigInteger$$get_IsPositive(x$$42)) {
    return BigInteger$$$posn$$Z67CCE57D((0, _n.BigNatModule$$$factorial)(BigInteger$$get_V(x$$42)));
  } else {
    return BigInteger$$$get_One();
  }
}

function BigInteger$$$op_UnaryPlus$$Z665282C2(n1) {
  return n1;
}

function BigInteger$$$FromInt64$$Z524259C1(x$$43) {
  return BigInteger$$$$002Ector$$Z524259C1(x$$43);
}

function BigInteger$$$FromInt32$$Z524259A4(x$$44) {
  return BigInteger$$$$002Ector$$Z524259A4(x$$44);
}

BigInteger.prototype.CompareTo = function (obj$$1) {
  const this$$$1 = this;

  if (obj$$1 instanceof BigInteger) {
    return BigInteger$$$compare$$56F059C0(this$$$1, obj$$1) | 0;
  } else {
    throw new Error("the objects are not comparable\\nParameter name: obj");
  }
};