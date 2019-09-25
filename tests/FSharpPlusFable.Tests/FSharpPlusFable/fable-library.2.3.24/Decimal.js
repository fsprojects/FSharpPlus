"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.compare = compare;
exports.equals = equals;
exports.abs = abs;
exports.round = round;
exports.truncate = truncate;
exports.ceiling = ceiling;
exports.floor = floor;
exports.pow = pow;
exports.sqrt = sqrt;
exports.op_Addition = op_Addition;
exports.op_Subtraction = op_Subtraction;
exports.op_Multiply = op_Multiply;
exports.op_Division = op_Division;
exports.op_Modulus = op_Modulus;
exports.op_UnaryNegation = op_UnaryNegation;
exports.toString = toString;
exports.tryParse = tryParse;
exports.parse = parse;
exports.toNumber = toNumber;
exports.fromIntArray = fromIntArray;
exports.fromInts = fromInts;
exports.fromParts = fromParts;
exports.getBits = getBits;
exports.makeRangeStepFunction = makeRangeStepFunction;
exports.negate = exports.remainder = exports.divide = exports.multiply = exports.subtract = exports.add = exports.get_MinValue = exports.get_MaxValue = exports.get_MinusOne = exports.get_One = exports.get_Zero = exports.default = void 0;

var _big = _interopRequireDefault(require("../lib/big"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var _default = _big.default;
exports.default = _default;
const get_Zero = new _big.default(0);
exports.get_Zero = get_Zero;
const get_One = new _big.default(1);
exports.get_One = get_One;
const get_MinusOne = new _big.default(-1);
exports.get_MinusOne = get_MinusOne;
const get_MaxValue = new _big.default("79228162514264337593543950335");
exports.get_MaxValue = get_MaxValue;
const get_MinValue = new _big.default("-79228162514264337593543950335");
exports.get_MinValue = get_MinValue;

function compare(x, y) {
  return x.cmp(y);
}

function equals(x, y) {
  return !x.cmp(y);
}

function abs(x) {
  return x.abs();
}

function round(x, digits = 0) {
  return x.round(digits, 2
  /* ROUND_HALF_EVEN */
  );
}

function truncate(x) {
  return x.round(0, 0
  /* ROUND_DOWN */
  );
}

function ceiling(x) {
  return x.round(0, x.cmp(0) >= 0 ? 3
  /* ROUND_UP */
  : 0
  /* ROUND_DOWN */
  );
}

function floor(x) {
  return x.round(0, x.cmp(0) >= 0 ? 0
  /* ROUND_DOWN */
  : 3
  /* ROUND_UP */
  );
}

function pow(x, n) {
  return x.pow(n);
}

function sqrt(x) {
  return x.sqrt();
}

function op_Addition(x, y) {
  return x.add(y);
}

function op_Subtraction(x, y) {
  return x.sub(y);
}

function op_Multiply(x, y) {
  return x.mul(y);
}

function op_Division(x, y) {
  return x.div(y);
}

function op_Modulus(x, y) {
  return x.mod(y);
}

function op_UnaryNegation(x) {
  const x2 = new _big.default(x);
  x2.s = -x2.s || 0;
  return x2;
}

const add = op_Addition;
exports.add = add;
const subtract = op_Subtraction;
exports.subtract = subtract;
const multiply = op_Multiply;
exports.multiply = multiply;
const divide = op_Division;
exports.divide = divide;
const remainder = op_Modulus;
exports.remainder = remainder;
const negate = op_UnaryNegation;
exports.negate = negate;

function toString(x) {
  return x.toString();
}

function tryParse(str) {
  try {
    return [true, new _big.default(str.trim())];
  } catch (_a) {
    return [false, get_Zero];
  }
}

function parse(str) {
  const [ok, value] = tryParse(str);

  if (ok) {
    return value;
  } else {
    throw new Error("Input string was not in a correct format.");
  }
}

function toNumber(x) {
  return +x;
}

function decimalToHex(dec, bitSize) {
  const hex = new Uint8Array(bitSize / 4 | 0);
  let hexCount = 1;

  for (let d = 0; d < dec.length; d++) {
    let value = dec[d];

    for (let i = 0; i < hexCount; i++) {
      const digit = hex[i] * 10 + value | 0;
      hex[i] = digit & 0xF;
      value = digit >> 4;
    }

    if (value !== 0) {
      hex[hexCount++] = value;
    }
  }

  return hex.slice(0, hexCount); // digits in reverse order
}

function hexToDecimal(hex, bitSize) {
  const dec = new Uint8Array(bitSize * 301 / 1000 + 1 | 0);
  let decCount = 1;

  for (let d = hex.length - 1; d >= 0; d--) {
    let carry = hex[d];

    for (let i = 0; i < decCount; i++) {
      const val = dec[i] * 16 + carry | 0;
      dec[i] = val % 10 | 0;
      carry = val / 10 | 0;
    }

    while (carry > 0) {
      dec[decCount++] = carry % 10 | 0;
      carry = carry / 10 | 0;
    }
  }

  return dec.slice(0, decCount); // digits in reverse order
}

function setInt32Bits(hexDigits, bits, offset) {
  for (let i = 0; i < 8; i++) {
    hexDigits[offset + i] = bits >> i * 4 & 0xF;
  }
}

function getInt32Bits(hexDigits, offset) {
  let bits = 0;

  for (let i = 0; i < 8; i++) {
    bits = bits | hexDigits[offset + i] << i * 4;
  }

  return bits;
}

function fromIntArray(bits) {
  return fromInts(bits[0], bits[1], bits[2], bits[3]);
}

function fromInts(low, mid, high, signExp) {
  const isNegative = signExp < 0;
  const scale = signExp >> 16 & 0x7F;
  return fromParts(low, mid, high, isNegative, scale);
}

function fromParts(low, mid, high, isNegative, scale) {
  const bitSize = 96;
  const hexDigits = new Uint8Array(bitSize / 4);
  setInt32Bits(hexDigits, low, 0);
  setInt32Bits(hexDigits, mid, 8);
  setInt32Bits(hexDigits, high, 16);
  const decDigits = hexToDecimal(hexDigits, bitSize);
  scale = scale & 0x7F;
  const big = new _big.default(0);
  big.c = Array.from(decDigits.reverse());
  big.e = decDigits.length - scale - 1;
  big.s = isNegative ? -1 : 1;
  const d = new _big.default(big);
  return d;
}

function getBits(d) {
  const bitSize = 96;
  const decDigits = Uint8Array.from(d.c);
  const hexDigits = decimalToHex(decDigits, bitSize);
  const low = getInt32Bits(hexDigits, 0);
  const mid = getInt32Bits(hexDigits, 8);
  const high = getInt32Bits(hexDigits, 16);
  const decStr = d.toString();
  const dotPos = decStr.indexOf(".");
  const scale = dotPos < 0 ? 0 : decStr.length - dotPos - 1;
  const signExp = (scale & 0x7F) << 16 | (d.s < 0 ? 0x80000000 : 0);
  return [low, mid, high, signExp];
}

function makeRangeStepFunction(step, last) {
  const stepComparedWithZero = step.cmp(get_Zero);

  if (stepComparedWithZero === 0) {
    throw new Error("The step of a range cannot be zero");
  }

  const stepGreaterThanZero = stepComparedWithZero > 0;
  return x => {
    const comparedWithLast = x.cmp(last);

    if (stepGreaterThanZero && comparedWithLast <= 0 || !stepGreaterThanZero && comparedWithLast >= 0) {
      return [x, op_Addition(x, step)];
    } else {
      return null;
    }
  };
}