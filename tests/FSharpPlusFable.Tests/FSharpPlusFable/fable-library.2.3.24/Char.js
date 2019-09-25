"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.isControl = isControl;
exports.isDigit = isDigit;
exports.isLetter = isLetter;
exports.isLetterOrDigit = isLetterOrDigit;
exports.isUpper = isUpper;
exports.isLower = isLower;
exports.isNumber = isNumber;
exports.isPunctuation = isPunctuation;
exports.isSeparator = isSeparator;
exports.isSymbol = isSymbol;
exports.isWhiteSpace = isWhiteSpace;
exports.isHighSurrogate = isHighSurrogate;
exports.isLowSurrogate = isLowSurrogate;
exports.isSurrogate = isSurrogate;
exports.isSurrogatePair = isSurrogatePair;
exports.parse = parse;
exports.getUnicodeCategory = void 0;

var Encoding = _interopRequireWildcard(require("./Encoding"));

var _Unicode = _interopRequireDefault(require("./Unicode.9.0.0"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _getRequireWildcardCache() { if (typeof WeakMap !== "function") return null; var cache = new WeakMap(); _getRequireWildcardCache = function () { return cache; }; return cache; }

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } var cache = _getRequireWildcardCache(); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; if (obj != null) { var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } } newObj.default = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// Adapted from: https://github.com/hakatashi/general-category
function decodeVByteToIntegerArray(buffer) {
  const ret = [];
  let carried = 0;
  let register = 0;

  for (let i = 0; i < buffer.length; ++i) {
    const byte = buffer[i] ^ 0xFF;
    register += (byte & 127) << carried * 7;
    carried++;

    if ((byte & 128) !== 0) {
      ret.push(register - 1);
      carried = register = 0;
    }
  }

  return ret;
}

function getCategory() {
  // unpack Unicode ranges and categories (delta encoded, vbyte encoded, utf8 encoded)
  const unicodeBuffer = Encoding.get_UTF8().getBytes(_Unicode.default);
  const unicodeDeltas = decodeVByteToIntegerArray(unicodeBuffer);
  const codepoints = new Uint32Array(unicodeDeltas.length / 2);
  const categories = new Uint8Array(unicodeDeltas.length / 2);
  const categoryEnum = new Uint8Array([14, 15, 29, 17, 16, 1, 3, 4, 2, 0, 6, 7, 5, 8, 9, 10, 18, 19, 21, 23, 22, 24, 20, 26, 27, 25, 28, 12, 13, 11]);
  let currentCodepoint = 0;

  for (let i = 0; i < unicodeDeltas.length; i += 2) {
    codepoints[i / 2] = currentCodepoint += unicodeDeltas[i];
    categories[i / 2] = unicodeDeltas[i + 1];
  } // binary search in unicode ranges


  return (s, index) => {
    const cp = s.charCodeAt(index || 0);
    let hi = codepoints.length;
    let lo = 0;

    while (hi - lo > 1) {
      const mid = Math.floor((hi + lo) / 2);
      const test = codepoints[mid];

      if (cp < test) {
        hi = mid;
      } else if (cp === test) {
        hi = lo = mid;
        break;
      } else if (test < cp) {
        lo = mid;
      }
    }

    return categoryEnum[categories[lo]];
  };
}

const isControlMask = 1 << 14
/* Control */
;
const isDigitMask = 1 << 8
/* DecimalDigitNumber */
;
const isLetterMask = 0 | 1 << 0
/* UppercaseLetter */
| 1 << 1
/* LowercaseLetter */
| 1 << 2
/* TitlecaseLetter */
| 1 << 3
/* ModifierLetter */
| 1 << 4
/* OtherLetter */
;
const isLetterOrDigitMask = isLetterMask | isDigitMask;
const isUpperMask = 1 << 0
/* UppercaseLetter */
;
const isLowerMask = 1 << 1
/* LowercaseLetter */
;
const isNumberMask = 0 | 1 << 8
/* DecimalDigitNumber */
| 1 << 9
/* LetterNumber */
| 1 << 10
/* OtherNumber */
;
const isPunctuationMask = 0 | 1 << 18
/* ConnectorPunctuation */
| 1 << 19
/* DashPunctuation */
| 1 << 20
/* OpenPunctuation */
| 1 << 21
/* ClosePunctuation */
| 1 << 22
/* InitialQuotePunctuation */
| 1 << 23
/* FinalQuotePunctuation */
| 1 << 24
/* OtherPunctuation */
;
const isSeparatorMask = 0 | 1 << 11
/* SpaceSeparator */
| 1 << 12
/* LineSeparator */
| 1 << 13
/* ParagraphSeparator */
;
const isSymbolMask = 0 | 1 << 25
/* MathSymbol */
| 1 << 26
/* CurrencySymbol */
| 1 << 27
/* ModifierSymbol */
| 1 << 28
/* OtherSymbol */
;
const isWhiteSpaceMask = 0 | 1 << 11
/* SpaceSeparator */
| 1 << 12
/* LineSeparator */
| 1 << 13
/* ParagraphSeparator */
;
const getUnicodeCategory = getCategory();
exports.getUnicodeCategory = getUnicodeCategory;

function isControl(s, index) {
  const test = 1 << getUnicodeCategory(s, index);
  return (test & isControlMask) !== 0;
}

function isDigit(s, index) {
  const test = 1 << getUnicodeCategory(s, index);
  return (test & isDigitMask) !== 0;
}

function isLetter(s, index) {
  const test = 1 << getUnicodeCategory(s, index);
  return (test & isLetterMask) !== 0;
}

function isLetterOrDigit(s, index) {
  const test = 1 << getUnicodeCategory(s, index);
  return (test & isLetterOrDigitMask) !== 0;
}

function isUpper(s, index) {
  const test = 1 << getUnicodeCategory(s, index);
  return (test & isUpperMask) !== 0;
}

function isLower(s, index) {
  const test = 1 << getUnicodeCategory(s, index);
  return (test & isLowerMask) !== 0;
}

function isNumber(s, index) {
  const test = 1 << getUnicodeCategory(s, index);
  return (test & isNumberMask) !== 0;
}

function isPunctuation(s, index) {
  const test = 1 << getUnicodeCategory(s, index);
  return (test & isPunctuationMask) !== 0;
}

function isSeparator(s, index) {
  const test = 1 << getUnicodeCategory(s, index);
  return (test & isSeparatorMask) !== 0;
}

function isSymbol(s, index) {
  const test = 1 << getUnicodeCategory(s, index);
  return (test & isSymbolMask) !== 0;
}

function isWhiteSpace(s, index) {
  const test = 1 << getUnicodeCategory(s, index);

  if ((test & isWhiteSpaceMask) !== 0) {
    return true;
  }

  const cp = s.charCodeAt(index || 0);
  return 0x09 <= cp && cp <= 0x0D || cp === 0x85 || cp === 0xA0;
}

function isHighSurrogate(s, index) {
  const cp = s.charCodeAt(index || 0);
  return 0xD800 <= cp && cp <= 0xDBFF;
}

function isLowSurrogate(s, index) {
  const cp = s.charCodeAt(index || 0);
  return 0xDC00 <= cp && cp <= 0xDFFF;
}

function isSurrogate(s, index) {
  const cp = s.charCodeAt(index || 0);
  return 0xD800 <= cp && cp <= 0xDFFF;
}

function isSurrogatePair(s, index) {
  return typeof index === "number" ? isHighSurrogate(s, index) && isLowSurrogate(s, index + 1) : isHighSurrogate(s) && isLowSurrogate(index);
}

function parse(input) {
  if (input.length === 1) {
    return input[0];
  } else {
    throw Error("String must be exactly one character long.");
  }
}