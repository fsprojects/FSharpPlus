"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.tryParse = tryParse;
exports.parse = parse;
exports.isInfinity = isInfinity;

function tryParse(str) {
  // TODO: test if value is valid and in range
  if (str != null && /\S/.test(str)) {
    const v = +str.replace("_", "");

    if (!Number.isNaN(v)) {
      return [true, v];
    }
  }

  return [false, 0];
}

function parse(str) {
  const [ok, value] = tryParse(str);

  if (ok) {
    return value;
  } else {
    throw new Error("Input string was not in a correct format.");
  }
} // JS Number.isFinite function evals false for NaN


function isInfinity(x) {
  return x === Number.POSITIVE_INFINITY || x === Number.NEGATIVE_INFINITY;
}