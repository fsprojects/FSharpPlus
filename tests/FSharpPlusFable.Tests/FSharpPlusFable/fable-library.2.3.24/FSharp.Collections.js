"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.HashIdentity$$$FromFunctions = HashIdentity$$$FromFunctions;
exports.HashIdentity$$$Structural = HashIdentity$$$Structural;
exports.HashIdentity$$$Reference = HashIdentity$$$Reference;
exports.ComparisonIdentity$$$FromFunction = ComparisonIdentity$$$FromFunction;
exports.ComparisonIdentity$$$Structural = ComparisonIdentity$$$Structural;

var _Util = require("./Util");

function HashIdentity$$$FromFunctions(hash, eq) {
  return {
    Equals(x, y) {
      return eq(x, y);
    },

    GetHashCode(x$$1) {
      return hash(x$$1);
    }

  };
}

function HashIdentity$$$Structural() {
  return HashIdentity$$$FromFunctions(_Util.structuralHash, _Util.equals);
}

function HashIdentity$$$Reference() {
  return HashIdentity$$$FromFunctions(_Util.identityHash, function (e1$$1, e2$$1) {
    return e1$$1 === e2$$1;
  });
}

function ComparisonIdentity$$$FromFunction(comparer) {
  return {
    Compare(x$$2, y$$1) {
      return comparer(x$$2, y$$1);
    }

  };
}

function ComparisonIdentity$$$Structural() {
  return ComparisonIdentity$$$FromFunction(_Util.compare);
}