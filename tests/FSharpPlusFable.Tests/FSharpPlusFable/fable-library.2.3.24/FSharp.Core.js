"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.LanguagePrimitives$$$FastGenericComparer = LanguagePrimitives$$$FastGenericComparer;
exports.LanguagePrimitives$$$FastGenericComparerFromTable = LanguagePrimitives$$$FastGenericComparerFromTable;
exports.LanguagePrimitives$$$FastGenericEqualityComparer = LanguagePrimitives$$$FastGenericEqualityComparer;
exports.LanguagePrimitives$$$FastGenericEqualityComparerFromTable = LanguagePrimitives$$$FastGenericEqualityComparerFromTable;
exports.Operators$$$Failure = Operators$$$Failure;
exports.Operators$$$FailurePattern = Operators$$$FailurePattern;
exports.Operators$$$NullArg = Operators$$$NullArg;
exports.Operators$$$Lock = Operators$$$Lock;
exports.ExtraTopLevelOperators$$$LazyPattern = ExtraTopLevelOperators$$$LazyPattern;
exports.PrintfModule$$$PrintFormatToStringBuilderThen = PrintfModule$$$PrintFormatToStringBuilderThen;
exports.PrintfModule$$$PrintFormatToStringBuilder = PrintfModule$$$PrintFormatToStringBuilder;
exports.LanguagePrimitives$$$GenericEqualityERComparer = exports.LanguagePrimitives$$$GenericEqualityComparer = void 0;

var _Util = require("./Util");

var _FSharp = require("./FSharp.Collections");

var _System = require("./System.Text");

const LanguagePrimitives$$$GenericEqualityComparer = {
  Equals(x, y) {
    return (0, _Util.equals)(x, y);
  },

  GetHashCode(x$$1) {
    return (0, _Util.structuralHash)(x$$1);
  }

};
exports.LanguagePrimitives$$$GenericEqualityComparer = LanguagePrimitives$$$GenericEqualityComparer;
const LanguagePrimitives$$$GenericEqualityERComparer = {
  Equals(x$$2, y$$1) {
    return (0, _Util.equals)(x$$2, y$$1);
  },

  GetHashCode(x$$3) {
    return (0, _Util.structuralHash)(x$$3);
  }

};
exports.LanguagePrimitives$$$GenericEqualityERComparer = LanguagePrimitives$$$GenericEqualityERComparer;

function LanguagePrimitives$$$FastGenericComparer() {
  return (0, _FSharp.ComparisonIdentity$$$Structural)();
}

function LanguagePrimitives$$$FastGenericComparerFromTable() {
  return (0, _FSharp.ComparisonIdentity$$$Structural)();
}

function LanguagePrimitives$$$FastGenericEqualityComparer() {
  return (0, _FSharp.HashIdentity$$$Structural)();
}

function LanguagePrimitives$$$FastGenericEqualityComparerFromTable() {
  return (0, _FSharp.HashIdentity$$$Structural)();
}

function Operators$$$Failure(message) {
  return new Error(message);
}

function Operators$$$FailurePattern(exn) {
  return exn.message;
}

function Operators$$$NullArg(x$$4) {
  throw new Error(x$$4);
}

function Operators$$$Lock(_lockObj, action) {
  return action();
}

function ExtraTopLevelOperators$$$LazyPattern(input) {
  return input.Value;
}

function PrintfModule$$$PrintFormatToStringBuilderThen(continuation, builder, format) {
  return format.cont(function append(s) {
    const value = (0, _System.StringBuilder$$Append$$Z721C83C5)(builder, s);
    value, null;
    return continuation();
  });
}

function PrintfModule$$$PrintFormatToStringBuilder(builder$$1, format$$1) {
  return PrintfModule$$$PrintFormatToStringBuilderThen(function () {
    null, null;
  }, builder$$1, format$$1);
}