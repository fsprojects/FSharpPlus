"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Comparer$00601$reflection = Comparer$00601$reflection;
exports.Comparer$00601$$$$002Ector = Comparer$00601$$$$002Ector;
exports.Comparer$00601$$$get_Default = Comparer$00601$$$get_Default;
exports.EqualityComparer$00601$reflection = EqualityComparer$00601$reflection;
exports.EqualityComparer$00601$$$$002Ector = EqualityComparer$00601$$$$002Ector;
exports.EqualityComparer$00601$$$get_Default = EqualityComparer$00601$$$get_Default;
exports.EqualityComparer$00601 = exports.Comparer$00601 = void 0;

var _Types = require("./Types");

var _Reflection = require("./Reflection");

var _Util = require("./Util");

const Comparer$00601 = (0, _Types.declare)(function System_Collections_Generic_Comparer() {});
exports.Comparer$00601 = Comparer$00601;

function Comparer$00601$reflection($gen$$3) {
  return (0, _Reflection.type)("System.Collections.Generic.Comparer`1", [$gen$$3]);
}

function Comparer$00601$$$$002Ector() {
  return this instanceof Comparer$00601 ? Comparer$00601.call(this) : new Comparer$00601();
}

function Comparer$00601$$$get_Default() {
  return {
    Compare(x, y) {
      return (0, _Util.compare)(x, y);
    }

  };
}

Comparer$00601.prototype.Compare = function (x$$1, y$$1) {
  return (0, _Util.compare)(x$$1, y$$1);
};

const EqualityComparer$00601 = (0, _Types.declare)(function System_Collections_Generic_EqualityComparer() {});
exports.EqualityComparer$00601 = EqualityComparer$00601;

function EqualityComparer$00601$reflection($gen$$4) {
  return (0, _Reflection.type)("System.Collections.Generic.EqualityComparer`1", [$gen$$4]);
}

function EqualityComparer$00601$$$$002Ector() {
  return this instanceof EqualityComparer$00601 ? EqualityComparer$00601.call(this) : new EqualityComparer$00601();
}

function EqualityComparer$00601$$$get_Default() {
  return {
    Equals(x$$2, y$$2) {
      return (0, _Util.equals)(x$$2, y$$2);
    },

    GetHashCode(x$$3) {
      return (0, _Util.structuralHash)(x$$3);
    }

  };
}

EqualityComparer$00601.prototype.Equals = function (x$$4, y$$3) {
  return (0, _Util.equals)(x$$4, y$$3);
};

EqualityComparer$00601.prototype.GetHashCode = function (x$$5) {
  return (0, _Util.structuralHash)(x$$5);
};