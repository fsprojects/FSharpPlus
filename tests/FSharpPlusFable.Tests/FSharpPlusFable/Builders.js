"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Ii$reflection = Ii$reflection;
exports.Ji$reflection = Ji$reflection;
exports.J$reflection = J$reflection;
exports.Idiomatic$reflection = Idiomatic$reflection;
exports.Idiomatic$$$op_Dollar$$6B72D4D9 = Idiomatic$$$op_Dollar$$6B72D4D9;
exports.Builder$reflection = Builder$reflection;
exports.Builder$$$$002Ector = Builder$$$$002Ector;
exports.Builder$$ReturnFrom$$1505 = Builder$$ReturnFrom$$1505;
exports.StrictBuilder$reflection = StrictBuilder$reflection;
exports.StrictBuilder$$$$002Ector = StrictBuilder$$$$002Ector;
exports.StrictBuilder$$Run$$FCFD9EF = StrictBuilder$$Run$$FCFD9EF;
exports.StrictBuilder$$TryWith$$Z570AC55B = StrictBuilder$$TryWith$$Z570AC55B;
exports.StrictBuilder$$TryFinally$$33907399 = StrictBuilder$$TryFinally$$33907399;
exports.DelayedBuilder$reflection = DelayedBuilder$reflection;
exports.DelayedBuilder$$$$002Ector = DelayedBuilder$$$$002Ector;
exports.DelayedBuilder$$Run$$1505 = DelayedBuilder$$Run$$1505;
exports.MonadPlusStrictBuilder$reflection = MonadPlusStrictBuilder$reflection;
exports.MonadPlusStrictBuilder$$$$002Ector = MonadPlusStrictBuilder$$$$002Ector;
exports.MonadFxStrictBuilder$reflection = MonadFxStrictBuilder$reflection;
exports.MonadFxStrictBuilder$$$$002Ector = MonadFxStrictBuilder$$$$002Ector;
exports.MonadPlusBuilder$reflection = MonadPlusBuilder$reflection;
exports.MonadPlusBuilder$$$$002Ector = MonadPlusBuilder$$$$002Ector;
exports.MonadPlusBuilder$$get_strict = MonadPlusBuilder$$get_strict;
exports.MonadFxBuilder$reflection = MonadFxBuilder$reflection;
exports.MonadFxBuilder$$$$002Ector = MonadFxBuilder$$$$002Ector;
exports.MonadFxBuilder$$get_strict = MonadFxBuilder$$get_strict;
exports.MonadFxBuilder$$get_plus = MonadFxBuilder$$get_plus;
exports.MonadFxBuilder$$get_plus$0027 = MonadFxBuilder$$get_plus$0027;
exports.MonadFxBuilder$$get_fx = MonadFxBuilder$$get_fx;
exports.MonadFxBuilder$$get_fx$0027 = MonadFxBuilder$$get_fx$0027;
exports.monad$0027 = exports.monad = exports.MonadFxBuilder = exports.MonadPlusBuilder = exports.MonadFxStrictBuilder = exports.MonadPlusStrictBuilder = exports.DelayedBuilder = exports.StrictBuilder = exports.Builder = exports.Idiomatic = exports.J = exports.Ji = exports.Ii = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

const Ii = (0, _Types.declare)(function FSharpPlus_Builders_Ii(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Ii = Ii;

function Ii$reflection() {
  return (0, _Reflection.union)("FSharpPlus.Builders.Ii", [], Ii, () => ["Ii"]);
}

const Ji = (0, _Types.declare)(function FSharpPlus_Builders_Ji(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Ji = Ji;

function Ji$reflection() {
  return (0, _Reflection.union)("FSharpPlus.Builders.Ji", [], Ji, () => ["Ji"]);
}

const J = (0, _Types.declare)(function FSharpPlus_Builders_J(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.J = J;

function J$reflection() {
  return (0, _Reflection.union)("FSharpPlus.Builders.J", [], J, () => ["J"]);
}

const Idiomatic = (0, _Types.declare)(function FSharpPlus_Builders_Idiomatic(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Idiomatic = Idiomatic;

function Idiomatic$reflection() {
  return (0, _Reflection.union)("FSharpPlus.Builders.Idiomatic", [], Idiomatic, () => ["Idiomatic"]);
}

function Idiomatic$$$op_Dollar$$6B72D4D9(_arg2, _arg3) {
  return function (x) {
    return x;
  };
}

const Builder = (0, _Types.declare)(function FSharpPlus_Builders_Builder() {});
exports.Builder = Builder;

function Builder$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Builders.Builder");
}

function Builder$$$$002Ector() {
  return this instanceof Builder ? Builder.call(this) : new Builder();
}

function Builder$$ReturnFrom$$1505(__, expr) {
  return expr;
}

const StrictBuilder = (0, _Types.declare)(function FSharpPlus_Builders_StrictBuilder() {
  const $this$$2 = this;
  Builder$$$$002Ector.call($this$$2);
}, Builder);
exports.StrictBuilder = StrictBuilder;

function StrictBuilder$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Builders.StrictBuilder");
}

function StrictBuilder$$$$002Ector() {
  return this instanceof StrictBuilder ? StrictBuilder.call(this) : new StrictBuilder();
}

function StrictBuilder$$Run$$FCFD9EF(__$$1, f) {
  return f();
}

function StrictBuilder$$TryWith$$Z570AC55B(__$$2, expr$$1, handler) {
  try {
    return expr$$1();
  } catch (e) {
    return handler(e);
  }
}

function StrictBuilder$$TryFinally$$33907399(__$$3, expr$$2, compensation) {
  try {
    return expr$$2();
  } finally {
    compensation();
  }
}

const DelayedBuilder = (0, _Types.declare)(function FSharpPlus_Builders_DelayedBuilder() {
  const $this$$3 = this;
  Builder$$$$002Ector.call($this$$3);
}, Builder);
exports.DelayedBuilder = DelayedBuilder;

function DelayedBuilder$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Builders.DelayedBuilder");
}

function DelayedBuilder$$$$002Ector() {
  return this instanceof DelayedBuilder ? DelayedBuilder.call(this) : new DelayedBuilder();
}

function DelayedBuilder$$Run$$1505(__$$4, f$$1) {
  return f$$1;
}

const MonadPlusStrictBuilder = (0, _Types.declare)(function FSharpPlus_Builders_MonadPlusStrictBuilder() {
  const $this$$4 = this;
  StrictBuilder$$$$002Ector.call($this$$4);
}, StrictBuilder);
exports.MonadPlusStrictBuilder = MonadPlusStrictBuilder;

function MonadPlusStrictBuilder$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Builders.MonadPlusStrictBuilder");
}

function MonadPlusStrictBuilder$$$$002Ector() {
  return this instanceof MonadPlusStrictBuilder ? MonadPlusStrictBuilder.call(this) : new MonadPlusStrictBuilder();
}

const MonadFxStrictBuilder = (0, _Types.declare)(function FSharpPlus_Builders_MonadFxStrictBuilder() {
  const $this$$5 = this;
  StrictBuilder$$$$002Ector.call($this$$5);
}, StrictBuilder);
exports.MonadFxStrictBuilder = MonadFxStrictBuilder;

function MonadFxStrictBuilder$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Builders.MonadFxStrictBuilder");
}

function MonadFxStrictBuilder$$$$002Ector() {
  return this instanceof MonadFxStrictBuilder ? MonadFxStrictBuilder.call(this) : new MonadFxStrictBuilder();
}

const MonadPlusBuilder = (0, _Types.declare)(function FSharpPlus_Builders_MonadPlusBuilder() {
  const $this$$6 = this;
  DelayedBuilder$$$$002Ector.call($this$$6);
}, DelayedBuilder);
exports.MonadPlusBuilder = MonadPlusBuilder;

function MonadPlusBuilder$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Builders.MonadPlusBuilder");
}

function MonadPlusBuilder$$$$002Ector() {
  return this instanceof MonadPlusBuilder ? MonadPlusBuilder.call(this) : new MonadPlusBuilder();
}

function MonadPlusBuilder$$get_strict(__$$5) {
  return MonadPlusStrictBuilder$$$$002Ector();
}

const MonadFxBuilder = (0, _Types.declare)(function FSharpPlus_Builders_MonadFxBuilder() {
  const $this$$7 = this;
  DelayedBuilder$$$$002Ector.call($this$$7);
}, DelayedBuilder);
exports.MonadFxBuilder = MonadFxBuilder;

function MonadFxBuilder$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Builders.MonadFxBuilder");
}

function MonadFxBuilder$$$$002Ector() {
  return this instanceof MonadFxBuilder ? MonadFxBuilder.call(this) : new MonadFxBuilder();
}

function MonadFxBuilder$$get_strict(__$$6) {
  return MonadFxStrictBuilder$$$$002Ector();
}

function MonadFxBuilder$$get_plus(__$$7) {
  return MonadPlusBuilder$$$$002Ector();
}

function MonadFxBuilder$$get_plus$0027(__$$8) {
  return MonadPlusStrictBuilder$$$$002Ector();
}

function MonadFxBuilder$$get_fx(this$) {
  return this$;
}

function MonadFxBuilder$$get_fx$0027(__$$9) {
  return MonadFxStrictBuilder$$$$002Ector();
}

const monad = MonadFxBuilder$$$$002Ector();
exports.monad = monad;
const monad$0027 = MonadFxStrictBuilder$$$$002Ector();
exports.monad$0027 = monad$0027;