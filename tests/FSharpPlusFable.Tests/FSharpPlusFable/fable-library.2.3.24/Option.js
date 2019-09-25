"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.some = some;
exports.value = value;
exports.defaultArg = defaultArg;
exports.defaultArgWith = defaultArgWith;
exports.filter = filter;
exports.map = map;
exports.choice1 = choice1;
exports.choice2 = choice2;
exports.tryValueIfChoice1 = tryValueIfChoice1;
exports.tryValueIfChoice2 = tryValueIfChoice2;
exports.ok = ok;
exports.error = error;
exports.mapOk = mapOk;
exports.mapError = mapError;
exports.bindOk = bindOk;
exports.Result = exports.Choice = exports.Some = void 0;

var _Types = require("./Types");

var _Util = require("./Util");

// Options are erased in runtime by Fable, but we have
// the `Some` type below to wrap values that would evaluate
// to null in runtime. These two rules must be followed:
// 1- None is always null in runtime, a non-strict null check
//    (`x == null`) is enough to check the case of an option.
// 2- To get the value of an option the `getValue` helper
//    below must **always** be used.
// export type Option<T> = T | Some<T>;
// Using a class here for better compatibility with TS files importing Some
class Some {
  constructor(value) {
    this.value = value;
  } // Don't add "Some" for consistency with erased options


  toString() {
    return String(this.value);
  }

  toJSON() {
    return this.value;
  }

  GetHashCode() {
    return (0, _Util.structuralHash)(this.value);
  }

  Equals(other) {
    return other == null ? false : (0, _Util.equals)(this.value, other instanceof Some ? other.value : other);
  }

  CompareTo(other) {
    return other == null ? 1 : (0, _Util.compare)(this.value, other instanceof Some ? other.value : other);
  }

}

exports.Some = Some;

function some(x) {
  return x == null || x instanceof Some ? new Some(x) : x;
}

function value(x, acceptNull) {
  if (x == null) {
    if (!acceptNull) {
      throw new Error("Option has no value");
    }

    return null;
  } else {
    return x instanceof Some ? x.value : x;
  }
}

function defaultArg(arg, defaultValue, f) {
  return arg == null ? defaultValue : f != null ? f(value(arg)) : value(arg);
}

function defaultArgWith(arg, defThunk) {
  return arg == null ? defThunk() : value(arg);
}

function filter(predicate, arg) {
  return arg != null ? !predicate(value(arg)) ? null : arg : arg;
}

function map(predicate, ...args) {
  return args.every(x => x != null) ? predicate.apply(null, args) : null;
} // CHOICE


const Choice = (0, _Types.declare)(function Choice(tag, name, field) {
  _Types.Union.call(this, tag, name, field);
}, _Types.Union);
exports.Choice = Choice;

function choice1(x) {
  return new Choice(0, "Choice1Of2", x);
}

function choice2(x) {
  return new Choice(1, "Choice2Of2", x);
}

function tryValueIfChoice1(x) {
  return x.tag === 0 ? some(x.fields[0]) : null;
}

function tryValueIfChoice2(x) {
  return x.tag === 1 ? some(x.fields[0]) : null;
} // RESULT


const Result = (0, _Types.declare)(function Result(tag, name, field) {
  _Types.Union.call(this, tag, name, field);
}, _Types.Union);
exports.Result = Result;

function ok(x) {
  return new Result(0, "Ok", x);
}

function error(x) {
  return new Result(1, "Error", x);
}

function mapOk(f, result) {
  return result.tag === 0 ? ok(f(result.fields[0])) : result;
}

function mapError(f, result) {
  return result.tag === 1 ? error(f(result.fields[0])) : result;
}

function bindOk(f, result) {
  return result.tag === 0 ? f(result.fields[0]) : result;
}