"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Sequence$reflection = Sequence$reflection;
exports.Traverse$reflection = Traverse$reflection;
exports.Traverse$$$Traverse$$Z37D683C4 = Traverse$$$Traverse$$Z37D683C4;
exports.Traverse$$$Traverse$$FAD55DC = Traverse$$$Traverse$$FAD55DC;
exports.Traverse$$$Traverse$$471F5AC6 = Traverse$$$Traverse$$471F5AC6;
exports.Sequence$$$Sequence$$517FDAAD = Sequence$$$Sequence$$517FDAAD;
exports.Traverse = exports.Sequence = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

var _Seq = require("./fable-library.2.3.24/Seq");

var _Option = require("./fable-library.2.3.24/Option");

var _Array = require("./fable-library.2.3.24/Array");

var _Async = require("./fable-library.2.3.24/Async");

var _AsyncBuilder = require("./fable-library.2.3.24/AsyncBuilder");

var _Internals = require("./Internals");

var _Extensions = require("./Extensions");

const Sequence = (0, _Types.declare)(function FSharpPlus_Control_Sequence() {});
exports.Sequence = Sequence;

function Sequence$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Sequence");
}

const Traverse = (0, _Types.declare)(function FSharpPlus_Control_Traverse() {});
exports.Traverse = Traverse;

function Traverse$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Traverse");
}

function Traverse$$$Traverse$$Z37D683C4(t, f, _output, _impl) {
  const ok = new _Types.FSharpRef(true);
  const res = (0, _Array.ofSeq)((0, _Seq.delay)(function () {
    return (0, _Seq.enumerateUsing)((0, _Seq.getEnumerator)(t), function (e) {
      return (0, _Seq.enumerateWhile)(function () {
        return e.MoveNext() ? ok.contents : false;
      }, (0, _Seq.delay)(function () {
        const matchValue = f(e.Current);

        if (matchValue == null) {
          ok.contents = false;
          return (0, _Seq.empty)();
        } else {
          const v = (0, _Option.value)(matchValue);
          return (0, _Seq.singleton)(v);
        }
      }));
    });
  }), Array);

  if (ok.contents) {
    return res;
  } else {
    return null;
  }
}

function Traverse$$$Traverse$$FAD55DC(t$$1, f$$1, _output$$1, _impl$$1) {
  return _AsyncBuilder.singleton.Delay(function () {
    return _AsyncBuilder.singleton.Bind((0, _Async.cancellationToken)(), function (_arg1) {
      return _AsyncBuilder.singleton.Return((0, _Seq.delay)(function () {
        return (0, _Seq.enumerateUsing)((0, _Seq.getEnumerator)(t$$1), function (enum$) {
          return (0, _Seq.enumerateWhile)(function () {
            return enum$.MoveNext();
          }, (0, _Seq.delay)(function () {
            return (0, _Seq.singleton)((0, _Async.runSynchronously)(f$$1(enum$.Current), null, _arg1));
          }));
        });
      }));
    });
  });
}

function Traverse$$$Traverse$$471F5AC6(t$$2, f$$2, _output$$2, _impl$$2) {
  return (0, _Option.defaultArg)(f$$2((0, _Internals.Id$$$run)(t$$2)), null, _Internals.Id$$$create);
}

function Sequence$$$Sequence$$517FDAAD(t$$3, _output$$3, _impl$$3) {
  return (0, _Extensions.Microsoft$002EFSharp$002EControl$002EFSharpAsync$00601$$Async$00601$002ESequence$002EStatic$$65E6EDE0)(t$$3);
}