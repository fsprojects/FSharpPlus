"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Kleisli$00602$reflection = Kleisli$00602$reflection;
exports.Kleisli$00602$$$Contramap$$Z9414FBE = Kleisli$00602$$$Contramap$$Z9414FBE;
exports.Kleisli$00602$$$get_App = Kleisli$00602$$$get_App;
exports.Kleisli$$$run = Kleisli$$$run;
exports.Kleisli$00602 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

const Kleisli$00602 = (0, _Types.declare)(function FSharpPlus_Data_Kleisli(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Kleisli$00602 = Kleisli$00602;

function Kleisli$00602$reflection($gen$$2, $gen$$3) {
  return (0, _Reflection.union)("FSharpPlus.Data.Kleisli`2", [$gen$$2, $gen$$3], Kleisli$00602, () => [["Kleisli", [(0, _Reflection.lambda)($gen$$2, $gen$$3)]]]);
}

function Kleisli$00602$$$Contramap$$Z9414FBE(_arg2, k) {
  return new Kleisli$00602(0, "Kleisli", function ($arg$$1) {
    return _arg2.fields[0](k($arg$$1));
  });
}

function Kleisli$00602$$$get_App() {
  return new Kleisli$00602(0, "Kleisli", function (tupledArg) {
    return tupledArg[0].fields[0](tupledArg[1]);
  });
}

function Kleisli$$$run(_arg1$$1) {
  return _arg1$$1.fields[0];
}