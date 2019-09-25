"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Validation$00602$reflection = Validation$00602$reflection;
exports.Validation$$$map = Validation$$$map;
exports.Validation$$$bimap = Validation$$$bimap;
exports.Validation$$$biFoldBack = Validation$$$biFoldBack;
exports.Validation$$$bind = Validation$$$bind;
exports.Validation$$$orElse = Validation$$$orElse;
exports.Validation$$$valueOr = Validation$$$valueOr;
exports.Validation$$$liftResult = Validation$$$liftResult;
exports.Validation$$$liftChoice = Validation$$$liftChoice;
exports.Validation$$$appValidation = Validation$$$appValidation;
exports.Validation$$$toResult = Validation$$$toResult;
exports.Validation$$$ofResult = Validation$$$ofResult;
exports.Validation$$$either = Validation$$$either;
exports.Validation$$$validate = Validation$$$validate;
exports.Validation$$$ensure = Validation$$$ensure;
exports.Validation$00602$$$Return$$2B594 = Validation$00602$$$Return$$2B594;
exports.Validation$00602$$$Map$$32BFBEA5 = Validation$00602$$$Map$$32BFBEA5;
exports.Validation$00602$$$Bimap$$1FA2ECC2 = Validation$00602$$$Bimap$$1FA2ECC2;
exports.Validation$00602 = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

var _Option = require("./fable-library.2.3.24/Option");

const Validation$00602 = (0, _Types.declare)(function FSharpPlus_Data_Validation(tag, name, ...fields) {
  _Types.Union.call(this, tag, name, ...fields);
}, _Types.Union);
exports.Validation$00602 = Validation$00602;

function Validation$00602$reflection($gen$$2, $gen$$3) {
  return (0, _Reflection.union)("FSharpPlus.Data.Validation`2", [$gen$$2, $gen$$3], Validation$00602, () => [["Failure", [$gen$$2]], ["Success", [$gen$$3]]]);
}

function Validation$$$map(f, _arg1) {
  if (_arg1.tag === 1) {
    return new Validation$00602(1, "Success", f(_arg1.fields[0]));
  } else {
    return new Validation$00602(0, "Failure", _arg1.fields[0]);
  }
}

function Validation$$$bimap(f$$1, g, _arg1$$1) {
  if (_arg1$$1.tag === 1) {
    return new Validation$00602(1, "Success", g(_arg1$$1.fields[0]));
  } else {
    return new Validation$00602(0, "Failure", f$$1(_arg1$$1.fields[0]));
  }
}

function Validation$$$biFoldBack(f$$2, g$$1, state, x) {
  if (state.tag === 0) {
    return f$$2(state.fields[0], x);
  } else {
    return g$$1(state.fields[0], x);
  }
}

function Validation$$$bind(f$$3, x$$1) {
  if (x$$1.tag === 1) {
    return f$$3(x$$1.fields[0]);
  } else {
    return new Validation$00602(0, "Failure", x$$1.fields[0]);
  }
}

function Validation$$$orElse(v, a$$4) {
  if (v.tag === 1) {
    return v.fields[0];
  } else {
    return a$$4;
  }
}

function Validation$$$valueOr(ea, v$$1) {
  if (v$$1.tag === 1) {
    return v$$1.fields[0];
  } else {
    return ea(v$$1.fields[0]);
  }
}

function Validation$$$liftResult(f$$4, _arg1$$2) {
  if (_arg1$$2.tag === 0) {
    return new Validation$00602(1, "Success", _arg1$$2.fields[0]);
  } else {
    return new Validation$00602(0, "Failure", f$$4(_arg1$$2.fields[0]));
  }
}

function Validation$$$liftChoice(f$$5) {
  return function (_arg1$$3) {
    if (_arg1$$3.tag === 1) {
      return new Validation$00602(1, "Success", _arg1$$3.fields[0]);
    } else {
      const arg0 = f$$5(_arg1$$3.fields[0]);
      return new Validation$00602(0, "Failure", arg0);
    }
  };
}

function Validation$$$appValidation(m, e1$0027, e2$0027) {
  if (e1$0027.tag === 1) {
    if (e2$0027.tag === 1) {
      return new Validation$00602(1, "Success", e1$0027.fields[0]);
    } else {
      return new Validation$00602(1, "Success", e1$0027.fields[0]);
    }
  } else if (e2$0027.tag === 1) {
    return new Validation$00602(1, "Success", e2$0027.fields[0]);
  } else {
    return new Validation$00602(0, "Failure", m(e1$0027.fields[0], e2$0027.fields[0]));
  }
}

function Validation$$$toResult(x$$3) {
  if (x$$3.tag === 0) {
    return new _Option.Result(1, "Error", x$$3.fields[0]);
  } else {
    return new _Option.Result(0, "Ok", x$$3.fields[0]);
  }
}

function Validation$$$ofResult(x$$4) {
  if (x$$4.tag === 1) {
    return new Validation$00602(0, "Failure", x$$4.fields[0]);
  } else {
    return new Validation$00602(1, "Success", x$$4.fields[0]);
  }
}

function Validation$$$either(f$$8, g$$4, _arg1$$5) {
  if (_arg1$$5.tag === 0) {
    return g$$4(_arg1$$5.fields[0]);
  } else {
    return f$$8(_arg1$$5.fields[0]);
  }
}

function Validation$$$validate(e$$10, p, a$$9) {
  if (p(a$$9)) {
    return new Validation$00602(1, "Success", a$$9);
  } else {
    return new Validation$00602(0, "Failure", e$$10);
  }
}

function Validation$$$ensure(e$$11, p$$1, _arg1$$6) {
  if (_arg1$$6.tag === 1) {
    return Validation$$$validate(e$$11, p$$1, _arg1$$6.fields[0]);
  } else {
    return new Validation$00602(0, "Failure", _arg1$$6.fields[0]);
  }
}

function Validation$00602$$$Return$$2B594(x$$6) {
  return new Validation$00602(1, "Success", x$$6);
}

function Validation$00602$$$Map$$32BFBEA5(x$$7, f$$9) {
  return Validation$$$map(f$$9, x$$7);
}

function Validation$00602$$$Bimap$$1FA2ECC2(x$$8, f$$10, g$$5) {
  return Validation$$$bimap(f$$10, g$$5, x$$8);
}