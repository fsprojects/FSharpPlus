"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.declare = declare;
exports.SystemObject = SystemObject;
exports.List = List;
exports.Union = Union;
exports.Record = Record;
exports.anonRecord = anonRecord;
exports.isException = isException;
exports.Attribute = exports.MatchFailureException = exports.FSharpException = exports.Exception = exports.FSharpRef = void 0;

var _Util = require("./Util");

function sameType(x, y) {
  return y != null && Object.getPrototypeOf(x).constructor === Object.getPrototypeOf(y).constructor;
} // Taken from Babel helpers


function inherits(subClass, superClass) {
  // if (typeof superClass !== "function" && superClass !== null) {
  //   throw new TypeError(
  //     "Super expression must either be null or a function, not " +
  //       typeof superClass
  //   );
  // }
  subClass.prototype = Object.create(superClass && superClass.prototype, {
    constructor: {
      value: subClass,
      enumerable: false,
      writable: true,
      configurable: true
    }
  }); // if (superClass)
  //   Object.setPrototypeOf
  //     ? Object.setPrototypeOf(subClass, superClass)
  //     : (subClass.__proto__ = superClass);
}

function declare(cons, superClass) {
  inherits(cons, superClass || SystemObject);
  return cons;
}

function SystemObject() {}

SystemObject.prototype.toString = function () {
  return "{" + Object.keys(this).map(k => k + " = " + String(this[k])).join(";\n ") + "}";
};

SystemObject.prototype.GetHashCode = function () {
  return (0, _Util.identityHash)(this);
};

SystemObject.prototype.Equals = function (other) {
  return this === other;
};

function compareList(self, other) {
  if (self === other) {
    return 0;
  } else {
    if (other == null) {
      return -1;
    }

    while (self.tail != null) {
      if (other.tail == null) {
        return 1;
      }

      const res = (0, _Util.compare)(self.head, other.head);

      if (res !== 0) {
        return res;
      }

      self = self.tail;
      other = other.tail;
    }

    return other.tail == null ? 0 : -1;
  }
}

function List(head, tail) {
  this.head = head;
  this.tail = tail;
}

List.prototype.toString = function () {
  return "[" + Array.from(this).map(x => String(x)).join("; ") + "]";
};

List.prototype.toJSON = function () {
  return Array.from(this);
};

List.prototype[Symbol.iterator] = function () {
  let cur = this;
  return {
    next: () => {
      const tmp = cur;
      cur = cur.tail;
      return {
        done: tmp.tail == null,
        value: tmp.head
      };
    }
  };
};

List.prototype.GetHashCode = function () {
  const hashes = Array.from(this).map(_Util.structuralHash);
  return (0, _Util.combineHashCodes)(hashes);
};

List.prototype.Equals = function (other) {
  return compareList(this, other) === 0;
};

List.prototype.CompareTo = function (other) {
  return compareList(this, other);
};

function Union(tag, name, ...fields) {
  this.tag = tag | 0;
  this.name = name;
  this.fields = fields;
}

Union.prototype.toString = function () {
  const len = this.fields.length;

  if (len === 0) {
    return this.name;
  } else if (len === 1) {
    return this.name + " " + String(this.fields[0]);
  } else {
    return this.name + " (" + this.fields.map(x => String(x)).join(",") + ")";
  }
};

Union.prototype.toJSON = function () {
  return this.fields.length === 0 ? this.name : [this.name].concat(this.fields);
};

Union.prototype.GetHashCode = function () {
  let hashes = this.fields.map(x => (0, _Util.structuralHash)(x));
  hashes.splice(0, 0, (0, _Util.numberHash)(this.tag));
  return (0, _Util.combineHashCodes)(hashes);
};

Union.prototype.Equals = function (other) {
  return this === other || sameType(this, other) && this.tag === other.tag && (0, _Util.equalArrays)(this.fields, other.fields);
};

Union.prototype.CompareTo = function (other) {
  if (this === other) {
    return 0;
  } else if (!sameType(this, other)) {
    return -1;
  } else if (this.tag === other.tag) {
    return (0, _Util.compareArrays)(this.fields, other.fields);
  } else {
    return this.tag < other.tag ? -1 : 1;
  }
};

function recordToJson(record, getFieldNames) {
  const o = {};
  const keys = getFieldNames == null ? Object.keys(record) : getFieldNames(record);

  for (let i = 0; i < keys.length; i++) {
    o[keys[i]] = record[keys[i]];
  }

  return o;
}

function recordEquals(self, other, getFieldNames) {
  if (self === other) {
    return true;
  } else if (!sameType(self, other)) {
    return false;
  } else {
    const thisNames = getFieldNames == null ? Object.keys(self) : getFieldNames(self);

    for (let i = 0; i < thisNames.length; i++) {
      if (!(0, _Util.equals)(self[thisNames[i]], other[thisNames[i]])) {
        return false;
      }
    }

    return true;
  }
}

function recordCompare(self, other, getFieldNames) {
  if (self === other) {
    return 0;
  } else if (!sameType(self, other)) {
    return -1;
  } else {
    const thisNames = getFieldNames == null ? Object.keys(self) : getFieldNames(self);

    for (let i = 0; i < thisNames.length; i++) {
      const result = (0, _Util.compare)(self[thisNames[i]], other[thisNames[i]]);

      if (result !== 0) {
        return result;
      }
    }

    return 0;
  }
}

function Record() {}

Record.prototype.toString = function () {
  return "{" + Object.keys(this).map(k => k + " = " + String(this[k])).join(";\n ") + "}";
};

Record.prototype.toJSON = function () {
  return recordToJson(this);
};

Record.prototype.GetHashCode = function () {
  const hashes = Object.keys(this).map(k => (0, _Util.structuralHash)(this[k]));
  return (0, _Util.combineHashCodes)(hashes);
};

Record.prototype.Equals = function (other) {
  return recordEquals(this, other);
};

Record.prototype.CompareTo = function (other) {
  return recordCompare(this, other);
};

function anonRecord(o) {
  return Object.assign(Object.create(Record.prototype), o);
}

const FSharpRef = declare(function FSharpRef(contents) {
  this.contents = contents;
}, Record); // EXCEPTIONS

exports.FSharpRef = FSharpRef;
const Exception = declare(function Exception(msg) {
  this.stack = Error().stack;
  this.message = msg;
});
exports.Exception = Exception;

function isException(x) {
  return x instanceof Error || x instanceof Exception;
}

function getFSharpExceptionFieldNames(self) {
  return Object.keys(self).filter(k => k !== "message" && k !== "stack");
}

const FSharpException = declare(function FSharpException() {
  Exception.call(this);
}, Exception);
exports.FSharpException = FSharpException;

FSharpException.prototype.toString = function () {
  const fieldNames = getFSharpExceptionFieldNames(this);
  const len = fieldNames.length;

  if (len === 0) {
    return this.message;
  } else if (len === 1) {
    return this.message + " " + String(this[fieldNames[0]]);
  } else {
    return this.message + " (" + fieldNames.map(k => String(this[k])).join(",") + ")";
  }
};

FSharpException.prototype.toJSON = function () {
  return recordToJson(this, getFSharpExceptionFieldNames);
};

FSharpException.prototype.GetHashCode = function () {
  const hashes = getFSharpExceptionFieldNames(this).map(k => (0, _Util.structuralHash)(this[k]));
  return (0, _Util.combineHashCodes)(hashes);
};

FSharpException.prototype.Equals = function (other) {
  return recordEquals(this, other, getFSharpExceptionFieldNames);
};

FSharpException.prototype.CompareTo = function (other) {
  return recordCompare(this, other, getFSharpExceptionFieldNames);
};

const MatchFailureException = declare(function MatchFailureException(arg1, arg2, arg3) {
  this.arg1 = arg1;
  this.arg2 = arg2 | 0;
  this.arg3 = arg3 | 0;
  this.message = "The match cases were incomplete";
}, FSharpException);
exports.MatchFailureException = MatchFailureException;
const Attribute = declare(function Attribute() {});
exports.Attribute = Attribute;