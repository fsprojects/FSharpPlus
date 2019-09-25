"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.create = create;
exports.escape = escape;
exports.unescape = unescape;
exports.isMatch = isMatch;
exports.match = match;
exports.matches = matches;
exports.options = options;
exports.replace = replace;
exports.split = split;

function create(pattern, options) {
  // Supported RegexOptions
  // * IgnoreCase:  0x0001
  // * Multiline:   0x0002
  // * Singleline:  0x0010
  // * ECMAScript:  0x0100 (ignored)
  if ((options & ~(1 ^ 2 ^ 16 ^ 256)) !== 0) {
    throw new Error("RegexOptions only supports: IgnoreCase, Multiline, Singleline and ECMAScript");
  }

  let flags = "g";
  flags += options & 1 ? "i" : ""; // 0x0001 RegexOptions.IgnoreCase

  flags += options & 2 ? "m" : "";
  flags += options & 16 ? "s" : "";
  return new RegExp(pattern, flags);
} // From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex


function escape(str) {
  return str.replace(/[\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
}

function unescape(str) {
  return str.replace(/\\([\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|])/g, "$1");
}

function isMatch(str, pattern, options = 0) {
  let reg;
  reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options, reg) : reg = create(pattern, options);
  return reg.test(str);
}

function match(str, pattern, options = 0) {
  let reg;
  reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options, reg) : reg = create(pattern, options);
  return reg.exec(str);
}

function matches(str, pattern, options = 0) {
  let reg;
  reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options, reg) : reg = create(pattern, options);

  if (!reg.global) {
    throw new Error("Non-global RegExp"); // Prevent infinite loop
  }

  let m = reg.exec(str);
  const matches = [];

  while (m !== null) {
    matches.push(m);
    m = reg.exec(str);
  }

  return matches;
}

function options(reg) {
  let options = 256; // ECMAScript

  options |= reg.ignoreCase ? 1 : 0;
  options |= reg.multiline ? 2 : 0;
  return options;
}

function replace(reg, input, replacement, limit, offset = 0) {
  function replacer() {
    let res = arguments[0];

    if (limit !== 0) {
      limit--;
      const match = [];
      const len = arguments.length;

      for (let i = 0; i < len - 2; i++) {
        match.push(arguments[i]);
      }

      match.index = arguments[len - 2];
      match.input = arguments[len - 1];
      res = replacement(match);
    }

    return res;
  }

  if (typeof reg === "string") {
    const tmp = reg;
    reg = create(input, limit);
    input = tmp;
    limit = undefined;
  }

  if (typeof replacement === "function") {
    limit = limit == null ? -1 : limit;
    return input.substring(0, offset) + input.substring(offset).replace(reg, replacer);
  } else {
    // $0 doesn't work with JS regex, see #1155
    replacement = replacement.replace(/\$0/g, s => "$&");

    if (limit != null) {
      let m;
      const sub1 = input.substring(offset);

      const _matches = matches(reg, sub1);

      const sub2 = matches.length > limit ? (m = _matches[limit - 1], sub1.substring(0, m.index + m[0].length)) : sub1;
      return input.substring(0, offset) + sub2.replace(reg, replacement) + input.substring(offset + sub2.length);
    } else {
      return input.replace(reg, replacement);
    }
  }
}

function split(reg, input, limit, offset = 0) {
  if (typeof reg === "string") {
    const tmp = reg;
    reg = create(input, limit);
    input = tmp;
    limit = undefined;
  }

  input = input.substring(offset);
  return input.split(reg, limit);
}