"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.StringBuilder$reflection = StringBuilder$reflection;
exports.StringBuilder$$$$002Ector$$Z18115A39 = StringBuilder$$$$002Ector$$Z18115A39;
exports.StringBuilder$$$$002Ector$$Z524259A4 = StringBuilder$$$$002Ector$$Z524259A4;
exports.StringBuilder$$$$002Ector$$Z721C83C5 = StringBuilder$$$$002Ector$$Z721C83C5;
exports.StringBuilder$$$$002Ector = StringBuilder$$$$002Ector;
exports.StringBuilder$$Append$$Z721C83C5 = StringBuilder$$Append$$Z721C83C5;
exports.StringBuilder$$Append$$244C7CD6 = StringBuilder$$Append$$244C7CD6;
exports.StringBuilder$$AppendFormat$$433E080 = StringBuilder$$AppendFormat$$433E080;
exports.StringBuilder = void 0;

var _Types = require("./Types");

var _Reflection = require("./Reflection");

var _String = require("./String");

const StringBuilder = (0, _Types.declare)(function System_Text_StringBuilder(value, capacity) {
  const $this$$1 = this;
  $this$$1.buf = [];

  if (!(value == null)) {
    $this$$1.buf.push(value);
  }
});
exports.StringBuilder = StringBuilder;

function StringBuilder$reflection() {
  return (0, _Reflection.type)("System.Text.StringBuilder");
}

function StringBuilder$$$$002Ector$$Z18115A39(value, capacity) {
  return this instanceof StringBuilder ? StringBuilder.call(this, value, capacity) : new StringBuilder(value, capacity);
}

function StringBuilder$$$$002Ector$$Z524259A4(capacity$$1) {
  return StringBuilder$$$$002Ector$$Z18115A39.call(this, null, capacity$$1);
}

function StringBuilder$$$$002Ector$$Z721C83C5(value$$1) {
  return StringBuilder$$$$002Ector$$Z18115A39.call(this, value$$1, 16);
}

function StringBuilder$$$$002Ector() {
  return StringBuilder$$$$002Ector$$Z18115A39.call(this, null, 16);
}

function StringBuilder$$Append$$Z721C83C5(x, s) {
  x.buf.push(s);
  return x;
}

function StringBuilder$$Append$$244C7CD6(x$$1, c) {
  x$$1.buf.push(c);
  return x$$1;
}

function StringBuilder$$AppendFormat$$433E080(x$$2, fmt, o) {
  x$$2.buf.push((0, _String.format)(fmt, o));
  return x$$2;
}

StringBuilder.prototype.toString = function () {
  const __ = this;

  return (0, _String.join)("", ...__.buf);
};