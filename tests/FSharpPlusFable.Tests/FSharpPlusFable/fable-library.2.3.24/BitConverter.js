"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.isLittleEndian = isLittleEndian;
exports.getBytesBoolean = getBytesBoolean;
exports.getBytesChar = getBytesChar;
exports.getBytesInt16 = getBytesInt16;
exports.getBytesInt32 = getBytesInt32;
exports.getBytesInt64 = getBytesInt64;
exports.getBytesUInt16 = getBytesUInt16;
exports.getBytesUInt32 = getBytesUInt32;
exports.getBytesUInt64 = getBytesUInt64;
exports.getBytesSingle = getBytesSingle;
exports.getBytesDouble = getBytesDouble;
exports.int64BitsToDouble = int64BitsToDouble;
exports.doubleToInt64Bits = doubleToInt64Bits;
exports.toBoolean = toBoolean;
exports.toChar = toChar;
exports.toInt16 = toInt16;
exports.toInt32 = toInt32;
exports.toInt64 = toInt64;
exports.toUInt16 = toUInt16;
exports.toUInt32 = toUInt32;
exports.toUInt64 = toUInt64;
exports.toSingle = toSingle;
exports.toDouble = toDouble;
exports.toString = toString;

var _Long = require("./Long");

const littleEndian = true;

function isLittleEndian() {
  return littleEndian;
}

function getBytesBoolean(value) {
  const bytes = new Uint8Array(1);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setUint8(0, value ? 1 : 0);
  return bytes;
}

function getBytesChar(value) {
  const bytes = new Uint8Array(2);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setUint16(0, value.charCodeAt(0), littleEndian);
  return bytes;
}

function getBytesInt16(value) {
  const bytes = new Uint8Array(2);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setInt16(0, value, littleEndian);
  return bytes;
}

function getBytesInt32(value) {
  const bytes = new Uint8Array(4);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setInt32(0, value, littleEndian);
  return bytes;
}

function getBytesInt64(value
/* Long */
) {
  const bytes = new Uint8Array(8);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setInt32(littleEndian ? 0 : 4, (0, _Long.getLowBits)(value), littleEndian);
  view.setInt32(littleEndian ? 4 : 0, (0, _Long.getHighBits)(value), littleEndian);
  return bytes;
}

function getBytesUInt16(value) {
  const bytes = new Uint8Array(2);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setUint16(0, value, littleEndian);
  return bytes;
}

function getBytesUInt32(value) {
  const bytes = new Uint8Array(4);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setUint32(0, value, littleEndian);
  return bytes;
}

function getBytesUInt64(value
/* Long */
) {
  const bytes = new Uint8Array(8);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setUint32(littleEndian ? 0 : 4, (0, _Long.getLowBitsUnsigned)(value), littleEndian);
  view.setUint32(littleEndian ? 4 : 0, (0, _Long.getHighBitsUnsigned)(value), littleEndian);
  return bytes;
}

function getBytesSingle(value) {
  const bytes = new Uint8Array(4);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setFloat32(0, value, littleEndian);
  return bytes;
}

function getBytesDouble(value) {
  const bytes = new Uint8Array(8);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setFloat64(0, value, littleEndian);
  return bytes;
}

function int64BitsToDouble(value
/* Long */
) {
  const buffer = new ArrayBuffer(8);
  const view = new DataView(buffer);
  view.setInt32(littleEndian ? 0 : 4, (0, _Long.getLowBits)(value), littleEndian);
  view.setInt32(littleEndian ? 4 : 0, (0, _Long.getHighBits)(value), littleEndian);
  return view.getFloat64(0, littleEndian);
}

function doubleToInt64Bits(value) {
  const buffer = new ArrayBuffer(8);
  const view = new DataView(buffer);
  view.setFloat64(0, value, littleEndian);
  const lowBits = view.getInt32(littleEndian ? 0 : 4, littleEndian);
  const highBits = view.getInt32(littleEndian ? 4 : 0, littleEndian);
  return (0, _Long.fromBits)(lowBits, highBits, false);
}

function toBoolean(bytes, offset) {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  return view.getUint8(offset) === 1 ? true : false;
}

function toChar(bytes, offset) {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const code = view.getUint16(offset, littleEndian);
  return String.fromCharCode(code);
}

function toInt16(bytes, offset) {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  return view.getInt16(offset, littleEndian);
}

function toInt32(bytes, offset) {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  return view.getInt32(offset, littleEndian);
}

function toInt64(bytes, offset) {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const lowBits = view.getInt32(offset + (littleEndian ? 0 : 4), littleEndian);
  const highBits = view.getInt32(offset + (littleEndian ? 4 : 0), littleEndian);
  return (0, _Long.fromBits)(lowBits, highBits, false);
}

function toUInt16(bytes, offset) {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  return view.getUint16(offset, littleEndian);
}

function toUInt32(bytes, offset) {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  return view.getUint32(offset, littleEndian);
}

function toUInt64(bytes, offset) {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const lowBits = view.getUint32(offset + (littleEndian ? 0 : 4), littleEndian);
  const highBits = view.getUint32(offset + (littleEndian ? 4 : 0), littleEndian);
  return (0, _Long.fromBits)(lowBits, highBits, true);
}

function toSingle(bytes, offset) {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  return view.getFloat32(offset, littleEndian);
}

function toDouble(bytes, offset) {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  return view.getFloat64(offset, littleEndian);
}

function toString(bytes, offset, count) {
  let ar = bytes;

  if (typeof offset !== "undefined" && typeof count !== "undefined") {
    ar = bytes.subarray(offset, offset + count);
  } else if (typeof offset !== "undefined") {
    ar = bytes.subarray(offset);
  }

  return Array.from(ar).map(b => ("0" + b.toString(16)).slice(-2)).join("-");
}