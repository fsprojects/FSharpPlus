"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Internals$002EParseArray$reflection = Internals$002EParseArray$reflection;
exports.Internals$002ETryParseArray$reflection = Internals$002ETryParseArray$reflection;
exports.Internals$002ETryParseArray = exports.Internals$002EParseArray = exports.Internals$$$formatters = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

const Internals$$$formatters = ["%b", "%d", "%i", "%s", "%u", "%x", "%X", "%o", "%e", "%E", "%f", "%F", "%g", "%G", "%M", "%c", "%A"];
exports.Internals$$$formatters = Internals$$$formatters;
const Internals$002EParseArray = (0, _Types.declare)(function FSharpPlus_Parsing_Internals_ParseArray() {});
exports.Internals$002EParseArray = Internals$002EParseArray;

function Internals$002EParseArray$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Parsing.Internals.ParseArray");
}

const Internals$002ETryParseArray = (0, _Types.declare)(function FSharpPlus_Parsing_Internals_TryParseArray() {});
exports.Internals$002ETryParseArray = Internals$002ETryParseArray;

function Internals$002ETryParseArray$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Parsing.Internals.TryParseArray");
}