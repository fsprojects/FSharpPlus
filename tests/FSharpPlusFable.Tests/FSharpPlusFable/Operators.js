"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Operators$$$dispose = Operators$$$dispose;

var _Util = require("./fable-library.2.3.24/Util");

function Operators$$$dispose(resource) {
  if ((0, _Util.equals)(resource, null)) {} else {
    resource.Dispose();
  }
}