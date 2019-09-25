"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Item$reflection = Item$reflection;
exports.Item$$$Item$$Z95E902C = Item$$$Item$$Z95E902C;
exports.Item$$$Item$$Z23C6249A = Item$$$Item$$Z23C6249A;
exports.Item$$$Item$$7F8A268C = Item$$$Item$$7F8A268C;
exports.TryItem$reflection = TryItem$reflection;
exports.TryItem$$$TryItem$$Z23D1C535 = TryItem$$$TryItem$$Z23D1C535;
exports.TryItem$$$TryItem$$Z9497187 = TryItem$$$TryItem$$Z9497187;
exports.TryItem$$$TryItem$$Z458E3FA7 = TryItem$$$TryItem$$Z458E3FA7;
exports.TryItem$$$TryItem$$Z4A0BC987 = TryItem$$$TryItem$$Z4A0BC987;
exports.TryItem$$$TryItem$$Z3718F610 = TryItem$$$TryItem$$Z3718F610;
exports.TryItem$$$TryItem$$Z668AA649 = TryItem$$$TryItem$$Z668AA649;
exports.TryItem$$$TryItem$$326376D7 = TryItem$$$TryItem$$326376D7;
exports.MapIndexed$reflection = MapIndexed$reflection;
exports.MapIndexed$$$MapIndexed$$64EA7550 = MapIndexed$$$MapIndexed$$64EA7550;
exports.MapIndexed$$$MapIndexed$$Z662E690B = MapIndexed$$$MapIndexed$$Z662E690B;
exports.MapIndexed$$$MapIndexed$$336E8730 = MapIndexed$$$MapIndexed$$336E8730;
exports.MapIndexed$$$MapIndexed$$Z2FF41926 = MapIndexed$$$MapIndexed$$Z2FF41926;
exports.MapIndexed$$$MapIndexed$$64866CCF = MapIndexed$$$MapIndexed$$64866CCF;
exports.MapIndexed$$$MapIndexed$$5663A6D6 = MapIndexed$$$MapIndexed$$5663A6D6;
exports.MapIndexed$$$MapIndexed$$6666B79E = MapIndexed$$$MapIndexed$$6666B79E;
exports.IterateIndexed$reflection = IterateIndexed$reflection;
exports.IterateIndexed$$$IterateIndexed$$ECA40AF = IterateIndexed$$$IterateIndexed$$ECA40AF;
exports.IterateIndexed$$$IterateIndexed$$Z30911236 = IterateIndexed$$$IterateIndexed$$Z30911236;
exports.IterateIndexed$$$IterateIndexed$$Z5A020471 = IterateIndexed$$$IterateIndexed$$Z5A020471;
exports.IterateIndexed$$$IterateIndexed$$6889E25 = IterateIndexed$$$IterateIndexed$$6889E25;
exports.IterateIndexed$$$IterateIndexed$$713EC821 = IterateIndexed$$$IterateIndexed$$713EC821;
exports.FoldIndexed$reflection = FoldIndexed$reflection;
exports.FoldIndexed$$$FoldIndexed$$Z49733982 = FoldIndexed$$$FoldIndexed$$Z49733982;
exports.FoldIndexed$$$FoldIndexed$$6CDC711B = FoldIndexed$$$FoldIndexed$$6CDC711B;
exports.FoldIndexed$$$FoldIndexed$$22912F31 = FoldIndexed$$$FoldIndexed$$22912F31;
exports.FoldIndexed$$$FoldIndexed$$33D87835 = FoldIndexed$$$FoldIndexed$$33D87835;
exports.TraverseIndexed$reflection = TraverseIndexed$reflection;
exports.FindIndex$reflection = FindIndex$reflection;
exports.FindIndex$$$FindIndex$$30BBD2AC = FindIndex$$$FindIndex$$30BBD2AC;
exports.FindIndex$$$FindIndex$$Z4B9623F9 = FindIndex$$$FindIndex$$Z4B9623F9;
exports.FindIndex$$$FindIndex$$3AE9C74D = FindIndex$$$FindIndex$$3AE9C74D;
exports.FindIndex$$$FindIndex$$414EBDAD = FindIndex$$$FindIndex$$414EBDAD;
exports.FindIndex$$$FindIndex$$5FC92BE8 = FindIndex$$$FindIndex$$5FC92BE8;
exports.FindIndex$$$FindIndex$$Z58ADA513 = FindIndex$$$FindIndex$$Z58ADA513;
exports.TryFindIndex$reflection = TryFindIndex$reflection;
exports.TryFindIndex$$$TryFindIndex$$437F0593 = TryFindIndex$$$TryFindIndex$$437F0593;
exports.TryFindIndex$$$TryFindIndex$$Z3852F4C8 = TryFindIndex$$$TryFindIndex$$Z3852F4C8;
exports.TryFindIndex$$$TryFindIndex$$492D1072 = TryFindIndex$$$TryFindIndex$$492D1072;
exports.TryFindIndex$$$TryFindIndex$$328A6A92 = TryFindIndex$$$TryFindIndex$$328A6A92;
exports.TryFindIndex$$$TryFindIndex$$2C0DFCD7 = TryFindIndex$$$TryFindIndex$$2C0DFCD7;
exports.TryFindIndex$$$TryFindIndex$$Z2B69722E = TryFindIndex$$$TryFindIndex$$Z2B69722E;
exports.FindSliceIndex$reflection = FindSliceIndex$reflection;
exports.FindSliceIndex$$$FindSliceIndex$$Z59BB29F3 = FindSliceIndex$$$FindSliceIndex$$Z59BB29F3;
exports.TryFindSliceIndex$reflection = TryFindSliceIndex$reflection;
exports.TryFindSliceIndex$$$TryFindSliceIndex$$15491252 = TryFindSliceIndex$$$TryFindSliceIndex$$15491252;
exports.TryFindSliceIndex = exports.FindSliceIndex = exports.TryFindIndex = exports.FindIndex = exports.TraverseIndexed = exports.FoldIndexed = exports.IterateIndexed = exports.MapIndexed = exports.TryItem = exports.Item = void 0;

var _Types = require("./fable-library.2.3.24/Types");

var _Reflection = require("./fable-library.2.3.24/Reflection");

var _Extensions = require("./Extensions");

var _System = require("./fable-library.2.3.24/System.Text");

var _Option = require("./fable-library.2.3.24/Option");

var _List = require("./fable-library.2.3.24/List");

var _Util = require("./fable-library.2.3.24/Util");

var _Map = require("./fable-library.2.3.24/Map");

var _Internals = require("./Internals");

var _Seq = require("./fable-library.2.3.24/Seq");

var _Array = require("./fable-library.2.3.24/Array");

const Item = (0, _Types.declare)(function FSharpPlus_Control_Item() {});
exports.Item = Item;

function Item$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.Item");
}

function Item$$$Item$$Z95E902C(x, n, _impl) {
  return (0, _Extensions.String$$$item)(n, x);
}

function Item$$$Item$$Z23C6249A(x$$1, n$$1, _impl$$1) {
  return String(x$$1)[n$$1];
}

function Item$$$Item$$7F8A268C(x$$2, n$$2, _impl$$2) {
  return x$$2[n$$2];
}

const TryItem = (0, _Types.declare)(function FSharpPlus_Control_TryItem() {});
exports.TryItem = TryItem;

function TryItem$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.TryItem");
}

function TryItem$$$TryItem$$Z23D1C535(x$$3, n$$3, _impl$$3) {
  return (0, _Extensions.String$$$tryItem)(n$$3, x$$3);
}

function TryItem$$$TryItem$$Z9497187(x$$4, n$$4, _impl$$4) {
  if (n$$4 >= 0 ? n$$4 < (0, _System.StringBuilder$$get_Length)(x$$4) : false) {
    return String(x$$4)[n$$4];
  } else {
    return null;
  }
}

function TryItem$$$TryItem$$Z458E3FA7(x$$5, n$$5, _impl$$5) {
  if (n$$5 >= 0 ? n$$5 < x$$5.length : false) {
    return (0, _Option.some)(x$$5[n$$5]);
  } else {
    return null;
  }
}

function TryItem$$$TryItem$$Z4A0BC987(x$$6, n$$6, _impl$$6) {
  return (0, _List.tryItem)(n$$6, x$$6);
}

function TryItem$$$TryItem$$Z3718F610(x$$7, n$$7, _impl$$7) {
  if (n$$7 >= 0 ? n$$7 < (0, _Util.count)(x$$7) : false) {
    return (0, _Option.some)(x$$7[n$$7]);
  } else {
    return null;
  }
}

function TryItem$$$TryItem$$Z668AA649(x$$8, n$$8, _impl$$8) {
  if (n$$8 >= 0 ? n$$8 < x$$8.Count : false) {
    return (0, _Option.some)(x$$8.Item(n$$8));
  } else {
    return null;
  }
}

function TryItem$$$TryItem$$326376D7(x$$9, k, _impl$$9) {
  return (0, _Map.FSharpMap$$TryFind$$2B595)(x$$9, k);
}

const MapIndexed = (0, _Types.declare)(function FSharpPlus_Control_MapIndexed() {});
exports.MapIndexed = MapIndexed;

function MapIndexed$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.MapIndexed");
}

function MapIndexed$$$MapIndexed$$64EA7550(x$$10, f, _impl$$10) {
  return f(null, (0, _Internals.Id$00601$$get_getValue)(x$$10));
}

function MapIndexed$$$MapIndexed$$Z662E690B(x$$11, f$$1, _impl$$11) {
  return (0, _Seq.mapIndexed)(f$$1, x$$11);
}

function MapIndexed$$$MapIndexed$$336E8730(x$$12, f$$2, _impl$$12) {
  return (0, _List.mapIndexed)(f$$2, x$$12);
}

function MapIndexed$$$MapIndexed$$Z2FF41926(x$$13, f$$3, _impl$$13) {
  return (0, _Array.mapIndexed)(f$$3, x$$13, Array);
}

function MapIndexed$$$MapIndexed$$64866CCF(_arg1, f$$4, _impl$$14) {
  return [_arg1[0], f$$4(_arg1[0], _arg1[1])];
}

function MapIndexed$$$MapIndexed$$5663A6D6(g, f$$5, _impl$$15) {
  return function (x$$14) {
    return f$$5(x$$14, g(x$$14));
  };
}

function MapIndexed$$$MapIndexed$$6666B79E(x$$15, f$$6, _impl$$16) {
  return (0, _Map.map)(f$$6, x$$15);
}

const IterateIndexed = (0, _Types.declare)(function FSharpPlus_Control_IterateIndexed() {});
exports.IterateIndexed = IterateIndexed;

function IterateIndexed$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.IterateIndexed");
}

function IterateIndexed$$$IterateIndexed$$ECA40AF(x$$16, f$$7, _impl$$17) {
  f$$7(null, (0, _Internals.Id$00601$$get_getValue)(x$$16));
}

function IterateIndexed$$$IterateIndexed$$Z30911236(x$$17, f$$8, _impl$$18) {
  (0, _Seq.iterateIndexed)(f$$8, x$$17);
}

function IterateIndexed$$$IterateIndexed$$Z5A020471(x$$18, f$$9, _impl$$19) {
  (0, _List.iterateIndexed)(f$$9, x$$18);
}

function IterateIndexed$$$IterateIndexed$$6889E25(x$$19, f$$10, _impl$$20) {
  (0, _Array.iterateIndexed)(f$$10, x$$19);
}

function IterateIndexed$$$IterateIndexed$$713EC821(x$$20, f$$11, _impl$$21) {
  (0, _Map.iterate)(f$$11, x$$20);
}

const FoldIndexed = (0, _Types.declare)(function FSharpPlus_Control_FoldIndexed() {});
exports.FoldIndexed = FoldIndexed;

function FoldIndexed$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.FoldIndexed");
}

function FoldIndexed$$$FoldIndexed$$Z49733982(x$$21, f$$12, z, _impl$$22) {
  let tuple;
  tuple = (0, _Seq.fold)(function folder(tupledArg, t) {
    return [f$$12(tupledArg[0], tupledArg[1], t), tupledArg[1] + 1];
  }, [z, 0], x$$21);
  return tuple[0];
}

function FoldIndexed$$$FoldIndexed$$6CDC711B(x$$22, f$$13, z$$1, _impl$$23) {
  let tuple$$1;
  tuple$$1 = (0, _List.fold)(function folder$$1(tupledArg$$1, t$$1) {
    return [f$$13(tupledArg$$1[0], tupledArg$$1[1], t$$1), tupledArg$$1[1] + 1];
  }, [z$$1, 0], x$$22);
  return tuple$$1[0];
}

function FoldIndexed$$$FoldIndexed$$22912F31(x$$23, f$$14, z$$2, _impl$$24) {
  let tuple$$2;
  tuple$$2 = (0, _Array.fold)(function folder$$2(tupledArg$$2, t$$2) {
    return [f$$14(tupledArg$$2[0], tupledArg$$2[1], t$$2), tupledArg$$2[1] + 1];
  }, [z$$2, 0], x$$23);
  return tuple$$2[0];
}

function FoldIndexed$$$FoldIndexed$$33D87835(_arg1$$1, f$$15, z$$3, _impl$$25) {
  return function (table) {
    return (0, _Map.fold)(f$$15, z$$3, table);
  };
}

const TraverseIndexed = (0, _Types.declare)(function FSharpPlus_Control_TraverseIndexed() {});
exports.TraverseIndexed = TraverseIndexed;

function TraverseIndexed$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.TraverseIndexed");
}

const FindIndex = (0, _Types.declare)(function FSharpPlus_Control_FindIndex() {});
exports.FindIndex = FindIndex;

function FindIndex$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.FindIndex");
}

function FindIndex$$$FindIndex$$30BBD2AC(x$$24, p$$3, _impl$$26) {
  return (0, _Extensions.String$$$findIndex)(p$$3, x$$24);
}

function FindIndex$$$FindIndex$$Z4B9623F9(x$$25, p$$4, _impl$$27) {
  return x$$25.findIndex(p$$4);
}

function FindIndex$$$FindIndex$$3AE9C74D(x$$26, p$$5, _impl$$28) {
  return (0, _Seq.findIndex)(p$$5, x$$26);
}

function FindIndex$$$FindIndex$$414EBDAD(x$$27, p$$6, _impl$$29) {
  return (0, _List.findIndex)(p$$6, x$$27);
}

function FindIndex$$$FindIndex$$5FC92BE8(x$$28, p$$7, _impl$$30) {
  return (0, _Seq.findIndex)(p$$7, x$$28);
}

function FindIndex$$$FindIndex$$Z58ADA513(x$$29, p$$8, _impl$$31) {
  return (0, _List.findIndex)(p$$8, new _Types.List((0, _Internals.Id$00601$$get_getValue)(x$$29), new _Types.List()));
}

const TryFindIndex = (0, _Types.declare)(function FSharpPlus_Control_TryFindIndex() {});
exports.TryFindIndex = TryFindIndex;

function TryFindIndex$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.TryFindIndex");
}

function TryFindIndex$$$TryFindIndex$$437F0593(x$$30, p$$9, _impl$$32) {
  return (0, _Extensions.String$$$tryFindIndex)(p$$9, x$$30);
}

function TryFindIndex$$$TryFindIndex$$Z3852F4C8(x$$31, p$$10, _impl$$33) {
  return (0, _Array.tryFindIndex)(p$$10, x$$31);
}

function TryFindIndex$$$TryFindIndex$$492D1072(x$$32, p$$11, _impl$$34) {
  return (0, _Seq.tryFindIndex)(p$$11, x$$32);
}

function TryFindIndex$$$TryFindIndex$$328A6A92(x$$33, p$$12, _impl$$35) {
  return (0, _List.tryFindIndex)(p$$12, x$$33);
}

function TryFindIndex$$$TryFindIndex$$2C0DFCD7(x$$34, p$$13, _impl$$36) {
  return (0, _Seq.tryFindIndex)(p$$13, x$$34);
}

function TryFindIndex$$$TryFindIndex$$Z2B69722E(x$$35, p$$14, _impl$$37) {
  return (0, _List.tryFindIndex)(p$$14, new _Types.List((0, _Internals.Id$00601$$get_getValue)(x$$35), new _Types.List()));
}

const FindSliceIndex = (0, _Types.declare)(function FSharpPlus_Control_FindSliceIndex() {});
exports.FindSliceIndex = FindSliceIndex;

function FindSliceIndex$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.FindSliceIndex");
}

function FindSliceIndex$$$FindSliceIndex$$Z59BB29F3(x$$36, e, _impl$$38) {
  return (0, _Extensions.String$$$findSliceIndex)(e, x$$36);
}

const TryFindSliceIndex = (0, _Types.declare)(function FSharpPlus_Control_TryFindSliceIndex() {});
exports.TryFindSliceIndex = TryFindSliceIndex;

function TryFindSliceIndex$reflection() {
  return (0, _Reflection.type)("FSharpPlus.Control.TryFindSliceIndex");
}

function TryFindSliceIndex$$$TryFindSliceIndex$$15491252(x$$37, e$$1, _impl$$39) {
  return (0, _Extensions.String$$$tryFindSliceIndex)(e$$1, x$$37);
}