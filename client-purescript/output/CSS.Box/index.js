// Generated by psc version 0.10.2
"use strict";
var Prelude = require("../Prelude");
var Data_Generic = require("../Data.Generic");
var CSS_Border = require("../CSS.Border");
var CSS_Color = require("../CSS.Color");
var CSS_Common = require("../CSS.Common");
var CSS_Property = require("../CSS.Property");
var CSS_Size = require("../CSS.Size");
var CSS_String = require("../CSS.String");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var Data_Eq = require("../Data.Eq");
var Data_Ord = require("../Data.Ord");
var Control_Apply = require("../Control.Apply");
var Data_Maybe = require("../Data.Maybe");
var Data_Unit = require("../Data.Unit");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Function = require("../Data.Function");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Tuple = require("../Data.Tuple");
var BoxType = function (x) {
    return x;
};
var valBoxType = new CSS_Property.Val(function (v) {
    return v;
});
var paddingBox = BoxType(CSS_String.fromString(CSS_Property.isStringValue)("padding-box"));
var isStringBoxType = new CSS_String.IsString(function ($23) {
    return BoxType(CSS_String.fromString(CSS_Property.isStringValue)($23));
});
var insetBoxShadow = function (x) {
    return function (y) {
        return function (w) {
            return function (c) {
                return function (z) {
                    return CSS_Stylesheet.prefixed(CSS_Property.valTuple(CSS_Border.valStroke)(CSS_Property.valTuple(CSS_Size.valSize)(CSS_Property.valTuple(CSS_Size.valSize)(CSS_Property.valTuple(CSS_Size.valSize)(CSS_Property.valColor)))))(Data_Semigroup.append(CSS_Property.semigroupPrefixed)(CSS_Common.browsers)(CSS_String.fromString(CSS_Property.isStringPrefixed)("box-shadow")))(new Data_Tuple.Tuple(x, new Data_Tuple.Tuple(y, new Data_Tuple.Tuple(w, new Data_Tuple.Tuple(c, z)))));
                };
            };
        };
    };
};
var inheritBoxType = new CSS_Common.Inherit(CSS_String.fromString(isStringBoxType)("inherit"));
var genericBoxType = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Box.BoxType" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(BoxType))(Data_Generic.fromSpine(CSS_Property.genericValue)(v.value1[0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Box.BoxType", [ {
        sigConstructor: "CSS.Box.BoxType", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Box.BoxType", [ function ($dollarq) {
        return Data_Generic.toSpine(CSS_Property.genericValue)(v);
    } ]);
});
var eqBoxType = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(CSS_Property.eqValue)(x)(y);
    };
});
var ordBoxType = new Data_Ord.Ord(function () {
    return eqBoxType;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(CSS_Property.ordValue)(x)(y);
    };
});
var contentBox = BoxType(CSS_String.fromString(CSS_Property.isStringValue)("content-box"));
var boxSizing = CSS_Stylesheet.key(valBoxType)(CSS_String.fromString(CSS_Property.isStringKey)("box-sizing"));
var boxShadow = function (x) {
    return function (y) {
        return function (w) {
            return function (c) {
                return CSS_Stylesheet.prefixed(CSS_Property.valTuple(CSS_Size.valSize)(CSS_Property.valTuple(CSS_Size.valSize)(CSS_Property.valTuple(CSS_Size.valSize)(CSS_Property.valColor))))(Data_Semigroup.append(CSS_Property.semigroupPrefixed)(CSS_Common.browsers)(CSS_String.fromString(CSS_Property.isStringPrefixed)("box-shadow")))(new Data_Tuple.Tuple(x, new Data_Tuple.Tuple(y, new Data_Tuple.Tuple(w, c))));
            };
        };
    };
};
var borderBox = BoxType(CSS_String.fromString(CSS_Property.isStringValue)("border-box"));
module.exports = {
    borderBox: borderBox, 
    boxShadow: boxShadow, 
    boxSizing: boxSizing, 
    contentBox: contentBox, 
    insetBoxShadow: insetBoxShadow, 
    paddingBox: paddingBox, 
    eqBoxType: eqBoxType, 
    ordBoxType: ordBoxType, 
    genericBoxType: genericBoxType, 
    isStringBoxType: isStringBoxType, 
    valBoxType: valBoxType, 
    inheritBoxType: inheritBoxType
};
//# sourceMappingURL=index.js.map
