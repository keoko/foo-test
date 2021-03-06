// Generated by psc version 0.10.2
"use strict";
var Prelude = require("../Prelude");
var Data_Generic = require("../Data.Generic");
var Data_NonEmpty = require("../Data.NonEmpty");
var CSS_Color = require("../CSS.Color");
var CSS_Property = require("../CSS.Property");
var CSS_Size = require("../CSS.Size");
var CSS_String = require("../CSS.String");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var Data_Eq = require("../Data.Eq");
var Data_Ord = require("../Data.Ord");
var Control_Apply = require("../Control.Apply");
var Data_Maybe = require("../Data.Maybe");
var Data_Unit = require("../Data.Unit");
var Data_Function = require("../Data.Function");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Functor = require("../Data.Functor");
var Control_Alternative = require("../Control.Alternative");
var GenericFontFamily = function (x) {
    return x;
};
var FontWeight = function (x) {
    return x;
};
var weight = function (i) {
    return FontWeight(CSS_Property.value(CSS_Property.valNumber)(i));
};
var valGenericFontFamily = new CSS_Property.Val(function (v) {
    return v;
});
var valFontWeight = new CSS_Property.Val(function (v) {
    return v;
});
var sansSerif = GenericFontFamily(CSS_String.fromString(CSS_Property.isStringValue)("sans-serif"));
var lighter = FontWeight(CSS_String.fromString(CSS_Property.isStringValue)("lighter"));
var genericGenericFontFamily = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Font.GenericFontFamily" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(GenericFontFamily))(Data_Generic.fromSpine(CSS_Property.genericValue)(v.value1[0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Font.GenericFontFamily", [ {
        sigConstructor: "CSS.Font.GenericFontFamily", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Font.GenericFontFamily", [ function ($dollarq) {
        return Data_Generic.toSpine(CSS_Property.genericValue)(v);
    } ]);
});
var genericFontWeight = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Font.FontWeight" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(FontWeight))(Data_Generic.fromSpine(CSS_Property.genericValue)(v.value1[0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Font.FontWeight", [ {
        sigConstructor: "CSS.Font.FontWeight", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Font.FontWeight", [ function ($dollarq) {
        return Data_Generic.toSpine(CSS_Property.genericValue)(v);
    } ]);
});
var fontWeight = CSS_Stylesheet.key(valFontWeight)(CSS_String.fromString(CSS_Property.isStringKey)("font-weight"));
var fontSize = CSS_Stylesheet.key(CSS_Size.valSize)(CSS_String.fromString(CSS_Property.isStringKey)("font-size"));
var fontFamily = function (a) {
    return function (b) {
        return CSS_Stylesheet.key(CSS_Property.valValue)(CSS_String.fromString(CSS_Property.isStringKey)("font-family"))(CSS_Property.value(CSS_Property.valList(CSS_Property.valValue))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.map(Data_Functor.functorArray)(function ($46) {
            return CSS_Property.value(CSS_Property.valString)(CSS_Property.quote($46));
        })(a))(Data_NonEmpty.oneOf(Control_Alternative.alternativeArray)(Data_Functor.map(Data_NonEmpty.functorNonEmpty(Data_Functor.functorArray))(CSS_Property.value(valGenericFontFamily))(b)))));
    };
};
var eqGenericFontFamily = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(CSS_Property.eqValue)(x)(y);
    };
});
var ordGenericFontFamily = new Data_Ord.Ord(function () {
    return eqGenericFontFamily;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(CSS_Property.ordValue)(x)(y);
    };
});
var eqFontWeight = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(CSS_Property.eqValue)(x)(y);
    };
});
var ordFontWeight = new Data_Ord.Ord(function () {
    return eqFontWeight;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(CSS_Property.ordValue)(x)(y);
    };
});
var color = CSS_Stylesheet.key(CSS_Property.valColor)(CSS_String.fromString(CSS_Property.isStringKey)("color"));
var bolder = FontWeight(CSS_String.fromString(CSS_Property.isStringValue)("bolder"));
var bold = FontWeight(CSS_String.fromString(CSS_Property.isStringValue)("bold"));
module.exports = {
    FontWeight: FontWeight, 
    GenericFontFamily: GenericFontFamily, 
    bold: bold, 
    bolder: bolder, 
    color: color, 
    fontFamily: fontFamily, 
    fontSize: fontSize, 
    fontWeight: fontWeight, 
    lighter: lighter, 
    sansSerif: sansSerif, 
    weight: weight, 
    eqGenericFontFamily: eqGenericFontFamily, 
    ordGenericFontFamily: ordGenericFontFamily, 
    genericGenericFontFamily: genericGenericFontFamily, 
    valGenericFontFamily: valGenericFontFamily, 
    eqFontWeight: eqFontWeight, 
    ordFontWeight: ordFontWeight, 
    genericFontWeight: genericFontWeight, 
    valFontWeight: valFontWeight
};
//# sourceMappingURL=index.js.map
