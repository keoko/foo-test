// Generated by psc version 0.10.2
"use strict";
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Enum = require("../Data.Enum");
var Data_Eq = require("../Data.Eq");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Bounded = require("../Data.Bounded");
var Data_Show = require("../Data.Show");
var HAVE_NOTHING = (function () {
    function HAVE_NOTHING() {

    };
    HAVE_NOTHING.value = new HAVE_NOTHING();
    return HAVE_NOTHING;
})();
var HAVE_METADATA = (function () {
    function HAVE_METADATA() {

    };
    HAVE_METADATA.value = new HAVE_METADATA();
    return HAVE_METADATA;
})();
var HAVE_CURRENT_DATA = (function () {
    function HAVE_CURRENT_DATA() {

    };
    HAVE_CURRENT_DATA.value = new HAVE_CURRENT_DATA();
    return HAVE_CURRENT_DATA;
})();
var HAVE_FUTURE_DATA = (function () {
    function HAVE_FUTURE_DATA() {

    };
    HAVE_FUTURE_DATA.value = new HAVE_FUTURE_DATA();
    return HAVE_FUTURE_DATA;
})();
var HAVE_ENOUGH_DATA = (function () {
    function HAVE_ENOUGH_DATA() {

    };
    HAVE_ENOUGH_DATA.value = new HAVE_ENOUGH_DATA();
    return HAVE_ENOUGH_DATA;
})();
var toEnumReadyState = function (v) {
    if (v === 0) {
        return new Data_Maybe.Just(HAVE_NOTHING.value);
    };
    if (v === 1) {
        return new Data_Maybe.Just(HAVE_METADATA.value);
    };
    if (v === 2) {
        return new Data_Maybe.Just(HAVE_CURRENT_DATA.value);
    };
    if (v === 3) {
        return new Data_Maybe.Just(HAVE_FUTURE_DATA.value);
    };
    if (v === 4) {
        return new Data_Maybe.Just(HAVE_ENOUGH_DATA.value);
    };
    return Data_Maybe.Nothing.value;
};
var showReadyState = new Data_Show.Show(function (v) {
    if (v instanceof HAVE_NOTHING) {
        return "HAVE_NOTHING";
    };
    if (v instanceof HAVE_METADATA) {
        return "HAVE_METADATA";
    };
    if (v instanceof HAVE_CURRENT_DATA) {
        return "HAVE_CURRENT_DATA";
    };
    if (v instanceof HAVE_FUTURE_DATA) {
        return "HAVE_FUTURE_DATA";
    };
    if (v instanceof HAVE_ENOUGH_DATA) {
        return "HAVE_ENOUGH_DATA";
    };
    throw new Error("Failed pattern match at DOM.HTML.HTMLMediaElement.ReadyState line 31, column 3 - line 32, column 3: " + [ v.constructor.name ]);
});
var fromEnumReadyState = function (v) {
    if (v instanceof HAVE_NOTHING) {
        return 0;
    };
    if (v instanceof HAVE_METADATA) {
        return 1;
    };
    if (v instanceof HAVE_CURRENT_DATA) {
        return 2;
    };
    if (v instanceof HAVE_FUTURE_DATA) {
        return 3;
    };
    if (v instanceof HAVE_ENOUGH_DATA) {
        return 4;
    };
    throw new Error("Failed pattern match at DOM.HTML.HTMLMediaElement.ReadyState line 49, column 3 - line 54, column 25: " + [ v.constructor.name ]);
};
var eqReadyState = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof HAVE_NOTHING && y instanceof HAVE_NOTHING) {
            return true;
        };
        if (x instanceof HAVE_METADATA && y instanceof HAVE_METADATA) {
            return true;
        };
        if (x instanceof HAVE_CURRENT_DATA && y instanceof HAVE_CURRENT_DATA) {
            return true;
        };
        if (x instanceof HAVE_FUTURE_DATA && y instanceof HAVE_FUTURE_DATA) {
            return true;
        };
        if (x instanceof HAVE_ENOUGH_DATA && y instanceof HAVE_ENOUGH_DATA) {
            return true;
        };
        return false;
    };
});
var ordReadyState = new Data_Ord.Ord(function () {
    return eqReadyState;
}, function (x) {
    return function (y) {
        if (x instanceof HAVE_NOTHING && y instanceof HAVE_NOTHING) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof HAVE_NOTHING) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof HAVE_NOTHING) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof HAVE_METADATA && y instanceof HAVE_METADATA) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof HAVE_METADATA) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof HAVE_METADATA) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof HAVE_CURRENT_DATA && y instanceof HAVE_CURRENT_DATA) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof HAVE_CURRENT_DATA) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof HAVE_CURRENT_DATA) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof HAVE_FUTURE_DATA && y instanceof HAVE_FUTURE_DATA) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof HAVE_FUTURE_DATA) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof HAVE_FUTURE_DATA) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof HAVE_ENOUGH_DATA && y instanceof HAVE_ENOUGH_DATA) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at DOM.HTML.HTMLMediaElement.ReadyState line 15, column 1 - line 15, column 48: " + [ x.constructor.name, y.constructor.name ]);
    };
});
var enumReadyState = new Data_Enum.Enum(function () {
    return ordReadyState;
}, Data_Enum.defaultPred(toEnumReadyState)(fromEnumReadyState), Data_Enum.defaultSucc(toEnumReadyState)(fromEnumReadyState));
var boundedReadyState = new Data_Bounded.Bounded(function () {
    return ordReadyState;
}, HAVE_NOTHING.value, HAVE_ENOUGH_DATA.value);
var boundedEnumReadyState = new Data_Enum.BoundedEnum(function () {
    return boundedReadyState;
}, function () {
    return enumReadyState;
}, 5, fromEnumReadyState, toEnumReadyState);
module.exports = {
    HAVE_NOTHING: HAVE_NOTHING, 
    HAVE_METADATA: HAVE_METADATA, 
    HAVE_CURRENT_DATA: HAVE_CURRENT_DATA, 
    HAVE_FUTURE_DATA: HAVE_FUTURE_DATA, 
    HAVE_ENOUGH_DATA: HAVE_ENOUGH_DATA, 
    eqReadyState: eqReadyState, 
    ordReadyState: ordReadyState, 
    boundedReadyState: boundedReadyState, 
    enumReadyState: enumReadyState, 
    boundedEnumReadyState: boundedEnumReadyState, 
    showReadyState: showReadyState
};
//# sourceMappingURL=index.js.map
