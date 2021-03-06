// Generated by psc version 0.10.2
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Enum = require("../Data.Enum");
var Data_Foreign = require("../Data.Foreign");
var Data_Maybe = require("../Data.Maybe");
var DOM_Event_Types = require("../DOM.Event.Types");
var Data_Eq = require("../Data.Eq");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Bounded = require("../Data.Bounded");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Pixel = (function () {
    function Pixel() {

    };
    Pixel.value = new Pixel();
    return Pixel;
})();
var Line = (function () {
    function Line() {

    };
    Line.value = new Line();
    return Line;
})();
var Page = (function () {
    function Page() {

    };
    Page.value = new Page();
    return Page;
})();
var toEnumDeltaMode = function (v) {
    if (v === 0) {
        return new Data_Maybe.Just(Pixel.value);
    };
    if (v === 1) {
        return new Data_Maybe.Just(Line.value);
    };
    if (v === 2) {
        return new Data_Maybe.Just(Page.value);
    };
    return Data_Maybe.Nothing.value;
};
var fromEnumDeltaMode = function (v) {
    if (v instanceof Pixel) {
        return 0;
    };
    if (v instanceof Line) {
        return 1;
    };
    if (v instanceof Page) {
        return 2;
    };
    throw new Error("Failed pattern match at DOM.Event.WheelEvent line 66, column 3 - line 69, column 13: " + [ v.constructor.name ]);
};
var eventToWheelEvent = function ($13) {
    return DOM_Event_Types.readWheelEvent(Data_Foreign.toForeign($13));
};
var eqDeltaMode = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof Pixel && y instanceof Pixel) {
            return true;
        };
        if (x instanceof Line && y instanceof Line) {
            return true;
        };
        if (x instanceof Page && y instanceof Page) {
            return true;
        };
        return false;
    };
});
var ordDeltaMode = new Data_Ord.Ord(function () {
    return eqDeltaMode;
}, function (x) {
    return function (y) {
        if (x instanceof Pixel && y instanceof Pixel) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Pixel) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Pixel) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Line && y instanceof Line) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Line) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Line) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Page && y instanceof Page) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at DOM.Event.WheelEvent line 41, column 1 - line 41, column 46: " + [ x.constructor.name, y.constructor.name ]);
    };
});
var enumDeltaMode = new Data_Enum.Enum(function () {
    return ordDeltaMode;
}, Data_Enum.defaultPred(toEnumDeltaMode)(fromEnumDeltaMode), Data_Enum.defaultSucc(toEnumDeltaMode)(fromEnumDeltaMode));
var boundedDeltaMode = new Data_Bounded.Bounded(function () {
    return ordDeltaMode;
}, Pixel.value, Page.value);
var boundedEnumDeltaMode = new Data_Enum.BoundedEnum(function () {
    return boundedDeltaMode;
}, function () {
    return enumDeltaMode;
}, 3, fromEnumDeltaMode, toEnumDeltaMode);
var deltaMode = function (dictPartial) {
    return function ($14) {
        return Data_Maybe.fromJust(dictPartial)(Data_Enum.toEnum(boundedEnumDeltaMode)($foreign.deltaModeIndex($14)));
    };
};
module.exports = {
    Pixel: Pixel, 
    Line: Line, 
    Page: Page, 
    deltaMode: deltaMode, 
    eventToWheelEvent: eventToWheelEvent, 
    fromEnumDeltaMode: fromEnumDeltaMode, 
    toEnumDeltaMode: toEnumDeltaMode, 
    eqDeltaMode: eqDeltaMode, 
    ordDeltaMode: ordDeltaMode, 
    boundedDeltaMode: boundedDeltaMode, 
    enumDeltaMode: enumDeltaMode, 
    boundedEnumDeltaMode: boundedEnumDeltaMode, 
    deltaModeIndex: $foreign.deltaModeIndex, 
    deltaX: $foreign.deltaX, 
    deltaY: $foreign.deltaY, 
    deltaZ: $foreign.deltaZ
};
//# sourceMappingURL=index.js.map
