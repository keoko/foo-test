// Generated by psc version 0.10.2
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Data_Enum = require("../Data.Enum");
var Data_Maybe = require("../Data.Maybe");
var DOM = require("../DOM");
var DOM_Event_EventPhase = require("../DOM.Event.EventPhase");
var DOM_Event_Types = require("../DOM.Event.Types");
var DOM_Node_Types = require("../DOM.Node.Types");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var eventPhase = function (dictPartial) {
    return function ($1) {
        return Data_Maybe.fromJust(dictPartial)(Data_Enum.toEnum(DOM_Event_EventPhase.boundedEnumEventPhase)($foreign.eventPhaseIndex($1)));
    };
};
module.exports = {
    eventPhase: eventPhase, 
    bubbles: $foreign.bubbles, 
    cancelable: $foreign.cancelable, 
    currentTarget: $foreign.currentTarget, 
    defaultPrevented: $foreign.defaultPrevented, 
    eventPhaseIndex: $foreign.eventPhaseIndex, 
    preventDefault: $foreign.preventDefault, 
    stopImmediatePropagation: $foreign.stopImmediatePropagation, 
    stopPropagation: $foreign.stopPropagation, 
    target: $foreign.target, 
    timeStamp: $foreign.timeStamp, 
    type_: $foreign.type_
};
//# sourceMappingURL=index.js.map
