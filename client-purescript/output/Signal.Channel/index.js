// Generated by psc version 0.10.2
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Prelude = require("../Prelude");
var Signal = require("../Signal");
var send = $foreign.sendP;
var channel = $foreign.channelP(Signal.constant);
module.exports = {
    channel: channel, 
    send: send, 
    subscribe: $foreign.subscribe
};
//# sourceMappingURL=index.js.map
