// Generated by psc version 0.10.2
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_ST = require("../Control.Monad.ST");
var Data_Maybe = require("../Data.Maybe");
var peek = $foreign.peekImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
module.exports = {
    peek: peek, 
    "delete": $foreign["delete"], 
    "new": $foreign["new"], 
    poke: $foreign.poke
};
//# sourceMappingURL=index.js.map