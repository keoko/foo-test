// Generated by psc version 0.10.2
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Maybe = require("../Data.Maybe");
var Data_Foreign = require("../Data.Foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans");
var Data_Identity = require("../Data.Identity");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Undefined = function (x) {
    return x;
};
var unUndefined = function (v) {
    return v;
};
var readUndefined = function (v) {
    return function (value) {
        if (Data_Foreign.isUndefined(value)) {
            return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(Data_Maybe.Nothing.value);
        };
        return Data_Functor.map(Control_Monad_Except_Trans.functorExceptT(Data_Identity.functorIdentity))(function ($5) {
            return Undefined(Data_Maybe.Just.create($5));
        })(v(value));
    };
};
module.exports = {
    Undefined: Undefined, 
    readUndefined: readUndefined, 
    unUndefined: unUndefined, 
    writeUndefined: $foreign.writeUndefined
};
//# sourceMappingURL=index.js.map
