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
var Null = function (x) {
    return x;
};
var unNull = function (v) {
    return v;
};
var readNull = function (v) {
    return function (value) {
        if (Data_Foreign.isNull(value)) {
            return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(Data_Maybe.Nothing.value);
        };
        return Data_Functor.map(Control_Monad_Except_Trans.functorExceptT(Data_Identity.functorIdentity))(function ($5) {
            return Null(Data_Maybe.Just.create($5));
        })(v(value));
    };
};
module.exports = {
    Null: Null, 
    readNull: readNull, 
    unNull: unNull, 
    writeNull: $foreign.writeNull
};
//# sourceMappingURL=index.js.map