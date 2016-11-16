// Generated by psc version 0.10.2
"use strict";
var Control_Comonad = require("../Control.Comonad");
var Control_Comonad_Env_Trans = require("../Control.Comonad.Env.Trans");
var Data_Tuple = require("../Data.Tuple");
var ComonadAsk = function (__superclass_Control$dotComonad$dotComonad_0, ask) {
    this["__superclass_Control.Comonad.Comonad_0"] = __superclass_Control$dotComonad$dotComonad_0;
    this.ask = ask;
};
var ComonadEnv = function (__superclass_Control$dotComonad$dotEnv$dotClass$dotComonadAsk_0, local) {
    this["__superclass_Control.Comonad.Env.Class.ComonadAsk_0"] = __superclass_Control$dotComonad$dotEnv$dotClass$dotComonadAsk_0;
    this.local = local;
};
var local = function (dict) {
    return dict.local;
};
var comonadAskTuple = new ComonadAsk(function () {
    return Data_Tuple.comonadTuple;
}, Data_Tuple.fst);
var comonadEnvTuple = new ComonadEnv(function () {
    return comonadAskTuple;
}, function (f) {
    return function (v) {
        return new Data_Tuple.Tuple(f(v.value0), v.value1);
    };
});
var comonadAskEnvT = function (dictComonad) {
    return new ComonadAsk(function () {
        return Control_Comonad_Env_Trans.comonadEnvT(dictComonad);
    }, function (v) {
        return Data_Tuple.fst(v);
    });
};
var comonadEnvEnvT = function (dictComonad) {
    return new ComonadEnv(function () {
        return comonadAskEnvT(dictComonad);
    }, function (f) {
        return function (v) {
            return new Data_Tuple.Tuple(f(v.value0), v.value1);
        };
    });
};
var ask = function (dict) {
    return dict.ask;
};
var asks = function (dictComonadEnv) {
    return function (f) {
        return function (x) {
            return f(ask(dictComonadEnv["__superclass_Control.Comonad.Env.Class.ComonadAsk_0"]())(x));
        };
    };
};
module.exports = {
    ComonadAsk: ComonadAsk, 
    ComonadEnv: ComonadEnv, 
    ask: ask, 
    asks: asks, 
    local: local, 
    comonadAskTuple: comonadAskTuple, 
    comonadEnvTuple: comonadEnvTuple, 
    comonadAskEnvT: comonadAskEnvT, 
    comonadEnvEnvT: comonadEnvEnvT
};
//# sourceMappingURL=index.js.map