// Generated by psc version 0.10.2
"use strict";
var Prelude = require("../Prelude");
var Control_Comonad = require("../Control.Comonad");
var Control_Comonad_Traced_Trans = require("../Control.Comonad.Traced.Trans");
var Data_Monoid = require("../Data.Monoid");
var Data_Tuple = require("../Data.Tuple");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var ComonadTraced = function (__superclass_Control$dotComonad$dotComonad_0, track) {
    this["__superclass_Control.Comonad.Comonad_0"] = __superclass_Control$dotComonad$dotComonad_0;
    this.track = track;
};
var track = function (dict) {
    return dict.track;
};
var tracks = function (dictComonadTraced) {
    return function (f) {
        return function (w) {
            return track(dictComonadTraced)(f(Control_Comonad.extract(dictComonadTraced["__superclass_Control.Comonad.Comonad_0"]())(w)))(w);
        };
    };
};
var listens = function (dictFunctor) {
    return function (f) {
        return function (v) {
            return Data_Functor.map(dictFunctor)(function (g) {
                return function (t) {
                    return new Data_Tuple.Tuple(g(t), f(t));
                };
            })(v);
        };
    };
};
var listen = function (dictFunctor) {
    return function (v) {
        return Data_Functor.map(dictFunctor)(function (f) {
            return function (t) {
                return new Data_Tuple.Tuple(f(t), t);
            };
        })(v);
    };
};
var comonadTracedTracedT = function (dictComonad) {
    return function (dictMonoid) {
        return new ComonadTraced(function () {
            return Control_Comonad_Traced_Trans.comonadTracedT(dictComonad)(dictMonoid);
        }, function (t) {
            return function (v) {
                return Control_Comonad.extract(dictComonad)(v)(t);
            };
        });
    };
};
var censor = function (dictFunctor) {
    return function (f) {
        return function (v) {
            return Data_Functor.map(dictFunctor)(function (v1) {
                return function ($18) {
                    return v1(f($18));
                };
            })(v);
        };
    };
};
module.exports = {
    ComonadTraced: ComonadTraced, 
    censor: censor, 
    listen: listen, 
    listens: listens, 
    track: track, 
    tracks: tracks, 
    comonadTracedTracedT: comonadTracedTracedT
};
//# sourceMappingURL=index.js.map
