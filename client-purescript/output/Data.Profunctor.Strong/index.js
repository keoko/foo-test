// Generated by psc version 0.10.2
"use strict";
var Prelude = require("../Prelude");
var Data_Profunctor = require("../Data.Profunctor");
var Data_Tuple = require("../Data.Tuple");
var Data_Functor = require("../Data.Functor");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Category = require("../Control.Category");
var Strong = function (__superclass_Data$dotProfunctor$dotProfunctor_0, first, second) {
    this["__superclass_Data.Profunctor.Profunctor_0"] = __superclass_Data$dotProfunctor$dotProfunctor_0;
    this.first = first;
    this.second = second;
};
var strongFn = new Strong(function () {
    return Data_Profunctor.profunctorFn;
}, function (a2b) {
    return function (v) {
        return new Data_Tuple.Tuple(a2b(v.value0), v.value1);
    };
}, Data_Functor.map(Data_Tuple.functorTuple));
var second = function (dict) {
    return dict.second;
};
var first = function (dict) {
    return dict.first;
};
var splitStrong = function (dictCategory) {
    return function (dictStrong) {
        return function (l) {
            return function (r) {
                return Control_Semigroupoid.composeFlipped(dictCategory["__superclass_Control.Semigroupoid.Semigroupoid_0"]())(first(dictStrong)(l))(second(dictStrong)(r));
            };
        };
    };
};
var fanout = function (dictCategory) {
    return function (dictStrong) {
        return function (l) {
            return function (r) {
                var split = Data_Profunctor.dimap(dictStrong["__superclass_Data.Profunctor.Profunctor_0"]())(Control_Category.id(Control_Category.categoryFn))(function (a) {
                    return new Data_Tuple.Tuple(a, a);
                })(Control_Category.id(dictCategory));
                return Control_Semigroupoid.composeFlipped(dictCategory["__superclass_Control.Semigroupoid.Semigroupoid_0"]())(split)(splitStrong(dictCategory)(dictStrong)(l)(r));
            };
        };
    };
};
module.exports = {
    Strong: Strong, 
    fanout: fanout, 
    first: first, 
    second: second, 
    splitStrong: splitStrong, 
    strongFn: strongFn
};
//# sourceMappingURL=index.js.map
