// Generated by psc version 0.10.2
"use strict";
var CSS = require("../CSS");
var CSS_Overflow = require("../CSS.Overflow");
var CSS_TextAlign = require("../CSS.TextAlign");
var CSS_Render = require("../CSS.Render");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var Data_Either = require("../Data.Either");
var Data_Foldable = require("../Data.Foldable");
var Data_Monoid = require("../Data.Monoid");
var Data_String = require("../Data.String");
var Data_Tuple = require("../Data.Tuple");
var Prelude = require("../Prelude");
var Pux_Html = require("../Pux.Html");
var Pux_Html_Attributes = require("../Pux.Html.Attributes");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var camelCase = function (str) {
    var capitalize = function (word) {
        return Data_String.toUpper(Data_String.take(1)(word)) + Data_String.drop(1)(word);
    };
    var pascalCase = Data_String.joinWith("")(Data_Functor.map(Data_Functor.functorArray)(capitalize)(Data_String.split("-")(str)));
    return Data_String.toLower(Data_String.take(1)(pascalCase)) + Data_String.drop(1)(pascalCase);
};
var css = function (rules) {
    var tuples = function (array) {
        return function (v) {
            if (v instanceof CSS_Stylesheet.Property) {
                return Data_Semigroup.append(Data_Semigroup.semigroupArray)(CSS_Render.collect(new Data_Tuple.Tuple(v.value0, v.value1)))(array);
            };
            return array;
        };
    };
    var tuple = function (p) {
        return Data_Either.either(function (v) {
            return Data_Monoid.mempty(Data_Tuple.monoidTuple(Data_Monoid.monoidString)(Data_Monoid.monoidString));
        })(function (v) {
            return new Data_Tuple.Tuple(camelCase(v.value0), v.value1);
        })(p);
    };
    return Data_Functor.map(Data_Functor.functorArray)(tuple)(Data_Foldable.foldl(Data_Foldable.foldableArray)(tuples)([  ])(CSS_Stylesheet.runS(rules)));
};
var style = function (rules) {
    return Pux_Html_Attributes.style(css(rules));
};
module.exports = {
    css: css, 
    style: style
};
//# sourceMappingURL=index.js.map
