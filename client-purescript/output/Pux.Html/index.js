// Generated by psc version 0.10.2
"use strict";
var Data_Array = require("../Data.Array");
var Pux_Html_Elements = require("../Pux.Html.Elements");
var withChildren = function (f) {
    return function (htmls) {
        return f([  ])(htmls);
    };
};
var withChild = function (f) {
    return function (html) {
        return f([  ])(Data_Array.singleton(html));
    };
};
var withTextChild = function (f) {
    return function (txt) {
        return withChild(f)(Pux_Html_Elements.text(txt));
    };
};
var withAttr = function (f) {
    return function (attr) {
        return function (attrs) {
            return function (children) {
                return f(Data_Array.cons(attr)(attrs))(children);
            };
        };
    };
};
module.exports = {
    withAttr: withAttr, 
    withChild: withChild, 
    withChildren: withChildren, 
    withTextChild: withTextChild
};
//# sourceMappingURL=index.js.map