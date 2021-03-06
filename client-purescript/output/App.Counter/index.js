// Generated by psc version 0.10.2
"use strict";
var Data_String = require("../Data.String");
var Prelude = require("../Prelude");
var Pux_Html = require("../Pux.Html");
var Pux_Html_Events = require("../Pux.Html.Events");
var Pux_Html_Elements = require("../Pux.Html.Elements");
var Data_Function = require("../Data.Function");
var Data_Show = require("../Data.Show");
var Increment = (function () {
    function Increment() {

    };
    Increment.value = new Increment();
    return Increment;
})();
var Decrement = (function () {
    function Decrement() {

    };
    Decrement.value = new Decrement();
    return Decrement;
})();
var TextChange = (function () {
    function TextChange(value0) {
        this.value0 = value0;
    };
    TextChange.create = function (value0) {
        return new TextChange(value0);
    };
    return TextChange;
})();
var view = function (state) {
    return Pux_Html_Elements.div([  ])([ Pux_Html_Elements.button([ Pux_Html_Events.onClick(Data_Function["const"](Increment.value)) ])([ Pux_Html_Elements.text("Increment") ]), Pux_Html_Elements.span([  ])([ Pux_Html_Elements.text(Data_Show.show(Data_Show.showString)(state)) ]), Pux_Html_Elements.div([  ])([ Pux_Html_Elements.textarea([ Pux_Html_Events.onInput(TextChange.create) ])([ Pux_Html_Elements.text(state) ]) ]), Pux_Html_Elements.button([ Pux_Html_Events.onClick(Data_Function["const"](Decrement.value)) ])([ Pux_Html_Elements.text("Decrement") ]) ]);
};
var update = function (v) {
    return function (state) {
        if (v instanceof TextChange) {
            return Data_String.toUpper(v.value0.target.value);
        };
        return state;
    };
};
var init = "";
module.exports = {
    Increment: Increment, 
    Decrement: Decrement, 
    TextChange: TextChange, 
    init: init, 
    update: update, 
    view: view
};
//# sourceMappingURL=index.js.map
