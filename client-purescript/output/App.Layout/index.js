// Generated by psc version 0.10.2
"use strict";
var App_Counter = require("../App.Counter");
var App_Header = require("../App.Header");
var App_NotFound = require("../App.NotFound");
var App_Interview = require("../App.Interview");
var App_Routes = require("../App.Routes");
var Prelude = require("../Prelude");
var Pux_Html = require("../Pux.Html");
var Pux_Html_Elements = require("../Pux.Html.Elements");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Child = (function () {
    function Child(value0) {
        this.value0 = value0;
    };
    Child.create = function (value0) {
        return new Child(value0);
    };
    return Child;
})();
var PageView = (function () {
    function PageView(value0) {
        this.value0 = value0;
    };
    PageView.create = function (value0) {
        return new PageView(value0);
    };
    return PageView;
})();
var view = function (state) {
    return Pux_Html_Elements.div([  ])([ App_Header.view(state), (function () {
        if (state.route instanceof App_Routes.Home) {
            return Data_Functor.map(Pux_Html_Elements.functorHtml)(Child.create)(App_Counter.view(state.count));
        };
        if (state.route instanceof App_Routes.NotFound) {
            return App_NotFound.view(state);
        };
        if (state.route instanceof App_Routes.Interview) {
            return App_Interview.view(state);
        };
        throw new Error("Failed pattern match at App.Layout line 33, column 7 - line 38, column 42: " + [ state.route.constructor.name ]);
    })() ]);
};
var update = function (v) {
    return function (state) {
        if (v instanceof PageView) {
            var $4 = {};
            for (var $5 in state) {
                if (state.hasOwnProperty($5)) {
                    $4[$5] = state[$5];
                };
            };
            $4.route = v.value0;
            return $4;
        };
        if (v instanceof Child) {
            var $8 = {};
            for (var $9 in state) {
                if (state.hasOwnProperty($9)) {
                    $8[$9] = state[$9];
                };
            };
            $8.count = App_Counter.update(v.value0)(state.count);
            return $8;
        };
        throw new Error("Failed pattern match at App.Layout line 25, column 1 - line 25, column 56: " + [ v.constructor.name, state.constructor.name ]);
    };
};
var init = {
    route: App_Routes.NotFound.value, 
    count: App_Counter.init
};
module.exports = {
    Child: Child, 
    PageView: PageView, 
    init: init, 
    update: update, 
    view: view
};
//# sourceMappingURL=index.js.map
