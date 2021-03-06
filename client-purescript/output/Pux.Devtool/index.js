// Generated by psc version 0.10.2
"use strict";
var $foreign = require("./foreign");
var Data_List = require("../Data.List");
var Data_Maybe = require("../Data.Maybe");
var Data_Tuple = require("../Data.Tuple");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Pux = require("../Pux");
var Pux_CSS = require("../Pux.CSS");
var Pux_Html = require("../Pux.Html");
var Pux_Html_Attributes = require("../Pux.Html.Attributes");
var Pux_Html_Events = require("../Pux.Html.Events");
var Prelude = require("../Prelude");
var Data_List_Types = require("../Data.List.Types");
var Data_Semiring = require("../Data.Semiring");
var Data_Functor = require("../Data.Functor");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Function = require("../Data.Function");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Pux_Html_Elements = require("../Pux.Html.Elements");
var Control_Bind = require("../Control.Bind");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var CSS_Display = require("../CSS.Display");
var CSS_Geometry = require("../CSS.Geometry");
var CSS_Size = require("../CSS.Size");
var CSS_Overflow = require("../CSS.Overflow");
var CSS_Background = require("../CSS.Background");
var Color = require("../Color");
var CSS_Font = require("../CSS.Font");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var CSS_Border = require("../CSS.Border");
var Signal = require("../Signal");
var Control_Applicative = require("../Control.Applicative");
var AppAction = (function () {
    function AppAction(value0) {
        this.value0 = value0;
    };
    AppAction.create = function (value0) {
        return new AppAction(value0);
    };
    return AppAction;
})();
var StepBack = (function () {
    function StepBack() {

    };
    StepBack.value = new StepBack();
    return StepBack;
})();
var StepForward = (function () {
    function StepForward() {

    };
    StepForward.value = new StepForward();
    return StepForward;
})();
var Rewind = (function () {
    function Rewind() {

    };
    Rewind.value = new Rewind();
    return Rewind;
})();
var FastForward = (function () {
    function FastForward() {

    };
    FastForward.value = new FastForward();
    return FastForward;
})();
var Clear = (function () {
    function Clear() {

    };
    Clear.value = new Clear();
    return Clear;
})();
var ToggleOpen = (function () {
    function ToggleOpen() {

    };
    ToggleOpen.value = new ToggleOpen();
    return ToggleOpen;
})();
var selectedState = function (s) {
    return Data_Maybe.fromMaybe(s.init)(Data_List.index(s.states)(s.index));
};
var selectedAction = function (s) {
    return Data_Maybe.fromMaybe("Awaiting action...")(Data_List.index(s.actions)(s.index));
};
var update = function (appUpdate) {
    return function (v) {
        return function (state) {
            if (v instanceof AppAction) {
                var effmodel = appUpdate(v.value0)(selectedState(state));
                return {
                    state: (function () {
                        var $5 = {};
                        for (var $6 in state) {
                            if (state.hasOwnProperty($6)) {
                                $5[$6] = state[$6];
                            };
                        };
                        $5.actions = new Data_List_Types.Cons($foreign.actionToString(v.value0), state.actions);
                        $5.states = new Data_List_Types.Cons(effmodel.state, state.states);
                        $5.length = state.length + 1 | 0;
                        $5.index = 0;
                        return $5;
                    })(), 
                    effects: Data_Functor.map(Data_Functor.functorArray)(Data_Functor.map(Control_Monad_Aff.functorAff)(AppAction.create))(effmodel.effects)
                };
            };
            if (v instanceof StepBack) {
                return Pux.noEffects((function () {
                    var $10 = {};
                    for (var $11 in state) {
                        if (state.hasOwnProperty($11)) {
                            $10[$11] = state[$11];
                        };
                    };
                    $10.index = (function () {
                        var $9 = state.index < state.length - 1;
                        if ($9) {
                            return state.index + 1 | 0;
                        };
                        if (!$9) {
                            return state.length - 1;
                        };
                        throw new Error("Failed pattern match at Pux.Devtool line 84, column 15 - line 86, column 38: " + [ $9.constructor.name ]);
                    })();
                    return $10;
                })());
            };
            if (v instanceof StepForward) {
                return Pux.noEffects((function () {
                    var $14 = {};
                    for (var $15 in state) {
                        if (state.hasOwnProperty($15)) {
                            $14[$15] = state[$15];
                        };
                    };
                    $14.index = (function () {
                        var $13 = state.index > 1;
                        if ($13) {
                            return state.index - 1;
                        };
                        if (!$13) {
                            return 0;
                        };
                        throw new Error("Failed pattern match at Pux.Devtool line 90, column 21 - line 90, column 72: " + [ $13.constructor.name ]);
                    })();
                    return $14;
                })());
            };
            if (v instanceof Rewind) {
                return Pux.noEffects((function () {
                    var $17 = {};
                    for (var $18 in state) {
                        if (state.hasOwnProperty($18)) {
                            $17[$18] = state[$18];
                        };
                    };
                    $17.index = state.length - 1;
                    return $17;
                })());
            };
            if (v instanceof FastForward) {
                return Pux.noEffects((function () {
                    var $20 = {};
                    for (var $21 in state) {
                        if (state.hasOwnProperty($21)) {
                            $20[$21] = state[$21];
                        };
                    };
                    $20.index = 0;
                    return $20;
                })());
            };
            if (v instanceof Clear) {
                return Pux.noEffects((function () {
                    var $23 = {};
                    for (var $24 in state) {
                        if (state.hasOwnProperty($24)) {
                            $23[$24] = state[$24];
                        };
                    };
                    $23.index = 0;
                    $23.length = 1;
                    $23.states = Data_List.singleton(selectedState(state));
                    $23.actions = Data_List.singleton(selectedAction(state));
                    return $23;
                })());
            };
            if (v instanceof ToggleOpen) {
                return Pux.noEffects((function () {
                    var $26 = {};
                    for (var $27 in state) {
                        if (state.hasOwnProperty($27)) {
                            $26[$27] = state[$27];
                        };
                    };
                    $26.opened = !state.opened;
                    return $26;
                })());
            };
            throw new Error("Failed pattern match at Pux.Devtool line 71, column 1 - line 81, column 4: " + [ appUpdate.constructor.name, v.constructor.name, state.constructor.name ]);
        };
    };
};
var view = function (appView) {
    return function (state) {
        return Pux_Html_Elements.div([  ])([ Pux_Html_Elements.style([  ])([ Pux_Html_Elements.text("\n        .pux-devtool {\n          z-index: 16777271;\n        }\n        \n        .pux-devtool-container {\n          font-family: sans-serif;\n          font-size: 14px;\n          line-height: 1.5;\n          position: relative;\n          padding: 1em;\n          overflow: scroll;\n          height: 100%;\n        }\n\n        .pux-devtool-icon {\n          display: inline-block;\n          height: 1.5em;\n        }\n\n        .pux-devtool-icon svg {\n          fill: #E6E6E6;\n          height: 1em;\n          width: 1em;\n          vertical-align: middle;\n        }\n\n        .pux-devtool-container h1 .pux-devtool-icon {\n          margin-right: .25em;\n        }\n\n        .pux-devtool-actions {\n          position: absolute;\n          top: 1em;\n          right: 0;\n        }\n\n        .pux-devtool-states, .pux-devtool-actions button {\n          display: inline-block;\n          margin-right: .5em;\n          vertical-align: middle;\n        }\n\n        .pux-devtool button {\n          color: #E6E6E6;\n          border: none;\n          border-radius: 3px;\n          background: #272129;\n          padding: .2em;\n        }\n\n        .pux-devtool button:hover {\n          cursor: pointer;\n          color: #FFFFFF;\n          text-decoration: underline;\n        }\n\n        .pux-devtool button:active {\n          background: #272129;\n          cursor: pointer;\n        }\n    ") ]), Pux_Html_Elements.div([ Pux_Html_Attributes.className("pux-devtool"), Pux_CSS.style(Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Display.position(CSS_Display.fixed))(function () {
            return Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Geometry.right(CSS_Size.px(0.0)))(function () {
                return Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Geometry.width(CSS_Size.px((function () {
                    if (state.opened) {
                        return state.width;
                    };
                    if (!state.opened) {
                        return 0.0;
                    };
                    throw new Error("Failed pattern match at Pux.Devtool line 175, column 13 - line 177, column 13: " + [ state.opened.constructor.name ]);
                })())))(function () {
                    return Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Geometry.top(CSS_Size.px(0.0)))(function () {
                        return Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Geometry.height(CSS_Size.pct(100.0)))(function () {
                            return Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Overflow.overflow(CSS_Overflow.visible))(function () {
                                return Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Background.backgroundColor(Color.rgb(66)(66)(84)))(function () {
                                    return CSS_Font.color(Color.rgb(249)(249)(249));
                                });
                            });
                        });
                    });
                });
            });
        })) ])([ Pux_Html_Elements.div([ Pux_Html_Attributes.className("pux-devtool-container") ])([ Pux_Html_Elements.h1([ Pux_CSS.style(Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Font.fontSize(CSS_Size.em(1.2)))(function () {
            return Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Geometry.marginTop(CSS_Size.px(0.0)))(function () {
                return CSS_Font.fontWeight(CSS_Font.weight(400.0));
            });
        })) ])([ Pux_Html_Elements.span([ Pux_Html_Attributes.className("pux-devtool-icon gear") ])([ Pux_Html_Elements.svg([ Pux_Html_Attributes.viewBox("0 0 12 14") ])([ Pux_Html_Elements.path([ Pux_Html_Attributes.d("M8 7q0-0.828-0.586-1.414t-1.414-0.586-1.414 0.586-0.586 1.414 0.586 1.414 1.414 0.586 1.414-0.586 0.586-1.414zM12 6.148v1.734q0 0.094-0.062 0.18t-0.156 0.102l-1.445 0.219q-0.148 0.422-0.305 0.711 0.273 0.391 0.836 1.078 0.078 0.094 0.078 0.195t-0.070 0.18q-0.211 0.289-0.773 0.844t-0.734 0.555q-0.094 0-0.203-0.070l-1.078-0.844q-0.344 0.18-0.711 0.297-0.125 1.062-0.227 1.453-0.055 0.219-0.281 0.219h-1.734q-0.109 0-0.191-0.066t-0.090-0.168l-0.219-1.438q-0.383-0.125-0.703-0.289l-1.102 0.836q-0.078 0.070-0.195 0.070-0.109 0-0.195-0.086-0.984-0.891-1.289-1.313-0.055-0.078-0.055-0.18 0-0.094 0.062-0.18 0.117-0.164 0.398-0.52t0.422-0.551q-0.211-0.391-0.32-0.773l-1.43-0.211q-0.102-0.016-0.164-0.098t-0.062-0.184v-1.734q0-0.094 0.062-0.18t0.148-0.102l1.453-0.219q0.109-0.359 0.305-0.719-0.312-0.445-0.836-1.078-0.078-0.094-0.078-0.187 0-0.078 0.070-0.18 0.203-0.281 0.77-0.84t0.738-0.559q0.102 0 0.203 0.078l1.078 0.836q0.344-0.18 0.711-0.297 0.125-1.062 0.227-1.453 0.055-0.219 0.281-0.219h1.734q0.109 0 0.191 0.066t0.090 0.168l0.219 1.438q0.383 0.125 0.703 0.289l1.109-0.836q0.070-0.070 0.187-0.070 0.102 0 0.195 0.078 1.008 0.93 1.289 1.328 0.055 0.062 0.055 0.172 0 0.094-0.062 0.18-0.117 0.164-0.398 0.52t-0.422 0.551q0.203 0.391 0.32 0.766l1.43 0.219q0.102 0.016 0.164 0.098t0.062 0.184z") ])([  ]) ]) ]), Pux_Html_Elements.text("Pux Devtool") ]), Pux_Html_Elements.div([ Pux_Html_Attributes.className("pux-devtool-actions") ])([ Pux_Html_Elements.span([ Pux_Html_Attributes.className("pux-devtool-states") ])([ Pux_Html_Elements.text(Data_Show.show(Data_Show.showInt)(state.length - state.index) + (" / " + Data_Show.show(Data_Show.showInt)(state.length))) ]), Pux_Html_Elements.button([ Pux_Html_Events.onClick(Data_Function["const"](Rewind.value)) ])([ Pux_Html_Elements.span([ Pux_Html_Attributes.className("pux-devtool-icon fast-backward") ])([ Pux_Html_Elements.svg([ Pux_Html_Attributes.viewBox("0 0 14 14") ])([ Pux_Html_Elements.path([ Pux_Html_Attributes.d("M13.648 1.102q0.148-0.148 0.25-0.102t0.102 0.25v11.5q0 0.203-0.102 0.25t-0.25-0.102l-5.547-5.547q-0.070-0.070-0.102-0.148v5.547q0 0.203-0.102 0.25t-0.25-0.102l-5.547-5.547q-0.070-0.070-0.102-0.148v5.297q0 0.203-0.148 0.352t-0.352 0.148h-1q-0.203 0-0.352-0.148t-0.148-0.352v-11q0-0.203 0.148-0.352t0.352-0.148h1q0.203 0 0.352 0.148t0.148 0.352v5.297q0.031-0.086 0.102-0.148l5.547-5.547q0.148-0.148 0.25-0.102t0.102 0.25v5.547q0.031-0.086 0.102-0.148z") ])([  ]) ]) ]) ]), Pux_Html_Elements.button([ Pux_Html_Events.onClick(Data_Function["const"](StepBack.value)) ])([ Pux_Html_Elements.span([ Pux_Html_Attributes.className("pux-devtool-icon step-backward") ])([ Pux_Html_Elements.svg([ Pux_Html_Attributes.viewBox("0 0 8 14") ])([ Pux_Html_Elements.path([ Pux_Html_Attributes.d("M7.648 1.102q0.148-0.148 0.25-0.102t0.102 0.25v11.5q0 0.203-0.102 0.25t-0.25-0.102l-5.547-5.547q-0.070-0.070-0.102-0.148v5.297q0 0.203-0.148 0.352t-0.352 0.148h-1q-0.203 0-0.352-0.148t-0.148-0.352v-11q0-0.203 0.148-0.352t0.352-0.148h1q0.203 0 0.352 0.148t0.148 0.352v5.297q0.031-0.086 0.102-0.148z") ])([  ]) ]) ]) ]), Pux_Html_Elements.button([ Pux_Html_Events.onClick(Data_Function["const"](StepForward.value)) ])([ Pux_Html_Elements.span([ Pux_Html_Attributes.className("pux-devtool-icon step-forward") ])([ Pux_Html_Elements.svg([ Pux_Html_Attributes.viewBox("0 0 8 14") ])([ Pux_Html_Elements.path([ Pux_Html_Attributes.d("M0.352 12.898q-0.148 0.148-0.25 0.102t-0.102-0.25v-11.5q0-0.203 0.102-0.25t0.25 0.102l5.547 5.547q0.062 0.062 0.102 0.148v-5.297q0-0.203 0.148-0.352t0.352-0.148h1q0.203 0 0.352 0.148t0.148 0.352v11q0 0.203-0.148 0.352t-0.352 0.148h-1q-0.203 0-0.352-0.148t-0.148-0.352v-5.297q-0.039 0.078-0.102 0.148z") ])([  ]) ]) ]) ]), Pux_Html_Elements.button([ Pux_Html_Events.onClick(Data_Function["const"](FastForward.value)) ])([ Pux_Html_Elements.span([ Pux_Html_Attributes.className("pux-devtool-icon fast-forward") ])([ Pux_Html_Elements.svg([ Pux_Html_Attributes.viewBox("0 0 14 14") ])([ Pux_Html_Elements.path([ Pux_Html_Attributes.d("M0.352 12.898q-0.148 0.148-0.25 0.102t-0.102-0.25v-11.5q0-0.203 0.102-0.25t0.25 0.102l5.547 5.547q0.062 0.062 0.102 0.148v-5.547q0-0.203 0.102-0.25t0.25 0.102l5.547 5.547q0.062 0.062 0.102 0.148v-5.297q0-0.203 0.148-0.352t0.352-0.148h1q0.203 0 0.352 0.148t0.148 0.352v11q0 0.203-0.148 0.352t-0.352 0.148h-1q-0.203 0-0.352-0.148t-0.148-0.352v-5.297q-0.039 0.078-0.102 0.148l-5.547 5.547q-0.148 0.148-0.25 0.102t-0.102-0.25v-5.547q-0.039 0.078-0.102 0.148z") ])([  ]) ]) ]) ]), Pux_Html_Elements.button([ Pux_Html_Events.onClick(Data_Function["const"](Clear.value)) ])([ Pux_Html_Elements.span([ Pux_Html_Attributes.className("pux-devtool-icon trash") ])([ Pux_Html_Elements.svg([ Pux_Html_Attributes.viewBox("0 0 11 14") ])([ Pux_Html_Elements.path([ Pux_Html_Attributes.d("M4 5.75v4.5q0 0.109-0.070 0.18t-0.18 0.070h-0.5q-0.109 0-0.18-0.070t-0.070-0.18v-4.5q0-0.109 0.070-0.18t0.18-0.070h0.5q0.109 0 0.18 0.070t0.070 0.18zM6 5.75v4.5q0 0.109-0.070 0.18t-0.18 0.070h-0.5q-0.109 0-0.18-0.070t-0.070-0.18v-4.5q0-0.109 0.070-0.18t0.18-0.070h0.5q0.109 0 0.18 0.070t0.070 0.18zM8 5.75v4.5q0 0.109-0.070 0.18t-0.18 0.070h-0.5q-0.109 0-0.18-0.070t-0.070-0.18v-4.5q0-0.109 0.070-0.18t0.18-0.070h0.5q0.109 0 0.18 0.070t0.070 0.18zM9 11.406v-7.406h-7v7.406q0 0.172 0.055 0.316t0.113 0.211 0.082 0.066h6.5q0.023 0 0.082-0.066t0.113-0.211 0.055-0.316zM3.75 3h3.5l-0.375-0.914q-0.055-0.070-0.133-0.086h-2.477q-0.078 0.016-0.133 0.086zM11 3.25v0.5q0 0.109-0.070 0.18t-0.18 0.070h-0.75v7.406q0 0.648-0.367 1.121t-0.883 0.473h-6.5q-0.516 0-0.883-0.457t-0.367-1.105v-7.438h-0.75q-0.109 0-0.18-0.070t-0.070-0.18v-0.5q0-0.109 0.070-0.18t0.18-0.070h2.414l0.547-1.305q0.117-0.289 0.422-0.492t0.617-0.203h2.5q0.312 0 0.617 0.203t0.422 0.492l0.547 1.305h2.414q0.109 0 0.18 0.070t0.070 0.18z") ])([  ]) ]) ]) ]) ]), Pux_Html_Elements.div([ Pux_Html_Attributes.style([ new Data_Tuple.Tuple("marginTop", "1em"), new Data_Tuple.Tuple("fontWeight", "bold") ]) ])([ Pux_Html_Elements.text(selectedAction(state)) ]), Pux_Html_Elements.div([ Pux_CSS.style(Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Font.fontSize(CSS_Size.em(0.8)))(function () {
            return CSS_Geometry.marginTop(CSS_Size.em(1.0));
        })), Pux_Html_Attributes.dangerouslySetInnerHTML($foreign.stateToString(selectedState(state))) ])([  ]) ]), Pux_Html_Elements.div([ Pux_Html_Attributes.className("toggle-hide"), Pux_Html_Events.onClick(Data_Function["const"](ToggleOpen.value)), Pux_Html_Attributes.style(Data_Semigroup.append(Data_Semigroup.semigroupArray)([ new Data_Tuple.Tuple("lineHeight", "30px"), new Data_Tuple.Tuple("cursor", "pointer"), new Data_Tuple.Tuple("textAlign", "center"), new Data_Tuple.Tuple("verticalAlign", "middle") ])(Pux_CSS.css(Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Display.position(CSS_Display.absolute))(function () {
            return Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Background.backgroundColor(Color.rgb(66)(66)(84)))(function () {
                return Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Border.borderRadius(CSS_Size.px(3.0))(CSS_Size.px(0.0))(CSS_Size.px(0.0))(CSS_Size.px(3.0)))(function () {
                    return Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Geometry.top(CSS_Size.pct(50.0)))(function () {
                        return Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Geometry.left(CSS_Size.px(-12.0)))(function () {
                            return Control_Bind.bind(CSS_Stylesheet.bindStyleM)(CSS_Geometry.height(CSS_Size.px(30.0)))(function () {
                                return CSS_Geometry.width(CSS_Size.px(12.0));
                            });
                        });
                    });
                });
            });
        })))) ])([ Pux_Html_Elements.span([ Pux_Html_Attributes.className("pux-devtool-icon caret-right") ])([ Pux_Html_Elements.svg([ Pux_Html_Attributes.viewBox("0 0 5 14") ])([ Pux_Html_Elements.path([ Pux_Html_Attributes.d("M4.5 7q0 0.203-0.148 0.352l-3.5 3.5q-0.148 0.148-0.352 0.148t-0.352-0.148-0.148-0.352v-7q0-0.203 0.148-0.352t0.352-0.148 0.352 0.148l3.5 3.5q0.148 0.148 0.148 0.352z") ])([  ]) ]) ]) ]) ]), Pux_Html_Elements.div([ Pux_Html_Attributes.className("pux-devtool-app-container"), Pux_CSS.style(CSS_Geometry.marginRight(CSS_Size.px((function () {
            if (state.opened) {
                return state.width;
            };
            if (!state.opened) {
                return 0.0;
            };
            throw new Error("Failed pattern match at Pux.Devtool line 290, column 13 - line 292, column 9: " + [ state.opened.constructor.name ]);
        })()))) ])([ Data_Functor.map(Pux_Html_Elements.functorHtml)(AppAction.create)(appView(selectedState(state))) ]) ]);
    };
};
var init = function (s) {
    return {
        actions: Data_List.singleton("App initialized. Awaiting action..."), 
        states: Data_List.singleton(s), 
        init: s, 
        length: 1, 
        index: 0, 
        opened: true, 
        width: 360.0
    };
};
var start = function (config) {
    return function __do() {
        var v = Pux.start({
            initialState: init(config.initialState), 
            update: update(config.update), 
            view: view(config.view), 
            inputs: Data_Functor.map(Data_Functor.functorArray)(Data_Functor.map(Signal.functorSignal)(AppAction.create))(config.inputs)
        })();
        var $32 = {};
        for (var $33 in v) {
            if (v.hasOwnProperty($33)) {
                $32[$33] = v[$33];
            };
        };
        $32.state = Data_Functor.map(Signal.functorSignal)(selectedState)(v.state);
        return $32;
    };
};
module.exports = {
    AppAction: AppAction, 
    StepBack: StepBack, 
    StepForward: StepForward, 
    Rewind: Rewind, 
    FastForward: FastForward, 
    Clear: Clear, 
    ToggleOpen: ToggleOpen, 
    init: init, 
    selectedAction: selectedAction, 
    selectedState: selectedState, 
    start: start, 
    update: update, 
    view: view, 
    actionToString: $foreign.actionToString, 
    stateToString: $foreign.stateToString
};
//# sourceMappingURL=index.js.map
