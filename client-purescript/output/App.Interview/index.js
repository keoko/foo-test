// Generated by psc version 0.10.2
"use strict";
var Pux_Html = require("../Pux.Html");
var Pux_Html_Elements = require("../Pux.Html.Elements");
var questionsBlock = function (state) {
    return Pux_Html_Elements.div([  ])([ Pux_Html_Elements.text("questions") ]);
};
var view = function (state) {
    return Pux_Html_Elements.div([  ])([ Pux_Html_Elements.h2([  ])([ Pux_Html_Elements.text("Create an Interview") ]), Pux_Html_Elements.text("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."), questionsBlock(state) ]);
};
module.exports = {
    questionsBlock: questionsBlock, 
    view: view
};
//# sourceMappingURL=index.js.map