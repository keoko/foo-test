// Generated by psc version 0.10.2
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Data_Foldable = require("../Data.Foldable");
var Data_Tuple = require("../Data.Tuple");
var DOM_File_Types = require("../DOM.File.Types");
var DOM_XHR_Types = require("../DOM.XHR.Types");
var Data_Unit = require("../Data.Unit");
var Data_Monoid = require("../Data.Monoid");
var FormDataString = (function () {
    function FormDataString(value0) {
        this.value0 = value0;
    };
    FormDataString.create = function (value0) {
        return new FormDataString(value0);
    };
    return FormDataString;
})();
var FormDataFile = (function () {
    function FormDataFile(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    FormDataFile.create = function (value0) {
        return function (value1) {
            return new FormDataFile(value0, value1);
        };
    };
    return FormDataFile;
})();
var FormDataBlob = (function () {
    function FormDataBlob(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    FormDataBlob.create = function (value0) {
        return function (value1) {
            return new FormDataBlob(value0, value1);
        };
    };
    return FormDataBlob;
})();
var toFormData = function (dictFoldable) {
    return function (dat) {
        var appendData = function (form) {
            return function (v) {
                if (v.value1 instanceof FormDataString) {
                    return $foreign.appendString(form)(v.value0)(v.value1.value0);
                };
                if (v.value1 instanceof FormDataFile) {
                    return $foreign.appendWithName(form)(v.value0)(v.value1.value1)(v.value1.value0);
                };
                if (v.value1 instanceof FormDataBlob) {
                    return $foreign.appendWithName(form)(v.value0)(v.value1.value1)(v.value1.value0);
                };
                throw new Error("Failed pattern match at DOM.XHR.FormData line 23, column 17 - line 23, column 93: " + [ form.constructor.name, v.constructor.name ]);
            };
        };
        var form = $foreign.newFormData(Data_Unit.unit);
        var _unit = Data_Foldable.foldMap(dictFoldable)(Data_Monoid.monoidUnit)(appendData(form))(dat);
        return form;
    };
};
module.exports = {
    FormDataString: FormDataString, 
    FormDataFile: FormDataFile, 
    FormDataBlob: FormDataBlob, 
    toFormData: toFormData
};
//# sourceMappingURL=index.js.map
