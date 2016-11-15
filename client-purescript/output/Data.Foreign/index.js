// Generated by psc version 0.10.2
"use strict";
var $foreign = require("./foreign");
var Prelude = require("../Prelude");
var Control_Monad_Except = require("../Control.Monad.Except");
var Data_Either = require("../Data.Either");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_Int = require("../Data.Int");
var Data_List_NonEmpty = require("../Data.List.NonEmpty");
var Data_Maybe = require("../Data.Maybe");
var Data_String = require("../Data.String");
var Data_Eq = require("../Data.Eq");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Show = require("../Data.Show");
var Data_Semigroup = require("../Data.Semigroup");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans");
var Data_Identity = require("../Data.Identity");
var Control_Applicative = require("../Control.Applicative");
var Data_Function = require("../Data.Function");
var Data_Boolean = require("../Data.Boolean");
var Prop = function (x) {
    return x;
};
var ForeignError = (function () {
    function ForeignError(value0) {
        this.value0 = value0;
    };
    ForeignError.create = function (value0) {
        return new ForeignError(value0);
    };
    return ForeignError;
})();
var TypeMismatch = (function () {
    function TypeMismatch(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TypeMismatch.create = function (value0) {
        return function (value1) {
            return new TypeMismatch(value0, value1);
        };
    };
    return TypeMismatch;
})();
var ErrorAtIndex = (function () {
    function ErrorAtIndex(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtIndex.create = function (value0) {
        return function (value1) {
            return new ErrorAtIndex(value0, value1);
        };
    };
    return ErrorAtIndex;
})();
var ErrorAtProperty = (function () {
    function ErrorAtProperty(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtProperty.create = function (value0) {
        return function (value1) {
            return new ErrorAtProperty(value0, value1);
        };
    };
    return ErrorAtProperty;
})();
var JSONError = (function () {
    function JSONError(value0) {
        this.value0 = value0;
    };
    JSONError.create = function (value0) {
        return new JSONError(value0);
    };
    return JSONError;
})();
var showForeignError = new Data_Show.Show(function (v) {
    if (v instanceof ForeignError) {
        return "(ForeignError " + (v.value0 + ")");
    };
    if (v instanceof ErrorAtIndex) {
        return "(ErrorAtIndex " + (Data_Show.show(Data_Show.showInt)(v.value0) + (" " + (Data_Show.show(showForeignError)(v.value1) + ")")));
    };
    if (v instanceof ErrorAtProperty) {
        return "(ErrorAtProperty " + (Data_Show.show(Data_Show.showString)(v.value0) + (" " + (Data_Show.show(showForeignError)(v.value1) + ")")));
    };
    if (v instanceof JSONError) {
        return "(JSONError " + (Data_Show.show(Data_Show.showString)(v.value0) + ")");
    };
    if (v instanceof TypeMismatch) {
        return "(TypeMismatch " + (Data_Show.show(Data_Show.showString)(v.value0) + (" " + (Data_Show.show(Data_Show.showString)(v.value1) + ")")));
    };
    throw new Error("Failed pattern match at Data.Foreign line 65, column 3 - line 66, column 3: " + [ v.constructor.name ]);
});
var renderForeignError = function (v) {
    if (v instanceof ForeignError) {
        return v.value0;
    };
    if (v instanceof ErrorAtIndex) {
        return "Error at array index " + (Data_Show.show(Data_Show.showInt)(v.value0) + (": " + Data_Show.show(showForeignError)(v.value1)));
    };
    if (v instanceof ErrorAtProperty) {
        return "Error at property " + (Data_Show.show(Data_Show.showString)(v.value0) + (": " + Data_Show.show(showForeignError)(v.value1)));
    };
    if (v instanceof JSONError) {
        return "JSON error: " + v.value0;
    };
    if (v instanceof TypeMismatch) {
        return "Type mismatch: expected " + (v.value0 + (", found " + v.value1));
    };
    throw new Error("Failed pattern match at Data.Foreign line 75, column 1 - line 75, column 44: " + [ v.constructor.name ]);
};
var fail = function ($112) {
    return Control_Monad_Error_Class.throwError(Control_Monad_Except_Trans.monadErrorExceptT(Data_Identity.monadIdentity))(Data_List_NonEmpty.singleton($112));
};
var parseJSON = function (json) {
    return $foreign.parseJSONImpl(function ($113) {
        return fail(JSONError.create($113));
    }, Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity)), json);
};
var readArray = function (value) {
    if ($foreign.isArray(value)) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))($foreign.unsafeFromForeign(value));
    };
    if (Data_Boolean.otherwise) {
        return fail(new TypeMismatch("array", $foreign.tagOf(value)));
    };
    throw new Error("Failed pattern match at Data.Foreign line 149, column 1 - line 151, column 58: " + [ value.constructor.name ]);
};
var unsafeReadTagged = function (tag) {
    return function (value) {
        if ($foreign.tagOf(value) === tag) {
            return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))($foreign.unsafeFromForeign(value));
        };
        if (Data_Boolean.otherwise) {
            return fail(new TypeMismatch(tag, $foreign.tagOf(value)));
        };
        throw new Error("Failed pattern match at Data.Foreign line 108, column 1 - line 110, column 54: " + [ tag.constructor.name, value.constructor.name ]);
    };
};
var readBoolean = unsafeReadTagged("Boolean");
var readNumber = unsafeReadTagged("Number");
var readInt = function (value) {
    var error = Data_Either.Left.create(Data_List_NonEmpty.singleton(new TypeMismatch("Int", $foreign.tagOf(value))));
    var fromNumber = function ($114) {
        return Data_Maybe.maybe(error)(Control_Applicative.pure(Data_Either.applicativeEither))(Data_Int.fromNumber($114));
    };
    return Control_Monad_Except.mapExcept(Data_Either.either(Data_Function["const"](error))(fromNumber))(readNumber(value));
};
var readString = unsafeReadTagged("String");
var readChar = function (value) {
    var error = Data_Either.Left.create(Data_List_NonEmpty.singleton(new TypeMismatch("Char", $foreign.tagOf(value))));
    var fromString = function ($115) {
        return Data_Maybe.maybe(error)(Control_Applicative.pure(Data_Either.applicativeEither))(Data_String.toChar($115));
    };
    return Control_Monad_Except.mapExcept(Data_Either.either(Data_Function["const"](error))(fromString))(readString(value));
};
var eqForeignError = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof ForeignError && y instanceof ForeignError) {
            return x.value0 === y.value0;
        };
        if (x instanceof TypeMismatch && y instanceof TypeMismatch) {
            return x.value0 === y.value0 && x.value1 === y.value1;
        };
        if (x instanceof ErrorAtIndex && y instanceof ErrorAtIndex) {
            return x.value0 === y.value0 && Data_Eq.eq(eqForeignError)(x.value1)(y.value1);
        };
        if (x instanceof ErrorAtProperty && y instanceof ErrorAtProperty) {
            return x.value0 === y.value0 && Data_Eq.eq(eqForeignError)(x.value1)(y.value1);
        };
        if (x instanceof JSONError && y instanceof JSONError) {
            return x.value0 === y.value0;
        };
        return false;
    };
});
var ordForeignError = new Data_Ord.Ord(function () {
    return eqForeignError;
}, function (x) {
    return function (y) {
        if (x instanceof ForeignError && y instanceof ForeignError) {
            return Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
        };
        if (x instanceof ForeignError) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof ForeignError) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof TypeMismatch && y instanceof TypeMismatch) {
            var $83 = Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
            if ($83 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if ($83 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(Data_Ord.ordString)(x.value1)(y.value1);
        };
        if (x instanceof TypeMismatch) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof TypeMismatch) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof ErrorAtIndex && y instanceof ErrorAtIndex) {
            var $92 = Data_Ord.compare(Data_Ord.ordInt)(x.value0)(y.value0);
            if ($92 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if ($92 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(ordForeignError)(x.value1)(y.value1);
        };
        if (x instanceof ErrorAtIndex) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof ErrorAtIndex) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof ErrorAtProperty && y instanceof ErrorAtProperty) {
            var $101 = Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
            if ($101 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if ($101 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(ordForeignError)(x.value1)(y.value1);
        };
        if (x instanceof ErrorAtProperty) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof ErrorAtProperty) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof JSONError && y instanceof JSONError) {
            return Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
        };
        throw new Error("Failed pattern match: " + [ x.constructor.name, y.constructor.name ]);
    };
});
module.exports = {
    ForeignError: ForeignError, 
    TypeMismatch: TypeMismatch, 
    ErrorAtIndex: ErrorAtIndex, 
    ErrorAtProperty: ErrorAtProperty, 
    JSONError: JSONError, 
    Prop: Prop, 
    fail: fail, 
    parseJSON: parseJSON, 
    readArray: readArray, 
    readBoolean: readBoolean, 
    readChar: readChar, 
    readInt: readInt, 
    readNumber: readNumber, 
    readString: readString, 
    unsafeReadTagged: unsafeReadTagged, 
    eqForeignError: eqForeignError, 
    ordForeignError: ordForeignError, 
    showForeignError: showForeignError, 
    isArray: $foreign.isArray, 
    isNull: $foreign.isNull, 
    isUndefined: $foreign.isUndefined, 
    tagOf: $foreign.tagOf, 
    toForeign: $foreign.toForeign, 
    typeOf: $foreign.typeOf, 
    unsafeFromForeign: $foreign.unsafeFromForeign, 
    writeObject: $foreign.writeObject
};
//# sourceMappingURL=index.js.map
