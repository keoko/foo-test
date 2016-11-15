// Generated by psc version 0.10.2
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Writer = require("../Control.Monad.Writer");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Data_Array = require("../Data.Array");
var Data_Generic = require("../Data.Generic");
var Data_Maybe = require("../Data.Maybe");
var Data_NonEmpty = require("../Data.NonEmpty");
var Data_Profunctor_Strong = require("../Data.Profunctor.Strong");
var Data_Tuple = require("../Data.Tuple");
var CSS_Property = require("../CSS.Property");
var CSS_Selector = require("../CSS.Selector");
var Data_Eq = require("../Data.Eq");
var Data_Ord = require("../Data.Ord");
var Control_Apply = require("../Control.Apply");
var Data_Unit = require("../Data.Unit");
var Data_Ordering = require("../Data.Ordering");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Functor = require("../Data.Functor");
var Data_Function = require("../Data.Function");
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Control_Applicative = require("../Control.Applicative");
var Control_Monad = require("../Control.Monad");
var Data_Semigroup = require("../Data.Semigroup");
var Control_Monad_Writer_Trans = require("../Control.Monad.Writer.Trans");
var Data_Monoid = require("../Data.Monoid");
var Data_Identity = require("../Data.Identity");
var Not = (function () {
    function Not() {

    };
    Not.value = new Not();
    return Not;
})();
var Only = (function () {
    function Only() {

    };
    Only.value = new Only();
    return Only;
})();
var MediaType = function (x) {
    return x;
};
var Feature = (function () {
    function Feature(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Feature.create = function (value0) {
        return function (value1) {
            return new Feature(value0, value1);
        };
    };
    return Feature;
})();
var MediaQuery = (function () {
    function MediaQuery(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    MediaQuery.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new MediaQuery(value0, value1, value2);
            };
        };
    };
    return MediaQuery;
})();
var Self = (function () {
    function Self(value0) {
        this.value0 = value0;
    };
    Self.create = function (value0) {
        return new Self(value0);
    };
    return Self;
})();
var Root = (function () {
    function Root(value0) {
        this.value0 = value0;
    };
    Root.create = function (value0) {
        return new Root(value0);
    };
    return Root;
})();
var Pop = (function () {
    function Pop(value0) {
        this.value0 = value0;
    };
    Pop.create = function (value0) {
        return new Pop(value0);
    };
    return Pop;
})();
var Child = (function () {
    function Child(value0) {
        this.value0 = value0;
    };
    Child.create = function (value0) {
        return new Child(value0);
    };
    return Child;
})();
var Sub = (function () {
    function Sub(value0) {
        this.value0 = value0;
    };
    Sub.create = function (value0) {
        return new Sub(value0);
    };
    return Sub;
})();
var Property = (function () {
    function Property(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Property.create = function (value0) {
        return function (value1) {
            return new Property(value0, value1);
        };
    };
    return Property;
})();
var Nested = (function () {
    function Nested(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Nested.create = function (value0) {
        return function (value1) {
            return new Nested(value0, value1);
        };
    };
    return Nested;
})();
var Query = (function () {
    function Query(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Query.create = function (value0) {
        return function (value1) {
            return new Query(value0, value1);
        };
    };
    return Query;
})();
var Face = (function () {
    function Face(value0) {
        this.value0 = value0;
    };
    Face.create = function (value0) {
        return new Face(value0);
    };
    return Face;
})();
var Keyframe = (function () {
    function Keyframe(value0) {
        this.value0 = value0;
    };
    Keyframe.create = function (value0) {
        return new Keyframe(value0);
    };
    return Keyframe;
})();
var Import = (function () {
    function Import(value0) {
        this.value0 = value0;
    };
    Import.create = function (value0) {
        return new Import(value0);
    };
    return Import;
})();
var Keyframes = (function () {
    function Keyframes(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Keyframes.create = function (value0) {
        return function (value1) {
            return new Keyframes(value0, value1);
        };
    };
    return Keyframes;
})();
var S = function (x) {
    return x;
};
var runS = function (v) {
    return Control_Monad_Writer.execWriter(v);
};
var rule = function ($424) {
    return S(Control_Monad_Writer_Class.tell(Control_Monad_Writer_Trans.monadTellWriterT(Data_Monoid.monoidArray)(Data_Identity.monadIdentity))(Data_Array.singleton($424)));
};
var select = function (sel) {
    return function (rs) {
        return rule(new Nested(new Sub(sel), runS(rs)));
    };
};
var query = function (ty) {
    return function (fs) {
        return function ($425) {
            return rule(Query.create(new MediaQuery(Data_Maybe.Nothing.value, ty, fs))(runS($425)));
        };
    };
};
var keyframes = function (n) {
    return function (xs) {
        return rule(new Keyframe(new Keyframes(n, Data_Functor.map(Data_NonEmpty.functorNonEmpty(Data_Functor.functorArray))(Data_Profunctor_Strong.second(Data_Profunctor_Strong.strongFn)(runS))(xs))));
    };
};
var keyframesFromTo = function (n) {
    return function (a) {
        return function (b) {
            return keyframes(n)(new Data_NonEmpty.NonEmpty(new Data_Tuple.Tuple(0.0, a), [ new Data_Tuple.Tuple(100.0, b) ]));
        };
    };
};
var key = function (dictVal) {
    return function (k) {
        return function (v) {
            return rule(new Property(CSS_Property.cast(k), CSS_Property.value(dictVal)(v)));
        };
    };
};
var prefixed = function (dictVal) {
    return function (xs) {
        return key(dictVal)(xs);
    };
};
var importUrl = function ($426) {
    return rule(Import.create($426));
};
var genericNotOrOnly = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.Not" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Not.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.Only" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Only.value);
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Stylesheet.NotOrOnly", [ {
        sigConstructor: "CSS.Stylesheet.Not", 
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.Stylesheet.Only", 
        sigValues: [  ]
    } ]);
}, function (v) {
    if (v instanceof Not) {
        return new Data_Generic.SProd("CSS.Stylesheet.Not", [  ]);
    };
    if (v instanceof Only) {
        return new Data_Generic.SProd("CSS.Stylesheet.Only", [  ]);
    };
    throw new Error("Failed pattern match at CSS.Stylesheet line 28, column 1 - line 28, column 54: " + [ v.constructor.name ]);
});
var genericMediaType = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.MediaType" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(MediaType))(Data_Generic.fromSpine(CSS_Property.genericValue)(v.value1[0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Stylesheet.MediaType", [ {
        sigConstructor: "CSS.Stylesheet.MediaType", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Stylesheet.MediaType", [ function ($dollarq) {
        return Data_Generic.toSpine(CSS_Property.genericValue)(v);
    } ]);
});
var genericFeature = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.Feature" && v.value1.length === 2)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Feature.create))(Data_Generic.fromSpine(Data_Generic.genericString)(v.value1[0](Data_Unit.unit))))(Data_Generic.fromSpine(Data_Generic.genericMaybe(CSS_Property.genericValue))(v.value1[1](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Stylesheet.Feature", [ {
        sigConstructor: "CSS.Stylesheet.Feature", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
        }, function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericMaybe(CSS_Property.genericValue))(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Stylesheet.Feature", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericString)(v.value0);
    }, function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericMaybe(CSS_Property.genericValue))(v.value1);
    } ]);
});
var genericMediaQuery = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.MediaQuery" && v.value1.length === 3)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(MediaQuery.create))(Data_Generic.fromSpine(Data_Generic.genericMaybe(genericNotOrOnly))(v.value1[0](Data_Unit.unit))))(Data_Generic.fromSpine(genericMediaType)(v.value1[1](Data_Unit.unit))))(Data_Generic.fromSpine(Data_Generic.genericNonEmpty(Data_Generic.genericArray(genericFeature))(genericFeature))(v.value1[2](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Stylesheet.MediaQuery", [ {
        sigConstructor: "CSS.Stylesheet.MediaQuery", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericMaybe(genericNotOrOnly))(Data_Generic.anyProxy);
        }, function ($dollarq1) {
            return Data_Generic.toSignature(genericMediaType)(Data_Generic.anyProxy);
        }, function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericNonEmpty(Data_Generic.genericArray(genericFeature))(genericFeature))(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Stylesheet.MediaQuery", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericMaybe(genericNotOrOnly))(v.value0);
    }, function ($dollarq) {
        return Data_Generic.toSpine(genericMediaType)(v.value1);
    }, function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericNonEmpty(Data_Generic.genericArray(genericFeature))(genericFeature))(v.value2);
    } ]);
});
var genericApp = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.Self" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Self.create))(Data_Generic.fromSpine(CSS_Selector.genericRefinement)(v.value1[0](Data_Unit.unit)));
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.Root" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Root.create))(Data_Generic.fromSpine(CSS_Selector.genericSelector)(v.value1[0](Data_Unit.unit)));
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.Pop" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Pop.create))(Data_Generic.fromSpine(Data_Generic.genericInt)(v.value1[0](Data_Unit.unit)));
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.Child" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Child.create))(Data_Generic.fromSpine(CSS_Selector.genericSelector)(v.value1[0](Data_Unit.unit)));
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.Sub" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Sub.create))(Data_Generic.fromSpine(CSS_Selector.genericSelector)(v.value1[0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Stylesheet.App", [ {
        sigConstructor: "CSS.Stylesheet.Self", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Selector.genericRefinement)(Data_Generic.anyProxy);
        } ]
    }, {
        sigConstructor: "CSS.Stylesheet.Root", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Selector.genericSelector)(Data_Generic.anyProxy);
        } ]
    }, {
        sigConstructor: "CSS.Stylesheet.Pop", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
        } ]
    }, {
        sigConstructor: "CSS.Stylesheet.Child", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Selector.genericSelector)(Data_Generic.anyProxy);
        } ]
    }, {
        sigConstructor: "CSS.Stylesheet.Sub", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Selector.genericSelector)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    if (v instanceof Self) {
        return new Data_Generic.SProd("CSS.Stylesheet.Self", [ function ($dollarq) {
            return Data_Generic.toSpine(CSS_Selector.genericRefinement)(v.value0);
        } ]);
    };
    if (v instanceof Root) {
        return new Data_Generic.SProd("CSS.Stylesheet.Root", [ function ($dollarq) {
            return Data_Generic.toSpine(CSS_Selector.genericSelector)(v.value0);
        } ]);
    };
    if (v instanceof Pop) {
        return new Data_Generic.SProd("CSS.Stylesheet.Pop", [ function ($dollarq) {
            return Data_Generic.toSpine(Data_Generic.genericInt)(v.value0);
        } ]);
    };
    if (v instanceof Child) {
        return new Data_Generic.SProd("CSS.Stylesheet.Child", [ function ($dollarq) {
            return Data_Generic.toSpine(CSS_Selector.genericSelector)(v.value0);
        } ]);
    };
    if (v instanceof Sub) {
        return new Data_Generic.SProd("CSS.Stylesheet.Sub", [ function ($dollarq) {
            return Data_Generic.toSpine(CSS_Selector.genericSelector)(v.value0);
        } ]);
    };
    throw new Error("Failed pattern match at CSS.Stylesheet line 51, column 1 - line 51, column 42: " + [ v.constructor.name ]);
});
var genericRule = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.Property" && v.value1.length === 2)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Property.create))(Data_Generic.fromSpine(CSS_Property.genericKey(Data_Generic.genericUnit))(v.value1[0](Data_Unit.unit))))(Data_Generic.fromSpine(CSS_Property.genericValue)(v.value1[1](Data_Unit.unit)));
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.Nested" && v.value1.length === 2)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Nested.create))(Data_Generic.fromSpine(genericApp)(v.value1[0](Data_Unit.unit))))(Data_Generic.fromSpine(Data_Generic.genericArray(genericRule))(v.value1[1](Data_Unit.unit)));
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.Query" && v.value1.length === 2)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Query.create))(Data_Generic.fromSpine(genericMediaQuery)(v.value1[0](Data_Unit.unit))))(Data_Generic.fromSpine(Data_Generic.genericArray(genericRule))(v.value1[1](Data_Unit.unit)));
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.Face" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Face.create))(Data_Generic.fromSpine(Data_Generic.genericArray(genericRule))(v.value1[0](Data_Unit.unit)));
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.Keyframe" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Keyframe.create))(Data_Generic.fromSpine(genericKeyframes)(v.value1[0](Data_Unit.unit)));
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.Import" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Import.create))(Data_Generic.fromSpine(Data_Generic.genericString)(v.value1[0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Stylesheet.Rule", [ {
        sigConstructor: "CSS.Stylesheet.Property", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Property.genericKey(Data_Generic.genericUnit))(Data_Generic.anyProxy);
        }, function ($dollarq1) {
            return Data_Generic.toSignature(CSS_Property.genericValue)(Data_Generic.anyProxy);
        } ]
    }, {
        sigConstructor: "CSS.Stylesheet.Nested", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(genericApp)(Data_Generic.anyProxy);
        }, function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericArray(genericRule))(Data_Generic.anyProxy);
        } ]
    }, {
        sigConstructor: "CSS.Stylesheet.Query", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(genericMediaQuery)(Data_Generic.anyProxy);
        }, function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericArray(genericRule))(Data_Generic.anyProxy);
        } ]
    }, {
        sigConstructor: "CSS.Stylesheet.Face", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericArray(genericRule))(Data_Generic.anyProxy);
        } ]
    }, {
        sigConstructor: "CSS.Stylesheet.Keyframe", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(genericKeyframes)(Data_Generic.anyProxy);
        } ]
    }, {
        sigConstructor: "CSS.Stylesheet.Import", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    if (v instanceof Property) {
        return new Data_Generic.SProd("CSS.Stylesheet.Property", [ function ($dollarq) {
            return Data_Generic.toSpine(CSS_Property.genericKey(Data_Generic.genericUnit))(v.value0);
        }, function ($dollarq) {
            return Data_Generic.toSpine(CSS_Property.genericValue)(v.value1);
        } ]);
    };
    if (v instanceof Nested) {
        return new Data_Generic.SProd("CSS.Stylesheet.Nested", [ function ($dollarq) {
            return Data_Generic.toSpine(genericApp)(v.value0);
        }, function ($dollarq) {
            return Data_Generic.toSpine(Data_Generic.genericArray(genericRule))(v.value1);
        } ]);
    };
    if (v instanceof Query) {
        return new Data_Generic.SProd("CSS.Stylesheet.Query", [ function ($dollarq) {
            return Data_Generic.toSpine(genericMediaQuery)(v.value0);
        }, function ($dollarq) {
            return Data_Generic.toSpine(Data_Generic.genericArray(genericRule))(v.value1);
        } ]);
    };
    if (v instanceof Face) {
        return new Data_Generic.SProd("CSS.Stylesheet.Face", [ function ($dollarq) {
            return Data_Generic.toSpine(Data_Generic.genericArray(genericRule))(v.value0);
        } ]);
    };
    if (v instanceof Keyframe) {
        return new Data_Generic.SProd("CSS.Stylesheet.Keyframe", [ function ($dollarq) {
            return Data_Generic.toSpine(genericKeyframes)(v.value0);
        } ]);
    };
    if (v instanceof Import) {
        return new Data_Generic.SProd("CSS.Stylesheet.Import", [ function ($dollarq) {
            return Data_Generic.toSpine(Data_Generic.genericString)(v.value0);
        } ]);
    };
    throw new Error("Failed pattern match: " + [ v.constructor.name ]);
});
var genericKeyframes = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Stylesheet.Keyframes" && v.value1.length === 2)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Keyframes.create))(Data_Generic.fromSpine(Data_Generic.genericString)(v.value1[0](Data_Unit.unit))))(Data_Generic.fromSpine(Data_Generic.genericNonEmpty(Data_Generic.genericArray(Data_Generic.genericTuple(Data_Generic.genericNumber)(Data_Generic.genericArray(genericRule))))(Data_Generic.genericTuple(Data_Generic.genericNumber)(Data_Generic.genericArray(genericRule))))(v.value1[1](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Stylesheet.Keyframes", [ {
        sigConstructor: "CSS.Stylesheet.Keyframes", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
        }, function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericNonEmpty(Data_Generic.genericArray(Data_Generic.genericTuple(Data_Generic.genericNumber)(Data_Generic.genericArray(genericRule))))(Data_Generic.genericTuple(Data_Generic.genericNumber)(Data_Generic.genericArray(genericRule))))(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Stylesheet.Keyframes", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericString)(v.value0);
    }, function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericNonEmpty(Data_Generic.genericArray(Data_Generic.genericTuple(Data_Generic.genericNumber)(Data_Generic.genericArray(genericRule))))(Data_Generic.genericTuple(Data_Generic.genericNumber)(Data_Generic.genericArray(genericRule))))(v.value1);
    } ]);
});
var functorStyleM = new Data_Functor.Functor(function (f) {
    return function (v) {
        return S(Data_Functor.map(Control_Monad_Writer_Trans.functorWriterT(Data_Identity.functorIdentity))(f)(v));
    };
});
var fontFace = function ($427) {
    return rule(Face.create(runS($427)));
};
var eqNotOrOnly = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof Not && y instanceof Not) {
            return true;
        };
        if (x instanceof Only && y instanceof Only) {
            return true;
        };
        return false;
    };
});
var ordNotOrOnly = new Data_Ord.Ord(function () {
    return eqNotOrOnly;
}, function (x) {
    return function (y) {
        if (x instanceof Not && y instanceof Not) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Not) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Not) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Only && y instanceof Only) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at CSS.Stylesheet line 27, column 1 - line 27, column 45: " + [ x.constructor.name, y.constructor.name ]);
    };
});
var eqMediaType = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(CSS_Property.eqValue)(x)(y);
    };
});
var ordMediaType = new Data_Ord.Ord(function () {
    return eqMediaType;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(CSS_Property.ordValue)(x)(y);
    };
});
var eqFeature = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x.value0 === y.value0 && Data_Eq.eq(Data_Maybe.eqMaybe(CSS_Property.eqValue))(x.value1)(y.value1);
    };
});
var eqMediaQuery = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(Data_Maybe.eqMaybe(eqNotOrOnly))(x.value0)(y.value0) && Data_Eq.eq(eqMediaType)(x.value1)(y.value1) && Data_Eq.eq(Data_NonEmpty.eqNonEmpty(eqFeature)(Data_Eq.eqArray(eqFeature)))(x.value2)(y.value2);
    };
});
var ordFeature = new Data_Ord.Ord(function () {
    return eqFeature;
}, function (x) {
    return function (y) {
        var $300 = Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
        if ($300 instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if ($300 instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        return Data_Ord.compare(Data_Maybe.ordMaybe(CSS_Property.ordValue))(x.value1)(y.value1);
    };
});
var ordMediaQuery = new Data_Ord.Ord(function () {
    return eqMediaQuery;
}, function (x) {
    return function (y) {
        var $307 = Data_Ord.compare(Data_Maybe.ordMaybe(ordNotOrOnly))(x.value0)(y.value0);
        if ($307 instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if ($307 instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        var $308 = Data_Ord.compare(ordMediaType)(x.value1)(y.value1);
        if ($308 instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if ($308 instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        return Data_Ord.compare(Data_NonEmpty.ordNonEmpty(ordFeature)(Data_Ord.ordArray(ordFeature)))(x.value2)(y.value2);
    };
});
var eqApp = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof Self && y instanceof Self) {
            return Data_Eq.eq(CSS_Selector.eqRefinement)(x.value0)(y.value0);
        };
        if (x instanceof Root && y instanceof Root) {
            return Data_Eq.eq(CSS_Selector.eqSelector)(x.value0)(y.value0);
        };
        if (x instanceof Pop && y instanceof Pop) {
            return x.value0 === y.value0;
        };
        if (x instanceof Child && y instanceof Child) {
            return Data_Eq.eq(CSS_Selector.eqSelector)(x.value0)(y.value0);
        };
        if (x instanceof Sub && y instanceof Sub) {
            return Data_Eq.eq(CSS_Selector.eqSelector)(x.value0)(y.value0);
        };
        return false;
    };
});
var eqRule = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof Property && y instanceof Property) {
            return Data_Eq.eq(CSS_Property.eqKey(Data_Eq.eqUnit))(x.value0)(y.value0) && Data_Eq.eq(CSS_Property.eqValue)(x.value1)(y.value1);
        };
        if (x instanceof Nested && y instanceof Nested) {
            return Data_Eq.eq(eqApp)(x.value0)(y.value0) && Data_Eq.eq(Data_Eq.eqArray(eqRule))(x.value1)(y.value1);
        };
        if (x instanceof Query && y instanceof Query) {
            return Data_Eq.eq(eqMediaQuery)(x.value0)(y.value0) && Data_Eq.eq(Data_Eq.eqArray(eqRule))(x.value1)(y.value1);
        };
        if (x instanceof Face && y instanceof Face) {
            return Data_Eq.eq(Data_Eq.eqArray(eqRule))(x.value0)(y.value0);
        };
        if (x instanceof Keyframe && y instanceof Keyframe) {
            return Data_Eq.eq(eqKeyframes)(x.value0)(y.value0);
        };
        if (x instanceof Import && y instanceof Import) {
            return x.value0 === y.value0;
        };
        return false;
    };
});
var eqKeyframes = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x.value0 === y.value0 && Data_Eq.eq(Data_NonEmpty.eqNonEmpty(Data_Tuple.eqTuple(Data_Eq.eqNumber)(Data_Eq.eqArray(eqRule)))(Data_Eq.eqArray(Data_Tuple.eqTuple(Data_Eq.eqNumber)(Data_Eq.eqArray(eqRule)))))(x.value1)(y.value1);
    };
});
var ordApp = new Data_Ord.Ord(function () {
    return eqApp;
}, function (x) {
    return function (y) {
        if (x instanceof Self && y instanceof Self) {
            return Data_Ord.compare(CSS_Selector.ordRefinement)(x.value0)(y.value0);
        };
        if (x instanceof Self) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Self) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Root && y instanceof Root) {
            return Data_Ord.compare(CSS_Selector.ordSelector)(x.value0)(y.value0);
        };
        if (x instanceof Root) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Root) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Pop && y instanceof Pop) {
            return Data_Ord.compare(Data_Ord.ordInt)(x.value0)(y.value0);
        };
        if (x instanceof Pop) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Pop) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Child && y instanceof Child) {
            return Data_Ord.compare(CSS_Selector.ordSelector)(x.value0)(y.value0);
        };
        if (x instanceof Child) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Child) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Sub && y instanceof Sub) {
            return Data_Ord.compare(CSS_Selector.ordSelector)(x.value0)(y.value0);
        };
        throw new Error("Failed pattern match at CSS.Stylesheet line 50, column 1 - line 50, column 34: " + [ x.constructor.name, y.constructor.name ]);
    };
});
var ordRule = new Data_Ord.Ord(function () {
    return eqRule;
}, function (x) {
    return function (y) {
        if (x instanceof Property && y instanceof Property) {
            var $375 = Data_Ord.compare(CSS_Property.ordKey(Data_Ord.ordUnit))(x.value0)(y.value0);
            if ($375 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if ($375 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(CSS_Property.ordValue)(x.value1)(y.value1);
        };
        if (x instanceof Property) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Property) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Nested && y instanceof Nested) {
            var $384 = Data_Ord.compare(ordApp)(x.value0)(y.value0);
            if ($384 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if ($384 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(Data_Ord.ordArray(ordRule))(x.value1)(y.value1);
        };
        if (x instanceof Nested) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Nested) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Query && y instanceof Query) {
            var $393 = Data_Ord.compare(ordMediaQuery)(x.value0)(y.value0);
            if ($393 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if ($393 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(Data_Ord.ordArray(ordRule))(x.value1)(y.value1);
        };
        if (x instanceof Query) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Query) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Face && y instanceof Face) {
            return Data_Ord.compare(Data_Ord.ordArray(ordRule))(x.value0)(y.value0);
        };
        if (x instanceof Face) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Face) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Keyframe && y instanceof Keyframe) {
            return Data_Ord.compare(ordKeyframes)(x.value0)(y.value0);
        };
        if (x instanceof Keyframe) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Keyframe) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Import && y instanceof Import) {
            return Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
        };
        throw new Error("Failed pattern match: " + [ x.constructor.name, y.constructor.name ]);
    };
});
var ordKeyframes = new Data_Ord.Ord(function () {
    return eqKeyframes;
}, function (x) {
    return function (y) {
        var $414 = Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
        if ($414 instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if ($414 instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        return Data_Ord.compare(Data_NonEmpty.ordNonEmpty(Data_Tuple.ordTuple(Data_Ord.ordNumber)(Data_Ord.ordArray(ordRule)))(Data_Ord.ordArray(Data_Tuple.ordTuple(Data_Ord.ordNumber)(Data_Ord.ordArray(ordRule)))))(x.value1)(y.value1);
    };
});
var applyStyleM = new Control_Apply.Apply(function () {
    return functorStyleM;
}, function (v) {
    return function (v1) {
        return S(Control_Apply.apply(Control_Monad_Writer_Trans.applyWriterT(Data_Semigroup.semigroupArray)(Data_Identity.applyIdentity))(v)(v1));
    };
});
var bindStyleM = new Control_Bind.Bind(function () {
    return applyStyleM;
}, function (v) {
    return function (f) {
        return S(Control_Bind.bind(Control_Monad_Writer_Trans.bindWriterT(Data_Semigroup.semigroupArray)(Data_Identity.bindIdentity))(v)(function ($428) {
            return (function (v1) {
                return v1;
            })(f($428));
        }));
    };
});
var semigroupCSS = new Data_Semigroup.Semigroup(Control_Apply.applySecond(applyStyleM));
var applicativeStyleM = new Control_Applicative.Applicative(function () {
    return applyStyleM;
}, function ($429) {
    return S(Control_Applicative.pure(Control_Monad_Writer_Trans.applicativeWriterT(Data_Monoid.monoidArray)(Data_Identity.applicativeIdentity))($429));
});
var monadStyleM = new Control_Monad.Monad(function () {
    return applicativeStyleM;
}, function () {
    return bindStyleM;
});
module.exports = {
    Self: Self, 
    Root: Root, 
    Pop: Pop, 
    Child: Child, 
    Sub: Sub, 
    Feature: Feature, 
    Keyframes: Keyframes, 
    MediaQuery: MediaQuery, 
    MediaType: MediaType, 
    Not: Not, 
    Only: Only, 
    Property: Property, 
    Nested: Nested, 
    Query: Query, 
    Face: Face, 
    Keyframe: Keyframe, 
    Import: Import, 
    S: S, 
    fontFace: fontFace, 
    importUrl: importUrl, 
    key: key, 
    keyframes: keyframes, 
    keyframesFromTo: keyframesFromTo, 
    prefixed: prefixed, 
    query: query, 
    rule: rule, 
    runS: runS, 
    select: select, 
    eqMediaType: eqMediaType, 
    ordMediaType: ordMediaType, 
    genericMediaType: genericMediaType, 
    eqNotOrOnly: eqNotOrOnly, 
    ordNotOrOnly: ordNotOrOnly, 
    genericNotOrOnly: genericNotOrOnly, 
    eqMediaQuery: eqMediaQuery, 
    ordMediaQuery: ordMediaQuery, 
    genericMediaQuery: genericMediaQuery, 
    eqFeature: eqFeature, 
    ordFeature: ordFeature, 
    genericFeature: genericFeature, 
    eqApp: eqApp, 
    ordApp: ordApp, 
    genericApp: genericApp, 
    eqKeyframes: eqKeyframes, 
    ordKeyframes: ordKeyframes, 
    genericKeyframes: genericKeyframes, 
    eqRule: eqRule, 
    ordRule: ordRule, 
    genericRule: genericRule, 
    functorStyleM: functorStyleM, 
    applyStyleM: applyStyleM, 
    bindStyleM: bindStyleM, 
    applicativeStyleM: applicativeStyleM, 
    monadStyleM: monadStyleM, 
    semigroupCSS: semigroupCSS
};
//# sourceMappingURL=index.js.map
