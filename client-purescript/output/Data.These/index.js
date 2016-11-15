// Generated by psc version 0.10.2
"use strict";
var Prelude = require("../Prelude");
var Control_Extend = require("../Control.Extend");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Bitraversable = require("../Data.Bitraversable");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Generic = require("../Data.Generic");
var Data_Maybe = require("../Data.Maybe");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Eq = require("../Data.Eq");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Control_Apply = require("../Control.Apply");
var Data_Unit = require("../Data.Unit");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Functor = require("../Data.Functor");
var Data_Foldable = require("../Data.Foldable");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Function = require("../Data.Function");
var Control_Applicative = require("../Control.Applicative");
var Data_Bifoldable = require("../Data.Bifoldable");
var Control_Category = require("../Control.Category");
var Control_Bind = require("../Control.Bind");
var Control_Monad = require("../Control.Monad");
var Data_Show = require("../Data.Show");
var This = (function () {
    function This(value0) {
        this.value0 = value0;
    };
    This.create = function (value0) {
        return new This(value0);
    };
    return This;
})();
var That = (function () {
    function That(value0) {
        this.value0 = value0;
    };
    That.create = function (value0) {
        return new That(value0);
    };
    return That;
})();
var Both = (function () {
    function Both(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Both.create = function (value0) {
        return function (value1) {
            return new Both(value0, value1);
        };
    };
    return Both;
})();
var thisOrBoth = function (a) {
    return function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return new This(a);
        };
        if (v instanceof Data_Maybe.Just) {
            return new Both(a, v.value0);
        };
        throw new Error("Failed pattern match at Data.These line 110, column 1 - line 110, column 30: " + [ a.constructor.name, v.constructor.name ]);
    };
};
var theseRight = function (v) {
    if (v instanceof Both) {
        return new Data_Maybe.Just(v.value1);
    };
    if (v instanceof That) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
};
var theseLeft = function (v) {
    if (v instanceof Both) {
        return new Data_Maybe.Just(v.value0);
    };
    if (v instanceof This) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
};
var these = function (v) {
    return function (v1) {
        return function (v2) {
            return function (v3) {
                if (v3 instanceof This) {
                    return v(v3.value0);
                };
                if (v3 instanceof That) {
                    return v1(v3.value0);
                };
                if (v3 instanceof Both) {
                    return v2(v3.value0)(v3.value1);
                };
                throw new Error("Failed pattern match at Data.These line 105, column 1 - line 105, column 27: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name, v3.constructor.name ]);
            };
        };
    };
};
var thatOrBoth = function (b) {
    return function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return new That(b);
        };
        if (v instanceof Data_Maybe.Just) {
            return new Both(v.value0, b);
        };
        throw new Error("Failed pattern match at Data.These line 114, column 1 - line 114, column 30: " + [ b.constructor.name, v.constructor.name ]);
    };
};
var showThese = function (dictShow) {
    return function (dictShow1) {
        return new Data_Show.Show(function (v) {
            if (v instanceof This) {
                return "(This " + (Data_Show.show(dictShow)(v.value0) + ")");
            };
            if (v instanceof That) {
                return "(That " + (Data_Show.show(dictShow1)(v.value0) + ")");
            };
            if (v instanceof Both) {
                return "(Both " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(dictShow1)(v.value1) + ")")));
            };
            throw new Error("Failed pattern match at Data.These line 100, column 3 - line 101, column 3: " + [ v.constructor.name ]);
        });
    };
};
var semigroupThese = function (dictSemigroup) {
    return function (dictSemigroup1) {
        return new Data_Semigroup.Semigroup(function (v) {
            return function (v1) {
                if (v instanceof This && v1 instanceof This) {
                    return new This(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0));
                };
                if (v instanceof This && v1 instanceof That) {
                    return new Both(v.value0, v1.value0);
                };
                if (v instanceof This && v1 instanceof Both) {
                    return new Both(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), v1.value1);
                };
                if (v instanceof That && v1 instanceof This) {
                    return new Both(v1.value0, v.value0);
                };
                if (v instanceof That && v1 instanceof That) {
                    return new That(Data_Semigroup.append(dictSemigroup1)(v.value0)(v1.value0));
                };
                if (v instanceof That && v1 instanceof Both) {
                    return new Both(v1.value0, Data_Semigroup.append(dictSemigroup1)(v.value0)(v1.value1));
                };
                if (v instanceof Both && v1 instanceof This) {
                    return new Both(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), v.value1);
                };
                if (v instanceof Both && v1 instanceof That) {
                    return new Both(v.value0, Data_Semigroup.append(dictSemigroup1)(v.value1)(v1.value0));
                };
                if (v instanceof Both && v1 instanceof Both) {
                    return new Both(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), Data_Semigroup.append(dictSemigroup1)(v.value1)(v1.value1));
                };
                throw new Error("Failed pattern match at Data.These line 25, column 3 - line 25, column 43: " + [ v.constructor.name, v1.constructor.name ]);
            };
        });
    };
};
var genericThese = function (dictGeneric) {
    return function (dictGeneric1) {
        return new Data_Generic.Generic(function (v) {
            if (v instanceof Data_Generic.SProd && (v.value0 === "Data.These.This" && v.value1.length === 1)) {
                return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(This.create))(Data_Generic.fromSpine(dictGeneric)(v.value1[0](Data_Unit.unit)));
            };
            if (v instanceof Data_Generic.SProd && (v.value0 === "Data.These.That" && v.value1.length === 1)) {
                return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(That.create))(Data_Generic.fromSpine(dictGeneric1)(v.value1[0](Data_Unit.unit)));
            };
            if (v instanceof Data_Generic.SProd && (v.value0 === "Data.These.Both" && v.value1.length === 2)) {
                return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Both.create))(Data_Generic.fromSpine(dictGeneric)(v.value1[0](Data_Unit.unit))))(Data_Generic.fromSpine(dictGeneric1)(v.value1[1](Data_Unit.unit)));
            };
            return Data_Maybe.Nothing.value;
        }, function ($dollarq) {
            return new Data_Generic.SigProd("Data.These.These", [ {
                sigConstructor: "Data.These.This", 
                sigValues: [ function ($dollarq1) {
                    return Data_Generic.toSignature(dictGeneric)(Data_Generic.anyProxy);
                } ]
            }, {
                sigConstructor: "Data.These.That", 
                sigValues: [ function ($dollarq1) {
                    return Data_Generic.toSignature(dictGeneric1)(Data_Generic.anyProxy);
                } ]
            }, {
                sigConstructor: "Data.These.Both", 
                sigValues: [ function ($dollarq1) {
                    return Data_Generic.toSignature(dictGeneric)(Data_Generic.anyProxy);
                }, function ($dollarq1) {
                    return Data_Generic.toSignature(dictGeneric1)(Data_Generic.anyProxy);
                } ]
            } ]);
        }, function (v) {
            if (v instanceof This) {
                return new Data_Generic.SProd("Data.These.This", [ function ($dollarq) {
                    return Data_Generic.toSpine(dictGeneric)(v.value0);
                } ]);
            };
            if (v instanceof That) {
                return new Data_Generic.SProd("Data.These.That", [ function ($dollarq) {
                    return Data_Generic.toSpine(dictGeneric1)(v.value0);
                } ]);
            };
            if (v instanceof Both) {
                return new Data_Generic.SProd("Data.These.Both", [ function ($dollarq) {
                    return Data_Generic.toSpine(dictGeneric)(v.value0);
                }, function ($dollarq) {
                    return Data_Generic.toSpine(dictGeneric1)(v.value1);
                } ]);
            };
            throw new Error("Failed pattern match at Data.These line 22, column 1 - line 22, column 78: " + [ v.constructor.name ]);
        });
    };
};
var functorThese = new Data_Functor.Functor(function (v) {
    return function (v1) {
        if (v1 instanceof Both) {
            return new Both(v1.value0, v(v1.value1));
        };
        if (v1 instanceof That) {
            return new That(v(v1.value0));
        };
        if (v1 instanceof This) {
            return new This(v1.value0);
        };
        throw new Error("Failed pattern match at Data.These line 36, column 3 - line 36, column 34: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var invariantThese = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorThese));
var fromThese = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof This) {
                return new Data_Tuple.Tuple(v2.value0, v1);
            };
            if (v2 instanceof That) {
                return new Data_Tuple.Tuple(v, v2.value0);
            };
            if (v2 instanceof Both) {
                return new Data_Tuple.Tuple(v2.value0, v2.value1);
            };
            throw new Error("Failed pattern match at Data.These line 118, column 1 - line 118, column 37: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var foldableThese = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function ($251) {
            return Data_Foldable.foldMap(Data_Foldable.foldableMaybe)(dictMonoid)(f)(theseRight($251));
        };
    };
}, function (f) {
    return function (z) {
        return function ($252) {
            return Data_Foldable.foldl(Data_Foldable.foldableMaybe)(f)(z)(theseRight($252));
        };
    };
}, function (f) {
    return function (z) {
        return function ($253) {
            return Data_Foldable.foldr(Data_Foldable.foldableMaybe)(f)(z)(theseRight($253));
        };
    };
});
var traversableThese = new Data_Traversable.Traversable(function () {
    return foldableThese;
}, function () {
    return functorThese;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof This) {
            return Control_Applicative.pure(dictApplicative)(new This(v.value0));
        };
        if (v instanceof That) {
            return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(That.create)(v.value0);
        };
        if (v instanceof Both) {
            return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Both.create(v.value0))(v.value1);
        };
        throw new Error("Failed pattern match at Data.These line 52, column 3 - line 52, column 36: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof This) {
                return Control_Applicative.pure(dictApplicative)(new This(v1.value0));
            };
            if (v1 instanceof That) {
                return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(That.create)(v(v1.value0));
            };
            if (v1 instanceof Both) {
                return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Both.create(v1.value0))(v(v1.value1));
            };
            throw new Error("Failed pattern match at Data.These line 49, column 3 - line 49, column 38: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
});
var extendEither = new Control_Extend.Extend(function () {
    return functorThese;
}, function (v) {
    return function (v1) {
        if (v1 instanceof This) {
            return new This(v1.value0);
        };
        return Data_Functor.map(functorThese)(Data_Function["const"](v(v1)))(v1);
    };
});
var eqThese = function (dictEq) {
    return function (dictEq1) {
        return new Data_Eq.Eq(function (x) {
            return function (y) {
                if (x instanceof This && y instanceof This) {
                    return Data_Eq.eq(dictEq)(x.value0)(y.value0);
                };
                if (x instanceof That && y instanceof That) {
                    return Data_Eq.eq(dictEq1)(x.value0)(y.value0);
                };
                if (x instanceof Both && y instanceof Both) {
                    return Data_Eq.eq(dictEq)(x.value0)(y.value0) && Data_Eq.eq(dictEq1)(x.value1)(y.value1);
                };
                return false;
            };
        });
    };
};
var ordThese = function (dictOrd) {
    return function (dictOrd1) {
        return new Data_Ord.Ord(function () {
            return eqThese(dictOrd["__superclass_Data.Eq.Eq_0"]())(dictOrd1["__superclass_Data.Eq.Eq_0"]());
        }, function (x) {
            return function (y) {
                if (x instanceof This && y instanceof This) {
                    return Data_Ord.compare(dictOrd)(x.value0)(y.value0);
                };
                if (x instanceof This) {
                    return Data_Ordering.LT.value;
                };
                if (y instanceof This) {
                    return Data_Ordering.GT.value;
                };
                if (x instanceof That && y instanceof That) {
                    return Data_Ord.compare(dictOrd1)(x.value0)(y.value0);
                };
                if (x instanceof That) {
                    return Data_Ordering.LT.value;
                };
                if (y instanceof That) {
                    return Data_Ordering.GT.value;
                };
                if (x instanceof Both && y instanceof Both) {
                    var $201 = Data_Ord.compare(dictOrd)(x.value0)(y.value0);
                    if ($201 instanceof Data_Ordering.LT) {
                        return Data_Ordering.LT.value;
                    };
                    if ($201 instanceof Data_Ordering.GT) {
                        return Data_Ordering.GT.value;
                    };
                    return Data_Ord.compare(dictOrd1)(x.value1)(y.value1);
                };
                throw new Error("Failed pattern match at Data.These line 21, column 1 - line 21, column 62: " + [ x.constructor.name, y.constructor.name ]);
            };
        });
    };
};
var bifunctorThese = new Data_Bifunctor.Bifunctor(function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof This) {
                return new This(v(v2.value0));
            };
            if (v2 instanceof That) {
                return new That(v1(v2.value0));
            };
            if (v2 instanceof Both) {
                return new Both(v(v2.value0), v1(v2.value1));
            };
            throw new Error("Failed pattern match at Data.These line 57, column 3 - line 57, column 34: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
});
var bifoldableThese = new Data_Bifoldable.Bifoldable(function (dictMonoid) {
    return function (f) {
        return function (g) {
            return these(f)(g)(function (x) {
                return function (y) {
                    return Data_Semigroup.append(dictMonoid["__superclass_Data.Semigroup.Semigroup_0"]())(f(x))(g(y));
                };
            });
        };
    };
}, function (f) {
    return function (g) {
        return function (z) {
            return these(function (v) {
                return f(z)(v);
            })(function (v) {
                return g(z)(v);
            })(function (x) {
                return function (y) {
                    return g(f(z)(x))(y);
                };
            });
        };
    };
}, function (f) {
    return function (g) {
        return function (z) {
            return these(function (v) {
                return f(v)(z);
            })(function (v) {
                return g(v)(z);
            })(function (x) {
                return function (y) {
                    return f(x)(g(y)(z));
                };
            });
        };
    };
});
var bitraversableThese = new Data_Bitraversable.Bitraversable(function () {
    return bifoldableThese;
}, function () {
    return bifunctorThese;
}, function (dictApplicative) {
    return Data_Bitraversable.bitraverse(bitraversableThese)(dictApplicative)(Control_Category.id(Control_Category.categoryFn))(Control_Category.id(Control_Category.categoryFn));
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            return function (v2) {
                if (v2 instanceof This) {
                    return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(This.create)(v(v2.value0));
                };
                if (v2 instanceof That) {
                    return Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(That.create)(v1(v2.value0));
                };
                if (v2 instanceof Both) {
                    return Control_Apply.apply(dictApplicative["__superclass_Control.Apply.Apply_0"]())(Data_Functor.map((dictApplicative["__superclass_Control.Apply.Apply_0"]())["__superclass_Data.Functor.Functor_0"]())(Both.create)(v(v2.value0)))(v1(v2.value1));
                };
                throw new Error("Failed pattern match at Data.These line 67, column 3 - line 67, column 41: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
            };
        };
    };
});
var applyThese = function (dictSemigroup) {
    return new Control_Apply.Apply(function () {
        return functorThese;
    }, function (v) {
        return function (v1) {
            if (v instanceof This) {
                return new This(v.value0);
            };
            if (v instanceof That && v1 instanceof This) {
                return new This(v1.value0);
            };
            if (v instanceof That && v1 instanceof That) {
                return new That(v.value0(v1.value0));
            };
            if (v instanceof That && v1 instanceof Both) {
                return new Both(v1.value0, v.value0(v1.value1));
            };
            if (v instanceof Both && v1 instanceof This) {
                return new This(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0));
            };
            if (v instanceof Both && v1 instanceof That) {
                return new Both(v.value0, v.value1(v1.value0));
            };
            if (v instanceof Both && v1 instanceof Both) {
                return new Both(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), v.value1(v1.value1));
            };
            throw new Error("Failed pattern match at Data.These line 73, column 3 - line 73, column 28: " + [ v.constructor.name, v1.constructor.name ]);
        };
    });
};
var bindThese = function (dictSemigroup) {
    return new Control_Bind.Bind(function () {
        return applyThese(dictSemigroup);
    }, function (v) {
        return function (v1) {
            if (v instanceof This) {
                return new This(v.value0);
            };
            if (v instanceof That) {
                return v1(v.value0);
            };
            if (v instanceof Both) {
                var $244 = v1(v.value1);
                if ($244 instanceof This) {
                    return new This(Data_Semigroup.append(dictSemigroup)(v.value0)($244.value0));
                };
                if ($244 instanceof That) {
                    return new Both(v.value0, $244.value0);
                };
                if ($244 instanceof Both) {
                    return new Both(Data_Semigroup.append(dictSemigroup)(v.value0)($244.value0), $244.value1);
                };
                throw new Error("Failed pattern match at Data.These line 88, column 5 - line 91, column 34: " + [ $244.constructor.name ]);
            };
            throw new Error("Failed pattern match at Data.These line 85, column 3 - line 85, column 27: " + [ v.constructor.name, v1.constructor.name ]);
        };
    });
};
var applicativeThese = function (dictSemigroup) {
    return new Control_Applicative.Applicative(function () {
        return applyThese(dictSemigroup);
    }, That.create);
};
var monadThese = function (dictSemigroup) {
    return new Control_Monad.Monad(function () {
        return applicativeThese(dictSemigroup);
    }, function () {
        return bindThese(dictSemigroup);
    });
};
module.exports = {
    This: This, 
    That: That, 
    Both: Both, 
    fromThese: fromThese, 
    thatOrBoth: thatOrBoth, 
    these: these, 
    theseLeft: theseLeft, 
    theseRight: theseRight, 
    thisOrBoth: thisOrBoth, 
    eqThese: eqThese, 
    ordThese: ordThese, 
    genericThese: genericThese, 
    semigroupThese: semigroupThese, 
    functorThese: functorThese, 
    invariantThese: invariantThese, 
    foldableThese: foldableThese, 
    traversableThese: traversableThese, 
    bifunctorThese: bifunctorThese, 
    bifoldableThese: bifoldableThese, 
    bitraversableThese: bitraversableThese, 
    applyThese: applyThese, 
    applicativeThese: applicativeThese, 
    bindThese: bindThese, 
    monadThese: monadThese, 
    extendEither: extendEither, 
    showThese: showThese
};
//# sourceMappingURL=index.js.map
