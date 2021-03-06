// Generated by psc version 0.10.2
"use strict";
var Prelude = require("../Prelude");
var Control_Lazy = require("../Control.Lazy");
var Data_Foldable = require("../Data.Foldable");
var Data_Lazy = require("../Data.Lazy");
var Data_List_Lazy_Types = require("../Data.List.Lazy.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_Newtype = require("../Data.Newtype");
var Data_NonEmpty = require("../Data.NonEmpty");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Control_Apply = require("../Control.Apply");
var Data_Functor = require("../Data.Functor");
var Data_Ring = require("../Data.Ring");
var Data_Ord = require("../Data.Ord");
var Data_Semiring = require("../Data.Semiring");
var Control_Applicative = require("../Control.Applicative");
var Data_Boolean = require("../Data.Boolean");
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Eq = require("../Data.Eq");
var Data_Ordering = require("../Data.Ordering");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Function = require("../Data.Function");
var Control_Category = require("../Control.Category");
var zipWith = function (f) {
    return function (xs) {
        return function (ys) {
            var go = function (v) {
                return function (v1) {
                    if (v instanceof Data_List_Lazy_Types.Nil) {
                        return Data_List_Lazy_Types.Nil.value;
                    };
                    if (v1 instanceof Data_List_Lazy_Types.Nil) {
                        return Data_List_Lazy_Types.Nil.value;
                    };
                    if (v instanceof Data_List_Lazy_Types.Cons && v1 instanceof Data_List_Lazy_Types.Cons) {
                        return new Data_List_Lazy_Types.Cons(f(v.value0)(v1.value0), zipWith(f)(v.value1)(v1.value1));
                    };
                    throw new Error("Failed pattern match at Data.List.Lazy line 638, column 3 - line 638, column 17: " + [ v.constructor.name, v1.constructor.name ]);
                };
            };
            return Control_Apply.apply(Data_Lazy.applyLazy)(Data_Functor.map(Data_Lazy.functorLazy)(go)(Data_Newtype.unwrap(Data_List_Lazy_Types.newtypeList)(xs)))(Data_Newtype.unwrap(Data_List_Lazy_Types.newtypeList)(ys));
        };
    };
};
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Traversable.sequence(Data_List_Lazy_Types.traversableList)(dictApplicative)(zipWith(f)(xs)(ys));
            };
        };
    };
};
var zip = zipWith(Data_Tuple.Tuple.create);
var updateAt = function (n) {
    return function (x) {
        return function (xs) {
            var go = function (v) {
                return function (v1) {
                    if (v1 instanceof Data_List_Lazy_Types.Nil) {
                        return Data_List_Lazy_Types.Nil.value;
                    };
                    if (v === 0 && v1 instanceof Data_List_Lazy_Types.Cons) {
                        return new Data_List_Lazy_Types.Cons(x, v1.value1);
                    };
                    if (v1 instanceof Data_List_Lazy_Types.Cons) {
                        return new Data_List_Lazy_Types.Cons(v1.value0, updateAt(v - 1)(x)(v1.value1));
                    };
                    throw new Error("Failed pattern match at Data.List.Lazy line 347, column 3 - line 347, column 17: " + [ v.constructor.name, v1.constructor.name ]);
                };
            };
            return Data_Functor.map(Data_Lazy.functorLazy)(go(n))(Data_Newtype.unwrap(Data_List_Lazy_Types.newtypeList)(xs));
        };
    };
};
var unzip = Data_Foldable.foldr(Data_List_Lazy_Types.foldableList)(function (v) {
    return function (v1) {
        return new Data_Tuple.Tuple(Data_List_Lazy_Types.cons(v.value0)(v1.value0), Data_List_Lazy_Types.cons(v.value1)(v1.value1));
    };
})(new Data_Tuple.Tuple(Data_List_Lazy_Types.nil, Data_List_Lazy_Types.nil));
var uncons = function (xs) {
    var $75 = Data_List_Lazy_Types.step(xs);
    if ($75 instanceof Data_List_Lazy_Types.Nil) {
        return Data_Maybe.Nothing.value;
    };
    if ($75 instanceof Data_List_Lazy_Types.Cons) {
        return new Data_Maybe.Just({
            head: $75.value0, 
            tail: $75.value1
        });
    };
    throw new Error("Failed pattern match at Data.List.Lazy line 265, column 13 - line 267, column 54: " + [ $75.constructor.name ]);
};
var toUnfoldable = function (dictUnfoldable) {
    return Data_Unfoldable.unfoldr(dictUnfoldable)(function (xs) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(function (rec) {
            return new Data_Tuple.Tuple(rec.head, rec.tail);
        })(uncons(xs));
    });
};
var takeWhile = function (p) {
    return function (xs) {
        var go = function (v) {
            if (v instanceof Data_List_Lazy_Types.Cons && p(v.value0)) {
                return new Data_List_Lazy_Types.Cons(v.value0, takeWhile(p)(v.value1));
            };
            return Data_List_Lazy_Types.Nil.value;
        };
        return Data_Functor.map(Data_Lazy.functorLazy)(go)(Data_Newtype.unwrap(Data_List_Lazy_Types.newtypeList)(xs));
    };
};
var take = function (n) {
    return function (xs) {
        var go = function (v) {
            return function (v1) {
                if (v <= 0) {
                    return Data_List_Lazy_Types.Nil.value;
                };
                if (v1 instanceof Data_List_Lazy_Types.Nil) {
                    return Data_List_Lazy_Types.Nil.value;
                };
                if (v1 instanceof Data_List_Lazy_Types.Cons) {
                    return new Data_List_Lazy_Types.Cons(v1.value0, take(v - 1)(v1.value1));
                };
                throw new Error("Failed pattern match at Data.List.Lazy line 471, column 3 - line 471, column 24: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        return Data_Functor.map(Data_Lazy.functorLazy)(go(n))(Data_Newtype.unwrap(Data_List_Lazy_Types.newtypeList)(xs));
    };
};
var tail = function (xs) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
        return v.tail;
    })(uncons(xs));
};
var span = function (p) {
    return function (xs) {
        var $85 = uncons(xs);
        if ($85 instanceof Data_Maybe.Just && p($85.value0.head)) {
            var $86 = span(p)($85.value0.tail);
            return {
                init: Data_List_Lazy_Types.cons($85.value0.head)($86.init), 
                rest: $86.rest
            };
        };
        return {
            init: Data_List_Lazy_Types.nil, 
            rest: xs
        };
    };
};
var snoc = function (xs) {
    return function (x) {
        return Data_Foldable.foldr(Data_List_Lazy_Types.foldableList)(Data_List_Lazy_Types.cons)(Data_List_Lazy_Types.cons(x)(Data_List_Lazy_Types.nil))(xs);
    };
};
var singleton = function (a) {
    return Data_List_Lazy_Types.cons(a)(Data_List_Lazy_Types.nil);
};
var reverse = function (xs) {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var acc = __copy_acc;
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Data_List_Lazy_Types.Nil) {
                    return acc;
                };
                if (v instanceof Data_List_Lazy_Types.Cons) {
                    var __tco_acc = Data_List_Lazy_Types.cons(v.value0)(acc);
                    var __tco_v = Data_List_Lazy_Types.step(v.value1);
                    acc = __tco_acc;
                    v = __tco_v;
                    continue tco;
                };
                throw new Error("Failed pattern match at Data.List.Lazy line 387, column 1 - line 390, column 49: " + [ acc.constructor.name, v.constructor.name ]);
            };
        };
    };
    return go(Data_List_Lazy_Types.nil)(Data_List_Lazy_Types.step(xs));
};
var replicateM = function (dictMonad) {
    return function (n) {
        return function (m) {
            if (n < 1) {
                return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(Data_List_Lazy_Types.nil);
            };
            if (Data_Boolean.otherwise) {
                return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(m)(function (v) {
                    return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(replicateM(dictMonad)(n - 1)(m))(function (v1) {
                        return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(Data_List_Lazy_Types.cons(v)(v1));
                    });
                });
            };
            throw new Error("Failed pattern match at Data.List.Lazy line 156, column 1 - line 161, column 23: " + [ n.constructor.name, m.constructor.name ]);
        };
    };
};
var repeat = function (x) {
    return Control_Lazy.fix(Data_List_Lazy_Types.lazyList)(function (xs) {
        return Data_List_Lazy_Types.cons(x)(xs);
    });
};
var replicate = function (i) {
    return function (xs) {
        return take(i)(repeat(xs));
    };
};
var range = function (start) {
    return function (end) {
        if (start > end) {
            var g = function (x) {
                if (x >= end) {
                    return new Data_Maybe.Just(new Data_Tuple.Tuple(x, x - 1));
                };
                if (Data_Boolean.otherwise) {
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Data.List.Lazy line 142, column 13 - line 143, column 38: " + [ x.constructor.name ]);
            };
            return Data_Unfoldable.unfoldr(Data_List_Lazy_Types.unfoldableList)(g)(start);
        };
        if (Data_Boolean.otherwise) {
            var f = function (x) {
                if (x <= end) {
                    return new Data_Maybe.Just(new Data_Tuple.Tuple(x, x + 1 | 0));
                };
                if (Data_Boolean.otherwise) {
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Data.List.Lazy line 147, column 5 - line 148, column 30: " + [ x.constructor.name ]);
            };
            return Data_Unfoldable.unfoldr(Data_List_Lazy_Types.unfoldableList)(f)(start);
        };
        throw new Error("Failed pattern match at Data.List.Lazy line 140, column 1 - line 148, column 30: " + [ start.constructor.name, end.constructor.name ]);
    };
};
var $$null = function ($192) {
    return Data_Maybe.isNothing(uncons($192));
};
var mapMaybe = function (f) {
    return function (xs) {
        var go = function (__copy_v) {
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Data_List_Lazy_Types.Nil) {
                    return Data_List_Lazy_Types.Nil.value;
                };
                if (v instanceof Data_List_Lazy_Types.Cons) {
                    var $105 = f(v.value0);
                    if ($105 instanceof Data_Maybe.Nothing) {
                        var __tco_v = Data_List_Lazy_Types.step(v.value1);
                        v = __tco_v;
                        continue tco;
                    };
                    if ($105 instanceof Data_Maybe.Just) {
                        return new Data_List_Lazy_Types.Cons($105.value0, mapMaybe(f)(v.value1));
                    };
                    throw new Error("Failed pattern match at Data.List.Lazy line 443, column 5 - line 445, column 39: " + [ $105.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.List.Lazy line 439, column 1 - line 445, column 39: " + [ v.constructor.name ]);
            };
        };
        return Data_Functor.map(Data_Lazy.functorLazy)(go)(Data_Newtype.unwrap(Data_List_Lazy_Types.newtypeList)(xs));
    };
};
var length = function (xs) {
    var go = function (v) {
        if (v instanceof Data_List_Lazy_Types.Nil) {
            return 0;
        };
        if (v instanceof Data_List_Lazy_Types.Cons) {
            return 1 + go(Data_List_Lazy_Types.step(v.value1)) | 0;
        };
        throw new Error("Failed pattern match at Data.List.Lazy line 189, column 1 - line 192, column 36: " + [ v.constructor.name ]);
    };
    return go(Data_List_Lazy_Types.step(xs));
};
var last = function (xs) {
    var go = function (__copy_v) {
        var v = __copy_v;
        tco: while (true) {
            if (v instanceof Data_List_Lazy_Types.Cons) {
                if ($$null(v.value1)) {
                    return new Data_Maybe.Just(v.value0);
                };
                if (Data_Boolean.otherwise) {
                    var __tco_v = Data_List_Lazy_Types.step(v.value1);
                    v = __tco_v;
                    continue tco;
                };
            };
            return Data_Maybe.Nothing.value;
        };
    };
    return go(Data_List_Lazy_Types.step(xs));
};
var iterate = function (f) {
    return function (x) {
        return Control_Lazy.fix(Data_List_Lazy_Types.lazyList)(function (xs) {
            return Data_List_Lazy_Types.cons(x)(Data_Functor.map(Data_List_Lazy_Types.functorList)(f)(xs));
        });
    };
};
var insertAt = function (v) {
    return function (x) {
        return function (xs) {
            if (v === 0) {
                return Data_List_Lazy_Types.cons(x)(xs);
            };
            var go = function (v1) {
                if (v1 instanceof Data_List_Lazy_Types.Nil) {
                    return new Data_List_Lazy_Types.Cons(x, Data_List_Lazy_Types.nil);
                };
                if (v1 instanceof Data_List_Lazy_Types.Cons) {
                    return new Data_List_Lazy_Types.Cons(v1.value0, insertAt(v - 1)(x)(v1.value1));
                };
                throw new Error("Failed pattern match at Data.List.Lazy line 320, column 3 - line 320, column 22: " + [ v1.constructor.name ]);
            };
            return Data_Functor.map(Data_Lazy.functorLazy)(go)(Data_Newtype.unwrap(Data_List_Lazy_Types.newtypeList)(xs));
        };
    };
};
var init = function (xs) {
    var go = function (v) {
        if (v instanceof Data_List_Lazy_Types.Cons) {
            if ($$null(v.value1)) {
                return new Data_Maybe.Just(Data_List_Lazy_Types.nil);
            };
            if (Data_Boolean.otherwise) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(Data_List_Lazy_Types.cons(v.value0))(go(Data_List_Lazy_Types.step(v.value1)));
            };
        };
        return Data_Maybe.Nothing.value;
    };
    return go(Data_List_Lazy_Types.step(xs));
};
var index = function (xs) {
    var go = function (__copy_v) {
        return function (__copy_v1) {
            var v = __copy_v;
            var v1 = __copy_v1;
            tco: while (true) {
                if (v instanceof Data_List_Lazy_Types.Nil) {
                    return Data_Maybe.Nothing.value;
                };
                if (v instanceof Data_List_Lazy_Types.Cons && v1 === 0) {
                    return new Data_Maybe.Just(v.value0);
                };
                if (v instanceof Data_List_Lazy_Types.Cons) {
                    var __tco_v = Data_List_Lazy_Types.step(v.value1);
                    var __tco_v1 = v1 - 1;
                    v = __tco_v;
                    v1 = __tco_v1;
                    continue tco;
                };
                throw new Error("Failed pattern match at Data.List.Lazy line 277, column 1 - line 281, column 42: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    };
    return go(Data_List_Lazy_Types.step(xs));
};
var head = function (xs) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
        return v.head;
    })(uncons(xs));
};
var transpose = function (xs) {
    var $130 = uncons(xs);
    if ($130 instanceof Data_Maybe.Nothing) {
        return xs;
    };
    if ($130 instanceof Data_Maybe.Just) {
        var $131 = uncons($130.value0.head);
        if ($131 instanceof Data_Maybe.Nothing) {
            return transpose($130.value0.tail);
        };
        if ($131 instanceof Data_Maybe.Just) {
            return Data_List_Lazy_Types.cons(Data_List_Lazy_Types.cons($131.value0.head)(mapMaybe(head)($130.value0.tail)))(transpose(Data_List_Lazy_Types.cons($131.value0.tail)(mapMaybe(tail)($130.value0.tail))));
        };
        throw new Error("Failed pattern match at Data.List.Lazy line 678, column 7 - line 682, column 71: " + [ $131.constructor.name ]);
    };
    throw new Error("Failed pattern match at Data.List.Lazy line 674, column 3 - line 682, column 71: " + [ $130.constructor.name ]);
};
var groupBy = function (eq) {
    return function (xs) {
        var go = function (v) {
            if (v instanceof Data_List_Lazy_Types.Nil) {
                return Data_List_Lazy_Types.Nil.value;
            };
            if (v instanceof Data_List_Lazy_Types.Cons) {
                var $139 = span(eq(v.value0))(v.value1);
                return new Data_List_Lazy_Types.Cons(Data_Lazy.defer(function (v1) {
                    return new Data_NonEmpty.NonEmpty(v.value0, $139.init);
                }), groupBy(eq)($139.rest));
            };
            throw new Error("Failed pattern match at Data.List.Lazy line 542, column 3 - line 542, column 15: " + [ v.constructor.name ]);
        };
        return Data_Functor.map(Data_Lazy.functorLazy)(go)(Data_Newtype.unwrap(Data_List_Lazy_Types.newtypeList)(xs));
    };
};
var group = function (dictEq) {
    return groupBy(Data_Eq.eq(dictEq));
};
var fromStep = function ($193) {
    return Data_List_Lazy_Types.List(Control_Applicative.pure(Data_Lazy.applicativeLazy)($193));
};
var insertBy = function (cmp) {
    return function (x) {
        return function (xs) {
            var go = function (v) {
                if (v instanceof Data_List_Lazy_Types.Nil) {
                    return new Data_List_Lazy_Types.Cons(x, Data_List_Lazy_Types.nil);
                };
                if (v instanceof Data_List_Lazy_Types.Cons) {
                    var $146 = cmp(x)(v.value0);
                    if ($146 instanceof Data_Ordering.GT) {
                        return new Data_List_Lazy_Types.Cons(v.value0, insertBy(cmp)(x)(v.value1));
                    };
                    return new Data_List_Lazy_Types.Cons(x, fromStep(v));
                };
                throw new Error("Failed pattern match at Data.List.Lazy line 217, column 3 - line 217, column 22: " + [ v.constructor.name ]);
            };
            return Data_Functor.map(Data_Lazy.functorLazy)(go)(Data_Newtype.unwrap(Data_List_Lazy_Types.newtypeList)(xs));
        };
    };
};
var insert = function (dictOrd) {
    return insertBy(Data_Ord.compare(dictOrd));
};
var fromFoldable = function (dictFoldable) {
    return Data_Foldable.foldr(dictFoldable)(Data_List_Lazy_Types.cons)(Data_List_Lazy_Types.nil);
};
var foldM = function (dictMonad) {
    return function (f) {
        return function (a) {
            return function (xs) {
                var $149 = uncons(xs);
                if ($149 instanceof Data_Maybe.Nothing) {
                    return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(a);
                };
                if ($149 instanceof Data_Maybe.Just) {
                    return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(f(a)($149.value0.head))(function (a$prime) {
                        return foldM(dictMonad)(f)(a$prime)($149.value0.tail);
                    });
                };
                throw new Error("Failed pattern match at Data.List.Lazy line 691, column 5 - line 694, column 52: " + [ $149.constructor.name ]);
            };
        };
    };
};
var findIndex = function (fn) {
    var go = function (n) {
        return function (list) {
            return Control_Bind.bind(Data_Maybe.bindMaybe)(uncons(list))(function (v) {
                var $154 = fn(v.head);
                if ($154) {
                    return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(n);
                };
                if (!$154) {
                    return go(n + 1 | 0)(v.tail);
                };
                throw new Error("Failed pattern match at Data.List.Lazy line 301, column 7 - line 303, column 32: " + [ $154.constructor.name ]);
            });
        };
    };
    return go(0);
};
var findLastIndex = function (fn) {
    return function (xs) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
            return length(xs) - 1 - v;
        })(findIndex(fn)(reverse(xs)));
    };
};
var filterM = function (dictMonad) {
    return function (p) {
        return function (list) {
            var $155 = uncons(list);
            if ($155 instanceof Data_Maybe.Nothing) {
                return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())(Data_List_Lazy_Types.nil);
            };
            if ($155 instanceof Data_Maybe.Just) {
                return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(p($155.value0.head))(function (v) {
                    return Control_Bind.bind(dictMonad["__superclass_Control.Bind.Bind_1"]())(filterM(dictMonad)(p)($155.value0.tail))(function (v1) {
                        return Control_Applicative.pure(dictMonad["__superclass_Control.Applicative.Applicative_0"]())((function () {
                            if (v) {
                                return Data_List_Lazy_Types.cons($155.value0.head)(v1);
                            };
                            if (!v) {
                                return v1;
                            };
                            throw new Error("Failed pattern match at Data.List.Lazy line 431, column 14 - line 431, column 48: " + [ v.constructor.name ]);
                        })());
                    });
                });
            };
            throw new Error("Failed pattern match at Data.List.Lazy line 426, column 5 - line 431, column 48: " + [ $155.constructor.name ]);
        };
    };
};
var filter = function (p) {
    return function (xs) {
        var go = function (__copy_v) {
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Data_List_Lazy_Types.Nil) {
                    return Data_List_Lazy_Types.Nil.value;
                };
                if (v instanceof Data_List_Lazy_Types.Cons) {
                    if (p(v.value0)) {
                        return new Data_List_Lazy_Types.Cons(v.value0, filter(p)(v.value1));
                    };
                    if (Data_Boolean.otherwise) {
                        var __tco_v = Data_List_Lazy_Types.step(v.value1);
                        v = __tco_v;
                        continue tco;
                    };
                };
                throw new Error("Failed pattern match at Data.List.Lazy line 409, column 1 - line 414, column 31: " + [ v.constructor.name ]);
            };
        };
        return Data_Functor.map(Data_Lazy.functorLazy)(go)(Data_Newtype.unwrap(Data_List_Lazy_Types.newtypeList)(xs));
    };
};
var intersectBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return filter(function (x) {
                return Data_Foldable.any(Data_List_Lazy_Types.foldableList)(Data_HeytingAlgebra.heytingAlgebraBoolean)(eq(x))(ys);
            })(xs);
        };
    };
};
var intersect = function (dictEq) {
    return intersectBy(Data_Eq.eq(dictEq));
};
var nubBy = function (eq) {
    return function (xs) {
        var go = function (v) {
            if (v instanceof Data_List_Lazy_Types.Nil) {
                return Data_List_Lazy_Types.Nil.value;
            };
            if (v instanceof Data_List_Lazy_Types.Cons) {
                return new Data_List_Lazy_Types.Cons(v.value0, nubBy(eq)(filter(function (y) {
                    return !eq(v.value0)(y);
                })(v.value1)));
            };
            throw new Error("Failed pattern match at Data.List.Lazy line 565, column 3 - line 565, column 15: " + [ v.constructor.name ]);
        };
        return Data_Functor.map(Data_Lazy.functorLazy)(go)(Data_Newtype.unwrap(Data_List_Lazy_Types.newtypeList)(xs));
    };
};
var nub = function (dictEq) {
    return nubBy(Data_Eq.eq(dictEq));
};
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var dropWhile = function (p) {
    return function (xs) {
        var go = function (__copy_v) {
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Data_List_Lazy_Types.Cons && p(v.value0)) {
                    var __tco_v = Data_List_Lazy_Types.step(v.value1);
                    v = __tco_v;
                    continue tco;
                };
                return fromStep(v);
            };
        };
        return go(Data_List_Lazy_Types.step(xs));
    };
};
var drop = function (n) {
    return function (xs) {
        var go = function (__copy_v) {
            return function (__copy_v1) {
                var v = __copy_v;
                var v1 = __copy_v1;
                tco: while (true) {
                    if (v === 0) {
                        return v1;
                    };
                    if (v1 instanceof Data_List_Lazy_Types.Nil) {
                        return Data_List_Lazy_Types.Nil.value;
                    };
                    if (v1 instanceof Data_List_Lazy_Types.Cons) {
                        var __tco_v = v - 1;
                        var __tco_v1 = Data_List_Lazy_Types.step(v1.value1);
                        v = __tco_v;
                        v1 = __tco_v1;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.List.Lazy line 488, column 1 - line 492, column 42: " + [ v.constructor.name, v1.constructor.name ]);
                };
            };
        };
        return Data_Functor.map(Data_Lazy.functorLazy)(go(n))(Data_Newtype.unwrap(Data_List_Lazy_Types.newtypeList)(xs));
    };
};
var slice = function (start) {
    return function (end) {
        return function (xs) {
            return take(end - start)(drop(start)(xs));
        };
    };
};
var deleteBy = function (eq) {
    return function (x) {
        return function (xs) {
            var go = function (v) {
                if (v instanceof Data_List_Lazy_Types.Nil) {
                    return Data_List_Lazy_Types.Nil.value;
                };
                if (v instanceof Data_List_Lazy_Types.Cons) {
                    if (eq(x)(v.value0)) {
                        return Data_List_Lazy_Types.step(v.value1);
                    };
                    if (Data_Boolean.otherwise) {
                        return new Data_List_Lazy_Types.Cons(v.value0, deleteBy(eq)(x)(v.value1));
                    };
                };
                throw new Error("Failed pattern match at Data.List.Lazy line 594, column 3 - line 594, column 15: " + [ v.constructor.name ]);
            };
            return Data_Functor.map(Data_Lazy.functorLazy)(go)(Data_Newtype.unwrap(Data_List_Lazy_Types.newtypeList)(xs));
        };
    };
};
var unionBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return Data_Semigroup.append(Data_List_Lazy_Types.semigroupList)(xs)(Data_Foldable.foldl(Data_List_Lazy_Types.foldableList)(Data_Function.flip(deleteBy(eq)))(nubBy(eq)(ys))(xs));
        };
    };
};
var union = function (dictEq) {
    return unionBy(Data_Eq.eq(dictEq));
};
var deleteAt = function (n) {
    return function (xs) {
        var go = function (v) {
            return function (v1) {
                if (v1 instanceof Data_List_Lazy_Types.Nil) {
                    return Data_List_Lazy_Types.Nil.value;
                };
                if (v === 0 && v1 instanceof Data_List_Lazy_Types.Cons) {
                    return Data_List_Lazy_Types.step(v1.value1);
                };
                if (v1 instanceof Data_List_Lazy_Types.Cons) {
                    return new Data_List_Lazy_Types.Cons(v1.value0, deleteAt(v - 1)(v1.value1));
                };
                throw new Error("Failed pattern match at Data.List.Lazy line 333, column 3 - line 333, column 17: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        return Data_Functor.map(Data_Lazy.functorLazy)(go(n))(Data_Newtype.unwrap(Data_List_Lazy_Types.newtypeList)(xs));
    };
};
var $$delete = function (dictEq) {
    return deleteBy(Data_Eq.eq(dictEq));
};
var difference = function (dictEq) {
    return Data_Foldable.foldl(Data_List_Lazy_Types.foldableList)(Data_Function.flip($$delete(dictEq)));
};
var cycle = function (xs) {
    return Control_Lazy.fix(Data_List_Lazy_Types.lazyList)(function (ys) {
        return Data_Semigroup.append(Data_List_Lazy_Types.semigroupList)(xs)(ys);
    });
};
var concatMap = Data_Function.flip(Control_Bind.bind(Data_List_Lazy_Types.bindList));
var concat = function (v) {
    return Control_Bind.bind(Data_List_Lazy_Types.bindList)(v)(Control_Category.id(Control_Category.categoryFn));
};
var catMaybes = mapMaybe(Control_Category.id(Control_Category.categoryFn));
var alterAt = function (n) {
    return function (f) {
        return function (xs) {
            var go = function (v) {
                return function (v1) {
                    if (v1 instanceof Data_List_Lazy_Types.Nil) {
                        return Data_List_Lazy_Types.Nil.value;
                    };
                    if (v === 0 && v1 instanceof Data_List_Lazy_Types.Cons) {
                        var $186 = f(v1.value0);
                        if ($186 instanceof Data_Maybe.Nothing) {
                            return Data_List_Lazy_Types.step(v1.value1);
                        };
                        if ($186 instanceof Data_Maybe.Just) {
                            return new Data_List_Lazy_Types.Cons($186.value0, v1.value1);
                        };
                        throw new Error("Failed pattern match at Data.List.Lazy line 374, column 22 - line 376, column 26: " + [ $186.constructor.name ]);
                    };
                    if (v1 instanceof Data_List_Lazy_Types.Cons) {
                        return new Data_List_Lazy_Types.Cons(v1.value0, alterAt(v - 1)(f)(v1.value1));
                    };
                    throw new Error("Failed pattern match at Data.List.Lazy line 373, column 3 - line 373, column 17: " + [ v.constructor.name, v1.constructor.name ]);
                };
            };
            return Data_Functor.map(Data_Lazy.functorLazy)(go(n))(Data_Newtype.unwrap(Data_List_Lazy_Types.newtypeList)(xs));
        };
    };
};
var modifyAt = function (n) {
    return function (f) {
        return alterAt(n)(function ($194) {
            return Data_Maybe.Just.create(f($194));
        });
    };
};
module.exports = {
    alterAt: alterAt, 
    catMaybes: catMaybes, 
    concat: concat, 
    concatMap: concatMap, 
    cycle: cycle, 
    "delete": $$delete, 
    deleteAt: deleteAt, 
    deleteBy: deleteBy, 
    difference: difference, 
    drop: drop, 
    dropWhile: dropWhile, 
    elemIndex: elemIndex, 
    elemLastIndex: elemLastIndex, 
    filter: filter, 
    filterM: filterM, 
    findIndex: findIndex, 
    findLastIndex: findLastIndex, 
    foldM: foldM, 
    fromFoldable: fromFoldable, 
    group: group, 
    groupBy: groupBy, 
    head: head, 
    index: index, 
    init: init, 
    insert: insert, 
    insertAt: insertAt, 
    insertBy: insertBy, 
    intersect: intersect, 
    intersectBy: intersectBy, 
    iterate: iterate, 
    last: last, 
    length: length, 
    mapMaybe: mapMaybe, 
    modifyAt: modifyAt, 
    nub: nub, 
    nubBy: nubBy, 
    "null": $$null, 
    range: range, 
    repeat: repeat, 
    replicate: replicate, 
    replicateM: replicateM, 
    reverse: reverse, 
    singleton: singleton, 
    slice: slice, 
    snoc: snoc, 
    span: span, 
    tail: tail, 
    take: take, 
    takeWhile: takeWhile, 
    toUnfoldable: toUnfoldable, 
    transpose: transpose, 
    uncons: uncons, 
    union: union, 
    unionBy: unionBy, 
    unzip: unzip, 
    updateAt: updateAt, 
    zip: zip, 
    zipWith: zipWith, 
    zipWithA: zipWithA
};
//# sourceMappingURL=index.js.map
