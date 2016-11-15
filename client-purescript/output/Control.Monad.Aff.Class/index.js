// Generated by psc version 0.10.2
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Cont_Trans = require("../Control.Monad.Cont.Trans");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans");
var Control_Monad_List_Trans = require("../Control.Monad.List.Trans");
var Control_Monad_Maybe_Trans = require("../Control.Monad.Maybe.Trans");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans");
var Control_Monad_RWS_Trans = require("../Control.Monad.RWS.Trans");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class");
var Control_Monad_Writer_Trans = require("../Control.Monad.Writer.Trans");
var Data_Monoid = require("../Data.Monoid");
var Control_Category = require("../Control.Category");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var MonadAff = function (__superclass_Control$dotMonad$dotEff$dotClass$dotMonadEff_0, liftAff) {
    this["__superclass_Control.Monad.Eff.Class.MonadEff_0"] = __superclass_Control$dotMonad$dotEff$dotClass$dotMonadEff_0;
    this.liftAff = liftAff;
};
var monadAffAff = new MonadAff(function () {
    return Control_Monad_Aff.monadEffAff;
}, Control_Category.id(Control_Category.categoryFn));
var liftAff = function (dict) {
    return dict.liftAff;
};
var monadAffContT = function (dictMonadAff) {
    return new MonadAff(function () {
        return Control_Monad_Cont_Trans.monadEffContT(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
    }, function ($10) {
        return Control_Monad_Trans_Class.lift(Control_Monad_Cont_Trans.monadTransContT)((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Control.Monad.Monad_0"]())(liftAff(dictMonadAff)($10));
    });
};
var monadAffExceptT = function (dictMonadAff) {
    return new MonadAff(function () {
        return Control_Monad_Except_Trans.monadEffExceptT(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
    }, function ($11) {
        return Control_Monad_Trans_Class.lift(Control_Monad_Except_Trans.monadTransExceptT)((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Control.Monad.Monad_0"]())(liftAff(dictMonadAff)($11));
    });
};
var monadAffListT = function (dictMonadAff) {
    return new MonadAff(function () {
        return Control_Monad_List_Trans.monadEffListT(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
    }, function ($12) {
        return Control_Monad_Trans_Class.lift(Control_Monad_List_Trans.monadTransListT)((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Control.Monad.Monad_0"]())(liftAff(dictMonadAff)($12));
    });
};
var monadAffMaybe = function (dictMonadAff) {
    return new MonadAff(function () {
        return Control_Monad_Maybe_Trans.monadEffMaybe(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
    }, function ($13) {
        return Control_Monad_Trans_Class.lift(Control_Monad_Maybe_Trans.monadTransMaybeT)((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Control.Monad.Monad_0"]())(liftAff(dictMonadAff)($13));
    });
};
var monadAffRWS = function (dictMonadAff) {
    return function (dictMonoid) {
        return new MonadAff(function () {
            return Control_Monad_RWS_Trans.monadEffRWS(dictMonoid)(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
        }, function ($14) {
            return Control_Monad_Trans_Class.lift(Control_Monad_RWS_Trans.monadTransRWST(dictMonoid))((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Control.Monad.Monad_0"]())(liftAff(dictMonadAff)($14));
        });
    };
};
var monadAffReader = function (dictMonadAff) {
    return new MonadAff(function () {
        return Control_Monad_Reader_Trans.monadEffReader(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
    }, function ($15) {
        return Control_Monad_Trans_Class.lift(Control_Monad_Reader_Trans.monadTransReaderT)((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Control.Monad.Monad_0"]())(liftAff(dictMonadAff)($15));
    });
};
var monadAffState = function (dictMonadAff) {
    return new MonadAff(function () {
        return Control_Monad_State_Trans.monadEffState(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
    }, function ($16) {
        return Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Control.Monad.Monad_0"]())(liftAff(dictMonadAff)($16));
    });
};
var monadAffWriter = function (dictMonadAff) {
    return function (dictMonoid) {
        return new MonadAff(function () {
            return Control_Monad_Writer_Trans.monadEffWriter(dictMonoid)(dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]());
        }, function ($17) {
            return Control_Monad_Trans_Class.lift(Control_Monad_Writer_Trans.monadTransWriterT(dictMonoid))((dictMonadAff["__superclass_Control.Monad.Eff.Class.MonadEff_0"]())["__superclass_Control.Monad.Monad_0"]())(liftAff(dictMonadAff)($17));
        });
    };
};
module.exports = {
    MonadAff: MonadAff, 
    liftAff: liftAff, 
    monadAffAff: monadAffAff, 
    monadAffContT: monadAffContT, 
    monadAffExceptT: monadAffExceptT, 
    monadAffListT: monadAffListT, 
    monadAffMaybe: monadAffMaybe, 
    monadAffReader: monadAffReader, 
    monadAffRWS: monadAffRWS, 
    monadAffState: monadAffState, 
    monadAffWriter: monadAffWriter
};
//# sourceMappingURL=index.js.map
