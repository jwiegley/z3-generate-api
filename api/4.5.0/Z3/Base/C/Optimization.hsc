{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_optimization.h"
module Z3.Base.C.Optimization where
import Foreign.Ptr
#strict_import
import Z3.Base.C.Api

{- | Create a new optimize context.

       \remark User must use #Z3_optimize_inc_ref and #Z3_optimize_dec_ref to manage optimize objects.
       Even if the context was created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
#ccall Z3_mk_optimize , <Z3_context> -> IO <Z3_optimize>

{- | Increment the reference counter of the given optimize context -}
#ccall Z3_optimize_inc_ref , <Z3_context> -> <Z3_optimize> -> IO ()

{- | Decrement the reference counter of the given optimize context. -}
#ccall Z3_optimize_dec_ref , <Z3_context> -> <Z3_optimize> -> IO ()

{- | Assert hard constraint to the optimization context. -}
#ccall Z3_optimize_assert , <Z3_context> -> <Z3_optimize> -> <Z3_ast> -> IO ()

{- | Assert soft constraint to the optimization context.
       \param c - context
       \param o - optimization context
       \param a - formula
       \param weight - a positive weight, penalty for violating soft constraint
       \param id - optional identifier to group soft constraints -}
#ccall Z3_optimize_assert_soft , <Z3_context> -> <Z3_optimize> -> <Z3_ast> -> <Z3_string> -> <Z3_symbol> -> IO ()

{- | Add a maximization constraint.
       \param c - context
       \param o - optimization context
       \param a - arithmetical term -}
#ccall Z3_optimize_maximize , <Z3_context> -> <Z3_optimize> -> <Z3_ast> -> IO ()

{- | Add a minimization constraint.
       \param c - context
       \param o - optimization context
       \param a - arithmetical term -}
#ccall Z3_optimize_minimize , <Z3_context> -> <Z3_optimize> -> <Z3_ast> -> IO ()

{- | Create a backtracking point.

       The optimize solver contains a set of rules, added facts and assertions.
       The set of rules, facts and assertions are restored upon calling #Z3_optimize_pop.

       \sa Z3_optimize_pop -}
#ccall Z3_optimize_push , <Z3_context> -> <Z3_optimize> -> IO ()

{- | Backtrack one level.

       \sa Z3_optimize_push

       \pre The number of calls to pop cannot exceed calls to push. -}
#ccall Z3_optimize_pop , <Z3_context> -> <Z3_optimize> -> IO ()

{- | Check consistency and produce optimal values.
       \param c - context
       \param o - optimization context -}
#ccall Z3_optimize_check , <Z3_context> -> <Z3_optimize> -> IO <Z3_lbool>

{- | Retrieve a string that describes the last status returned by #Z3_optimize_check.

       Use this method when #Z3_optimize_check returns Z3_L_UNDEF. -}
#ccall Z3_optimize_get_reason_unknown , <Z3_context> -> <Z3_optimize> -> IO <Z3_string>

{- | Retrieve the model for the last #Z3_optimize_check

       The error handler is invoked if a model is not available because
       the commands above were not invoked for the given optimization
       solver, or if the result was \c Z3_L_FALSE. -}
#ccall Z3_optimize_get_model , <Z3_context> -> <Z3_optimize> -> IO <Z3_model>

{- | Set parameters on optimization context.

       \param c - context
       \param o - optimization context
       \param p - parameters -}
#ccall Z3_optimize_set_params , <Z3_context> -> <Z3_optimize> -> <Z3_params> -> IO ()

{- | Return the parameter description set for the given optimize object.

       \param c - context
       \param o - optimization context -}
#ccall Z3_optimize_get_param_descrs , <Z3_context> -> <Z3_optimize> -> IO <Z3_param_descrs>

{- | Retrieve lower bound value or approximation for the i'th optimization objective.

       \param c - context
       \param o - optimization context
       \param idx - index of optimization objective -}
#ccall Z3_optimize_get_lower , <Z3_context> -> <Z3_optimize> -> CUInt -> IO <Z3_ast>

{- | Retrieve upper bound value or approximation for the i'th optimization objective.

       \param c - context
       \param o - optimization context
       \param idx - index of optimization objective -}
#ccall Z3_optimize_get_upper , <Z3_context> -> <Z3_optimize> -> CUInt -> IO <Z3_ast>

{- | Print the current context as a string.
       \param c - context.
       \param o - optimization context. -}
#ccall Z3_optimize_to_string , <Z3_context> -> <Z3_optimize> -> IO <Z3_string>

{- | Parse an SMT-LIB2 string with assertions,
       soft constraints and optimization objectives.
       Add the parsed constraints and objectives to the optimization context.

       \param c - context.
       \param o - optimize context.
       \param s - string containing SMT2 specification. -}
#ccall Z3_optimize_from_string , <Z3_context> -> <Z3_optimize> -> <Z3_string> -> IO ()

{- | Parse an SMT-LIB2 file with assertions,
       soft constraints and optimization objectives.
       Add the parsed constraints and objectives to the optimization context.

       \param c - context.
       \param o - optimize context.
       \param s - string containing SMT2 specification. -}
#ccall Z3_optimize_from_file , <Z3_context> -> <Z3_optimize> -> <Z3_string> -> IO ()

{- | Return a string containing a description of parameters accepted by optimize. -}
#ccall Z3_optimize_get_help , <Z3_context> -> <Z3_optimize> -> IO <Z3_string>

{- | Retrieve statistics information from the last call to #Z3_optimize_check -}
#ccall Z3_optimize_get_statistics , <Z3_context> -> <Z3_optimize> -> IO <Z3_stats>

{- | Return the set of asserted formulas on the optimization context. -}
#ccall Z3_optimize_get_assertions , <Z3_context> -> <Z3_optimize> -> IO <Z3_ast_vector>

{- | Return objectives on the optimization context.
       If the objective function is a max-sat objective it is returned
       as a Pseudo-Boolean (minimization) sum of the form (+ (if f1 w1 0) (if f2 w2 0) ...)
       If the objective function is entered as a maximization objective, then return
       the corresponding minimization objective. In this way the resulting objective
       function is always returned as a minimization objective. -}
#ccall Z3_optimize_get_objectives , <Z3_context> -> <Z3_optimize> -> IO <Z3_ast_vector>
