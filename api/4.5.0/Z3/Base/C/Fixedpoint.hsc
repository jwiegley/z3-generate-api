{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_fixedpoint.h"
module Z3.Base.C.Fixedpoint where
import Foreign.Ptr
#strict_import
import Z3.Base.C.Api

{- | Create a new fixedpoint context.

       \remark User must use #Z3_fixedpoint_inc_ref and #Z3_fixedpoint_dec_ref to manage fixedpoint objects.
       Even if the context was created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
#ccall Z3_mk_fixedpoint , <Z3_context> -> IO <Z3_fixedpoint>

{- | Increment the reference counter of the given fixedpoint context -}
#ccall Z3_fixedpoint_inc_ref , <Z3_context> -> <Z3_fixedpoint> -> IO ()

{- | Decrement the reference counter of the given fixedpoint context. -}
#ccall Z3_fixedpoint_dec_ref , <Z3_context> -> <Z3_fixedpoint> -> IO ()

{- | Add a universal Horn clause as a named rule.
       The \c horn_rule should be of the form:

       \code
           horn_rule ::= (forall (bound-vars) horn_rule)
                      |  (=> atoms horn_rule)
                      |  atom
       \endcode -}
#ccall Z3_fixedpoint_add_rule , <Z3_context> -> <Z3_fixedpoint> -> <Z3_ast> -> <Z3_symbol> -> IO ()

{- | Add a Database fact.

       \param c - context
       \param d - fixed point context
       \param r - relation signature for the row.
       \param num_args - number of columns for the given row.
       \param args - array of the row elements.

       The number of arguments \c num_args should be equal to the number
       of sorts in the domain of \c r. Each sort in the domain should be an integral
      (bit-vector, Boolean or or finite domain sort).

       The call has the same effect as adding a rule where \c r is applied to the arguments. -}
#ccall Z3_fixedpoint_add_fact , <Z3_context> -> <Z3_fixedpoint> -> <Z3_func_decl> -> CUInt -> Ptr CUInt -> IO ()

{- | Assert a constraint to the fixedpoint context.

       The constraints are used as background axioms when the fixedpoint engine uses the PDR mode.
       They are ignored for standard Datalog mode. -}
#ccall Z3_fixedpoint_assert , <Z3_context> -> <Z3_fixedpoint> -> <Z3_ast> -> IO ()

{- | Pose a query against the asserted rules.

        \code
           query ::= (exists (bound-vars) query)
                 |  literals
        \endcode

        query returns
        - Z3_L_FALSE if the query is unsatisfiable.
        - Z3_L_TRUE if the query is satisfiable. Obtain the answer by calling #Z3_fixedpoint_get_answer.
        - Z3_L_UNDEF if the query was interrupted, timed out or otherwise failed. -}
#ccall Z3_fixedpoint_query , <Z3_context> -> <Z3_fixedpoint> -> <Z3_ast> -> IO <Z3_lbool>

{- | Pose multiple queries against the asserted rules.

        The queries are encoded as relations (function declarations).

        query returns
        - Z3_L_FALSE if the query is unsatisfiable.
        - Z3_L_TRUE if the query is satisfiable. Obtain the answer by calling #Z3_fixedpoint_get_answer.
        - Z3_L_UNDEF if the query was interrupted, timed out or otherwise failed. -}
#ccall Z3_fixedpoint_query_relations , <Z3_context> -> <Z3_fixedpoint> -> CUInt -> Ptr <Z3_func_decl> -> IO <Z3_lbool>

{- | Retrieve a formula that encodes satisfying answers to the query.


       When used in Datalog mode, the returned answer is a disjunction of conjuncts.
       Each conjunct encodes values of the bound variables of the query that are satisfied.
       In PDR mode, the returned answer is a single conjunction.

       When used in Datalog mode the previous call to Z3_fixedpoint_query must have returned Z3_L_TRUE.
       When used with the PDR engine, the previous call must have been either Z3_L_TRUE or Z3_L_FALSE. -}
#ccall Z3_fixedpoint_get_answer , <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_ast>

{- | Retrieve a string that describes the last status returned by #Z3_fixedpoint_query.

       Use this method when #Z3_fixedpoint_query returns Z3_L_UNDEF. -}
#ccall Z3_fixedpoint_get_reason_unknown , <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_string>

{- | Update a named rule.
       A rule with the same name must have been previously created. -}
#ccall Z3_fixedpoint_update_rule , <Z3_context> -> <Z3_fixedpoint> -> <Z3_ast> -> <Z3_symbol> -> IO ()

{- | Query the PDR engine for the maximal levels properties are known about predicate.

       This call retrieves the maximal number of relevant unfoldings
       of \c pred with respect to the current exploration state.
       Note: this functionality is PDR specific. -}
#ccall Z3_fixedpoint_get_num_levels , <Z3_context> -> <Z3_fixedpoint> -> <Z3_func_decl> -> IO ()
#ccall Z3_fixedpoint_get_cover_delta , <Z3_context> -> <Z3_fixedpoint> -> CInt -> <Z3_func_decl> -> IO <Z3_ast>

{- | Add property about the predicate \c pred.
       Add a property of predicate \c pred at \c level.
       It gets pushed forward when possible.

       Note: level = -1 is treated as the fixedpoint. So passing -1 for the \c level
       means that the property is true of the fixed-point unfolding with respect to \c pred.

       Note: this functionality is PDR specific. -}
#ccall Z3_fixedpoint_add_cover , <Z3_context> -> <Z3_fixedpoint> -> CInt -> <Z3_func_decl> -> <Z3_ast> -> IO ()

{- | Retrieve statistics information from the last call to #Z3_fixedpoint_query. -}
#ccall Z3_fixedpoint_get_statistics , <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_stats>

{- | Register relation as Fixedpoint defined.
       Fixedpoint defined relations have least-fixedpoint semantics.
       For example, the relation is empty if it does not occur
       in a head or a fact. -}
#ccall Z3_fixedpoint_register_relation , <Z3_context> -> <Z3_fixedpoint> -> <Z3_func_decl> -> IO ()

{- | Configure the predicate representation.

       It sets the predicate to use a set of domains given by the list of symbols.
       The domains given by the list of symbols must belong to a set
       of built-in domains. -}
#ccall Z3_fixedpoint_set_predicate_representation , <Z3_context> -> <Z3_fixedpoint> -> <Z3_func_decl> -> CUInt -> Ptr <Z3_symbol> -> IO ()

{- | Retrieve set of rules from fixedpoint context. -}
#ccall Z3_fixedpoint_get_rules , <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_ast_vector>

{- | Retrieve set of background assertions from fixedpoint context. -}
#ccall Z3_fixedpoint_get_assertions , <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_ast_vector>

{- | Set parameters on fixedpoint context. -}
#ccall Z3_fixedpoint_set_params , <Z3_context> -> <Z3_fixedpoint> -> <Z3_params> -> IO ()

{- | Return a string describing all fixedpoint available parameters. -}
#ccall Z3_fixedpoint_get_help , <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_string>

{- | Return the parameter description set for the given fixedpoint object. -}
#ccall Z3_fixedpoint_get_param_descrs , <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_param_descrs>

{- | Print the current rules and background axioms as a string.
       \param c - context.
       \param f - fixedpoint context.
       \param num_queries - number of additional queries to print.
       \param queries - additional queries. -}
#ccall Z3_fixedpoint_to_string , <Z3_context> -> <Z3_fixedpoint> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_string>

{- | Parse an SMT-LIB2 string with fixedpoint rules.
       Add the rules to the current fixedpoint context.
       Return the set of queries in the string.

       \param c - context.
       \param f - fixedpoint context.
       \param s - string containing SMT2 specification. -}
#ccall Z3_fixedpoint_from_string , <Z3_context> -> <Z3_fixedpoint> -> <Z3_string> -> IO <Z3_ast_vector>

{- | Parse an SMT-LIB2 file with fixedpoint rules.
       Add the rules to the current fixedpoint context.
       Return the set of queries in the file.

       \param c - context.
       \param f - fixedpoint context.
       \param s - string containing SMT2 specification. -}
#ccall Z3_fixedpoint_from_file , <Z3_context> -> <Z3_fixedpoint> -> <Z3_string> -> IO <Z3_ast_vector>

{- | Create a backtracking point.

       The fixedpoint solver contains a set of rules, added facts and assertions.
       The set of rules, facts and assertions are restored upon calling #Z3_fixedpoint_pop.

       \sa Z3_fixedpoint_pop -}
#ccall Z3_fixedpoint_push , <Z3_context> -> <Z3_fixedpoint> -> IO ()

{- | Backtrack one backtracking point.

       \sa Z3_fixedpoint_push

       \pre The number of calls to pop cannot exceed calls to push. -}
#ccall Z3_fixedpoint_pop , <Z3_context> -> <Z3_fixedpoint> -> IO ()
#callback_t Z3_fixedpoint_reduce_assign_callback_fptr , Ptr () -> <Z3_func_decl> -> CUInt -> Ptr <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> IO ()
#callback_t Z3_fixedpoint_reduce_app_callback_fptr , Ptr () -> <Z3_func_decl> -> CUInt -> Ptr <Z3_ast> -> Ptr <Z3_ast> -> IO ()

{- | The following utilities allows adding user-defined domains. -}
#ccall Z3_fixedpoint_init , <Z3_context> -> <Z3_fixedpoint> -> Ptr () -> IO ()

{- | Register a callback to destructive updates.

       Registers are identified with terms encoded as fresh constants, -}
#ccall Z3_fixedpoint_set_reduce_assign_callback , <Z3_context> -> <Z3_fixedpoint> -> <Z3_fixedpoint_reduce_assign_callback_fptr> -> IO ()

{- | Register a callback for buildling terms based on the relational operators. -}
#ccall Z3_fixedpoint_set_reduce_app_callback , <Z3_context> -> <Z3_fixedpoint> -> <Z3_fixedpoint_reduce_app_callback_fptr> -> IO ()
