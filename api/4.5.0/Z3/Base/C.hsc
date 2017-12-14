{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_algebraic.h"
#include "z3_ast_containers.h"
#include "z3_fixedpoint.h"
#include "z3_fpa.h"
#include "z3_interp.h"
#include "z3_optimization.h"
#include "z3_polynomial.h"
#include "z3_rcf.h"

module Z3.Base.C where
import Foreign.Ptr
#strict_import

{- | Z3 Boolean type. It is just an alias for \c int. -}
foreign import ccall unsafe "Z3_global_param_set"
  z3_global_param_set :: <Z3_string> -> <Z3_string> -> IO ()

{- | Restore the value of all global (and module) parameters.
       This command will not affect already created objects (such as tactics and solvers).

       \sa Z3_global_param_set -}
foreign import ccall unsafe "Z3_global_param_reset_all"
  z3_global_param_reset_all :: IO ()

{- | Get a global (or module) parameter.

       Returns \c Z3_FALSE if the parameter value does not exist.

       \sa Z3_global_param_set

       \remark This function cannot be invoked simultaneously from different threads without synchronization.
       The result string stored in param_value is stored in shared location. -}
foreign import ccall unsafe "Z3_global_param_get"
  z3_global_param_get :: <Z3_string> -> <Z3_string_ptr> -> IO CInt
foreign import ccall unsafe "Z3_mk_config"
  z3_mk_config :: IO <Z3_config>
foreign import ccall unsafe "Z3_del_config"
  z3_del_config :: <Z3_config> -> IO ()
foreign import ccall unsafe "Z3_set_param_value"
  z3_set_param_value :: <Z3_config> -> <Z3_string> -> <Z3_string> -> IO ()
foreign import ccall unsafe "Z3_mk_context"
  z3_mk_context :: <Z3_config> -> IO <Z3_context>

{- | Create a context using the given configuration.
       This function is similar to #Z3_mk_context. However,
       in the context returned by this function, the user
       is responsible for managing Z3_ast reference counters.
       Managing reference counters is a burden and error-prone,
       but allows the user to use the memory more efficiently.
       The user must invoke #Z3_inc_ref for any Z3_ast returned
       by Z3, and #Z3_dec_ref whenever the Z3_ast is not needed
       anymore. This idiom is similar to the one used in
       BDD (binary decision diagrams) packages such as CUDD.

       Remarks:

       - Z3_sort, Z3_func_decl, Z3_app, Z3_pattern are Z3_ast's.
       - After a context is created, the configuration cannot be changed.
       - All main interaction with Z3 happens in the context of a \c Z3_context.
       - Z3 uses hash-consing, i.e., when the same Z3_ast is created twice,
         Z3 will return the same pointer twice. -}
foreign import ccall unsafe "Z3_mk_context_rc"
  z3_mk_context_rc :: <Z3_config> -> IO <Z3_context>

{- | Delete the given logical context.

       \sa Z3_mk_context -}
foreign import ccall unsafe "Z3_del_context"
  z3_del_context :: <Z3_context> -> IO ()

{- | Increment the reference counter of the given AST.
       The context \c c should have been created using #Z3_mk_context_rc.
       This function is a NOOP if \c c was created using #Z3_mk_context. -}
foreign import ccall unsafe "Z3_inc_ref"
  z3_inc_ref :: <Z3_context> -> <Z3_ast> -> IO ()

{- | Decrement the reference counter of the given AST.
       The context \c c should have been created using #Z3_mk_context_rc.
       This function is a NOOP if \c c was created using #Z3_mk_context. -}
foreign import ccall unsafe "Z3_dec_ref"
  z3_dec_ref :: <Z3_context> -> <Z3_ast> -> IO ()
foreign import ccall unsafe "Z3_update_param_value"
  z3_update_param_value :: <Z3_context> -> <Z3_string> -> <Z3_string> -> IO ()

{- | Interrupt the execution of a Z3 procedure.
       This procedure can be used to interrupt: solvers, simplifiers and tactics. -}
foreign import ccall unsafe "Z3_interrupt"
  z3_interrupt :: <Z3_context> -> IO ()

{- | Create a Z3 (empty) parameter set.
       Starting at Z3 4.0, parameter sets are used to configure many components such as:
       simplifiers, tactics, solvers, etc.

       \remark Reference counting must be used to manage parameter sets, even when the Z3_context was
       created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
foreign import ccall unsafe "Z3_mk_params"
  z3_mk_params :: <Z3_context> -> IO <Z3_params>

{- | Increment the reference counter of the given parameter set. -}
foreign import ccall unsafe "Z3_params_inc_ref"
  z3_params_inc_ref :: <Z3_context> -> <Z3_params> -> IO ()

{- | Decrement the reference counter of the given parameter set. -}
foreign import ccall unsafe "Z3_params_dec_ref"
  z3_params_dec_ref :: <Z3_context> -> <Z3_params> -> IO ()

{- | Add a Boolean parameter \c k with value \c v to the parameter set \c p. -}
foreign import ccall unsafe "Z3_params_set_bool"
  z3_params_set_bool :: <Z3_context> -> <Z3_params> -> <Z3_symbol> -> CInt -> IO ()

{- | Add a unsigned parameter \c k with value \c v to the parameter set \c p. -}
foreign import ccall unsafe "Z3_params_set_uint"
  z3_params_set_uint :: <Z3_context> -> <Z3_params> -> <Z3_symbol> -> CUInt -> IO ()

{- | Add a double parameter \c k with value \c v to the parameter set \c p. -}
foreign import ccall unsafe "Z3_params_set_double"
  z3_params_set_double :: <Z3_context> -> <Z3_params> -> <Z3_symbol> -> CDouble -> IO ()

{- | Add a symbol parameter \c k with value \c v to the parameter set \c p. -}
foreign import ccall unsafe "Z3_params_set_symbol"
  z3_params_set_symbol :: <Z3_context> -> <Z3_params> -> <Z3_symbol> -> <Z3_symbol> -> IO ()

{- | Convert a parameter set into a string. This function is mainly used for printing the
       contents of a parameter set. -}
foreign import ccall unsafe "Z3_params_to_string"
  z3_params_to_string :: <Z3_context> -> <Z3_params> -> IO <Z3_string>

{- | Validate the parameter set \c p against the parameter description set \c d.

       The procedure invokes the error handler if \c p is invalid. -}
foreign import ccall unsafe "Z3_params_validate"
  z3_params_validate :: <Z3_context> -> <Z3_params> -> <Z3_param_descrs> -> IO ()

{- | Increment the reference counter of the given parameter description set. -}
foreign import ccall unsafe "Z3_param_descrs_inc_ref"
  z3_param_descrs_inc_ref :: <Z3_context> -> <Z3_param_descrs> -> IO ()

{- | Decrement the reference counter of the given parameter description set. -}
foreign import ccall unsafe "Z3_param_descrs_dec_ref"
  z3_param_descrs_dec_ref :: <Z3_context> -> <Z3_param_descrs> -> IO ()

{- | Return the kind associated with the given parameter name \c n. -}
foreign import ccall unsafe "Z3_param_descrs_get_kind"
  z3_param_descrs_get_kind :: <Z3_context> -> <Z3_param_descrs> -> <Z3_symbol> -> IO <Z3_param_kind>

{- | Return the number of parameters in the given parameter description set. -}
foreign import ccall unsafe "Z3_param_descrs_size"
  z3_param_descrs_size :: <Z3_context> -> <Z3_param_descrs> -> IO ()

{- | Return the number of parameters in the given parameter description set.

       \pre i < Z3_param_descrs_size(c, p) -}
foreign import ccall unsafe "Z3_param_descrs_get_name"
  z3_param_descrs_get_name :: <Z3_context> -> <Z3_param_descrs> -> CUInt -> IO <Z3_symbol>

{- | Retrieve documentation string corresponding to parameter name \c s. -}
foreign import ccall unsafe "Z3_param_descrs_get_documentation"
  z3_param_descrs_get_documentation :: <Z3_context> -> <Z3_param_descrs> -> <Z3_symbol> -> IO <Z3_string>

{- | Convert a parameter description set into a string. This function is mainly used for printing the
       contents of a parameter description set. -}
foreign import ccall unsafe "Z3_param_descrs_to_string"
  z3_param_descrs_to_string :: <Z3_context> -> <Z3_param_descrs> -> IO <Z3_string>

{- | Create a Z3 symbol using an integer.

       Symbols are used to name several term and type constructors.

       NB. Not all integers can be passed to this function.
       The legal range of unsigned integers is 0 to 2^30-1.

       \sa Z3_mk_string_symbol -}
foreign import ccall unsafe "Z3_mk_int_symbol"
  z3_mk_int_symbol :: <Z3_context> -> CInt -> IO <Z3_symbol>

{- | Create a Z3 symbol using a C string.

       Symbols are used to name several term and type constructors.

       \sa Z3_mk_int_symbol -}
foreign import ccall unsafe "Z3_mk_string_symbol"
  z3_mk_string_symbol :: <Z3_context> -> <Z3_string> -> IO <Z3_symbol>

{- | Create a free (uninterpreted) type using the given name (symbol).

       Two free types are considered the same iff the have the same name. -}
foreign import ccall unsafe "Z3_mk_uninterpreted_sort"
  z3_mk_uninterpreted_sort :: <Z3_context> -> <Z3_symbol> -> IO <Z3_sort>

{- | Create the Boolean type.

       This type is used to create propositional variables and predicates. -}
foreign import ccall unsafe "Z3_mk_bool_sort"
  z3_mk_bool_sort :: <Z3_context> -> IO <Z3_sort>

{- | Create the integer type.

       This type is not the int type found in programming languages.
       A machine integer can be represented using bit-vectors. The function
       #Z3_mk_bv_sort creates a bit-vector type.

       \sa Z3_mk_bv_sort -}
foreign import ccall unsafe "Z3_mk_int_sort"
  z3_mk_int_sort :: <Z3_context> -> IO <Z3_sort>

{- | Create the real type.

       Note that this type is not a floating point number. -}
foreign import ccall unsafe "Z3_mk_real_sort"
  z3_mk_real_sort :: <Z3_context> -> IO <Z3_sort>

{- | Create a bit-vector type of the given size.

       This type can also be seen as a machine integer.

       \remark The size of the bit-vector type must be greater than zero. -}
foreign import ccall unsafe "Z3_mk_bv_sort"
  z3_mk_bv_sort :: <Z3_context> -> CUInt -> IO <Z3_sort>

{- | Create a named finite domain sort.

       To create constants that belong to the finite domain,
       use the APIs for creating numerals and pass a numeric
       constant together with the sort returned by this call.
       The numeric constant should be between 0 and the less
       than the size of the domain.

       \sa Z3_get_finite_domain_sort_size -}
foreign import ccall unsafe "Z3_mk_finite_domain_sort"
  z3_mk_finite_domain_sort :: <Z3_context> -> <Z3_symbol> -> CULong -> IO <Z3_sort>

{- | Create an array type.

       We usually represent the array type as: \ccode{[domain -> range]}.
       Arrays are usually used to model the heap/memory in software verification.

       \sa Z3_mk_select
       \sa Z3_mk_store -}
foreign import ccall unsafe "Z3_mk_array_sort"
  z3_mk_array_sort :: <Z3_context> -> <Z3_sort> -> <Z3_sort> -> IO <Z3_sort>

{- | Create a tuple type.

       A tuple with \c n fields has a constructor and \c n projections.
       This function will also declare the constructor and projection functions.

       \param c logical context
       \param mk_tuple_name name of the constructor function associated with the tuple type.
       \param num_fields number of fields in the tuple type.
       \param field_names name of the projection functions.
       \param field_sorts type of the tuple fields.
       \param mk_tuple_decl output parameter that will contain the constructor declaration.
       \param proj_decl output parameter that will contain the projection function declarations. This field must be a buffer of size \c num_fields allocated by the user. -}
foreign import ccall unsafe "Z3_mk_tuple_sort"
  z3_mk_tuple_sort :: <Z3_context> -> <Z3_symbol> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> IO <Z3_sort>

{- | Create a enumeration sort.

       An enumeration sort with \c n elements.
       This function will also declare the functions corresponding to the enumerations.

       \param c logical context
       \param name name of the enumeration sort.
       \param n number of elemenets in enumeration sort.
       \param enum_names names of the enumerated elements.
       \param enum_consts constants corresponding to the enumerated elements.
       \param enum_testers predicates testing if terms of the enumeration sort correspond to an enumeration.

       For example, if this function is called with three symbols A, B, C and the name S, then
       \c s is a sort whose name is S, and the function returns three terms corresponding to A, B, C in
       \c enum_consts. The array \c enum_testers has three predicates of type \ccode{(s -> Bool)}.
       The first predicate (corresponding to A) is true when applied to A, and false otherwise.
       Similarly for the other predicates. -}
foreign import ccall unsafe "Z3_mk_enumeration_sort"
  z3_mk_enumeration_sort :: <Z3_context> -> <Z3_symbol> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> IO <Z3_sort>

{- | Create a list sort

       A list sort over \c elem_sort
       This function declares the corresponding constructors and testers for lists.

       \param c logical context
       \param name name of the list sort.
       \param elem_sort sort of list elements.
       \param nil_decl declaration for the empty list.
       \param is_nil_decl test for the empty list.
       \param cons_decl declaration for a cons cell.
       \param is_cons_decl cons cell test.
       \param head_decl list head.
       \param tail_decl list tail. -}
foreign import ccall unsafe "Z3_mk_list_sort"
  z3_mk_list_sort :: <Z3_context> -> <Z3_symbol> -> <Z3_sort> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> IO <Z3_sort>

{- | Create a constructor.

       \param c logical context.
       \param name constructor name.
       \param recognizer name of recognizer function.
       \param num_fields number of fields in constructor.
       \param field_names names of the constructor fields.
       \param sorts field sorts, 0 if the field sort refers to a recursive sort.
       \param sort_refs reference to datatype sort that is an argument to the constructor; if the corresponding
                        sort reference is 0, then the value in sort_refs should be an index referring to
                        one of the recursive datatypes that is declared. -}
foreign import ccall unsafe "Z3_mk_constructor"
  z3_mk_constructor :: <Z3_context> -> <Z3_symbol> -> <Z3_symbol> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> Ptr CUInt -> IO <Z3_constructor>

{- | Reclaim memory allocated to constructor.

       \param c logical context.
       \param constr constructor. -}
foreign import ccall unsafe "Z3_del_constructor"
  z3_del_constructor :: <Z3_context> -> <Z3_constructor> -> IO ()

{- | Create datatype, such as lists, trees, records, enumerations or unions of records.
       The datatype may be recursive. Return the datatype sort.

       \param c logical context.
	   \param name name of datatype.
       \param num_constructors number of constructors passed in.
       \param constructors array of constructor containers. -}
foreign import ccall unsafe "Z3_mk_datatype"
  z3_mk_datatype :: <Z3_context> -> <Z3_symbol> -> CUInt -> Ptr <Z3_constructor> -> IO <Z3_sort>

{- | Create list of constructors.

       \param c logical context.
       \param num_constructors number of constructors in list.
       \param constructors list of constructors. -}
foreign import ccall unsafe "Z3_mk_constructor_list"
  z3_mk_constructor_list :: <Z3_context> -> CUInt -> Ptr <Z3_constructor> -> IO <Z3_constructor_list>

{- | Reclaim memory allocated for constructor list.

       Each constructor inside the constructor list must be independently reclaimed using #Z3_del_constructor.

       \param c logical context.
       \param clist constructor list container. -}
foreign import ccall unsafe "Z3_del_constructor_list"
  z3_del_constructor_list :: <Z3_context> -> <Z3_constructor_list> -> IO ()

{- | Create mutually recursive datatypes.

       \param c logical context.
       \param num_sorts number of datatype sorts.
       \param sort_names names of datatype sorts.
       \param sorts array of datatype sorts.
       \param constructor_lists list of constructors, one list per sort. -}
foreign import ccall unsafe "Z3_mk_datatypes"
  z3_mk_datatypes :: <Z3_context> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> Ptr <Z3_constructor_list> -> IO ()

{- | Query constructor for declared functions.

       \param c logical context.
       \param constr constructor container. The container must have been passed in to a #Z3_mk_datatype call.
       \param num_fields number of accessor fields in the constructor.
       \param constructor constructor function declaration, allocated by user.
       \param tester constructor test function declaration, allocated by user.
       \param accessors array of accessor function declarations allocated by user. The array must contain num_fields elements. -}
foreign import ccall unsafe "Z3_query_constructor"
  z3_query_constructor :: <Z3_context> -> <Z3_constructor> -> CUInt -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> IO ()

{- | Declare a constant or function.

       \param c logical context.
       \param s name of the constant or function.
       \param domain_size number of arguments. It is 0 when declaring a constant.
       \param domain array containing the sort of each argument. The array must contain domain_size elements. It is 0 when declaring a constant.
       \param range sort of the constant or the return sort of the function.

       After declaring a constant or function, the function
       #Z3_mk_app can be used to create a constant or function
       application.

       \sa Z3_mk_app -}
foreign import ccall unsafe "Z3_mk_func_decl"
  z3_mk_func_decl :: <Z3_context> -> <Z3_symbol> -> CUInt -> Ptr <Z3_sort> -> <Z3_sort> -> IO <Z3_func_decl>

{- | Create a constant or function application.

       \sa Z3_mk_func_decl -}
foreign import ccall unsafe "Z3_mk_app"
  z3_mk_app :: <Z3_context> -> <Z3_func_decl> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Declare and create a constant.

       This function is a shorthand for:
       \code
       Z3_func_decl d = Z3_mk_func_decl(c, s, 0, 0, ty);
       Z3_ast n            = Z3_mk_app(c, d, 0, 0);
       \endcode

       \sa Z3_mk_func_decl
       \sa Z3_mk_app -}
foreign import ccall unsafe "Z3_mk_const"
  z3_mk_const :: <Z3_context> -> <Z3_symbol> -> <Z3_sort> -> IO <Z3_ast>

{- | Declare a fresh constant or function.

       Z3 will generate an unique name for this function declaration.
       If prefix is different from \c NULL, then the name generate by Z3 will start with \c prefix.

       \remark If \c prefix is \c NULL, then it is assumed to be the empty string.

       \sa Z3_mk_func_decl -}
foreign import ccall unsafe "Z3_mk_fresh_func_decl"
  z3_mk_fresh_func_decl :: <Z3_context> -> <Z3_string> -> CUInt -> Ptr <Z3_sort> -> <Z3_sort> -> IO <Z3_func_decl>

{- | Declare and create a fresh constant.

       This function is a shorthand for:
       \code Z3_func_decl d = Z3_mk_fresh_func_decl(c, prefix, 0, 0, ty); Z3_ast n = Z3_mk_app(c, d, 0, 0); \endcode

       \remark If \c prefix is \c NULL, then it is assumed to be the empty string.

       \sa Z3_mk_func_decl
       \sa Z3_mk_app -}
foreign import ccall unsafe "Z3_mk_fresh_const"
  z3_mk_fresh_const :: <Z3_context> -> <Z3_string> -> <Z3_sort> -> IO <Z3_ast>

{- | Create an AST node representing \c true. -}
foreign import ccall unsafe "Z3_mk_true"
  z3_mk_true :: <Z3_context> -> IO <Z3_ast>

{- | Create an AST node representing \c false. -}
foreign import ccall unsafe "Z3_mk_false"
  z3_mk_false :: <Z3_context> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{l = r}.

        The nodes \c l and \c r must have the same type. -}
foreign import ccall unsafe "Z3_mk_eq"
  z3_mk_eq :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{distinct(args[0], ..., args[num_args-1])}.

       The \c distinct construct is used for declaring the arguments pairwise distinct.
       That is, \ccode{Forall 0 <= i < j < num_args. not args[i] = args[j]}.

       All arguments must have the same sort.

       \remark The number of arguments of a distinct construct must be greater than one. -}
foreign import ccall unsafe "Z3_mk_distinct"
  z3_mk_distinct :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{not(a)}.

        The node \c a must have Boolean sort. -}
foreign import ccall unsafe "Z3_mk_not"
  z3_mk_not :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing an if-then-else: \ccode{ite(t1, t2, t3)}.

       The node \c t1 must have Boolean sort, \c t2 and \c t3 must have the same sort.
       The sort of the new node is equal to the sort of \c t2 and \c t3. -}
foreign import ccall unsafe "Z3_mk_ite"
  z3_mk_ite :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{t1 iff t2}.

       The nodes \c t1 and \c t2 must have Boolean sort. -}
foreign import ccall unsafe "Z3_mk_iff"
  z3_mk_iff :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{t1 implies t2}.

       The nodes \c t1 and \c t2 must have Boolean sort. -}
foreign import ccall unsafe "Z3_mk_implies"
  z3_mk_implies :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{t1 xor t2}.

       The nodes \c t1 and \c t2 must have Boolean sort. -}
foreign import ccall unsafe "Z3_mk_xor"
  z3_mk_xor :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{args[0] and ... and args[num_args-1]}.

       The array \c args must have \c num_args elements.
       All arguments must have Boolean sort.

       \remark The number of arguments must be greater than zero. -}
foreign import ccall unsafe "Z3_mk_and"
  z3_mk_and :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{args[0] or ... or args[num_args-1]}.

       The array \c args must have \c num_args elements.
       All arguments must have Boolean sort.

       \remark The number of arguments must be greater than zero. -}
foreign import ccall unsafe "Z3_mk_or"
  z3_mk_or :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{args[0] + ... + args[num_args-1]}.

       The array \c args must have \c num_args elements.
       All arguments must have int or real sort.

       \remark The number of arguments must be greater than zero. -}
foreign import ccall unsafe "Z3_mk_add"
  z3_mk_add :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{args[0] * ... * args[num_args-1]}.

       The array \c args must have \c num_args elements.
       All arguments must have int or real sort.

       \remark Z3 has limited support for non-linear arithmetic.
       \remark The number of arguments must be greater than zero. -}
foreign import ccall unsafe "Z3_mk_mul"
  z3_mk_mul :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{args[0] - ... - args[num_args - 1]}.

       The array \c args must have \c num_args elements.
       All arguments must have int or real sort.

       \remark The number of arguments must be greater than zero. -}
foreign import ccall unsafe "Z3_mk_sub"
  z3_mk_sub :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{ - arg}.

       The arguments must have int or real type. -}
foreign import ccall unsafe "Z3_mk_unary_minus"
  z3_mk_unary_minus :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{arg1 div arg2}.

       The arguments must either both have int type or both have real type.
       If the arguments have int type, then the result type is an int type, otherwise the
       the result type is real. -}
foreign import ccall unsafe "Z3_mk_div"
  z3_mk_div :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{arg1 mod arg2}.

       The arguments must have int type. -}
foreign import ccall unsafe "Z3_mk_mod"
  z3_mk_mod :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{arg1 rem arg2}.

       The arguments must have int type. -}
foreign import ccall unsafe "Z3_mk_rem"
  z3_mk_rem :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{arg1 ^ arg2}.

       The arguments must have int or real type. -}
foreign import ccall unsafe "Z3_mk_power"
  z3_mk_power :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create less than.

        The nodes \c t1 and \c t2 must have the same sort, and must be int or real. -}
foreign import ccall unsafe "Z3_mk_lt"
  z3_mk_lt :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create less than or equal to.

        The nodes \c t1 and \c t2 must have the same sort, and must be int or real. -}
foreign import ccall unsafe "Z3_mk_le"
  z3_mk_le :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create greater than.

        The nodes \c t1 and \c t2 must have the same sort, and must be int or real. -}
foreign import ccall unsafe "Z3_mk_gt"
  z3_mk_gt :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create greater than or equal to.

        The nodes \c t1 and \c t2 must have the same sort, and must be int or real. -}
foreign import ccall unsafe "Z3_mk_ge"
  z3_mk_ge :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Coerce an integer to a real.

        There is also a converse operation exposed.
        It follows the semantics prescribed by the SMT-LIB standard.

        You can take the floor of a real by
        creating an auxiliary integer constant \c k and
        and asserting \ccode{mk_int2real(k) <= t1 < mk_int2real(k)+1}.

        The node \c t1 must have sort integer.

        \sa Z3_mk_real2int
        \sa Z3_mk_is_int -}
foreign import ccall unsafe "Z3_mk_int2real"
  z3_mk_int2real :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Coerce a real to an integer.

        The semantics of this function follows the SMT-LIB standard
        for the function to_int

        \sa Z3_mk_int2real
        \sa Z3_mk_is_int -}
foreign import ccall unsafe "Z3_mk_real2int"
  z3_mk_real2int :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Check if a real number is an integer.

        \sa Z3_mk_int2real
        \sa Z3_mk_real2int -}
foreign import ccall unsafe "Z3_mk_is_int"
  z3_mk_is_int :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Bitwise negation.

       The node \c t1 must have a bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvnot"
  z3_mk_bvnot :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Take conjunction of bits in vector, return vector of length 1.

       The node \c t1 must have a bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvredand"
  z3_mk_bvredand :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Take disjunction of bits in vector, return vector of length 1.

       The node \c t1 must have a bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvredor"
  z3_mk_bvredor :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Bitwise and.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvand"
  z3_mk_bvand :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Bitwise or.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvor"
  z3_mk_bvor :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Bitwise exclusive-or.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvxor"
  z3_mk_bvxor :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Bitwise nand.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvnand"
  z3_mk_bvnand :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Bitwise nor.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvnor"
  z3_mk_bvnor :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Bitwise xnor.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvxnor"
  z3_mk_bvxnor :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Standard two's complement unary minus.

       The node \c t1 must have bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvneg"
  z3_mk_bvneg :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Standard two's complement addition.

        The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvadd"
  z3_mk_bvadd :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Standard two's complement subtraction.

        The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvsub"
  z3_mk_bvsub :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Standard two's complement multiplication.

        The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvmul"
  z3_mk_bvmul :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Unsigned division.

        It is defined as the \c floor of \ccode{t1/t2} if \c t2 is
        different from zero. If \ccode{t2} is zero, then the result
        is undefined.

        The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvudiv"
  z3_mk_bvudiv :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Two's complement signed division.

        It is defined in the following way:

        - The \c floor of \ccode{t1/t2} if \c t2 is different from zero, and \ccode{t1*t2 >= 0}.

        - The \c ceiling of \ccode{t1/t2} if \c t2 is different from zero, and \ccode{t1*t2 < 0}.

        If \ccode{t2} is zero, then the result is undefined.

        The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvsdiv"
  z3_mk_bvsdiv :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Unsigned remainder.

       It is defined as \ccode{t1 - (t1 /u t2) * t2}, where \ccode{/u} represents unsigned division.

       If \ccode{t2} is zero, then the result is undefined.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvurem"
  z3_mk_bvurem :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Two's complement signed remainder (sign follows dividend).

       It is defined as \ccode{t1 - (t1 /s t2) * t2}, where \ccode{/s} represents signed division.
       The most significant bit (sign) of the result is equal to the most significant bit of \c t1.

       If \ccode{t2} is zero, then the result is undefined.

       The nodes \c t1 and \c t2 must have the same bit-vector sort.

       \sa Z3_mk_bvsmod -}
foreign import ccall unsafe "Z3_mk_bvsrem"
  z3_mk_bvsrem :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Two's complement signed remainder (sign follows divisor).

       If \ccode{t2} is zero, then the result is undefined.

       The nodes \c t1 and \c t2 must have the same bit-vector sort.

       \sa Z3_mk_bvsrem -}
foreign import ccall unsafe "Z3_mk_bvsmod"
  z3_mk_bvsmod :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Unsigned less than.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvult"
  z3_mk_bvult :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Two's complement signed less than.

       It abbreviates:
       \code
        (or (and (= (extract[|m-1|:|m-1|] t1) bit1)
                (= (extract[|m-1|:|m-1|] t2) bit0))
            (and (= (extract[|m-1|:|m-1|] t1) (extract[|m-1|:|m-1|] t2))
                (bvult t1 t2)))
       \endcode

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvslt"
  z3_mk_bvslt :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Unsigned less than or equal to.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvule"
  z3_mk_bvule :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Two's complement signed less than or equal to.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvsle"
  z3_mk_bvsle :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Unsigned greater than or equal to.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvuge"
  z3_mk_bvuge :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Two's complement signed greater than or equal to.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvsge"
  z3_mk_bvsge :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Unsigned greater than.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvugt"
  z3_mk_bvugt :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Two's complement signed greater than.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvsgt"
  z3_mk_bvsgt :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Concatenate the given bit-vectors.

       The nodes \c t1 and \c t2 must have (possibly different) bit-vector sorts

       The result is a bit-vector of size \ccode{n1+n2}, where \c n1 (\c n2) is the size
       of \c t1 (\c t2). -}
foreign import ccall unsafe "Z3_mk_concat"
  z3_mk_concat :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Extract the bits \c high down to \c low from a bit-vector of
       size \c m to yield a new bit-vector of size \c n, where \ccode{n = high - low + 1}.

       The node \c t1 must have a bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_extract"
  z3_mk_extract :: <Z3_context> -> CUInt -> CUInt -> <Z3_ast> -> IO <Z3_ast>

{- | Sign-extend of the given bit-vector to the (signed) equivalent bit-vector of
       size \ccode{m+i}, where \c m is the size of the given
       bit-vector.

       The node \c t1 must have a bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_sign_ext"
  z3_mk_sign_ext :: <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>

{- | Extend the given bit-vector with zeros to the (unsigned) equivalent
       bit-vector of size \ccode{m+i}, where \c m is the size of the
       given bit-vector.

       The node \c t1 must have a bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_zero_ext"
  z3_mk_zero_ext :: <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>

{- | Repeat the given bit-vector up length \ccode{i}.

       The node \c t1 must have a bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_repeat"
  z3_mk_repeat :: <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>

{- | Shift left.

       It is equivalent to multiplication by \ccode{2^x} where \c x is the value of the
       third argument.

       NB. The semantics of shift operations varies between environments. This
       definition does not necessarily capture directly the semantics of the
       programming language or assembly architecture you are modeling.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvshl"
  z3_mk_bvshl :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Logical shift right.

       It is equivalent to unsigned division by \ccode{2^x} where \c x is the
       value of the third argument.

       NB. The semantics of shift operations varies between environments. This
       definition does not necessarily capture directly the semantics of the
       programming language or assembly architecture you are modeling.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvlshr"
  z3_mk_bvlshr :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Arithmetic shift right.

       It is like logical shift right except that the most significant
       bits of the result always copy the most significant bit of the
       second argument.

       The semantics of shift operations varies between environments. This
       definition does not necessarily capture directly the semantics of the
       programming language or assembly architecture you are modeling.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvashr"
  z3_mk_bvashr :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Rotate bits of \c t1 to the left \c i times.

       The node \c t1 must have a bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_rotate_left"
  z3_mk_rotate_left :: <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>

{- | Rotate bits of \c t1 to the right \c i times.

       The node \c t1 must have a bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_rotate_right"
  z3_mk_rotate_right :: <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>

{- | Rotate bits of \c t1 to the left \c t2 times.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_ext_rotate_left"
  z3_mk_ext_rotate_left :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Rotate bits of \c t1 to the right \c t2 times.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_ext_rotate_right"
  z3_mk_ext_rotate_right :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an \c n bit bit-vector from the integer argument \c t1.

       NB. This function is essentially treated as uninterpreted.
       So you cannot expect Z3 to precisely reflect the semantics of this function
       when solving constraints with this function.

       The node \c t1 must have integer sort. -}
foreign import ccall unsafe "Z3_mk_int2bv"
  z3_mk_int2bv :: <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>

{- | Create an integer from the bit-vector argument \c t1.
       If \c is_signed is false, then the bit-vector \c t1 is treated as unsigned.
       So the result is non-negative
       and in the range \ccode{[0..2^N-1]}, where N are the number of bits in \c t1.
       If \c is_signed is true, \c t1 is treated as a signed bit-vector.

       This function is essentially treated as uninterpreted.
       So you cannot expect Z3 to precisely reflect the semantics of this function
       when solving constraints with this function.

       The node \c t1 must have a bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bv2int"
  z3_mk_bv2int :: <Z3_context> -> <Z3_ast> -> CInt -> IO <Z3_ast>

{- | Create a predicate that checks that the bit-wise addition
       of \c t1 and \c t2 does not overflow.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvadd_no_overflow"
  z3_mk_bvadd_no_overflow :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> CInt -> IO <Z3_ast>

{- | Create a predicate that checks that the bit-wise signed addition
       of \c t1 and \c t2 does not underflow.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvadd_no_underflow"
  z3_mk_bvadd_no_underflow :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a predicate that checks that the bit-wise signed subtraction
       of \c t1 and \c t2 does not overflow.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvsub_no_overflow"
  z3_mk_bvsub_no_overflow :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a predicate that checks that the bit-wise subtraction
       of \c t1 and \c t2 does not underflow.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvsub_no_underflow"
  z3_mk_bvsub_no_underflow :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> CInt -> IO <Z3_ast>

{- | Create a predicate that checks that the bit-wise signed division
       of \c t1 and \c t2 does not overflow.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvsdiv_no_overflow"
  z3_mk_bvsdiv_no_overflow :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Check that bit-wise negation does not overflow when
       \c t1 is interpreted as a signed bit-vector.

       The node \c t1 must have bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvneg_no_overflow"
  z3_mk_bvneg_no_overflow :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a predicate that checks that the bit-wise multiplication
       of \c t1 and \c t2 does not overflow.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvmul_no_overflow"
  z3_mk_bvmul_no_overflow :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> CInt -> IO <Z3_ast>

{- | Create a predicate that checks that the bit-wise signed multiplication
       of \c t1 and \c t2 does not underflow.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_bvmul_no_underflow"
  z3_mk_bvmul_no_underflow :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Array read.
       The argument \c a is the array and \c i is the index of the array that gets read.

       The node \c a must have an array sort \ccode{[domain -> range]},
       and \c i must have the sort \c domain.
       The sort of the result is \c range.

       \sa Z3_mk_array_sort
       \sa Z3_mk_store -}
foreign import ccall unsafe "Z3_mk_select"
  z3_mk_select :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Array update.

       The node \c a must have an array sort \ccode{[domain -> range]}, \c i must have sort \c domain,
       \c v must have sort range. The sort of the result is \ccode{[domain -> range]}.
       The semantics of this function is given by the theory of arrays described in the SMT-LIB
       standard. See http://smtlib.org for more details.
       The result of this function is an array that is equal to \c a (with respect to \c select)
       on all indices except for \c i, where it maps to \c v (and the \c select of \c a with
       respect to \c i may be a different value).

       \sa Z3_mk_array_sort
       \sa Z3_mk_select -}
foreign import ccall unsafe "Z3_mk_store"
  z3_mk_store :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create the constant array.

        The resulting term is an array, such that a \c select on an arbitrary index
        produces the value \c v.

        \param c logical context.
        \param domain domain sort for the array.
        \param v value that the array maps to. -}
foreign import ccall unsafe "Z3_mk_const_array"
  z3_mk_const_array :: <Z3_context> -> <Z3_sort> -> <Z3_ast> -> IO <Z3_ast>

{- | Map f on the argument arrays.

       The \c n nodes \c args must be of array sorts \ccode{[domain_i -> range_i]}.
       The function declaration \c f must have type \ccode{ range_1 .. range_n -> range}.
       \c v must have sort range. The sort of the result is \ccode{[domain_i -> range]}.

       \sa Z3_mk_array_sort
       \sa Z3_mk_store
       \sa Z3_mk_select -}
foreign import ccall unsafe "Z3_mk_map"
  z3_mk_map :: <Z3_context> -> <Z3_func_decl> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Access the array default value.
        Produces the default range value, for arrays that can be represented as
        finite maps with a default range value.

        \param c logical context.
        \param array array value whose default range value is accessed. -}
foreign import ccall unsafe "Z3_mk_array_default"
  z3_mk_array_default :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Create Set type. -}
foreign import ccall unsafe "Z3_mk_set_sort"
  z3_mk_set_sort :: <Z3_context> -> <Z3_sort> -> IO <Z3_sort>

{- | Create the empty set. -}
foreign import ccall unsafe "Z3_mk_empty_set"
  z3_mk_empty_set :: <Z3_context> -> <Z3_sort> -> IO <Z3_ast>

{- | Create the full set. -}
foreign import ccall unsafe "Z3_mk_full_set"
  z3_mk_full_set :: <Z3_context> -> <Z3_sort> -> IO <Z3_ast>

{- | Add an element to a set.

       The first argument must be a set, the second an element. -}
foreign import ccall unsafe "Z3_mk_set_add"
  z3_mk_set_add :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Remove an element to a set.

       The first argument must be a set, the second an element. -}
foreign import ccall unsafe "Z3_mk_set_del"
  z3_mk_set_del :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Take the union of a list of sets. -}
foreign import ccall unsafe "Z3_mk_set_union"
  z3_mk_set_union :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Take the intersection of a list of sets. -}
foreign import ccall unsafe "Z3_mk_set_intersect"
  z3_mk_set_intersect :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Take the set difference between two sets. -}
foreign import ccall unsafe "Z3_mk_set_difference"
  z3_mk_set_difference :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Take the complement of a set. -}
foreign import ccall unsafe "Z3_mk_set_complement"
  z3_mk_set_complement :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Check for set membership.

       The first argument should be an element type of the set. -}
foreign import ccall unsafe "Z3_mk_set_member"
  z3_mk_set_member :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Check for subsetness of sets. -}
foreign import ccall unsafe "Z3_mk_set_subset"
  z3_mk_set_subset :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create array extensionality index given two arrays with the same sort.
              The meaning is given by the axiom:
              (=> (= (select A (array-ext A B)) (select B (array-ext A B))) (= A B)) -}
foreign import ccall unsafe "Z3_mk_array_ext"
  z3_mk_array_ext :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a numeral of a given sort.

       \param c logical context.
       \param numeral A string representing the numeral value in decimal notation. The string may be of the form \code{[num]*[.[num]*][E[+|-][num]+]}.
                      If the given sort is a real, then the numeral can be a rational, that is, a string of the form \ccode{[num]* / [num]*}.                      
       \param ty The sort of the numeral. In the current implementation, the given sort can be an int, real, finite-domain, or bit-vectors of arbitrary size.

       \sa Z3_mk_int
       \sa Z3_mk_unsigned_int -}
foreign import ccall unsafe "Z3_mk_numeral"
  z3_mk_numeral :: <Z3_context> -> <Z3_string> -> <Z3_sort> -> IO <Z3_ast>

{- | Create a real from a fraction.

       \param c logical context.
       \param num numerator of rational.
       \param den denomerator of rational.

       \pre den != 0

       \sa Z3_mk_numeral
       \sa Z3_mk_int
       \sa Z3_mk_unsigned_int -}
foreign import ccall unsafe "Z3_mk_real"
  z3_mk_real :: <Z3_context> -> CInt -> CInt -> IO <Z3_ast>

{- | Create a numeral of an int, bit-vector, or finite-domain sort.

       This function can be use to create numerals that fit in a machine integer.
       It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string.

       \sa Z3_mk_numeral -}
foreign import ccall unsafe "Z3_mk_int"
  z3_mk_int :: <Z3_context> -> CInt -> <Z3_sort> -> IO <Z3_ast>

{- | Create a numeral of a int, bit-vector, or finite-domain sort.

       This function can be use to create numerals that fit in a machine unsinged integer.
       It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string.

       \sa Z3_mk_numeral -}
foreign import ccall unsafe "Z3_mk_unsigned_int"
  z3_mk_unsigned_int :: <Z3_context> -> CUInt -> <Z3_sort> -> IO <Z3_ast>

{- | Create a numeral of a int, bit-vector, or finite-domain sort.

       This function can be use to create numerals that fit in a machine __int64 integer.
       It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string.

       \sa Z3_mk_numeral -}
foreign import ccall unsafe "Z3_mk_int64"
  z3_mk_int64 :: <Z3_context> -> CLong -> <Z3_sort> -> IO <Z3_ast>

{- | Create a numeral of a int, bit-vector, or finite-domain sort.

       This function can be use to create numerals that fit in a machine unsigned __int64 integer.
       It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string.

       \sa Z3_mk_numeral -}
foreign import ccall unsafe "Z3_mk_unsigned_int64"
  z3_mk_unsigned_int64 :: <Z3_context> -> CULong -> <Z3_sort> -> IO <Z3_ast>

{- | Create a sequence sort out of the sort for the elements. -}
foreign import ccall unsafe "Z3_mk_seq_sort"
  z3_mk_seq_sort :: <Z3_context> -> <Z3_sort> -> IO <Z3_sort>

{- | Check if \c s is a sequence sort. -}
foreign import ccall unsafe "Z3_is_seq_sort"
  z3_is_seq_sort :: <Z3_context> -> <Z3_sort> -> IO CInt

{- | Create a regular expression sort out of a sequence sort. -}
foreign import ccall unsafe "Z3_mk_re_sort"
  z3_mk_re_sort :: <Z3_context> -> <Z3_sort> -> IO <Z3_sort>

{- | Check if \c s is a regular expression sort. -}
foreign import ccall unsafe "Z3_is_re_sort"
  z3_is_re_sort :: <Z3_context> -> <Z3_sort> -> IO CInt

{- | Create a sort for 8 bit strings.

       This function creates a sort for ASCII strings.
       Each character is 8 bits. -}
foreign import ccall unsafe "Z3_mk_string_sort"
  z3_mk_string_sort :: <Z3_context> -> IO <Z3_sort>

{- | Check if \c s is a string sort. -}
foreign import ccall unsafe "Z3_is_string_sort"
  z3_is_string_sort :: <Z3_context> -> <Z3_sort> -> IO CInt

{- | Create a string constant out of the string that is passed in -}
foreign import ccall unsafe "Z3_mk_string"
  z3_mk_string :: <Z3_context> -> <Z3_string> -> IO <Z3_ast>

{- | Determine if \c s is a string constant. -}
foreign import ccall unsafe "Z3_is_string"
  z3_is_string :: <Z3_context> -> <Z3_ast> -> IO CInt

{- | Retrieve the string constant stored in \c s.

       \pre  Z3_is_string(c, s) -}
foreign import ccall unsafe "Z3_get_string"
  z3_get_string :: <Z3_context> -> <Z3_ast> -> IO <Z3_string>

{- | Create an empty sequence of the sequence sort \c seq.

       \pre s is a sequence sort. -}
foreign import ccall unsafe "Z3_mk_seq_empty"
  z3_mk_seq_empty :: <Z3_context> -> <Z3_sort> -> IO <Z3_ast>

{- | Create a unit sequence of \c a. -}
foreign import ccall unsafe "Z3_mk_seq_unit"
  z3_mk_seq_unit :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Concatenate sequences.

       \pre n > 0 -}
foreign import ccall unsafe "Z3_mk_seq_concat"
  z3_mk_seq_concat :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Check if \c prefix is a prefix of \c s.

       \pre prefix and s are the same sequence sorts. -}
foreign import ccall unsafe "Z3_mk_seq_prefix"
  z3_mk_seq_prefix :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Check if \c suffix is a suffix of \c s.

       \pre \c suffix and \c s are the same sequence sorts. -}
foreign import ccall unsafe "Z3_mk_seq_suffix"
  z3_mk_seq_suffix :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Check if \c container contains \c containee.

       \pre \c container and \c containee are the same sequence sorts. -}
foreign import ccall unsafe "Z3_mk_seq_contains"
  z3_mk_seq_contains :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Extract subsequence starting at \c offset of \c length. -}
foreign import ccall unsafe "Z3_mk_seq_extract"
  z3_mk_seq_extract :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Replace the first occurrence of \c src with \c dst in \c s. -}
foreign import ccall unsafe "Z3_mk_seq_replace"
  z3_mk_seq_replace :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Retrieve from \s the unit sequence positioned at position \c index. -}
foreign import ccall unsafe "Z3_mk_seq_at"
  z3_mk_seq_at :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Return the length of the sequence \c s. -}
foreign import ccall unsafe "Z3_mk_seq_length"
  z3_mk_seq_length :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Return index of first occurrence of \c substr in \c s starting from offset \c offset.
       If \c s does not contain \c substr, then the value is -1, if \c offset is the length of \c s, then the value is -1 as well.
       The function is under-specified if \c offset is negative or larger than the length of \c s. -}
foreign import ccall unsafe "Z3_mk_seq_index"
  z3_mk_seq_index :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a regular expression that accepts the sequence \c seq. -}
foreign import ccall unsafe "Z3_mk_seq_to_re"
  z3_mk_seq_to_re :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Check if \c seq is in the language generated by the regular expression \c re. -}
foreign import ccall unsafe "Z3_mk_seq_in_re"
  z3_mk_seq_in_re :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create the regular language \c re+. -}
foreign import ccall unsafe "Z3_mk_re_plus"
  z3_mk_re_plus :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Create the regular language \c re*. -}
foreign import ccall unsafe "Z3_mk_re_star"
  z3_mk_re_star :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Create the regular language \c [re]. -}
foreign import ccall unsafe "Z3_mk_re_option"
  z3_mk_re_option :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Create the union of the regular languages.

       \pre n > 0 -}
foreign import ccall unsafe "Z3_mk_re_union"
  z3_mk_re_union :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create the concatenation of the regular languages.

       \pre n > 0 -}
foreign import ccall unsafe "Z3_mk_re_concat"
  z3_mk_re_concat :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create a pattern for quantifier instantiation.

       Z3 uses pattern matching to instantiate quantifiers. If a
       pattern is not provided for a quantifier, then Z3 will
       automatically compute a set of patterns for it. However, for
       optimal performance, the user should provide the patterns.

       Patterns comprise a list of terms. The list should be
       non-empty.  If the list comprises of more than one term, it is
       a called a multi-pattern.

       In general, one can pass in a list of (multi-)patterns in the
       quantifier constructor.

       \sa Z3_mk_forall
       \sa Z3_mk_exists -}
foreign import ccall unsafe "Z3_mk_pattern"
  z3_mk_pattern :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_pattern>

{- | Create a bound variable.

       Bound variables are indexed by de-Bruijn indices. It is perhaps easiest to explain
       the meaning of de-Bruijn indices by indicating the compilation process from
       non-de-Bruijn formulas to de-Bruijn format.

       \verbatim
       abs(forall (x1) phi) = forall (x1) abs1(phi, x1, 0)
       abs(forall (x1, x2) phi) = abs(forall (x1) abs(forall (x2) phi))
       abs1(x, x, n) = b_n
       abs1(y, x, n) = y
       abs1(f(t1,...,tn), x, n) = f(abs1(t1,x,n), ..., abs1(tn,x,n))
       abs1(forall (x1) phi, x, n) = forall (x1) (abs1(phi, x, n+1))
       \endverbatim

       The last line is significant: the index of a bound variable is different depending
       on the scope in which it appears. The deeper x appears, the higher is its
       index.

       \param c logical context
       \param index de-Bruijn index
       \param ty sort of the bound variable

       \sa Z3_mk_forall
       \sa Z3_mk_exists -}
foreign import ccall unsafe "Z3_mk_bound"
  z3_mk_bound :: <Z3_context> -> CUInt -> <Z3_sort> -> IO <Z3_ast>

{- | Create a forall formula. It takes an expression \c body that contains bound variables
       of the same sorts as the sorts listed in the array \c sorts. The bound variables are de-Bruijn indices created
       using #Z3_mk_bound. The array \c decl_names contains the names that the quantified formula uses for the
       bound variables. Z3 applies the convention that the last element in the \c decl_names and \c sorts array
       refers to the variable with index 0, the second to last element of \c decl_names and \c sorts refers
       to the variable with index 1, etc.

       \param c logical context.
       \param weight quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
       \param num_patterns number of patterns.
       \param patterns array containing the patterns created using #Z3_mk_pattern.
       \param num_decls number of variables to be bound.
       \param sorts the sorts of the bound variables.
       \param decl_names names of the bound variables
       \param body the body of the quantifier.

       \sa Z3_mk_pattern
       \sa Z3_mk_bound
       \sa Z3_mk_exists -}
foreign import ccall unsafe "Z3_mk_forall"
  z3_mk_forall :: <Z3_context> -> CUInt -> CUInt -> Ptr <Z3_pattern> -> CUInt -> Ptr <Z3_sort> -> Ptr <Z3_symbol> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an exists formula. Similar to #Z3_mk_forall.

       \sa Z3_mk_pattern
       \sa Z3_mk_bound
       \sa Z3_mk_forall
       \sa Z3_mk_quantifier -}
foreign import ccall unsafe "Z3_mk_exists"
  z3_mk_exists :: <Z3_context> -> CUInt -> CUInt -> Ptr <Z3_pattern> -> CUInt -> Ptr <Z3_sort> -> Ptr <Z3_symbol> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a quantifier - universal or existential, with pattern hints.
       See the documentation for #Z3_mk_forall for an explanation of the parameters.

       \param c logical context.
       \param is_forall flag to indicate if this is a universal or existential quantifier.
       \param weight quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
       \param num_patterns number of patterns.
       \param patterns array containing the patterns created using #Z3_mk_pattern.
       \param num_decls number of variables to be bound.
       \param sorts array of sorts of the bound variables.
       \param decl_names names of the bound variables.
       \param body the body of the quantifier.

       \sa Z3_mk_pattern
       \sa Z3_mk_bound
       \sa Z3_mk_forall
       \sa Z3_mk_exists -}
foreign import ccall unsafe "Z3_mk_quantifier"
  z3_mk_quantifier :: <Z3_context> -> CInt -> CUInt -> CUInt -> Ptr <Z3_pattern> -> CUInt -> Ptr <Z3_sort> -> Ptr <Z3_symbol> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a quantifier - universal or existential, with pattern hints, no patterns, and attributes

       \param c logical context.
       \param is_forall flag to indicate if this is a universal or existential quantifier.
       \param quantifier_id identifier to identify quantifier
       \param skolem_id identifier to identify skolem constants introduced by quantifier.
       \param weight quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
       \param num_patterns number of patterns.
       \param patterns array containing the patterns created using #Z3_mk_pattern.
       \param num_no_patterns number of no_patterns.
       \param no_patterns array containing subexpressions to be excluded from inferred patterns.
       \param num_decls number of variables to be bound.
       \param sorts array of sorts of the bound variables.
       \param decl_names names of the bound variables.
       \param body the body of the quantifier.

       \sa Z3_mk_pattern
       \sa Z3_mk_bound
       \sa Z3_mk_forall
       \sa Z3_mk_exists -}
foreign import ccall unsafe "Z3_mk_quantifier_ex"
  z3_mk_quantifier_ex :: <Z3_context> -> CInt -> CUInt -> <Z3_symbol> -> <Z3_symbol> -> CUInt -> Ptr <Z3_pattern> -> CUInt -> Ptr <Z3_ast> -> CUInt -> Ptr <Z3_sort> -> Ptr <Z3_symbol> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a universal quantifier using a list of constants that
       will form the set of bound variables.

       \param c logical context.
       \param weight quantifiers are associated with weights indicating the importance of using
              the quantifier during instantiation. By default, pass the weight 0.
       \param num_bound number of constants to be abstracted into bound variables.
       \param bound array of constants to be abstracted into bound variables.
       \param num_patterns number of patterns.
       \param patterns array containing the patterns created using #Z3_mk_pattern.
       \param body the body of the quantifier.

       \sa Z3_mk_pattern
       \sa Z3_mk_exists_const -}
foreign import ccall unsafe "Z3_mk_forall_const"
  z3_mk_forall_const :: <Z3_context> -> CUInt -> CUInt -> Ptr <Z3_app> -> CUInt -> Ptr <Z3_pattern> -> <Z3_ast> -> IO <Z3_ast>

{- | Similar to #Z3_mk_forall_const.

       \brief Create an existential quantifier using a list of constants that
       will form the set of bound variables.

       \param c logical context.
       \param weight quantifiers are associated with weights indicating the importance of using
              the quantifier during instantiation. By default, pass the weight 0.
       \param num_bound number of constants to be abstracted into bound variables.
       \param bound array of constants to be abstracted into bound variables.
       \param num_patterns number of patterns.
       \param patterns array containing the patterns created using #Z3_mk_pattern.
       \param body the body of the quantifier.

       \sa Z3_mk_pattern
       \sa Z3_mk_forall_const -}
foreign import ccall unsafe "Z3_mk_exists_const"
  z3_mk_exists_const :: <Z3_context> -> CUInt -> CUInt -> Ptr <Z3_app> -> CUInt -> Ptr <Z3_pattern> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a universal or existential quantifier using a list of
       constants that will form the set of bound variables. -}
foreign import ccall unsafe "Z3_mk_quantifier_const"
  z3_mk_quantifier_const :: <Z3_context> -> CInt -> CUInt -> CUInt -> Ptr <Z3_app> -> CUInt -> Ptr <Z3_pattern> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a universal or existential quantifier using a list of
       constants that will form the set of bound variables. -}
foreign import ccall unsafe "Z3_mk_quantifier_const_ex"
  z3_mk_quantifier_const_ex :: <Z3_context> -> CInt -> CUInt -> <Z3_symbol> -> <Z3_symbol> -> CUInt -> Ptr <Z3_app> -> CUInt -> Ptr <Z3_pattern> -> CUInt -> Ptr <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Return \c Z3_INT_SYMBOL if the symbol was constructed
       using #Z3_mk_int_symbol, and \c Z3_STRING_SYMBOL if the symbol
       was constructed using #Z3_mk_string_symbol. -}
foreign import ccall unsafe "Z3_get_symbol_kind"
  z3_get_symbol_kind :: <Z3_context> -> <Z3_symbol> -> IO <Z3_symbol_kind>

{- | Return the symbol int value.

       \pre Z3_get_symbol_kind(s) == Z3_INT_SYMBOL

       \sa Z3_mk_int_symbol -}
foreign import ccall unsafe "Z3_get_symbol_int"
  z3_get_symbol_int :: <Z3_context> -> <Z3_symbol> -> IO CInt

{- | Return the symbol name.

       \pre Z3_get_symbol_string(s) == Z3_STRING_SYMBOL

       \warning The returned buffer is statically allocated by Z3. It will
       be automatically deallocated when #Z3_del_context is invoked.
       So, the buffer is invalidated in the next call to \c Z3_get_symbol_string.

       \sa Z3_mk_string_symbol -}
foreign import ccall unsafe "Z3_get_symbol_string"
  z3_get_symbol_string :: <Z3_context> -> <Z3_symbol> -> IO <Z3_string>

{- | Return the sort name as a symbol. -}
foreign import ccall unsafe "Z3_get_sort_name"
  z3_get_sort_name :: <Z3_context> -> <Z3_sort> -> IO <Z3_symbol>

{- | Return a unique identifier for \c s. -}
foreign import ccall unsafe "Z3_get_sort_id"
  z3_get_sort_id :: <Z3_context> -> <Z3_sort> -> IO ()

{- | Convert a \c Z3_sort into \c Z3_ast. This is just type casting. -}
foreign import ccall unsafe "Z3_sort_to_ast"
  z3_sort_to_ast :: <Z3_context> -> <Z3_sort> -> IO <Z3_ast>

{- | compare sorts. -}
foreign import ccall unsafe "Z3_is_eq_sort"
  z3_is_eq_sort :: <Z3_context> -> <Z3_sort> -> <Z3_sort> -> IO CInt

{- | Return the sort kind (e.g., array, tuple, int, bool, etc).

       \sa Z3_sort_kind -}
foreign import ccall unsafe "Z3_get_sort_kind"
  z3_get_sort_kind :: <Z3_context> -> <Z3_sort> -> IO <Z3_sort_kind>

{- | Return the size of the given bit-vector sort.

       \pre Z3_get_sort_kind(c, t) == Z3_BV_SORT

       \sa Z3_mk_bv_sort
       \sa Z3_get_sort_kind -}
foreign import ccall unsafe "Z3_get_bv_sort_size"
  z3_get_bv_sort_size :: <Z3_context> -> <Z3_sort> -> IO ()

{- | Store the size of the sort in \c r. Return Z3_FALSE if the call failed.
        That is, Z3_get_sort_kind(s) == Z3_FINITE_DOMAIN_SORT -}
foreign import ccall unsafe "Z3_get_finite_domain_sort_size"
  z3_get_finite_domain_sort_size :: <Z3_context> -> <Z3_sort> -> Ptr CULong -> IO CInt

{- | Return the domain of the given array sort.

       \pre Z3_get_sort_kind(c, t) == Z3_ARRAY_SORT

       \sa Z3_mk_array_sort
       \sa Z3_get_sort_kind -}
foreign import ccall unsafe "Z3_get_array_sort_domain"
  z3_get_array_sort_domain :: <Z3_context> -> <Z3_sort> -> IO <Z3_sort>

{- | Return the range of the given array sort.

       \pre Z3_get_sort_kind(c, t) == Z3_ARRAY_SORT

       \sa Z3_mk_array_sort
       \sa Z3_get_sort_kind -}
foreign import ccall unsafe "Z3_get_array_sort_range"
  z3_get_array_sort_range :: <Z3_context> -> <Z3_sort> -> IO <Z3_sort>

{- | Return the constructor declaration of the given tuple
       sort.

       \pre Z3_get_sort_kind(c, t) == Z3_DATATYPE_SORT

       \sa Z3_mk_tuple_sort
       \sa Z3_get_sort_kind -}
foreign import ccall unsafe "Z3_get_tuple_sort_mk_decl"
  z3_get_tuple_sort_mk_decl :: <Z3_context> -> <Z3_sort> -> IO <Z3_func_decl>

{- | Return the number of fields of the given tuple sort.

       \pre Z3_get_sort_kind(c, t) == Z3_DATATYPE_SORT

       \sa Z3_mk_tuple_sort
       \sa Z3_get_sort_kind -}
foreign import ccall unsafe "Z3_get_tuple_sort_num_fields"
  z3_get_tuple_sort_num_fields :: <Z3_context> -> <Z3_sort> -> IO ()

{- | Return the i-th field declaration (i.e., projection function declaration)
       of the given tuple sort.

       \pre Z3_get_sort_kind(t) == Z3_DATATYPE_SORT
       \pre i < Z3_get_tuple_sort_num_fields(c, t)

       \sa Z3_mk_tuple_sort
       \sa Z3_get_sort_kind -}
foreign import ccall unsafe "Z3_get_tuple_sort_field_decl"
  z3_get_tuple_sort_field_decl :: <Z3_context> -> <Z3_sort> -> CUInt -> IO <Z3_func_decl>

{- | Return number of constructors for datatype.

        \pre Z3_get_sort_kind(t) == Z3_DATATYPE_SORT

        \sa Z3_get_datatype_sort_constructor
        \sa Z3_get_datatype_sort_recognizer
        \sa Z3_get_datatype_sort_constructor_accessor -}
foreign import ccall unsafe "Z3_get_datatype_sort_num_constructors"
  z3_get_datatype_sort_num_constructors :: <Z3_context> -> <Z3_sort> -> IO ()

{- | Return idx'th constructor.

        \pre Z3_get_sort_kind(t) == Z3_DATATYPE_SORT
        \pre idx < Z3_get_datatype_sort_num_constructors(c, t)

        \sa Z3_get_datatype_sort_num_constructors
        \sa Z3_get_datatype_sort_recognizer
        \sa Z3_get_datatype_sort_constructor_accessor -}
foreign import ccall unsafe "Z3_get_datatype_sort_constructor"
  z3_get_datatype_sort_constructor :: <Z3_context> -> <Z3_sort> -> CUInt -> IO <Z3_func_decl>

{- | Return idx'th recognizer.

        \pre Z3_get_sort_kind(t) == Z3_DATATYPE_SORT
        \pre idx < Z3_get_datatype_sort_num_constructors(c, t)

        \sa Z3_get_datatype_sort_num_constructors
        \sa Z3_get_datatype_sort_constructor
        \sa Z3_get_datatype_sort_constructor_accessor -}
foreign import ccall unsafe "Z3_get_datatype_sort_recognizer"
  z3_get_datatype_sort_recognizer :: <Z3_context> -> <Z3_sort> -> CUInt -> IO <Z3_func_decl>

{- | Return idx_a'th accessor for the idx_c'th constructor.

        \pre Z3_get_sort_kind(t) == Z3_DATATYPE_SORT
        \pre idx_c < Z3_get_datatype_sort_num_constructors(c, t)
        \pre idx_a < Z3_get_domain_size(c, Z3_get_datatype_sort_constructor(c, idx_c))

        \sa Z3_get_datatype_sort_num_constructors
        \sa Z3_get_datatype_sort_constructor
        \sa Z3_get_datatype_sort_recognizer -}
foreign import ccall unsafe "Z3_get_datatype_sort_constructor_accessor"
  z3_get_datatype_sort_constructor_accessor :: <Z3_context> -> <Z3_sort> -> CUInt -> CUInt -> IO <Z3_func_decl>

{- | Update record field with a value.

       This corresponds to the 'with' construct in OCaml.
       It has the effect of updating a record field with a given value.
       The remaining fields are left unchanged. It is the record
       equivalent of an array store (see \sa Z3_mk_store).
       If the datatype has more than one constructor, then the update function
       behaves as identity if there is a miss-match between the accessor and
       constructor. For example ((_ update-field car) nil 1) is nil,
       while ((_ update-field car) (cons 2 nil) 1) is (cons 1 nil).


       \pre Z3_get_sort_kind(Z3_get_sort(c, t)) == Z3_get_domain(c, field_access, 1) == Z3_DATATYPE_SORT
       \pre Z3_get_sort(c, value) == Z3_get_range(c, field_access) -}
foreign import ccall unsafe "Z3_datatype_update_field"
  z3_datatype_update_field :: <Z3_context> -> <Z3_func_decl> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Return arity of relation.

        \pre Z3_get_sort_kind(s) == Z3_RELATION_SORT

        \sa Z3_get_relation_column -}
foreign import ccall unsafe "Z3_get_relation_arity"
  z3_get_relation_arity :: <Z3_context> -> <Z3_sort> -> IO ()

{- | Return sort at i'th column of relation sort.

        \pre Z3_get_sort_kind(c, s) == Z3_RELATION_SORT
        \pre col < Z3_get_relation_arity(c, s)

        \sa Z3_get_relation_arity -}
foreign import ccall unsafe "Z3_get_relation_column"
  z3_get_relation_column :: <Z3_context> -> <Z3_sort> -> CUInt -> IO <Z3_sort>

{- | Pseudo-Boolean relations.

       Encode p1 + p2 + ... + pn <= k -}
foreign import ccall unsafe "Z3_mk_atmost"
  z3_mk_atmost :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> CUInt -> IO <Z3_ast>

{- | Pseudo-Boolean relations.

       Encode k1*p1 + k2*p2 + ... + kn*pn <= k -}
foreign import ccall unsafe "Z3_mk_pble"
  z3_mk_pble :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> Ptr CInt -> CInt -> IO <Z3_ast>

{- | Pseudo-Boolean relations.

       Encode k1*p1 + k2*p2 + ... + kn*pn = k -}
foreign import ccall unsafe "Z3_mk_pbeq"
  z3_mk_pbeq :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> Ptr CInt -> CInt -> IO <Z3_ast>

{- | Convert a \c Z3_func_decl into \c Z3_ast. This is just type casting. -}
foreign import ccall unsafe "Z3_func_decl_to_ast"
  z3_func_decl_to_ast :: <Z3_context> -> <Z3_func_decl> -> IO <Z3_ast>

{- | Compare terms. -}
foreign import ccall unsafe "Z3_is_eq_func_decl"
  z3_is_eq_func_decl :: <Z3_context> -> <Z3_func_decl> -> <Z3_func_decl> -> IO CInt

{- | Return a unique identifier for \c f. -}
foreign import ccall unsafe "Z3_get_func_decl_id"
  z3_get_func_decl_id :: <Z3_context> -> <Z3_func_decl> -> IO ()

{- | Return the constant declaration name as a symbol. -}
foreign import ccall unsafe "Z3_get_decl_name"
  z3_get_decl_name :: <Z3_context> -> <Z3_func_decl> -> IO <Z3_symbol>

{- | Return declaration kind corresponding to declaration. -}
foreign import ccall unsafe "Z3_get_decl_kind"
  z3_get_decl_kind :: <Z3_context> -> <Z3_func_decl> -> IO <Z3_decl_kind>

{- | Return the number of parameters of the given declaration.

       \sa Z3_get_arity -}
foreign import ccall unsafe "Z3_get_domain_size"
  z3_get_domain_size :: <Z3_context> -> <Z3_func_decl> -> IO ()

{- | Alias for \c Z3_get_domain_size.

       \sa Z3_get_domain_size -}
foreign import ccall unsafe "Z3_get_arity"
  z3_get_arity :: <Z3_context> -> <Z3_func_decl> -> IO ()

{- | Return the sort of the i-th parameter of the given function declaration.

       \pre i < Z3_get_domain_size(d)

       \sa Z3_get_domain_size -}
foreign import ccall unsafe "Z3_get_domain"
  z3_get_domain :: <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_sort>

{- | Return the range of the given declaration.

       If \c d is a constant (i.e., has zero arguments), then this
       function returns the sort of the constant. -}
foreign import ccall unsafe "Z3_get_range"
  z3_get_range :: <Z3_context> -> <Z3_func_decl> -> IO <Z3_sort>

{- | Return the number of parameters associated with a declaration. -}
foreign import ccall unsafe "Z3_get_decl_num_parameters"
  z3_get_decl_num_parameters :: <Z3_context> -> <Z3_func_decl> -> IO ()

{- | Return the parameter type associated with a declaration.

       \param c the context
       \param d the function declaration
       \param idx is the index of the named parameter it should be between 0 and the number of parameters. -}
foreign import ccall unsafe "Z3_get_decl_parameter_kind"
  z3_get_decl_parameter_kind :: <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_parameter_kind>

{- | Return the integer value associated with an integer parameter.

       \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_INT -}
foreign import ccall unsafe "Z3_get_decl_int_parameter"
  z3_get_decl_int_parameter :: <Z3_context> -> <Z3_func_decl> -> CUInt -> IO CInt

{- | Return the double value associated with an double parameter.

       \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_DOUBLE -}
foreign import ccall unsafe "Z3_get_decl_double_parameter"
  z3_get_decl_double_parameter :: <Z3_context> -> <Z3_func_decl> -> CUInt -> IO CDouble

{- | Return the double value associated with an double parameter.

       \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_SYMBOL -}
foreign import ccall unsafe "Z3_get_decl_symbol_parameter"
  z3_get_decl_symbol_parameter :: <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_symbol>

{- | Return the sort value associated with a sort parameter.

       \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_SORT -}
foreign import ccall unsafe "Z3_get_decl_sort_parameter"
  z3_get_decl_sort_parameter :: <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_sort>

{- | Return the expresson value associated with an expression parameter.

       \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_AST -}
foreign import ccall unsafe "Z3_get_decl_ast_parameter"
  z3_get_decl_ast_parameter :: <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_ast>

{- | Return the expresson value associated with an expression parameter.

       \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_FUNC_DECL -}
foreign import ccall unsafe "Z3_get_decl_func_decl_parameter"
  z3_get_decl_func_decl_parameter :: <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_func_decl>

{- | Return the rational value, as a string, associated with a rational parameter.

       \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_RATIONAL -}
foreign import ccall unsafe "Z3_get_decl_rational_parameter"
  z3_get_decl_rational_parameter :: <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_string>

{- | Convert a \c Z3_app into \c Z3_ast. This is just type casting. -}
foreign import ccall unsafe "Z3_app_to_ast"
  z3_app_to_ast :: <Z3_context> -> <Z3_app> -> IO <Z3_ast>

{- | Return the declaration of a constant or function application. -}
foreign import ccall unsafe "Z3_get_app_decl"
  z3_get_app_decl :: <Z3_context> -> <Z3_app> -> IO <Z3_func_decl>

{- | Return the number of argument of an application. If \c t
       is an constant, then the number of arguments is 0. -}
foreign import ccall unsafe "Z3_get_app_num_args"
  z3_get_app_num_args :: <Z3_context> -> <Z3_app> -> IO ()

{- | Return the i-th argument of the given application.

       \pre i < Z3_get_num_args(c, a) -}
foreign import ccall unsafe "Z3_get_app_arg"
  z3_get_app_arg :: <Z3_context> -> <Z3_app> -> CUInt -> IO <Z3_ast>

{- | Compare terms. -}
foreign import ccall unsafe "Z3_is_eq_ast"
  z3_is_eq_ast :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt

{- | Return a unique identifier for \c t.
        The identifier is unique up to structural equality. Thus, two ast nodes
        created by the same context and having the same children and same function symbols
        have the same identifiers. Ast nodes created in the same context, but having
        different children or different functions have different identifiers.
        Variables and quantifiers are also assigned different identifiers according to
        their structure. -}
foreign import ccall unsafe "Z3_get_ast_id"
  z3_get_ast_id :: <Z3_context> -> <Z3_ast> -> IO ()

{- | Return a hash code for the given AST.
       The hash code is structural. You can use Z3_get_ast_id interchangably with
       this function. -}
foreign import ccall unsafe "Z3_get_ast_hash"
  z3_get_ast_hash :: <Z3_context> -> <Z3_ast> -> IO ()

{- | Return the sort of an AST node.

       The AST node must be a constant, application, numeral, bound variable, or quantifier. -}
foreign import ccall unsafe "Z3_get_sort"
  z3_get_sort :: <Z3_context> -> <Z3_ast> -> IO <Z3_sort>

{- | Return true if the given expression \c t is well sorted. -}
foreign import ccall unsafe "Z3_is_well_sorted"
  z3_is_well_sorted :: <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return Z3_L_TRUE if \c a is true, Z3_L_FALSE if it is false, and Z3_L_UNDEF otherwise. -}
foreign import ccall unsafe "Z3_get_bool_value"
  z3_get_bool_value :: <Z3_context> -> <Z3_ast> -> IO <Z3_lbool>

{- | Return the kind of the given AST. -}
foreign import ccall unsafe "Z3_get_ast_kind"
  z3_get_ast_kind :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast_kind>
foreign import ccall unsafe "Z3_is_app"
  z3_is_app :: <Z3_context> -> <Z3_ast> -> IO CInt
foreign import ccall unsafe "Z3_is_numeral_ast"
  z3_is_numeral_ast :: <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return true if the give AST is a real algebraic number. -}
foreign import ccall unsafe "Z3_is_algebraic_number"
  z3_is_algebraic_number :: <Z3_context> -> <Z3_ast> -> IO CInt

{- | Convert an \c ast into an \c APP_AST. This is just type casting.

       \pre \code Z3_get_ast_kind(c, a) == \c Z3_APP_AST \endcode -}
foreign import ccall unsafe "Z3_to_app"
  z3_to_app :: <Z3_context> -> <Z3_ast> -> IO <Z3_app>

{- | Convert an AST into a FUNC_DECL_AST. This is just type casting.

       \pre \code Z3_get_ast_kind(c, a) == Z3_FUNC_DECL_AST \endcode -}
foreign import ccall unsafe "Z3_to_func_decl"
  z3_to_func_decl :: <Z3_context> -> <Z3_ast> -> IO <Z3_func_decl>

{- | Return numeral value, as a string of a numeric constant term

       \pre Z3_get_ast_kind(c, a) == Z3_NUMERAL_AST -}
foreign import ccall unsafe "Z3_get_numeral_string"
  z3_get_numeral_string :: <Z3_context> -> <Z3_ast> -> IO <Z3_string>

{- | Return numeral as a string in decimal notation.
       The result has at most \c precision decimal places.

       \pre Z3_get_ast_kind(c, a) == Z3_NUMERAL_AST || Z3_is_algebraic_number(c, a) -}
foreign import ccall unsafe "Z3_get_numeral_decimal_string"
  z3_get_numeral_decimal_string :: <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_string>

{- | Return the numerator (as a numeral AST) of a numeral AST of sort Real.

       \pre Z3_get_ast_kind(c, a) == Z3_NUMERAL_AST -}
foreign import ccall unsafe "Z3_get_numerator"
  z3_get_numerator :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Return the denominator (as a numeral AST) of a numeral AST of sort Real.

       \pre Z3_get_ast_kind(c, a) == Z3_NUMERAL_AST -}
foreign import ccall unsafe "Z3_get_denominator"
  z3_get_denominator :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Return numeral value, as a pair of 64 bit numbers if the representation fits.

       \param c logical context.
       \param a term.
       \param num numerator.
       \param den denominator.

       Return \c Z3_TRUE if the numeral value fits in 64 bit numerals, \c Z3_FALSE otherwise.

       \pre Z3_get_ast_kind(a) == Z3_NUMERAL_AST -}
foreign import ccall unsafe "Z3_get_numeral_small"
  z3_get_numeral_small :: <Z3_context> -> <Z3_ast> -> Ptr CLong -> Ptr CLong -> IO CInt

{- | Similar to #Z3_get_numeral_string, but only succeeds if
       the value can fit in a machine int. Return Z3_TRUE if the call succeeded.

       \pre Z3_get_ast_kind(c, v) == Z3_NUMERAL_AST

       \sa Z3_get_numeral_string -}
foreign import ccall unsafe "Z3_get_numeral_int"
  z3_get_numeral_int :: <Z3_context> -> <Z3_ast> -> Ptr CInt -> IO CInt

{- | Similar to #Z3_get_numeral_string, but only succeeds if
       the value can fit in a machine unsigned int. Return Z3_TRUE if the call succeeded.

       \pre Z3_get_ast_kind(c, v) == Z3_NUMERAL_AST

       \sa Z3_get_numeral_string -}
foreign import ccall unsafe "Z3_get_numeral_uint"
  z3_get_numeral_uint :: <Z3_context> -> <Z3_ast> -> Ptr CUInt -> IO CInt

{- | Similar to #Z3_get_numeral_string, but only succeeds if
       the value can fit in a machine unsigned __int64 int. Return Z3_TRUE if the call succeeded.

       \pre Z3_get_ast_kind(c, v) == Z3_NUMERAL_AST

       \sa Z3_get_numeral_string -}
foreign import ccall unsafe "Z3_get_numeral_uint64"
  z3_get_numeral_uint64 :: <Z3_context> -> <Z3_ast> -> Ptr CULong -> IO CInt

{- | Similar to #Z3_get_numeral_string, but only succeeds if
       the value can fit in a machine __int64 int. Return Z3_TRUE if the call succeeded.

       \pre Z3_get_ast_kind(c, v) == Z3_NUMERAL_AST

       \sa Z3_get_numeral_string -}
foreign import ccall unsafe "Z3_get_numeral_int64"
  z3_get_numeral_int64 :: <Z3_context> -> <Z3_ast> -> Ptr CLong -> IO CInt

{- | Similar to #Z3_get_numeral_string, but only succeeds if
       the value can fit as a rational number as machine __int64 int. Return Z3_TRUE if the call succeeded.

       \pre Z3_get_ast_kind(c, v) == Z3_NUMERAL_AST

       \sa Z3_get_numeral_string -}
foreign import ccall unsafe "Z3_get_numeral_rational_int64"
  z3_get_numeral_rational_int64 :: <Z3_context> -> <Z3_ast> -> Ptr CLong -> Ptr CLong -> IO CInt

{- | Return a lower bound for the given real algebraic number.
       The interval isolating the number is smaller than 1/10^precision.
       The result is a numeral AST of sort Real.

       \pre Z3_is_algebraic_number(c, a) -}
foreign import ccall unsafe "Z3_get_algebraic_number_lower"
  z3_get_algebraic_number_lower :: <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_ast>

{- | Return a upper bound for the given real algebraic number.
       The interval isolating the number is smaller than 1/10^precision.
       The result is a numeral AST of sort Real.

       \pre Z3_is_algebraic_number(c, a) -}
foreign import ccall unsafe "Z3_get_algebraic_number_upper"
  z3_get_algebraic_number_upper :: <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_ast>

{- | Convert a Z3_pattern into Z3_ast. This is just type casting. -}
foreign import ccall unsafe "Z3_pattern_to_ast"
  z3_pattern_to_ast :: <Z3_context> -> <Z3_pattern> -> IO <Z3_ast>

{- | Return number of terms in pattern. -}
foreign import ccall unsafe "Z3_get_pattern_num_terms"
  z3_get_pattern_num_terms :: <Z3_context> -> <Z3_pattern> -> IO ()

{- | Return i'th ast in pattern. -}
foreign import ccall unsafe "Z3_get_pattern"
  z3_get_pattern :: <Z3_context> -> <Z3_pattern> -> CUInt -> IO <Z3_ast>

{- | Return index of de-Brujin bound variable.

       \pre Z3_get_ast_kind(a) == Z3_VAR_AST -}
foreign import ccall unsafe "Z3_get_index_value"
  z3_get_index_value :: <Z3_context> -> <Z3_ast> -> IO ()

{- | Determine if quantifier is universal.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
foreign import ccall unsafe "Z3_is_quantifier_forall"
  z3_is_quantifier_forall :: <Z3_context> -> <Z3_ast> -> IO CInt

{- | Obtain weight of quantifier.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
foreign import ccall unsafe "Z3_get_quantifier_weight"
  z3_get_quantifier_weight :: <Z3_context> -> <Z3_ast> -> IO ()

{- | Return number of patterns used in quantifier.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
foreign import ccall unsafe "Z3_get_quantifier_num_patterns"
  z3_get_quantifier_num_patterns :: <Z3_context> -> <Z3_ast> -> IO ()

{- | Return i'th pattern.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
foreign import ccall unsafe "Z3_get_quantifier_pattern_ast"
  z3_get_quantifier_pattern_ast :: <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_pattern>

{- | Return number of no_patterns used in quantifier.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
foreign import ccall unsafe "Z3_get_quantifier_num_no_patterns"
  z3_get_quantifier_num_no_patterns :: <Z3_context> -> <Z3_ast> -> IO ()

{- | Return i'th no_pattern.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
foreign import ccall unsafe "Z3_get_quantifier_no_pattern_ast"
  z3_get_quantifier_no_pattern_ast :: <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_ast>

{- | Return number of bound variables of quantifier.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
foreign import ccall unsafe "Z3_get_quantifier_num_bound"
  z3_get_quantifier_num_bound :: <Z3_context> -> <Z3_ast> -> IO ()

{- | Return symbol of the i'th bound variable.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
foreign import ccall unsafe "Z3_get_quantifier_bound_name"
  z3_get_quantifier_bound_name :: <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_symbol>

{- | Return sort of the i'th bound variable.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
foreign import ccall unsafe "Z3_get_quantifier_bound_sort"
  z3_get_quantifier_bound_sort :: <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_sort>

{- | Return body of quantifier.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
foreign import ccall unsafe "Z3_get_quantifier_body"
  z3_get_quantifier_body :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Interface to simplifier.

        Provides an interface to the AST simplifier used by Z3.
        It returns an AST object which is equal to the argument.
        The returned AST is simplified using algebraic simplificaiton rules,
        such as constant propagation (propagating true/false over logical connectives). -}
foreign import ccall unsafe "Z3_simplify"
  z3_simplify :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Interface to simplifier.

        Provides an interface to the AST simplifier used by Z3.
        This procedure is similar to #Z3_simplify, but the behavior of the simplifier
        can be configured using the given parameter set. -}
foreign import ccall unsafe "Z3_simplify_ex"
  z3_simplify_ex :: <Z3_context> -> <Z3_ast> -> <Z3_params> -> IO <Z3_ast>

{- | Return a string describing all available parameters. -}
foreign import ccall unsafe "Z3_simplify_get_help"
  z3_simplify_get_help :: <Z3_context> -> IO <Z3_string>

{- | Return the parameter description set for the simplify procedure. -}
foreign import ccall unsafe "Z3_simplify_get_param_descrs"
  z3_simplify_get_param_descrs :: <Z3_context> -> IO <Z3_param_descrs>

{- | Update the arguments of term \c a using the arguments \c args.
       The number of arguments \c num_args should coincide
       with the number of arguments to \c a.
       If \c a is a quantifier, then num_args has to be 1. -}
foreign import ccall unsafe "Z3_update_term"
  z3_update_term :: <Z3_context> -> <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Substitute every occurrence of \ccode{from[i]} in \c a with \ccode{to[i]}, for \c i smaller than \c num_exprs.
       The result is the new AST. The arrays \c from and \c to must have size \c num_exprs.
       For every \c i smaller than \c num_exprs, we must have that sort of \ccode{from[i]} must be equal to sort of \ccode{to[i]}. -}
foreign import ccall unsafe "Z3_substitute"
  z3_substitute :: <Z3_context> -> <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Substitute the free variables in \c a with the expressions in \c to.
       For every \c i smaller than \c num_exprs, the variable with de-Bruijn index \c i is replaced with term \ccode{to[i]}. -}
foreign import ccall unsafe "Z3_substitute_vars"
  z3_substitute_vars :: <Z3_context> -> <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Translate/Copy the AST \c a from context \c source to context \c target.
       AST \c a must have been created using context \c source.
       \pre source != target -}
foreign import ccall unsafe "Z3_translate"
  z3_translate :: <Z3_context> -> <Z3_ast> -> <Z3_context> -> IO <Z3_ast>

{- | Increment the reference counter of the given model. -}
foreign import ccall unsafe "Z3_model_inc_ref"
  z3_model_inc_ref :: <Z3_context> -> <Z3_model> -> IO ()

{- | Decrement the reference counter of the given model. -}
foreign import ccall unsafe "Z3_model_dec_ref"
  z3_model_dec_ref :: <Z3_context> -> <Z3_model> -> IO ()

{- | Evaluate the AST node \c t in the given model.
       Return \c Z3_TRUE if succeeded, and store the result in \c v.

       If \c model_completion is Z3_TRUE, then Z3 will assign an interpretation for any constant or function that does
       not have an interpretation in \c m. These constants and functions were essentially don't cares.

       If \c model_completion is Z3_FALSE, then Z3 will not assign interpretations to constants for functions that do
       not have interpretations in \c m. Evaluation behaves as the identify function in this case.

       The evaluation may fail for the following reasons:

       - \c t contains a quantifier.

       - the model \c m is partial, that is, it doesn't have a complete interpretation for uninterpreted functions.
       That is, the option \ccode{MODEL_PARTIAL=true} was used.

       - \c t is type incorrect.

       - \c Z3_interrupt was invoked during evaluation. -}
foreign import ccall unsafe "Z3_model_eval"
  z3_model_eval :: <Z3_context> -> <Z3_model> -> <Z3_ast> -> CInt -> Ptr <Z3_ast> -> IO CInt

{- | Return the interpretation (i.e., assignment) of constant \c a in the model \c m.
       Return \c NULL, if the model does not assign an interpretation for \c a.
       That should be interpreted as: the value of \c a does not matter.

       \pre Z3_get_arity(c, a) == 0 -}
foreign import ccall unsafe "Z3_model_get_const_interp"
  z3_model_get_const_interp :: <Z3_context> -> <Z3_model> -> <Z3_func_decl> -> IO <Z3_ast>

{- | Test if there exists an interpretation (i.e., assignment) for \c a in the model \c m. -}
foreign import ccall unsafe "Z3_model_has_interp"
  z3_model_has_interp :: <Z3_context> -> <Z3_model> -> <Z3_func_decl> -> IO CInt

{- | Return the interpretation of the function \c f in the model \c m.
       Return \c NULL, if the model does not assign an interpretation for \c f.
       That should be interpreted as: the \c f does not matter.

       \pre Z3_get_arity(c, f) > 0

       \remark Reference counting must be used to manage Z3_func_interp objects, even when the Z3_context was
       created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
foreign import ccall unsafe "Z3_model_get_func_interp"
  z3_model_get_func_interp :: <Z3_context> -> <Z3_model> -> <Z3_func_decl> -> IO <Z3_func_interp>

{- | Return the number of constants assigned by the given model.

       \sa Z3_model_get_const_decl -}
foreign import ccall unsafe "Z3_model_get_num_consts"
  z3_model_get_num_consts :: <Z3_context> -> <Z3_model> -> IO ()

{- | Return the i-th constant in the given model.

       \pre i < Z3_model_get_num_consts(c, m)

       \sa Z3_model_eval -}
foreign import ccall unsafe "Z3_model_get_const_decl"
  z3_model_get_const_decl :: <Z3_context> -> <Z3_model> -> CUInt -> IO <Z3_func_decl>

{- | Return the number of function interpretations in the given model.

       A function interpretation is represented as a finite map and an 'else' value.
       Each entry in the finite map represents the value of a function given a set of arguments. -}
foreign import ccall unsafe "Z3_model_get_num_funcs"
  z3_model_get_num_funcs :: <Z3_context> -> <Z3_model> -> IO ()

{- | Return the declaration of the i-th function in the given model.

       \pre i < Z3_model_get_num_funcs(c, m)

       \sa Z3_model_get_num_funcs -}
foreign import ccall unsafe "Z3_model_get_func_decl"
  z3_model_get_func_decl :: <Z3_context> -> <Z3_model> -> CUInt -> IO <Z3_func_decl>

{- | Return the number of uninterpreted sorts that \c m assigs an interpretation to.

       Z3 also provides an intepretation for uninterpreted sorts used in a formua.
       The interpretation for a sort \c s is a finite set of distinct values. We say this finite set is
       the "universe" of \c s.

       \sa Z3_model_get_sort
       \sa Z3_model_get_sort_universe -}
foreign import ccall unsafe "Z3_model_get_num_sorts"
  z3_model_get_num_sorts :: <Z3_context> -> <Z3_model> -> IO ()

{- | Return a uninterpreted sort that \c m assigns an interpretation.

       \pre i < Z3_model_get_num_sorts(c, m)

       \sa Z3_model_get_num_sorts
       \sa Z3_model_get_sort_universe -}
foreign import ccall unsafe "Z3_model_get_sort"
  z3_model_get_sort :: <Z3_context> -> <Z3_model> -> CUInt -> IO <Z3_sort>

{- | Return the finite set of distinct values that represent the interpretation for sort \c s.

       \sa Z3_model_get_num_sorts
       \sa Z3_model_get_sort -}
foreign import ccall unsafe "Z3_model_get_sort_universe"
  z3_model_get_sort_universe :: <Z3_context> -> <Z3_model> -> <Z3_sort> -> IO <Z3_ast_vector>

{- | The \ccode{(_ as-array f)} AST node is a construct for assigning interpretations for arrays in Z3.
       It is the array such that forall indices \c i we have that \ccode{(select (_ as-array f) i)} is equal to \ccode{(f i)}.
       This procedure returns Z3_TRUE if the \c a is an \c as-array AST node.

       Z3 current solvers have minimal support for \c as_array nodes.

       \sa Z3_get_as_array_func_decl -}
foreign import ccall unsafe "Z3_is_as_array"
  z3_is_as_array :: <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return the function declaration \c f associated with a \ccode{(_ as_array f)} node.

       \sa Z3_is_as_array -}
foreign import ccall unsafe "Z3_get_as_array_func_decl"
  z3_get_as_array_func_decl :: <Z3_context> -> <Z3_ast> -> IO <Z3_func_decl>

{- | Increment the reference counter of the given Z3_func_interp object. -}
foreign import ccall unsafe "Z3_func_interp_inc_ref"
  z3_func_interp_inc_ref :: <Z3_context> -> <Z3_func_interp> -> IO ()

{- | Decrement the reference counter of the given Z3_func_interp object. -}
foreign import ccall unsafe "Z3_func_interp_dec_ref"
  z3_func_interp_dec_ref :: <Z3_context> -> <Z3_func_interp> -> IO ()

{- | Return the number of entries in the given function interpretation.

       A function interpretation is represented as a finite map and an 'else' value.
       Each entry in the finite map represents the value of a function given a set of arguments.
       This procedure return the number of element in the finite map of \c f. -}
foreign import ccall unsafe "Z3_func_interp_get_num_entries"
  z3_func_interp_get_num_entries :: <Z3_context> -> <Z3_func_interp> -> IO ()

{- | Return a "point" of the given function intepretation. It represents the
       value of \c f in a particular point.

       \pre i < Z3_func_interp_get_num_entries(c, f)

       \sa Z3_func_interp_get_num_entries -}
foreign import ccall unsafe "Z3_func_interp_get_entry"
  z3_func_interp_get_entry :: <Z3_context> -> <Z3_func_interp> -> CUInt -> IO <Z3_func_entry>

{- | Return the 'else' value of the given function interpretation.

       A function interpretation is represented as a finite map and an 'else' value.
       This procedure returns the 'else' value. -}
foreign import ccall unsafe "Z3_func_interp_get_else"
  z3_func_interp_get_else :: <Z3_context> -> <Z3_func_interp> -> IO <Z3_ast>

{- | Return the arity (number of arguments) of the given function interpretation. -}
foreign import ccall unsafe "Z3_func_interp_get_arity"
  z3_func_interp_get_arity :: <Z3_context> -> <Z3_func_interp> -> IO ()

{- | Increment the reference counter of the given Z3_func_entry object. -}
foreign import ccall unsafe "Z3_func_entry_inc_ref"
  z3_func_entry_inc_ref :: <Z3_context> -> <Z3_func_entry> -> IO ()

{- | Decrement the reference counter of the given Z3_func_entry object. -}
foreign import ccall unsafe "Z3_func_entry_dec_ref"
  z3_func_entry_dec_ref :: <Z3_context> -> <Z3_func_entry> -> IO ()

{- | Return the value of this point.

       A Z3_func_entry object represents an element in the finite map used to encode
       a function interpretation.

       \sa Z3_func_interp_get_entry -}
foreign import ccall unsafe "Z3_func_entry_get_value"
  z3_func_entry_get_value :: <Z3_context> -> <Z3_func_entry> -> IO <Z3_ast>

{- | Return the number of arguments in a Z3_func_entry object.

       \sa Z3_func_interp_get_entry -}
foreign import ccall unsafe "Z3_func_entry_get_num_args"
  z3_func_entry_get_num_args :: <Z3_context> -> <Z3_func_entry> -> IO ()

{- | Return an argument of a Z3_func_entry object.

       \pre i < Z3_func_entry_get_num_args(c, e)

       \sa Z3_func_interp_get_entry -}
foreign import ccall unsafe "Z3_func_entry_get_arg"
  z3_func_entry_get_arg :: <Z3_context> -> <Z3_func_entry> -> CUInt -> IO <Z3_ast>

{- | Log interaction to a file.

       extra_API('Z3_open_log', INT, (_in(STRING),)) -}
foreign import ccall unsafe "Z3_open_log"
  z3_open_log :: <Z3_string> -> IO CInt

{- | Append user-defined string to interaction log.

       The interaction log is opened using Z3_open_log.
       It contains the formulas that are checked using Z3.
       You can use this command to append comments, for instance.

       extra_API('Z3_append_log', VOID, (_in(STRING),)) -}
foreign import ccall unsafe "Z3_append_log"
  z3_append_log :: <Z3_string> -> IO ()

{- | Close interaction log.

       extra_API('Z3_close_log', VOID, ()) -}
foreign import ccall unsafe "Z3_close_log"
  z3_close_log :: IO ()

{- | Enable/disable printing warning messages to the console.

       Warnings are printed after passing \c true, warning messages are
       suppressed after calling this method with \c false. -}
foreign import ccall unsafe "Z3_toggle_warning_messages"
  z3_toggle_warning_messages :: CInt -> IO ()

{- | Select mode for the format used for pretty-printing AST nodes.

       The default mode for pretty printing AST nodes is to produce
       SMT-LIB style output where common subexpressions are printed
       at each occurrence. The mode is called Z3_PRINT_SMTLIB_FULL.
       To print shared common subexpressions only once,
       use the Z3_PRINT_LOW_LEVEL mode.
       To print in way that conforms to SMT-LIB standards and uses let
       expressions to share common sub-expressions use Z3_PRINT_SMTLIB_COMPLIANT.

       \sa Z3_ast_to_string
       \sa Z3_pattern_to_string
       \sa Z3_func_decl_to_string -}
foreign import ccall unsafe "Z3_set_ast_print_mode"
  z3_set_ast_print_mode :: <Z3_context> -> <Z3_ast_print_mode> -> IO ()

{- | Convert the given AST node into a string.

       \warning The result buffer is statically allocated by Z3. It will
       be automatically deallocated when #Z3_del_context is invoked.
       So, the buffer is invalidated in the next call to \c Z3_ast_to_string.

       \sa Z3_pattern_to_string
       \sa Z3_sort_to_string -}
foreign import ccall unsafe "Z3_ast_to_string"
  z3_ast_to_string :: <Z3_context> -> <Z3_ast> -> IO <Z3_string>
foreign import ccall unsafe "Z3_pattern_to_string"
  z3_pattern_to_string :: <Z3_context> -> <Z3_pattern> -> IO <Z3_string>
foreign import ccall unsafe "Z3_sort_to_string"
  z3_sort_to_string :: <Z3_context> -> <Z3_sort> -> IO <Z3_string>
foreign import ccall unsafe "Z3_func_decl_to_string"
  z3_func_decl_to_string :: <Z3_context> -> <Z3_func_decl> -> IO <Z3_string>

{- | Convert the given model into a string.

       \warning The result buffer is statically allocated by Z3. It will
       be automatically deallocated when #Z3_del_context is invoked.
       So, the buffer is invalidated in the next call to \c Z3_model_to_string. -}
foreign import ccall unsafe "Z3_model_to_string"
  z3_model_to_string :: <Z3_context> -> <Z3_model> -> IO <Z3_string>

{- | Convert the given benchmark into SMT-LIB formatted string.

       \warning The result buffer is statically allocated by Z3. It will
       be automatically deallocated when #Z3_del_context is invoked.
       So, the buffer is invalidated in the next call to \c Z3_benchmark_to_smtlib_string.

       \param c - context.
       \param name - name of benchmark. The argument is optional.
       \param logic - the benchmark logic.
       \param status - the status string (sat, unsat, or unknown)
       \param attributes - other attributes, such as source, difficulty or category.
       \param num_assumptions - number of assumptions.
       \param assumptions - auxiliary assumptions.
       \param formula - formula to be checked for consistency in conjunction with assumptions. -}
foreign import ccall unsafe "Z3_benchmark_to_smtlib_string"
  z3_benchmark_to_smtlib_string :: <Z3_context> -> <Z3_string> -> <Z3_string> -> <Z3_string> -> <Z3_string> -> CUInt -> Ptr <Z3_ast> -> <Z3_ast> -> IO <Z3_string>

{- | Parse the given string using the SMT-LIB2 parser.

       It returns a formula comprising of the conjunction of assertions in the scope
       (up to push/pop) at the end of the string. -}
foreign import ccall unsafe "Z3_parse_smtlib2_string"
  z3_parse_smtlib2_string :: <Z3_context> -> <Z3_string> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_func_decl> -> IO <Z3_ast>

{- | Similar to #Z3_parse_smtlib2_string, but reads the benchmark from a file. -}
foreign import ccall unsafe "Z3_parse_smtlib2_file"
  z3_parse_smtlib2_file :: <Z3_context> -> <Z3_string> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_func_decl> -> IO <Z3_ast>

{- | Parse the given string using the SMT-LIB parser.

       The symbol table of the parser can be initialized using the given sorts and declarations.
       The symbols in the arrays \c sort_names and \c decl_names don't need to match the names
       of the sorts and declarations in the arrays \c sorts and \c decls. This is an useful feature
       since we can use arbitrary names to reference sorts and declarations defined using the C API.

       The formulas, assumptions and declarations defined in \c str can be extracted using the functions:
       #Z3_get_smtlib_num_formulas, #Z3_get_smtlib_formula, #Z3_get_smtlib_num_assumptions, #Z3_get_smtlib_assumption,
       #Z3_get_smtlib_num_decls, and #Z3_get_smtlib_decl. -}
foreign import ccall unsafe "Z3_parse_smtlib_string"
  z3_parse_smtlib_string :: <Z3_context> -> <Z3_string> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_func_decl> -> IO ()

{- | Similar to #Z3_parse_smtlib_string, but reads the benchmark from a file. -}
foreign import ccall unsafe "Z3_parse_smtlib_file"
  z3_parse_smtlib_file :: <Z3_context> -> <Z3_string> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_func_decl> -> IO ()

{- | Return the number of SMTLIB formulas parsed by the last call to #Z3_parse_smtlib_string or #Z3_parse_smtlib_file. -}
foreign import ccall unsafe "Z3_get_smtlib_num_formulas"
  z3_get_smtlib_num_formulas :: <Z3_context> -> IO ()

{- | Return the i-th formula parsed by the last call to #Z3_parse_smtlib_string or #Z3_parse_smtlib_file.

       \pre i < Z3_get_smtlib_num_formulas(c) -}
foreign import ccall unsafe "Z3_get_smtlib_formula"
  z3_get_smtlib_formula :: <Z3_context> -> CUInt -> IO <Z3_ast>

{- | Return the number of SMTLIB assumptions parsed by #Z3_parse_smtlib_string or #Z3_parse_smtlib_file. -}
foreign import ccall unsafe "Z3_get_smtlib_num_assumptions"
  z3_get_smtlib_num_assumptions :: <Z3_context> -> IO ()

{- | Return the i-th assumption parsed by the last call to #Z3_parse_smtlib_string or #Z3_parse_smtlib_file.

       \pre i < Z3_get_smtlib_num_assumptions(c) -}
foreign import ccall unsafe "Z3_get_smtlib_assumption"
  z3_get_smtlib_assumption :: <Z3_context> -> CUInt -> IO <Z3_ast>

{- | Return the number of declarations parsed by #Z3_parse_smtlib_string or #Z3_parse_smtlib_file. -}
foreign import ccall unsafe "Z3_get_smtlib_num_decls"
  z3_get_smtlib_num_decls :: <Z3_context> -> IO ()

{- | Return the i-th declaration parsed by the last call to #Z3_parse_smtlib_string or #Z3_parse_smtlib_file.

       \pre i < Z3_get_smtlib_num_decls(c) -}
foreign import ccall unsafe "Z3_get_smtlib_decl"
  z3_get_smtlib_decl :: <Z3_context> -> CUInt -> IO <Z3_func_decl>

{- | Return the number of sorts parsed by #Z3_parse_smtlib_string or #Z3_parse_smtlib_file. -}
foreign import ccall unsafe "Z3_get_smtlib_num_sorts"
  z3_get_smtlib_num_sorts :: <Z3_context> -> IO ()

{- | Return the i-th sort parsed by the last call to #Z3_parse_smtlib_string or #Z3_parse_smtlib_file.

       \pre i < Z3_get_smtlib_num_sorts(c) -}
foreign import ccall unsafe "Z3_get_smtlib_sort"
  z3_get_smtlib_sort :: <Z3_context> -> CUInt -> IO <Z3_sort>

{- | Retrieve that last error message information generated from parsing. -}
foreign import ccall unsafe "Z3_get_smtlib_error"
  z3_get_smtlib_error :: <Z3_context> -> IO <Z3_string>

{- | Return the error code for the last API call.

       A call to a Z3 function may return a non Z3_OK error code,
       when it is not used correctly.

       \sa Z3_set_error_handler -}
foreign import ccall unsafe "Z3_get_error_code"
  z3_get_error_code :: <Z3_context> -> IO <Z3_error_code>

{- | Register a Z3 error handler.

       A call to a Z3 function may return a non Z3_OK error code, when
       it is not used correctly.  An error handler can be registered
       and will be called in this case.  To disable the use of the
       error handler, simply register with \c h=NULL.

       \warning Log files, created using #Z3_open_log, may be potentially incomplete/incorrect if error handlers are used.

       \sa Z3_get_error_code -}
foreign import ccall unsafe "Z3_set_error_handler"
  z3_set_error_handler :: <Z3_context> -> <Z3_error_handler> -> IO ()

{- | Set an error. -}
foreign import ccall unsafe "Z3_set_error"
  z3_set_error :: <Z3_context> -> <Z3_error_code> -> IO ()

{- | Return a string describing the given error code. -}
foreign import ccall unsafe "Z3_get_error_msg"
  z3_get_error_msg :: <Z3_context> -> <Z3_error_code> -> IO <Z3_string>

{- | Return a string describing the given error code. 
       Retained function name for backwards compatibility within v4.1 -}
foreign import ccall unsafe "Z3_get_error_msg_ex"
  z3_get_error_msg_ex :: <Z3_context> -> <Z3_error_code> -> IO <Z3_string>

{- | Return Z3 version number information. -}
foreign import ccall unsafe "Z3_get_version"
  z3_get_version :: Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> IO ()

{- | Return a string that fully describes the version of Z3 in use. -}
foreign import ccall unsafe "Z3_get_full_version"
  z3_get_full_version :: IO <Z3_string>

{- | Enable tracing messages tagged as \c tag when Z3 is compiled in debug mode.
       It is a NOOP otherwise -}
foreign import ccall unsafe "Z3_enable_trace"
  z3_enable_trace :: <Z3_string> -> IO ()

{- | Disable tracing messages tagged as \c tag when Z3 is compiled in debug mode.
       It is a NOOP otherwise -}
foreign import ccall unsafe "Z3_disable_trace"
  z3_disable_trace :: <Z3_string> -> IO ()

{- | Reset all allocated resources.

       Use this facility on out-of memory errors.
       It allows discharging the previous state and resuming afresh.
       Any pointers previously returned by the API
       become invalid. -}
foreign import ccall unsafe "Z3_reset_memory"
  z3_reset_memory :: IO ()

{- | Destroy all allocated resources.

       Any pointers previously returned by the API become invalid.
       Can be used for memory leak detection. -}
foreign import ccall unsafe "Z3_finalize_memory"
  z3_finalize_memory :: IO ()

{- | Create a goal (aka problem). A goal is essentially a set
       of formulas, that can be solved and/or transformed using
       tactics and solvers.

       If models == true, then model generation is enabled for the new goal.

       If unsat_cores == true, then unsat core generation is enabled for the new goal.

       If proofs == true, then proof generation is enabled for the new goal. Remark, the
       Z3 context c must have been created with proof generation support.

       \remark Reference counting must be used to manage goals, even when the Z3_context was
       created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
foreign import ccall unsafe "Z3_mk_goal"
  z3_mk_goal :: <Z3_context> -> CInt -> CInt -> CInt -> IO <Z3_goal>

{- | Increment the reference counter of the given goal. -}
foreign import ccall unsafe "Z3_goal_inc_ref"
  z3_goal_inc_ref :: <Z3_context> -> <Z3_goal> -> IO ()

{- | Decrement the reference counter of the given goal. -}
foreign import ccall unsafe "Z3_goal_dec_ref"
  z3_goal_dec_ref :: <Z3_context> -> <Z3_goal> -> IO ()

{- | Return the "precision" of the given goal. Goals can be transformed using over and under approximations.
       A under approximation is applied when the objective is to find a model for a given goal.
       An over approximation is applied when the objective is to find a proof for a given goal. -}
foreign import ccall unsafe "Z3_goal_precision"
  z3_goal_precision :: <Z3_context> -> <Z3_goal> -> IO <Z3_goal_prec>

{- | Add a new formula \c a to the given goal. -}
foreign import ccall unsafe "Z3_goal_assert"
  z3_goal_assert :: <Z3_context> -> <Z3_goal> -> <Z3_ast> -> IO ()

{- | Return true if the given goal contains the formula \c false. -}
foreign import ccall unsafe "Z3_goal_inconsistent"
  z3_goal_inconsistent :: <Z3_context> -> <Z3_goal> -> IO CInt

{- | Return the depth of the given goal. It tracks how many transformations were applied to it. -}
foreign import ccall unsafe "Z3_goal_depth"
  z3_goal_depth :: <Z3_context> -> <Z3_goal> -> IO ()

{- | Erase all formulas from the given goal. -}
foreign import ccall unsafe "Z3_goal_reset"
  z3_goal_reset :: <Z3_context> -> <Z3_goal> -> IO ()

{- | Return the number of formulas in the given goal. -}
foreign import ccall unsafe "Z3_goal_size"
  z3_goal_size :: <Z3_context> -> <Z3_goal> -> IO ()

{- | Return a formula from the given goal.

       \pre idx < Z3_goal_size(c, g) -}
foreign import ccall unsafe "Z3_goal_formula"
  z3_goal_formula :: <Z3_context> -> <Z3_goal> -> CUInt -> IO <Z3_ast>

{- | Return the number of formulas, subformulas and terms in the given goal. -}
foreign import ccall unsafe "Z3_goal_num_exprs"
  z3_goal_num_exprs :: <Z3_context> -> <Z3_goal> -> IO ()

{- | Return true if the goal is empty, and it is precise or the product of a under approximation. -}
foreign import ccall unsafe "Z3_goal_is_decided_sat"
  z3_goal_is_decided_sat :: <Z3_context> -> <Z3_goal> -> IO CInt

{- | Return true if the goal contains false, and it is precise or the product of an over approximation. -}
foreign import ccall unsafe "Z3_goal_is_decided_unsat"
  z3_goal_is_decided_unsat :: <Z3_context> -> <Z3_goal> -> IO CInt

{- | Copy a goal \c g from the context \c source to a the context \c target. -}
foreign import ccall unsafe "Z3_goal_translate"
  z3_goal_translate :: <Z3_context> -> <Z3_goal> -> <Z3_context> -> IO <Z3_goal>

{- | Convert a goal into a string. -}
foreign import ccall unsafe "Z3_goal_to_string"
  z3_goal_to_string :: <Z3_context> -> <Z3_goal> -> IO <Z3_string>

{- | Return a tactic associated with the given name.
       The complete list of tactics may be obtained using the procedures #Z3_get_num_tactics and #Z3_get_tactic_name.
       It may also be obtained using the command \ccode{(help-tactic)} in the SMT 2.0 front-end.

       Tactics are the basic building block for creating custom solvers for specific problem domains. -}
foreign import ccall unsafe "Z3_mk_tactic"
  z3_mk_tactic :: <Z3_context> -> <Z3_string> -> IO <Z3_tactic>

{- | Increment the reference counter of the given tactic. -}
foreign import ccall unsafe "Z3_tactic_inc_ref"
  z3_tactic_inc_ref :: <Z3_context> -> <Z3_tactic> -> IO ()

{- | Decrement the reference counter of the given tactic. -}
foreign import ccall unsafe "Z3_tactic_dec_ref"
  z3_tactic_dec_ref :: <Z3_context> -> <Z3_tactic> -> IO ()

{- | Return a probe associated with the given name.
       The complete list of probes may be obtained using the procedures #Z3_get_num_probes and #Z3_get_probe_name.
       It may also be obtained using the command \ccode{(help-tactic)} in the SMT 2.0 front-end.

       Probes are used to inspect a goal (aka problem) and collect information that may be used to decide
       which solver and/or preprocessing step will be used. -}
foreign import ccall unsafe "Z3_mk_probe"
  z3_mk_probe :: <Z3_context> -> <Z3_string> -> IO <Z3_probe>

{- | Increment the reference counter of the given probe. -}
foreign import ccall unsafe "Z3_probe_inc_ref"
  z3_probe_inc_ref :: <Z3_context> -> <Z3_probe> -> IO ()

{- | Decrement the reference counter of the given probe. -}
foreign import ccall unsafe "Z3_probe_dec_ref"
  z3_probe_dec_ref :: <Z3_context> -> <Z3_probe> -> IO ()

{- | Return a tactic that applies \c t1 to a given goal and \c t2
       to every subgoal produced by t1. -}
foreign import ccall unsafe "Z3_tactic_and_then"
  z3_tactic_and_then :: <Z3_context> -> <Z3_tactic> -> <Z3_tactic> -> IO <Z3_tactic>

{- | Return a tactic that first applies \c t1 to a given goal,
       if it fails then returns the result of \c t2 applied to the given goal. -}
foreign import ccall unsafe "Z3_tactic_or_else"
  z3_tactic_or_else :: <Z3_context> -> <Z3_tactic> -> <Z3_tactic> -> IO <Z3_tactic>

{- | Return a tactic that applies the given tactics in parallel. -}
foreign import ccall unsafe "Z3_tactic_par_or"
  z3_tactic_par_or :: <Z3_context> -> CUInt -> Ptr <Z3_tactic> -> IO <Z3_tactic>

{- | Return a tactic that applies \c t1 to a given goal and then \c t2
       to every subgoal produced by t1. The subgoals are processed in parallel. -}
foreign import ccall unsafe "Z3_tactic_par_and_then"
  z3_tactic_par_and_then :: <Z3_context> -> <Z3_tactic> -> <Z3_tactic> -> IO <Z3_tactic>

{- | Return a tactic that applies \c t to a given goal for \c ms milliseconds.
       If \c t does not terminate in \c ms milliseconds, then it fails. -}
foreign import ccall unsafe "Z3_tactic_try_for"
  z3_tactic_try_for :: <Z3_context> -> <Z3_tactic> -> CUInt -> IO <Z3_tactic>

{- | Return a tactic that applies \c t to a given goal is the probe \c p evaluates to true.
       If \c p evaluates to false, then the new tactic behaves like the skip tactic. -}
foreign import ccall unsafe "Z3_tactic_when"
  z3_tactic_when :: <Z3_context> -> <Z3_probe> -> <Z3_tactic> -> IO <Z3_tactic>

{- | Return a tactic that applies \c t1 to a given goal if the probe \c p evaluates to true,
       and \c t2 if \c p evaluates to false. -}
foreign import ccall unsafe "Z3_tactic_cond"
  z3_tactic_cond :: <Z3_context> -> <Z3_probe> -> <Z3_tactic> -> <Z3_tactic> -> IO <Z3_tactic>

{- | Return a tactic that keeps applying \c t until the goal is not modified anymore or the maximum
       number of iterations \c max is reached. -}
foreign import ccall unsafe "Z3_tactic_repeat"
  z3_tactic_repeat :: <Z3_context> -> <Z3_tactic> -> CUInt -> IO <Z3_tactic>

{- | Return a tactic that just return the given goal. -}
foreign import ccall unsafe "Z3_tactic_skip"
  z3_tactic_skip :: <Z3_context> -> IO <Z3_tactic>

{- | Return a tactic that always fails. -}
foreign import ccall unsafe "Z3_tactic_fail"
  z3_tactic_fail :: <Z3_context> -> IO <Z3_tactic>

{- | Return a tactic that fails if the probe \c p evaluates to false. -}
foreign import ccall unsafe "Z3_tactic_fail_if"
  z3_tactic_fail_if :: <Z3_context> -> <Z3_probe> -> IO <Z3_tactic>

{- | Return a tactic that fails if the goal is not trivially satisfiable (i.e., empty) or
       trivially unsatisfiable (i.e., contains false). -}
foreign import ccall unsafe "Z3_tactic_fail_if_not_decided"
  z3_tactic_fail_if_not_decided :: <Z3_context> -> IO <Z3_tactic>

{- | Return a tactic that applies \c t using the given set of parameters. -}
foreign import ccall unsafe "Z3_tactic_using_params"
  z3_tactic_using_params :: <Z3_context> -> <Z3_tactic> -> <Z3_params> -> IO <Z3_tactic>

{- | Return a probe that always evaluates to val. -}
foreign import ccall unsafe "Z3_probe_const"
  z3_probe_const :: <Z3_context> -> CDouble -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when the value returned by \c p1 is less than the value returned by \c p2.

       \remark For probes, "true" is any value different from 0.0. -}
foreign import ccall unsafe "Z3_probe_lt"
  z3_probe_lt :: <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when the value returned by \c p1 is greater than the value returned by \c p2.

       \remark For probes, "true" is any value different from 0.0. -}
foreign import ccall unsafe "Z3_probe_gt"
  z3_probe_gt :: <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when the value returned by \c p1 is less than or equal to the value returned by \c p2.

       \remark For probes, "true" is any value different from 0.0. -}
foreign import ccall unsafe "Z3_probe_le"
  z3_probe_le :: <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when the value returned by \c p1 is greater than or equal to the value returned by \c p2.

       \remark For probes, "true" is any value different from 0.0. -}
foreign import ccall unsafe "Z3_probe_ge"
  z3_probe_ge :: <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when the value returned by \c p1 is equal to the value returned by \c p2.

       \remark For probes, "true" is any value different from 0.0. -}
foreign import ccall unsafe "Z3_probe_eq"
  z3_probe_eq :: <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when \c p1 and \c p2 evaluates to true.

       \remark For probes, "true" is any value different from 0.0. -}
foreign import ccall unsafe "Z3_probe_and"
  z3_probe_and :: <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when \c p1 or \c p2 evaluates to true.

       \remark For probes, "true" is any value different from 0.0. -}
foreign import ccall unsafe "Z3_probe_or"
  z3_probe_or :: <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when \c p does not evaluate to true.

       \remark For probes, "true" is any value different from 0.0. -}
foreign import ccall unsafe "Z3_probe_not"
  z3_probe_not :: <Z3_context> -> <Z3_probe> -> IO <Z3_probe>

{- | Return the number of builtin tactics available in Z3. -}
foreign import ccall unsafe "Z3_get_num_tactics"
  z3_get_num_tactics :: <Z3_context> -> IO ()

{- | Return the name of the idx tactic.

       \pre i < Z3_get_num_tactics(c) -}
foreign import ccall unsafe "Z3_get_tactic_name"
  z3_get_tactic_name :: <Z3_context> -> CUInt -> IO <Z3_string>

{- | Return the number of builtin probes available in Z3. -}
foreign import ccall unsafe "Z3_get_num_probes"
  z3_get_num_probes :: <Z3_context> -> IO ()

{- | Return the name of the i probe.

       \pre i < Z3_get_num_probes(c) -}
foreign import ccall unsafe "Z3_get_probe_name"
  z3_get_probe_name :: <Z3_context> -> CUInt -> IO <Z3_string>

{- | Return a string containing a description of parameters accepted by the given tactic. -}
foreign import ccall unsafe "Z3_tactic_get_help"
  z3_tactic_get_help :: <Z3_context> -> <Z3_tactic> -> IO <Z3_string>

{- | Return the parameter description set for the given tactic object. -}
foreign import ccall unsafe "Z3_tactic_get_param_descrs"
  z3_tactic_get_param_descrs :: <Z3_context> -> <Z3_tactic> -> IO <Z3_param_descrs>

{- | Return a string containing a description of the tactic with the given name. -}
foreign import ccall unsafe "Z3_tactic_get_descr"
  z3_tactic_get_descr :: <Z3_context> -> <Z3_string> -> IO <Z3_string>

{- | Return a string containing a description of the probe with the given name. -}
foreign import ccall unsafe "Z3_probe_get_descr"
  z3_probe_get_descr :: <Z3_context> -> <Z3_string> -> IO <Z3_string>

{- | Execute the probe over the goal. The probe always produce a double value.
       "Boolean" probes return 0.0 for false, and a value different from 0.0 for true. -}
foreign import ccall unsafe "Z3_probe_apply"
  z3_probe_apply :: <Z3_context> -> <Z3_probe> -> <Z3_goal> -> IO CDouble

{- | Apply tactic \c t to the goal \c g. -}
foreign import ccall unsafe "Z3_tactic_apply"
  z3_tactic_apply :: <Z3_context> -> <Z3_tactic> -> <Z3_goal> -> IO <Z3_apply_result>

{- | Apply tactic \c t to the goal \c g using the parameter set \c p. -}
foreign import ccall unsafe "Z3_tactic_apply_ex"
  z3_tactic_apply_ex :: <Z3_context> -> <Z3_tactic> -> <Z3_goal> -> <Z3_params> -> IO <Z3_apply_result>

{- | Increment the reference counter of the given \c Z3_apply_result object. -}
foreign import ccall unsafe "Z3_apply_result_inc_ref"
  z3_apply_result_inc_ref :: <Z3_context> -> <Z3_apply_result> -> IO ()

{- | Decrement the reference counter of the given \c Z3_apply_result object. -}
foreign import ccall unsafe "Z3_apply_result_dec_ref"
  z3_apply_result_dec_ref :: <Z3_context> -> <Z3_apply_result> -> IO ()

{- | Convert the \c Z3_apply_result object returned by #Z3_tactic_apply into a string. -}
foreign import ccall unsafe "Z3_apply_result_to_string"
  z3_apply_result_to_string :: <Z3_context> -> <Z3_apply_result> -> IO <Z3_string>

{- | Return the number of subgoals in the \c Z3_apply_result object returned by #Z3_tactic_apply. -}
foreign import ccall unsafe "Z3_apply_result_get_num_subgoals"
  z3_apply_result_get_num_subgoals :: <Z3_context> -> <Z3_apply_result> -> IO ()

{- | Return one of the subgoals in the \c Z3_apply_result object returned by #Z3_tactic_apply.

       \pre i < Z3_apply_result_get_num_subgoals(c, r) -}
foreign import ccall unsafe "Z3_apply_result_get_subgoal"
  z3_apply_result_get_subgoal :: <Z3_context> -> <Z3_apply_result> -> CUInt -> IO <Z3_goal>

{- | Convert a model for the subgoal \c Z3_apply_result_get_subgoal(c, r, i) into a model for the original goal \c g.
       Where \c g is the goal used to create \c r using \c Z3_tactic_apply(c, t, g). -}
foreign import ccall unsafe "Z3_apply_result_convert_model"
  z3_apply_result_convert_model :: <Z3_context> -> <Z3_apply_result> -> CUInt -> <Z3_model> -> IO <Z3_model>

{- | Create a new (incremental) solver. This solver also uses a
       set of builtin tactics for handling the first check-sat command, and
       check-sat commands that take more than a given number of milliseconds to be solved.

       \remark User must use #Z3_solver_inc_ref and #Z3_solver_dec_ref to manage solver objects.
       Even if the context was created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
foreign import ccall unsafe "Z3_mk_solver"
  z3_mk_solver :: <Z3_context> -> IO <Z3_solver>

{- | Create a new (incremental) solver.

       The function #Z3_solver_get_model retrieves a model if the
       assertions is satisfiable (i.e., the result is \c
       Z3_L_TRUE) and model construction is enabled.
       The function #Z3_solver_get_model can also be used even
       if the result is \c Z3_L_UNDEF, but the returned model
       is not guaranteed to satisfy quantified assertions.

       \remark User must use #Z3_solver_inc_ref and #Z3_solver_dec_ref to manage solver objects.
       Even if the context was created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
foreign import ccall unsafe "Z3_mk_simple_solver"
  z3_mk_simple_solver :: <Z3_context> -> IO <Z3_solver>

{- | Create a new solver customized for the given logic.
       It behaves like #Z3_mk_solver if the logic is unknown or unsupported.

       \remark User must use #Z3_solver_inc_ref and #Z3_solver_dec_ref to manage solver objects.
       Even if the context was created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
foreign import ccall unsafe "Z3_mk_solver_for_logic"
  z3_mk_solver_for_logic :: <Z3_context> -> <Z3_symbol> -> IO <Z3_solver>

{- | Create a new solver that is implemented using the given tactic.
       The solver supports the commands #Z3_solver_push and #Z3_solver_pop, but it
       will always solve each #Z3_solver_check from scratch.

       \remark User must use #Z3_solver_inc_ref and #Z3_solver_dec_ref to manage solver objects.
       Even if the context was created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
foreign import ccall unsafe "Z3_mk_solver_from_tactic"
  z3_mk_solver_from_tactic :: <Z3_context> -> <Z3_tactic> -> IO <Z3_solver>

{- | Copy a solver \c s from the context \c source to a the context \c target. -}
foreign import ccall unsafe "Z3_solver_translate"
  z3_solver_translate :: <Z3_context> -> <Z3_solver> -> <Z3_context> -> IO <Z3_solver>

{- | Return a string describing all solver available parameters. -}
foreign import ccall unsafe "Z3_solver_get_help"
  z3_solver_get_help :: <Z3_context> -> <Z3_solver> -> IO <Z3_string>

{- | Return the parameter description set for the given solver object. -}
foreign import ccall unsafe "Z3_solver_get_param_descrs"
  z3_solver_get_param_descrs :: <Z3_context> -> <Z3_solver> -> IO <Z3_param_descrs>

{- | Set the given solver using the given parameters. -}
foreign import ccall unsafe "Z3_solver_set_params"
  z3_solver_set_params :: <Z3_context> -> <Z3_solver> -> <Z3_params> -> IO ()

{- | Increment the reference counter of the given solver. -}
foreign import ccall unsafe "Z3_solver_inc_ref"
  z3_solver_inc_ref :: <Z3_context> -> <Z3_solver> -> IO ()

{- | Decrement the reference counter of the given solver. -}
foreign import ccall unsafe "Z3_solver_dec_ref"
  z3_solver_dec_ref :: <Z3_context> -> <Z3_solver> -> IO ()

{- | Create a backtracking point.

       The solver contains a stack of assertions.

       \sa Z3_solver_pop -}
foreign import ccall unsafe "Z3_solver_push"
  z3_solver_push :: <Z3_context> -> <Z3_solver> -> IO ()

{- | Backtrack \c n backtracking points.

       \sa Z3_solver_push

       \pre n <= Z3_solver_get_num_scopes(c, s) -}
foreign import ccall unsafe "Z3_solver_pop"
  z3_solver_pop :: <Z3_context> -> <Z3_solver> -> CUInt -> IO ()

{- | Remove all assertions from the solver. -}
foreign import ccall unsafe "Z3_solver_reset"
  z3_solver_reset :: <Z3_context> -> <Z3_solver> -> IO ()

{- | Return the number of backtracking points.

       \sa Z3_solver_push
       \sa Z3_solver_pop -}
foreign import ccall unsafe "Z3_solver_get_num_scopes"
  z3_solver_get_num_scopes :: <Z3_context> -> <Z3_solver> -> IO ()

{- | Assert a constraint into the solver.

       The functions #Z3_solver_check and #Z3_solver_check_assumptions should be
       used to check whether the logical context is consistent or not. -}
foreign import ccall unsafe "Z3_solver_assert"
  z3_solver_assert :: <Z3_context> -> <Z3_solver> -> <Z3_ast> -> IO ()

{- | Assert a constraint \c a into the solver, and track it (in the unsat) core using
       the Boolean constant \c p.

       This API is an alternative to #Z3_solver_check_assumptions for extracting unsat cores.
       Both APIs can be used in the same solver. The unsat core will contain a combination
       of the Boolean variables provided using Z3_solver_assert_and_track and the Boolean literals
       provided using #Z3_solver_check_assumptions.

       \pre \c a must be a Boolean expression
       \pre \c p must be a Boolean constant (aka variable). -}
foreign import ccall unsafe "Z3_solver_assert_and_track"
  z3_solver_assert_and_track :: <Z3_context> -> <Z3_solver> -> <Z3_ast> -> <Z3_ast> -> IO ()

{- | Return the set of asserted formulas on the solver. -}
foreign import ccall unsafe "Z3_solver_get_assertions"
  z3_solver_get_assertions :: <Z3_context> -> <Z3_solver> -> IO <Z3_ast_vector>

{- | Check whether the assertions in a given solver are consistent or not.

       The function #Z3_solver_get_model retrieves a model if the
       assertions is satisfiable (i.e., the result is \c
       Z3_L_TRUE) and model construction is enabled.
       Note that if the call returns Z3_L_UNDEF, Z3 does not
       ensure that calls to #Z3_solver_get_model succeed and any models
       produced in this case are not guaranteed to satisfy the assertions.

       The function #Z3_solver_get_proof retrieves a proof if proof
       generation was enabled when the context was created, and the
       assertions are unsatisfiable (i.e., the result is \c Z3_L_FALSE). -}
foreign import ccall unsafe "Z3_solver_check"
  z3_solver_check :: <Z3_context> -> <Z3_solver> -> IO <Z3_lbool>

{- | Check whether the assertions in the given solver and
       optional assumptions are consistent or not.

       The function #Z3_solver_get_unsat_core retrieves the subset of the
       assumptions used in the unsatisfiability proof produced by Z3.

       \sa Z3_solver_check -}
foreign import ccall unsafe "Z3_solver_check_assumptions"
  z3_solver_check_assumptions :: <Z3_context> -> <Z3_solver> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_lbool>

{- | Retrieve congruence class representatives for terms.

       The function can be used for relying on Z3 to identify equal terms under the current
       set of assumptions. The array of terms and array of class identifiers should have
       the same length. The class identifiers are numerals that are assigned to the same
       value for their corresponding terms if the current context forces the terms to be
       equal. You cannot deduce that terms corresponding to different numerals must be all different,
       (especially when using non-convex theories).
       All implied equalities are returned by this call.
       This means that two terms map to the same class identifier if and only if
       the current context implies that they are equal.

       A side-effect of the function is a satisfiability check on the assertions on the solver that is passed in.
       The function return Z3_L_FALSE if the current assertions are not satisfiable. -}
foreign import ccall unsafe "Z3_get_implied_equalities"
  z3_get_implied_equalities :: <Z3_context> -> <Z3_solver> -> CUInt -> Ptr <Z3_ast> -> Ptr CUInt -> IO <Z3_lbool>

{- | retrieve consequences from solver that determine values of the supplied function symbols. -}
foreign import ccall unsafe "Z3_solver_get_consequences"
  z3_solver_get_consequences :: <Z3_context> -> <Z3_solver> -> <Z3_ast_vector> -> <Z3_ast_vector> -> <Z3_ast_vector> -> IO <Z3_lbool>

{- | Retrieve the model for the last #Z3_solver_check or #Z3_solver_check_assumptions

       The error handler is invoked if a model is not available because
       the commands above were not invoked for the given solver, or if the result was \c Z3_L_FALSE. -}
foreign import ccall unsafe "Z3_solver_get_model"
  z3_solver_get_model :: <Z3_context> -> <Z3_solver> -> IO <Z3_model>

{- | Retrieve the proof for the last #Z3_solver_check or #Z3_solver_check_assumptions

       The error handler is invoked if proof generation is not enabled,
       or if the commands above were not invoked for the given solver,
       or if the result was different from \c Z3_L_FALSE. -}
foreign import ccall unsafe "Z3_solver_get_proof"
  z3_solver_get_proof :: <Z3_context> -> <Z3_solver> -> IO <Z3_ast>

{- | Retrieve the unsat core for the last #Z3_solver_check_assumptions
       The unsat core is a subset of the assumptions \c a. -}
foreign import ccall unsafe "Z3_solver_get_unsat_core"
  z3_solver_get_unsat_core :: <Z3_context> -> <Z3_solver> -> IO <Z3_ast_vector>

{- | Return a brief justification for an "unknown" result (i.e., Z3_L_UNDEF) for
       the commands #Z3_solver_check and #Z3_solver_check_assumptions -}
foreign import ccall unsafe "Z3_solver_get_reason_unknown"
  z3_solver_get_reason_unknown :: <Z3_context> -> <Z3_solver> -> IO <Z3_string>

{- | Return statistics for the given solver.

       \remark User must use #Z3_stats_inc_ref and #Z3_stats_dec_ref to manage Z3_stats objects. -}
foreign import ccall unsafe "Z3_solver_get_statistics"
  z3_solver_get_statistics :: <Z3_context> -> <Z3_solver> -> IO <Z3_stats>

{- | Convert a solver into a string. -}
foreign import ccall unsafe "Z3_solver_to_string"
  z3_solver_to_string :: <Z3_context> -> <Z3_solver> -> IO <Z3_string>

{- | Convert a statistics into a string. -}
foreign import ccall unsafe "Z3_stats_to_string"
  z3_stats_to_string :: <Z3_context> -> <Z3_stats> -> IO <Z3_string>

{- | Increment the reference counter of the given statistics object. -}
foreign import ccall unsafe "Z3_stats_inc_ref"
  z3_stats_inc_ref :: <Z3_context> -> <Z3_stats> -> IO ()

{- | Decrement the reference counter of the given statistics object. -}
foreign import ccall unsafe "Z3_stats_dec_ref"
  z3_stats_dec_ref :: <Z3_context> -> <Z3_stats> -> IO ()

{- | Return the number of statistical data in \c s. -}
foreign import ccall unsafe "Z3_stats_size"
  z3_stats_size :: <Z3_context> -> <Z3_stats> -> IO ()

{- | Return the key (a string) for a particular statistical data.

       \pre idx < Z3_stats_size(c, s) -}
foreign import ccall unsafe "Z3_stats_get_key"
  z3_stats_get_key :: <Z3_context> -> <Z3_stats> -> CUInt -> IO <Z3_string>

{- | Return Z3_TRUE if the given statistical data is a unsigned integer.

       \pre idx < Z3_stats_size(c, s) -}
foreign import ccall unsafe "Z3_stats_is_uint"
  z3_stats_is_uint :: <Z3_context> -> <Z3_stats> -> CUInt -> IO CInt

{- | Return Z3_TRUE if the given statistical data is a double.

       \pre idx < Z3_stats_size(c, s) -}
foreign import ccall unsafe "Z3_stats_is_double"
  z3_stats_is_double :: <Z3_context> -> <Z3_stats> -> CUInt -> IO CInt

{- | Return the unsigned value of the given statistical data.

       \pre idx < Z3_stats_size(c, s) && Z3_stats_is_uint(c, s) -}
foreign import ccall unsafe "Z3_stats_get_uint_value"
  z3_stats_get_uint_value :: <Z3_context> -> <Z3_stats> -> CUInt -> IO ()

{- | Return the double value of the given statistical data.

       \pre idx < Z3_stats_size(c, s) && Z3_stats_is_double(c, s) -}
foreign import ccall unsafe "Z3_stats_get_double_value"
  z3_stats_get_double_value :: <Z3_context> -> <Z3_stats> -> CUInt -> IO CDouble

{- | Return the estimated allocated memory in bytes. -}
foreign import ccall unsafe "Z3_get_estimated_alloc_size"
  z3_get_estimated_alloc_size :: IO CULong

{- | Return Z3_TRUE if \c can be used as value in the Z3 real algebraic
       number package. -}
foreign import ccall unsafe "Z3_algebraic_is_value"
  z3_algebraic_is_value :: <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return the Z3_TRUE if \c a is positive, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a) -}
foreign import ccall unsafe "Z3_algebraic_is_pos"
  z3_algebraic_is_pos :: <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return the Z3_TRUE if \c a is negative, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a) -}
foreign import ccall unsafe "Z3_algebraic_is_neg"
  z3_algebraic_is_neg :: <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return the Z3_TRUE if \c a is zero, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a) -}
foreign import ccall unsafe "Z3_algebraic_is_zero"
  z3_algebraic_is_zero :: <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return 1 if \c a is positive, 0 if \c a is zero, and -1 if \c a is negative.

       \pre Z3_algebraic_is_value(c, a) -}
foreign import ccall unsafe "Z3_algebraic_sign"
  z3_algebraic_sign :: <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return the value a + b.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b)
       \post Z3_algebraic_is_value(c, result) -}
foreign import ccall unsafe "Z3_algebraic_add"
  z3_algebraic_add :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Return the value a - b.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b)
       \post Z3_algebraic_is_value(c, result) -}
foreign import ccall unsafe "Z3_algebraic_sub"
  z3_algebraic_sub :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Return the value a * b.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b)
       \post Z3_algebraic_is_value(c, result) -}
foreign import ccall unsafe "Z3_algebraic_mul"
  z3_algebraic_mul :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Return the value a / b.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b)
       \pre !Z3_algebraic_is_zero(c, b)
       \post Z3_algebraic_is_value(c, result) -}
foreign import ccall unsafe "Z3_algebraic_div"
  z3_algebraic_div :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Return the a^(1/k)

       \pre Z3_algebraic_is_value(c, a)
       \pre k is even => !Z3_algebraic_is_neg(c, a)
       \post Z3_algebraic_is_value(c, result) -}
foreign import ccall unsafe "Z3_algebraic_root"
  z3_algebraic_root :: <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_ast>

{- | Return the a^k

       \pre Z3_algebraic_is_value(c, a)
       \post Z3_algebraic_is_value(c, result) -}
foreign import ccall unsafe "Z3_algebraic_power"
  z3_algebraic_power :: <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_ast>

{- | Return Z3_TRUE if a < b, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b) -}
foreign import ccall unsafe "Z3_algebraic_lt"
  z3_algebraic_lt :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt

{- | Return Z3_TRUE if a > b, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b) -}
foreign import ccall unsafe "Z3_algebraic_gt"
  z3_algebraic_gt :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt

{- | Return Z3_TRUE if a <= b, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b) -}
foreign import ccall unsafe "Z3_algebraic_le"
  z3_algebraic_le :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt

{- | Return Z3_TRUE if a >= b, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b) -}
foreign import ccall unsafe "Z3_algebraic_ge"
  z3_algebraic_ge :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt

{- | Return Z3_TRUE if a == b, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b) -}
foreign import ccall unsafe "Z3_algebraic_eq"
  z3_algebraic_eq :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt

{- | Return Z3_TRUE if a != b, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b) -}
foreign import ccall unsafe "Z3_algebraic_neq"
  z3_algebraic_neq :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt

{- | Given a multivariate polynomial p(x_0, ..., x_{n-1}, x_n), returns the
       roots of the univariate polynomial p(a[0], ..., a[n-1], x_n).

       \pre p is a Z3 expression that contains only arithmetic terms and free variables.
       \pre forall i in [0, n) Z3_algebraic_is_value(c, a[i])
       \post forall r in result Z3_algebraic_is_value(c, result) -}
foreign import ccall unsafe "Z3_algebraic_roots"
  z3_algebraic_roots :: <Z3_context> -> <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast_vector>

{- | Given a multivariate polynomial p(x_0, ..., x_{n-1}), return the
       sign of p(a[0], ..., a[n-1]).

       \pre p is a Z3 expression that contains only arithmetic terms and free variables.
       \pre forall i in [0, n) Z3_algebraic_is_value(c, a[i]) -}
foreign import ccall unsafe "Z3_algebraic_eval"
  z3_algebraic_eval :: <Z3_context> -> <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> IO CInt

{- | Return an empty AST vector.

       \remark Reference counting must be used to manage AST vectors, even when the Z3_context was
       created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
foreign import ccall unsafe "Z3_mk_ast_vector"
  z3_mk_ast_vector :: <Z3_context> -> IO <Z3_ast_vector>

{- | Increment the reference counter of the given AST vector. -}
foreign import ccall unsafe "Z3_ast_vector_inc_ref"
  z3_ast_vector_inc_ref :: <Z3_context> -> <Z3_ast_vector> -> IO ()

{- | Decrement the reference counter of the given AST vector. -}
foreign import ccall unsafe "Z3_ast_vector_dec_ref"
  z3_ast_vector_dec_ref :: <Z3_context> -> <Z3_ast_vector> -> IO ()

{- | Return the size of the given AST vector. -}
foreign import ccall unsafe "Z3_ast_vector_size"
  z3_ast_vector_size :: <Z3_context> -> <Z3_ast_vector> -> IO ()

{- | Return the AST at position \c i in the AST vector \c v.

       \pre i < Z3_ast_vector_size(c, v) -}
foreign import ccall unsafe "Z3_ast_vector_get"
  z3_ast_vector_get :: <Z3_context> -> <Z3_ast_vector> -> CUInt -> IO <Z3_ast>

{- | Update position \c i of the AST vector \c v with the AST \c a.

       \pre i < Z3_ast_vector_size(c, v) -}
foreign import ccall unsafe "Z3_ast_vector_set"
  z3_ast_vector_set :: <Z3_context> -> <Z3_ast_vector> -> CUInt -> <Z3_ast> -> IO ()

{- | Resize the AST vector \c v. -}
foreign import ccall unsafe "Z3_ast_vector_resize"
  z3_ast_vector_resize :: <Z3_context> -> <Z3_ast_vector> -> CUInt -> IO ()

{- | Add the AST \c a in the end of the AST vector \c v. The size of \c v is increased by one. -}
foreign import ccall unsafe "Z3_ast_vector_push"
  z3_ast_vector_push :: <Z3_context> -> <Z3_ast_vector> -> <Z3_ast> -> IO ()

{- | Translate the AST vector \c v from context \c s into an AST vector in context \c t. -}
foreign import ccall unsafe "Z3_ast_vector_translate"
  z3_ast_vector_translate :: <Z3_context> -> <Z3_ast_vector> -> <Z3_context> -> IO <Z3_ast_vector>

{- | Convert AST vector into a string. -}
foreign import ccall unsafe "Z3_ast_vector_to_string"
  z3_ast_vector_to_string :: <Z3_context> -> <Z3_ast_vector> -> IO <Z3_string>

{- | Return an empty mapping from AST to AST

    \remark Reference counting must be used to manage AST maps, even when the Z3_context was
    created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
foreign import ccall unsafe "Z3_mk_ast_map"
  z3_mk_ast_map :: <Z3_context> -> IO <Z3_ast_map>

{- | Increment the reference counter of the given AST map. -}
foreign import ccall unsafe "Z3_ast_map_inc_ref"
  z3_ast_map_inc_ref :: <Z3_context> -> <Z3_ast_map> -> IO ()

{- | Decrement the reference counter of the given AST map. -}
foreign import ccall unsafe "Z3_ast_map_dec_ref"
  z3_ast_map_dec_ref :: <Z3_context> -> <Z3_ast_map> -> IO ()

{- | Return true if the map \c m contains the AST key \c k. -}
foreign import ccall unsafe "Z3_ast_map_contains"
  z3_ast_map_contains :: <Z3_context> -> <Z3_ast_map> -> <Z3_ast> -> IO CInt

{- | Return the value associated with the key \c k.

    The procedure invokes the error handler if \c k is not in the map. -}
foreign import ccall unsafe "Z3_ast_map_find"
  z3_ast_map_find :: <Z3_context> -> <Z3_ast_map> -> <Z3_ast> -> IO <Z3_ast>

{- | Store/Replace a new key, value pair in the given map. -}
foreign import ccall unsafe "Z3_ast_map_insert"
  z3_ast_map_insert :: <Z3_context> -> <Z3_ast_map> -> <Z3_ast> -> <Z3_ast> -> IO ()

{- | Erase a key from the map. -}
foreign import ccall unsafe "Z3_ast_map_erase"
  z3_ast_map_erase :: <Z3_context> -> <Z3_ast_map> -> <Z3_ast> -> IO ()

{- | Remove all keys from the given map. -}
foreign import ccall unsafe "Z3_ast_map_reset"
  z3_ast_map_reset :: <Z3_context> -> <Z3_ast_map> -> IO ()

{- | Return the size of the given map. -}
foreign import ccall unsafe "Z3_ast_map_size"
  z3_ast_map_size :: <Z3_context> -> <Z3_ast_map> -> IO ()

{- | Return the keys stored in the given map. -}
foreign import ccall unsafe "Z3_ast_map_keys"
  z3_ast_map_keys :: <Z3_context> -> <Z3_ast_map> -> IO <Z3_ast_vector>

{- | Convert the given map into a string. -}
foreign import ccall unsafe "Z3_ast_map_to_string"
  z3_ast_map_to_string :: <Z3_context> -> <Z3_ast_map> -> IO <Z3_string>

{- | Create a new fixedpoint context.

       \remark User must use #Z3_fixedpoint_inc_ref and #Z3_fixedpoint_dec_ref to manage fixedpoint objects.
       Even if the context was created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
foreign import ccall unsafe "Z3_mk_fixedpoint"
  z3_mk_fixedpoint :: <Z3_context> -> IO <Z3_fixedpoint>

{- | Increment the reference counter of the given fixedpoint context -}
foreign import ccall unsafe "Z3_fixedpoint_inc_ref"
  z3_fixedpoint_inc_ref :: <Z3_context> -> <Z3_fixedpoint> -> IO ()

{- | Decrement the reference counter of the given fixedpoint context. -}
foreign import ccall unsafe "Z3_fixedpoint_dec_ref"
  z3_fixedpoint_dec_ref :: <Z3_context> -> <Z3_fixedpoint> -> IO ()

{- | Add a universal Horn clause as a named rule.
       The \c horn_rule should be of the form:

       \code
           horn_rule ::= (forall (bound-vars) horn_rule)
                      |  (=> atoms horn_rule)
                      |  atom
       \endcode -}
foreign import ccall unsafe "Z3_fixedpoint_add_rule"
  z3_fixedpoint_add_rule :: <Z3_context> -> <Z3_fixedpoint> -> <Z3_ast> -> <Z3_symbol> -> IO ()

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
foreign import ccall unsafe "Z3_fixedpoint_add_fact"
  z3_fixedpoint_add_fact :: <Z3_context> -> <Z3_fixedpoint> -> <Z3_func_decl> -> CUInt -> Ptr CUInt -> IO ()

{- | Assert a constraint to the fixedpoint context.

       The constraints are used as background axioms when the fixedpoint engine uses the PDR mode.
       They are ignored for standard Datalog mode. -}
foreign import ccall unsafe "Z3_fixedpoint_assert"
  z3_fixedpoint_assert :: <Z3_context> -> <Z3_fixedpoint> -> <Z3_ast> -> IO ()

{- | Pose a query against the asserted rules.

        \code
           query ::= (exists (bound-vars) query)
                 |  literals
        \endcode

        query returns
        - Z3_L_FALSE if the query is unsatisfiable.
        - Z3_L_TRUE if the query is satisfiable. Obtain the answer by calling #Z3_fixedpoint_get_answer.
        - Z3_L_UNDEF if the query was interrupted, timed out or otherwise failed. -}
foreign import ccall unsafe "Z3_fixedpoint_query"
  z3_fixedpoint_query :: <Z3_context> -> <Z3_fixedpoint> -> <Z3_ast> -> IO <Z3_lbool>

{- | Pose multiple queries against the asserted rules.

        The queries are encoded as relations (function declarations).

        query returns
        - Z3_L_FALSE if the query is unsatisfiable.
        - Z3_L_TRUE if the query is satisfiable. Obtain the answer by calling #Z3_fixedpoint_get_answer.
        - Z3_L_UNDEF if the query was interrupted, timed out or otherwise failed. -}
foreign import ccall unsafe "Z3_fixedpoint_query_relations"
  z3_fixedpoint_query_relations :: <Z3_context> -> <Z3_fixedpoint> -> CUInt -> Ptr <Z3_func_decl> -> IO <Z3_lbool>

{- | Retrieve a formula that encodes satisfying answers to the query.


       When used in Datalog mode, the returned answer is a disjunction of conjuncts.
       Each conjunct encodes values of the bound variables of the query that are satisfied.
       In PDR mode, the returned answer is a single conjunction.

       When used in Datalog mode the previous call to Z3_fixedpoint_query must have returned Z3_L_TRUE.
       When used with the PDR engine, the previous call must have been either Z3_L_TRUE or Z3_L_FALSE. -}
foreign import ccall unsafe "Z3_fixedpoint_get_answer"
  z3_fixedpoint_get_answer :: <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_ast>

{- | Retrieve a string that describes the last status returned by #Z3_fixedpoint_query.

       Use this method when #Z3_fixedpoint_query returns Z3_L_UNDEF. -}
foreign import ccall unsafe "Z3_fixedpoint_get_reason_unknown"
  z3_fixedpoint_get_reason_unknown :: <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_string>

{- | Update a named rule.
       A rule with the same name must have been previously created. -}
foreign import ccall unsafe "Z3_fixedpoint_update_rule"
  z3_fixedpoint_update_rule :: <Z3_context> -> <Z3_fixedpoint> -> <Z3_ast> -> <Z3_symbol> -> IO ()

{- | Query the PDR engine for the maximal levels properties are known about predicate.

       This call retrieves the maximal number of relevant unfoldings
       of \c pred with respect to the current exploration state.
       Note: this functionality is PDR specific. -}
foreign import ccall unsafe "Z3_fixedpoint_get_num_levels"
  z3_fixedpoint_get_num_levels :: <Z3_context> -> <Z3_fixedpoint> -> <Z3_func_decl> -> IO ()
foreign import ccall unsafe "Z3_fixedpoint_get_cover_delta"
  z3_fixedpoint_get_cover_delta :: <Z3_context> -> <Z3_fixedpoint> -> CInt -> <Z3_func_decl> -> IO <Z3_ast>

{- | Add property about the predicate \c pred.
       Add a property of predicate \c pred at \c level.
       It gets pushed forward when possible.

       Note: level = -1 is treated as the fixedpoint. So passing -1 for the \c level
       means that the property is true of the fixed-point unfolding with respect to \c pred.

       Note: this functionality is PDR specific. -}
foreign import ccall unsafe "Z3_fixedpoint_add_cover"
  z3_fixedpoint_add_cover :: <Z3_context> -> <Z3_fixedpoint> -> CInt -> <Z3_func_decl> -> <Z3_ast> -> IO ()

{- | Retrieve statistics information from the last call to #Z3_fixedpoint_query. -}
foreign import ccall unsafe "Z3_fixedpoint_get_statistics"
  z3_fixedpoint_get_statistics :: <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_stats>

{- | Register relation as Fixedpoint defined.
       Fixedpoint defined relations have least-fixedpoint semantics.
       For example, the relation is empty if it does not occur
       in a head or a fact. -}
foreign import ccall unsafe "Z3_fixedpoint_register_relation"
  z3_fixedpoint_register_relation :: <Z3_context> -> <Z3_fixedpoint> -> <Z3_func_decl> -> IO ()

{- | Configure the predicate representation.

       It sets the predicate to use a set of domains given by the list of symbols.
       The domains given by the list of symbols must belong to a set
       of built-in domains. -}
foreign import ccall unsafe "Z3_fixedpoint_set_predicate_representation"
  z3_fixedpoint_set_predicate_representation :: <Z3_context> -> <Z3_fixedpoint> -> <Z3_func_decl> -> CUInt -> Ptr <Z3_symbol> -> IO ()

{- | Retrieve set of rules from fixedpoint context. -}
foreign import ccall unsafe "Z3_fixedpoint_get_rules"
  z3_fixedpoint_get_rules :: <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_ast_vector>

{- | Retrieve set of background assertions from fixedpoint context. -}
foreign import ccall unsafe "Z3_fixedpoint_get_assertions"
  z3_fixedpoint_get_assertions :: <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_ast_vector>

{- | Set parameters on fixedpoint context. -}
foreign import ccall unsafe "Z3_fixedpoint_set_params"
  z3_fixedpoint_set_params :: <Z3_context> -> <Z3_fixedpoint> -> <Z3_params> -> IO ()

{- | Return a string describing all fixedpoint available parameters. -}
foreign import ccall unsafe "Z3_fixedpoint_get_help"
  z3_fixedpoint_get_help :: <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_string>

{- | Return the parameter description set for the given fixedpoint object. -}
foreign import ccall unsafe "Z3_fixedpoint_get_param_descrs"
  z3_fixedpoint_get_param_descrs :: <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_param_descrs>

{- | Print the current rules and background axioms as a string.
       \param c - context.
       \param f - fixedpoint context.
       \param num_queries - number of additional queries to print.
       \param queries - additional queries. -}
foreign import ccall unsafe "Z3_fixedpoint_to_string"
  z3_fixedpoint_to_string :: <Z3_context> -> <Z3_fixedpoint> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_string>

{- | Parse an SMT-LIB2 string with fixedpoint rules.
       Add the rules to the current fixedpoint context.
       Return the set of queries in the string.

       \param c - context.
       \param f - fixedpoint context.
       \param s - string containing SMT2 specification. -}
foreign import ccall unsafe "Z3_fixedpoint_from_string"
  z3_fixedpoint_from_string :: <Z3_context> -> <Z3_fixedpoint> -> <Z3_string> -> IO <Z3_ast_vector>

{- | Parse an SMT-LIB2 file with fixedpoint rules.
       Add the rules to the current fixedpoint context.
       Return the set of queries in the file.

       \param c - context.
       \param f - fixedpoint context.
       \param s - string containing SMT2 specification. -}
foreign import ccall unsafe "Z3_fixedpoint_from_file"
  z3_fixedpoint_from_file :: <Z3_context> -> <Z3_fixedpoint> -> <Z3_string> -> IO <Z3_ast_vector>

{- | Create a backtracking point.

       The fixedpoint solver contains a set of rules, added facts and assertions.
       The set of rules, facts and assertions are restored upon calling #Z3_fixedpoint_pop.

       \sa Z3_fixedpoint_pop -}
foreign import ccall unsafe "Z3_fixedpoint_push"
  z3_fixedpoint_push :: <Z3_context> -> <Z3_fixedpoint> -> IO ()

{- | Backtrack one backtracking point.

       \sa Z3_fixedpoint_push

       \pre The number of calls to pop cannot exceed calls to push. -}
foreign import ccall unsafe "Z3_fixedpoint_pop"
  z3_fixedpoint_pop :: <Z3_context> -> <Z3_fixedpoint> -> IO ()

{- | The following utilities allows adding user-defined domains. -}
foreign import ccall unsafe "Z3_fixedpoint_init"
  z3_fixedpoint_init :: <Z3_context> -> <Z3_fixedpoint> -> Ptr () -> IO ()

{- | Register a callback to destructive updates.

       Registers are identified with terms encoded as fresh constants, -}
foreign import ccall unsafe "Z3_fixedpoint_set_reduce_assign_callback"
  z3_fixedpoint_set_reduce_assign_callback :: <Z3_context> -> <Z3_fixedpoint> -> <Z3_fixedpoint_reduce_assign_callback_fptr> -> IO ()

{- | Register a callback for buildling terms based on the relational operators. -}
foreign import ccall unsafe "Z3_fixedpoint_set_reduce_app_callback"
  z3_fixedpoint_set_reduce_app_callback :: <Z3_context> -> <Z3_fixedpoint> -> <Z3_fixedpoint_reduce_app_callback_fptr> -> IO ()

{- | Create the RoundingMode sort.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_rounding_mode_sort"
  z3_mk_fpa_rounding_mode_sort :: <Z3_context> -> IO <Z3_sort>

{- | Create a numeral of RoundingMode sort which represents the NearestTiesToEven rounding mode.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_round_nearest_ties_to_even"
  z3_mk_fpa_round_nearest_ties_to_even :: <Z3_context> -> IO <Z3_ast>

{- | Create a numeral of RoundingMode sort which represents the NearestTiesToEven rounding mode.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_rne"
  z3_mk_fpa_rne :: <Z3_context> -> IO <Z3_ast>

{- | Create a numeral of RoundingMode sort which represents the NearestTiesToAway rounding mode.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_round_nearest_ties_to_away"
  z3_mk_fpa_round_nearest_ties_to_away :: <Z3_context> -> IO <Z3_ast>

{- | Create a numeral of RoundingMode sort which represents the NearestTiesToAway rounding mode.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_rna"
  z3_mk_fpa_rna :: <Z3_context> -> IO <Z3_ast>

{- | Create a numeral of RoundingMode sort which represents the TowardPositive rounding mode.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_round_toward_positive"
  z3_mk_fpa_round_toward_positive :: <Z3_context> -> IO <Z3_ast>

{- | Create a numeral of RoundingMode sort which represents the TowardPositive rounding mode.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_rtp"
  z3_mk_fpa_rtp :: <Z3_context> -> IO <Z3_ast>

{- | Create a numeral of RoundingMode sort which represents the TowardNegative rounding mode.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_round_toward_negative"
  z3_mk_fpa_round_toward_negative :: <Z3_context> -> IO <Z3_ast>

{- | Create a numeral of RoundingMode sort which represents the TowardNegative rounding mode.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_rtn"
  z3_mk_fpa_rtn :: <Z3_context> -> IO <Z3_ast>

{- | Create a numeral of RoundingMode sort which represents the TowardZero rounding mode.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_round_toward_zero"
  z3_mk_fpa_round_toward_zero :: <Z3_context> -> IO <Z3_ast>

{- | Create a numeral of RoundingMode sort which represents the TowardZero rounding mode.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_rtz"
  z3_mk_fpa_rtz :: <Z3_context> -> IO <Z3_ast>

{- | Create a FloatingPoint sort.

        \param c logical context
        \param ebits number of exponent bits
        \param sbits number of significand bits

        \remark ebits must be larger than 1 and sbits must be larger than 2. -}
foreign import ccall unsafe "Z3_mk_fpa_sort"
  z3_mk_fpa_sort :: <Z3_context> -> CUInt -> CUInt -> IO <Z3_sort>

{- | Create the half-precision (16-bit) FloatingPoint sort.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_sort_half"
  z3_mk_fpa_sort_half :: <Z3_context> -> IO <Z3_sort>

{- | Create the half-precision (16-bit) FloatingPoint sort.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_sort_16"
  z3_mk_fpa_sort_16 :: <Z3_context> -> IO <Z3_sort>

{- | Create the single-precision (32-bit) FloatingPoint sort.

        \param c logical context. -}
foreign import ccall unsafe "Z3_mk_fpa_sort_single"
  z3_mk_fpa_sort_single :: <Z3_context> -> IO <Z3_sort>

{- | Create the single-precision (32-bit) FloatingPoint sort.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_sort_32"
  z3_mk_fpa_sort_32 :: <Z3_context> -> IO <Z3_sort>

{- | Create the double-precision (64-bit) FloatingPoint sort.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_sort_double"
  z3_mk_fpa_sort_double :: <Z3_context> -> IO <Z3_sort>

{- | Create the double-precision (64-bit) FloatingPoint sort.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_sort_64"
  z3_mk_fpa_sort_64 :: <Z3_context> -> IO <Z3_sort>

{- | Create the quadruple-precision (128-bit) FloatingPoint sort.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_sort_quadruple"
  z3_mk_fpa_sort_quadruple :: <Z3_context> -> IO <Z3_sort>

{- | Create the quadruple-precision (128-bit) FloatingPoint sort.

        \param c logical context -}
foreign import ccall unsafe "Z3_mk_fpa_sort_128"
  z3_mk_fpa_sort_128 :: <Z3_context> -> IO <Z3_sort>

{- | Create a floating-point NaN of sort s.

        \param c logical context
        \param s target sort -}
foreign import ccall unsafe "Z3_mk_fpa_nan"
  z3_mk_fpa_nan :: <Z3_context> -> <Z3_sort> -> IO <Z3_ast>

{- | Create a floating-point infinity of sort s.

        \param c logical context
        \param s target sort
        \param negative indicates whether the result should be negative

        When \c negative is true, -oo will be generated instead of +oo. -}
foreign import ccall unsafe "Z3_mk_fpa_inf"
  z3_mk_fpa_inf :: <Z3_context> -> <Z3_sort> -> CInt -> IO <Z3_ast>

{- | Create a floating-point zero of sort s.

        \param c logical context
        \param s target sort
        \param negative indicates whether the result should be negative

        When \c negative is true, -zero will be generated instead of +zero. -}
foreign import ccall unsafe "Z3_mk_fpa_zero"
  z3_mk_fpa_zero :: <Z3_context> -> <Z3_sort> -> CInt -> IO <Z3_ast>

{- | Create an expression of FloatingPoint sort from three bit-vector expressions.

        This is the operator named `fp' in the SMT FP theory definition.
        Note that \c sign is required to be a bit-vector of size 1. Significand and exponent
        are required to be greater than 1 and 2 respectively. The FloatingPoint sort
        of the resulting expression is automatically determined from the bit-vector sizes
        of the arguments.

        \param c logical context
        \param sgn sign
        \param exp exponent
        \param sig significand -}
foreign import ccall unsafe "Z3_mk_fpa_fp"
  z3_mk_fpa_fp :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a numeral of FloatingPoint sort from a float.

        This function is used to create numerals that fit in a float value.
        It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string.

        \param c logical context
        \param v value
        \param ty sort

        ty must be a FloatingPoint sort

        \sa Z3_mk_numeral -}
foreign import ccall unsafe "Z3_mk_fpa_numeral_float"
  z3_mk_fpa_numeral_float :: <Z3_context> -> CFloat -> <Z3_sort> -> IO <Z3_ast>

{- | Create a numeral of FloatingPoint sort from a double.

        This function is used to create numerals that fit in a double value.
        It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string.

        \param c logical context
        \param v value
        \param ty sort

        ty must be a FloatingPoint sort

        \sa Z3_mk_numeral -}
foreign import ccall unsafe "Z3_mk_fpa_numeral_double"
  z3_mk_fpa_numeral_double :: <Z3_context> -> CDouble -> <Z3_sort> -> IO <Z3_ast>

{- | Create a numeral of FloatingPoint sort from a signed integer.

        \param c logical context
        \param v value
        \param ty result sort

        ty must be a FloatingPoint sort

        \sa Z3_mk_numeral -}
foreign import ccall unsafe "Z3_mk_fpa_numeral_int"
  z3_mk_fpa_numeral_int :: <Z3_context> -> CInt -> <Z3_sort> -> IO <Z3_ast>

{- | Create a numeral of FloatingPoint sort from a sign bit and two integers.

        \param c logical context
        \param sgn sign bit (true == negative)
        \param sig significand
        \param exp exponent
        \param ty result sort

        ty must be a FloatingPoint sort

        \sa Z3_mk_numeral -}
foreign import ccall unsafe "Z3_mk_fpa_numeral_int_uint"
  z3_mk_fpa_numeral_int_uint :: <Z3_context> -> CInt -> CInt -> CUInt -> <Z3_sort> -> IO <Z3_ast>

{- | Create a numeral of FloatingPoint sort from a sign bit and two 64-bit integers.

        \param c logical context
        \param sgn sign bit (true == negative)
        \param sig significand
        \param exp exponent
        \param ty result sort

        ty must be a FloatingPoint sort

        \sa Z3_mk_numeral -}
foreign import ccall unsafe "Z3_mk_fpa_numeral_int64_uint64"
  z3_mk_fpa_numeral_int64_uint64 :: <Z3_context> -> CInt -> CLong -> CULong -> <Z3_sort> -> IO <Z3_ast>

{- | Floating-point absolute value

        \param c logical context
        \param t term of FloatingPoint sort -}
foreign import ccall unsafe "Z3_mk_fpa_abs"
  z3_mk_fpa_abs :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Floating-point negation

        \param c logical context
        \param t term of FloatingPoint sort -}
foreign import ccall unsafe "Z3_mk_fpa_neg"
  z3_mk_fpa_neg :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Floating-point addition

        \param c logical context
        \param rm term of RoundingMode sort
        \param t1 term of FloatingPoint sort
        \param t2 term of FloatingPoint sort

        rm must be of RoundingMode sort, t1 and t2 must have the same FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_add"
  z3_mk_fpa_add :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Floating-point subtraction

        \param c logical context
        \param rm term of RoundingMode sort
        \param t1 term of FloatingPoint sort
        \param t2 term of FloatingPoint sort

        rm must be of RoundingMode sort, t1 and t2 must have the same FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_sub"
  z3_mk_fpa_sub :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Floating-point multiplication

        \param c logical context
        \param rm term of RoundingMode sort
        \param t1 term of FloatingPoint sort
        \param t2 term of FloatingPoint sort

        rm must be of RoundingMode sort, t1 and t2 must have the same FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_mul"
  z3_mk_fpa_mul :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Floating-point division

        \param c logical context
        \param rm term of RoundingMode sort
        \param t1 term of FloatingPoint sort.
        \param t2 term of FloatingPoint sort

        The nodes rm must be of RoundingMode sort t1 and t2 must have the same FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_div"
  z3_mk_fpa_div :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Floating-point fused multiply-add.

        \param c logical context
        \param rm term of RoundingMode sort
        \param t1 term of FloatingPoint sort
        \param t2 term of FloatingPoint sor
        \param t3 term of FloatingPoint sort

        The result is round((t1 * t2) + t3)

        rm must be of RoundingMode sort, t1, t2, and t3 must have the same FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_fma"
  z3_mk_fpa_fma :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Floating-point square root

        \param c logical context
        \param rm term of RoundingMode sort
        \param t term of FloatingPoint sort

        rm must be of RoundingMode sort, t must have FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_sqrt"
  z3_mk_fpa_sqrt :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Floating-point remainder

        \param c logical context
        \param t1 term of FloatingPoint sort
        \param t2 term of FloatingPoint sort

        t1 and t2 must have the same FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_rem"
  z3_mk_fpa_rem :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Floating-point roundToIntegral. Rounds a floating-point number to
        the closest integer, again represented as a floating-point number.

        \param c logical context
        \param rm term of RoundingMode sort
        \param t term of FloatingPoint sort

        t must be of FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_round_to_integral"
  z3_mk_fpa_round_to_integral :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Minimum of floating-point numbers.

        \param c logical context
        \param t1 term of FloatingPoint sort
        \param t2 term of FloatingPoint sort

        t1, t2 must have the same FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_min"
  z3_mk_fpa_min :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Maximum of floating-point numbers.

        \param c logical context
        \param t1 term of FloatingPoint sort
        \param t2 term of FloatingPoint sort

        t1, t2 must have the same FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_max"
  z3_mk_fpa_max :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Floating-point less than or equal.

        \param c logical context
        \param t1 term of FloatingPoint sort
        \param t2 term of FloatingPoint sort

        t1 and t2 must have the same FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_leq"
  z3_mk_fpa_leq :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Floating-point less than.

        \param c logical context
        \param t1 term of FloatingPoint sort
        \param t2 term of FloatingPoint sort

        t1 and t2 must have the same FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_lt"
  z3_mk_fpa_lt :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Floating-point greater than or equal.

        \param c logical context
        \param t1 term of FloatingPoint sort
        \param t2 term of FloatingPoint sort

        t1 and t2 must have the same FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_geq"
  z3_mk_fpa_geq :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Floating-point greater than.

        \param c logical context
        \param t1 term of FloatingPoint sort
        \param t2 term of FloatingPoint sort

        t1 and t2 must have the same FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_gt"
  z3_mk_fpa_gt :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Floating-point equality.

        \param c logical context
        \param t1 term of FloatingPoint sort
        \param t2 term of FloatingPoint sort

        Note that this is IEEE 754 equality (as opposed to SMT-LIB =).

        t1 and t2 must have the same FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_eq"
  z3_mk_fpa_eq :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Predicate indicating whether t is a normal floating-point number.

        \param c logical context
        \param t term of FloatingPoint sort

        t must have FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_is_normal"
  z3_mk_fpa_is_normal :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Predicate indicating whether t is a subnormal floating-point number.

        \param c logical context
        \param t term of FloatingPoint sort

        t must have FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_is_subnormal"
  z3_mk_fpa_is_subnormal :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Predicate indicating whether t is a floating-point number with zero value, i.e., +zero or -zero.

        \param c logical context
        \param t term of FloatingPoint sort

        t must have FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_is_zero"
  z3_mk_fpa_is_zero :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Predicate indicating whether t is a floating-point number representing +oo or -oo.

        \param c logical context
        \param t term of FloatingPoint sort

        t must have FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_is_infinite"
  z3_mk_fpa_is_infinite :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Predicate indicating whether t is a NaN.

        \param c logical context
        \param t term of FloatingPoint sort

        t must have FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_is_nan"
  z3_mk_fpa_is_nan :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Predicate indicating whether t is a negative floating-point number.

        \param c logical context
        \param t term of FloatingPoint sort

        t must have FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_is_negative"
  z3_mk_fpa_is_negative :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Predicate indicating whether t is a positive floating-point number.

        \param c logical context
        \param t term of FloatingPoint sort

        t must have FloatingPoint sort. -}
foreign import ccall unsafe "Z3_mk_fpa_is_positive"
  z3_mk_fpa_is_positive :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Conversion of a single IEEE 754-2008 bit-vector into a floating-point number.

        Produces a term that represents the conversion of a bit-vector term bv to a
        floating-point term of sort s.

        \param c logical context
        \param bv a bit-vector term
        \param s floating-point sort

        s must be a FloatingPoint sort, t must be of bit-vector sort, and the bit-vector
        size of bv must be equal to ebits+sbits of s. The format of the bit-vector is
        as defined by the IEEE 754-2008 interchange format. -}
foreign import ccall unsafe "Z3_mk_fpa_to_fp_bv"
  z3_mk_fpa_to_fp_bv :: <Z3_context> -> <Z3_ast> -> <Z3_sort> -> IO <Z3_ast>

{- | Conversion of a FloatingPoint term into another term of different FloatingPoint sort.

        Produces a term that represents the conversion of a floating-point term t to a
        floating-point term of sort s. If necessary, the result will be rounded according
        to rounding mode rm.

        \param c logical context
        \param rm term of RoundingMode sort
        \param t term of FloatingPoint sort
        \param s floating-point sort

        s must be a FloatingPoint sort, rm must be of RoundingMode sort, t must be of floating-point sort. -}
foreign import ccall unsafe "Z3_mk_fpa_to_fp_float"
  z3_mk_fpa_to_fp_float :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_sort> -> IO <Z3_ast>

{- | Conversion of a term of real sort into a term of FloatingPoint sort.

        Produces a term that represents the conversion of term t of real sort into a
        floating-point term of sort s. If necessary, the result will be rounded according
        to rounding mode rm.

        \param c logical context
        \param rm term of RoundingMode sort
        \param t term of Real sort
        \param s floating-point sort

        s must be a FloatingPoint sort, rm must be of RoundingMode sort, t must be of real sort. -}
foreign import ccall unsafe "Z3_mk_fpa_to_fp_real"
  z3_mk_fpa_to_fp_real :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_sort> -> IO <Z3_ast>

{- | Conversion of a 2's complement signed bit-vector term into a term of FloatingPoint sort.

        Produces a term that represents the conversion of the bit-vector term t into a
        floating-point term of sort s. The bit-vector t is taken to be in signed
        2's complement format. If necessary, the result will be rounded according
        to rounding mode rm.

        \param c logical context
        \param rm term of RoundingMode sort
        \param t term of bit-vector sort
        \param s floating-point sort

        s must be a FloatingPoint sort, rm must be of RoundingMode sort, t must be of bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_fpa_to_fp_signed"
  z3_mk_fpa_to_fp_signed :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_sort> -> IO <Z3_ast>

{- | Conversion of a 2's complement unsigned bit-vector term into a term of FloatingPoint sort.

        Produces a term that represents the conversion of the bit-vector term t into a
        floating-point term of sort s. The bit-vector t is taken to be in unsigned
        2's complement format. If necessary, the result will be rounded according
        to rounding mode rm.

        \param c logical context
        \param rm term of RoundingMode sort
        \param t term of bit-vector sort
        \param s floating-point sort

        s must be a FloatingPoint sort, rm must be of RoundingMode sort, t must be of bit-vector sort. -}
foreign import ccall unsafe "Z3_mk_fpa_to_fp_unsigned"
  z3_mk_fpa_to_fp_unsigned :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_sort> -> IO <Z3_ast>

{- | Conversion of a floating-point term into an unsigned bit-vector.

        Produces a term that represents the conversion of the floating-poiunt term t into a
        bit-vector term of size sz in unsigned 2's complement format. If necessary, the result
        will be rounded according to rounding mode rm.

        \param c logical context
        \param rm term of RoundingMode sort
        \param t term of FloatingPoint sort
        \param sz size of the resulting bit-vector -}
foreign import ccall unsafe "Z3_mk_fpa_to_ubv"
  z3_mk_fpa_to_ubv :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> CUInt -> IO <Z3_ast>

{- | Conversion of a floating-point term into a signed bit-vector.

        Produces a term that represents the conversion of the floating-poiunt term t into a
        bit-vector term of size sz in signed 2's complement format. If necessary, the result
        will be rounded according to rounding mode rm.

        \param c logical context
        \param rm term of RoundingMode sort
        \param t term of FloatingPoint sort
        \param sz size of the resulting bit-vector -}
foreign import ccall unsafe "Z3_mk_fpa_to_sbv"
  z3_mk_fpa_to_sbv :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> CUInt -> IO <Z3_ast>

{- | Conversion of a floating-point term into a real-numbered term.

        Produces a term that represents the conversion of the floating-poiunt term t into a
        real number. Note that this type of conversion will often result in non-linear
        constraints over real terms.

        \param c logical context
        \param t term of FloatingPoint sort -}
foreign import ccall unsafe "Z3_mk_fpa_to_real"
  z3_mk_fpa_to_real :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Retrieves the number of bits reserved for the exponent in a FloatingPoint sort.

        \param c logical context
        \param s FloatingPoint sort -}
foreign import ccall unsafe "Z3_fpa_get_ebits"
  z3_fpa_get_ebits :: <Z3_context> -> <Z3_sort> -> IO ()

{- | Retrieves the number of bits reserved for the significand in a FloatingPoint sort.

        \param c logical context
        \param s FloatingPoint sort -}
foreign import ccall unsafe "Z3_fpa_get_sbits"
  z3_fpa_get_sbits :: <Z3_context> -> <Z3_sort> -> IO ()

{- | Retrieves the sign of a floating-point literal.

        \param c logical context
        \param t a floating-point numeral
        \param sgn sign

        Remarks: sets \c sgn to 0 if `t' is positive and to 1 otherwise, except for
        NaN, which is an invalid argument. -}
foreign import ccall unsafe "Z3_fpa_get_numeral_sign"
  z3_fpa_get_numeral_sign :: <Z3_context> -> <Z3_ast> -> Ptr CInt -> IO CInt

{- | Return the significand value of a floating-point numeral as a string.

        \param c logical context
        \param t a floating-point numeral

        Remarks: The significand s is always 0.0 <= s < 2.0; the resulting string is long
        enough to represent the real significand precisely. -}
foreign import ccall unsafe "Z3_fpa_get_numeral_significand_string"
  z3_fpa_get_numeral_significand_string :: <Z3_context> -> <Z3_ast> -> IO <Z3_string>

{- | Return the significand value of a floating-point numeral as a uint64.

        \param c logical context
        \param t a floating-point numeral
        \param n pointer to output uint64

        Remarks: This function extracts the significand bits in `t`, without the
        hidden bit or normalization. Sets the Z3_INVALID_ARG error code if the
        significand does not fit into a uint64. -}
foreign import ccall unsafe "Z3_fpa_get_numeral_significand_uint64"
  z3_fpa_get_numeral_significand_uint64 :: <Z3_context> -> <Z3_ast> -> Ptr CULong -> IO CInt

{- | Return the exponent value of a floating-point numeral as a string

        \param c logical context
        \param t a floating-point numeral

        Remarks: This function extracts the exponent in `t`, without normalization. -}
foreign import ccall unsafe "Z3_fpa_get_numeral_exponent_string"
  z3_fpa_get_numeral_exponent_string :: <Z3_context> -> <Z3_ast> -> IO <Z3_string>

{- | Return the exponent value of a floating-point numeral as a signed 64-bit integer

        \param c logical context
        \param t a floating-point numeral
        \param n exponent

        Remarks: This function extracts the exponent in `t`, without normalization. -}
foreign import ccall unsafe "Z3_fpa_get_numeral_exponent_int64"
  z3_fpa_get_numeral_exponent_int64 :: <Z3_context> -> <Z3_ast> -> Ptr CLong -> IO CInt

{- | Conversion of a floating-point term into a bit-vector term in IEEE 754-2008 format.

        \param c logical context
        \param t term of FloatingPoint sort

        t must have FloatingPoint sort. The size of the resulting bit-vector is automatically
        determined.

        Note that IEEE 754-2008 allows multiple different representations of NaN. This conversion
        knows only one NaN and it will always produce the same bit-vector represenatation of
        that NaN. -}
foreign import ccall unsafe "Z3_mk_fpa_to_ieee_bv"
  z3_mk_fpa_to_ieee_bv :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Conversion of a real-sorted significand and an integer-sorted exponent into a term of FloatingPoint sort.

        Produces a term that represents the conversion of sig * 2^exp into a
        floating-point term of sort s. If necessary, the result will be rounded
        according to rounding mode rm.

        \param c logical context
        \param rm term of RoundingMode sort
        \param exp exponent term of Int sort
        \param sig significand term of Real sort
        \param s FloatingPoint sort

        s must be a FloatingPoint sort, rm must be of RoundingMode sort, exp must be of int sort, sig must be of real sort. -}
foreign import ccall unsafe "Z3_mk_fpa_to_fp_int_real"
  z3_mk_fpa_to_fp_int_real :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> <Z3_sort> -> IO <Z3_ast>

{- | Create an AST node marking a formula position for interpolation.

    The node \c a must have Boolean sort. -}
foreign import ccall unsafe "Z3_mk_interpolant"
  z3_mk_interpolant :: <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | This function generates a Z3 context suitable for generation of
    interpolants. Formulas can be generated as abstract syntax trees in
    this context using the Z3 C API.

    Interpolants are also generated as AST's in this context.

    If cfg is non-null, it will be used as the base configuration
    for the Z3 context. This makes it possible to set Z3 options
    to be used during interpolation. This feature should be used
    with some caution however, as it may be that certain Z3 options
    are incompatible with interpolation. -}
foreign import ccall unsafe "Z3_mk_interpolation_context"
  z3_mk_interpolation_context :: <Z3_config> -> IO <Z3_context>
foreign import ccall unsafe "Z3_get_interpolant"
  z3_get_interpolant :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_params> -> IO <Z3_ast_vector>
foreign import ccall unsafe "Z3_compute_interpolant"
  z3_compute_interpolant :: <Z3_context> -> <Z3_ast> -> <Z3_params> -> Ptr <Z3_ast_vector> -> Ptr <Z3_model> -> IO <Z3_lbool>
foreign import ccall unsafe "Z3_interpolation_profile"
  z3_interpolation_profile :: <Z3_context> -> IO <Z3_string>

{- | Read an interpolation problem from file.

       \param ctx The Z3 context. This resets the error handler of ctx.
       \param filename The file name to read.
       \param num Returns length of sequence.
       \param cnsts Returns sequence of formulas (do not free)
       \param parents Returns the parents vector (or NULL for sequence)
       \param error Returns an error message in case of failure (do not free the string)
       \param num_theory Number of theory terms
       \param theory Theory terms

       Returns true on success.

       File formats: Currently two formats are supported, based on
       SMT-LIB2. For sequence interpolants, the sequence of constraints is
       represented by the sequence of "assert" commands in the file.

       For tree interpolants, one symbol of type bool is associated to
       each vertex of the tree. For each vertex v there is an "assert"
       of the form:

       (implies (and c1 ... cn f) v)

       where c1 .. cn are the children of v (which must precede v in the file)
       and f is the formula assiciated to node v. The last formula in the
       file is the root vertex, and is represented by the predicate "false".

       A solution to a tree interpolation problem can be thought of as a
       valuation of the vertices that makes all the implications true
       where each value is represented using the common symbols between
       the formulas in the subtree and the remainder of the formulas. -}
foreign import ccall unsafe "Z3_read_interpolation_problem"
  z3_read_interpolation_problem :: <Z3_context> -> Ptr CUInt -> Ptr (Ptr <Z3_ast>) -> Ptr (Ptr CUInt) -> <Z3_string> -> <Z3_string_ptr> -> Ptr CUInt -> Ptr (Ptr <Z3_ast>) -> IO CInt
foreign import ccall unsafe "Z3_check_interpolant"
  z3_check_interpolant :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> Ptr CUInt -> Ptr <Z3_ast> -> <Z3_string_ptr> -> CUInt -> Ptr <Z3_ast> -> IO CInt
foreign import ccall unsafe "Z3_write_interpolation_problem"
  z3_write_interpolation_problem :: <Z3_context> -> CUInt -> Ptr <Z3_ast> -> Ptr CUInt -> <Z3_string> -> CUInt -> Ptr <Z3_ast> -> IO ()

{- | Create a new optimize context.

       \remark User must use #Z3_optimize_inc_ref and #Z3_optimize_dec_ref to manage optimize objects.
       Even if the context was created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
foreign import ccall unsafe "Z3_mk_optimize"
  z3_mk_optimize :: <Z3_context> -> IO <Z3_optimize>

{- | Increment the reference counter of the given optimize context -}
foreign import ccall unsafe "Z3_optimize_inc_ref"
  z3_optimize_inc_ref :: <Z3_context> -> <Z3_optimize> -> IO ()

{- | Decrement the reference counter of the given optimize context. -}
foreign import ccall unsafe "Z3_optimize_dec_ref"
  z3_optimize_dec_ref :: <Z3_context> -> <Z3_optimize> -> IO ()

{- | Assert hard constraint to the optimization context. -}
foreign import ccall unsafe "Z3_optimize_assert"
  z3_optimize_assert :: <Z3_context> -> <Z3_optimize> -> <Z3_ast> -> IO ()

{- | Assert soft constraint to the optimization context.
       \param c - context
       \param o - optimization context
       \param a - formula
       \param weight - a positive weight, penalty for violating soft constraint
       \param id - optional identifier to group soft constraints -}
foreign import ccall unsafe "Z3_optimize_assert_soft"
  z3_optimize_assert_soft :: <Z3_context> -> <Z3_optimize> -> <Z3_ast> -> <Z3_string> -> <Z3_symbol> -> IO ()

{- | Add a maximization constraint.
       \param c - context
       \param o - optimization context
       \param a - arithmetical term -}
foreign import ccall unsafe "Z3_optimize_maximize"
  z3_optimize_maximize :: <Z3_context> -> <Z3_optimize> -> <Z3_ast> -> IO ()

{- | Add a minimization constraint.
       \param c - context
       \param o - optimization context
       \param a - arithmetical term -}
foreign import ccall unsafe "Z3_optimize_minimize"
  z3_optimize_minimize :: <Z3_context> -> <Z3_optimize> -> <Z3_ast> -> IO ()

{- | Create a backtracking point.

       The optimize solver contains a set of rules, added facts and assertions.
       The set of rules, facts and assertions are restored upon calling #Z3_optimize_pop.

       \sa Z3_optimize_pop -}
foreign import ccall unsafe "Z3_optimize_push"
  z3_optimize_push :: <Z3_context> -> <Z3_optimize> -> IO ()

{- | Backtrack one level.

       \sa Z3_optimize_push

       \pre The number of calls to pop cannot exceed calls to push. -}
foreign import ccall unsafe "Z3_optimize_pop"
  z3_optimize_pop :: <Z3_context> -> <Z3_optimize> -> IO ()

{- | Check consistency and produce optimal values.
       \param c - context
       \param o - optimization context -}
foreign import ccall unsafe "Z3_optimize_check"
  z3_optimize_check :: <Z3_context> -> <Z3_optimize> -> IO <Z3_lbool>

{- | Retrieve a string that describes the last status returned by #Z3_optimize_check.

       Use this method when #Z3_optimize_check returns Z3_L_UNDEF. -}
foreign import ccall unsafe "Z3_optimize_get_reason_unknown"
  z3_optimize_get_reason_unknown :: <Z3_context> -> <Z3_optimize> -> IO <Z3_string>

{- | Retrieve the model for the last #Z3_optimize_check

       The error handler is invoked if a model is not available because
       the commands above were not invoked for the given optimization
       solver, or if the result was \c Z3_L_FALSE. -}
foreign import ccall unsafe "Z3_optimize_get_model"
  z3_optimize_get_model :: <Z3_context> -> <Z3_optimize> -> IO <Z3_model>

{- | Set parameters on optimization context.

       \param c - context
       \param o - optimization context
       \param p - parameters -}
foreign import ccall unsafe "Z3_optimize_set_params"
  z3_optimize_set_params :: <Z3_context> -> <Z3_optimize> -> <Z3_params> -> IO ()

{- | Return the parameter description set for the given optimize object.

       \param c - context
       \param o - optimization context -}
foreign import ccall unsafe "Z3_optimize_get_param_descrs"
  z3_optimize_get_param_descrs :: <Z3_context> -> <Z3_optimize> -> IO <Z3_param_descrs>

{- | Retrieve lower bound value or approximation for the i'th optimization objective.

       \param c - context
       \param o - optimization context
       \param idx - index of optimization objective -}
foreign import ccall unsafe "Z3_optimize_get_lower"
  z3_optimize_get_lower :: <Z3_context> -> <Z3_optimize> -> CUInt -> IO <Z3_ast>

{- | Retrieve upper bound value or approximation for the i'th optimization objective.

       \param c - context
       \param o - optimization context
       \param idx - index of optimization objective -}
foreign import ccall unsafe "Z3_optimize_get_upper"
  z3_optimize_get_upper :: <Z3_context> -> <Z3_optimize> -> CUInt -> IO <Z3_ast>

{- | Print the current context as a string.
       \param c - context.
       \param o - optimization context. -}
foreign import ccall unsafe "Z3_optimize_to_string"
  z3_optimize_to_string :: <Z3_context> -> <Z3_optimize> -> IO <Z3_string>

{- | Parse an SMT-LIB2 string with assertions,
       soft constraints and optimization objectives.
       Add the parsed constraints and objectives to the optimization context.

       \param c - context.
       \param o - optimize context.
       \param s - string containing SMT2 specification. -}
foreign import ccall unsafe "Z3_optimize_from_string"
  z3_optimize_from_string :: <Z3_context> -> <Z3_optimize> -> <Z3_string> -> IO ()

{- | Parse an SMT-LIB2 file with assertions,
       soft constraints and optimization objectives.
       Add the parsed constraints and objectives to the optimization context.

       \param c - context.
       \param o - optimize context.
       \param s - string containing SMT2 specification. -}
foreign import ccall unsafe "Z3_optimize_from_file"
  z3_optimize_from_file :: <Z3_context> -> <Z3_optimize> -> <Z3_string> -> IO ()

{- | Return a string containing a description of parameters accepted by optimize. -}
foreign import ccall unsafe "Z3_optimize_get_help"
  z3_optimize_get_help :: <Z3_context> -> <Z3_optimize> -> IO <Z3_string>

{- | Retrieve statistics information from the last call to #Z3_optimize_check -}
foreign import ccall unsafe "Z3_optimize_get_statistics"
  z3_optimize_get_statistics :: <Z3_context> -> <Z3_optimize> -> IO <Z3_stats>

{- | Return the set of asserted formulas on the optimization context. -}
foreign import ccall unsafe "Z3_optimize_get_assertions"
  z3_optimize_get_assertions :: <Z3_context> -> <Z3_optimize> -> IO <Z3_ast_vector>

{- | Return objectives on the optimization context.
       If the objective function is a max-sat objective it is returned
       as a Pseudo-Boolean (minimization) sum of the form (+ (if f1 w1 0) (if f2 w2 0) ...)
       If the objective function is entered as a maximization objective, then return
       the corresponding minimization objective. In this way the resulting objective
       function is always returned as a minimization objective. -}
foreign import ccall unsafe "Z3_optimize_get_objectives"
  z3_optimize_get_objectives :: <Z3_context> -> <Z3_optimize> -> IO <Z3_ast_vector>

{- | Return the nonzero subresultants of \c p and \c q with respect to the "variable" \c x.

       \pre \c p, \c q and \c x are Z3 expressions where \c p and \c q are arithmetic terms.
       Note that, any subterm that cannot be viewed as a polynomial is assumed to be a variable.
       Example: f(a) is a considered to be a variable in the polynomial

       f(a)*f(a) + 2*f(a) + 1 -}
foreign import ccall unsafe "Z3_polynomial_subresultants"
  z3_polynomial_subresultants :: <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast_vector>

{- | Delete a RCF numeral created using the RCF API. -}
foreign import ccall unsafe "Z3_rcf_del"
  z3_rcf_del :: <Z3_context> -> <Z3_rcf_num> -> IO ()

{- | Return a RCF rational using the given string. -}
foreign import ccall unsafe "Z3_rcf_mk_rational"
  z3_rcf_mk_rational :: <Z3_context> -> <Z3_string> -> IO <Z3_rcf_num>

{- | Return a RCF small integer. -}
foreign import ccall unsafe "Z3_rcf_mk_small_int"
  z3_rcf_mk_small_int :: <Z3_context> -> CInt -> IO <Z3_rcf_num>

{- | Return Pi -}
foreign import ccall unsafe "Z3_rcf_mk_pi"
  z3_rcf_mk_pi :: <Z3_context> -> IO <Z3_rcf_num>

{- | Return e (Euler's constant) -}
foreign import ccall unsafe "Z3_rcf_mk_e"
  z3_rcf_mk_e :: <Z3_context> -> IO <Z3_rcf_num>

{- | Return a new infinitesimal that is smaller than all elements in the Z3 field. -}
foreign import ccall unsafe "Z3_rcf_mk_infinitesimal"
  z3_rcf_mk_infinitesimal :: <Z3_context> -> IO <Z3_rcf_num>

{- | Store in roots the roots of the polynomial <tt>a[n-1]*x^{n-1} + ... + a[0]</tt>.
       The output vector \c roots must have size \c n.
       It returns the number of roots of the polynomial.

       \pre The input polynomial is not the zero polynomial. -}
foreign import ccall unsafe "Z3_rcf_mk_roots"
  z3_rcf_mk_roots :: <Z3_context> -> CUInt -> Ptr <Z3_rcf_num> -> Ptr <Z3_rcf_num> -> IO ()

{- | Return the value a + b. -}
foreign import ccall unsafe "Z3_rcf_add"
  z3_rcf_add :: <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO <Z3_rcf_num>

{- | Return the value a - b. -}
foreign import ccall unsafe "Z3_rcf_sub"
  z3_rcf_sub :: <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO <Z3_rcf_num>

{- | Return the value a * b. -}
foreign import ccall unsafe "Z3_rcf_mul"
  z3_rcf_mul :: <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO <Z3_rcf_num>

{- | Return the value a / b. -}
foreign import ccall unsafe "Z3_rcf_div"
  z3_rcf_div :: <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO <Z3_rcf_num>

{- | Return the value -a -}
foreign import ccall unsafe "Z3_rcf_neg"
  z3_rcf_neg :: <Z3_context> -> <Z3_rcf_num> -> IO <Z3_rcf_num>

{- | Return the value 1/a -}
foreign import ccall unsafe "Z3_rcf_inv"
  z3_rcf_inv :: <Z3_context> -> <Z3_rcf_num> -> IO <Z3_rcf_num>

{- | Return the value a^k -}
foreign import ccall unsafe "Z3_rcf_power"
  z3_rcf_power :: <Z3_context> -> <Z3_rcf_num> -> CUInt -> IO <Z3_rcf_num>

{- | Return Z3_TRUE if a < b -}
foreign import ccall unsafe "Z3_rcf_lt"
  z3_rcf_lt :: <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt

{- | Return Z3_TRUE if a > b -}
foreign import ccall unsafe "Z3_rcf_gt"
  z3_rcf_gt :: <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt

{- | Return Z3_TRUE if a <= b -}
foreign import ccall unsafe "Z3_rcf_le"
  z3_rcf_le :: <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt

{- | Return Z3_TRUE if a >= b -}
foreign import ccall unsafe "Z3_rcf_ge"
  z3_rcf_ge :: <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt

{- | Return Z3_TRUE if a == b -}
foreign import ccall unsafe "Z3_rcf_eq"
  z3_rcf_eq :: <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt

{- | Return Z3_TRUE if a != b -}
foreign import ccall unsafe "Z3_rcf_neq"
  z3_rcf_neq :: <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt

{- | Convert the RCF numeral into a string. -}
foreign import ccall unsafe "Z3_rcf_num_to_string"
  z3_rcf_num_to_string :: <Z3_context> -> <Z3_rcf_num> -> CInt -> CInt -> IO <Z3_string>

{- | Convert the RCF numeral into a string in decimal notation. -}
foreign import ccall unsafe "Z3_rcf_num_to_decimal_string"
  z3_rcf_num_to_decimal_string :: <Z3_context> -> <Z3_rcf_num> -> CUInt -> IO <Z3_string>

{- | Extract the "numerator" and "denominator" of the given RCF numeral.
       We have that a = n/d, moreover n and d are not represented using rational functions. -}
foreign import ccall unsafe "Z3_rcf_get_numerator_denominator"
  z3_rcf_get_numerator_denominator :: <Z3_context> -> <Z3_rcf_num> -> Ptr <Z3_rcf_num> -> Ptr <Z3_rcf_num> -> IO ()