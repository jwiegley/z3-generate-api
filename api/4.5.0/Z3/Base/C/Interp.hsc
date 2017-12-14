{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_interp.h"
module Z3.Base.C.Interp where
import Foreign.Ptr
#strict_import
import Z3.Base.C.Api

{- | Create an AST node marking a formula position for interpolation.

    The node \c a must have Boolean sort. -}
#ccall Z3_mk_interpolant , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | This function generates a Z3 context suitable for generation of
    interpolants. Formulas can be generated as abstract syntax trees in
    this context using the Z3 C API.

    Interpolants are also generated as AST's in this context.

    If cfg is non-null, it will be used as the base configuration
    for the Z3 context. This makes it possible to set Z3 options
    to be used during interpolation. This feature should be used
    with some caution however, as it may be that certain Z3 options
    are incompatible with interpolation. -}
#ccall Z3_mk_interpolation_context , <Z3_config> -> IO <Z3_context>
#ccall Z3_get_interpolant , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_params> -> IO <Z3_ast_vector>
#ccall Z3_compute_interpolant , <Z3_context> -> <Z3_ast> -> <Z3_params> -> Ptr <Z3_ast_vector> -> Ptr <Z3_model> -> IO <Z3_lbool>
#ccall Z3_interpolation_profile , <Z3_context> -> IO <Z3_string>

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
#ccall Z3_read_interpolation_problem , <Z3_context> -> Ptr CUInt -> Ptr (Ptr <Z3_ast>) -> Ptr (Ptr CUInt) -> <Z3_string> -> <Z3_string_ptr> -> Ptr CUInt -> Ptr (Ptr <Z3_ast>) -> IO CInt
#ccall Z3_check_interpolant , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> Ptr CUInt -> Ptr <Z3_ast> -> <Z3_string_ptr> -> CUInt -> Ptr <Z3_ast> -> IO CInt
#ccall Z3_write_interpolation_problem , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> Ptr CUInt -> <Z3_string> -> CUInt -> Ptr <Z3_ast> -> IO ()
