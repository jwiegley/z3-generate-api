{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_polynomial.h"
module Z3.Base.C.Polynomial where
import Foreign.Ptr
#strict_import
import Z3.Base.C.Api

{- | Return the nonzero subresultants of \c p and \c q with respect to the "variable" \c x.

       \pre \c p, \c q and \c x are Z3 expressions where \c p and \c q are arithmetic terms.
       Note that, any subterm that cannot be viewed as a polynomial is assumed to be a variable.
       Example: f(a) is a considered to be a variable in the polynomial

       f(a)*f(a) + 2*f(a) + 1 -}
#ccall Z3_polynomial_subresultants , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast_vector>
