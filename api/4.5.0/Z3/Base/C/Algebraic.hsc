{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_algebraic.h"
module Z3.Base.C.Algebraic where
import Foreign.Ptr
#strict_import
import Z3.Base.C.Api

{- | Return Z3_TRUE if \c can be used as value in the Z3 real algebraic
       number package. -}
#ccall Z3_algebraic_is_value , <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return the Z3_TRUE if \c a is positive, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a) -}
#ccall Z3_algebraic_is_pos , <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return the Z3_TRUE if \c a is negative, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a) -}
#ccall Z3_algebraic_is_neg , <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return the Z3_TRUE if \c a is zero, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a) -}
#ccall Z3_algebraic_is_zero , <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return 1 if \c a is positive, 0 if \c a is zero, and -1 if \c a is negative.

       \pre Z3_algebraic_is_value(c, a) -}
#ccall Z3_algebraic_sign , <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return the value a + b.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b)
       \post Z3_algebraic_is_value(c, result) -}
#ccall Z3_algebraic_add , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Return the value a - b.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b)
       \post Z3_algebraic_is_value(c, result) -}
#ccall Z3_algebraic_sub , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Return the value a * b.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b)
       \post Z3_algebraic_is_value(c, result) -}
#ccall Z3_algebraic_mul , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Return the value a / b.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b)
       \pre !Z3_algebraic_is_zero(c, b)
       \post Z3_algebraic_is_value(c, result) -}
#ccall Z3_algebraic_div , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Return the a^(1/k)

       \pre Z3_algebraic_is_value(c, a)
       \pre k is even => !Z3_algebraic_is_neg(c, a)
       \post Z3_algebraic_is_value(c, result) -}
#ccall Z3_algebraic_root , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_ast>

{- | Return the a^k

       \pre Z3_algebraic_is_value(c, a)
       \post Z3_algebraic_is_value(c, result) -}
#ccall Z3_algebraic_power , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_ast>

{- | Return Z3_TRUE if a < b, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b) -}
#ccall Z3_algebraic_lt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt

{- | Return Z3_TRUE if a > b, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b) -}
#ccall Z3_algebraic_gt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt

{- | Return Z3_TRUE if a <= b, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b) -}
#ccall Z3_algebraic_le , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt

{- | Return Z3_TRUE if a >= b, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b) -}
#ccall Z3_algebraic_ge , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt

{- | Return Z3_TRUE if a == b, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b) -}
#ccall Z3_algebraic_eq , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt

{- | Return Z3_TRUE if a != b, and Z3_FALSE otherwise.

       \pre Z3_algebraic_is_value(c, a)
       \pre Z3_algebraic_is_value(c, b) -}
#ccall Z3_algebraic_neq , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt

{- | Given a multivariate polynomial p(x_0, ..., x_{n-1}, x_n), returns the
       roots of the univariate polynomial p(a[0], ..., a[n-1], x_n).

       \pre p is a Z3 expression that contains only arithmetic terms and free variables.
       \pre forall i in [0, n) Z3_algebraic_is_value(c, a[i])
       \post forall r in result Z3_algebraic_is_value(c, result) -}
#ccall Z3_algebraic_roots , <Z3_context> -> <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast_vector>

{- | Given a multivariate polynomial p(x_0, ..., x_{n-1}), return the
       sign of p(a[0], ..., a[n-1]).

       \pre p is a Z3 expression that contains only arithmetic terms and free variables.
       \pre forall i in [0, n) Z3_algebraic_is_value(c, a[i]) -}
#ccall Z3_algebraic_eval , <Z3_context> -> <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> IO CInt
