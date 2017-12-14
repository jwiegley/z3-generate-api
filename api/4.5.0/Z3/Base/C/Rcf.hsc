{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_rcf.h"
module Z3.Base.C.Rcf where
import Foreign.Ptr
#strict_import
import Z3.Base.C.Api

{- | Delete a RCF numeral created using the RCF API. -}
#ccall Z3_rcf_del , <Z3_context> -> <Z3_rcf_num> -> IO ()

{- | Return a RCF rational using the given string. -}
#ccall Z3_rcf_mk_rational , <Z3_context> -> <Z3_string> -> IO <Z3_rcf_num>

{- | Return a RCF small integer. -}
#ccall Z3_rcf_mk_small_int , <Z3_context> -> CInt -> IO <Z3_rcf_num>

{- | Return Pi -}
#ccall Z3_rcf_mk_pi , <Z3_context> -> IO <Z3_rcf_num>

{- | Return e (Euler's constant) -}
#ccall Z3_rcf_mk_e , <Z3_context> -> IO <Z3_rcf_num>

{- | Return a new infinitesimal that is smaller than all elements in the Z3 field. -}
#ccall Z3_rcf_mk_infinitesimal , <Z3_context> -> IO <Z3_rcf_num>

{- | Store in roots the roots of the polynomial <tt>a[n-1]*x^{n-1} + ... + a[0]</tt>.
       The output vector \c roots must have size \c n.
       It returns the number of roots of the polynomial.

       \pre The input polynomial is not the zero polynomial. -}
#ccall Z3_rcf_mk_roots , <Z3_context> -> CUInt -> Ptr <Z3_rcf_num> -> Ptr <Z3_rcf_num> -> IO ()

{- | Return the value a + b. -}
#ccall Z3_rcf_add , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO <Z3_rcf_num>

{- | Return the value a - b. -}
#ccall Z3_rcf_sub , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO <Z3_rcf_num>

{- | Return the value a * b. -}
#ccall Z3_rcf_mul , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO <Z3_rcf_num>

{- | Return the value a / b. -}
#ccall Z3_rcf_div , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO <Z3_rcf_num>

{- | Return the value -a -}
#ccall Z3_rcf_neg , <Z3_context> -> <Z3_rcf_num> -> IO <Z3_rcf_num>

{- | Return the value 1/a -}
#ccall Z3_rcf_inv , <Z3_context> -> <Z3_rcf_num> -> IO <Z3_rcf_num>

{- | Return the value a^k -}
#ccall Z3_rcf_power , <Z3_context> -> <Z3_rcf_num> -> CUInt -> IO <Z3_rcf_num>

{- | Return Z3_TRUE if a < b -}
#ccall Z3_rcf_lt , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt

{- | Return Z3_TRUE if a > b -}
#ccall Z3_rcf_gt , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt

{- | Return Z3_TRUE if a <= b -}
#ccall Z3_rcf_le , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt

{- | Return Z3_TRUE if a >= b -}
#ccall Z3_rcf_ge , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt

{- | Return Z3_TRUE if a == b -}
#ccall Z3_rcf_eq , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt

{- | Return Z3_TRUE if a != b -}
#ccall Z3_rcf_neq , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt

{- | Convert the RCF numeral into a string. -}
#ccall Z3_rcf_num_to_string , <Z3_context> -> <Z3_rcf_num> -> CInt -> CInt -> IO <Z3_string>

{- | Convert the RCF numeral into a string in decimal notation. -}
#ccall Z3_rcf_num_to_decimal_string , <Z3_context> -> <Z3_rcf_num> -> CUInt -> IO <Z3_string>

{- | Extract the "numerator" and "denominator" of the given RCF numeral.
       We have that a = n/d, moreover n and d are not represented using rational functions. -}
#ccall Z3_rcf_get_numerator_denominator , <Z3_context> -> <Z3_rcf_num> -> Ptr <Z3_rcf_num> -> Ptr <Z3_rcf_num> -> IO ()
