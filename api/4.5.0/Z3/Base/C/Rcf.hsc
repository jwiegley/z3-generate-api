{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_rcf.h"

module Z3.Base.C.Rcf where
import Foreign.Ptr
import Foreign.C.Types
import Z3.Base.C.Api
#ccall Z3_rcf_del , <Z3_context> -> <Z3_rcf_num> -> IO ()
#ccall Z3_rcf_mk_rational , <Z3_context> -> <Z3_string> -> IO <Z3_rcf_num>
#ccall Z3_rcf_mk_small_int , <Z3_context> -> CInt -> IO <Z3_rcf_num>
#ccall Z3_rcf_mk_pi , <Z3_context> -> IO <Z3_rcf_num>
#ccall Z3_rcf_mk_e , <Z3_context> -> IO <Z3_rcf_num>
#ccall Z3_rcf_mk_infinitesimal , <Z3_context> -> IO <Z3_rcf_num>
#ccall Z3_rcf_mk_roots , <Z3_context> -> CUInt -> Ptr <Z3_rcf_num> -> Ptr <Z3_rcf_num> -> IO ()
#ccall Z3_rcf_add , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO <Z3_rcf_num>
#ccall Z3_rcf_sub , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO <Z3_rcf_num>
#ccall Z3_rcf_mul , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO <Z3_rcf_num>
#ccall Z3_rcf_div , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO <Z3_rcf_num>
#ccall Z3_rcf_neg , <Z3_context> -> <Z3_rcf_num> -> IO <Z3_rcf_num>
#ccall Z3_rcf_inv , <Z3_context> -> <Z3_rcf_num> -> IO <Z3_rcf_num>
#ccall Z3_rcf_power , <Z3_context> -> <Z3_rcf_num> -> CUInt -> IO <Z3_rcf_num>
#ccall Z3_rcf_lt , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt
#ccall Z3_rcf_gt , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt
#ccall Z3_rcf_le , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt
#ccall Z3_rcf_ge , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt
#ccall Z3_rcf_eq , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt
#ccall Z3_rcf_neq , <Z3_context> -> <Z3_rcf_num> -> <Z3_rcf_num> -> IO CInt
#ccall Z3_rcf_num_to_string , <Z3_context> -> <Z3_rcf_num> -> CInt -> CInt -> IO <Z3_string>
#ccall Z3_rcf_num_to_decimal_string , <Z3_context> -> <Z3_rcf_num> -> CUInt -> IO <Z3_string>
#ccall Z3_rcf_get_numerator_denominator , <Z3_context> -> <Z3_rcf_num> -> Ptr <Z3_rcf_num> -> Ptr <Z3_rcf_num> -> IO ()
