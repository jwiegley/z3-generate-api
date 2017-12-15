{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_fpa.h"

module Z3.Base.C.Fpa where
import Foreign.Ptr
import Foreign.C.Types
import Z3.Base.C.Api
#ccall Z3_mk_fpa_rounding_mode_sort , <Z3_context> -> IO <Z3_sort>
#ccall Z3_mk_fpa_round_nearest_ties_to_even , <Z3_context> -> IO <Z3_ast>
#ccall Z3_mk_fpa_rne , <Z3_context> -> IO <Z3_ast>
#ccall Z3_mk_fpa_round_nearest_ties_to_away , <Z3_context> -> IO <Z3_ast>
#ccall Z3_mk_fpa_rna , <Z3_context> -> IO <Z3_ast>
#ccall Z3_mk_fpa_round_toward_positive , <Z3_context> -> IO <Z3_ast>
#ccall Z3_mk_fpa_rtp , <Z3_context> -> IO <Z3_ast>
#ccall Z3_mk_fpa_round_toward_negative , <Z3_context> -> IO <Z3_ast>
#ccall Z3_mk_fpa_rtn , <Z3_context> -> IO <Z3_ast>
#ccall Z3_mk_fpa_round_toward_zero , <Z3_context> -> IO <Z3_ast>
#ccall Z3_mk_fpa_rtz , <Z3_context> -> IO <Z3_ast>
#ccall Z3_mk_fpa_sort , <Z3_context> -> CUInt -> CUInt -> IO <Z3_sort>
#ccall Z3_mk_fpa_sort_half , <Z3_context> -> IO <Z3_sort>
#ccall Z3_mk_fpa_sort_16 , <Z3_context> -> IO <Z3_sort>
#ccall Z3_mk_fpa_sort_single , <Z3_context> -> IO <Z3_sort>
#ccall Z3_mk_fpa_sort_32 , <Z3_context> -> IO <Z3_sort>
#ccall Z3_mk_fpa_sort_double , <Z3_context> -> IO <Z3_sort>
#ccall Z3_mk_fpa_sort_64 , <Z3_context> -> IO <Z3_sort>
#ccall Z3_mk_fpa_sort_quadruple , <Z3_context> -> IO <Z3_sort>
#ccall Z3_mk_fpa_sort_128 , <Z3_context> -> IO <Z3_sort>
#ccall Z3_mk_fpa_nan , <Z3_context> -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_fpa_inf , <Z3_context> -> <Z3_sort> -> CInt -> IO <Z3_ast>
#ccall Z3_mk_fpa_zero , <Z3_context> -> <Z3_sort> -> CInt -> IO <Z3_ast>
#ccall Z3_mk_fpa_fp , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_numeral_float , <Z3_context> -> CFloat -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_fpa_numeral_double , <Z3_context> -> CDouble -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_fpa_numeral_int , <Z3_context> -> CInt -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_fpa_numeral_int_uint , <Z3_context> -> CInt -> CInt -> CUInt -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_fpa_numeral_int64_uint64 , <Z3_context> -> CInt -> CLong -> CULong -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_fpa_abs , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_neg , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_add , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_sub , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_mul , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_div , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_fma , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_sqrt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_rem , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_round_to_integral , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_min , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_max , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_leq , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_lt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_geq , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_gt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_eq , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_is_normal , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_is_subnormal , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_is_zero , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_is_infinite , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_is_nan , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_is_negative , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_is_positive , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_to_fp_bv , <Z3_context> -> <Z3_ast> -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_fpa_to_fp_float , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_fpa_to_fp_real , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_fpa_to_fp_signed , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_fpa_to_fp_unsigned , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_fpa_to_ubv , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> CUInt -> IO <Z3_ast>
#ccall Z3_mk_fpa_to_sbv , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> CUInt -> IO <Z3_ast>
#ccall Z3_mk_fpa_to_real , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_fpa_get_ebits , <Z3_context> -> <Z3_sort> -> IO ()
#ccall Z3_fpa_get_sbits , <Z3_context> -> <Z3_sort> -> IO ()
#ccall Z3_fpa_get_numeral_sign , <Z3_context> -> <Z3_ast> -> Ptr CInt -> IO CInt
#ccall Z3_fpa_get_numeral_significand_string , <Z3_context> -> <Z3_ast> -> IO <Z3_string>
#ccall Z3_fpa_get_numeral_significand_uint64 , <Z3_context> -> <Z3_ast> -> Ptr CULong -> IO CInt
#ccall Z3_fpa_get_numeral_exponent_string , <Z3_context> -> <Z3_ast> -> IO <Z3_string>
#ccall Z3_fpa_get_numeral_exponent_int64 , <Z3_context> -> <Z3_ast> -> Ptr CLong -> IO CInt
#ccall Z3_mk_fpa_to_ieee_bv , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_fpa_to_fp_int_real , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> <Z3_sort> -> IO <Z3_ast>
