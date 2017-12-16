{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_interp.h"

module Z3.Base.C.Interp where
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Z3.Base.C.Api
#ccall Z3_mk_interpolant , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_interpolation_context , <Z3_config> -> IO <Z3_context>
#ccall Z3_get_interpolant , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_params> -> IO <Z3_ast_vector>
#ccall Z3_compute_interpolant , <Z3_context> -> <Z3_ast> -> <Z3_params> -> Ptr <Z3_ast_vector> -> Ptr <Z3_model> -> IO <Z3_lbool>
#ccall Z3_interpolation_profile , <Z3_context> -> IO <Z3_string>
#ccall Z3_read_interpolation_problem , <Z3_context> -> Ptr CUInt -> Ptr (Ptr <Z3_ast>) -> Ptr (Ptr CUInt) -> <Z3_string> -> <Z3_string_ptr> -> Ptr CUInt -> Ptr (Ptr <Z3_ast>) -> IO CInt
#ccall Z3_check_interpolant , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> Ptr CUInt -> Ptr <Z3_ast> -> <Z3_string_ptr> -> CUInt -> Ptr <Z3_ast> -> IO CInt
#ccall Z3_write_interpolation_problem , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> Ptr CUInt -> <Z3_string> -> CUInt -> Ptr <Z3_ast> -> IO ()
