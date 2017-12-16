{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_algebraic.h"

module Z3.Base.C.Algebraic where
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Z3.Base.C.Api
#ccall Z3_algebraic_is_value , <Z3_context> -> <Z3_ast> -> IO CInt
#ccall Z3_algebraic_is_pos , <Z3_context> -> <Z3_ast> -> IO CInt
#ccall Z3_algebraic_is_neg , <Z3_context> -> <Z3_ast> -> IO CInt
#ccall Z3_algebraic_is_zero , <Z3_context> -> <Z3_ast> -> IO CInt
#ccall Z3_algebraic_sign , <Z3_context> -> <Z3_ast> -> IO CInt
#ccall Z3_algebraic_add , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_algebraic_sub , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_algebraic_mul , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_algebraic_div , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_algebraic_root , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_ast>
#ccall Z3_algebraic_power , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_ast>
#ccall Z3_algebraic_lt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt
#ccall Z3_algebraic_gt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt
#ccall Z3_algebraic_le , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt
#ccall Z3_algebraic_ge , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt
#ccall Z3_algebraic_eq , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt
#ccall Z3_algebraic_neq , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt
#ccall Z3_algebraic_roots , <Z3_context> -> <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast_vector>
#ccall Z3_algebraic_eval , <Z3_context> -> <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> IO CInt
