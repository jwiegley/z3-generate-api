{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_optimization.h"

module Z3.Base.C.Optimization where
import Foreign.Ptr
import Foreign.C.Types
import Z3.Base.C.Api
#ccall Z3_mk_optimize , <Z3_context> -> IO <Z3_optimize>
#ccall Z3_optimize_inc_ref , <Z3_context> -> <Z3_optimize> -> IO ()
#ccall Z3_optimize_dec_ref , <Z3_context> -> <Z3_optimize> -> IO ()
#ccall Z3_optimize_assert , <Z3_context> -> <Z3_optimize> -> <Z3_ast> -> IO ()
#ccall Z3_optimize_assert_soft , <Z3_context> -> <Z3_optimize> -> <Z3_ast> -> <Z3_string> -> <Z3_symbol> -> IO ()
#ccall Z3_optimize_maximize , <Z3_context> -> <Z3_optimize> -> <Z3_ast> -> IO ()
#ccall Z3_optimize_minimize , <Z3_context> -> <Z3_optimize> -> <Z3_ast> -> IO ()
#ccall Z3_optimize_push , <Z3_context> -> <Z3_optimize> -> IO ()
#ccall Z3_optimize_pop , <Z3_context> -> <Z3_optimize> -> IO ()
#ccall Z3_optimize_check , <Z3_context> -> <Z3_optimize> -> IO <Z3_lbool>
#ccall Z3_optimize_get_reason_unknown , <Z3_context> -> <Z3_optimize> -> IO <Z3_string>
#ccall Z3_optimize_get_model , <Z3_context> -> <Z3_optimize> -> IO <Z3_model>
#ccall Z3_optimize_set_params , <Z3_context> -> <Z3_optimize> -> <Z3_params> -> IO ()
#ccall Z3_optimize_get_param_descrs , <Z3_context> -> <Z3_optimize> -> IO <Z3_param_descrs>
#ccall Z3_optimize_get_lower , <Z3_context> -> <Z3_optimize> -> CUInt -> IO <Z3_ast>
#ccall Z3_optimize_get_upper , <Z3_context> -> <Z3_optimize> -> CUInt -> IO <Z3_ast>
#ccall Z3_optimize_to_string , <Z3_context> -> <Z3_optimize> -> IO <Z3_string>
#ccall Z3_optimize_from_string , <Z3_context> -> <Z3_optimize> -> <Z3_string> -> IO ()
#ccall Z3_optimize_from_file , <Z3_context> -> <Z3_optimize> -> <Z3_string> -> IO ()
#ccall Z3_optimize_get_help , <Z3_context> -> <Z3_optimize> -> IO <Z3_string>
#ccall Z3_optimize_get_statistics , <Z3_context> -> <Z3_optimize> -> IO <Z3_stats>
#ccall Z3_optimize_get_assertions , <Z3_context> -> <Z3_optimize> -> IO <Z3_ast_vector>
#ccall Z3_optimize_get_objectives , <Z3_context> -> <Z3_optimize> -> IO <Z3_ast_vector>
