{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_fixedpoint.h"

module Z3.Base.C.Fixedpoint where
import Foreign.Ptr
import Foreign.C.Types
import Z3.Base.C.Api
#ccall Z3_mk_fixedpoint , <Z3_context> -> IO <Z3_fixedpoint>
#ccall Z3_fixedpoint_inc_ref , <Z3_context> -> <Z3_fixedpoint> -> IO ()
#ccall Z3_fixedpoint_dec_ref , <Z3_context> -> <Z3_fixedpoint> -> IO ()
#ccall Z3_fixedpoint_add_rule , <Z3_context> -> <Z3_fixedpoint> -> <Z3_ast> -> <Z3_symbol> -> IO ()
#ccall Z3_fixedpoint_add_fact , <Z3_context> -> <Z3_fixedpoint> -> <Z3_func_decl> -> CUInt -> Ptr CUInt -> IO ()
#ccall Z3_fixedpoint_assert , <Z3_context> -> <Z3_fixedpoint> -> <Z3_ast> -> IO ()
#ccall Z3_fixedpoint_query , <Z3_context> -> <Z3_fixedpoint> -> <Z3_ast> -> IO <Z3_lbool>
#ccall Z3_fixedpoint_query_relations , <Z3_context> -> <Z3_fixedpoint> -> CUInt -> Ptr <Z3_func_decl> -> IO <Z3_lbool>
#ccall Z3_fixedpoint_get_answer , <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_ast>
#ccall Z3_fixedpoint_get_reason_unknown , <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_string>
#ccall Z3_fixedpoint_update_rule , <Z3_context> -> <Z3_fixedpoint> -> <Z3_ast> -> <Z3_symbol> -> IO ()
#ccall Z3_fixedpoint_get_num_levels , <Z3_context> -> <Z3_fixedpoint> -> <Z3_func_decl> -> IO ()
#ccall Z3_fixedpoint_get_cover_delta , <Z3_context> -> <Z3_fixedpoint> -> CInt -> <Z3_func_decl> -> IO <Z3_ast>
#ccall Z3_fixedpoint_add_cover , <Z3_context> -> <Z3_fixedpoint> -> CInt -> <Z3_func_decl> -> <Z3_ast> -> IO ()
#ccall Z3_fixedpoint_get_statistics , <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_stats>
#ccall Z3_fixedpoint_register_relation , <Z3_context> -> <Z3_fixedpoint> -> <Z3_func_decl> -> IO ()
#ccall Z3_fixedpoint_set_predicate_representation , <Z3_context> -> <Z3_fixedpoint> -> <Z3_func_decl> -> CUInt -> Ptr <Z3_symbol> -> IO ()
#ccall Z3_fixedpoint_get_rules , <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_ast_vector>
#ccall Z3_fixedpoint_get_assertions , <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_ast_vector>
#ccall Z3_fixedpoint_set_params , <Z3_context> -> <Z3_fixedpoint> -> <Z3_params> -> IO ()
#ccall Z3_fixedpoint_get_help , <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_string>
#ccall Z3_fixedpoint_get_param_descrs , <Z3_context> -> <Z3_fixedpoint> -> IO <Z3_param_descrs>
#ccall Z3_fixedpoint_to_string , <Z3_context> -> <Z3_fixedpoint> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_string>
#ccall Z3_fixedpoint_from_string , <Z3_context> -> <Z3_fixedpoint> -> <Z3_string> -> IO <Z3_ast_vector>
#ccall Z3_fixedpoint_from_file , <Z3_context> -> <Z3_fixedpoint> -> <Z3_string> -> IO <Z3_ast_vector>
#ccall Z3_fixedpoint_push , <Z3_context> -> <Z3_fixedpoint> -> IO ()
#ccall Z3_fixedpoint_pop , <Z3_context> -> <Z3_fixedpoint> -> IO ()
#callback_t Z3_fixedpoint_reduce_assign_callback_fptr , Ptr () -> <Z3_func_decl> -> CUInt -> Ptr <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> IO ()
#callback_t Z3_fixedpoint_reduce_app_callback_fptr , Ptr () -> <Z3_func_decl> -> CUInt -> Ptr <Z3_ast> -> Ptr <Z3_ast> -> IO ()
#ccall Z3_fixedpoint_init , <Z3_context> -> <Z3_fixedpoint> -> Ptr () -> IO ()
#ccall Z3_fixedpoint_set_reduce_assign_callback , <Z3_context> -> <Z3_fixedpoint> -> <Z3_fixedpoint_reduce_assign_callback_fptr> -> IO ()
#ccall Z3_fixedpoint_set_reduce_app_callback , <Z3_context> -> <Z3_fixedpoint> -> <Z3_fixedpoint_reduce_app_callback_fptr> -> IO ()
