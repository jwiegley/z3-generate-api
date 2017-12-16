{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_ast_containers.h"

module Z3.Base.C.AstContainers where
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Z3.Base.C.Api
#ccall Z3_mk_ast_vector , <Z3_context> -> IO <Z3_ast_vector>
#ccall Z3_ast_vector_inc_ref , <Z3_context> -> <Z3_ast_vector> -> IO ()
#ccall Z3_ast_vector_dec_ref , <Z3_context> -> <Z3_ast_vector> -> IO ()
#ccall Z3_ast_vector_size , <Z3_context> -> <Z3_ast_vector> -> IO CUInt
#ccall Z3_ast_vector_get , <Z3_context> -> <Z3_ast_vector> -> CUInt -> IO <Z3_ast>
#ccall Z3_ast_vector_set , <Z3_context> -> <Z3_ast_vector> -> CUInt -> <Z3_ast> -> IO ()
#ccall Z3_ast_vector_resize , <Z3_context> -> <Z3_ast_vector> -> CUInt -> IO ()
#ccall Z3_ast_vector_push , <Z3_context> -> <Z3_ast_vector> -> <Z3_ast> -> IO ()
#ccall Z3_ast_vector_translate , <Z3_context> -> <Z3_ast_vector> -> <Z3_context> -> IO <Z3_ast_vector>
#ccall Z3_ast_vector_to_string , <Z3_context> -> <Z3_ast_vector> -> IO <Z3_string>
#ccall Z3_mk_ast_map , <Z3_context> -> IO <Z3_ast_map>
#ccall Z3_ast_map_inc_ref , <Z3_context> -> <Z3_ast_map> -> IO ()
#ccall Z3_ast_map_dec_ref , <Z3_context> -> <Z3_ast_map> -> IO ()
#ccall Z3_ast_map_contains , <Z3_context> -> <Z3_ast_map> -> <Z3_ast> -> IO CInt
#ccall Z3_ast_map_find , <Z3_context> -> <Z3_ast_map> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_ast_map_insert , <Z3_context> -> <Z3_ast_map> -> <Z3_ast> -> <Z3_ast> -> IO ()
#ccall Z3_ast_map_erase , <Z3_context> -> <Z3_ast_map> -> <Z3_ast> -> IO ()
#ccall Z3_ast_map_reset , <Z3_context> -> <Z3_ast_map> -> IO ()
#ccall Z3_ast_map_size , <Z3_context> -> <Z3_ast_map> -> IO CUInt
#ccall Z3_ast_map_keys , <Z3_context> -> <Z3_ast_map> -> IO <Z3_ast_vector>
#ccall Z3_ast_map_to_string , <Z3_context> -> <Z3_ast_map> -> IO <Z3_string>
