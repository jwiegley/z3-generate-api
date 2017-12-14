{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_ast_containers.h"
module Z3.Base.C.AstContainers where
import Foreign.Ptr
#strict_import
import Z3.Base.C.Api

{- | Return an empty AST vector.

       \remark Reference counting must be used to manage AST vectors, even when the Z3_context was
       created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
#ccall Z3_mk_ast_vector , <Z3_context> -> IO <Z3_ast_vector>

{- | Increment the reference counter of the given AST vector. -}
#ccall Z3_ast_vector_inc_ref , <Z3_context> -> <Z3_ast_vector> -> IO ()

{- | Decrement the reference counter of the given AST vector. -}
#ccall Z3_ast_vector_dec_ref , <Z3_context> -> <Z3_ast_vector> -> IO ()

{- | Return the size of the given AST vector. -}
#ccall Z3_ast_vector_size , <Z3_context> -> <Z3_ast_vector> -> IO ()

{- | Return the AST at position \c i in the AST vector \c v.

       \pre i < Z3_ast_vector_size(c, v) -}
#ccall Z3_ast_vector_get , <Z3_context> -> <Z3_ast_vector> -> CUInt -> IO <Z3_ast>

{- | Update position \c i of the AST vector \c v with the AST \c a.

       \pre i < Z3_ast_vector_size(c, v) -}
#ccall Z3_ast_vector_set , <Z3_context> -> <Z3_ast_vector> -> CUInt -> <Z3_ast> -> IO ()

{- | Resize the AST vector \c v. -}
#ccall Z3_ast_vector_resize , <Z3_context> -> <Z3_ast_vector> -> CUInt -> IO ()

{- | Add the AST \c a in the end of the AST vector \c v. The size of \c v is increased by one. -}
#ccall Z3_ast_vector_push , <Z3_context> -> <Z3_ast_vector> -> <Z3_ast> -> IO ()

{- | Translate the AST vector \c v from context \c s into an AST vector in context \c t. -}
#ccall Z3_ast_vector_translate , <Z3_context> -> <Z3_ast_vector> -> <Z3_context> -> IO <Z3_ast_vector>

{- | Convert AST vector into a string. -}
#ccall Z3_ast_vector_to_string , <Z3_context> -> <Z3_ast_vector> -> IO <Z3_string>

{- | Return an empty mapping from AST to AST

    \remark Reference counting must be used to manage AST maps, even when the Z3_context was
    created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
#ccall Z3_mk_ast_map , <Z3_context> -> IO <Z3_ast_map>

{- | Increment the reference counter of the given AST map. -}
#ccall Z3_ast_map_inc_ref , <Z3_context> -> <Z3_ast_map> -> IO ()

{- | Decrement the reference counter of the given AST map. -}
#ccall Z3_ast_map_dec_ref , <Z3_context> -> <Z3_ast_map> -> IO ()

{- | Return true if the map \c m contains the AST key \c k. -}
#ccall Z3_ast_map_contains , <Z3_context> -> <Z3_ast_map> -> <Z3_ast> -> IO CInt

{- | Return the value associated with the key \c k.

    The procedure invokes the error handler if \c k is not in the map. -}
#ccall Z3_ast_map_find , <Z3_context> -> <Z3_ast_map> -> <Z3_ast> -> IO <Z3_ast>

{- | Store/Replace a new key, value pair in the given map. -}
#ccall Z3_ast_map_insert , <Z3_context> -> <Z3_ast_map> -> <Z3_ast> -> <Z3_ast> -> IO ()

{- | Erase a key from the map. -}
#ccall Z3_ast_map_erase , <Z3_context> -> <Z3_ast_map> -> <Z3_ast> -> IO ()

{- | Remove all keys from the given map. -}
#ccall Z3_ast_map_reset , <Z3_context> -> <Z3_ast_map> -> IO ()

{- | Return the size of the given map. -}
#ccall Z3_ast_map_size , <Z3_context> -> <Z3_ast_map> -> IO ()

{- | Return the keys stored in the given map. -}
#ccall Z3_ast_map_keys , <Z3_context> -> <Z3_ast_map> -> IO <Z3_ast_vector>

{- | Convert the given map into a string. -}
#ccall Z3_ast_map_to_string , <Z3_context> -> <Z3_ast_map> -> IO <Z3_string>
