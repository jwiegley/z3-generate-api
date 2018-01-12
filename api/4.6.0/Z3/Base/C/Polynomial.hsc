{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_polynomial.h"

module Z3.Base.C.Polynomial where
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Z3.Base.C.Api
#ccall Z3_polynomial_subresultants , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast_vector>
