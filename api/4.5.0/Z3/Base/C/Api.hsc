{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"

module Z3.Base.C.Api where
import Foreign.Ptr
import Foreign.C.Types
data Z3_app
type C'Z3_app = Ptr Z3_app
data Z3_param_descrs
type C'Z3_param_descrs = Ptr Z3_param_descrs
data Z3_ast_map
type C'Z3_ast_map = Ptr Z3_ast_map
data Z3_func_interp
type C'Z3_func_interp = Ptr Z3_func_interp
data Z3_literals
type C'Z3_literals = Ptr Z3_literals
data Z3_string_ptr
type C'Z3_string_ptr = Ptr Z3_string_ptr
data Z3_ast
type C'Z3_ast = Ptr Z3_ast
data Z3_sort
type C'Z3_sort = Ptr Z3_sort
data Z3_ast_vector
type C'Z3_ast_vector = Ptr Z3_ast_vector
data Z3_stats
type C'Z3_stats = Ptr Z3_stats
data Z3_func_decl
type C'Z3_func_decl = Ptr Z3_func_decl
data Z3_optimize
type C'Z3_optimize = Ptr Z3_optimize
data Z3_goal
type C'Z3_goal = Ptr Z3_goal
data Z3_symbol
type C'Z3_symbol = Ptr Z3_symbol
data Z3_model
type C'Z3_model = Ptr Z3_model
data Z3_rcf_num
type C'Z3_rcf_num = Ptr Z3_rcf_num
data Z3_constructor_list
type C'Z3_constructor_list = Ptr Z3_constructor_list
data Z3_apply_result
type C'Z3_apply_result = Ptr Z3_apply_result
data Z3_params
type C'Z3_params = Ptr Z3_params
data Z3_config
type C'Z3_config = Ptr Z3_config
data Z3_pattern
type C'Z3_pattern = Ptr Z3_pattern
data Z3_string
type C'Z3_string = Ptr Z3_string
data Z3_func_entry
type C'Z3_func_entry = Ptr Z3_func_entry
data Z3_context
type C'Z3_context = Ptr Z3_context
data Z3_fixedpoint
type C'Z3_fixedpoint = Ptr Z3_fixedpoint
data Z3_tactic
type C'Z3_tactic = Ptr Z3_tactic
data Z3_probe
type C'Z3_probe = Ptr Z3_probe
data Z3_solver
type C'Z3_solver = Ptr Z3_solver
data Z3_constructor
type C'Z3_constructor = Ptr Z3_constructor
{- typedef int Z3_bool; -}
#synonym_t Z3_bool , CInt
{- typedef enum {
            Z3_L_FALSE = -1, Z3_L_UNDEF, Z3_L_TRUE
        } Z3_lbool; -}
#integral_t Z3_lbool
#num Z3_L_FALSE
#num Z3_L_UNDEF
#num Z3_L_TRUE
{- typedef enum {
            Z3_INT_SYMBOL, Z3_STRING_SYMBOL
        } Z3_symbol_kind; -}
#integral_t Z3_symbol_kind
#num Z3_INT_SYMBOL
#num Z3_STRING_SYMBOL
{- typedef enum {
            Z3_PARAMETER_INT,
            Z3_PARAMETER_DOUBLE,
            Z3_PARAMETER_RATIONAL,
            Z3_PARAMETER_SYMBOL,
            Z3_PARAMETER_SORT,
            Z3_PARAMETER_AST,
            Z3_PARAMETER_FUNC_DECL
        } Z3_parameter_kind; -}
#integral_t Z3_parameter_kind
#num Z3_PARAMETER_INT
#num Z3_PARAMETER_DOUBLE
#num Z3_PARAMETER_RATIONAL
#num Z3_PARAMETER_SYMBOL
#num Z3_PARAMETER_SORT
#num Z3_PARAMETER_AST
#num Z3_PARAMETER_FUNC_DECL
{- typedef enum {
            Z3_UNINTERPRETED_SORT,
            Z3_BOOL_SORT,
            Z3_INT_SORT,
            Z3_REAL_SORT,
            Z3_BV_SORT,
            Z3_ARRAY_SORT,
            Z3_DATATYPE_SORT,
            Z3_RELATION_SORT,
            Z3_FINITE_DOMAIN_SORT,
            Z3_FLOATING_POINT_SORT,
            Z3_ROUNDING_MODE_SORT,
            Z3_SEQ_SORT,
            Z3_RE_SORT,
            Z3_UNKNOWN_SORT = 1000
        } Z3_sort_kind; -}
#integral_t Z3_sort_kind
#num Z3_UNINTERPRETED_SORT
#num Z3_BOOL_SORT
#num Z3_INT_SORT
#num Z3_REAL_SORT
#num Z3_BV_SORT
#num Z3_ARRAY_SORT
#num Z3_DATATYPE_SORT
#num Z3_RELATION_SORT
#num Z3_FINITE_DOMAIN_SORT
#num Z3_FLOATING_POINT_SORT
#num Z3_ROUNDING_MODE_SORT
#num Z3_SEQ_SORT
#num Z3_RE_SORT
#num Z3_UNKNOWN_SORT
{- typedef enum {
            Z3_NUMERAL_AST,
            Z3_APP_AST,
            Z3_VAR_AST,
            Z3_QUANTIFIER_AST,
            Z3_SORT_AST,
            Z3_FUNC_DECL_AST,
            Z3_UNKNOWN_AST = 1000
        } Z3_ast_kind; -}
#integral_t Z3_ast_kind
#num Z3_NUMERAL_AST
#num Z3_APP_AST
#num Z3_VAR_AST
#num Z3_QUANTIFIER_AST
#num Z3_SORT_AST
#num Z3_FUNC_DECL_AST
#num Z3_UNKNOWN_AST
{- typedef enum {
            Z3_OP_TRUE = 0x100,
            Z3_OP_FALSE,
            Z3_OP_EQ,
            Z3_OP_DISTINCT,
            Z3_OP_ITE,
            Z3_OP_AND,
            Z3_OP_OR,
            Z3_OP_IFF,
            Z3_OP_XOR,
            Z3_OP_NOT,
            Z3_OP_IMPLIES,
            Z3_OP_OEQ,
            Z3_OP_INTERP,
            Z3_OP_ANUM = 0x200,
            Z3_OP_AGNUM,
            Z3_OP_LE,
            Z3_OP_GE,
            Z3_OP_LT,
            Z3_OP_GT,
            Z3_OP_ADD,
            Z3_OP_SUB,
            Z3_OP_UMINUS,
            Z3_OP_MUL,
            Z3_OP_DIV,
            Z3_OP_IDIV,
            Z3_OP_REM,
            Z3_OP_MOD,
            Z3_OP_TO_REAL,
            Z3_OP_TO_INT,
            Z3_OP_IS_INT,
            Z3_OP_POWER,
            Z3_OP_STORE = 0x300,
            Z3_OP_SELECT,
            Z3_OP_CONST_ARRAY,
            Z3_OP_ARRAY_MAP,
            Z3_OP_ARRAY_DEFAULT,
            Z3_OP_SET_UNION,
            Z3_OP_SET_INTERSECT,
            Z3_OP_SET_DIFFERENCE,
            Z3_OP_SET_COMPLEMENT,
            Z3_OP_SET_SUBSET,
            Z3_OP_AS_ARRAY,
            Z3_OP_ARRAY_EXT,
            Z3_OP_BNUM = 0x400,
            Z3_OP_BIT1,
            Z3_OP_BIT0,
            Z3_OP_BNEG,
            Z3_OP_BADD,
            Z3_OP_BSUB,
            Z3_OP_BMUL,
            Z3_OP_BSDIV,
            Z3_OP_BUDIV,
            Z3_OP_BSREM,
            Z3_OP_BUREM,
            Z3_OP_BSMOD,
            Z3_OP_BSDIV0,
            Z3_OP_BUDIV0,
            Z3_OP_BSREM0,
            Z3_OP_BUREM0,
            Z3_OP_BSMOD0,
            Z3_OP_ULEQ,
            Z3_OP_SLEQ,
            Z3_OP_UGEQ,
            Z3_OP_SGEQ,
            Z3_OP_ULT,
            Z3_OP_SLT,
            Z3_OP_UGT,
            Z3_OP_SGT,
            Z3_OP_BAND,
            Z3_OP_BOR,
            Z3_OP_BNOT,
            Z3_OP_BXOR,
            Z3_OP_BNAND,
            Z3_OP_BNOR,
            Z3_OP_BXNOR,
            Z3_OP_CONCAT,
            Z3_OP_SIGN_EXT,
            Z3_OP_ZERO_EXT,
            Z3_OP_EXTRACT,
            Z3_OP_REPEAT,
            Z3_OP_BREDOR,
            Z3_OP_BREDAND,
            Z3_OP_BCOMP,
            Z3_OP_BSHL,
            Z3_OP_BLSHR,
            Z3_OP_BASHR,
            Z3_OP_ROTATE_LEFT,
            Z3_OP_ROTATE_RIGHT,
            Z3_OP_EXT_ROTATE_LEFT,
            Z3_OP_EXT_ROTATE_RIGHT,
            Z3_OP_INT2BV,
            Z3_OP_BV2INT,
            Z3_OP_CARRY,
            Z3_OP_XOR3,
            Z3_OP_BSMUL_NO_OVFL,
            Z3_OP_BUMUL_NO_OVFL,
            Z3_OP_BSMUL_NO_UDFL,
            Z3_OP_BSDIV_I,
            Z3_OP_BUDIV_I,
            Z3_OP_BSREM_I,
            Z3_OP_BUREM_I,
            Z3_OP_BSMOD_I,
            Z3_OP_PR_UNDEF = 0x500,
            Z3_OP_PR_TRUE,
            Z3_OP_PR_ASSERTED,
            Z3_OP_PR_GOAL,
            Z3_OP_PR_MODUS_PONENS,
            Z3_OP_PR_REFLEXIVITY,
            Z3_OP_PR_SYMMETRY,
            Z3_OP_PR_TRANSITIVITY,
            Z3_OP_PR_TRANSITIVITY_STAR,
            Z3_OP_PR_MONOTONICITY,
            Z3_OP_PR_QUANT_INTRO,
            Z3_OP_PR_DISTRIBUTIVITY,
            Z3_OP_PR_AND_ELIM,
            Z3_OP_PR_NOT_OR_ELIM,
            Z3_OP_PR_REWRITE,
            Z3_OP_PR_REWRITE_STAR,
            Z3_OP_PR_PULL_QUANT,
            Z3_OP_PR_PULL_QUANT_STAR,
            Z3_OP_PR_PUSH_QUANT,
            Z3_OP_PR_ELIM_UNUSED_VARS,
            Z3_OP_PR_DER,
            Z3_OP_PR_QUANT_INST,
            Z3_OP_PR_HYPOTHESIS,
            Z3_OP_PR_LEMMA,
            Z3_OP_PR_UNIT_RESOLUTION,
            Z3_OP_PR_IFF_TRUE,
            Z3_OP_PR_IFF_FALSE,
            Z3_OP_PR_COMMUTATIVITY,
            Z3_OP_PR_DEF_AXIOM,
            Z3_OP_PR_DEF_INTRO,
            Z3_OP_PR_APPLY_DEF,
            Z3_OP_PR_IFF_OEQ,
            Z3_OP_PR_NNF_POS,
            Z3_OP_PR_NNF_NEG,
            Z3_OP_PR_NNF_STAR,
            Z3_OP_PR_CNF_STAR,
            Z3_OP_PR_SKOLEMIZE,
            Z3_OP_PR_MODUS_PONENS_OEQ,
            Z3_OP_PR_TH_LEMMA,
            Z3_OP_PR_HYPER_RESOLVE,
            Z3_OP_RA_STORE = 0x600,
            Z3_OP_RA_EMPTY,
            Z3_OP_RA_IS_EMPTY,
            Z3_OP_RA_JOIN,
            Z3_OP_RA_UNION,
            Z3_OP_RA_WIDEN,
            Z3_OP_RA_PROJECT,
            Z3_OP_RA_FILTER,
            Z3_OP_RA_NEGATION_FILTER,
            Z3_OP_RA_RENAME,
            Z3_OP_RA_COMPLEMENT,
            Z3_OP_RA_SELECT,
            Z3_OP_RA_CLONE,
            Z3_OP_FD_CONSTANT,
            Z3_OP_FD_LT,
            Z3_OP_SEQ_UNIT,
            Z3_OP_SEQ_EMPTY,
            Z3_OP_SEQ_CONCAT,
            Z3_OP_SEQ_PREFIX,
            Z3_OP_SEQ_SUFFIX,
            Z3_OP_SEQ_CONTAINS,
            Z3_OP_SEQ_EXTRACT,
            Z3_OP_SEQ_REPLACE,
            Z3_OP_SEQ_AT,
            Z3_OP_SEQ_LENGTH,
            Z3_OP_SEQ_INDEX,
            Z3_OP_SEQ_TO_RE,
            Z3_OP_SEQ_IN_RE,
            Z3_OP_RE_PLUS,
            Z3_OP_RE_STAR,
            Z3_OP_RE_OPTION,
            Z3_OP_RE_CONCAT,
            Z3_OP_RE_UNION,
            Z3_OP_LABEL = 0x700,
            Z3_OP_LABEL_LIT,
            Z3_OP_DT_CONSTRUCTOR = 0x800,
            Z3_OP_DT_RECOGNISER,
            Z3_OP_DT_ACCESSOR,
            Z3_OP_DT_UPDATE_FIELD,
            Z3_OP_PB_AT_MOST = 0x900,
            Z3_OP_PB_LE,
            Z3_OP_PB_GE,
            Z3_OP_PB_EQ,
            Z3_OP_FPA_RM_NEAREST_TIES_TO_EVEN,
            Z3_OP_FPA_RM_NEAREST_TIES_TO_AWAY,
            Z3_OP_FPA_RM_TOWARD_POSITIVE,
            Z3_OP_FPA_RM_TOWARD_NEGATIVE,
            Z3_OP_FPA_RM_TOWARD_ZERO,
            Z3_OP_FPA_NUM,
            Z3_OP_FPA_PLUS_INF,
            Z3_OP_FPA_MINUS_INF,
            Z3_OP_FPA_NAN,
            Z3_OP_FPA_PLUS_ZERO,
            Z3_OP_FPA_MINUS_ZERO,
            Z3_OP_FPA_ADD,
            Z3_OP_FPA_SUB,
            Z3_OP_FPA_NEG,
            Z3_OP_FPA_MUL,
            Z3_OP_FPA_DIV,
            Z3_OP_FPA_REM,
            Z3_OP_FPA_ABS,
            Z3_OP_FPA_MIN,
            Z3_OP_FPA_MAX,
            Z3_OP_FPA_FMA,
            Z3_OP_FPA_SQRT,
            Z3_OP_FPA_ROUND_TO_INTEGRAL,
            Z3_OP_FPA_EQ,
            Z3_OP_FPA_LT,
            Z3_OP_FPA_GT,
            Z3_OP_FPA_LE,
            Z3_OP_FPA_GE,
            Z3_OP_FPA_IS_NAN,
            Z3_OP_FPA_IS_INF,
            Z3_OP_FPA_IS_ZERO,
            Z3_OP_FPA_IS_NORMAL,
            Z3_OP_FPA_IS_SUBNORMAL,
            Z3_OP_FPA_IS_NEGATIVE,
            Z3_OP_FPA_IS_POSITIVE,
            Z3_OP_FPA_FP,
            Z3_OP_FPA_TO_FP,
            Z3_OP_FPA_TO_FP_UNSIGNED,
            Z3_OP_FPA_TO_UBV,
            Z3_OP_FPA_TO_SBV,
            Z3_OP_FPA_TO_REAL,
            Z3_OP_FPA_TO_IEEE_BV,
            Z3_OP_FPA_MIN_I,
            Z3_OP_FPA_MAX_I,
            Z3_OP_INTERNAL,
            Z3_OP_UNINTERPRETED
        } Z3_decl_kind; -}
#integral_t Z3_decl_kind
#num Z3_OP_TRUE
#num Z3_OP_FALSE
#num Z3_OP_EQ
#num Z3_OP_DISTINCT
#num Z3_OP_ITE
#num Z3_OP_AND
#num Z3_OP_OR
#num Z3_OP_IFF
#num Z3_OP_XOR
#num Z3_OP_NOT
#num Z3_OP_IMPLIES
#num Z3_OP_OEQ
#num Z3_OP_INTERP
#num Z3_OP_ANUM
#num Z3_OP_AGNUM
#num Z3_OP_LE
#num Z3_OP_GE
#num Z3_OP_LT
#num Z3_OP_GT
#num Z3_OP_ADD
#num Z3_OP_SUB
#num Z3_OP_UMINUS
#num Z3_OP_MUL
#num Z3_OP_DIV
#num Z3_OP_IDIV
#num Z3_OP_REM
#num Z3_OP_MOD
#num Z3_OP_TO_REAL
#num Z3_OP_TO_INT
#num Z3_OP_IS_INT
#num Z3_OP_POWER
#num Z3_OP_STORE
#num Z3_OP_SELECT
#num Z3_OP_CONST_ARRAY
#num Z3_OP_ARRAY_MAP
#num Z3_OP_ARRAY_DEFAULT
#num Z3_OP_SET_UNION
#num Z3_OP_SET_INTERSECT
#num Z3_OP_SET_DIFFERENCE
#num Z3_OP_SET_COMPLEMENT
#num Z3_OP_SET_SUBSET
#num Z3_OP_AS_ARRAY
#num Z3_OP_ARRAY_EXT
#num Z3_OP_BNUM
#num Z3_OP_BIT1
#num Z3_OP_BIT0
#num Z3_OP_BNEG
#num Z3_OP_BADD
#num Z3_OP_BSUB
#num Z3_OP_BMUL
#num Z3_OP_BSDIV
#num Z3_OP_BUDIV
#num Z3_OP_BSREM
#num Z3_OP_BUREM
#num Z3_OP_BSMOD
#num Z3_OP_BSDIV0
#num Z3_OP_BUDIV0
#num Z3_OP_BSREM0
#num Z3_OP_BUREM0
#num Z3_OP_BSMOD0
#num Z3_OP_ULEQ
#num Z3_OP_SLEQ
#num Z3_OP_UGEQ
#num Z3_OP_SGEQ
#num Z3_OP_ULT
#num Z3_OP_SLT
#num Z3_OP_UGT
#num Z3_OP_SGT
#num Z3_OP_BAND
#num Z3_OP_BOR
#num Z3_OP_BNOT
#num Z3_OP_BXOR
#num Z3_OP_BNAND
#num Z3_OP_BNOR
#num Z3_OP_BXNOR
#num Z3_OP_CONCAT
#num Z3_OP_SIGN_EXT
#num Z3_OP_ZERO_EXT
#num Z3_OP_EXTRACT
#num Z3_OP_REPEAT
#num Z3_OP_BREDOR
#num Z3_OP_BREDAND
#num Z3_OP_BCOMP
#num Z3_OP_BSHL
#num Z3_OP_BLSHR
#num Z3_OP_BASHR
#num Z3_OP_ROTATE_LEFT
#num Z3_OP_ROTATE_RIGHT
#num Z3_OP_EXT_ROTATE_LEFT
#num Z3_OP_EXT_ROTATE_RIGHT
#num Z3_OP_INT2BV
#num Z3_OP_BV2INT
#num Z3_OP_CARRY
#num Z3_OP_XOR3
#num Z3_OP_BSMUL_NO_OVFL
#num Z3_OP_BUMUL_NO_OVFL
#num Z3_OP_BSMUL_NO_UDFL
#num Z3_OP_BSDIV_I
#num Z3_OP_BUDIV_I
#num Z3_OP_BSREM_I
#num Z3_OP_BUREM_I
#num Z3_OP_BSMOD_I
#num Z3_OP_PR_UNDEF
#num Z3_OP_PR_TRUE
#num Z3_OP_PR_ASSERTED
#num Z3_OP_PR_GOAL
#num Z3_OP_PR_MODUS_PONENS
#num Z3_OP_PR_REFLEXIVITY
#num Z3_OP_PR_SYMMETRY
#num Z3_OP_PR_TRANSITIVITY
#num Z3_OP_PR_TRANSITIVITY_STAR
#num Z3_OP_PR_MONOTONICITY
#num Z3_OP_PR_QUANT_INTRO
#num Z3_OP_PR_DISTRIBUTIVITY
#num Z3_OP_PR_AND_ELIM
#num Z3_OP_PR_NOT_OR_ELIM
#num Z3_OP_PR_REWRITE
#num Z3_OP_PR_REWRITE_STAR
#num Z3_OP_PR_PULL_QUANT
#num Z3_OP_PR_PULL_QUANT_STAR
#num Z3_OP_PR_PUSH_QUANT
#num Z3_OP_PR_ELIM_UNUSED_VARS
#num Z3_OP_PR_DER
#num Z3_OP_PR_QUANT_INST
#num Z3_OP_PR_HYPOTHESIS
#num Z3_OP_PR_LEMMA
#num Z3_OP_PR_UNIT_RESOLUTION
#num Z3_OP_PR_IFF_TRUE
#num Z3_OP_PR_IFF_FALSE
#num Z3_OP_PR_COMMUTATIVITY
#num Z3_OP_PR_DEF_AXIOM
#num Z3_OP_PR_DEF_INTRO
#num Z3_OP_PR_APPLY_DEF
#num Z3_OP_PR_IFF_OEQ
#num Z3_OP_PR_NNF_POS
#num Z3_OP_PR_NNF_NEG
#num Z3_OP_PR_NNF_STAR
#num Z3_OP_PR_CNF_STAR
#num Z3_OP_PR_SKOLEMIZE
#num Z3_OP_PR_MODUS_PONENS_OEQ
#num Z3_OP_PR_TH_LEMMA
#num Z3_OP_PR_HYPER_RESOLVE
#num Z3_OP_RA_STORE
#num Z3_OP_RA_EMPTY
#num Z3_OP_RA_IS_EMPTY
#num Z3_OP_RA_JOIN
#num Z3_OP_RA_UNION
#num Z3_OP_RA_WIDEN
#num Z3_OP_RA_PROJECT
#num Z3_OP_RA_FILTER
#num Z3_OP_RA_NEGATION_FILTER
#num Z3_OP_RA_RENAME
#num Z3_OP_RA_COMPLEMENT
#num Z3_OP_RA_SELECT
#num Z3_OP_RA_CLONE
#num Z3_OP_FD_CONSTANT
#num Z3_OP_FD_LT
#num Z3_OP_SEQ_UNIT
#num Z3_OP_SEQ_EMPTY
#num Z3_OP_SEQ_CONCAT
#num Z3_OP_SEQ_PREFIX
#num Z3_OP_SEQ_SUFFIX
#num Z3_OP_SEQ_CONTAINS
#num Z3_OP_SEQ_EXTRACT
#num Z3_OP_SEQ_REPLACE
#num Z3_OP_SEQ_AT
#num Z3_OP_SEQ_LENGTH
#num Z3_OP_SEQ_INDEX
#num Z3_OP_SEQ_TO_RE
#num Z3_OP_SEQ_IN_RE
#num Z3_OP_RE_PLUS
#num Z3_OP_RE_STAR
#num Z3_OP_RE_OPTION
#num Z3_OP_RE_CONCAT
#num Z3_OP_RE_UNION
#num Z3_OP_LABEL
#num Z3_OP_LABEL_LIT
#num Z3_OP_DT_CONSTRUCTOR
#num Z3_OP_DT_RECOGNISER
#num Z3_OP_DT_ACCESSOR
#num Z3_OP_DT_UPDATE_FIELD
#num Z3_OP_PB_AT_MOST
#num Z3_OP_PB_LE
#num Z3_OP_PB_GE
#num Z3_OP_PB_EQ
#num Z3_OP_FPA_RM_NEAREST_TIES_TO_EVEN
#num Z3_OP_FPA_RM_NEAREST_TIES_TO_AWAY
#num Z3_OP_FPA_RM_TOWARD_POSITIVE
#num Z3_OP_FPA_RM_TOWARD_NEGATIVE
#num Z3_OP_FPA_RM_TOWARD_ZERO
#num Z3_OP_FPA_NUM
#num Z3_OP_FPA_PLUS_INF
#num Z3_OP_FPA_MINUS_INF
#num Z3_OP_FPA_NAN
#num Z3_OP_FPA_PLUS_ZERO
#num Z3_OP_FPA_MINUS_ZERO
#num Z3_OP_FPA_ADD
#num Z3_OP_FPA_SUB
#num Z3_OP_FPA_NEG
#num Z3_OP_FPA_MUL
#num Z3_OP_FPA_DIV
#num Z3_OP_FPA_REM
#num Z3_OP_FPA_ABS
#num Z3_OP_FPA_MIN
#num Z3_OP_FPA_MAX
#num Z3_OP_FPA_FMA
#num Z3_OP_FPA_SQRT
#num Z3_OP_FPA_ROUND_TO_INTEGRAL
#num Z3_OP_FPA_EQ
#num Z3_OP_FPA_LT
#num Z3_OP_FPA_GT
#num Z3_OP_FPA_LE
#num Z3_OP_FPA_GE
#num Z3_OP_FPA_IS_NAN
#num Z3_OP_FPA_IS_INF
#num Z3_OP_FPA_IS_ZERO
#num Z3_OP_FPA_IS_NORMAL
#num Z3_OP_FPA_IS_SUBNORMAL
#num Z3_OP_FPA_IS_NEGATIVE
#num Z3_OP_FPA_IS_POSITIVE
#num Z3_OP_FPA_FP
#num Z3_OP_FPA_TO_FP
#num Z3_OP_FPA_TO_FP_UNSIGNED
#num Z3_OP_FPA_TO_UBV
#num Z3_OP_FPA_TO_SBV
#num Z3_OP_FPA_TO_REAL
#num Z3_OP_FPA_TO_IEEE_BV
#num Z3_OP_FPA_MIN_I
#num Z3_OP_FPA_MAX_I
#num Z3_OP_INTERNAL
#num Z3_OP_UNINTERPRETED
{- typedef enum {
            Z3_PK_UINT,
            Z3_PK_BOOL,
            Z3_PK_DOUBLE,
            Z3_PK_SYMBOL,
            Z3_PK_STRING,
            Z3_PK_OTHER,
            Z3_PK_INVALID
        } Z3_param_kind; -}
#integral_t Z3_param_kind
#num Z3_PK_UINT
#num Z3_PK_BOOL
#num Z3_PK_DOUBLE
#num Z3_PK_SYMBOL
#num Z3_PK_STRING
#num Z3_PK_OTHER
#num Z3_PK_INVALID
{- typedef enum {
            Z3_PRINT_SMTLIB_FULL,
            Z3_PRINT_LOW_LEVEL,
            Z3_PRINT_SMTLIB_COMPLIANT,
            Z3_PRINT_SMTLIB2_COMPLIANT
        } Z3_ast_print_mode; -}
#integral_t Z3_ast_print_mode
#num Z3_PRINT_SMTLIB_FULL
#num Z3_PRINT_LOW_LEVEL
#num Z3_PRINT_SMTLIB_COMPLIANT
#num Z3_PRINT_SMTLIB2_COMPLIANT
{- typedef enum {
            Z3_OK,
            Z3_SORT_ERROR,
            Z3_IOB,
            Z3_INVALID_ARG,
            Z3_PARSER_ERROR,
            Z3_NO_PARSER,
            Z3_INVALID_PATTERN,
            Z3_MEMOUT_FAIL,
            Z3_FILE_ACCESS_ERROR,
            Z3_INTERNAL_FATAL,
            Z3_INVALID_USAGE,
            Z3_DEC_REF_ERROR,
            Z3_EXCEPTION
        } Z3_error_code; -}
#integral_t Z3_error_code
#num Z3_OK
#num Z3_SORT_ERROR
#num Z3_IOB
#num Z3_INVALID_ARG
#num Z3_PARSER_ERROR
#num Z3_NO_PARSER
#num Z3_INVALID_PATTERN
#num Z3_MEMOUT_FAIL
#num Z3_FILE_ACCESS_ERROR
#num Z3_INTERNAL_FATAL
#num Z3_INVALID_USAGE
#num Z3_DEC_REF_ERROR
#num Z3_EXCEPTION
#callback_t Z3_error_handler , <Z3_context> -> <Z3_error_code> -> IO ()
{- typedef enum {
            Z3_GOAL_PRECISE, Z3_GOAL_UNDER, Z3_GOAL_OVER, Z3_GOAL_UNDER_OVER
        } Z3_goal_prec; -}
#integral_t Z3_goal_prec
#num Z3_GOAL_PRECISE
#num Z3_GOAL_UNDER
#num Z3_GOAL_OVER
#num Z3_GOAL_UNDER_OVER
#ccall Z3_global_param_set , <Z3_string> -> <Z3_string> -> IO ()
#ccall Z3_global_param_reset_all , IO ()
#ccall Z3_global_param_get , <Z3_string> -> <Z3_string_ptr> -> IO CInt
#ccall Z3_mk_config , IO <Z3_config>
#ccall Z3_del_config , <Z3_config> -> IO ()
#ccall Z3_set_param_value , <Z3_config> -> <Z3_string> -> <Z3_string> -> IO ()
#ccall Z3_mk_context , <Z3_config> -> IO <Z3_context>
#ccall Z3_mk_context_rc , <Z3_config> -> IO <Z3_context>
#ccall Z3_del_context , <Z3_context> -> IO ()
#ccall Z3_inc_ref , <Z3_context> -> <Z3_ast> -> IO ()
#ccall Z3_dec_ref , <Z3_context> -> <Z3_ast> -> IO ()
#ccall Z3_update_param_value , <Z3_context> -> <Z3_string> -> <Z3_string> -> IO ()
#ccall Z3_interrupt , <Z3_context> -> IO ()
#ccall Z3_mk_params , <Z3_context> -> IO <Z3_params>
#ccall Z3_params_inc_ref , <Z3_context> -> <Z3_params> -> IO ()
#ccall Z3_params_dec_ref , <Z3_context> -> <Z3_params> -> IO ()
#ccall Z3_params_set_bool , <Z3_context> -> <Z3_params> -> <Z3_symbol> -> CInt -> IO ()
#ccall Z3_params_set_uint , <Z3_context> -> <Z3_params> -> <Z3_symbol> -> CUInt -> IO ()
#ccall Z3_params_set_double , <Z3_context> -> <Z3_params> -> <Z3_symbol> -> CDouble -> IO ()
#ccall Z3_params_set_symbol , <Z3_context> -> <Z3_params> -> <Z3_symbol> -> <Z3_symbol> -> IO ()
#ccall Z3_params_to_string , <Z3_context> -> <Z3_params> -> IO <Z3_string>
#ccall Z3_params_validate , <Z3_context> -> <Z3_params> -> <Z3_param_descrs> -> IO ()
#ccall Z3_param_descrs_inc_ref , <Z3_context> -> <Z3_param_descrs> -> IO ()
#ccall Z3_param_descrs_dec_ref , <Z3_context> -> <Z3_param_descrs> -> IO ()
#ccall Z3_param_descrs_get_kind , <Z3_context> -> <Z3_param_descrs> -> <Z3_symbol> -> IO <Z3_param_kind>
#ccall Z3_param_descrs_size , <Z3_context> -> <Z3_param_descrs> -> IO ()
#ccall Z3_param_descrs_get_name , <Z3_context> -> <Z3_param_descrs> -> CUInt -> IO <Z3_symbol>
#ccall Z3_param_descrs_get_documentation , <Z3_context> -> <Z3_param_descrs> -> <Z3_symbol> -> IO <Z3_string>
#ccall Z3_param_descrs_to_string , <Z3_context> -> <Z3_param_descrs> -> IO <Z3_string>
#ccall Z3_mk_int_symbol , <Z3_context> -> CInt -> IO <Z3_symbol>
#ccall Z3_mk_string_symbol , <Z3_context> -> <Z3_string> -> IO <Z3_symbol>
#ccall Z3_mk_uninterpreted_sort , <Z3_context> -> <Z3_symbol> -> IO <Z3_sort>
#ccall Z3_mk_bool_sort , <Z3_context> -> IO <Z3_sort>
#ccall Z3_mk_int_sort , <Z3_context> -> IO <Z3_sort>
#ccall Z3_mk_real_sort , <Z3_context> -> IO <Z3_sort>
#ccall Z3_mk_bv_sort , <Z3_context> -> CUInt -> IO <Z3_sort>
#ccall Z3_mk_finite_domain_sort , <Z3_context> -> <Z3_symbol> -> CULong -> IO <Z3_sort>
#ccall Z3_mk_array_sort , <Z3_context> -> <Z3_sort> -> <Z3_sort> -> IO <Z3_sort>
#ccall Z3_mk_tuple_sort , <Z3_context> -> <Z3_symbol> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> IO <Z3_sort>
#ccall Z3_mk_enumeration_sort , <Z3_context> -> <Z3_symbol> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> IO <Z3_sort>
#ccall Z3_mk_list_sort , <Z3_context> -> <Z3_symbol> -> <Z3_sort> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> IO <Z3_sort>
#ccall Z3_mk_constructor , <Z3_context> -> <Z3_symbol> -> <Z3_symbol> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> Ptr CUInt -> IO <Z3_constructor>
#ccall Z3_del_constructor , <Z3_context> -> <Z3_constructor> -> IO ()
#ccall Z3_mk_datatype , <Z3_context> -> <Z3_symbol> -> CUInt -> Ptr <Z3_constructor> -> IO <Z3_sort>
#ccall Z3_mk_constructor_list , <Z3_context> -> CUInt -> Ptr <Z3_constructor> -> IO <Z3_constructor_list>
#ccall Z3_del_constructor_list , <Z3_context> -> <Z3_constructor_list> -> IO ()
#ccall Z3_mk_datatypes , <Z3_context> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> Ptr <Z3_constructor_list> -> IO ()
#ccall Z3_query_constructor , <Z3_context> -> <Z3_constructor> -> CUInt -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> IO ()
#ccall Z3_mk_func_decl , <Z3_context> -> <Z3_symbol> -> CUInt -> Ptr <Z3_sort> -> <Z3_sort> -> IO <Z3_func_decl>
#ccall Z3_mk_app , <Z3_context> -> <Z3_func_decl> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_const , <Z3_context> -> <Z3_symbol> -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_fresh_func_decl , <Z3_context> -> <Z3_string> -> CUInt -> Ptr <Z3_sort> -> <Z3_sort> -> IO <Z3_func_decl>
#ccall Z3_mk_fresh_const , <Z3_context> -> <Z3_string> -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_true , <Z3_context> -> IO <Z3_ast>
#ccall Z3_mk_false , <Z3_context> -> IO <Z3_ast>
#ccall Z3_mk_eq , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_distinct , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_not , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_ite , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_iff , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_implies , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_xor , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_and , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_or , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_add , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_mul , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_sub , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_unary_minus , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_div , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_mod , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_rem , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_power , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_lt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_le , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_gt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_ge , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_int2real , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_real2int , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_is_int , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvnot , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvredand , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvredor , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvand , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvor , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvxor , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvnand , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvnor , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvxnor , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvneg , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvadd , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvsub , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvmul , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvudiv , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvsdiv , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvurem , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvsrem , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvsmod , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvult , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvslt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvule , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvsle , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvuge , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvsge , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvugt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvsgt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_concat , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_extract , <Z3_context> -> CUInt -> CUInt -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_sign_ext , <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_zero_ext , <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_repeat , <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvshl , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvlshr , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvashr , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_rotate_left , <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_rotate_right , <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_ext_rotate_left , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_ext_rotate_right , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_int2bv , <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bv2int , <Z3_context> -> <Z3_ast> -> CInt -> IO <Z3_ast>
#ccall Z3_mk_bvadd_no_overflow , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> CInt -> IO <Z3_ast>
#ccall Z3_mk_bvadd_no_underflow , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvsub_no_overflow , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvsub_no_underflow , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> CInt -> IO <Z3_ast>
#ccall Z3_mk_bvsdiv_no_overflow , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvneg_no_overflow , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_bvmul_no_overflow , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> CInt -> IO <Z3_ast>
#ccall Z3_mk_bvmul_no_underflow , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_select , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_store , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_const_array , <Z3_context> -> <Z3_sort> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_map , <Z3_context> -> <Z3_func_decl> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_array_default , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_set_sort , <Z3_context> -> <Z3_sort> -> IO <Z3_sort>
#ccall Z3_mk_empty_set , <Z3_context> -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_full_set , <Z3_context> -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_set_add , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_set_del , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_set_union , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_set_intersect , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_set_difference , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_set_complement , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_set_member , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_set_subset , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_array_ext , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_numeral , <Z3_context> -> <Z3_string> -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_real , <Z3_context> -> CInt -> CInt -> IO <Z3_ast>
#ccall Z3_mk_int , <Z3_context> -> CInt -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_unsigned_int , <Z3_context> -> CUInt -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_int64 , <Z3_context> -> CLong -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_unsigned_int64 , <Z3_context> -> CULong -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_seq_sort , <Z3_context> -> <Z3_sort> -> IO <Z3_sort>
#ccall Z3_is_seq_sort , <Z3_context> -> <Z3_sort> -> IO CInt
#ccall Z3_mk_re_sort , <Z3_context> -> <Z3_sort> -> IO <Z3_sort>
#ccall Z3_is_re_sort , <Z3_context> -> <Z3_sort> -> IO CInt
#ccall Z3_mk_string_sort , <Z3_context> -> IO <Z3_sort>
#ccall Z3_is_string_sort , <Z3_context> -> <Z3_sort> -> IO CInt
#ccall Z3_mk_string , <Z3_context> -> <Z3_string> -> IO <Z3_ast>
#ccall Z3_is_string , <Z3_context> -> <Z3_ast> -> IO CInt
#ccall Z3_get_string , <Z3_context> -> <Z3_ast> -> IO <Z3_string>
#ccall Z3_mk_seq_empty , <Z3_context> -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_seq_unit , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_seq_concat , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_seq_prefix , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_seq_suffix , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_seq_contains , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_seq_extract , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_seq_replace , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_seq_at , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_seq_length , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_seq_index , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_seq_to_re , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_seq_in_re , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_re_plus , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_re_star , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_re_option , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_re_union , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_re_concat , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_pattern , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_pattern>
#ccall Z3_mk_bound , <Z3_context> -> CUInt -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_mk_forall , <Z3_context> -> CUInt -> CUInt -> Ptr <Z3_pattern> -> CUInt -> Ptr <Z3_sort> -> Ptr <Z3_symbol> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_exists , <Z3_context> -> CUInt -> CUInt -> Ptr <Z3_pattern> -> CUInt -> Ptr <Z3_sort> -> Ptr <Z3_symbol> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_quantifier , <Z3_context> -> CInt -> CUInt -> CUInt -> Ptr <Z3_pattern> -> CUInt -> Ptr <Z3_sort> -> Ptr <Z3_symbol> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_quantifier_ex , <Z3_context> -> CInt -> CUInt -> <Z3_symbol> -> <Z3_symbol> -> CUInt -> Ptr <Z3_pattern> -> CUInt -> Ptr <Z3_ast> -> CUInt -> Ptr <Z3_sort> -> Ptr <Z3_symbol> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_forall_const , <Z3_context> -> CUInt -> CUInt -> Ptr <Z3_app> -> CUInt -> Ptr <Z3_pattern> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_exists_const , <Z3_context> -> CUInt -> CUInt -> Ptr <Z3_app> -> CUInt -> Ptr <Z3_pattern> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_quantifier_const , <Z3_context> -> CInt -> CUInt -> CUInt -> Ptr <Z3_app> -> CUInt -> Ptr <Z3_pattern> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_mk_quantifier_const_ex , <Z3_context> -> CInt -> CUInt -> <Z3_symbol> -> <Z3_symbol> -> CUInt -> Ptr <Z3_app> -> CUInt -> Ptr <Z3_pattern> -> CUInt -> Ptr <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_get_symbol_kind , <Z3_context> -> <Z3_symbol> -> IO <Z3_symbol_kind>
#ccall Z3_get_symbol_int , <Z3_context> -> <Z3_symbol> -> IO CInt
#ccall Z3_get_symbol_string , <Z3_context> -> <Z3_symbol> -> IO <Z3_string>
#ccall Z3_get_sort_name , <Z3_context> -> <Z3_sort> -> IO <Z3_symbol>
#ccall Z3_get_sort_id , <Z3_context> -> <Z3_sort> -> IO ()
#ccall Z3_sort_to_ast , <Z3_context> -> <Z3_sort> -> IO <Z3_ast>
#ccall Z3_is_eq_sort , <Z3_context> -> <Z3_sort> -> <Z3_sort> -> IO CInt
#ccall Z3_get_sort_kind , <Z3_context> -> <Z3_sort> -> IO <Z3_sort_kind>
#ccall Z3_get_bv_sort_size , <Z3_context> -> <Z3_sort> -> IO ()
#ccall Z3_get_finite_domain_sort_size , <Z3_context> -> <Z3_sort> -> Ptr CULong -> IO CInt
#ccall Z3_get_array_sort_domain , <Z3_context> -> <Z3_sort> -> IO <Z3_sort>
#ccall Z3_get_array_sort_range , <Z3_context> -> <Z3_sort> -> IO <Z3_sort>
#ccall Z3_get_tuple_sort_mk_decl , <Z3_context> -> <Z3_sort> -> IO <Z3_func_decl>
#ccall Z3_get_tuple_sort_num_fields , <Z3_context> -> <Z3_sort> -> IO ()
#ccall Z3_get_tuple_sort_field_decl , <Z3_context> -> <Z3_sort> -> CUInt -> IO <Z3_func_decl>
#ccall Z3_get_datatype_sort_num_constructors , <Z3_context> -> <Z3_sort> -> IO ()
#ccall Z3_get_datatype_sort_constructor , <Z3_context> -> <Z3_sort> -> CUInt -> IO <Z3_func_decl>
#ccall Z3_get_datatype_sort_recognizer , <Z3_context> -> <Z3_sort> -> CUInt -> IO <Z3_func_decl>
#ccall Z3_get_datatype_sort_constructor_accessor , <Z3_context> -> <Z3_sort> -> CUInt -> CUInt -> IO <Z3_func_decl>
#ccall Z3_datatype_update_field , <Z3_context> -> <Z3_func_decl> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_get_relation_arity , <Z3_context> -> <Z3_sort> -> IO ()
#ccall Z3_get_relation_column , <Z3_context> -> <Z3_sort> -> CUInt -> IO <Z3_sort>
#ccall Z3_mk_atmost , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> CUInt -> IO <Z3_ast>
#ccall Z3_mk_pble , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> Ptr CInt -> CInt -> IO <Z3_ast>
#ccall Z3_mk_pbeq , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> Ptr CInt -> CInt -> IO <Z3_ast>
#ccall Z3_func_decl_to_ast , <Z3_context> -> <Z3_func_decl> -> IO <Z3_ast>
#ccall Z3_is_eq_func_decl , <Z3_context> -> <Z3_func_decl> -> <Z3_func_decl> -> IO CInt
#ccall Z3_get_func_decl_id , <Z3_context> -> <Z3_func_decl> -> IO ()
#ccall Z3_get_decl_name , <Z3_context> -> <Z3_func_decl> -> IO <Z3_symbol>
#ccall Z3_get_decl_kind , <Z3_context> -> <Z3_func_decl> -> IO <Z3_decl_kind>
#ccall Z3_get_domain_size , <Z3_context> -> <Z3_func_decl> -> IO ()
#ccall Z3_get_arity , <Z3_context> -> <Z3_func_decl> -> IO ()
#ccall Z3_get_domain , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_sort>
#ccall Z3_get_range , <Z3_context> -> <Z3_func_decl> -> IO <Z3_sort>
#ccall Z3_get_decl_num_parameters , <Z3_context> -> <Z3_func_decl> -> IO ()
#ccall Z3_get_decl_parameter_kind , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_parameter_kind>
#ccall Z3_get_decl_int_parameter , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO CInt
#ccall Z3_get_decl_double_parameter , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO CDouble
#ccall Z3_get_decl_symbol_parameter , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_symbol>
#ccall Z3_get_decl_sort_parameter , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_sort>
#ccall Z3_get_decl_ast_parameter , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_ast>
#ccall Z3_get_decl_func_decl_parameter , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_func_decl>
#ccall Z3_get_decl_rational_parameter , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_string>
#ccall Z3_app_to_ast , <Z3_context> -> <Z3_app> -> IO <Z3_ast>
#ccall Z3_get_app_decl , <Z3_context> -> <Z3_app> -> IO <Z3_func_decl>
#ccall Z3_get_app_num_args , <Z3_context> -> <Z3_app> -> IO ()
#ccall Z3_get_app_arg , <Z3_context> -> <Z3_app> -> CUInt -> IO <Z3_ast>
#ccall Z3_is_eq_ast , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt
#ccall Z3_get_ast_id , <Z3_context> -> <Z3_ast> -> IO ()
#ccall Z3_get_ast_hash , <Z3_context> -> <Z3_ast> -> IO ()
#ccall Z3_get_sort , <Z3_context> -> <Z3_ast> -> IO <Z3_sort>
#ccall Z3_is_well_sorted , <Z3_context> -> <Z3_ast> -> IO CInt
#ccall Z3_get_bool_value , <Z3_context> -> <Z3_ast> -> IO <Z3_lbool>
#ccall Z3_get_ast_kind , <Z3_context> -> <Z3_ast> -> IO <Z3_ast_kind>
#ccall Z3_is_app , <Z3_context> -> <Z3_ast> -> IO CInt
#ccall Z3_is_numeral_ast , <Z3_context> -> <Z3_ast> -> IO CInt
#ccall Z3_is_algebraic_number , <Z3_context> -> <Z3_ast> -> IO CInt
#ccall Z3_to_app , <Z3_context> -> <Z3_ast> -> IO <Z3_app>
#ccall Z3_to_func_decl , <Z3_context> -> <Z3_ast> -> IO <Z3_func_decl>
#ccall Z3_get_numeral_string , <Z3_context> -> <Z3_ast> -> IO <Z3_string>
#ccall Z3_get_numeral_decimal_string , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_string>
#ccall Z3_get_numerator , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_get_denominator , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_get_numeral_small , <Z3_context> -> <Z3_ast> -> Ptr CLong -> Ptr CLong -> IO CInt
#ccall Z3_get_numeral_int , <Z3_context> -> <Z3_ast> -> Ptr CInt -> IO CInt
#ccall Z3_get_numeral_uint , <Z3_context> -> <Z3_ast> -> Ptr CUInt -> IO CInt
#ccall Z3_get_numeral_uint64 , <Z3_context> -> <Z3_ast> -> Ptr CULong -> IO CInt
#ccall Z3_get_numeral_int64 , <Z3_context> -> <Z3_ast> -> Ptr CLong -> IO CInt
#ccall Z3_get_numeral_rational_int64 , <Z3_context> -> <Z3_ast> -> Ptr CLong -> Ptr CLong -> IO CInt
#ccall Z3_get_algebraic_number_lower , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_ast>
#ccall Z3_get_algebraic_number_upper , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_ast>
#ccall Z3_pattern_to_ast , <Z3_context> -> <Z3_pattern> -> IO <Z3_ast>
#ccall Z3_get_pattern_num_terms , <Z3_context> -> <Z3_pattern> -> IO ()
#ccall Z3_get_pattern , <Z3_context> -> <Z3_pattern> -> CUInt -> IO <Z3_ast>
#ccall Z3_get_index_value , <Z3_context> -> <Z3_ast> -> IO ()
#ccall Z3_is_quantifier_forall , <Z3_context> -> <Z3_ast> -> IO CInt
#ccall Z3_get_quantifier_weight , <Z3_context> -> <Z3_ast> -> IO ()
#ccall Z3_get_quantifier_num_patterns , <Z3_context> -> <Z3_ast> -> IO ()
#ccall Z3_get_quantifier_pattern_ast , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_pattern>
#ccall Z3_get_quantifier_num_no_patterns , <Z3_context> -> <Z3_ast> -> IO ()
#ccall Z3_get_quantifier_no_pattern_ast , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_ast>
#ccall Z3_get_quantifier_num_bound , <Z3_context> -> <Z3_ast> -> IO ()
#ccall Z3_get_quantifier_bound_name , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_symbol>
#ccall Z3_get_quantifier_bound_sort , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_sort>
#ccall Z3_get_quantifier_body , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_simplify , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>
#ccall Z3_simplify_ex , <Z3_context> -> <Z3_ast> -> <Z3_params> -> IO <Z3_ast>
#ccall Z3_simplify_get_help , <Z3_context> -> IO <Z3_string>
#ccall Z3_simplify_get_param_descrs , <Z3_context> -> IO <Z3_param_descrs>
#ccall Z3_update_term , <Z3_context> -> <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_substitute , <Z3_context> -> <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_substitute_vars , <Z3_context> -> <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
#ccall Z3_translate , <Z3_context> -> <Z3_ast> -> <Z3_context> -> IO <Z3_ast>
#ccall Z3_model_inc_ref , <Z3_context> -> <Z3_model> -> IO ()
#ccall Z3_model_dec_ref , <Z3_context> -> <Z3_model> -> IO ()
#ccall Z3_model_eval , <Z3_context> -> <Z3_model> -> <Z3_ast> -> CInt -> Ptr <Z3_ast> -> IO CInt
#ccall Z3_model_get_const_interp , <Z3_context> -> <Z3_model> -> <Z3_func_decl> -> IO <Z3_ast>
#ccall Z3_model_has_interp , <Z3_context> -> <Z3_model> -> <Z3_func_decl> -> IO CInt
#ccall Z3_model_get_func_interp , <Z3_context> -> <Z3_model> -> <Z3_func_decl> -> IO <Z3_func_interp>
#ccall Z3_model_get_num_consts , <Z3_context> -> <Z3_model> -> IO ()
#ccall Z3_model_get_const_decl , <Z3_context> -> <Z3_model> -> CUInt -> IO <Z3_func_decl>
#ccall Z3_model_get_num_funcs , <Z3_context> -> <Z3_model> -> IO ()
#ccall Z3_model_get_func_decl , <Z3_context> -> <Z3_model> -> CUInt -> IO <Z3_func_decl>
#ccall Z3_model_get_num_sorts , <Z3_context> -> <Z3_model> -> IO ()
#ccall Z3_model_get_sort , <Z3_context> -> <Z3_model> -> CUInt -> IO <Z3_sort>
#ccall Z3_model_get_sort_universe , <Z3_context> -> <Z3_model> -> <Z3_sort> -> IO <Z3_ast_vector>
#ccall Z3_is_as_array , <Z3_context> -> <Z3_ast> -> IO CInt
#ccall Z3_get_as_array_func_decl , <Z3_context> -> <Z3_ast> -> IO <Z3_func_decl>
#ccall Z3_func_interp_inc_ref , <Z3_context> -> <Z3_func_interp> -> IO ()
#ccall Z3_func_interp_dec_ref , <Z3_context> -> <Z3_func_interp> -> IO ()
#ccall Z3_func_interp_get_num_entries , <Z3_context> -> <Z3_func_interp> -> IO ()
#ccall Z3_func_interp_get_entry , <Z3_context> -> <Z3_func_interp> -> CUInt -> IO <Z3_func_entry>
#ccall Z3_func_interp_get_else , <Z3_context> -> <Z3_func_interp> -> IO <Z3_ast>
#ccall Z3_func_interp_get_arity , <Z3_context> -> <Z3_func_interp> -> IO ()
#ccall Z3_func_entry_inc_ref , <Z3_context> -> <Z3_func_entry> -> IO ()
#ccall Z3_func_entry_dec_ref , <Z3_context> -> <Z3_func_entry> -> IO ()
#ccall Z3_func_entry_get_value , <Z3_context> -> <Z3_func_entry> -> IO <Z3_ast>
#ccall Z3_func_entry_get_num_args , <Z3_context> -> <Z3_func_entry> -> IO ()
#ccall Z3_func_entry_get_arg , <Z3_context> -> <Z3_func_entry> -> CUInt -> IO <Z3_ast>
#ccall Z3_open_log , <Z3_string> -> IO CInt
#ccall Z3_append_log , <Z3_string> -> IO ()
#ccall Z3_close_log , IO ()
#ccall Z3_toggle_warning_messages , CInt -> IO ()
#ccall Z3_set_ast_print_mode , <Z3_context> -> <Z3_ast_print_mode> -> IO ()
#ccall Z3_ast_to_string , <Z3_context> -> <Z3_ast> -> IO <Z3_string>
#ccall Z3_pattern_to_string , <Z3_context> -> <Z3_pattern> -> IO <Z3_string>
#ccall Z3_sort_to_string , <Z3_context> -> <Z3_sort> -> IO <Z3_string>
#ccall Z3_func_decl_to_string , <Z3_context> -> <Z3_func_decl> -> IO <Z3_string>
#ccall Z3_model_to_string , <Z3_context> -> <Z3_model> -> IO <Z3_string>
#ccall Z3_benchmark_to_smtlib_string , <Z3_context> -> <Z3_string> -> <Z3_string> -> <Z3_string> -> <Z3_string> -> CUInt -> Ptr <Z3_ast> -> <Z3_ast> -> IO <Z3_string>
#ccall Z3_parse_smtlib2_string , <Z3_context> -> <Z3_string> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_func_decl> -> IO <Z3_ast>
#ccall Z3_parse_smtlib2_file , <Z3_context> -> <Z3_string> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_func_decl> -> IO <Z3_ast>
#ccall Z3_parse_smtlib_string , <Z3_context> -> <Z3_string> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_func_decl> -> IO ()
#ccall Z3_parse_smtlib_file , <Z3_context> -> <Z3_string> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_func_decl> -> IO ()
#ccall Z3_get_smtlib_num_formulas , <Z3_context> -> IO ()
#ccall Z3_get_smtlib_formula , <Z3_context> -> CUInt -> IO <Z3_ast>
#ccall Z3_get_smtlib_num_assumptions , <Z3_context> -> IO ()
#ccall Z3_get_smtlib_assumption , <Z3_context> -> CUInt -> IO <Z3_ast>
#ccall Z3_get_smtlib_num_decls , <Z3_context> -> IO ()
#ccall Z3_get_smtlib_decl , <Z3_context> -> CUInt -> IO <Z3_func_decl>
#ccall Z3_get_smtlib_num_sorts , <Z3_context> -> IO ()
#ccall Z3_get_smtlib_sort , <Z3_context> -> CUInt -> IO <Z3_sort>
#ccall Z3_get_smtlib_error , <Z3_context> -> IO <Z3_string>
#ccall Z3_get_error_code , <Z3_context> -> IO <Z3_error_code>
#ccall Z3_set_error_handler , <Z3_context> -> <Z3_error_handler> -> IO ()
#ccall Z3_set_error , <Z3_context> -> <Z3_error_code> -> IO ()
#ccall Z3_get_error_msg , <Z3_context> -> <Z3_error_code> -> IO <Z3_string>
#ccall Z3_get_error_msg_ex , <Z3_context> -> <Z3_error_code> -> IO <Z3_string>
#ccall Z3_get_version , Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> IO ()
#ccall Z3_get_full_version , IO <Z3_string>
#ccall Z3_enable_trace , <Z3_string> -> IO ()
#ccall Z3_disable_trace , <Z3_string> -> IO ()
#ccall Z3_reset_memory , IO ()
#ccall Z3_finalize_memory , IO ()
#ccall Z3_mk_goal , <Z3_context> -> CInt -> CInt -> CInt -> IO <Z3_goal>
#ccall Z3_goal_inc_ref , <Z3_context> -> <Z3_goal> -> IO ()
#ccall Z3_goal_dec_ref , <Z3_context> -> <Z3_goal> -> IO ()
#ccall Z3_goal_precision , <Z3_context> -> <Z3_goal> -> IO <Z3_goal_prec>
#ccall Z3_goal_assert , <Z3_context> -> <Z3_goal> -> <Z3_ast> -> IO ()
#ccall Z3_goal_inconsistent , <Z3_context> -> <Z3_goal> -> IO CInt
#ccall Z3_goal_depth , <Z3_context> -> <Z3_goal> -> IO ()
#ccall Z3_goal_reset , <Z3_context> -> <Z3_goal> -> IO ()
#ccall Z3_goal_size , <Z3_context> -> <Z3_goal> -> IO ()
#ccall Z3_goal_formula , <Z3_context> -> <Z3_goal> -> CUInt -> IO <Z3_ast>
#ccall Z3_goal_num_exprs , <Z3_context> -> <Z3_goal> -> IO ()
#ccall Z3_goal_is_decided_sat , <Z3_context> -> <Z3_goal> -> IO CInt
#ccall Z3_goal_is_decided_unsat , <Z3_context> -> <Z3_goal> -> IO CInt
#ccall Z3_goal_translate , <Z3_context> -> <Z3_goal> -> <Z3_context> -> IO <Z3_goal>
#ccall Z3_goal_to_string , <Z3_context> -> <Z3_goal> -> IO <Z3_string>
#ccall Z3_mk_tactic , <Z3_context> -> <Z3_string> -> IO <Z3_tactic>
#ccall Z3_tactic_inc_ref , <Z3_context> -> <Z3_tactic> -> IO ()
#ccall Z3_tactic_dec_ref , <Z3_context> -> <Z3_tactic> -> IO ()
#ccall Z3_mk_probe , <Z3_context> -> <Z3_string> -> IO <Z3_probe>
#ccall Z3_probe_inc_ref , <Z3_context> -> <Z3_probe> -> IO ()
#ccall Z3_probe_dec_ref , <Z3_context> -> <Z3_probe> -> IO ()
#ccall Z3_tactic_and_then , <Z3_context> -> <Z3_tactic> -> <Z3_tactic> -> IO <Z3_tactic>
#ccall Z3_tactic_or_else , <Z3_context> -> <Z3_tactic> -> <Z3_tactic> -> IO <Z3_tactic>
#ccall Z3_tactic_par_or , <Z3_context> -> CUInt -> Ptr <Z3_tactic> -> IO <Z3_tactic>
#ccall Z3_tactic_par_and_then , <Z3_context> -> <Z3_tactic> -> <Z3_tactic> -> IO <Z3_tactic>
#ccall Z3_tactic_try_for , <Z3_context> -> <Z3_tactic> -> CUInt -> IO <Z3_tactic>
#ccall Z3_tactic_when , <Z3_context> -> <Z3_probe> -> <Z3_tactic> -> IO <Z3_tactic>
#ccall Z3_tactic_cond , <Z3_context> -> <Z3_probe> -> <Z3_tactic> -> <Z3_tactic> -> IO <Z3_tactic>
#ccall Z3_tactic_repeat , <Z3_context> -> <Z3_tactic> -> CUInt -> IO <Z3_tactic>
#ccall Z3_tactic_skip , <Z3_context> -> IO <Z3_tactic>
#ccall Z3_tactic_fail , <Z3_context> -> IO <Z3_tactic>
#ccall Z3_tactic_fail_if , <Z3_context> -> <Z3_probe> -> IO <Z3_tactic>
#ccall Z3_tactic_fail_if_not_decided , <Z3_context> -> IO <Z3_tactic>
#ccall Z3_tactic_using_params , <Z3_context> -> <Z3_tactic> -> <Z3_params> -> IO <Z3_tactic>
#ccall Z3_probe_const , <Z3_context> -> CDouble -> IO <Z3_probe>
#ccall Z3_probe_lt , <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>
#ccall Z3_probe_gt , <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>
#ccall Z3_probe_le , <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>
#ccall Z3_probe_ge , <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>
#ccall Z3_probe_eq , <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>
#ccall Z3_probe_and , <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>
#ccall Z3_probe_or , <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>
#ccall Z3_probe_not , <Z3_context> -> <Z3_probe> -> IO <Z3_probe>
#ccall Z3_get_num_tactics , <Z3_context> -> IO ()
#ccall Z3_get_tactic_name , <Z3_context> -> CUInt -> IO <Z3_string>
#ccall Z3_get_num_probes , <Z3_context> -> IO ()
#ccall Z3_get_probe_name , <Z3_context> -> CUInt -> IO <Z3_string>
#ccall Z3_tactic_get_help , <Z3_context> -> <Z3_tactic> -> IO <Z3_string>
#ccall Z3_tactic_get_param_descrs , <Z3_context> -> <Z3_tactic> -> IO <Z3_param_descrs>
#ccall Z3_tactic_get_descr , <Z3_context> -> <Z3_string> -> IO <Z3_string>
#ccall Z3_probe_get_descr , <Z3_context> -> <Z3_string> -> IO <Z3_string>
#ccall Z3_probe_apply , <Z3_context> -> <Z3_probe> -> <Z3_goal> -> IO CDouble
#ccall Z3_tactic_apply , <Z3_context> -> <Z3_tactic> -> <Z3_goal> -> IO <Z3_apply_result>
#ccall Z3_tactic_apply_ex , <Z3_context> -> <Z3_tactic> -> <Z3_goal> -> <Z3_params> -> IO <Z3_apply_result>
#ccall Z3_apply_result_inc_ref , <Z3_context> -> <Z3_apply_result> -> IO ()
#ccall Z3_apply_result_dec_ref , <Z3_context> -> <Z3_apply_result> -> IO ()
#ccall Z3_apply_result_to_string , <Z3_context> -> <Z3_apply_result> -> IO <Z3_string>
#ccall Z3_apply_result_get_num_subgoals , <Z3_context> -> <Z3_apply_result> -> IO ()
#ccall Z3_apply_result_get_subgoal , <Z3_context> -> <Z3_apply_result> -> CUInt -> IO <Z3_goal>
#ccall Z3_apply_result_convert_model , <Z3_context> -> <Z3_apply_result> -> CUInt -> <Z3_model> -> IO <Z3_model>
#ccall Z3_mk_solver , <Z3_context> -> IO <Z3_solver>
#ccall Z3_mk_simple_solver , <Z3_context> -> IO <Z3_solver>
#ccall Z3_mk_solver_for_logic , <Z3_context> -> <Z3_symbol> -> IO <Z3_solver>
#ccall Z3_mk_solver_from_tactic , <Z3_context> -> <Z3_tactic> -> IO <Z3_solver>
#ccall Z3_solver_translate , <Z3_context> -> <Z3_solver> -> <Z3_context> -> IO <Z3_solver>
#ccall Z3_solver_get_help , <Z3_context> -> <Z3_solver> -> IO <Z3_string>
#ccall Z3_solver_get_param_descrs , <Z3_context> -> <Z3_solver> -> IO <Z3_param_descrs>
#ccall Z3_solver_set_params , <Z3_context> -> <Z3_solver> -> <Z3_params> -> IO ()
#ccall Z3_solver_inc_ref , <Z3_context> -> <Z3_solver> -> IO ()
#ccall Z3_solver_dec_ref , <Z3_context> -> <Z3_solver> -> IO ()
#ccall Z3_solver_push , <Z3_context> -> <Z3_solver> -> IO ()
#ccall Z3_solver_pop , <Z3_context> -> <Z3_solver> -> CUInt -> IO ()
#ccall Z3_solver_reset , <Z3_context> -> <Z3_solver> -> IO ()
#ccall Z3_solver_get_num_scopes , <Z3_context> -> <Z3_solver> -> IO ()
#ccall Z3_solver_assert , <Z3_context> -> <Z3_solver> -> <Z3_ast> -> IO ()
#ccall Z3_solver_assert_and_track , <Z3_context> -> <Z3_solver> -> <Z3_ast> -> <Z3_ast> -> IO ()
#ccall Z3_solver_get_assertions , <Z3_context> -> <Z3_solver> -> IO <Z3_ast_vector>
#ccall Z3_solver_check , <Z3_context> -> <Z3_solver> -> IO <Z3_lbool>
#ccall Z3_solver_check_assumptions , <Z3_context> -> <Z3_solver> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_lbool>
#ccall Z3_get_implied_equalities , <Z3_context> -> <Z3_solver> -> CUInt -> Ptr <Z3_ast> -> Ptr CUInt -> IO <Z3_lbool>
#ccall Z3_solver_get_consequences , <Z3_context> -> <Z3_solver> -> <Z3_ast_vector> -> <Z3_ast_vector> -> <Z3_ast_vector> -> IO <Z3_lbool>
#ccall Z3_solver_get_model , <Z3_context> -> <Z3_solver> -> IO <Z3_model>
#ccall Z3_solver_get_proof , <Z3_context> -> <Z3_solver> -> IO <Z3_ast>
#ccall Z3_solver_get_unsat_core , <Z3_context> -> <Z3_solver> -> IO <Z3_ast_vector>
#ccall Z3_solver_get_reason_unknown , <Z3_context> -> <Z3_solver> -> IO <Z3_string>
#ccall Z3_solver_get_statistics , <Z3_context> -> <Z3_solver> -> IO <Z3_stats>
#ccall Z3_solver_to_string , <Z3_context> -> <Z3_solver> -> IO <Z3_string>
#ccall Z3_stats_to_string , <Z3_context> -> <Z3_stats> -> IO <Z3_string>
#ccall Z3_stats_inc_ref , <Z3_context> -> <Z3_stats> -> IO ()
#ccall Z3_stats_dec_ref , <Z3_context> -> <Z3_stats> -> IO ()
#ccall Z3_stats_size , <Z3_context> -> <Z3_stats> -> IO ()
#ccall Z3_stats_get_key , <Z3_context> -> <Z3_stats> -> CUInt -> IO <Z3_string>
#ccall Z3_stats_is_uint , <Z3_context> -> <Z3_stats> -> CUInt -> IO CInt
#ccall Z3_stats_is_double , <Z3_context> -> <Z3_stats> -> CUInt -> IO CInt
#ccall Z3_stats_get_uint_value , <Z3_context> -> <Z3_stats> -> CUInt -> IO ()
#ccall Z3_stats_get_double_value , <Z3_context> -> <Z3_stats> -> CUInt -> IO CDouble
#ccall Z3_get_estimated_alloc_size , IO CULong
