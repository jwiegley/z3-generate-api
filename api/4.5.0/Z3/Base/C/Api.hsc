{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "z3_macros.h"
#include "z3_api.h"
#include "z3_api.h"
module Z3.Base.C.Api where
import Foreign.Ptr
#strict_import
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

{- | Z3 Boolean type. It is just an alias for \c int. -}
#ccall Z3_global_param_set , <Z3_string> -> <Z3_string> -> IO ()

{- | Restore the value of all global (and module) parameters.
       This command will not affect already created objects (such as tactics and solvers).

       \sa Z3_global_param_set -}
#ccall Z3_global_param_reset_all , IO ()

{- | Get a global (or module) parameter.

       Returns \c Z3_FALSE if the parameter value does not exist.

       \sa Z3_global_param_set

       \remark This function cannot be invoked simultaneously from different threads without synchronization.
       The result string stored in param_value is stored in shared location. -}
#ccall Z3_global_param_get , <Z3_string> -> <Z3_string_ptr> -> IO CInt
#ccall Z3_mk_config , IO <Z3_config>
#ccall Z3_del_config , <Z3_config> -> IO ()
#ccall Z3_set_param_value , <Z3_config> -> <Z3_string> -> <Z3_string> -> IO ()
#ccall Z3_mk_context , <Z3_config> -> IO <Z3_context>

{- | Create a context using the given configuration.
       This function is similar to #Z3_mk_context. However,
       in the context returned by this function, the user
       is responsible for managing Z3_ast reference counters.
       Managing reference counters is a burden and error-prone,
       but allows the user to use the memory more efficiently.
       The user must invoke #Z3_inc_ref for any Z3_ast returned
       by Z3, and #Z3_dec_ref whenever the Z3_ast is not needed
       anymore. This idiom is similar to the one used in
       BDD (binary decision diagrams) packages such as CUDD.

       Remarks:

       - Z3_sort, Z3_func_decl, Z3_app, Z3_pattern are Z3_ast's.
       - After a context is created, the configuration cannot be changed.
       - All main interaction with Z3 happens in the context of a \c Z3_context.
       - Z3 uses hash-consing, i.e., when the same Z3_ast is created twice,
         Z3 will return the same pointer twice. -}
#ccall Z3_mk_context_rc , <Z3_config> -> IO <Z3_context>

{- | Delete the given logical context.

       \sa Z3_mk_context -}
#ccall Z3_del_context , <Z3_context> -> IO ()

{- | Increment the reference counter of the given AST.
       The context \c c should have been created using #Z3_mk_context_rc.
       This function is a NOOP if \c c was created using #Z3_mk_context. -}
#ccall Z3_inc_ref , <Z3_context> -> <Z3_ast> -> IO ()

{- | Decrement the reference counter of the given AST.
       The context \c c should have been created using #Z3_mk_context_rc.
       This function is a NOOP if \c c was created using #Z3_mk_context. -}
#ccall Z3_dec_ref , <Z3_context> -> <Z3_ast> -> IO ()
#ccall Z3_update_param_value , <Z3_context> -> <Z3_string> -> <Z3_string> -> IO ()

{- | Interrupt the execution of a Z3 procedure.
       This procedure can be used to interrupt: solvers, simplifiers and tactics. -}
#ccall Z3_interrupt , <Z3_context> -> IO ()

{- | Create a Z3 (empty) parameter set.
       Starting at Z3 4.0, parameter sets are used to configure many components such as:
       simplifiers, tactics, solvers, etc.

       \remark Reference counting must be used to manage parameter sets, even when the Z3_context was
       created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
#ccall Z3_mk_params , <Z3_context> -> IO <Z3_params>

{- | Increment the reference counter of the given parameter set. -}
#ccall Z3_params_inc_ref , <Z3_context> -> <Z3_params> -> IO ()

{- | Decrement the reference counter of the given parameter set. -}
#ccall Z3_params_dec_ref , <Z3_context> -> <Z3_params> -> IO ()

{- | Add a Boolean parameter \c k with value \c v to the parameter set \c p. -}
#ccall Z3_params_set_bool , <Z3_context> -> <Z3_params> -> <Z3_symbol> -> CInt -> IO ()

{- | Add a unsigned parameter \c k with value \c v to the parameter set \c p. -}
#ccall Z3_params_set_uint , <Z3_context> -> <Z3_params> -> <Z3_symbol> -> CUInt -> IO ()

{- | Add a double parameter \c k with value \c v to the parameter set \c p. -}
#ccall Z3_params_set_double , <Z3_context> -> <Z3_params> -> <Z3_symbol> -> CDouble -> IO ()

{- | Add a symbol parameter \c k with value \c v to the parameter set \c p. -}
#ccall Z3_params_set_symbol , <Z3_context> -> <Z3_params> -> <Z3_symbol> -> <Z3_symbol> -> IO ()

{- | Convert a parameter set into a string. This function is mainly used for printing the
       contents of a parameter set. -}
#ccall Z3_params_to_string , <Z3_context> -> <Z3_params> -> IO <Z3_string>

{- | Validate the parameter set \c p against the parameter description set \c d.

       The procedure invokes the error handler if \c p is invalid. -}
#ccall Z3_params_validate , <Z3_context> -> <Z3_params> -> <Z3_param_descrs> -> IO ()

{- | Increment the reference counter of the given parameter description set. -}
#ccall Z3_param_descrs_inc_ref , <Z3_context> -> <Z3_param_descrs> -> IO ()

{- | Decrement the reference counter of the given parameter description set. -}
#ccall Z3_param_descrs_dec_ref , <Z3_context> -> <Z3_param_descrs> -> IO ()

{- | Return the kind associated with the given parameter name \c n. -}
#ccall Z3_param_descrs_get_kind , <Z3_context> -> <Z3_param_descrs> -> <Z3_symbol> -> IO <Z3_param_kind>

{- | Return the number of parameters in the given parameter description set. -}
#ccall Z3_param_descrs_size , <Z3_context> -> <Z3_param_descrs> -> IO ()

{- | Return the number of parameters in the given parameter description set.

       \pre i < Z3_param_descrs_size(c, p) -}
#ccall Z3_param_descrs_get_name , <Z3_context> -> <Z3_param_descrs> -> CUInt -> IO <Z3_symbol>

{- | Retrieve documentation string corresponding to parameter name \c s. -}
#ccall Z3_param_descrs_get_documentation , <Z3_context> -> <Z3_param_descrs> -> <Z3_symbol> -> IO <Z3_string>

{- | Convert a parameter description set into a string. This function is mainly used for printing the
       contents of a parameter description set. -}
#ccall Z3_param_descrs_to_string , <Z3_context> -> <Z3_param_descrs> -> IO <Z3_string>

{- | Create a Z3 symbol using an integer.

       Symbols are used to name several term and type constructors.

       NB. Not all integers can be passed to this function.
       The legal range of unsigned integers is 0 to 2^30-1.

       \sa Z3_mk_string_symbol -}
#ccall Z3_mk_int_symbol , <Z3_context> -> CInt -> IO <Z3_symbol>

{- | Create a Z3 symbol using a C string.

       Symbols are used to name several term and type constructors.

       \sa Z3_mk_int_symbol -}
#ccall Z3_mk_string_symbol , <Z3_context> -> <Z3_string> -> IO <Z3_symbol>

{- | Create a free (uninterpreted) type using the given name (symbol).

       Two free types are considered the same iff the have the same name. -}
#ccall Z3_mk_uninterpreted_sort , <Z3_context> -> <Z3_symbol> -> IO <Z3_sort>

{- | Create the Boolean type.

       This type is used to create propositional variables and predicates. -}
#ccall Z3_mk_bool_sort , <Z3_context> -> IO <Z3_sort>

{- | Create the integer type.

       This type is not the int type found in programming languages.
       A machine integer can be represented using bit-vectors. The function
       #Z3_mk_bv_sort creates a bit-vector type.

       \sa Z3_mk_bv_sort -}
#ccall Z3_mk_int_sort , <Z3_context> -> IO <Z3_sort>

{- | Create the real type.

       Note that this type is not a floating point number. -}
#ccall Z3_mk_real_sort , <Z3_context> -> IO <Z3_sort>

{- | Create a bit-vector type of the given size.

       This type can also be seen as a machine integer.

       \remark The size of the bit-vector type must be greater than zero. -}
#ccall Z3_mk_bv_sort , <Z3_context> -> CUInt -> IO <Z3_sort>

{- | Create a named finite domain sort.

       To create constants that belong to the finite domain,
       use the APIs for creating numerals and pass a numeric
       constant together with the sort returned by this call.
       The numeric constant should be between 0 and the less
       than the size of the domain.

       \sa Z3_get_finite_domain_sort_size -}
#ccall Z3_mk_finite_domain_sort , <Z3_context> -> <Z3_symbol> -> CULong -> IO <Z3_sort>

{- | Create an array type.

       We usually represent the array type as: \ccode{[domain -> range]}.
       Arrays are usually used to model the heap/memory in software verification.

       \sa Z3_mk_select
       \sa Z3_mk_store -}
#ccall Z3_mk_array_sort , <Z3_context> -> <Z3_sort> -> <Z3_sort> -> IO <Z3_sort>

{- | Create a tuple type.

       A tuple with \c n fields has a constructor and \c n projections.
       This function will also declare the constructor and projection functions.

       \param c logical context
       \param mk_tuple_name name of the constructor function associated with the tuple type.
       \param num_fields number of fields in the tuple type.
       \param field_names name of the projection functions.
       \param field_sorts type of the tuple fields.
       \param mk_tuple_decl output parameter that will contain the constructor declaration.
       \param proj_decl output parameter that will contain the projection function declarations. This field must be a buffer of size \c num_fields allocated by the user. -}
#ccall Z3_mk_tuple_sort , <Z3_context> -> <Z3_symbol> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> IO <Z3_sort>

{- | Create a enumeration sort.

       An enumeration sort with \c n elements.
       This function will also declare the functions corresponding to the enumerations.

       \param c logical context
       \param name name of the enumeration sort.
       \param n number of elemenets in enumeration sort.
       \param enum_names names of the enumerated elements.
       \param enum_consts constants corresponding to the enumerated elements.
       \param enum_testers predicates testing if terms of the enumeration sort correspond to an enumeration.

       For example, if this function is called with three symbols A, B, C and the name S, then
       \c s is a sort whose name is S, and the function returns three terms corresponding to A, B, C in
       \c enum_consts. The array \c enum_testers has three predicates of type \ccode{(s -> Bool)}.
       The first predicate (corresponding to A) is true when applied to A, and false otherwise.
       Similarly for the other predicates. -}
#ccall Z3_mk_enumeration_sort , <Z3_context> -> <Z3_symbol> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> IO <Z3_sort>

{- | Create a list sort

       A list sort over \c elem_sort
       This function declares the corresponding constructors and testers for lists.

       \param c logical context
       \param name name of the list sort.
       \param elem_sort sort of list elements.
       \param nil_decl declaration for the empty list.
       \param is_nil_decl test for the empty list.
       \param cons_decl declaration for a cons cell.
       \param is_cons_decl cons cell test.
       \param head_decl list head.
       \param tail_decl list tail. -}
#ccall Z3_mk_list_sort , <Z3_context> -> <Z3_symbol> -> <Z3_sort> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> IO <Z3_sort>

{- | Create a constructor.

       \param c logical context.
       \param name constructor name.
       \param recognizer name of recognizer function.
       \param num_fields number of fields in constructor.
       \param field_names names of the constructor fields.
       \param sorts field sorts, 0 if the field sort refers to a recursive sort.
       \param sort_refs reference to datatype sort that is an argument to the constructor; if the corresponding
                        sort reference is 0, then the value in sort_refs should be an index referring to
                        one of the recursive datatypes that is declared. -}
#ccall Z3_mk_constructor , <Z3_context> -> <Z3_symbol> -> <Z3_symbol> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> Ptr CUInt -> IO <Z3_constructor>

{- | Reclaim memory allocated to constructor.

       \param c logical context.
       \param constr constructor. -}
#ccall Z3_del_constructor , <Z3_context> -> <Z3_constructor> -> IO ()

{- | Create datatype, such as lists, trees, records, enumerations or unions of records.
       The datatype may be recursive. Return the datatype sort.

       \param c logical context.
	   \param name name of datatype.
       \param num_constructors number of constructors passed in.
       \param constructors array of constructor containers. -}
#ccall Z3_mk_datatype , <Z3_context> -> <Z3_symbol> -> CUInt -> Ptr <Z3_constructor> -> IO <Z3_sort>

{- | Create list of constructors.

       \param c logical context.
       \param num_constructors number of constructors in list.
       \param constructors list of constructors. -}
#ccall Z3_mk_constructor_list , <Z3_context> -> CUInt -> Ptr <Z3_constructor> -> IO <Z3_constructor_list>

{- | Reclaim memory allocated for constructor list.

       Each constructor inside the constructor list must be independently reclaimed using #Z3_del_constructor.

       \param c logical context.
       \param clist constructor list container. -}
#ccall Z3_del_constructor_list , <Z3_context> -> <Z3_constructor_list> -> IO ()

{- | Create mutually recursive datatypes.

       \param c logical context.
       \param num_sorts number of datatype sorts.
       \param sort_names names of datatype sorts.
       \param sorts array of datatype sorts.
       \param constructor_lists list of constructors, one list per sort. -}
#ccall Z3_mk_datatypes , <Z3_context> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> Ptr <Z3_constructor_list> -> IO ()

{- | Query constructor for declared functions.

       \param c logical context.
       \param constr constructor container. The container must have been passed in to a #Z3_mk_datatype call.
       \param num_fields number of accessor fields in the constructor.
       \param constructor constructor function declaration, allocated by user.
       \param tester constructor test function declaration, allocated by user.
       \param accessors array of accessor function declarations allocated by user. The array must contain num_fields elements. -}
#ccall Z3_query_constructor , <Z3_context> -> <Z3_constructor> -> CUInt -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> Ptr <Z3_func_decl> -> IO ()

{- | Declare a constant or function.

       \param c logical context.
       \param s name of the constant or function.
       \param domain_size number of arguments. It is 0 when declaring a constant.
       \param domain array containing the sort of each argument. The array must contain domain_size elements. It is 0 when declaring a constant.
       \param range sort of the constant or the return sort of the function.

       After declaring a constant or function, the function
       #Z3_mk_app can be used to create a constant or function
       application.

       \sa Z3_mk_app -}
#ccall Z3_mk_func_decl , <Z3_context> -> <Z3_symbol> -> CUInt -> Ptr <Z3_sort> -> <Z3_sort> -> IO <Z3_func_decl>

{- | Create a constant or function application.

       \sa Z3_mk_func_decl -}
#ccall Z3_mk_app , <Z3_context> -> <Z3_func_decl> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Declare and create a constant.

       This function is a shorthand for:
       \code
       Z3_func_decl d = Z3_mk_func_decl(c, s, 0, 0, ty);
       Z3_ast n            = Z3_mk_app(c, d, 0, 0);
       \endcode

       \sa Z3_mk_func_decl
       \sa Z3_mk_app -}
#ccall Z3_mk_const , <Z3_context> -> <Z3_symbol> -> <Z3_sort> -> IO <Z3_ast>

{- | Declare a fresh constant or function.

       Z3 will generate an unique name for this function declaration.
       If prefix is different from \c NULL, then the name generate by Z3 will start with \c prefix.

       \remark If \c prefix is \c NULL, then it is assumed to be the empty string.

       \sa Z3_mk_func_decl -}
#ccall Z3_mk_fresh_func_decl , <Z3_context> -> <Z3_string> -> CUInt -> Ptr <Z3_sort> -> <Z3_sort> -> IO <Z3_func_decl>

{- | Declare and create a fresh constant.

       This function is a shorthand for:
       \code Z3_func_decl d = Z3_mk_fresh_func_decl(c, prefix, 0, 0, ty); Z3_ast n = Z3_mk_app(c, d, 0, 0); \endcode

       \remark If \c prefix is \c NULL, then it is assumed to be the empty string.

       \sa Z3_mk_func_decl
       \sa Z3_mk_app -}
#ccall Z3_mk_fresh_const , <Z3_context> -> <Z3_string> -> <Z3_sort> -> IO <Z3_ast>

{- | Create an AST node representing \c true. -}
#ccall Z3_mk_true , <Z3_context> -> IO <Z3_ast>

{- | Create an AST node representing \c false. -}
#ccall Z3_mk_false , <Z3_context> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{l = r}.

        The nodes \c l and \c r must have the same type. -}
#ccall Z3_mk_eq , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{distinct(args[0], ..., args[num_args-1])}.

       The \c distinct construct is used for declaring the arguments pairwise distinct.
       That is, \ccode{Forall 0 <= i < j < num_args. not args[i] = args[j]}.

       All arguments must have the same sort.

       \remark The number of arguments of a distinct construct must be greater than one. -}
#ccall Z3_mk_distinct , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{not(a)}.

        The node \c a must have Boolean sort. -}
#ccall Z3_mk_not , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing an if-then-else: \ccode{ite(t1, t2, t3)}.

       The node \c t1 must have Boolean sort, \c t2 and \c t3 must have the same sort.
       The sort of the new node is equal to the sort of \c t2 and \c t3. -}
#ccall Z3_mk_ite , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{t1 iff t2}.

       The nodes \c t1 and \c t2 must have Boolean sort. -}
#ccall Z3_mk_iff , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{t1 implies t2}.

       The nodes \c t1 and \c t2 must have Boolean sort. -}
#ccall Z3_mk_implies , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{t1 xor t2}.

       The nodes \c t1 and \c t2 must have Boolean sort. -}
#ccall Z3_mk_xor , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{args[0] and ... and args[num_args-1]}.

       The array \c args must have \c num_args elements.
       All arguments must have Boolean sort.

       \remark The number of arguments must be greater than zero. -}
#ccall Z3_mk_and , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{args[0] or ... or args[num_args-1]}.

       The array \c args must have \c num_args elements.
       All arguments must have Boolean sort.

       \remark The number of arguments must be greater than zero. -}
#ccall Z3_mk_or , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{args[0] + ... + args[num_args-1]}.

       The array \c args must have \c num_args elements.
       All arguments must have int or real sort.

       \remark The number of arguments must be greater than zero. -}
#ccall Z3_mk_add , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{args[0] * ... * args[num_args-1]}.

       The array \c args must have \c num_args elements.
       All arguments must have int or real sort.

       \remark Z3 has limited support for non-linear arithmetic.
       \remark The number of arguments must be greater than zero. -}
#ccall Z3_mk_mul , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{args[0] - ... - args[num_args - 1]}.

       The array \c args must have \c num_args elements.
       All arguments must have int or real sort.

       \remark The number of arguments must be greater than zero. -}
#ccall Z3_mk_sub , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{ - arg}.

       The arguments must have int or real type. -}
#ccall Z3_mk_unary_minus , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{arg1 div arg2}.

       The arguments must either both have int type or both have real type.
       If the arguments have int type, then the result type is an int type, otherwise the
       the result type is real. -}
#ccall Z3_mk_div , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{arg1 mod arg2}.

       The arguments must have int type. -}
#ccall Z3_mk_mod , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{arg1 rem arg2}.

       The arguments must have int type. -}
#ccall Z3_mk_rem , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an AST node representing \ccode{arg1 ^ arg2}.

       The arguments must have int or real type. -}
#ccall Z3_mk_power , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create less than.

        The nodes \c t1 and \c t2 must have the same sort, and must be int or real. -}
#ccall Z3_mk_lt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create less than or equal to.

        The nodes \c t1 and \c t2 must have the same sort, and must be int or real. -}
#ccall Z3_mk_le , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create greater than.

        The nodes \c t1 and \c t2 must have the same sort, and must be int or real. -}
#ccall Z3_mk_gt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create greater than or equal to.

        The nodes \c t1 and \c t2 must have the same sort, and must be int or real. -}
#ccall Z3_mk_ge , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Coerce an integer to a real.

        There is also a converse operation exposed.
        It follows the semantics prescribed by the SMT-LIB standard.

        You can take the floor of a real by
        creating an auxiliary integer constant \c k and
        and asserting \ccode{mk_int2real(k) <= t1 < mk_int2real(k)+1}.

        The node \c t1 must have sort integer.

        \sa Z3_mk_real2int
        \sa Z3_mk_is_int -}
#ccall Z3_mk_int2real , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Coerce a real to an integer.

        The semantics of this function follows the SMT-LIB standard
        for the function to_int

        \sa Z3_mk_int2real
        \sa Z3_mk_is_int -}
#ccall Z3_mk_real2int , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Check if a real number is an integer.

        \sa Z3_mk_int2real
        \sa Z3_mk_real2int -}
#ccall Z3_mk_is_int , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Bitwise negation.

       The node \c t1 must have a bit-vector sort. -}
#ccall Z3_mk_bvnot , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Take conjunction of bits in vector, return vector of length 1.

       The node \c t1 must have a bit-vector sort. -}
#ccall Z3_mk_bvredand , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Take disjunction of bits in vector, return vector of length 1.

       The node \c t1 must have a bit-vector sort. -}
#ccall Z3_mk_bvredor , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Bitwise and.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvand , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Bitwise or.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvor , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Bitwise exclusive-or.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvxor , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Bitwise nand.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvnand , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Bitwise nor.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvnor , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Bitwise xnor.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvxnor , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Standard two's complement unary minus.

       The node \c t1 must have bit-vector sort. -}
#ccall Z3_mk_bvneg , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Standard two's complement addition.

        The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvadd , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Standard two's complement subtraction.

        The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvsub , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Standard two's complement multiplication.

        The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvmul , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Unsigned division.

        It is defined as the \c floor of \ccode{t1/t2} if \c t2 is
        different from zero. If \ccode{t2} is zero, then the result
        is undefined.

        The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvudiv , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Two's complement signed division.

        It is defined in the following way:

        - The \c floor of \ccode{t1/t2} if \c t2 is different from zero, and \ccode{t1*t2 >= 0}.

        - The \c ceiling of \ccode{t1/t2} if \c t2 is different from zero, and \ccode{t1*t2 < 0}.

        If \ccode{t2} is zero, then the result is undefined.

        The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvsdiv , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Unsigned remainder.

       It is defined as \ccode{t1 - (t1 /u t2) * t2}, where \ccode{/u} represents unsigned division.

       If \ccode{t2} is zero, then the result is undefined.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvurem , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Two's complement signed remainder (sign follows dividend).

       It is defined as \ccode{t1 - (t1 /s t2) * t2}, where \ccode{/s} represents signed division.
       The most significant bit (sign) of the result is equal to the most significant bit of \c t1.

       If \ccode{t2} is zero, then the result is undefined.

       The nodes \c t1 and \c t2 must have the same bit-vector sort.

       \sa Z3_mk_bvsmod -}
#ccall Z3_mk_bvsrem , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Two's complement signed remainder (sign follows divisor).

       If \ccode{t2} is zero, then the result is undefined.

       The nodes \c t1 and \c t2 must have the same bit-vector sort.

       \sa Z3_mk_bvsrem -}
#ccall Z3_mk_bvsmod , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Unsigned less than.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvult , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Two's complement signed less than.

       It abbreviates:
       \code
        (or (and (= (extract[|m-1|:|m-1|] t1) bit1)
                (= (extract[|m-1|:|m-1|] t2) bit0))
            (and (= (extract[|m-1|:|m-1|] t1) (extract[|m-1|:|m-1|] t2))
                (bvult t1 t2)))
       \endcode

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvslt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Unsigned less than or equal to.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvule , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Two's complement signed less than or equal to.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvsle , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Unsigned greater than or equal to.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvuge , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Two's complement signed greater than or equal to.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvsge , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Unsigned greater than.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvugt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Two's complement signed greater than.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvsgt , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Concatenate the given bit-vectors.

       The nodes \c t1 and \c t2 must have (possibly different) bit-vector sorts

       The result is a bit-vector of size \ccode{n1+n2}, where \c n1 (\c n2) is the size
       of \c t1 (\c t2). -}
#ccall Z3_mk_concat , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Extract the bits \c high down to \c low from a bit-vector of
       size \c m to yield a new bit-vector of size \c n, where \ccode{n = high - low + 1}.

       The node \c t1 must have a bit-vector sort. -}
#ccall Z3_mk_extract , <Z3_context> -> CUInt -> CUInt -> <Z3_ast> -> IO <Z3_ast>

{- | Sign-extend of the given bit-vector to the (signed) equivalent bit-vector of
       size \ccode{m+i}, where \c m is the size of the given
       bit-vector.

       The node \c t1 must have a bit-vector sort. -}
#ccall Z3_mk_sign_ext , <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>

{- | Extend the given bit-vector with zeros to the (unsigned) equivalent
       bit-vector of size \ccode{m+i}, where \c m is the size of the
       given bit-vector.

       The node \c t1 must have a bit-vector sort. -}
#ccall Z3_mk_zero_ext , <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>

{- | Repeat the given bit-vector up length \ccode{i}.

       The node \c t1 must have a bit-vector sort. -}
#ccall Z3_mk_repeat , <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>

{- | Shift left.

       It is equivalent to multiplication by \ccode{2^x} where \c x is the value of the
       third argument.

       NB. The semantics of shift operations varies between environments. This
       definition does not necessarily capture directly the semantics of the
       programming language or assembly architecture you are modeling.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvshl , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Logical shift right.

       It is equivalent to unsigned division by \ccode{2^x} where \c x is the
       value of the third argument.

       NB. The semantics of shift operations varies between environments. This
       definition does not necessarily capture directly the semantics of the
       programming language or assembly architecture you are modeling.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvlshr , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Arithmetic shift right.

       It is like logical shift right except that the most significant
       bits of the result always copy the most significant bit of the
       second argument.

       The semantics of shift operations varies between environments. This
       definition does not necessarily capture directly the semantics of the
       programming language or assembly architecture you are modeling.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvashr , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Rotate bits of \c t1 to the left \c i times.

       The node \c t1 must have a bit-vector sort. -}
#ccall Z3_mk_rotate_left , <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>

{- | Rotate bits of \c t1 to the right \c i times.

       The node \c t1 must have a bit-vector sort. -}
#ccall Z3_mk_rotate_right , <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>

{- | Rotate bits of \c t1 to the left \c t2 times.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_ext_rotate_left , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Rotate bits of \c t1 to the right \c t2 times.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_ext_rotate_right , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an \c n bit bit-vector from the integer argument \c t1.

       NB. This function is essentially treated as uninterpreted.
       So you cannot expect Z3 to precisely reflect the semantics of this function
       when solving constraints with this function.

       The node \c t1 must have integer sort. -}
#ccall Z3_mk_int2bv , <Z3_context> -> CUInt -> <Z3_ast> -> IO <Z3_ast>

{- | Create an integer from the bit-vector argument \c t1.
       If \c is_signed is false, then the bit-vector \c t1 is treated as unsigned.
       So the result is non-negative
       and in the range \ccode{[0..2^N-1]}, where N are the number of bits in \c t1.
       If \c is_signed is true, \c t1 is treated as a signed bit-vector.

       This function is essentially treated as uninterpreted.
       So you cannot expect Z3 to precisely reflect the semantics of this function
       when solving constraints with this function.

       The node \c t1 must have a bit-vector sort. -}
#ccall Z3_mk_bv2int , <Z3_context> -> <Z3_ast> -> CInt -> IO <Z3_ast>

{- | Create a predicate that checks that the bit-wise addition
       of \c t1 and \c t2 does not overflow.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvadd_no_overflow , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> CInt -> IO <Z3_ast>

{- | Create a predicate that checks that the bit-wise signed addition
       of \c t1 and \c t2 does not underflow.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvadd_no_underflow , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a predicate that checks that the bit-wise signed subtraction
       of \c t1 and \c t2 does not overflow.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvsub_no_overflow , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a predicate that checks that the bit-wise subtraction
       of \c t1 and \c t2 does not underflow.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvsub_no_underflow , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> CInt -> IO <Z3_ast>

{- | Create a predicate that checks that the bit-wise signed division
       of \c t1 and \c t2 does not overflow.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvsdiv_no_overflow , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Check that bit-wise negation does not overflow when
       \c t1 is interpreted as a signed bit-vector.

       The node \c t1 must have bit-vector sort. -}
#ccall Z3_mk_bvneg_no_overflow , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a predicate that checks that the bit-wise multiplication
       of \c t1 and \c t2 does not overflow.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvmul_no_overflow , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> CInt -> IO <Z3_ast>

{- | Create a predicate that checks that the bit-wise signed multiplication
       of \c t1 and \c t2 does not underflow.

       The nodes \c t1 and \c t2 must have the same bit-vector sort. -}
#ccall Z3_mk_bvmul_no_underflow , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Array read.
       The argument \c a is the array and \c i is the index of the array that gets read.

       The node \c a must have an array sort \ccode{[domain -> range]},
       and \c i must have the sort \c domain.
       The sort of the result is \c range.

       \sa Z3_mk_array_sort
       \sa Z3_mk_store -}
#ccall Z3_mk_select , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Array update.

       The node \c a must have an array sort \ccode{[domain -> range]}, \c i must have sort \c domain,
       \c v must have sort range. The sort of the result is \ccode{[domain -> range]}.
       The semantics of this function is given by the theory of arrays described in the SMT-LIB
       standard. See http://smtlib.org for more details.
       The result of this function is an array that is equal to \c a (with respect to \c select)
       on all indices except for \c i, where it maps to \c v (and the \c select of \c a with
       respect to \c i may be a different value).

       \sa Z3_mk_array_sort
       \sa Z3_mk_select -}
#ccall Z3_mk_store , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create the constant array.

        The resulting term is an array, such that a \c select on an arbitrary index
        produces the value \c v.

        \param c logical context.
        \param domain domain sort for the array.
        \param v value that the array maps to. -}
#ccall Z3_mk_const_array , <Z3_context> -> <Z3_sort> -> <Z3_ast> -> IO <Z3_ast>

{- | Map f on the argument arrays.

       The \c n nodes \c args must be of array sorts \ccode{[domain_i -> range_i]}.
       The function declaration \c f must have type \ccode{ range_1 .. range_n -> range}.
       \c v must have sort range. The sort of the result is \ccode{[domain_i -> range]}.

       \sa Z3_mk_array_sort
       \sa Z3_mk_store
       \sa Z3_mk_select -}
#ccall Z3_mk_map , <Z3_context> -> <Z3_func_decl> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Access the array default value.
        Produces the default range value, for arrays that can be represented as
        finite maps with a default range value.

        \param c logical context.
        \param array array value whose default range value is accessed. -}
#ccall Z3_mk_array_default , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Create Set type. -}
#ccall Z3_mk_set_sort , <Z3_context> -> <Z3_sort> -> IO <Z3_sort>

{- | Create the empty set. -}
#ccall Z3_mk_empty_set , <Z3_context> -> <Z3_sort> -> IO <Z3_ast>

{- | Create the full set. -}
#ccall Z3_mk_full_set , <Z3_context> -> <Z3_sort> -> IO <Z3_ast>

{- | Add an element to a set.

       The first argument must be a set, the second an element. -}
#ccall Z3_mk_set_add , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Remove an element to a set.

       The first argument must be a set, the second an element. -}
#ccall Z3_mk_set_del , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Take the union of a list of sets. -}
#ccall Z3_mk_set_union , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Take the intersection of a list of sets. -}
#ccall Z3_mk_set_intersect , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Take the set difference between two sets. -}
#ccall Z3_mk_set_difference , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Take the complement of a set. -}
#ccall Z3_mk_set_complement , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Check for set membership.

       The first argument should be an element type of the set. -}
#ccall Z3_mk_set_member , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Check for subsetness of sets. -}
#ccall Z3_mk_set_subset , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create array extensionality index given two arrays with the same sort.
              The meaning is given by the axiom:
              (=> (= (select A (array-ext A B)) (select B (array-ext A B))) (= A B)) -}
#ccall Z3_mk_array_ext , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a numeral of a given sort.

       \param c logical context.
       \param numeral A string representing the numeral value in decimal notation. The string may be of the form \code{[num]*[.[num]*][E[+|-][num]+]}.
                      If the given sort is a real, then the numeral can be a rational, that is, a string of the form \ccode{[num]* / [num]*}.                      
       \param ty The sort of the numeral. In the current implementation, the given sort can be an int, real, finite-domain, or bit-vectors of arbitrary size.

       \sa Z3_mk_int
       \sa Z3_mk_unsigned_int -}
#ccall Z3_mk_numeral , <Z3_context> -> <Z3_string> -> <Z3_sort> -> IO <Z3_ast>

{- | Create a real from a fraction.

       \param c logical context.
       \param num numerator of rational.
       \param den denomerator of rational.

       \pre den != 0

       \sa Z3_mk_numeral
       \sa Z3_mk_int
       \sa Z3_mk_unsigned_int -}
#ccall Z3_mk_real , <Z3_context> -> CInt -> CInt -> IO <Z3_ast>

{- | Create a numeral of an int, bit-vector, or finite-domain sort.

       This function can be use to create numerals that fit in a machine integer.
       It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string.

       \sa Z3_mk_numeral -}
#ccall Z3_mk_int , <Z3_context> -> CInt -> <Z3_sort> -> IO <Z3_ast>

{- | Create a numeral of a int, bit-vector, or finite-domain sort.

       This function can be use to create numerals that fit in a machine unsinged integer.
       It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string.

       \sa Z3_mk_numeral -}
#ccall Z3_mk_unsigned_int , <Z3_context> -> CUInt -> <Z3_sort> -> IO <Z3_ast>

{- | Create a numeral of a int, bit-vector, or finite-domain sort.

       This function can be use to create numerals that fit in a machine __int64 integer.
       It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string.

       \sa Z3_mk_numeral -}
#ccall Z3_mk_int64 , <Z3_context> -> CLong -> <Z3_sort> -> IO <Z3_ast>

{- | Create a numeral of a int, bit-vector, or finite-domain sort.

       This function can be use to create numerals that fit in a machine unsigned __int64 integer.
       It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string.

       \sa Z3_mk_numeral -}
#ccall Z3_mk_unsigned_int64 , <Z3_context> -> CULong -> <Z3_sort> -> IO <Z3_ast>

{- | Create a sequence sort out of the sort for the elements. -}
#ccall Z3_mk_seq_sort , <Z3_context> -> <Z3_sort> -> IO <Z3_sort>

{- | Check if \c s is a sequence sort. -}
#ccall Z3_is_seq_sort , <Z3_context> -> <Z3_sort> -> IO CInt

{- | Create a regular expression sort out of a sequence sort. -}
#ccall Z3_mk_re_sort , <Z3_context> -> <Z3_sort> -> IO <Z3_sort>

{- | Check if \c s is a regular expression sort. -}
#ccall Z3_is_re_sort , <Z3_context> -> <Z3_sort> -> IO CInt

{- | Create a sort for 8 bit strings.

       This function creates a sort for ASCII strings.
       Each character is 8 bits. -}
#ccall Z3_mk_string_sort , <Z3_context> -> IO <Z3_sort>

{- | Check if \c s is a string sort. -}
#ccall Z3_is_string_sort , <Z3_context> -> <Z3_sort> -> IO CInt

{- | Create a string constant out of the string that is passed in -}
#ccall Z3_mk_string , <Z3_context> -> <Z3_string> -> IO <Z3_ast>

{- | Determine if \c s is a string constant. -}
#ccall Z3_is_string , <Z3_context> -> <Z3_ast> -> IO CInt

{- | Retrieve the string constant stored in \c s.

       \pre  Z3_is_string(c, s) -}
#ccall Z3_get_string , <Z3_context> -> <Z3_ast> -> IO <Z3_string>

{- | Create an empty sequence of the sequence sort \c seq.

       \pre s is a sequence sort. -}
#ccall Z3_mk_seq_empty , <Z3_context> -> <Z3_sort> -> IO <Z3_ast>

{- | Create a unit sequence of \c a. -}
#ccall Z3_mk_seq_unit , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Concatenate sequences.

       \pre n > 0 -}
#ccall Z3_mk_seq_concat , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Check if \c prefix is a prefix of \c s.

       \pre prefix and s are the same sequence sorts. -}
#ccall Z3_mk_seq_prefix , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Check if \c suffix is a suffix of \c s.

       \pre \c suffix and \c s are the same sequence sorts. -}
#ccall Z3_mk_seq_suffix , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Check if \c container contains \c containee.

       \pre \c container and \c containee are the same sequence sorts. -}
#ccall Z3_mk_seq_contains , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Extract subsequence starting at \c offset of \c length. -}
#ccall Z3_mk_seq_extract , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Replace the first occurrence of \c src with \c dst in \c s. -}
#ccall Z3_mk_seq_replace , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Retrieve from \s the unit sequence positioned at position \c index. -}
#ccall Z3_mk_seq_at , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Return the length of the sequence \c s. -}
#ccall Z3_mk_seq_length , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Return index of first occurrence of \c substr in \c s starting from offset \c offset.
       If \c s does not contain \c substr, then the value is -1, if \c offset is the length of \c s, then the value is -1 as well.
       The function is under-specified if \c offset is negative or larger than the length of \c s. -}
#ccall Z3_mk_seq_index , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a regular expression that accepts the sequence \c seq. -}
#ccall Z3_mk_seq_to_re , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Check if \c seq is in the language generated by the regular expression \c re. -}
#ccall Z3_mk_seq_in_re , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Create the regular language \c re+. -}
#ccall Z3_mk_re_plus , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Create the regular language \c re*. -}
#ccall Z3_mk_re_star , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Create the regular language \c [re]. -}
#ccall Z3_mk_re_option , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Create the union of the regular languages.

       \pre n > 0 -}
#ccall Z3_mk_re_union , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create the concatenation of the regular languages.

       \pre n > 0 -}
#ccall Z3_mk_re_concat , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Create a pattern for quantifier instantiation.

       Z3 uses pattern matching to instantiate quantifiers. If a
       pattern is not provided for a quantifier, then Z3 will
       automatically compute a set of patterns for it. However, for
       optimal performance, the user should provide the patterns.

       Patterns comprise a list of terms. The list should be
       non-empty.  If the list comprises of more than one term, it is
       a called a multi-pattern.

       In general, one can pass in a list of (multi-)patterns in the
       quantifier constructor.

       \sa Z3_mk_forall
       \sa Z3_mk_exists -}
#ccall Z3_mk_pattern , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_pattern>

{- | Create a bound variable.

       Bound variables are indexed by de-Bruijn indices. It is perhaps easiest to explain
       the meaning of de-Bruijn indices by indicating the compilation process from
       non-de-Bruijn formulas to de-Bruijn format.

       \verbatim
       abs(forall (x1) phi) = forall (x1) abs1(phi, x1, 0)
       abs(forall (x1, x2) phi) = abs(forall (x1) abs(forall (x2) phi))
       abs1(x, x, n) = b_n
       abs1(y, x, n) = y
       abs1(f(t1,...,tn), x, n) = f(abs1(t1,x,n), ..., abs1(tn,x,n))
       abs1(forall (x1) phi, x, n) = forall (x1) (abs1(phi, x, n+1))
       \endverbatim

       The last line is significant: the index of a bound variable is different depending
       on the scope in which it appears. The deeper x appears, the higher is its
       index.

       \param c logical context
       \param index de-Bruijn index
       \param ty sort of the bound variable

       \sa Z3_mk_forall
       \sa Z3_mk_exists -}
#ccall Z3_mk_bound , <Z3_context> -> CUInt -> <Z3_sort> -> IO <Z3_ast>

{- | Create a forall formula. It takes an expression \c body that contains bound variables
       of the same sorts as the sorts listed in the array \c sorts. The bound variables are de-Bruijn indices created
       using #Z3_mk_bound. The array \c decl_names contains the names that the quantified formula uses for the
       bound variables. Z3 applies the convention that the last element in the \c decl_names and \c sorts array
       refers to the variable with index 0, the second to last element of \c decl_names and \c sorts refers
       to the variable with index 1, etc.

       \param c logical context.
       \param weight quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
       \param num_patterns number of patterns.
       \param patterns array containing the patterns created using #Z3_mk_pattern.
       \param num_decls number of variables to be bound.
       \param sorts the sorts of the bound variables.
       \param decl_names names of the bound variables
       \param body the body of the quantifier.

       \sa Z3_mk_pattern
       \sa Z3_mk_bound
       \sa Z3_mk_exists -}
#ccall Z3_mk_forall , <Z3_context> -> CUInt -> CUInt -> Ptr <Z3_pattern> -> CUInt -> Ptr <Z3_sort> -> Ptr <Z3_symbol> -> <Z3_ast> -> IO <Z3_ast>

{- | Create an exists formula. Similar to #Z3_mk_forall.

       \sa Z3_mk_pattern
       \sa Z3_mk_bound
       \sa Z3_mk_forall
       \sa Z3_mk_quantifier -}
#ccall Z3_mk_exists , <Z3_context> -> CUInt -> CUInt -> Ptr <Z3_pattern> -> CUInt -> Ptr <Z3_sort> -> Ptr <Z3_symbol> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a quantifier - universal or existential, with pattern hints.
       See the documentation for #Z3_mk_forall for an explanation of the parameters.

       \param c logical context.
       \param is_forall flag to indicate if this is a universal or existential quantifier.
       \param weight quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
       \param num_patterns number of patterns.
       \param patterns array containing the patterns created using #Z3_mk_pattern.
       \param num_decls number of variables to be bound.
       \param sorts array of sorts of the bound variables.
       \param decl_names names of the bound variables.
       \param body the body of the quantifier.

       \sa Z3_mk_pattern
       \sa Z3_mk_bound
       \sa Z3_mk_forall
       \sa Z3_mk_exists -}
#ccall Z3_mk_quantifier , <Z3_context> -> CInt -> CUInt -> CUInt -> Ptr <Z3_pattern> -> CUInt -> Ptr <Z3_sort> -> Ptr <Z3_symbol> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a quantifier - universal or existential, with pattern hints, no patterns, and attributes

       \param c logical context.
       \param is_forall flag to indicate if this is a universal or existential quantifier.
       \param quantifier_id identifier to identify quantifier
       \param skolem_id identifier to identify skolem constants introduced by quantifier.
       \param weight quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
       \param num_patterns number of patterns.
       \param patterns array containing the patterns created using #Z3_mk_pattern.
       \param num_no_patterns number of no_patterns.
       \param no_patterns array containing subexpressions to be excluded from inferred patterns.
       \param num_decls number of variables to be bound.
       \param sorts array of sorts of the bound variables.
       \param decl_names names of the bound variables.
       \param body the body of the quantifier.

       \sa Z3_mk_pattern
       \sa Z3_mk_bound
       \sa Z3_mk_forall
       \sa Z3_mk_exists -}
#ccall Z3_mk_quantifier_ex , <Z3_context> -> CInt -> CUInt -> <Z3_symbol> -> <Z3_symbol> -> CUInt -> Ptr <Z3_pattern> -> CUInt -> Ptr <Z3_ast> -> CUInt -> Ptr <Z3_sort> -> Ptr <Z3_symbol> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a universal quantifier using a list of constants that
       will form the set of bound variables.

       \param c logical context.
       \param weight quantifiers are associated with weights indicating the importance of using
              the quantifier during instantiation. By default, pass the weight 0.
       \param num_bound number of constants to be abstracted into bound variables.
       \param bound array of constants to be abstracted into bound variables.
       \param num_patterns number of patterns.
       \param patterns array containing the patterns created using #Z3_mk_pattern.
       \param body the body of the quantifier.

       \sa Z3_mk_pattern
       \sa Z3_mk_exists_const -}
#ccall Z3_mk_forall_const , <Z3_context> -> CUInt -> CUInt -> Ptr <Z3_app> -> CUInt -> Ptr <Z3_pattern> -> <Z3_ast> -> IO <Z3_ast>

{- | Similar to #Z3_mk_forall_const.

       \brief Create an existential quantifier using a list of constants that
       will form the set of bound variables.

       \param c logical context.
       \param weight quantifiers are associated with weights indicating the importance of using
              the quantifier during instantiation. By default, pass the weight 0.
       \param num_bound number of constants to be abstracted into bound variables.
       \param bound array of constants to be abstracted into bound variables.
       \param num_patterns number of patterns.
       \param patterns array containing the patterns created using #Z3_mk_pattern.
       \param body the body of the quantifier.

       \sa Z3_mk_pattern
       \sa Z3_mk_forall_const -}
#ccall Z3_mk_exists_const , <Z3_context> -> CUInt -> CUInt -> Ptr <Z3_app> -> CUInt -> Ptr <Z3_pattern> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a universal or existential quantifier using a list of
       constants that will form the set of bound variables. -}
#ccall Z3_mk_quantifier_const , <Z3_context> -> CInt -> CUInt -> CUInt -> Ptr <Z3_app> -> CUInt -> Ptr <Z3_pattern> -> <Z3_ast> -> IO <Z3_ast>

{- | Create a universal or existential quantifier using a list of
       constants that will form the set of bound variables. -}
#ccall Z3_mk_quantifier_const_ex , <Z3_context> -> CInt -> CUInt -> <Z3_symbol> -> <Z3_symbol> -> CUInt -> Ptr <Z3_app> -> CUInt -> Ptr <Z3_pattern> -> CUInt -> Ptr <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Return \c Z3_INT_SYMBOL if the symbol was constructed
       using #Z3_mk_int_symbol, and \c Z3_STRING_SYMBOL if the symbol
       was constructed using #Z3_mk_string_symbol. -}
#ccall Z3_get_symbol_kind , <Z3_context> -> <Z3_symbol> -> IO <Z3_symbol_kind>

{- | Return the symbol int value.

       \pre Z3_get_symbol_kind(s) == Z3_INT_SYMBOL

       \sa Z3_mk_int_symbol -}
#ccall Z3_get_symbol_int , <Z3_context> -> <Z3_symbol> -> IO CInt

{- | Return the symbol name.

       \pre Z3_get_symbol_string(s) == Z3_STRING_SYMBOL

       \warning The returned buffer is statically allocated by Z3. It will
       be automatically deallocated when #Z3_del_context is invoked.
       So, the buffer is invalidated in the next call to \c Z3_get_symbol_string.

       \sa Z3_mk_string_symbol -}
#ccall Z3_get_symbol_string , <Z3_context> -> <Z3_symbol> -> IO <Z3_string>

{- | Return the sort name as a symbol. -}
#ccall Z3_get_sort_name , <Z3_context> -> <Z3_sort> -> IO <Z3_symbol>

{- | Return a unique identifier for \c s. -}
#ccall Z3_get_sort_id , <Z3_context> -> <Z3_sort> -> IO ()

{- | Convert a \c Z3_sort into \c Z3_ast. This is just type casting. -}
#ccall Z3_sort_to_ast , <Z3_context> -> <Z3_sort> -> IO <Z3_ast>

{- | compare sorts. -}
#ccall Z3_is_eq_sort , <Z3_context> -> <Z3_sort> -> <Z3_sort> -> IO CInt

{- | Return the sort kind (e.g., array, tuple, int, bool, etc).

       \sa Z3_sort_kind -}
#ccall Z3_get_sort_kind , <Z3_context> -> <Z3_sort> -> IO <Z3_sort_kind>

{- | Return the size of the given bit-vector sort.

       \pre Z3_get_sort_kind(c, t) == Z3_BV_SORT

       \sa Z3_mk_bv_sort
       \sa Z3_get_sort_kind -}
#ccall Z3_get_bv_sort_size , <Z3_context> -> <Z3_sort> -> IO ()

{- | Store the size of the sort in \c r. Return Z3_FALSE if the call failed.
        That is, Z3_get_sort_kind(s) == Z3_FINITE_DOMAIN_SORT -}
#ccall Z3_get_finite_domain_sort_size , <Z3_context> -> <Z3_sort> -> Ptr CULong -> IO CInt

{- | Return the domain of the given array sort.

       \pre Z3_get_sort_kind(c, t) == Z3_ARRAY_SORT

       \sa Z3_mk_array_sort
       \sa Z3_get_sort_kind -}
#ccall Z3_get_array_sort_domain , <Z3_context> -> <Z3_sort> -> IO <Z3_sort>

{- | Return the range of the given array sort.

       \pre Z3_get_sort_kind(c, t) == Z3_ARRAY_SORT

       \sa Z3_mk_array_sort
       \sa Z3_get_sort_kind -}
#ccall Z3_get_array_sort_range , <Z3_context> -> <Z3_sort> -> IO <Z3_sort>

{- | Return the constructor declaration of the given tuple
       sort.

       \pre Z3_get_sort_kind(c, t) == Z3_DATATYPE_SORT

       \sa Z3_mk_tuple_sort
       \sa Z3_get_sort_kind -}
#ccall Z3_get_tuple_sort_mk_decl , <Z3_context> -> <Z3_sort> -> IO <Z3_func_decl>

{- | Return the number of fields of the given tuple sort.

       \pre Z3_get_sort_kind(c, t) == Z3_DATATYPE_SORT

       \sa Z3_mk_tuple_sort
       \sa Z3_get_sort_kind -}
#ccall Z3_get_tuple_sort_num_fields , <Z3_context> -> <Z3_sort> -> IO ()

{- | Return the i-th field declaration (i.e., projection function declaration)
       of the given tuple sort.

       \pre Z3_get_sort_kind(t) == Z3_DATATYPE_SORT
       \pre i < Z3_get_tuple_sort_num_fields(c, t)

       \sa Z3_mk_tuple_sort
       \sa Z3_get_sort_kind -}
#ccall Z3_get_tuple_sort_field_decl , <Z3_context> -> <Z3_sort> -> CUInt -> IO <Z3_func_decl>

{- | Return number of constructors for datatype.

        \pre Z3_get_sort_kind(t) == Z3_DATATYPE_SORT

        \sa Z3_get_datatype_sort_constructor
        \sa Z3_get_datatype_sort_recognizer
        \sa Z3_get_datatype_sort_constructor_accessor -}
#ccall Z3_get_datatype_sort_num_constructors , <Z3_context> -> <Z3_sort> -> IO ()

{- | Return idx'th constructor.

        \pre Z3_get_sort_kind(t) == Z3_DATATYPE_SORT
        \pre idx < Z3_get_datatype_sort_num_constructors(c, t)

        \sa Z3_get_datatype_sort_num_constructors
        \sa Z3_get_datatype_sort_recognizer
        \sa Z3_get_datatype_sort_constructor_accessor -}
#ccall Z3_get_datatype_sort_constructor , <Z3_context> -> <Z3_sort> -> CUInt -> IO <Z3_func_decl>

{- | Return idx'th recognizer.

        \pre Z3_get_sort_kind(t) == Z3_DATATYPE_SORT
        \pre idx < Z3_get_datatype_sort_num_constructors(c, t)

        \sa Z3_get_datatype_sort_num_constructors
        \sa Z3_get_datatype_sort_constructor
        \sa Z3_get_datatype_sort_constructor_accessor -}
#ccall Z3_get_datatype_sort_recognizer , <Z3_context> -> <Z3_sort> -> CUInt -> IO <Z3_func_decl>

{- | Return idx_a'th accessor for the idx_c'th constructor.

        \pre Z3_get_sort_kind(t) == Z3_DATATYPE_SORT
        \pre idx_c < Z3_get_datatype_sort_num_constructors(c, t)
        \pre idx_a < Z3_get_domain_size(c, Z3_get_datatype_sort_constructor(c, idx_c))

        \sa Z3_get_datatype_sort_num_constructors
        \sa Z3_get_datatype_sort_constructor
        \sa Z3_get_datatype_sort_recognizer -}
#ccall Z3_get_datatype_sort_constructor_accessor , <Z3_context> -> <Z3_sort> -> CUInt -> CUInt -> IO <Z3_func_decl>

{- | Update record field with a value.

       This corresponds to the 'with' construct in OCaml.
       It has the effect of updating a record field with a given value.
       The remaining fields are left unchanged. It is the record
       equivalent of an array store (see \sa Z3_mk_store).
       If the datatype has more than one constructor, then the update function
       behaves as identity if there is a miss-match between the accessor and
       constructor. For example ((_ update-field car) nil 1) is nil,
       while ((_ update-field car) (cons 2 nil) 1) is (cons 1 nil).


       \pre Z3_get_sort_kind(Z3_get_sort(c, t)) == Z3_get_domain(c, field_access, 1) == Z3_DATATYPE_SORT
       \pre Z3_get_sort(c, value) == Z3_get_range(c, field_access) -}
#ccall Z3_datatype_update_field , <Z3_context> -> <Z3_func_decl> -> <Z3_ast> -> <Z3_ast> -> IO <Z3_ast>

{- | Return arity of relation.

        \pre Z3_get_sort_kind(s) == Z3_RELATION_SORT

        \sa Z3_get_relation_column -}
#ccall Z3_get_relation_arity , <Z3_context> -> <Z3_sort> -> IO ()

{- | Return sort at i'th column of relation sort.

        \pre Z3_get_sort_kind(c, s) == Z3_RELATION_SORT
        \pre col < Z3_get_relation_arity(c, s)

        \sa Z3_get_relation_arity -}
#ccall Z3_get_relation_column , <Z3_context> -> <Z3_sort> -> CUInt -> IO <Z3_sort>

{- | Pseudo-Boolean relations.

       Encode p1 + p2 + ... + pn <= k -}
#ccall Z3_mk_atmost , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> CUInt -> IO <Z3_ast>

{- | Pseudo-Boolean relations.

       Encode k1*p1 + k2*p2 + ... + kn*pn <= k -}
#ccall Z3_mk_pble , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> Ptr CInt -> CInt -> IO <Z3_ast>

{- | Pseudo-Boolean relations.

       Encode k1*p1 + k2*p2 + ... + kn*pn = k -}
#ccall Z3_mk_pbeq , <Z3_context> -> CUInt -> Ptr <Z3_ast> -> Ptr CInt -> CInt -> IO <Z3_ast>

{- | Convert a \c Z3_func_decl into \c Z3_ast. This is just type casting. -}
#ccall Z3_func_decl_to_ast , <Z3_context> -> <Z3_func_decl> -> IO <Z3_ast>

{- | Compare terms. -}
#ccall Z3_is_eq_func_decl , <Z3_context> -> <Z3_func_decl> -> <Z3_func_decl> -> IO CInt

{- | Return a unique identifier for \c f. -}
#ccall Z3_get_func_decl_id , <Z3_context> -> <Z3_func_decl> -> IO ()

{- | Return the constant declaration name as a symbol. -}
#ccall Z3_get_decl_name , <Z3_context> -> <Z3_func_decl> -> IO <Z3_symbol>

{- | Return declaration kind corresponding to declaration. -}
#ccall Z3_get_decl_kind , <Z3_context> -> <Z3_func_decl> -> IO <Z3_decl_kind>

{- | Return the number of parameters of the given declaration.

       \sa Z3_get_arity -}
#ccall Z3_get_domain_size , <Z3_context> -> <Z3_func_decl> -> IO ()

{- | Alias for \c Z3_get_domain_size.

       \sa Z3_get_domain_size -}
#ccall Z3_get_arity , <Z3_context> -> <Z3_func_decl> -> IO ()

{- | Return the sort of the i-th parameter of the given function declaration.

       \pre i < Z3_get_domain_size(d)

       \sa Z3_get_domain_size -}
#ccall Z3_get_domain , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_sort>

{- | Return the range of the given declaration.

       If \c d is a constant (i.e., has zero arguments), then this
       function returns the sort of the constant. -}
#ccall Z3_get_range , <Z3_context> -> <Z3_func_decl> -> IO <Z3_sort>

{- | Return the number of parameters associated with a declaration. -}
#ccall Z3_get_decl_num_parameters , <Z3_context> -> <Z3_func_decl> -> IO ()

{- | Return the parameter type associated with a declaration.

       \param c the context
       \param d the function declaration
       \param idx is the index of the named parameter it should be between 0 and the number of parameters. -}
#ccall Z3_get_decl_parameter_kind , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_parameter_kind>

{- | Return the integer value associated with an integer parameter.

       \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_INT -}
#ccall Z3_get_decl_int_parameter , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO CInt

{- | Return the double value associated with an double parameter.

       \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_DOUBLE -}
#ccall Z3_get_decl_double_parameter , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO CDouble

{- | Return the double value associated with an double parameter.

       \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_SYMBOL -}
#ccall Z3_get_decl_symbol_parameter , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_symbol>

{- | Return the sort value associated with a sort parameter.

       \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_SORT -}
#ccall Z3_get_decl_sort_parameter , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_sort>

{- | Return the expresson value associated with an expression parameter.

       \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_AST -}
#ccall Z3_get_decl_ast_parameter , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_ast>

{- | Return the expresson value associated with an expression parameter.

       \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_FUNC_DECL -}
#ccall Z3_get_decl_func_decl_parameter , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_func_decl>

{- | Return the rational value, as a string, associated with a rational parameter.

       \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_RATIONAL -}
#ccall Z3_get_decl_rational_parameter , <Z3_context> -> <Z3_func_decl> -> CUInt -> IO <Z3_string>

{- | Convert a \c Z3_app into \c Z3_ast. This is just type casting. -}
#ccall Z3_app_to_ast , <Z3_context> -> <Z3_app> -> IO <Z3_ast>

{- | Return the declaration of a constant or function application. -}
#ccall Z3_get_app_decl , <Z3_context> -> <Z3_app> -> IO <Z3_func_decl>

{- | Return the number of argument of an application. If \c t
       is an constant, then the number of arguments is 0. -}
#ccall Z3_get_app_num_args , <Z3_context> -> <Z3_app> -> IO ()

{- | Return the i-th argument of the given application.

       \pre i < Z3_get_num_args(c, a) -}
#ccall Z3_get_app_arg , <Z3_context> -> <Z3_app> -> CUInt -> IO <Z3_ast>

{- | Compare terms. -}
#ccall Z3_is_eq_ast , <Z3_context> -> <Z3_ast> -> <Z3_ast> -> IO CInt

{- | Return a unique identifier for \c t.
        The identifier is unique up to structural equality. Thus, two ast nodes
        created by the same context and having the same children and same function symbols
        have the same identifiers. Ast nodes created in the same context, but having
        different children or different functions have different identifiers.
        Variables and quantifiers are also assigned different identifiers according to
        their structure. -}
#ccall Z3_get_ast_id , <Z3_context> -> <Z3_ast> -> IO ()

{- | Return a hash code for the given AST.
       The hash code is structural. You can use Z3_get_ast_id interchangably with
       this function. -}
#ccall Z3_get_ast_hash , <Z3_context> -> <Z3_ast> -> IO ()

{- | Return the sort of an AST node.

       The AST node must be a constant, application, numeral, bound variable, or quantifier. -}
#ccall Z3_get_sort , <Z3_context> -> <Z3_ast> -> IO <Z3_sort>

{- | Return true if the given expression \c t is well sorted. -}
#ccall Z3_is_well_sorted , <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return Z3_L_TRUE if \c a is true, Z3_L_FALSE if it is false, and Z3_L_UNDEF otherwise. -}
#ccall Z3_get_bool_value , <Z3_context> -> <Z3_ast> -> IO <Z3_lbool>

{- | Return the kind of the given AST. -}
#ccall Z3_get_ast_kind , <Z3_context> -> <Z3_ast> -> IO <Z3_ast_kind>
#ccall Z3_is_app , <Z3_context> -> <Z3_ast> -> IO CInt
#ccall Z3_is_numeral_ast , <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return true if the give AST is a real algebraic number. -}
#ccall Z3_is_algebraic_number , <Z3_context> -> <Z3_ast> -> IO CInt

{- | Convert an \c ast into an \c APP_AST. This is just type casting.

       \pre \code Z3_get_ast_kind(c, a) == \c Z3_APP_AST \endcode -}
#ccall Z3_to_app , <Z3_context> -> <Z3_ast> -> IO <Z3_app>

{- | Convert an AST into a FUNC_DECL_AST. This is just type casting.

       \pre \code Z3_get_ast_kind(c, a) == Z3_FUNC_DECL_AST \endcode -}
#ccall Z3_to_func_decl , <Z3_context> -> <Z3_ast> -> IO <Z3_func_decl>

{- | Return numeral value, as a string of a numeric constant term

       \pre Z3_get_ast_kind(c, a) == Z3_NUMERAL_AST -}
#ccall Z3_get_numeral_string , <Z3_context> -> <Z3_ast> -> IO <Z3_string>

{- | Return numeral as a string in decimal notation.
       The result has at most \c precision decimal places.

       \pre Z3_get_ast_kind(c, a) == Z3_NUMERAL_AST || Z3_is_algebraic_number(c, a) -}
#ccall Z3_get_numeral_decimal_string , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_string>

{- | Return the numerator (as a numeral AST) of a numeral AST of sort Real.

       \pre Z3_get_ast_kind(c, a) == Z3_NUMERAL_AST -}
#ccall Z3_get_numerator , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Return the denominator (as a numeral AST) of a numeral AST of sort Real.

       \pre Z3_get_ast_kind(c, a) == Z3_NUMERAL_AST -}
#ccall Z3_get_denominator , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Return numeral value, as a pair of 64 bit numbers if the representation fits.

       \param c logical context.
       \param a term.
       \param num numerator.
       \param den denominator.

       Return \c Z3_TRUE if the numeral value fits in 64 bit numerals, \c Z3_FALSE otherwise.

       \pre Z3_get_ast_kind(a) == Z3_NUMERAL_AST -}
#ccall Z3_get_numeral_small , <Z3_context> -> <Z3_ast> -> Ptr CLong -> Ptr CLong -> IO CInt

{- | Similar to #Z3_get_numeral_string, but only succeeds if
       the value can fit in a machine int. Return Z3_TRUE if the call succeeded.

       \pre Z3_get_ast_kind(c, v) == Z3_NUMERAL_AST

       \sa Z3_get_numeral_string -}
#ccall Z3_get_numeral_int , <Z3_context> -> <Z3_ast> -> Ptr CInt -> IO CInt

{- | Similar to #Z3_get_numeral_string, but only succeeds if
       the value can fit in a machine unsigned int. Return Z3_TRUE if the call succeeded.

       \pre Z3_get_ast_kind(c, v) == Z3_NUMERAL_AST

       \sa Z3_get_numeral_string -}
#ccall Z3_get_numeral_uint , <Z3_context> -> <Z3_ast> -> Ptr CUInt -> IO CInt

{- | Similar to #Z3_get_numeral_string, but only succeeds if
       the value can fit in a machine unsigned __int64 int. Return Z3_TRUE if the call succeeded.

       \pre Z3_get_ast_kind(c, v) == Z3_NUMERAL_AST

       \sa Z3_get_numeral_string -}
#ccall Z3_get_numeral_uint64 , <Z3_context> -> <Z3_ast> -> Ptr CULong -> IO CInt

{- | Similar to #Z3_get_numeral_string, but only succeeds if
       the value can fit in a machine __int64 int. Return Z3_TRUE if the call succeeded.

       \pre Z3_get_ast_kind(c, v) == Z3_NUMERAL_AST

       \sa Z3_get_numeral_string -}
#ccall Z3_get_numeral_int64 , <Z3_context> -> <Z3_ast> -> Ptr CLong -> IO CInt

{- | Similar to #Z3_get_numeral_string, but only succeeds if
       the value can fit as a rational number as machine __int64 int. Return Z3_TRUE if the call succeeded.

       \pre Z3_get_ast_kind(c, v) == Z3_NUMERAL_AST

       \sa Z3_get_numeral_string -}
#ccall Z3_get_numeral_rational_int64 , <Z3_context> -> <Z3_ast> -> Ptr CLong -> Ptr CLong -> IO CInt

{- | Return a lower bound for the given real algebraic number.
       The interval isolating the number is smaller than 1/10^precision.
       The result is a numeral AST of sort Real.

       \pre Z3_is_algebraic_number(c, a) -}
#ccall Z3_get_algebraic_number_lower , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_ast>

{- | Return a upper bound for the given real algebraic number.
       The interval isolating the number is smaller than 1/10^precision.
       The result is a numeral AST of sort Real.

       \pre Z3_is_algebraic_number(c, a) -}
#ccall Z3_get_algebraic_number_upper , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_ast>

{- | Convert a Z3_pattern into Z3_ast. This is just type casting. -}
#ccall Z3_pattern_to_ast , <Z3_context> -> <Z3_pattern> -> IO <Z3_ast>

{- | Return number of terms in pattern. -}
#ccall Z3_get_pattern_num_terms , <Z3_context> -> <Z3_pattern> -> IO ()

{- | Return i'th ast in pattern. -}
#ccall Z3_get_pattern , <Z3_context> -> <Z3_pattern> -> CUInt -> IO <Z3_ast>

{- | Return index of de-Brujin bound variable.

       \pre Z3_get_ast_kind(a) == Z3_VAR_AST -}
#ccall Z3_get_index_value , <Z3_context> -> <Z3_ast> -> IO ()

{- | Determine if quantifier is universal.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
#ccall Z3_is_quantifier_forall , <Z3_context> -> <Z3_ast> -> IO CInt

{- | Obtain weight of quantifier.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
#ccall Z3_get_quantifier_weight , <Z3_context> -> <Z3_ast> -> IO ()

{- | Return number of patterns used in quantifier.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
#ccall Z3_get_quantifier_num_patterns , <Z3_context> -> <Z3_ast> -> IO ()

{- | Return i'th pattern.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
#ccall Z3_get_quantifier_pattern_ast , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_pattern>

{- | Return number of no_patterns used in quantifier.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
#ccall Z3_get_quantifier_num_no_patterns , <Z3_context> -> <Z3_ast> -> IO ()

{- | Return i'th no_pattern.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
#ccall Z3_get_quantifier_no_pattern_ast , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_ast>

{- | Return number of bound variables of quantifier.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
#ccall Z3_get_quantifier_num_bound , <Z3_context> -> <Z3_ast> -> IO ()

{- | Return symbol of the i'th bound variable.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
#ccall Z3_get_quantifier_bound_name , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_symbol>

{- | Return sort of the i'th bound variable.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
#ccall Z3_get_quantifier_bound_sort , <Z3_context> -> <Z3_ast> -> CUInt -> IO <Z3_sort>

{- | Return body of quantifier.

       \pre Z3_get_ast_kind(a) == Z3_QUANTIFIER_AST -}
#ccall Z3_get_quantifier_body , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Interface to simplifier.

        Provides an interface to the AST simplifier used by Z3.
        It returns an AST object which is equal to the argument.
        The returned AST is simplified using algebraic simplificaiton rules,
        such as constant propagation (propagating true/false over logical connectives). -}
#ccall Z3_simplify , <Z3_context> -> <Z3_ast> -> IO <Z3_ast>

{- | Interface to simplifier.

        Provides an interface to the AST simplifier used by Z3.
        This procedure is similar to #Z3_simplify, but the behavior of the simplifier
        can be configured using the given parameter set. -}
#ccall Z3_simplify_ex , <Z3_context> -> <Z3_ast> -> <Z3_params> -> IO <Z3_ast>

{- | Return a string describing all available parameters. -}
#ccall Z3_simplify_get_help , <Z3_context> -> IO <Z3_string>

{- | Return the parameter description set for the simplify procedure. -}
#ccall Z3_simplify_get_param_descrs , <Z3_context> -> IO <Z3_param_descrs>

{- | Update the arguments of term \c a using the arguments \c args.
       The number of arguments \c num_args should coincide
       with the number of arguments to \c a.
       If \c a is a quantifier, then num_args has to be 1. -}
#ccall Z3_update_term , <Z3_context> -> <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Substitute every occurrence of \ccode{from[i]} in \c a with \ccode{to[i]}, for \c i smaller than \c num_exprs.
       The result is the new AST. The arrays \c from and \c to must have size \c num_exprs.
       For every \c i smaller than \c num_exprs, we must have that sort of \ccode{from[i]} must be equal to sort of \ccode{to[i]}. -}
#ccall Z3_substitute , <Z3_context> -> <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Substitute the free variables in \c a with the expressions in \c to.
       For every \c i smaller than \c num_exprs, the variable with de-Bruijn index \c i is replaced with term \ccode{to[i]}. -}
#ccall Z3_substitute_vars , <Z3_context> -> <Z3_ast> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>

{- | Translate/Copy the AST \c a from context \c source to context \c target.
       AST \c a must have been created using context \c source.
       \pre source != target -}
#ccall Z3_translate , <Z3_context> -> <Z3_ast> -> <Z3_context> -> IO <Z3_ast>

{- | Increment the reference counter of the given model. -}
#ccall Z3_model_inc_ref , <Z3_context> -> <Z3_model> -> IO ()

{- | Decrement the reference counter of the given model. -}
#ccall Z3_model_dec_ref , <Z3_context> -> <Z3_model> -> IO ()

{- | Evaluate the AST node \c t in the given model.
       Return \c Z3_TRUE if succeeded, and store the result in \c v.

       If \c model_completion is Z3_TRUE, then Z3 will assign an interpretation for any constant or function that does
       not have an interpretation in \c m. These constants and functions were essentially don't cares.

       If \c model_completion is Z3_FALSE, then Z3 will not assign interpretations to constants for functions that do
       not have interpretations in \c m. Evaluation behaves as the identify function in this case.

       The evaluation may fail for the following reasons:

       - \c t contains a quantifier.

       - the model \c m is partial, that is, it doesn't have a complete interpretation for uninterpreted functions.
       That is, the option \ccode{MODEL_PARTIAL=true} was used.

       - \c t is type incorrect.

       - \c Z3_interrupt was invoked during evaluation. -}
#ccall Z3_model_eval , <Z3_context> -> <Z3_model> -> <Z3_ast> -> CInt -> Ptr <Z3_ast> -> IO CInt

{- | Return the interpretation (i.e., assignment) of constant \c a in the model \c m.
       Return \c NULL, if the model does not assign an interpretation for \c a.
       That should be interpreted as: the value of \c a does not matter.

       \pre Z3_get_arity(c, a) == 0 -}
#ccall Z3_model_get_const_interp , <Z3_context> -> <Z3_model> -> <Z3_func_decl> -> IO <Z3_ast>

{- | Test if there exists an interpretation (i.e., assignment) for \c a in the model \c m. -}
#ccall Z3_model_has_interp , <Z3_context> -> <Z3_model> -> <Z3_func_decl> -> IO CInt

{- | Return the interpretation of the function \c f in the model \c m.
       Return \c NULL, if the model does not assign an interpretation for \c f.
       That should be interpreted as: the \c f does not matter.

       \pre Z3_get_arity(c, f) > 0

       \remark Reference counting must be used to manage Z3_func_interp objects, even when the Z3_context was
       created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
#ccall Z3_model_get_func_interp , <Z3_context> -> <Z3_model> -> <Z3_func_decl> -> IO <Z3_func_interp>

{- | Return the number of constants assigned by the given model.

       \sa Z3_model_get_const_decl -}
#ccall Z3_model_get_num_consts , <Z3_context> -> <Z3_model> -> IO ()

{- | Return the i-th constant in the given model.

       \pre i < Z3_model_get_num_consts(c, m)

       \sa Z3_model_eval -}
#ccall Z3_model_get_const_decl , <Z3_context> -> <Z3_model> -> CUInt -> IO <Z3_func_decl>

{- | Return the number of function interpretations in the given model.

       A function interpretation is represented as a finite map and an 'else' value.
       Each entry in the finite map represents the value of a function given a set of arguments. -}
#ccall Z3_model_get_num_funcs , <Z3_context> -> <Z3_model> -> IO ()

{- | Return the declaration of the i-th function in the given model.

       \pre i < Z3_model_get_num_funcs(c, m)

       \sa Z3_model_get_num_funcs -}
#ccall Z3_model_get_func_decl , <Z3_context> -> <Z3_model> -> CUInt -> IO <Z3_func_decl>

{- | Return the number of uninterpreted sorts that \c m assigs an interpretation to.

       Z3 also provides an intepretation for uninterpreted sorts used in a formua.
       The interpretation for a sort \c s is a finite set of distinct values. We say this finite set is
       the "universe" of \c s.

       \sa Z3_model_get_sort
       \sa Z3_model_get_sort_universe -}
#ccall Z3_model_get_num_sorts , <Z3_context> -> <Z3_model> -> IO ()

{- | Return a uninterpreted sort that \c m assigns an interpretation.

       \pre i < Z3_model_get_num_sorts(c, m)

       \sa Z3_model_get_num_sorts
       \sa Z3_model_get_sort_universe -}
#ccall Z3_model_get_sort , <Z3_context> -> <Z3_model> -> CUInt -> IO <Z3_sort>

{- | Return the finite set of distinct values that represent the interpretation for sort \c s.

       \sa Z3_model_get_num_sorts
       \sa Z3_model_get_sort -}
#ccall Z3_model_get_sort_universe , <Z3_context> -> <Z3_model> -> <Z3_sort> -> IO <Z3_ast_vector>

{- | The \ccode{(_ as-array f)} AST node is a construct for assigning interpretations for arrays in Z3.
       It is the array such that forall indices \c i we have that \ccode{(select (_ as-array f) i)} is equal to \ccode{(f i)}.
       This procedure returns Z3_TRUE if the \c a is an \c as-array AST node.

       Z3 current solvers have minimal support for \c as_array nodes.

       \sa Z3_get_as_array_func_decl -}
#ccall Z3_is_as_array , <Z3_context> -> <Z3_ast> -> IO CInt

{- | Return the function declaration \c f associated with a \ccode{(_ as_array f)} node.

       \sa Z3_is_as_array -}
#ccall Z3_get_as_array_func_decl , <Z3_context> -> <Z3_ast> -> IO <Z3_func_decl>

{- | Increment the reference counter of the given Z3_func_interp object. -}
#ccall Z3_func_interp_inc_ref , <Z3_context> -> <Z3_func_interp> -> IO ()

{- | Decrement the reference counter of the given Z3_func_interp object. -}
#ccall Z3_func_interp_dec_ref , <Z3_context> -> <Z3_func_interp> -> IO ()

{- | Return the number of entries in the given function interpretation.

       A function interpretation is represented as a finite map and an 'else' value.
       Each entry in the finite map represents the value of a function given a set of arguments.
       This procedure return the number of element in the finite map of \c f. -}
#ccall Z3_func_interp_get_num_entries , <Z3_context> -> <Z3_func_interp> -> IO ()

{- | Return a "point" of the given function intepretation. It represents the
       value of \c f in a particular point.

       \pre i < Z3_func_interp_get_num_entries(c, f)

       \sa Z3_func_interp_get_num_entries -}
#ccall Z3_func_interp_get_entry , <Z3_context> -> <Z3_func_interp> -> CUInt -> IO <Z3_func_entry>

{- | Return the 'else' value of the given function interpretation.

       A function interpretation is represented as a finite map and an 'else' value.
       This procedure returns the 'else' value. -}
#ccall Z3_func_interp_get_else , <Z3_context> -> <Z3_func_interp> -> IO <Z3_ast>

{- | Return the arity (number of arguments) of the given function interpretation. -}
#ccall Z3_func_interp_get_arity , <Z3_context> -> <Z3_func_interp> -> IO ()

{- | Increment the reference counter of the given Z3_func_entry object. -}
#ccall Z3_func_entry_inc_ref , <Z3_context> -> <Z3_func_entry> -> IO ()

{- | Decrement the reference counter of the given Z3_func_entry object. -}
#ccall Z3_func_entry_dec_ref , <Z3_context> -> <Z3_func_entry> -> IO ()

{- | Return the value of this point.

       A Z3_func_entry object represents an element in the finite map used to encode
       a function interpretation.

       \sa Z3_func_interp_get_entry -}
#ccall Z3_func_entry_get_value , <Z3_context> -> <Z3_func_entry> -> IO <Z3_ast>

{- | Return the number of arguments in a Z3_func_entry object.

       \sa Z3_func_interp_get_entry -}
#ccall Z3_func_entry_get_num_args , <Z3_context> -> <Z3_func_entry> -> IO ()

{- | Return an argument of a Z3_func_entry object.

       \pre i < Z3_func_entry_get_num_args(c, e)

       \sa Z3_func_interp_get_entry -}
#ccall Z3_func_entry_get_arg , <Z3_context> -> <Z3_func_entry> -> CUInt -> IO <Z3_ast>

{- | Log interaction to a file.

       extra_API('Z3_open_log', INT, (_in(STRING),)) -}
#ccall Z3_open_log , <Z3_string> -> IO CInt

{- | Append user-defined string to interaction log.

       The interaction log is opened using Z3_open_log.
       It contains the formulas that are checked using Z3.
       You can use this command to append comments, for instance.

       extra_API('Z3_append_log', VOID, (_in(STRING),)) -}
#ccall Z3_append_log , <Z3_string> -> IO ()

{- | Close interaction log.

       extra_API('Z3_close_log', VOID, ()) -}
#ccall Z3_close_log , IO ()

{- | Enable/disable printing warning messages to the console.

       Warnings are printed after passing \c true, warning messages are
       suppressed after calling this method with \c false. -}
#ccall Z3_toggle_warning_messages , CInt -> IO ()

{- | Select mode for the format used for pretty-printing AST nodes.

       The default mode for pretty printing AST nodes is to produce
       SMT-LIB style output where common subexpressions are printed
       at each occurrence. The mode is called Z3_PRINT_SMTLIB_FULL.
       To print shared common subexpressions only once,
       use the Z3_PRINT_LOW_LEVEL mode.
       To print in way that conforms to SMT-LIB standards and uses let
       expressions to share common sub-expressions use Z3_PRINT_SMTLIB_COMPLIANT.

       \sa Z3_ast_to_string
       \sa Z3_pattern_to_string
       \sa Z3_func_decl_to_string -}
#ccall Z3_set_ast_print_mode , <Z3_context> -> <Z3_ast_print_mode> -> IO ()

{- | Convert the given AST node into a string.

       \warning The result buffer is statically allocated by Z3. It will
       be automatically deallocated when #Z3_del_context is invoked.
       So, the buffer is invalidated in the next call to \c Z3_ast_to_string.

       \sa Z3_pattern_to_string
       \sa Z3_sort_to_string -}
#ccall Z3_ast_to_string , <Z3_context> -> <Z3_ast> -> IO <Z3_string>
#ccall Z3_pattern_to_string , <Z3_context> -> <Z3_pattern> -> IO <Z3_string>
#ccall Z3_sort_to_string , <Z3_context> -> <Z3_sort> -> IO <Z3_string>
#ccall Z3_func_decl_to_string , <Z3_context> -> <Z3_func_decl> -> IO <Z3_string>

{- | Convert the given model into a string.

       \warning The result buffer is statically allocated by Z3. It will
       be automatically deallocated when #Z3_del_context is invoked.
       So, the buffer is invalidated in the next call to \c Z3_model_to_string. -}
#ccall Z3_model_to_string , <Z3_context> -> <Z3_model> -> IO <Z3_string>

{- | Convert the given benchmark into SMT-LIB formatted string.

       \warning The result buffer is statically allocated by Z3. It will
       be automatically deallocated when #Z3_del_context is invoked.
       So, the buffer is invalidated in the next call to \c Z3_benchmark_to_smtlib_string.

       \param c - context.
       \param name - name of benchmark. The argument is optional.
       \param logic - the benchmark logic.
       \param status - the status string (sat, unsat, or unknown)
       \param attributes - other attributes, such as source, difficulty or category.
       \param num_assumptions - number of assumptions.
       \param assumptions - auxiliary assumptions.
       \param formula - formula to be checked for consistency in conjunction with assumptions. -}
#ccall Z3_benchmark_to_smtlib_string , <Z3_context> -> <Z3_string> -> <Z3_string> -> <Z3_string> -> <Z3_string> -> CUInt -> Ptr <Z3_ast> -> <Z3_ast> -> IO <Z3_string>

{- | Parse the given string using the SMT-LIB2 parser.

       It returns a formula comprising of the conjunction of assertions in the scope
       (up to push/pop) at the end of the string. -}
#ccall Z3_parse_smtlib2_string , <Z3_context> -> <Z3_string> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_func_decl> -> IO <Z3_ast>

{- | Similar to #Z3_parse_smtlib2_string, but reads the benchmark from a file. -}
#ccall Z3_parse_smtlib2_file , <Z3_context> -> <Z3_string> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_func_decl> -> IO <Z3_ast>

{- | Parse the given string using the SMT-LIB parser.

       The symbol table of the parser can be initialized using the given sorts and declarations.
       The symbols in the arrays \c sort_names and \c decl_names don't need to match the names
       of the sorts and declarations in the arrays \c sorts and \c decls. This is an useful feature
       since we can use arbitrary names to reference sorts and declarations defined using the C API.

       The formulas, assumptions and declarations defined in \c str can be extracted using the functions:
       #Z3_get_smtlib_num_formulas, #Z3_get_smtlib_formula, #Z3_get_smtlib_num_assumptions, #Z3_get_smtlib_assumption,
       #Z3_get_smtlib_num_decls, and #Z3_get_smtlib_decl. -}
#ccall Z3_parse_smtlib_string , <Z3_context> -> <Z3_string> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_func_decl> -> IO ()

{- | Similar to #Z3_parse_smtlib_string, but reads the benchmark from a file. -}
#ccall Z3_parse_smtlib_file , <Z3_context> -> <Z3_string> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_sort> -> CUInt -> Ptr <Z3_symbol> -> Ptr <Z3_func_decl> -> IO ()

{- | Return the number of SMTLIB formulas parsed by the last call to #Z3_parse_smtlib_string or #Z3_parse_smtlib_file. -}
#ccall Z3_get_smtlib_num_formulas , <Z3_context> -> IO ()

{- | Return the i-th formula parsed by the last call to #Z3_parse_smtlib_string or #Z3_parse_smtlib_file.

       \pre i < Z3_get_smtlib_num_formulas(c) -}
#ccall Z3_get_smtlib_formula , <Z3_context> -> CUInt -> IO <Z3_ast>

{- | Return the number of SMTLIB assumptions parsed by #Z3_parse_smtlib_string or #Z3_parse_smtlib_file. -}
#ccall Z3_get_smtlib_num_assumptions , <Z3_context> -> IO ()

{- | Return the i-th assumption parsed by the last call to #Z3_parse_smtlib_string or #Z3_parse_smtlib_file.

       \pre i < Z3_get_smtlib_num_assumptions(c) -}
#ccall Z3_get_smtlib_assumption , <Z3_context> -> CUInt -> IO <Z3_ast>

{- | Return the number of declarations parsed by #Z3_parse_smtlib_string or #Z3_parse_smtlib_file. -}
#ccall Z3_get_smtlib_num_decls , <Z3_context> -> IO ()

{- | Return the i-th declaration parsed by the last call to #Z3_parse_smtlib_string or #Z3_parse_smtlib_file.

       \pre i < Z3_get_smtlib_num_decls(c) -}
#ccall Z3_get_smtlib_decl , <Z3_context> -> CUInt -> IO <Z3_func_decl>

{- | Return the number of sorts parsed by #Z3_parse_smtlib_string or #Z3_parse_smtlib_file. -}
#ccall Z3_get_smtlib_num_sorts , <Z3_context> -> IO ()

{- | Return the i-th sort parsed by the last call to #Z3_parse_smtlib_string or #Z3_parse_smtlib_file.

       \pre i < Z3_get_smtlib_num_sorts(c) -}
#ccall Z3_get_smtlib_sort , <Z3_context> -> CUInt -> IO <Z3_sort>

{- | Retrieve that last error message information generated from parsing. -}
#ccall Z3_get_smtlib_error , <Z3_context> -> IO <Z3_string>

{- | Return the error code for the last API call.

       A call to a Z3 function may return a non Z3_OK error code,
       when it is not used correctly.

       \sa Z3_set_error_handler -}
#ccall Z3_get_error_code , <Z3_context> -> IO <Z3_error_code>

{- | Register a Z3 error handler.

       A call to a Z3 function may return a non Z3_OK error code, when
       it is not used correctly.  An error handler can be registered
       and will be called in this case.  To disable the use of the
       error handler, simply register with \c h=NULL.

       \warning Log files, created using #Z3_open_log, may be potentially incomplete/incorrect if error handlers are used.

       \sa Z3_get_error_code -}
#ccall Z3_set_error_handler , <Z3_context> -> <Z3_error_handler> -> IO ()

{- | Set an error. -}
#ccall Z3_set_error , <Z3_context> -> <Z3_error_code> -> IO ()

{- | Return a string describing the given error code. -}
#ccall Z3_get_error_msg , <Z3_context> -> <Z3_error_code> -> IO <Z3_string>

{- | Return a string describing the given error code. 
       Retained function name for backwards compatibility within v4.1 -}
#ccall Z3_get_error_msg_ex , <Z3_context> -> <Z3_error_code> -> IO <Z3_string>

{- | Return Z3 version number information. -}
#ccall Z3_get_version , Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> IO ()

{- | Return a string that fully describes the version of Z3 in use. -}
#ccall Z3_get_full_version , IO <Z3_string>

{- | Enable tracing messages tagged as \c tag when Z3 is compiled in debug mode.
       It is a NOOP otherwise -}
#ccall Z3_enable_trace , <Z3_string> -> IO ()

{- | Disable tracing messages tagged as \c tag when Z3 is compiled in debug mode.
       It is a NOOP otherwise -}
#ccall Z3_disable_trace , <Z3_string> -> IO ()

{- | Reset all allocated resources.

       Use this facility on out-of memory errors.
       It allows discharging the previous state and resuming afresh.
       Any pointers previously returned by the API
       become invalid. -}
#ccall Z3_reset_memory , IO ()

{- | Destroy all allocated resources.

       Any pointers previously returned by the API become invalid.
       Can be used for memory leak detection. -}
#ccall Z3_finalize_memory , IO ()

{- | Create a goal (aka problem). A goal is essentially a set
       of formulas, that can be solved and/or transformed using
       tactics and solvers.

       If models == true, then model generation is enabled for the new goal.

       If unsat_cores == true, then unsat core generation is enabled for the new goal.

       If proofs == true, then proof generation is enabled for the new goal. Remark, the
       Z3 context c must have been created with proof generation support.

       \remark Reference counting must be used to manage goals, even when the Z3_context was
       created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
#ccall Z3_mk_goal , <Z3_context> -> CInt -> CInt -> CInt -> IO <Z3_goal>

{- | Increment the reference counter of the given goal. -}
#ccall Z3_goal_inc_ref , <Z3_context> -> <Z3_goal> -> IO ()

{- | Decrement the reference counter of the given goal. -}
#ccall Z3_goal_dec_ref , <Z3_context> -> <Z3_goal> -> IO ()

{- | Return the "precision" of the given goal. Goals can be transformed using over and under approximations.
       A under approximation is applied when the objective is to find a model for a given goal.
       An over approximation is applied when the objective is to find a proof for a given goal. -}
#ccall Z3_goal_precision , <Z3_context> -> <Z3_goal> -> IO <Z3_goal_prec>

{- | Add a new formula \c a to the given goal. -}
#ccall Z3_goal_assert , <Z3_context> -> <Z3_goal> -> <Z3_ast> -> IO ()

{- | Return true if the given goal contains the formula \c false. -}
#ccall Z3_goal_inconsistent , <Z3_context> -> <Z3_goal> -> IO CInt

{- | Return the depth of the given goal. It tracks how many transformations were applied to it. -}
#ccall Z3_goal_depth , <Z3_context> -> <Z3_goal> -> IO ()

{- | Erase all formulas from the given goal. -}
#ccall Z3_goal_reset , <Z3_context> -> <Z3_goal> -> IO ()

{- | Return the number of formulas in the given goal. -}
#ccall Z3_goal_size , <Z3_context> -> <Z3_goal> -> IO ()

{- | Return a formula from the given goal.

       \pre idx < Z3_goal_size(c, g) -}
#ccall Z3_goal_formula , <Z3_context> -> <Z3_goal> -> CUInt -> IO <Z3_ast>

{- | Return the number of formulas, subformulas and terms in the given goal. -}
#ccall Z3_goal_num_exprs , <Z3_context> -> <Z3_goal> -> IO ()

{- | Return true if the goal is empty, and it is precise or the product of a under approximation. -}
#ccall Z3_goal_is_decided_sat , <Z3_context> -> <Z3_goal> -> IO CInt

{- | Return true if the goal contains false, and it is precise or the product of an over approximation. -}
#ccall Z3_goal_is_decided_unsat , <Z3_context> -> <Z3_goal> -> IO CInt

{- | Copy a goal \c g from the context \c source to a the context \c target. -}
#ccall Z3_goal_translate , <Z3_context> -> <Z3_goal> -> <Z3_context> -> IO <Z3_goal>

{- | Convert a goal into a string. -}
#ccall Z3_goal_to_string , <Z3_context> -> <Z3_goal> -> IO <Z3_string>

{- | Return a tactic associated with the given name.
       The complete list of tactics may be obtained using the procedures #Z3_get_num_tactics and #Z3_get_tactic_name.
       It may also be obtained using the command \ccode{(help-tactic)} in the SMT 2.0 front-end.

       Tactics are the basic building block for creating custom solvers for specific problem domains. -}
#ccall Z3_mk_tactic , <Z3_context> -> <Z3_string> -> IO <Z3_tactic>

{- | Increment the reference counter of the given tactic. -}
#ccall Z3_tactic_inc_ref , <Z3_context> -> <Z3_tactic> -> IO ()

{- | Decrement the reference counter of the given tactic. -}
#ccall Z3_tactic_dec_ref , <Z3_context> -> <Z3_tactic> -> IO ()

{- | Return a probe associated with the given name.
       The complete list of probes may be obtained using the procedures #Z3_get_num_probes and #Z3_get_probe_name.
       It may also be obtained using the command \ccode{(help-tactic)} in the SMT 2.0 front-end.

       Probes are used to inspect a goal (aka problem) and collect information that may be used to decide
       which solver and/or preprocessing step will be used. -}
#ccall Z3_mk_probe , <Z3_context> -> <Z3_string> -> IO <Z3_probe>

{- | Increment the reference counter of the given probe. -}
#ccall Z3_probe_inc_ref , <Z3_context> -> <Z3_probe> -> IO ()

{- | Decrement the reference counter of the given probe. -}
#ccall Z3_probe_dec_ref , <Z3_context> -> <Z3_probe> -> IO ()

{- | Return a tactic that applies \c t1 to a given goal and \c t2
       to every subgoal produced by t1. -}
#ccall Z3_tactic_and_then , <Z3_context> -> <Z3_tactic> -> <Z3_tactic> -> IO <Z3_tactic>

{- | Return a tactic that first applies \c t1 to a given goal,
       if it fails then returns the result of \c t2 applied to the given goal. -}
#ccall Z3_tactic_or_else , <Z3_context> -> <Z3_tactic> -> <Z3_tactic> -> IO <Z3_tactic>

{- | Return a tactic that applies the given tactics in parallel. -}
#ccall Z3_tactic_par_or , <Z3_context> -> CUInt -> Ptr <Z3_tactic> -> IO <Z3_tactic>

{- | Return a tactic that applies \c t1 to a given goal and then \c t2
       to every subgoal produced by t1. The subgoals are processed in parallel. -}
#ccall Z3_tactic_par_and_then , <Z3_context> -> <Z3_tactic> -> <Z3_tactic> -> IO <Z3_tactic>

{- | Return a tactic that applies \c t to a given goal for \c ms milliseconds.
       If \c t does not terminate in \c ms milliseconds, then it fails. -}
#ccall Z3_tactic_try_for , <Z3_context> -> <Z3_tactic> -> CUInt -> IO <Z3_tactic>

{- | Return a tactic that applies \c t to a given goal is the probe \c p evaluates to true.
       If \c p evaluates to false, then the new tactic behaves like the skip tactic. -}
#ccall Z3_tactic_when , <Z3_context> -> <Z3_probe> -> <Z3_tactic> -> IO <Z3_tactic>

{- | Return a tactic that applies \c t1 to a given goal if the probe \c p evaluates to true,
       and \c t2 if \c p evaluates to false. -}
#ccall Z3_tactic_cond , <Z3_context> -> <Z3_probe> -> <Z3_tactic> -> <Z3_tactic> -> IO <Z3_tactic>

{- | Return a tactic that keeps applying \c t until the goal is not modified anymore or the maximum
       number of iterations \c max is reached. -}
#ccall Z3_tactic_repeat , <Z3_context> -> <Z3_tactic> -> CUInt -> IO <Z3_tactic>

{- | Return a tactic that just return the given goal. -}
#ccall Z3_tactic_skip , <Z3_context> -> IO <Z3_tactic>

{- | Return a tactic that always fails. -}
#ccall Z3_tactic_fail , <Z3_context> -> IO <Z3_tactic>

{- | Return a tactic that fails if the probe \c p evaluates to false. -}
#ccall Z3_tactic_fail_if , <Z3_context> -> <Z3_probe> -> IO <Z3_tactic>

{- | Return a tactic that fails if the goal is not trivially satisfiable (i.e., empty) or
       trivially unsatisfiable (i.e., contains false). -}
#ccall Z3_tactic_fail_if_not_decided , <Z3_context> -> IO <Z3_tactic>

{- | Return a tactic that applies \c t using the given set of parameters. -}
#ccall Z3_tactic_using_params , <Z3_context> -> <Z3_tactic> -> <Z3_params> -> IO <Z3_tactic>

{- | Return a probe that always evaluates to val. -}
#ccall Z3_probe_const , <Z3_context> -> CDouble -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when the value returned by \c p1 is less than the value returned by \c p2.

       \remark For probes, "true" is any value different from 0.0. -}
#ccall Z3_probe_lt , <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when the value returned by \c p1 is greater than the value returned by \c p2.

       \remark For probes, "true" is any value different from 0.0. -}
#ccall Z3_probe_gt , <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when the value returned by \c p1 is less than or equal to the value returned by \c p2.

       \remark For probes, "true" is any value different from 0.0. -}
#ccall Z3_probe_le , <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when the value returned by \c p1 is greater than or equal to the value returned by \c p2.

       \remark For probes, "true" is any value different from 0.0. -}
#ccall Z3_probe_ge , <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when the value returned by \c p1 is equal to the value returned by \c p2.

       \remark For probes, "true" is any value different from 0.0. -}
#ccall Z3_probe_eq , <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when \c p1 and \c p2 evaluates to true.

       \remark For probes, "true" is any value different from 0.0. -}
#ccall Z3_probe_and , <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when \c p1 or \c p2 evaluates to true.

       \remark For probes, "true" is any value different from 0.0. -}
#ccall Z3_probe_or , <Z3_context> -> <Z3_probe> -> <Z3_probe> -> IO <Z3_probe>

{- | Return a probe that evaluates to "true" when \c p does not evaluate to true.

       \remark For probes, "true" is any value different from 0.0. -}
#ccall Z3_probe_not , <Z3_context> -> <Z3_probe> -> IO <Z3_probe>

{- | Return the number of builtin tactics available in Z3. -}
#ccall Z3_get_num_tactics , <Z3_context> -> IO ()

{- | Return the name of the idx tactic.

       \pre i < Z3_get_num_tactics(c) -}
#ccall Z3_get_tactic_name , <Z3_context> -> CUInt -> IO <Z3_string>

{- | Return the number of builtin probes available in Z3. -}
#ccall Z3_get_num_probes , <Z3_context> -> IO ()

{- | Return the name of the i probe.

       \pre i < Z3_get_num_probes(c) -}
#ccall Z3_get_probe_name , <Z3_context> -> CUInt -> IO <Z3_string>

{- | Return a string containing a description of parameters accepted by the given tactic. -}
#ccall Z3_tactic_get_help , <Z3_context> -> <Z3_tactic> -> IO <Z3_string>

{- | Return the parameter description set for the given tactic object. -}
#ccall Z3_tactic_get_param_descrs , <Z3_context> -> <Z3_tactic> -> IO <Z3_param_descrs>

{- | Return a string containing a description of the tactic with the given name. -}
#ccall Z3_tactic_get_descr , <Z3_context> -> <Z3_string> -> IO <Z3_string>

{- | Return a string containing a description of the probe with the given name. -}
#ccall Z3_probe_get_descr , <Z3_context> -> <Z3_string> -> IO <Z3_string>

{- | Execute the probe over the goal. The probe always produce a double value.
       "Boolean" probes return 0.0 for false, and a value different from 0.0 for true. -}
#ccall Z3_probe_apply , <Z3_context> -> <Z3_probe> -> <Z3_goal> -> IO CDouble

{- | Apply tactic \c t to the goal \c g. -}
#ccall Z3_tactic_apply , <Z3_context> -> <Z3_tactic> -> <Z3_goal> -> IO <Z3_apply_result>

{- | Apply tactic \c t to the goal \c g using the parameter set \c p. -}
#ccall Z3_tactic_apply_ex , <Z3_context> -> <Z3_tactic> -> <Z3_goal> -> <Z3_params> -> IO <Z3_apply_result>

{- | Increment the reference counter of the given \c Z3_apply_result object. -}
#ccall Z3_apply_result_inc_ref , <Z3_context> -> <Z3_apply_result> -> IO ()

{- | Decrement the reference counter of the given \c Z3_apply_result object. -}
#ccall Z3_apply_result_dec_ref , <Z3_context> -> <Z3_apply_result> -> IO ()

{- | Convert the \c Z3_apply_result object returned by #Z3_tactic_apply into a string. -}
#ccall Z3_apply_result_to_string , <Z3_context> -> <Z3_apply_result> -> IO <Z3_string>

{- | Return the number of subgoals in the \c Z3_apply_result object returned by #Z3_tactic_apply. -}
#ccall Z3_apply_result_get_num_subgoals , <Z3_context> -> <Z3_apply_result> -> IO ()

{- | Return one of the subgoals in the \c Z3_apply_result object returned by #Z3_tactic_apply.

       \pre i < Z3_apply_result_get_num_subgoals(c, r) -}
#ccall Z3_apply_result_get_subgoal , <Z3_context> -> <Z3_apply_result> -> CUInt -> IO <Z3_goal>

{- | Convert a model for the subgoal \c Z3_apply_result_get_subgoal(c, r, i) into a model for the original goal \c g.
       Where \c g is the goal used to create \c r using \c Z3_tactic_apply(c, t, g). -}
#ccall Z3_apply_result_convert_model , <Z3_context> -> <Z3_apply_result> -> CUInt -> <Z3_model> -> IO <Z3_model>

{- | Create a new (incremental) solver. This solver also uses a
       set of builtin tactics for handling the first check-sat command, and
       check-sat commands that take more than a given number of milliseconds to be solved.

       \remark User must use #Z3_solver_inc_ref and #Z3_solver_dec_ref to manage solver objects.
       Even if the context was created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
#ccall Z3_mk_solver , <Z3_context> -> IO <Z3_solver>

{- | Create a new (incremental) solver.

       The function #Z3_solver_get_model retrieves a model if the
       assertions is satisfiable (i.e., the result is \c
       Z3_L_TRUE) and model construction is enabled.
       The function #Z3_solver_get_model can also be used even
       if the result is \c Z3_L_UNDEF, but the returned model
       is not guaranteed to satisfy quantified assertions.

       \remark User must use #Z3_solver_inc_ref and #Z3_solver_dec_ref to manage solver objects.
       Even if the context was created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
#ccall Z3_mk_simple_solver , <Z3_context> -> IO <Z3_solver>

{- | Create a new solver customized for the given logic.
       It behaves like #Z3_mk_solver if the logic is unknown or unsupported.

       \remark User must use #Z3_solver_inc_ref and #Z3_solver_dec_ref to manage solver objects.
       Even if the context was created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
#ccall Z3_mk_solver_for_logic , <Z3_context> -> <Z3_symbol> -> IO <Z3_solver>

{- | Create a new solver that is implemented using the given tactic.
       The solver supports the commands #Z3_solver_push and #Z3_solver_pop, but it
       will always solve each #Z3_solver_check from scratch.

       \remark User must use #Z3_solver_inc_ref and #Z3_solver_dec_ref to manage solver objects.
       Even if the context was created using #Z3_mk_context instead of #Z3_mk_context_rc. -}
#ccall Z3_mk_solver_from_tactic , <Z3_context> -> <Z3_tactic> -> IO <Z3_solver>

{- | Copy a solver \c s from the context \c source to a the context \c target. -}
#ccall Z3_solver_translate , <Z3_context> -> <Z3_solver> -> <Z3_context> -> IO <Z3_solver>

{- | Return a string describing all solver available parameters. -}
#ccall Z3_solver_get_help , <Z3_context> -> <Z3_solver> -> IO <Z3_string>

{- | Return the parameter description set for the given solver object. -}
#ccall Z3_solver_get_param_descrs , <Z3_context> -> <Z3_solver> -> IO <Z3_param_descrs>

{- | Set the given solver using the given parameters. -}
#ccall Z3_solver_set_params , <Z3_context> -> <Z3_solver> -> <Z3_params> -> IO ()

{- | Increment the reference counter of the given solver. -}
#ccall Z3_solver_inc_ref , <Z3_context> -> <Z3_solver> -> IO ()

{- | Decrement the reference counter of the given solver. -}
#ccall Z3_solver_dec_ref , <Z3_context> -> <Z3_solver> -> IO ()

{- | Create a backtracking point.

       The solver contains a stack of assertions.

       \sa Z3_solver_pop -}
#ccall Z3_solver_push , <Z3_context> -> <Z3_solver> -> IO ()

{- | Backtrack \c n backtracking points.

       \sa Z3_solver_push

       \pre n <= Z3_solver_get_num_scopes(c, s) -}
#ccall Z3_solver_pop , <Z3_context> -> <Z3_solver> -> CUInt -> IO ()

{- | Remove all assertions from the solver. -}
#ccall Z3_solver_reset , <Z3_context> -> <Z3_solver> -> IO ()

{- | Return the number of backtracking points.

       \sa Z3_solver_push
       \sa Z3_solver_pop -}
#ccall Z3_solver_get_num_scopes , <Z3_context> -> <Z3_solver> -> IO ()

{- | Assert a constraint into the solver.

       The functions #Z3_solver_check and #Z3_solver_check_assumptions should be
       used to check whether the logical context is consistent or not. -}
#ccall Z3_solver_assert , <Z3_context> -> <Z3_solver> -> <Z3_ast> -> IO ()

{- | Assert a constraint \c a into the solver, and track it (in the unsat) core using
       the Boolean constant \c p.

       This API is an alternative to #Z3_solver_check_assumptions for extracting unsat cores.
       Both APIs can be used in the same solver. The unsat core will contain a combination
       of the Boolean variables provided using Z3_solver_assert_and_track and the Boolean literals
       provided using #Z3_solver_check_assumptions.

       \pre \c a must be a Boolean expression
       \pre \c p must be a Boolean constant (aka variable). -}
#ccall Z3_solver_assert_and_track , <Z3_context> -> <Z3_solver> -> <Z3_ast> -> <Z3_ast> -> IO ()

{- | Return the set of asserted formulas on the solver. -}
#ccall Z3_solver_get_assertions , <Z3_context> -> <Z3_solver> -> IO <Z3_ast_vector>

{- | Check whether the assertions in a given solver are consistent or not.

       The function #Z3_solver_get_model retrieves a model if the
       assertions is satisfiable (i.e., the result is \c
       Z3_L_TRUE) and model construction is enabled.
       Note that if the call returns Z3_L_UNDEF, Z3 does not
       ensure that calls to #Z3_solver_get_model succeed and any models
       produced in this case are not guaranteed to satisfy the assertions.

       The function #Z3_solver_get_proof retrieves a proof if proof
       generation was enabled when the context was created, and the
       assertions are unsatisfiable (i.e., the result is \c Z3_L_FALSE). -}
#ccall Z3_solver_check , <Z3_context> -> <Z3_solver> -> IO <Z3_lbool>

{- | Check whether the assertions in the given solver and
       optional assumptions are consistent or not.

       The function #Z3_solver_get_unsat_core retrieves the subset of the
       assumptions used in the unsatisfiability proof produced by Z3.

       \sa Z3_solver_check -}
#ccall Z3_solver_check_assumptions , <Z3_context> -> <Z3_solver> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_lbool>

{- | Retrieve congruence class representatives for terms.

       The function can be used for relying on Z3 to identify equal terms under the current
       set of assumptions. The array of terms and array of class identifiers should have
       the same length. The class identifiers are numerals that are assigned to the same
       value for their corresponding terms if the current context forces the terms to be
       equal. You cannot deduce that terms corresponding to different numerals must be all different,
       (especially when using non-convex theories).
       All implied equalities are returned by this call.
       This means that two terms map to the same class identifier if and only if
       the current context implies that they are equal.

       A side-effect of the function is a satisfiability check on the assertions on the solver that is passed in.
       The function return Z3_L_FALSE if the current assertions are not satisfiable. -}
#ccall Z3_get_implied_equalities , <Z3_context> -> <Z3_solver> -> CUInt -> Ptr <Z3_ast> -> Ptr CUInt -> IO <Z3_lbool>

{- | retrieve consequences from solver that determine values of the supplied function symbols. -}
#ccall Z3_solver_get_consequences , <Z3_context> -> <Z3_solver> -> <Z3_ast_vector> -> <Z3_ast_vector> -> <Z3_ast_vector> -> IO <Z3_lbool>

{- | Retrieve the model for the last #Z3_solver_check or #Z3_solver_check_assumptions

       The error handler is invoked if a model is not available because
       the commands above were not invoked for the given solver, or if the result was \c Z3_L_FALSE. -}
#ccall Z3_solver_get_model , <Z3_context> -> <Z3_solver> -> IO <Z3_model>

{- | Retrieve the proof for the last #Z3_solver_check or #Z3_solver_check_assumptions

       The error handler is invoked if proof generation is not enabled,
       or if the commands above were not invoked for the given solver,
       or if the result was different from \c Z3_L_FALSE. -}
#ccall Z3_solver_get_proof , <Z3_context> -> <Z3_solver> -> IO <Z3_ast>

{- | Retrieve the unsat core for the last #Z3_solver_check_assumptions
       The unsat core is a subset of the assumptions \c a. -}
#ccall Z3_solver_get_unsat_core , <Z3_context> -> <Z3_solver> -> IO <Z3_ast_vector>

{- | Return a brief justification for an "unknown" result (i.e., Z3_L_UNDEF) for
       the commands #Z3_solver_check and #Z3_solver_check_assumptions -}
#ccall Z3_solver_get_reason_unknown , <Z3_context> -> <Z3_solver> -> IO <Z3_string>

{- | Return statistics for the given solver.

       \remark User must use #Z3_stats_inc_ref and #Z3_stats_dec_ref to manage Z3_stats objects. -}
#ccall Z3_solver_get_statistics , <Z3_context> -> <Z3_solver> -> IO <Z3_stats>

{- | Convert a solver into a string. -}
#ccall Z3_solver_to_string , <Z3_context> -> <Z3_solver> -> IO <Z3_string>

{- | Convert a statistics into a string. -}
#ccall Z3_stats_to_string , <Z3_context> -> <Z3_stats> -> IO <Z3_string>

{- | Increment the reference counter of the given statistics object. -}
#ccall Z3_stats_inc_ref , <Z3_context> -> <Z3_stats> -> IO ()

{- | Decrement the reference counter of the given statistics object. -}
#ccall Z3_stats_dec_ref , <Z3_context> -> <Z3_stats> -> IO ()

{- | Return the number of statistical data in \c s. -}
#ccall Z3_stats_size , <Z3_context> -> <Z3_stats> -> IO ()

{- | Return the key (a string) for a particular statistical data.

       \pre idx < Z3_stats_size(c, s) -}
#ccall Z3_stats_get_key , <Z3_context> -> <Z3_stats> -> CUInt -> IO <Z3_string>

{- | Return Z3_TRUE if the given statistical data is a unsigned integer.

       \pre idx < Z3_stats_size(c, s) -}
#ccall Z3_stats_is_uint , <Z3_context> -> <Z3_stats> -> CUInt -> IO CInt

{- | Return Z3_TRUE if the given statistical data is a double.

       \pre idx < Z3_stats_size(c, s) -}
#ccall Z3_stats_is_double , <Z3_context> -> <Z3_stats> -> CUInt -> IO CInt

{- | Return the unsigned value of the given statistical data.

       \pre idx < Z3_stats_size(c, s) && Z3_stats_is_uint(c, s) -}
#ccall Z3_stats_get_uint_value , <Z3_context> -> <Z3_stats> -> CUInt -> IO ()

{- | Return the double value of the given statistical data.

       \pre idx < Z3_stats_size(c, s) && Z3_stats_is_double(c, s) -}
#ccall Z3_stats_get_double_value , <Z3_context> -> <Z3_stats> -> CUInt -> IO CDouble

{- | Return the estimated allocated memory in bytes. -}
#ccall Z3_get_estimated_alloc_size , IO CULong
