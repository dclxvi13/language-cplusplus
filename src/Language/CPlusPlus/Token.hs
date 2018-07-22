module Language.CPlusPlus.Token where

import qualified Language.CPlusPlus.Lexer as L

import Text.Parsec
import Text.Parsec.Pos (newPos)

type P = Parsec [CppToken] ()
type CppToken = L.L L.Token

cppToken :: L.Token -> P ()
cppToken t' = cppToken' (\t -> if t' == t then Just () else Nothing)

cppToken' :: (L.Token -> Maybe a) -> P a
cppToken' test = token showTok posFromTok testTok
                                  where
                                    showTok    (L.L _ t)      = show t
                                    posFromTok (L.L (l,c) _)  = newPos "" l c
                                    testTok    (L.L _ t)      = test t

kwAlignas         = cppToken L.KW_Alignas
kwAlignof         = cppToken L.KW_Alignof
kwAsm             = cppToken L.KW_Asm
kwAuto            = cppToken L.KW_Auto
kwBool            = cppToken L.KW_Bool
kwBreak           = cppToken L.KW_Break
kwCase            = cppToken L.KW_Case
kwCatch           = cppToken L.KW_Catch
kwChar            = cppToken L.KW_Char
kwChar16T         = cppToken L.KW_Char16T
kwChar32T         = cppToken L.KW_Char32T
kwClass           = cppToken L.KW_Class
kwConst           = cppToken L.KW_Const
kwConstexpr       = cppToken L.KW_Constexpr
kwConstCast       = cppToken L.KW_ConstCast
kwContinue        = cppToken L.KW_Continue
kwDecltype        = cppToken L.KW_Decltype
kwDefault         = cppToken L.KW_Default
kwDelete          = cppToken L.KW_Delete
kwDo              = cppToken L.KW_Do
kwDouble          = cppToken L.KW_Double
kwDynamicCast     = cppToken L.KW_DynamicCast
kwElse            = cppToken L.KW_Else
kwEnum            = cppToken L.KW_Enum
kwExplicit        = cppToken L.KW_Explicit
kwExport          = cppToken L.KW_Export
kwExtern          = cppToken L.KW_Extern
kwFalse           = cppToken L.KW_False
kwFloat           = cppToken L.KW_Float
kwFor             = cppToken L.KW_For
kwFriend          = cppToken L.KW_Friend
kwGoto            = cppToken L.KW_Goto
kwIf              = cppToken L.KW_If
kwInline          = cppToken L.KW_Int
kwInt             = cppToken L.KW_Inline
kwLong            = cppToken L.KW_Long
kwMutable         = cppToken L.KW_Mutable
kwNamespace       = cppToken L.KW_Namespace
kwNew             = cppToken L.KW_New
kwNoexcept        = cppToken L.KW_Noexcept
kwNullptr         = cppToken L.KW_Nullptr
kwOperator        = cppToken L.KW_Operator
kwPrivate         = cppToken L.KW_Private
kwProtected       = cppToken L.KW_Protected
kwPublic          = cppToken L.KW_Public
kwRegister        = cppToken L.KW_Register
kwReinterpretCast = cppToken L.KW_ReinterpretCast
kwReturn          = cppToken L.KW_Return
kwShort           = cppToken L.KW_Short
kwSigned          = cppToken L.KW_Signed
kwSizeof          = cppToken L.KW_Sizeof
kwStatic          = cppToken L.KW_Static
kwStaticAssert    = cppToken L.KW_StaticAssert
kwStaticCast      = cppToken L.KW_StaticCast
kwStruct          = cppToken L.KW_Struct
kwSwitch          = cppToken L.KW_Switch
kwTemplate        = cppToken L.KW_Template
kwThis            = cppToken L.KW_This
kwThreadLocal     = cppToken L.KW_ThreadLocal
kwThrow           = cppToken L.KW_Throw
kwTrue            = cppToken L.KW_True
kwTry             = cppToken L.KW_Try
kwTypedef         = cppToken L.KW_Typedef
kwTypeid          = cppToken L.KW_Typeid
kwTypename        = cppToken L.KW_Typename
kwUnion           = cppToken L.KW_Union
kwUnsigned        = cppToken L.KW_Unsigned
kwUsing           = cppToken L.KW_Using
kwVirtual         = cppToken L.KW_Virtual
kwVoid            = cppToken L.KW_Void
kwVolatile        = cppToken L.KW_Volatile
kwWCharT          = cppToken L.KW_WCharT
kwWhile           = cppToken L.KW_While

leftBrace           = cppToken L.Punc_LeftBrace
rightBrace          = cppToken L.Punc_RightBrace
leftBracket         = cppToken L.Punc_LeftBracket
rightBracket        = cppToken L.Punc_RightBracket
hash                = cppToken L.Punc_Hash
doubleHash          = cppToken L.Punc_DoubleHash
leftParen           = cppToken L.Punc_LeftParen
rightParen          = cppToken L.Punc_RightParen
semi                = cppToken L.Punc_Semi
colon               = cppToken L.Punc_Colon
questionMark        = cppToken L.Punc_QuestionMark
doubleColon         = cppToken L.Punc_DoubleColon
dot                 = cppToken L.Punc_Dot
threeDot            = cppToken L.Punc_ThreeDot
--opNew               = cppToken L.Op_
--opDelete            = cppToken L.Op_
opDotPtr            = cppToken L.Op_DotPtr
opPlus              = cppToken L.Op_Plus
opMinus             = cppToken L.Op_Minus
opMul               = cppToken L.Op_Mul
opDiv               = cppToken L.Op_Div
opRem               = cppToken L.Op_Rem
opXor               = cppToken L.Op_Xor
opAnd               = cppToken L.Op_And
opOr                = cppToken L.Op_Or
opTilda             = cppToken L.Op_Tilda
opNot               = cppToken L.Op_Not
opAssign            = cppToken L.Op_Assign
opLess              = cppToken L.Op_Less
opGreater           = cppToken L.Op_Greater
opAssignPlus        = cppToken L.Op_AssignPlus
opAssignMinus       = cppToken L.Op_AssignMinus
opAssignMul         = cppToken L.Op_AssignMul
opAssignDiv         = cppToken L.Op_AssignDiv
opAssignRem         = cppToken L.Op_AssignRem
opAssignXor         = cppToken L.Op_AssignXor
opAssignAnd         = cppToken L.Op_AssignAnd
opAssignOr          = cppToken L.Op_AssignOr
opLeftShift         = cppToken L.Op_LeftShift
opRightShift        = cppToken L.Op_RightShift
opAssignLeftShift   = cppToken L.Op_AssignLeftShift
opAssignRightShift  = cppToken L.Op_AssignRightShift
opEq                = cppToken L.Op_Eq
opNotEq             = cppToken L.Op_NotEq
opLessEq            = cppToken L.Op_LessEq
opGreaterEq         = cppToken L.Op_GreaterEq
opLogicalAnd        = cppToken L.Op_LogicalAnd
opLogicalOr         = cppToken L.Op_LogicalOr
opIncrement         = cppToken L.Op_Increment
opDecrement         = cppToken L.Op_Decrement
comma               = cppToken L.Punc_Comma
opArrowPtr          = cppToken L.Op_Arrow
opArrow             = cppToken L.Op_ArrowPtr

ppIf                = cppToken L.PP_If
ppIfdef             = cppToken L.PP_Ifdef
ppIfndef            = cppToken L.PP_Ifndef
ppElif              = cppToken L.PP_Elif
ppElse              = cppToken L.PP_Else
ppEndif             = cppToken L.PP_Endif
ppInclude           = cppToken L.PP_Include
ppDefine            = cppToken L.PP_Define
ppUndef             = cppToken L.PP_Undef
ppLine              = cppToken L.PP_Line
ppError             = cppToken L.PP_Error
ppPragma            = cppToken L.PP_Pragma

eol                 = cppToken L.EOL

integerLiteral      = cppToken' (\t -> case t of
                                        L.Literal_Integer s     -> Just s
                                        _                       -> Nothing)
characterLiteral    = cppToken' (\t -> case t of
                                        L.Literal_Char s        -> Just s
                                        _                       -> Nothing)
floatingLiteral     = cppToken' (\t -> case t of
                                        L.Literal_Float s       -> Just s
                                        _                       -> Nothing)
stringLiteral       = cppToken' (\t -> case t of
                                        L.Literal_String s      -> Just s
                                        _                       -> Nothing)
booleanLiteral      = cppToken' (\t -> case t of
                                        L.Literal_Boolean s     -> Just s
                                        _                       -> Nothing)
pointerLiteral      = cppToken' (\t -> case t of
                                        L.Literal_NullPtr s     -> Just s
                                        _                       -> Nothing)
userDefinedLiteral  = cppToken' (\t -> case t of
                                        L.Literal_UserDefined s -> Just s
                                        _                       -> Nothing)

ident = cppToken' (\t -> case t of
                            L.Id s -> Just s
                            _      -> Nothing)

