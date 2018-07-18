module Language.CPlusPlus where

import           Text.Parsec

type P = Parsec String ()

type Literal = String

data CLiteral
  = IntegerLiteral String
  | CharacterLiteral String
  | FloatingLiteral String
  | StringLiteral String
  | BooleanLiteral Bool
  | NullPtr
  | UserDefinedLiteral String
  deriving (Show, Eq)

data Name
  = Id String
  | TypeName Type
  | ConversionFunctionId TypeId
  | OperatorFunctionId String
  | OperatorFunctionTemplateId String [TemplateArgument]
  | LiteralOperatorId String
  | SimpleTemplateId String [TemplateArgument]
  | LiteralOperatorTemplateId String [TemplateArgument]
  deriving (Show, Eq)

data Expression
  = LiteralExpr Literal
  | This
  | UnqualifiedId Name
  | Destructor Name
  | DeclTypeDestructor Type
  | QualifiedId [Name] Name
  | LambdaExpr [Capture]
               (Maybe Declarator)
               Statement
  | GetByIndex Expression
               Expression
  | FunctionCallExpr Expression
                     [Expression]
  | InitExpr Type
             [Expression]
  | AccessByRef String
                Expression
  | AccessByPtr String
                Expression
  | PseudoDestructor [Name]
                     Expression
  | UnaryOperatorCall UnaryOperator
                      Expression
  | DynamicCast TypeId
                Expression
  | StaticCast TypeId
               Expression
  | ReinterpretCast TypeId
                    Expression
  | ConstCast TypeId
              Expression
  | TypeIdOfExpr Expression
  | TypeIdOfType TypeId
  | SizeOfE Expression
  | SizeOfT TypeId
  | SizeOfI String
  | AlignOf TypeId
  | NoExcept Expression
  | DeleteExpr Expression
  | NewExpr [Expression]
            TypeId
            [Expression]
  | NewWithNewType [Expression] TypeId [Expression]
  | CastExpr TypeId
             Expression
  | PtrByRef Expression
             Expression
  | PtrByPtr Expression
             Expression
  | BinaryOperation BinaryOperator
                    Expression
                    Expression
  | ConditionalExpr Expression
                    Expression
                    Expression
  | Assign Expression
           Expression
  | AssignMul Expression
              Expression
  | AssignDiv Expression
              Expression
  | AssignRem Expression
              Expression
  | AssignAdd Expression
              Expression
  | AssignSub Expression
              Expression
  | AssignLShift Expression
                 Expression
  | AssignRShift Expression
                 Expression
  | AssignAnd Expression
              Expression
  | AssignXor Expression
              Expression
  | AssignOr Expression
             Expression
  | CommaExpr Expression
              Expression
  | BracedInitList [Expression]
  | Throw (Maybe Expression)
  deriving (Show, Eq)

data UnaryOperator
  = IncrementPost
  | DecrementPost
  | IncrementPref
  | DecrementPref
  | UnaryPlus
  | UnaryMinus
  | Not
  | BitNot
  | Ref
  | Indirection
  deriving (Show, Eq)

data BinaryOperator
  = Add
  | Sub
  | Mul
  | Div
  | Rem
  | LeftShift
  | RightShift
  | Less
  | LessOrEq
  | Greater
  | GreaterOrEq
  | Equal
  | NotEqual
  | And
  | ExclusiveOr
  | InclusiveOr
  | LogicalAnd
  | LogicalOr
  deriving (Show, Eq)

data Statement
  = Labeled [AttributeSpecifier]
            String
            Statement
  | Case [AttributeSpecifier]
         Expression
         Statement
  | Default [AttributeSpecifier]
            Statement
  | ExpressionStatement (Maybe Expression)
  | Compound [Statement]
  | IfStatement Condition
                Statement
  | IfElseStatement Condition
                    Statement
                    Statement
  | Switch Condition
           Statement
  | While Condition
          Statement
  | DoWhile Expression
            Statement
  | For { forInitializer :: Statement
        , forCondition :: Maybe Condition
        , forModifier :: Maybe Expression
        , forBody :: Statement }
  | ForRanged Statement Expression Statement
  | Break
  | Continue
  | Return (Maybe Expression)
  | Goto String
  | DeclarationStatement Declaration
  | TryBlock Statement [Handler]
  deriving (Show, Eq)

data AttributeSpecifier
  = AttributeList [Attribute]
  | AlignmentSpecifierType TypeId (Maybe String)
  | AlignmentSpecifierExpr Expression (Maybe String)
  deriving (Show, Eq)

data Declaration
  = SimpleDecl [AttributeSpecifier] [DeclSpecifier] [InitDeclarator]
  | ForRangedDeclaration [AttributeSpecifier] [DeclSpecifier] Declarator
  | Alias String TypeId
  | StaticAssert Expression Literal
  | EmptyDecl
  | AttributeDecl [AttributeSpecifier]
  | OpaqueEnum
    { opaqueEnumKey        :: EnumKey
    , opaqueEnumAttributes :: [AttributeSpecifier]
    , opaqueEnumIdentifier :: String
    , opaqueEnumBase       :: [DeclSpecifier]
    }
  | NamespaceDeclaration
    { namespaceInline       :: Maybe String
    , namespaceIdentifier   :: Maybe String
    , namespaceDeclarations :: [Declaration]
    }
  | NamespaceAlias String [Name] String
  | UsingDeclaration [Name] Expression
  | UsingDirective [AttributeSpecifier] [Name] String
  | AsmDefinition Literal
  | ExplicitSpecialization Declaration
  | LinkageSpecification Literal [Declaration]
  | FunctionDefinition [AttributeSpecifier] [DeclSpecifier] Declarator Statement
  | DefaultFunctionDefinition [AttributeSpecifier] [DeclSpecifier] Declarator
  | DeleteFunctionDefinition [AttributeSpecifier] [DeclSpecifier] Declarator
  | TemplateDeclaration [TemplateParameter] Declaration
  deriving (Show, Eq)

newtype TranslationUnit = TranslationUnit [Declaration] deriving (Show, Eq)

---------------------------------------------
-- Lexer
---------------------------------------------
hexQuad :: P String
hexQuad = do
  d1 <- hexDigit
  d2 <- hexDigit
  d3 <- hexDigit
  d4 <- hexDigit
  return [d1, d2, d3, d4]

universalCharacterName = do
  c <- char '\\'
  u <- oneOf "uU"
  quad <- hexQuad
  return $ c : u : quad

preprocessingToken =
  choice
    [ headerName
    , identifier
    , ppNumber
    , characterLiteral
    , user_defined_character_literal
    , stringLiteral
    , user_defined_string_literal
    , preprocessingOpOrPunc
    ]

cppToken = choice [identifier, keyword, literal, preprocessingOpOrPunc] --operator_token, punctuator]

headerName = angles sourceCharSeq <|> quotes sourceCharSeq

sourceCharSeq :: P String
sourceCharSeq = many1 $ nondigit <|> digit

nondigit = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"

identifier :: P String
identifier = do
  startC <- nondigit
  str <- many $ nondigit <|> digit
  let id = startC : str
  if elem id keywords
    then unexpected "keyword"
    else return id

ppNumber = choice [expNumber, number, digits]

digits :: P String
digits = many1 digit

number :: P String
number = do
  d <- many digit
  dot <- char '.'
  d' <- many digit
  return $ d ++ [dot] ++ d'

expNumber = do
  n <- number
  e <- oneOf "eE"
  s <- sign
  return $ n ++ [e] ++ s

kwAlignas = reserved "alignas"

kwAlignof = reserved "alignof"

kwAsm = reserved "asm"

kwAuto = reserved "auto"

kwBool = reserved "bool"

kwBreak = reserved "break"

kwCase = reserved "case"

kwCatch = reserved "catch"

kwChar = reserved "char"

kwChar16T = reserved "char16_t"

kwChar32T = reserved "char32_t"

kwClass = reserved "class"

kwConst = reserved "const"

kwConstexpr = reserved "constexpr"

kwConstCast = reserved "const_cast"

kwContinue = reserved "continue"

kwDecltype = reserved "decltype"

kwDefault = reserved "default"

kwDelete = reserved "delete"

kwDo = reserved "do"

kwDouble = reserved "double"

kwDynamicCast = reserved "dynamic_cast"

kwElse = reserved "else"

kwEnum = reserved "enum"

kwExplicit = reserved "explicit"

kwExport = reserved "export"

kwExtern = reserved "extern"

kwFalse = reserved "false"

kwFloat = reserved "float"

kwFor = reserved "for"

kwFriend = reserved "friend"

kwGoto = reserved "goto"

kwIf = reserved "if"

kwInline = reserved "inline"

kwInt = reserved "int"

kwLong = reserved "long"

kwMutable = reserved "mutable"

kwNamespace = reserved "namespace"

kwNew = reserved "new"

kwNoexcept = reserved "noexcept"

kwNullptr = reserved "nullptr"

kwOperator = reserved "operator"

kwPrivate = reserved "private"

kwProtected = reserved "protected"

kwPublic = reserved "public"

kwRegister = reserved "register"

kwReinterpretCast = reserved "reinterpret_cast"

kwReturn = reserved "return"

kwShort = reserved "short"

kwSigned = reserved "signed"

kwSizeof = reserved "sizeof"

kwStatic = reserved "static"

kwStaticAssert = reserved "static_assert"

kwStaticCast = reserved "static_cast"

kwStruct = reserved "struct"

kwSwitch = reserved "switch"

kwTemplate = reserved "template"

kwThis = reserved "this"

kwThreadLocal = reserved "thread_local"

kwThrow = reserved "throw"

kwTrue = reserved "true"

kwTry = reserved "try"

kwTypedef = reserved "typedef"

kwTypeid = reserved "typeid"

kwTypename = reserved "typename"

kwUnion = reserved "union"

kwUnsigned = reserved "unsigned"

kwUsing = reserved "using"

kwVirtual = reserved "virtual"

kwVoid = reserved "void"

kwVolatile = reserved "volatile"

kwWCharT = reserved "wchar_t"

kwWhile = reserved "while"

keywords =
  [ "alignas"
  , "alignof"
  , "asm"
  , "auto"
  , "bool"
  , "break"
  , "case"
  , "catch"
  , "char"
  , "char16_t"
  , "char32_t"
  , "class"
  , "const"
  , "constexpr"
  , "const_cast"
  , "continue"
  , "decltype"
  , "default"
  , "delete"
  , "do"
  , "double"
  , "dynamic_cast"
  , "else"
  , "enum"
  , "explicit"
  , "export"
  , "extern"
  , "false"
  , "float"
  , "for"
  , "friend"
  , "goto"
  , "if"
  , "inline"
  , "int"
  , "long"
  , "mutable"
  , "namespace"
  , "new"
  , "noexcept"
  , "nullptr"
  , "operator"
  , "private"
  , "protected"
  , "public"
  , "register"
  , "reinterpret_cast"
  , "return"
  , "short"
  , "signed"
  , "sizeof"
  , "static"
  , "static_assert"
  , "static_cast"
  , "struct"
  , "switch"
  , "template"
  , "this"
  , "thread_local"
  , "throw"
  , "true"
  , "try"
  , "typedef"
  , "typeid"
  , "typename"
  , "union"
  , "unsigned"
  , "using"
  , "virtual"
  , "void"
  , "volatile"
  , "wchar_t"
  , "while"
  ]

keyword =
  choice
    [ kwAlignas
    , kwAlignof
    , kwAsm
    , kwAuto
    , kwBool
    , kwBreak
    , kwCase
    , kwCatch
    , kwChar
    , kwChar16T
    , kwChar32T
    , kwClass
    , kwConst
    , kwConstexpr
    , kwConstCast
    , kwContinue
    , kwDecltype
    , kwDefault
    , kwDelete
    , kwDo
    , kwDouble
    , kwDynamicCast
    , kwElse
    , kwEnum
    , kwExplicit
    , kwExport
    , kwExtern
    , kwFalse
    , kwFloat
    , kwFor
    , kwFriend
    , kwGoto
    , kwIf
    , kwInline
    , kwInt
    , kwLong
    , kwMutable
    , kwNamespace
    , kwNew
    , kwNoexcept
    , kwNullptr
    , kwOperator
    , kwPrivate
    , kwProtected
    , kwPublic
    , kwRegister
    , kwReinterpretCast
    , kwReturn
    , kwShort
    , kwSigned
    , kwSizeof
    , kwStatic
    , kwStaticAssert
    , kwStaticCast
    , kwStruct
    , kwSwitch
    , kwTemplate
    , kwThis
    , kwThreadLocal
    , kwThrow
    , kwTrue
    , kwTry
    , kwTypedef
    , kwTypeid
    , kwTypename
    , kwUnion
    , kwUnsigned
    , kwUsing
    , kwVirtual
    , kwVoid
    , kwVolatile
    , kwWCharT
    , kwWhile
    ]

leftBrace    = string "{"
rightBrace   = string "}"
leftBracket  = string "["
rightBracket = string "]"
hash         = string "#"
doubleHash   = string "##"
leftParen    = string "("
rightParen   = string ")"
--op         = string "<:"
--op         = string ":>"
--op         = string "<%"
--op         = string "%>"
--op         = string "%:"
--op         = string "%:%:"
semi         = string ";"
colon        = string ":"
threeDot     = string "..."
opNew        = string "new"
opDelete     = string "delete"
questionMark = string "?"
doubleColon  = string "::"
dot          = string "."
opDotPtr     = string ".*"
opPlus       = string "+"
opMinus      = string "-"
opStar       = string "*"
opDiv        = string "/"
opRem        = string "%"
opXor        = string "^"
opAnd        = string "&"
opOr         = string "|"
opTilda      = string "~"
opNot        = string "!"
opAssign     = string "="
opLess       = string "<"
opGreater    = string ">"
opAssignPlus = string "+="
opAssignMinus = string "-="
opAssignMul  = string "*="
opAssignDiv  = string "/="
opAssignRem  = string "%="
opAssignXor  = string "^="
opAssignAnd  = string "&="
opAssignOr   = string "|="
opLeftShift  = string "<<"
opRightShift = string ">>"
opAssignLeftShift  = string "<<="
opAssignRightShift = string ">>="
opEq               = string "=="
opNotEq            = string "!="
opLessEq           = string "<="
opGreaterEq        = string ">="
opLogicalAnd       = string "&&"
opLogicalOr        = string "||"
opIncrement        = string "++"
opDecrement        = string "--"
comma              = string ","
opArrowPtr         = string "->*"
opArrow            = string "->"
litAnd             = string "and"
litAndEq           = string "and_eq"
litBitAnd          = string "bitand"
litBitOr           = string "bitor"
litCompl           = string "compl"
litNot             = string "not"
litNotEq           = string "not_eq"
litOr              = string "or"
litOrEq            = string "or_eq"
litXor             = string "xor"
litXorEq           = string "xor_eq"

preprocessingOpOrPunc :: P String
preprocessingOpOrPunc =
  choice
    [ leftBrace
    , rightBrace
    , leftBracket
    , rightBracket
    , hash
    , doubleHash
    , leftParen
    , rightParen
    , string "<:"
    , string ":>"
    , string "<%"
    , string "%>"
    , string "%:"
    , string "%:%:"
    , semi
    , colon
    , threeDot
    , opNew
    , opDelete
    , questionMark
    , doubleColon
    , dot
    , opDotPtr
    , opPlus
    , opMinus
    , opStar
    , opDiv
    , opRem
    , opXor
    , opAnd
    , opOr
    , opTilda
    , opNot
    , opAssign
    , opLess
    , opGreater
    , opAssignPlus
    , opAssignMinus
    , opAssignMul
    , opAssignDiv
    , opAssignRem
    , opAssignXor
    , opAssignAnd
    , opAssignOr
    , opLeftShift
    , opRightShift
    , opAssignLeftShift
    , opAssignRightShift
    , opEq
    , opNotEq
    , opLessEq
    , opGreaterEq
    , opLogicalAnd
    , opLogicalOr
    , opIncrement
    , opDecrement
    , comma
    , opArrowPtr
    , opArrow
    , litAnd
    , litAndEq
    , litBitAnd
    , litBitOr
    , litCompl
    , litNot
    , litNotEq
    , litOr
    , litOrEq
    , litXor
    , litXorEq
    ]

literal :: P Literal
literal =
  choice
    [ integerLiteral
    , characterLiteral
    , floatingLiteral
    , stringLiteral
    , booleanLiteral
    , pointerLiteral
    , userDefinedLiteral
    ]

integerLiteral =
  (do lit <- decimalLiteral
      suf <- option "" integerSuffix
      return $ lit ++ suf) <|>
  (do lit <- octalLiteral
      suf <- option "" integerSuffix
      return $ lit ++ suf) <|>
  (do lit <- hexadecimalLiteral
      suf <- option "" integerSuffix
      return $ lit ++ suf)

nonzeroDigit = oneOf "123456789"

decimalLiteral = do
  start <- nonzeroDigit
  ds <- many digit
  return $ start : ds

octalLiteral = do
  start <- char '0'
  ds <- many1 octDigit
  return $ start : ds

hexadecimalLiteral = do
  start <- string "0x"
  ds <- many1 hexDigit
  return $ start ++ ds

integerSuffix =
  (do ll <- longLongSuffix
      u <- option "" unsignedSuffix
      return $ ll ++ u) <|>
  (do l <- longSuffix
      u <- option "" unsignedSuffix
      return $ l ++ u) <|>
  (do u <- unsignedSuffix
      ll <- option "" longLongSuffix
      return $ u ++ ll) <|>
  (do u <- unsignedSuffix
      l <- option "" longSuffix
      return (u ++ l))

unsignedSuffix = string "u" <|> string "U"

longSuffix = string "l" <|> string "L"

longLongSuffix = string "ll" <|> string "LL"

characterLiteral = do
  pref <- option "" $ string "u " <|> string "U " <|> string "L "
  cs <- single_quotes $ many1 char_seq
  return $ foldl (++) pref cs

char_seq :: P String
char_seq = sourceCharSeq <|> escape_seq <|> universalCharacterName

escape_seq = simple_escape_seq <|> octal_escape_seq <|> hexadecimal_escape_seq

simple_escape_seq =
  choice
    [ string "\\\'"
    , string "\\\""
    , string "\\?"
    , string "\\\\"
    , string "\\a"
    , string "\\b"
    , string "\\f"
    , string "\\n"
    , string "\\r"
    , string "\\t"
    , string "\\v"
    ]

octal_escape_seq = do
  c <- char '\\'
  ds <- many1 octDigit
  return $ c : ds

hexadecimal_escape_seq = do
  pref <- string "\\x"
  ds <- many1 hexDigit
  return $ pref ++ ds

floatingLiteral =
  (do f <- fractional_constant
      ex <- option "" exponent_part
      suff <- option "" floating_suffix
      return $ f ++ ex ++ suff) <|>
  (do ds <- digits
      ex <- exponent_part
      suff <- option "" floating_suffix
      return $ ds ++ ex ++ suff)

fractional_constant =
  (do ds <- many digit
      dot <- char '.'
      ds' <- digits
      return $ ds ++ [dot] ++ ds') <|>
  (do ds <- digits
      dot <- char '.'
      return $ ds ++ [dot])

exponent_part = do
  e <- oneOf "eE"
  s <- option "" sign
  ds <- digits
  return $ [e] ++ s ++ ds

sign = string "+" <|> string "-"

floating_suffix = choice [string "f", string "F", string "l", string "L"]

stringLiteral =
  (do opt <- option "" encoding_prefix
      str <- quotes s_char_seq
      return $ opt ++ "\"" ++ (foldl (++) "" str) ++ "\"") <|>
  (do opt <- option "" encoding_prefix
      r <- char 'R'
      str <- raw_string
      return $ opt ++ "\"" ++ [r] ++ "\"" ++ str)

encoding_prefix = string "u8" <|> string "u" <|> string "U" <|> string "L"

s_char_seq = many s_char

s_char = universalCharacterName <|> escape_seq <|> (many $ noneOf "\n\\")

raw_string =
  quotes $ do
    d1 <- option "" d_char_seq
    lp <- leftParen
    r <- option "" r_char_seq
    rp <- rightParen
    d2 <- option "" d_char_seq
    return $ d1 ++ lp ++ r ++ rp ++ d2

r_char_seq = many r_char

r_char = noneOf ")"

d_char_seq = many d_char

d_char = noneOf " ()\\\t\v\n"

booleanLiteral = kwTrue <|> kwFalse

pointerLiteral = kwNullptr

userDefinedLiteral =
  user_defined_integer_literal <|>
  user_defined_floating_literal <|>
  user_defined_string_literal <|>
  user_defined_character_literal

user_defined_integer_literal =
  (do d <- decimalLiteral
      suff <- udSuffix
      return $ d ++ suff) <|>
  (do o <- octalLiteral
      suff <- udSuffix
      return $ o ++ suff) <|>
  (do h <- hexadecimalLiteral
      suff <- udSuffix
      return $ h ++ suff)

user_defined_floating_literal =
  (do fr <- fractional_constant
      e <- option "" exponent_part
      suff <- udSuffix
      return $ fr ++ e ++ suff) <|>
  (do ds <- digits
      e <- exponent_part
      suff <- udSuffix
      return $ ds ++ e ++ suff)

user_defined_string_literal = do
  str <- stringLiteral
  suff <- udSuffix
  return $ str ++ suff

user_defined_character_literal = do
  ch <- characterLiteral
  suff <- udSuffix
  return $ ch ++ suff

udSuffix = identifier

---------------------------------------------------------------------------
-- Basic
---------------------------------------------------------------------------
translationUnit :: P TranslationUnit
translationUnit = do
  ds <- declaration_seq
  return $ TranslationUnit ds

---------------------------------------------------------------------------
-- Expression
---------------------------------------------------------------------------
primaryExpression :: P Expression
primaryExpression =
  (do l <- literal
      return $ LiteralExpr l) <|>
  (do kwThis
      return This) <|>
  (do expr <- parens expression
      return expr) <|>
  idExpression <|>
  lambdaExpression

idExpression = qualifiedId <|> unqualifiedId

unqualifiedId =
  (do id <- identifier
      return $ UnqualifiedId (Id id)) <|>
  (do n <- operator_function_id <|> conversion_function_id <|> literal_operator_id <|> template_id
      return $ UnqualifiedId n) <|>
  (do opTilda
      id <- class_name
      return $ Destructor id) <|>
  (do opTilda
      id <- decltype_specifier
      return $ DeclTypeDestructor id)

qualifiedId =
  (do optional doubleColon
      ids <- nestedNameSpecifier
      optional kwTemplate
      UnqualifiedId id <- unqualifiedId
      return $ QualifiedId ids id) <|>
  (do doubleColon
      id <- identifier
      return $ QualifiedId [] (Id id)) <|>
  (do doubleColon
      id <- operator_function_id <|> literal_operator_id <|> template_id
      return $ QualifiedId [] id)

nestedNameSpecifier :: P [Name]
nestedNameSpecifier =
  (do t <- type_name
      doubleColon
      return [t]) <|>
  (do n <- namespace_name
      doubleColon
      return [Id n]) <|>
  (do d <- decltype_specifier
      doubleColon
      return [TypeName d]) <|>
  (do ids <- nestedNameSpecifier
      id <- identifier
      doubleColon
      return $ ids ++ [Id id]) <|>
  (do ids <- nestedNameSpecifier
      optional kwTemplate
      s <- simple_template_id
      doubleColon
      return $ ids ++ [s])

lambdaExpression = do
  intro <- lambdaIntroducer
  decl <- optionMaybe lambla_declarator
  ss <- compound_statement
  return $ LambdaExpr intro decl ss

lambdaIntroducer = brackets $ option [] lambdaCapture

data Capture
  = DefaultRefCapture
  | DefaultValueCapture
  | ThisCapture
  | IdCapture String
  | RefCapture String
  | ThreeDot deriving (Show, Eq)

lambdaCapture :: P [Capture]
lambdaCapture =
  (do def <- capture_default
      char ','
      cs <- capture_list
      return $ def : cs) <|>
  (do c <- capture_default
      return [c]) <|>
  capture_list

capture_default =
  (do string "&"
      return DefaultRefCapture) <|>
  (do opAssign
      return DefaultValueCapture)

capture_list :: P [Capture]
capture_list = do
  l <- sepBy1 capture $ char ','
  threeDot <- optionMaybe $ threeDot
  case threeDot of
    Nothing -> return l
    Just _  -> return $ l ++ [ThreeDot]

capture =
  (do kwThis
      return ThisCapture) <|>
  (do i <- identifier
      return $ IdCapture i) <|>
  (do char '&'
      id <- identifier
      return $ RefCapture id)

lambla_declarator :: P Declarator
lambla_declarator = do
  params <- parameter_declaration_clause
  m <- optionMaybe kwMutable
  e <- optionMaybe exception_specification
  as <- option [] attribute_specifier_seq
  tr <- optionMaybe trailing_return_type
  return $ LambdaDeclarator params m e as tr

postfix_expression :: P Expression
postfix_expression =
  primaryExpression <|>
  (do e <- postfix_expression
      e' <- brackets expression
      return $ GetByIndex e e') <|>
  (do e <- postfix_expression
      es <- parens expression_list
      return $ FunctionCallExpr e es) <|>
  (do id <- simple_type_specifier <|> typename_specifier
      es <- parens expression_list
      return $ InitExpr id es) <|>
  (do id <- simple_type_specifier <|> typename_specifier
      es <- bracedInitList
      return $ InitExpr id [es]) <|>
  (do e <- postfix_expression
      char '.'
      optional kwTemplate
      id <- identifier
      return $ AccessByRef id e) <|>
  (do e <- postfix_expression
      opArrow
      optional kwTemplate
      id <- identifier
      return $ AccessByPtr id e) <|>
  (do e <- postfix_expression
      dot <|> opArrow
      ids <- pseudo_destructor_name
      return $ PseudoDestructor ids e) <|>
  (do e <- postfix_expression
      string "++"
      return $ UnaryOperatorCall IncrementPost e) <|>
  (do e <- postfix_expression
      string "--"
      return $ UnaryOperatorCall DecrementPost e) <|>
  (do kwDynamicCast
      t <- angles type_id
      e <- parens expression
      return $ DynamicCast t e) <|>
  (do kwStaticCast
      t <- angles type_id
      e <- parens expression
      return $ StaticCast t e) <|>
  (do kwReinterpretCast
      t <- angles type_id
      e <- parens expression
      return $ ReinterpretCast t e) <|>
  (do kwConstCast
      t <- angles type_id
      e <- parens expression
      return $ ConstCast t e) <|>
  (do kwTypeid
      e <- parens expression
      return $ TypeIdOfExpr e) <|>
  (do kwTypeid
      id <- type_id
      return $ TypeIdOfType id)

expression_list :: P [Expression]
expression_list = initializer_list

pseudo_destructor_name :: P [Name]
pseudo_destructor_name =
  (do char '~'
      d <- decltype_specifier
      return [TypeName d]) <|>
  (do optional $ doubleColon
      ids <- option [] nestedNameSpecifier
      char '~'
      t <- type_name
      return $ ids ++ [t]) <|>
  (do optional $ doubleColon
      ids <- option [] nestedNameSpecifier
      t <- type_name
      string "::~"
      t' <- type_name
      return $ ids ++ [t, t']) <|>
  (do optional $ doubleColon
      ids <- nestedNameSpecifier
      kwTemplate
      id <- simple_template_id
      string "::~"
      t <- type_name
      return $ ids ++ [id, t])

unary_expression :: P Expression
unary_expression =
  (do string "++"
      e <- cast_expression
      return $ UnaryOperatorCall IncrementPref e) <|>
  (do string "--"
      e <- cast_expression
      return $ UnaryOperatorCall DecrementPref e) <|>
  (do string "-"
      e <- cast_expression
      return $ UnaryOperatorCall UnaryMinus e) <|>
  (do string "+"
      e <- cast_expression
      return $ UnaryOperatorCall UnaryPlus e) <|>
  (do string "!"
      e <- cast_expression
      return $ UnaryOperatorCall Not e) <|>
  (do string "~"
      e <- cast_expression
      return $ UnaryOperatorCall BitNot e) <|>
  (do string "&"
      e <- cast_expression
      return $ UnaryOperatorCall Ref e) <|>
  (do string "*"
      e <- cast_expression
      return $ UnaryOperatorCall Indirection e) <|>
  (do kwSizeof
      e <- unary_expression
      return $ SizeOfE e) <|>
  (do kwSizeof
      t <- parens type_id
      return $ SizeOfT t) <|>
  (do kwSizeof
      threeDot
      id <- parens identifier
      return $ SizeOfI id) <|>
  (do kwAlignof
      t <- parens type_id
      return $ AlignOf t) <|>
  noexcept_expression <|>
  new_expression <|>
  delete_expression <|>
  postfix_expression

noexcept_expression = do
  kwNoexcept
  e <- parens expression
  return $ NoExcept e

new_expression =
  (do optional $ doubleColon
      kwNew
      p <- option [] newPlacement
      t <- parens type_id
      es <- newInitializer
      return $ NewExpr p t es) <|>
  (do optional $ doubleColon
      kwNew
      p <- option [] newPlacement
      t <- newTypeId
      es <- option [] newInitializer
      return $ NewWithNewType p t es)

newPlacement = parens expression_list

newTypeId :: P TypeId
newTypeId = do
  ts <- typeSpecifierSeq
  d <- optionMaybe newDeclarator
  return (ts, d)

newDeclarator :: P Declarator
newDeclarator =
  (do p <- ptr_operator
      d <- optionMaybe newDeclarator
      return $ PtrNewDeclarator p d) <|>
  noptrNewDeclarator

noptrNewDeclarator =
  (do e <- brackets expression
      as <- option [] attribute_specifier_seq
      return $ NoptrNewDeclarator Nothing e as) <|>
  (do d <- noptrNewDeclarator
      e <- brackets constant_expression
      as <- attribute_specifier_seq
      return $ NoptrNewDeclarator (Just d) e as)

newInitializer :: P [Expression]
newInitializer =
  do e <- bracedInitList
     return [e]
     <|> (option [] $ parens expression_list)

delete_expression = do
  optional $ doubleColon
  kwDelete
  optional $ string "[]"
  e <- cast_expression
  return $ DeleteExpr e

cast_expression :: P Expression
cast_expression =
  (do t <- parens type_id
      e <- cast_expression
      return $ CastExpr t e) <|>
  unary_expression

pm_expression =
  (do e <- pm_expression
      string ".*"
      e' <- cast_expression
      return $ PtrByRef e e') <|>
  (do e <- pm_expression
      string "->*"
      e' <- cast_expression
      return $ PtrByPtr e e') <|>
  cast_expression

multiplicative_expression =
  (do e <- multiplicative_expression
      string "*"
      e' <- pm_expression
      return $ BinaryOperation Mul e e') <|>
  (do e <- multiplicative_expression
      string "/"
      e' <- pm_expression
      return $ BinaryOperation Div e e') <|>
  (do e <- multiplicative_expression
      string "%"
      e' <- pm_expression
      return $ BinaryOperation Rem e e') <|>
  pm_expression

additive_expression =
  (do e <- additive_expression
      string "+"
      e' <- multiplicative_expression
      return $ BinaryOperation Add e e') <|>
  (do e <- additive_expression
      string "-"
      e' <- multiplicative_expression
      return $ BinaryOperation Sub e e') <|>
  multiplicative_expression

shift_expression =
  (do e <- shift_expression
      string "<<"
      e' <- additive_expression
      return $ BinaryOperation LeftShift e e') <|>
  (do e <- shift_expression
      string ">>"
      e' <- additive_expression
      return $ BinaryOperation RightShift e e') <|>
  additive_expression

relational_expression =
  (do e <- relational_expression
      opLess
      e' <- shift_expression
      return $ BinaryOperation Less e e') <|>
  (do e <- relational_expression
      string "<="
      e' <- shift_expression
      return $ BinaryOperation LessOrEq e e') <|>
  (do e <- relational_expression
      opGreater
      e' <- shift_expression
      return $ BinaryOperation Greater e e') <|>
  (do e <- relational_expression
      string ">="
      e' <- shift_expression
      return $ BinaryOperation GreaterOrEq e e') <|>
  shift_expression

equality_expression =
  (do e <- equality_expression
      string "=="
      e' <- relational_expression
      return $ BinaryOperation Equal e e') <|>
  (do e <- equality_expression
      string "!="
      e' <- relational_expression
      return $ BinaryOperation NotEqual e e') <|>
  relational_expression

and_expression =
  (do e <- and_expression
      string "&"
      e' <- equality_expression
      return $ BinaryOperation And e e') <|>
  equality_expression

exclusive_or_expression =
  (do e <- exclusive_or_expression
      string "^"
      e' <- and_expression
      return $ BinaryOperation ExclusiveOr e e') <|>
  and_expression

inclusive_or_expression =
  (do e <- inclusive_or_expression
      string "|"
      e' <- exclusive_or_expression
      return $ BinaryOperation InclusiveOr e e') <|>
  exclusive_or_expression

logical_and_expression =
  (do e <- logical_and_expression
      string "&&"
      e' <- inclusive_or_expression
      return $ BinaryOperation LogicalAnd e e') <|>
  inclusive_or_expression

logical_or_expression =
  (do e <- logical_or_expression
      string "||"
      e' <- logical_and_expression
      return $ BinaryOperation LogicalOr e e') <|>
  logical_and_expression

conditional_expression =
  (do cond <- logical_or_expression
      string "?"
      t <- expression
      colon
      f <- assignment_expression
      return $ ConditionalExpr cond t f) <|>
  logical_or_expression

assignment_expression =
  throw_expression <|>
  (do e <- logical_or_expression
      op <- assignment_operator
      init <- initializerClause
      case op of
        "="   -> return $ Assign e init
        "*="  -> return $ AssignMul e init
        "/="  -> return $ AssignDiv e init
        "%="  -> return $ AssignRem e init
        "+="  -> return $ AssignAdd e init
        "-="  -> return $ AssignSub e init
        "<<=" -> return $ AssignLShift e init
        ">>=" -> return $ AssignRShift e init
        "&="  -> return $ AssignAnd e init
        "^="  -> return $ AssignXor e init
        "|="  -> return $ AssignOr e init) <|>
  conditional_expression

assignment_operator =
  choice
    [ opAssign
    , string "*="
    , string "/="
    , string "%="
    , string "+="
    , string "-="
    , string "<<="
    , string ">>="
    , string "&="
    , string "^="
    , string "|="
    ]

expression :: P Expression
expression =
  (do e <- expression
      comma
      e' <- assignment_expression
      return $ CommaExpr e e') <|>
  assignment_expression

constant_expression = conditional_expression

-------------------------------------------------------------------
-- Statements
-------------------------------------------------------------------
statement :: P Statement
statement =
  choice
    [ labeled_statement
    , expression_statement
    , compound_statement
    , selection_statement
    , iteration_statement
    , jump_statement
    , declaration_statement
    , try_block
    ]

labeled_statement =
  (do attrs <- option [] attribute_specifier_seq
      i <- identifier
      colon
      s <- statement
      return $ Labeled attrs i s) <|>
  (do attrs <- option [] attribute_specifier_seq
      kwCase
      e <- constant_expression
      colon
      s <- statement
      return $ Case attrs e s) <|>
  (do attrs <- option [] attribute_specifier_seq
      kwDefault
      colon
      s <- statement
      return $ Default attrs s)

expression_statement = do
  e <- option Nothing $ do {e' <- expression; return $ Just e'}
  semi
  return $ ExpressionStatement e

compound_statement = do
  ss <- braces $ many statement
  return $ Compound ss

selection_statement =
  (do kwIf
      cond <- parens condition
      s <- statement
      kwElse
      s' <- statement
      return $ IfElseStatement cond s s') <|>
  (do kwIf
      cond <- parens condition
      s <- statement
      return $ IfStatement cond s) <|>
  (do kwSwitch
      cond <- parens condition
      s <- statement
      return $ Switch cond s)

data Condition = Condition Expression | InitializerCondition deriving (Show, Eq)

-- TODO complete condition
condition :: P Condition
condition =
  (do e <- expression
      return $ Condition e)
  -- <|> (do ...)

iteration_statement :: P Statement
iteration_statement =
  (do kwWhile
      c <- parens condition
      s <- statement
      return $ While c s) <|>
  (do kwDo
      s <- statement
      kwWhile
      e <- parens expression
      return $ DoWhile e s) <|>
  (do kwFor
      leftParen
      i <- forInitStatement
      c <- optionMaybe condition
      semi
      e <- optionMaybe expression
      rightParen
      body <- statement
      return $ For i c e body) <|>
  (do kwFor
      leftParen
      r <- for_range_declaration
      colon
      rr <- for_range_initializer
      rightParen
      s <- statement
      return $ ForRanged r rr s)

forInitStatement = expression_statement <|>
  (do s <- simple_declaration
      return $ DeclarationStatement s)

for_range_declaration = do
  as <- option [] attribute_specifier_seq
  t <- typeSpecifierSeq
  d <- declarator
  return $ DeclarationStatement $ ForRangedDeclaration as t d

for_range_initializer = expression
  <|> bracedInitList

jump_statement =
  (do kwBreak
      semi
      return Break) <|>
  (do kwContinue
      semi
      return Continue) <|>
  (do kwReturn
      e <- optionMaybe $ expression <|> bracedInitList
      semi
      return $ Return e) <|>
  (do kwGoto
      i <- identifier
      semi
      return $ Goto i)

declaration_statement = do
  d <- block_declaration
  return $ DeclarationStatement d

-------------------------------------------------------------------
-- Declarations
-------------------------------------------------------------------

declaration_seq = many1 declaration

declaration :: P Declaration
declaration =
  choice
    [ block_declaration
    , function_definition
    , template_declaration
    , explicit_instantiation
    , explicit_specialization
    , linkage_specification
    , namespace_definition
    , empty_declaration
    , attribute_declaration
    ]

block_declaration =
  choice
    [ simple_declaration
    , asm_definition
    , namespace_alias_definition
    , using_declaration
    , using_directive
    , static_assert_declaration
    , alias_declaration
    , opaque_enum_declaration
    ]

alias_declaration = do
  kwUsing
  i <- identifier
  opAssign
  t <- type_id
  semi
  return $ Alias i t

simple_declaration :: P Declaration
simple_declaration = do
  as <- option [] attribute_specifier_seq
  ds <- decl_specifier_seq
  is <- init_declarator_list
  semi
  return $ SimpleDecl as ds is

static_assert_declaration = do
  kwStaticAssert
  leftParen
  e <- constant_expression
  comma
  s <- stringLiteral
  rightParen
  semi
  return $ StaticAssert e s

empty_declaration = do
  semi
  return EmptyDecl

attribute_declaration = do
  attr <- attribute_specifier_seq
  semi
  return $ AttributeDecl attr

data DeclSpecifier
  = StorageClass String
  | FuncSpecifier String
  | Friend
  | Typedef
  | ConstExpr
  | Const
  | Volatile
  | SimpleTypeSpecifier Type
  | ElaboratedTypeSpecifier [AttributeSpecifier]
                            [Name]
                            String
  | ElaboratedTemplateSpecifier [Name]
                                Name
  | ElaboratedEnumSpecifier [Name]
                            String
  | CVSpecifier
  | EnumSpecifier { enumKey :: EnumKey
                  , enumAttributes :: [AttributeSpecifier]
                  , enumIdentifier :: Maybe String
                  , enumBase :: [DeclSpecifier]
                  , enumerators :: [Enumerator] }
  | NestedEnumSpecifier { nestedEnumKey :: EnumKey
                        , nestedEnumAttributes :: [AttributeSpecifier]
                        , nestedNameEnum :: [Name]
                        , nestedEnumIdentifier :: String
                        , nestedEnumBase :: [DeclSpecifier]
                        , nestedEnumerators :: [Enumerator] }
  | ClassSpecifier
  deriving (Show, Eq)

--TODO add attributes
decl_specifier_seq = many1 decl_specifier

decl_specifier :: P DeclSpecifier
decl_specifier =
  storage_class_specifier <|>
  type_specifier <|>
  function_specifier <|>
  (do kwFriend
      return Friend) <|>
  (do kwTypedef
      return Typedef) <|>
  (do kwConstexpr
      return ConstExpr)

storage_class_specifier =
  (do i <- kwAuto
      return $ StorageClass i ) <|>
  (do i <- kwRegister
      return $ StorageClass i ) <|>
  (do i <- kwStatic
      return $ StorageClass i ) <|>
  (do i <- kwThreadLocal
      return $ StorageClass i ) <|>
  (do i <- kwExtern
      return $ StorageClass i )  <|>
  (do i <- kwMutable
      return $ StorageClass i )

function_specifier =
  (do i <- kwInline
      return $ FuncSpecifier i ) <|>
  (do i <- kwVirtual
      return $ FuncSpecifier i ) <|>
  (do i <- kwExplicit
      return $ FuncSpecifier i )

-------------------------------------------------------
-- Types
-------------------------------------------------------

data Type
  = AutoT
  | NestedType [Name] Name
  | NestedTemplate [Name] Name
  | CharT
  | Char16T
  | Char32T
  | WCharT
  | BoolT
  | ShortT
  | IntT
  | LongT
  | Signed
  | Unsigned
  | FloatT
  | DoubleT
  | VoidT
  | DeclType Expression
  deriving (Show, Eq)

typedef_name :: P Name
typedef_name = do
  i <- identifier
  return $ Id i

type_specifier :: P DeclSpecifier
type_specifier =
  trailing_type_specifier <|> class_specifier <|> enum_specifier

trailing_type_specifier :: P DeclSpecifier
trailing_type_specifier =
  (do t <- simple_type_specifier
      return $ SimpleTypeSpecifier t ) <|>
  elaborated_type_specifier <|>
  typename_specifier <|>
  cv_qualifier

typeSpecifierSeq :: P [DeclSpecifier]
typeSpecifierSeq = many1 type_specifier

typename_specifier_seq :: P [String]
typename_specifier_seq = many1 typename_specifier

-- TODO add attributes
trailing_type_specifier_seq :: P [DeclSpecifier]
trailing_type_specifier_seq = many1 trailing_type_specifier

simple_type_specifier :: P Type
simple_type_specifier =
  (do optional $ doubleColon
      n <- option [] nestedNameSpecifier
      t <- type_name
      return $ NestedType n t) <|>
  (do optional $ doubleColon
      n <- nestedNameSpecifier
      kwTemplate
      s <- simple_template_id
      return $ NestedTemplate n s) <|>
  (do kwChar
      return CharT) <|>
  (do kwChar16T
      return Char16T) <|>
  (do kwChar32T
      return Char32T) <|>
  (do kwWCharT
      return WCharT) <|>
  (do kwBool
      return BoolT) <|>
  (do kwShort
      return ShortT) <|>
  (do kwInt
      return IntT) <|>
  (do kwLong
      return LongT) <|>
  (do kwSigned
      return Signed) <|>
  (do kwUnsigned
      return Unsigned) <|>
  (do kwFloat
      return FloatT) <|>
  (do kwDouble
      return DoubleT) <|>
  (do kwVoid
      return VoidT) <|>
  (do kwAuto
      return AutoT) <|>
  decltype_specifier

type_name :: P Name
type_name = class_name <|> enum_name <|> typedef_name <|> simple_template_id

decltype_specifier = do
  kwDecltype
  e <- parens expression
  return $ DeclType e

elaborated_type_specifier :: P DeclSpecifier
elaborated_type_specifier =
  (do class_key
      attrs <- option [] attribute_specifier_seq
      optional $ doubleColon
      n <- option [] nestedNameSpecifier
      i <- identifier
      return $ ElaboratedTypeSpecifier attrs n i) <|>
  (do class_key
      optional $ doubleColon
      n <- option [] nestedNameSpecifier
      optional kwTemplate
      s <- simple_template_id
      return $ ElaboratedTemplateSpecifier n s) <|>
  (do kwEnum
      optional $ doubleColon
      n <- option [] nestedNameSpecifier
      i <- identifier
      return $ ElaboratedEnumSpecifier n i)

data Enumerator = Enumerator String (Maybe Expression) deriving (Show, Eq)

data EnumKey = Enum | EnumStruct | EnumClass deriving (Show, Eq)

enum_name :: P Name
enum_name = do
  i <- identifier
  return $ Id i

enum_specifier :: P DeclSpecifier
enum_specifier = do
  e <- enum_head
  list <- braces $ (option [] enumerator_list) <|> (do {l <- enumerator_list; comma; return l})
  return $ e list

enum_head =
  (do key <- enum_key
      attrs <- option [] attribute_specifier_seq
      i <- optionMaybe identifier
      base <- option [] enum_base
      return $ EnumSpecifier key attrs i base) <|>
  (do key <- enum_key
      attrs <- option [] attribute_specifier_seq
      n <- nestedNameSpecifier
      i <- identifier
      base <- option [] enum_base
      return $ NestedEnumSpecifier key attrs n i base)

opaque_enum_declaration = do
  key <- enum_key
  attrs <- option [] attribute_specifier_seq
  i <- identifier
  base <- option [] enum_base
  return $ OpaqueEnum key attrs i base


enum_key =
  (do {kwEnum; return Enum}) <|>
  (do {kwEnum; kwClass; return EnumClass}) <|>
  (do {kwEnum; kwStruct; return EnumStruct})

enum_base = do
  colon
  ts <- typeSpecifierSeq
  return ts

enumerator_list = sepBy1 enumerator_definition (comma)

enumerator_definition :: P Enumerator
enumerator_definition =
  (do n <- enumerator
      opAssign
      e <- constant_expression
      return $ Enumerator n (Just e)) <|>
  (do n <- enumerator
      return $ Enumerator n Nothing)

enumerator = identifier

namespace_name :: P String
namespace_name = original_namespace_name <|> namespace_alias

original_namespace_name = identifier

namespace_definition :: P Declaration
namespace_definition = named_namespace_definition <|> unnamed_namespace_definition

named_namespace_definition = original_namespace_definition <|> extention_namespace_definition

original_namespace_definition = do
  inl <- optionMaybe kwInline
  kwNamespace
  name <- identifier
  ds <- braces namespace_body
  return $ NamespaceDeclaration inl (Just name) ds

extention_namespace_definition = do
  inl <- optionMaybe kwInline
  kwNamespace
  name <- original_namespace_name
  ds <- braces namespace_body
  return $ NamespaceDeclaration inl (Just name) ds

unnamed_namespace_definition = do
  inl <- optionMaybe kwInline
  kwNamespace
  ds <- braces namespace_body
  return $ NamespaceDeclaration inl Nothing ds

namespace_body = option [] declaration_seq

namespace_alias = identifier

namespace_alias_definition :: P Declaration
namespace_alias_definition = do
  kwNamespace
  i <- identifier
  opAssign
  (ns, n) <- qualified_namespace_specifier
  semi
  return $ NamespaceAlias i ns n

qualified_namespace_specifier = do
  optional $ doubleColon
  n <- option [] nestedNameSpecifier
  i <- namespace_name
  return (n, i)

using_declaration :: P Declaration
using_declaration =
  (do kwUsing
      doubleColon
      e <- unqualifiedId
      semi
      return $ UsingDeclaration [] e) <|>
  (do kwUsing
      optional kwTypename
      optional $ doubleColon
      ns <- nestedNameSpecifier
      e <- unqualifiedId
      semi
      return $ UsingDeclaration ns e)

using_directive :: P Declaration
using_directive = do
  as <- option [] attribute_specifier_seq
  kwUsing
  kwNamespace
  optional $ doubleColon
  ns <- option [] nestedNameSpecifier
  n <- namespace_name
  semi
  return $ UsingDirective as ns n

asm_definition :: P Declaration
asm_definition = do
  kwAsm
  s <- parens stringLiteral
  semi
  return $ AsmDefinition s

linkage_specification :: P Declaration
linkage_specification =
  (do kwExtern
      l <- stringLiteral
      d <- declaration
      return $ LinkageSpecification l [d]) <|>
  (do kwExtern
      l <- stringLiteral
      ds <- braces $ option [] declaration_seq
      return $ LinkageSpecification l ds)

data Attribute = Attribute AttributeToken String deriving (Show, Eq)

data AttributeToken = AttributeT String | AttributeWithNamespace String String deriving (Show, Eq)

attribute_specifier_seq = many1 attribute_specifier

attribute_specifier =
  alignment_specifier <|>
  (do leftBracket
      leftBracket
      l <- attribute_list
      rightBracket
      rightBracket
      return $ AttributeList l)

-- TODO alignment specifier
alignment_specifier =
  (do kwAlignas
      leftParen
      t <- type_id
      dots <- optionMaybe $ threeDot
      rightParen
      return $ AlignmentSpecifierType t dots) <|>
  (do kwAlignas
      leftParen
      e <- expression
      dots <- optionMaybe $ threeDot
      rightParen
      return $ AlignmentSpecifierExpr e dots)

-- TODO add dots
attribute_list :: P [Attribute]
attribute_list = do
  as <- sepBy1 attribute $ comma
  dots <- optionMaybe $ threeDot
  return as

attribute = do
  a <- attribute_token
  c <- option "" attribute_argument_clause
  return $ Attribute a c

attribute_token :: P AttributeToken
attribute_token = attribute_scoped_token <|> (do { i <- identifier; return $ AttributeT i})

attribute_scoped_token = do
  n <- attribute_namespace
  doubleColon
  i <- identifier
  return $ AttributeWithNamespace n i

attribute_namespace = identifier

-- TODO attribute argument clause
attribute_argument_clause = undefined

balanced_token_seq :: P [[String]]
balanced_token_seq = many1 balanced_token

-- TODO balanced token
balanced_token = undefined

type InitDeclarator = (Declarator, [Expression])

init_declarator_list :: P [InitDeclarator]
init_declarator_list = sepBy1 init_declarator $ comma

init_declarator :: P InitDeclarator
init_declarator = do
  d <- declarator
  i <- option [] initializer
  return (d, i)

data Declarator
  = PtrDeclarator PtrOperator Declarator
  | DeclaratorId DId [AttributeSpecifier]
  | NoptrDeclaratorParams Declarator ParametersAndQualifiers
  | NoptrDeclarator Declarator (Maybe Expression) [AttributeSpecifier]
  | ParametersDeclarator Declarator ParametersAndQualifiers TypeId
  | ParametersAbstractDeclarator (Maybe Declarator) ParametersAndQualifiers TypeId
  | NoptrAbstractDeclaratorParams (Maybe Declarator) ParametersAndQualifiers
  | NoptrAbstractDeclarator (Maybe Declarator) Expression [AttributeSpecifier]
  | PtrAbstractDeclarator PtrOperator (Maybe Declarator)

  | PtrNewDeclarator PtrOperator (Maybe Declarator)
  | NoptrNewDeclarator (Maybe Declarator) Expression [AttributeSpecifier]

  | LambdaDeclarator [Parameter] (Maybe String) (Maybe ExceptionSpecification) [AttributeSpecifier] (Maybe TypeId)
  | ConversionDeclarator PtrOperator (Maybe Declarator)
  deriving (Show, Eq)

data PtrOperator = PtrOperator
  { ptrOperatorString :: String
  , ptrOperatorAttributes :: [AttributeSpecifier]
  , ptrOperatorNestedNames :: [Name]
  , ptrOperatorCvQualifiers :: [DeclSpecifier]
  } deriving (Show, Eq)

declarator :: P Declarator
declarator =
  (do d <- noptr_declarator
      ps <- parameters_and_qualifiers
      t <- trailing_return_type
      return $ ParametersDeclarator d ps t) <|> ptr_declarator

ptr_declarator :: P Declarator
ptr_declarator = noptr_declarator <|>
  (do op <- ptr_operator
      d <- ptr_declarator
      return $ PtrDeclarator op d)

noptr_declarator :: P Declarator
noptr_declarator =
  (do d <- declarator_id
      as <- option [] attribute_specifier_seq
      return $ DeclaratorId d as) <|>
  (do d <- noptr_declarator
      ps <- parameters_and_qualifiers
      return $ NoptrDeclaratorParams d ps) <|>
  (do d <- noptr_declarator
      e <- brackets $ optionMaybe constant_expression
      as <- attribute_specifier_seq
      return $ NoptrDeclarator d e as) <|>
  parens ptr_declarator

type ParametersAndQualifiers = ([Parameter], [AttributeSpecifier], [DeclSpecifier], Maybe String, Maybe ExceptionSpecification)

parameters_and_qualifiers :: P ParametersAndQualifiers
parameters_and_qualifiers = do
  ps <- parens parameter_declaration_clause
  as <- option [] attribute_specifier_seq
  cvs <- option [] cv_qualifier_seq
  r <- optionMaybe ref_qualifier
  ex <- optionMaybe exception_specification
  return (ps, as, cvs, r, ex)

trailing_return_type :: P TypeId
trailing_return_type = do
  opArrow
  ts <- trailing_type_specifier_seq
  a <- optionMaybe abstract_declarator
  return (ts, a)

ptr_operator :: P PtrOperator
ptr_operator =
  (do optional $ doubleColon
      ns <- option [] nestedNameSpecifier
      s <- string "*"
      as <- option [] attribute_specifier_seq
      cvs <- cv_qualifier_seq
      return $ PtrOperator s as ns cvs) <|>
  (do s <- string "&&" <|> string "&"
      as <- option [] attribute_specifier_seq
      return $ PtrOperator s as [] [])

cv_qualifier_seq = many1 cv_qualifier

cv_qualifier :: P DeclSpecifier
cv_qualifier =
  (do kwConst
      return Const) <|>
  (do kwVolatile
      return Volatile)

ref_qualifier :: P String
ref_qualifier = string "&" <|> string "&&"

data DId = DIExpr String Expression | DIClassName [Name] Name deriving (Show, Eq)

declarator_id =
  (do t <- option "" $ threeDot
      e <- idExpression
      return $ DIExpr t e) <|>
  (do optional $ doubleColon
      ns <- option [] nestedNameSpecifier
      n <- class_name
      return $ DIClassName ns n)

type TypeId = ([DeclSpecifier], Maybe Declarator)

type_id :: P TypeId
type_id = do
  ts <- typeSpecifierSeq
  d <- optionMaybe abstract_declarator
  return (ts, d)

abstract_declarator :: P Declarator
abstract_declarator =
  (do d <- optionMaybe noptr_abstract_declarator
      ps <- parameters_and_qualifiers
      t <- trailing_return_type
      return $ ParametersAbstractDeclarator d ps t) <|>
  ptr_abstract_declarator

ptr_abstract_declarator =
  (do op <- ptr_operator
      d <- optionMaybe ptr_abstract_declarator
      return $ PtrAbstractDeclarator op d) <|>
  noptr_abstract_declarator

noptr_abstract_declarator =
  (do d <- optionMaybe noptr_abstract_declarator
      ps <- parameters_and_qualifiers
      return $ NoptrAbstractDeclaratorParams d ps) <|>
  (do d <- optionMaybe noptr_abstract_declarator
      e <- brackets constant_expression
      as <- option [] attribute_specifier_seq
      return $ NoptrAbstractDeclarator d e as) <|>
  parens ptr_abstract_declarator

-- TODO add ...
parameter_declaration_clause :: P [Parameter]
parameter_declaration_clause = option [] parameter_declaration_list

parameter_declaration_list = sepBy1 parameter_declaration $ comma

data Parameter = Parameter
  { parameterAttibutes :: [AttributeSpecifier]
  , parameterDeclSpecifiers :: [DeclSpecifier]
  , parameterDeclarator :: (Maybe Declarator)
  , parameterInitializer :: Maybe Expression
  }
  deriving (Show, Eq)

parameter_declaration :: P Parameter
parameter_declaration =
  (do as <- option [] attribute_specifier_seq
      ds <- decl_specifier_seq
      d <- declarator
      opAssign
      i <- initializerClause
      return $ Parameter as ds (Just d) (Just i)) <|>
  (do as <- option [] attribute_specifier_seq
      ds <- decl_specifier_seq
      d <- optionMaybe abstract_declarator
      opAssign
      i <- initializerClause
      return $ Parameter as ds d (Just i)) <|>
  (do as <- option [] attribute_specifier_seq
      ds <- decl_specifier_seq
      d <- declarator
      return $ Parameter as ds (Just d) Nothing) <|>
  (do as <- option [] attribute_specifier_seq
      ds <- decl_specifier_seq
      d <- optionMaybe abstract_declarator
      return $ Parameter as ds d Nothing)

function_definition :: P Declaration
function_definition =
  (do as <- attribute_specifier_seq
      ds <- decl_specifier_seq
      d <- declarator
      opAssign
      kwDefault
      return $ DefaultFunctionDefinition as ds d) <|>
  (do as <- attribute_specifier_seq
      ds <- decl_specifier_seq
      d <- declarator
      opAssign
      kwDelete
      return $ DeleteFunctionDefinition as ds d) <|>
  (do as <- attribute_specifier_seq
      ds <- decl_specifier_seq
      d <- declarator
      body <- function_body
      return $ FunctionDefinition as ds d body)

-- TODO function body
function_body :: P Statement
function_body =
  (do c <- optionMaybe ctor_initializer
      cs <- compound_statement
      return cs) <|>
  function_try_block

initializer :: P [Expression]
initializer =
  (do b <- braceOrEqualInitializer
      return [b]) <|>
  (do es <- parens expression_list
      return es)

braceOrEqualInitializer =
  (do opAssign
      c <- initializerClause
      return c) <|> bracedInitList

initializerClause :: P Expression
initializerClause = assignment_expression <|> bracedInitList

-- TODO add dots
initializer_list = do
  es <- sepBy1 initializerClause $ comma
  dots <- optionMaybe $ threeDot
  return es

bracedInitList :: P Expression
bracedInitList =
  (do leftBrace
      rightBrace
      return $ BracedInitList []) <|>
  (do leftBrace
      es <- initializer_list
      optional $ comma
      rightBrace
      return $ BracedInitList es)

-------------------------------------------------------------
-- Class
-------------------------------------------------------------

class_name :: P Name
class_name =
  (do i <- identifier
      return $ Id i) <|>
  simple_template_id

-- TODO class
class_specifier :: P DeclSpecifier
class_specifier = do
  head <- class_head
  ms <- braces member_specification
  return ClassSpecifier

-- TODO class head
class_head =
  (do class_key
      as <- option [] attribute_specifier_seq
      n <- class_head_name
      vs <- class_virt_specifier_seq
      b <- optionMaybe base_clause
      return ()) <|>
  (do class_key
      as <- option [] attribute_specifier_seq
      b <- optionMaybe base_clause
      return ())

class_head_name = do
  nes <- option [] nestedNameSpecifier
  n <- class_name
  return $ nes ++ [n]

class_virt_specifier_seq = many1 class_virt_specifier

class_virt_specifier = string "final" <|> kwExplicit

class_key = kwClass <|> kwStruct <|> kwUnion

-- TODO member specification
member_specification =
  (do d <- member_declaration
      m <- member_specification
      return ()) <|>
  (do a <- access_specifier
      colon
      m <- member_specification
      return ())

-- TODO member declaration
member_declaration =
  (do as <- option [] attribute_specifier_seq
      ds <- option [] decl_specifier_seq
      ms <- option [] member_declarator_list
      semi
      return ()) <|>
  (do f <- function_definition
      optional semi
      return ()) <|>
  (do u <- using_declaration
      return ()) <|>
  (do s <- static_assert_declaration
      return ()) <|>
  (do t <- template_declaration
      return ()) <|>
  (do a <- alias_declaration
      return ())

member_declarator_list = sepBy1 member_declarator comma

-- TODO member declarator
member_declarator =
  (do d <- declarator
      vs <- option [] virt_specifier_seq
      p <- option False pure_specifier
      return ()) <|>
  (do d <- declarator
      vs <- option [] virt_specifier_seq
      b <- optionMaybe braceOrEqualInitializer
      return ()) <|>
  (do i <- optionMaybe identifier
      as <- option [] attribute_specifier_seq
      vs <- option [] virt_specifier_seq
      colon
      e <- constant_expression
      return ())

virt_specifier_seq = many1 virt_specifier

virt_specifier = string "override" <|> string "final" <|> kwNew

pure_specifier :: P Bool
pure_specifier = do
  opAssign
  string "0"
  return True

base_clause = do
  colon
  bs <- base_specifier_list
  return bs

-- TODO add dots
base_specifier_list = do
  bs <- sepBy1 base_specifier comma
  dots <- optionMaybe threeDot
  return bs

-- TODO base specifier
base_specifier =
  (do as <- option [] attribute_specifier_seq
      ns <- base_type_specifier
      return ()) <|>
  (do as <- option [] attribute_specifier_seq
      v <- kwVirtual
      a <- optionMaybe access_specifier
      ns <- base_type_specifier
      return ()) <|>
  (do as <- option [] attribute_specifier_seq
      a <- access_specifier
      v <- optionMaybe kwVirtual
      ns <- base_type_specifier
      return ())

class_or_decltype :: P [Name]
class_or_decltype =
  (do optional doubleColon
      ns <- option [] nestedNameSpecifier
      n <- class_name
      return $ ns ++ [n]) <|>
  (do d <- decltype_specifier
      return $ [TypeName d])

base_type_specifier = class_or_decltype

access_specifier = kwPrivate <|> kwProtected <|> kwPublic

conversion_function_id :: P Name
conversion_function_id = do
  kwOperator
  t <- conversion_type_id
  return $ ConversionFunctionId t

conversion_type_id :: P TypeId
conversion_type_id = do
  ts <- typeSpecifierSeq
  d <- optionMaybe conversion_declarator
  return (ts, d)

conversion_declarator :: P Declarator
conversion_declarator = do
  op <- ptr_operator
  d <- optionMaybe conversion_declarator
  return $ ConversionDeclarator op d

ctor_initializer = do
  colon
  ms <- mem_initializer_list
  return ms

-- TODO add dots
mem_initializer_list = do
  ms <- sepBy1 mem_initializer comma
  dots <- optionMaybe threeDot
  return ms

mem_initializer =
  (do m <- mem_initializer_id
      es <- parens $ option [] expression_list
      return (m, es)) <|>
  (do m <- mem_initializer_id
      b <- bracedInitList
      return (m, [b]))

mem_initializer_id :: P [Name]
mem_initializer_id =
  (do i <- identifier
      return [Id i]) <|>
  class_or_decltype

operator_function_id :: P Name
operator_function_id =
  (do kwOperator
      op <- overloadable_operator
      return $ OperatorFunctionId op) <|>
  (do kwOperator
      op <- overloadable_operator
      as <- angles $ option [] template_argument_list
      return $ OperatorFunctionTemplateId op as)

overloadable_operator :: P String
overloadable_operator =
  choice
    [ kwNew
    , kwDelete
    , (do {n <- kwNew; l <- leftBracket; r <- rightBracket; return $ n ++ l ++ r})
    , (do {d <- kwDelete; l <- leftBracket; r <- rightBracket; return $ d ++ l ++ r})
    , opPlus
    , opMinus
    , opStar
    , opDiv
    , opRem
    , opXor
    , opAnd
    , opOr
    , opTilda
    , opNot
    , opAssign
    , opLess
    , opGreater
    , opAssignPlus
    , opAssignMinus
    , opAssignMul
    , opAssignDiv
    , opAssignRem
    , opAssignAnd
    , opAssignOr
    , opLeftShift
    , opRightShift
    , opAssignLeftShift
    , opAssignRightShift
    , opEq
    , opNotEq
    , opLessEq
    , opGreaterEq
    , opLogicalAnd
    , opLogicalOr
    , opIncrement
    , opDecrement
    , comma
    , opArrowPtr
    , opArrow
    , (do {l <- leftParen; r <- rightParen; return $ l++r})
    , (do {l <- leftBracket; r <- rightBracket; return $ l++r})
    ]

literal_operator_id :: P Name
literal_operator_id = do
  kwOperator
  string "\"\""
  i <- identifier
  return $ LiteralOperatorId i

-----------------------------------------------------------------------
-- Template
-----------------------------------------------------------------------

template_declaration :: P Declaration
template_declaration = do
  kwTemplate
  ps <- angles template_parameter_list
  d <- declaration
  return $ TemplateDeclaration ps d

template_parameter_list :: P [TemplateParameter]
template_parameter_list = sepBy1 template_parameter $ comma

data TemplateParameter
  = PlainParameter Parameter
  | ClassParameter (Maybe String) (Maybe String)
  | ClassParameterWithType (Maybe String) TypeId
  | TypenameParameter (Maybe String) (Maybe String)
  | TypenameParameterWithType (Maybe String) TypeId
  | TemplateParameter
  deriving (Show, Eq)

template_parameter :: P TemplateParameter
template_parameter =
  (do p <- parameter_declaration
      return $ PlainParameter p) <|>
  type_parameter

type_parameter :: P TemplateParameter
type_parameter = undefined

simple_template_id :: P Name
simple_template_id = do
  name <- template_name
  args <- angles $ option [] template_argument_list
  return $ SimpleTemplateId name args

template_id :: P Name
template_id =
  simple_template_id <|>
  (do OperatorFunctionId id <- operator_function_id
      as <- angles $ option [] template_argument_list
      return $ OperatorFunctionTemplateId id as) <|>
  (do LiteralOperatorId id <- literal_operator_id
      as <- angles $ option [] template_argument_list
      return $ LiteralOperatorTemplateId id as)

template_name :: P String
template_name = identifier

data TemplateArgument
  = ArgumentConstExpr Expression
  | ArgumentTypeId TypeId
  | ArgumentIdExpr Expression
  deriving (Show, Eq)

-- TODO add dots
template_argument_list :: P [TemplateArgument]
template_argument_list = do
  args <- sepBy1 template_argument $ comma
  dots <- optionMaybe $ threeDot
  return args

template_argument =
  (do e <- constant_expression
      return $ ArgumentConstExpr e) <|>
  (do t <- type_id
      return $ ArgumentTypeId t) <|>
  (do e <- idExpression
      return $ ArgumentIdExpr e)

typename_specifier = undefined

explicit_instantiation :: P Declaration
explicit_instantiation = undefined

explicit_specialization :: P Declaration
explicit_specialization = do
  kwTemplate
  opLess
  opGreater
  d <- declaration
  return $ ExplicitSpecialization d

-----------------------------------------------------------
-- Exception
-----------------------------------------------------------

try_block :: P Statement
try_block = do
  kwTry
  s <- compound_statement
  hs <- handler_seq
  return $ TryBlock s hs

-- TODO function try block
function_try_block :: P Statement
function_try_block = undefined

data Handler = Handler ExceptionDeclaration Statement deriving (Show, Eq)

handler_seq = many1 handler

handler = do
  kwCatch
  d <- parens exception_declaration
  s <- compound_statement
  return $ Handler d s

data ExceptionDeclaration
  = EDThreeDot
  | ExceptionDeclaration [AttributeSpecifier]
                         TypeId
  deriving (Show, Eq)

exception_declaration =
  (do threeDot
      return EDThreeDot) <|>
  (do as <- option [] attribute_specifier_seq
      t <- typeSpecifierSeq
      d <- declarator
      return $ ExceptionDeclaration as (t, (Just d))) <|>
  (do as <- option [] attribute_specifier_seq
      t <- typeSpecifierSeq
      d <- optionMaybe abstract_declarator
      return $ ExceptionDeclaration as (t, d))

throw_expression :: P Expression
throw_expression = do
  kwThrow
  e <- optionMaybe assignment_expression
  return $ Throw e

data ExceptionSpecification
  = DynamicExceptionSpecification [TypeId]
  | NoExceptSpecification (Maybe Expression)
  deriving (Show, Eq)

exception_specification = dynamic_exception_specification <|> noexcept_specification

dynamic_exception_specification = do
  kwThrow
  ts <- parens $ option [] type_id_list
  return $ DynamicExceptionSpecification ts

-- TODO add dots
type_id_list = do
  ts <- sepBy1 type_id $ comma
  dots <- optionMaybe $ threeDot
  return ts

noexcept_specification =
  (do kwNoexcept
      e <- parens constant_expression
      return $ NoExceptSpecification (Just e)) <|>
  (do kwNoexcept
      return $ NoExceptSpecification Nothing)

------------------------------------------------------------------------

------------------------------------------------------------------------
-- Preprocessor
------------------------------------------------------------------------

data PPFile = PPFile [PPDirective] deriving (Show, Eq)

data PPDirective
  = PPIf { ppIfExpr :: Expression
         , ppIfGroup :: [PPDirective]
         , ppElifGroups :: [(Expression, [PPDirective])]
         , ppIfElseGroup :: [PPDirective]
         }
  | PPIfDef String [PPDirective] [PPDirective]
  | PPIfNDef String [PPDirective] [PPDirective]
  | PPInclude [String]
  | PPDefineS String [String]
  | PPDefineF String [String] [String]
  | PPUndef String
  | PPLine [String]
  | PPError [String]
  | PPPragma [String]
  | PPEmpty
  | PPTextLine [String]
  | PPNonDirective [String]
  deriving (Show, Eq)

preprocessing_file :: P PPFile
preprocessing_file = do
  gs <- option [] group
  return $ PPFile gs

group = many1 group_part

group_part :: P PPDirective
group_part = if_section <|> control_line <|> text_line <|> (do {hash; n <- non_directive; return $ PPNonDirective n})

if_section =
  (do string "#if"
      e <- constant_expression
      new_line
      pps <- option [] group
      elifs <- option [] elif_groups
      elsegroup <- option [] else_group
      endif_line
      return $ PPIf e pps elifs elsegroup) <|>
  (do string "#ifdef"
      e <- identifier
      new_line
      pps <- option [] group
      elsegroup <- option [] else_group
      endif_line
      return $ PPIfDef e pps elsegroup) <|>
  (do string "#ifndef"
      e <- identifier
      new_line
      pps <- option [] group
      elsegroup <- option [] else_group
      endif_line
      return $ PPIfNDef e pps elsegroup)

--if_group = undefined

elif_groups = many1 elif_group

elif_group :: P (Expression, [PPDirective])
elif_group = do
  string "#elif"
  e <- constant_expression
  new_line
  pps <- option [] group
  return (e, pps)

else_group = do
  string "#else"
  new_line
  pps <- option [] group
  return pps

endif_line = do
  string "#endif"
  new_line
  return ()

control_line :: P PPDirective
control_line =
  (do string "#include"
      ps <- pp_tokens
      new_line
      return $ PPInclude ps) <|>
  (do string "#define"
      i <- identifier
      rs <- replacement_list
      new_line
      return $ PPDefineS i rs) <|>
  (do string "#define"
      i <- identifier
      lparen
      ids <- sepBy (identifier <|> threeDot) comma
      rs <- replacement_list
      new_line
      return $ PPDefineF i ids rs) <|>
  (do string "#undef"
      i <- identifier
      new_line
      return $ PPUndef i) <|>
  (do string "#line"
      pps <- pp_tokens
      new_line
      return $ PPLine pps) <|>
  (do string "#error"
      pps <- option [] pp_tokens
      new_line
      return $ PPError pps) <|>
  (do string "#pragma"
      pps <- option [] pp_tokens
      new_line
      return $ PPPragma pps) <|>
  (do hash
      new_line
      return PPEmpty)

text_line :: P PPDirective
text_line = do
  pp <- option [] pp_tokens
  new_line
  return $ PPTextLine pp

non_directive :: P [String]
non_directive = do
  pp <- pp_tokens
  new_line
  return pp

lparen :: P String
lparen = do
  c <- leftParen
  notFollowedBy space
  return c

identifier_list :: P [String]
identifier_list = sepBy1 identifier $ comma

replacement_list :: P [String]
replacement_list = option [] pp_tokens

pp_tokens :: P [String]
pp_tokens = many1 preprocessingToken

new_line :: P Char
new_line = newline

---------------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------------
angles :: P a -> P a
angles = between (opLess) (opGreater)

brackets :: P a -> P a
brackets = between (leftBracket) (rightBracket)

braces :: P a -> P a
braces = between (leftBrace) (rightBrace)

parens :: P a -> P a
parens = between (leftParen) (rightParen)

quotes :: P a -> P a
quotes = between (char '"') (char '"')

single_quotes :: P a -> P a
single_quotes = between (char '\'') (char '\'')

reserved str = do
  s <- string str
  notFollowedBy (digit <|> nondigit)
  return s
