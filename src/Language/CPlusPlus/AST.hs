module Language.CPlusPlus.AST where

import Text.Parsec.Pos (SourcePos)

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
  = Id SourcePos String
  | TypeName Type
  | ConversionFunctionId TypeId
  | OperatorFunctionId String
  | OperatorFunctionTemplateId String [TemplateArgument]
  | LiteralOperatorId String
  | SimpleTemplateId String [TemplateArgument]
  | LiteralOperatorTemplateId String [TemplateArgument]
  | ThreeDotName SourcePos
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


data Condition = Condition Expression | InitializerCondition deriving (Show, Eq)

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

data Capture
  = DefaultRefCapture
  | DefaultValueCapture
  | ThisCapture
  | IdCapture String
  | RefCapture String
  | ThreeDot deriving (Show, Eq)

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


data Enumerator = Enumerator String (Maybe Expression) deriving (Show, Eq)

data EnumKey = Enum | EnumStruct | EnumClass deriving (Show, Eq)

data Attribute = Attribute AttributeToken String deriving (Show, Eq)

data AttributeToken = AttributeT String | AttributeWithNamespace String String deriving (Show, Eq)

type InitDeclarator = (Declarator, [Expression])

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

type ParametersAndQualifiers = ([Parameter], [AttributeSpecifier], [DeclSpecifier], Maybe String, Maybe ExceptionSpecification)

data DId = DIExpr String Expression | DIClassName [Name] Name deriving (Show, Eq)

type TypeId = ([DeclSpecifier], Maybe Declarator)

data Parameter = Parameter
  { parameterAttibutes :: [AttributeSpecifier]
  , parameterDeclSpecifiers :: [DeclSpecifier]
  , parameterDeclarator :: (Maybe Declarator)
  , parameterInitializer :: Maybe Expression
  }
  deriving (Show, Eq)

data TemplateParameter
  = PlainParameter Parameter
  | ClassParameter (Maybe String) (Maybe String)
  | ClassParameterWithType (Maybe String) TypeId
  | TypenameParameter (Maybe String) (Maybe String)
  | TypenameParameterWithType (Maybe String) TypeId
  | TemplateParameter
  deriving (Show, Eq)

data TemplateArgument
  = ArgumentConstExpr Expression
  | ArgumentTypeId TypeId
  | ArgumentIdExpr Expression
  deriving (Show, Eq)

data Handler = Handler ExceptionDeclaration Statement deriving (Show, Eq)

data ExceptionDeclaration
  = EDThreeDot
  | ExceptionDeclaration [AttributeSpecifier]
                         TypeId
  deriving (Show, Eq)

data ExceptionSpecification
  = DynamicExceptionSpecification [TypeId]
  | NoExceptSpecification (Maybe Expression)
  deriving (Show, Eq)
