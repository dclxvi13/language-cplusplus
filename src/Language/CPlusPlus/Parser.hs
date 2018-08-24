module Language.CPlusPlus.Parser where

--import Language.CPlusPlus.AST
import           Language.CPlusPlus.Base
import           Language.CPlusPlus.Token

import           Text.Parsec              hiding (parse)

parse :: P a -> String -> [CppToken] -> Either ParseError a
parse p source input = runParser p () source input

testParse :: P a -> [CppToken] -> Either ParseError a
testParse p input = runParser p () "" input

data Literal = Literal
  { _literalPos   :: SourcePos
  , _literalValue :: String
  } deriving (Show, Eq)

data Id = Id
  { _idPos   :: SourcePos
  , _idValue :: String
  } deriving (Show, Eq)

identifier :: P Id
identifier = do
  pos <- getPosition
  s <- ident
  return $ Id pos s

literal :: P Literal
literal = do
  pos <- getPosition
  s <- choice
           [ integerLiteral
           , characterLiteral
           , floatingLiteral
           , stringLiteral
           , booleanLiteral
           , pointerLiteral
           , userDefinedLiteral
           ]
  return $ Literal pos s

-- Hyperlinked C++ BNF Grammar
-- By Alessio Marchetti
--
-- Version 3.2
--
-- Last updated: 12-Feb-2016
-- BNF Grammar Rules
-- basic.link
-- translation-unit:
--  	declaration-seq[opt]
data TranslationUnit =
  TU [Declaration]
  deriving (Show, Eq)

translationUnit :: P TranslationUnit
translationUnit = do
  ds <- option [] declarationSeq
  return $ TU ds

-- expr.prim.general
-- primary-expression:
--  	literal
--  	this
--  	( expression )
--  	id-expression
--  	lambda-expression     C++0x
-- id-expression:
--  	unqualified-id
--  	qualified-id
-- unqualified-id:
--  	identifier
--  	operator-function-id
--  	conversion-function-id
--  	literal-operator-id     C++0x
--  	~ class-name
--  	~ decltype-specifier     C++0x
--  	template-id
-- qualified-id:
--  	::[opt] nested-name-specifier template[opt] unqualified-id
--  	:: identifier
--  	:: operator-function-id
--  	:: literal-operator-id     C++0x
--  	:: template-id
-- nested-name-specifier:
--  	type-name ::
--  	namespace-name ::
--  	decltype-specifier ::     C++0x
--  	nested-name-specifier identifier ::
--  	nested-name-specifier template[opt] simple-template-id ::
data Expression
  -- primary expression
  = LiteralExpression { _literalExpressionPos   :: SourcePos
                      , _literalExpressionValue :: Literal }
  | ThisExpression { _thisExpressionPos :: SourcePos }
  | ParensedExpression { _parensedExpressionPos   :: SourcePos
                       , _parensedExpressionValue :: Expression }
  | IdExpression { _idExpressionPos   :: SourcePos
                 , _idExpressionValue :: Either UnqualifiedId QualifiedId }
  | LambdaExpression { _lambdaExpressionPos        :: SourcePos
                     , _lambdaExpressionIntroducer :: LambdaIntroducer
                     , _lambdaExpressionDeclarator :: Maybe LambdaDeclarator
                     , _lambdaExpressionStatement  :: Statement }
  -- postfix expression
  --  	primary-expression
  --  	postfix-expression [ expression ]
  | GetByIndexExpression { _getByIndexExpressionPos   :: SourcePos
                         , _getByIndexExpressionFrom  :: Expression
                         , _getByIndexExpressionIndex :: Expression }
  --  	postfix-expression [ braced-init-list[opt] ]     C++0x
  | GetByBracedExpression { _getByBracedExpressionPos      :: SourcePos
                          , _getByBracedExpressionFrom     :: Expression
                          , _getByBracedExpressionInitList :: Maybe BracedInitList }
  --  	postfix-expression ( expression-list[opt] )
  | CallExpression { _callExpressionPos    :: SourcePos
                   , _callExpressionCallee :: Expression
                   , _callExpressionArgs   :: Maybe ExpressionList }
  --  	simple-type-specifier ( expression-list[opt] )
  | SimpleTypeCallExpression { _simpleTypeCallExpressionPos  :: SourcePos
                             , _simpleTypeCallExpressionType :: SimpleTypeSpecifier
                             , _simpleTypeCallExpressionArgs :: Maybe ExpressionList }
  --  	typename-specifier ( expression-list[opt] )
  | TypenameCallExpression { _typenameCallExpressionPos      :: SourcePos
                           , _typenameCallExpressionTypename :: TypeSpecifier
                           , _typenameCallExpressionArgs     :: Maybe ExpressionList }
  --  	simple-type-specifier braced-init-list     C++0x
  | SimpleTypeWithBracedExpression { _simpleTypeWithBracedExpressionPos        :: SourcePos
                                   , _simpleTypeWithBracedExpressionSimpleType :: SimpleTypeSpecifier
                                   , _simpleTypeWithBracedExpressionBraced     :: BracedInitList }
  --  	typename-specifier braced-init-list     C++0x
  | TypenameWithBracedExpression { _typenameWithBracedExpressionPos      :: SourcePos
                                 , _typenameWithBracedExpressionTypename :: TypenameSpecifier
                                 , _typenameWithBracedExpressionBraced   :: BracedInitList }
  --  	postfix-expression . template[opt] id-expression
  | GetFromRefExpression { _getFromRefExpressionPos :: SourcePos
                         , _getFromRefExpressionRef :: Expression
                         , _getFromRefExpressionIsTemplate :: Bool
                         , _getFromRefExpressionElem :: Expression }
  --  	postfix-expression -> template[opt] id-expression
  | GetFromPtrExpression { _getFromPtrExpressionPos :: SourcePos
                         , _getFromPtrExpressionPtr :: Expression
                         , _getFromPtrExpressionIsTemplate :: Bool
                         , _getFromPtrExpressionElem :: Expression }
  --  	postfix-expression . pseudo-destructor-name
  | GetDestructorFromRefExpression { _getDestructorFromRefExpressionPos :: SourcePos
                                   , _getDestructorFromRefExpressionRef :: Expression
                                   , _getDestructorFromRefExpressionDestructor :: PseudoDestructorName }
  --  	postfix-expression -> pseudo-destructor-name
  | GetDestructorFromPtrExpression { _getDestructorFromPtrExpressionPos :: SourcePos
                                   , _getDestructorFromPtrExpressionPtr :: Expression
                                   , _getDestructorFromPtrExpressionDestructor :: PseudoDestructorName }
  --  	postfix-expression ++
  | PostIncrementExpression { _postIncrementExpressionPos :: SourcePos
                            , _postIncrementExpressionValue :: Expression }
  --  	postfix-expression --
  | PostDecrementExpression { _postDecrementExpressionPos :: SourcePos
                            , _postDecrementExpressionValue :: Expression }
  --  	dynamic_cast < type-id > ( expression )
  | DinamicCastExpression { _dinamicCastExpressionPos :: SourcePos
                          , _dinamicCastExpressionType :: TypeId
                          , _dinamicCastExpressionCasted :: Expression }
  --  	static_cast < type-id > ( expression )
  | StaticCastExpression { _staticCastExpressionPos :: SourcePos
                         , _staticCastExpressionType :: TypeId
                         , _staticCastExpressionCasted :: Expression }
  --  	reinterpret_cast < type-id > ( expression )
  | ReinterpretCastExpression { _reinterpretCastExpressionPos :: SourcePos
                              , _reinterpretCastExpressionType :: TypeId
                              , _reinterpretCastExpressionCasted :: Expression }
  --  	const_cast < type-id > ( expression )
  | ConstCastExpression { _constCastExpressionPos :: SourcePos
                        , _constCastExpressionType :: TypeId
                        , _constCastExpressionCasted :: Expression}
  --  	typeid ( expression )
  --  	typeid ( type-id )
  | TypeIdExpression { _typeIdExpressionPos :: SourcePos
                     , _typeIdExpressionCalled :: Either Expression TypeId }
  --  	++ cast-expression
  | PrefIncrementExpression { _prefIncrementExpressionPos :: SourcePos
                            , _prefIncrementExpressionValue :: Expression }
  --  	-- cast-expression
  | PrefDecrementExpression { _prefDecrementExpressionPos :: SourcePos
                            , _prefDecrementExpressionValue :: Expression }
  --  	unary-operator cast-expression
  | UnaryOperationExpression { _unaryOperationExpressionPos :: SourcePos
                             , _unaryOperationExpressionOperator :: UnaryOperator
                             , _unaryOperationExpressionValue :: Expression }
  --  	sizeof unary-expression
  | SizeOfExpression { _sizeOfExpressionPos :: SourcePos
                     , _sizeOfExpressionValue :: Expression }
  --  	sizeof ( type-id )
  | SizeOfTypeExpression { _sizeOfTypeExpressionPos :: SourcePos
                         , _sizeOfTypeExpressionType :: TypeId }
  --  	sizeof ... ( identifier )     C++0x
  | SizeOfThreeDottedExpression { _sizeOfThreeDottedExpressionPos :: SourcePos
                                , _sizeOfThreeDottedExpressionId :: Id }
  --  	alignof ( type-id )     C++0x
  | AlignOfExpression { _alignOfExpressionPos :: SourcePos
                      , _alignOfExpressionType :: TypeId }
  --  	noexcept ( expression )     C++0x
  | NoexceptExpression { _noexceptExpressionPos :: SourcePos
                       , _noexceptExpressionValue :: Expression }
  --  	::opt new new-placement[opt] new-type-id new-initializer[opt]
  | NewExpression { _newExpressionPos :: SourcePos
                  , _newExpressionPlacement :: Maybe ExpressionList
                  , _newExpressionTypeId :: NewTypeId
                  , _newExpressionInitializer :: Maybe NewInitializer }
  --  	::opt new new-placement[opt] ( type-id ) new-initializer[opt]
  | NewParensedExpression { _newParensedExpressionPos :: SourcePos
                          , _newParensedExpressionPlacement :: Maybe ExpressionList
                          , _newParensedExpressionTypeId :: TypeId
                          , _newParensedExpressionInitializer :: Maybe NewInitializer }
  --  	::opt delete cast-expression
  | DeleteExpression { _deleteExpressionPos :: SourcePos
                     , _deleteExpressionValue :: Expression }
  --  	::opt delete [ ] cast-expression
  | DeleteArrayExpression { _deleteArrayExpressionPos :: SourcePos
                          , _deleteArrayExpressionValue :: Expression }
  --  	( type-id ) cast-expression
  | CastExpression { _castExpressionPos :: SourcePos
                   , _castExpressionType :: TypeId
                   , _castExpressionValue :: Expression }
  --  	pm-expression .* cast-expression
  | GetPtrFromRefExpression { _getPtrFromRefExpressionPos :: SourcePos
                            , _getPtrFromRefExpressionFrom :: Expression
                            , _getPtrFromRefExpressionPtr :: Expression }
  --  	pm-expression ->* cast-expression
  | GetPtrFromPtrExpression { _getPtrFromPtrExpressionPos :: SourcePos
                            , _getPtrFromPtrExpressionFrom :: Expression
                            , _getPtrFromPtrExpressionPtr :: Expression }
  -- binary expression
  | BinaryOperationExpression { _binaryOperationExpressionPos :: SourcePos
                              , _binaryOperationExpressionOperator :: BinaryOperator
                              , _binaryOperationExpressionLeft :: Expression
                              , _binaryOperationExpressionRight :: Expression }
  -- conditional expression
  | ConditionalExpression { _conditionalExpressionPos :: SourcePos
                          , _conditionalExpressionCondition :: Expression
                          , _conditionalExpressionTruePart :: Expression
                          , _conditionalExpressionFalsePart :: Expression }
  -- logical-or-expression assignment-operator initializer-clause
  | AssignmentExpression { _assignmentExpressionPos :: SourcePos
                         , _assignmentExpressionValue :: Expression
                         , _assignmentExpressionOperator :: AssignmentOperator
                         , _assignmentExpressionInitializer :: InitializerClause }
  -- throw assignment-expression[opt]
  | ThrowExpression { _throwExpressionPos :: SourcePos
                    , _throwExpressionValue :: Maybe Expression }
  -- comma expression
  | CommaExpression { _commaExpressionPos :: SourcePos
                    , _commaExpressionLeft :: Expression
                    , _commaExpressionRight :: Expression }
  deriving (Show, Eq)

primaryExpression :: P Expression
primaryExpression =
  (do pos <- getPosition
      l <- literal
      return $ LiteralExpression pos l) <|>
  (do pos <- getPosition
      kwThis
      return $ ThisExpression pos) <|>
  (do pos <- getPosition
      e <- parens $ expression
      return $ ParensedExpression pos e) <|>
  idExpression <|>
  lambdaExpression

idExpression :: P Expression
idExpression = undefined

data UnqualifiedId
  = UnqualifiedIdIdentifier { _unqualifiedIdIdentifierPos   :: SourcePos
                            , _unqualifiedIdIdentifierValue :: Id }
  | UnqualifiedIdOperatorFunctionId { _unqualifiedIdOperatorFunctionIdPos   :: SourcePos
                                    , _unqualifiedIdOperatorFunctionIdValue :: OperatorFunctionId }
  | UnqualifiedIdConversionFunctionId { _unqualifiedIdConversionFunctionIdPos   :: SourcePos
                                      , _unqualifiedIdConversionFunctionIdValue :: ConversionFunctionId }
  | UnqualifiedIdLiteralOperatorId { _unqualifiedIdLiteralOperatorIdPos   :: SourcePos
                                   , _unqualifiedIdLiteralOperatorIdValue :: LiteralOperatorId }
  | UnqualifiedIdDestructorClass { _unqualifiedIdDestructorClassPos   :: SourcePos
                                 , _unqualifiedIdDestructorClassValue :: ClassName }
  | UnqualifiedIdDestructorDecltype { _unqualifiedIdDestructorDecltypePos   :: SourcePos
                                    , _unqualifiedIdDestructorDecltypeValue :: DecltypeSpecifier }
  | UnqualifiedIdTemplateId { _unqualifiedIdTemplateIdPos   :: SourcePos
                            , _unqualifiedIdTemplateIdValue :: TemplateId }
  deriving (Show, Eq)

data QualifiedId
  = QualifiedIdNestedId { _qualifiedIdNestedIdPos         :: SourcePos
                        , _qualifiedIdNestedNameSpecifier :: NestedNameSpecifier
                        , _qualifiedIdNestedIdIsTemplate  :: Bool
                        , _qualifiedIdNestedUnqualifiedId :: UnqualifiedId }
  | QualifiedIdIdentifier { _qualifiedIdIdentifierPos   :: SourcePos
                          , _qualifiedIdIdentifierValue :: Id }
  | QualifiedIdOperatorFunctionId { _qualifiedIdOperatorFunctionIdPos   :: SourcePos
                                  , _qualifiedIdOperatorFunctionIdValue :: OperatorFunctionId }
  | QualifiedIdLiteralOperatorId { _qualifiedIdLiteralOperatorIdPos   :: SourcePos
                                 , _qualifiedIdLiteralOperatorIdValue :: LiteralOperatorId }
  | QualifiedIdTemplateId { _qualifiedIdTemplateIdPos   :: SourcePos
                          , _qualifiedIdTemplateIdValue :: TemplateId }
  deriving (Show, Eq)

unqualifiedId :: P UnqualifiedId
unqualifiedId = undefined

qualifiedId :: P QualifiedId
qualifiedId = undefined

data NestedNameSpecifier
  = NestedNameSpecifierType { _nestedNameSpecifierTypePos   :: SourcePos
                            , _nestedNameSpecifierTypeValue :: TypeName }
  | NestedNameSpecifierNamespace { _nestedNameSpecifierNamespacePos   :: SourcePos
                                 , _nestedNameSpecifierNamespaceValue :: NamespaceName }
  | NestedNameSpecifierDecltype { _nestedNameSpecifierDecltypeSpecifierPos   :: SourcePos
                                , _nestedNameSpecifierDecltypeSpecifierValue :: DecltypeSpecifier }
  | NestedNameSpecifierIdentifier { _nestedNameSpecifierIdentifierPos    :: SourcePos
                                  , _nestedNameSpecifierIdentifierPrefix :: NestedNameSpecifier
                                  , _nestedNameSpecifierIdentifierValue  :: Id}
  | NestedNameSpecifierTemplate { _nestedNameSpecifierTemplatePos        :: SourcePos
                                , _nestedNameSpecifierTemplatePrefix     :: NestedNameSpecifier
                                , _nestedNameSpecifierTemplateIsTemplate :: Bool
                                , _nestedNameSpecifierTemplateValue      :: SimpleTemplateId }
  deriving (Show, Eq)

nestedNameSpecifier :: P NestedNameSpecifier
nestedNameSpecifier = undefined

-- expr.prim.lambda
-- lambda-expression:
--  	lambda-introducer lambda-declarator[opt] compound-statement     C++0x
-- lambda-introducer:
--  	[ lambda-capture[opt] ]     C++0x
-- lambda-capture:
--  	capture-default     C++0x
--  	capture-list     C++0x
--  	capture-default , capture-list     C++0x
-- capture-default:
--  	&     C++0x
--  	=     C++0x
-- capture-list:
--  	capture ...[opt]     C++0x
--  	capture-list , capture ...[opt]     C++0x
-- capture:
--  	identifier     C++0x
--  	& identifier     C++0x
--  	this     C++0x
-- lambda-declarator:
--  	( parameter-declaration-clause ) mutable[opt] exception-specification[opt] attribute-specifier-seq[opt] trailing-return-type[opt]     C++0x
lambdaExpression :: P Expression
lambdaExpression = undefined

data LambdaIntroducer = LambdaIntroducer
  { _lambdaIntroducerPos :: SourcePos
  , _lambdaIntroducerValue :: Maybe LambdaCapture
  } deriving (Show, Eq)

lambdaIntroducer :: P LambdaIntroducer
lambdaIntroducer = undefined

data LambdaCapture
  = LambdaCaptureDefault { _lambdaCaptureDefaultPos   :: SourcePos
                         , _lambdaCaptureDefaultValue :: CaptureDefault }
  | LambdaCaptureList { _lambdaCaptureListPos   :: SourcePos
                      , _lambdaCaptureListValue :: CaptureList }
  | LambdaCaptureDefaultAndList { _lambdaCaptureDefaultAndListPos    :: SourcePos
                                , _lambdaCaptureDefaultAndListFirst  :: CaptureDefault
                                , _lambdaCaptureDefaultAndListSecond :: CaptureList }
  deriving (Show, Eq)

lambdaCapture :: P LambdaCapture
lambdaCapture = undefined

data CaptureDefault = CaptureDefault
  { _captureDefaultPos  :: SourcePos
  , _captureDefaultType :: CaptureDefaultType
  } deriving (Show, Eq)

data CaptureDefaultType
  = CaptureByRef
  | CaptureByValue
  deriving (Show, Eq)

captureDefault :: P CaptureDefault
captureDefault = undefined

data CaptureList = CaptureList
  { _captureListPos         :: SourcePos
  , _captureListValue       :: [Capture]
  , _captureListHasThreeDot :: Bool
  } deriving (Show, Eq)

captureList :: P CaptureList
captureList = undefined

data Capture
  = CaptureIdentifier { _captureIdentifierRef   :: SourcePos
                      , _captureIdentifierValue :: Id }
  | CaptureRef { _captureRefPos   :: SourcePos
               , _captureRefValue :: Id }
  | CaptureThis { _captureThisPos :: SourcePos }
  deriving (Show, Eq)

capture :: P Capture
capture = undefined

data LambdaDeclarator = LambdaDeclarator
  { _lambdaDeclaratorPos                        :: SourcePos
  , _lambdaDeclaratorParameterDeclarationClause :: ParameterDeclarationClause
  , _lambdaDeclaratorIsMutable                  :: Bool
  , _lambdaDeclaratorExceptionSpecification     :: Maybe ExceptionSpecification
  , _lambdaDeclaratorAttributeSpecifierSeq      :: [AttributeSpecifier]
  , _lambdaDeclaratorTrailingReturnType         :: Maybe TrailingReturnType
  } deriving (Show, Eq)

lambdaDeclarator :: P LambdaDeclarator
lambdaDeclarator = undefined

-- expr.post
-- postfix-expression:
--  	primary-expression
--  	postfix-expression [ expression ]
--  	postfix-expression [ braced-init-list[opt] ]     C++0x
--  	postfix-expression ( expression-list[opt] )
--  	simple-type-specifier ( expression-list[opt] )
--  	typename-specifier ( expression-list[opt] )
--  	simple-type-specifier braced-init-list     C++0x
--  	typename-specifier braced-init-list     C++0x
--  	postfix-expression . template[opt] id-expression
--  	postfix-expression -> template[opt] id-expression
--  	postfix-expression . pseudo-destructor-name
--  	postfix-expression -> pseudo-destructor-name
--  	postfix-expression ++
--  	postfix-expression --
--  	dynamic_cast < type-id > ( expression )
--  	static_cast < type-id > ( expression )
--  	reinterpret_cast < type-id > ( expression )
--  	const_cast < type-id > ( expression )
--  	typeid ( expression )
--  	typeid ( type-id )
-- expression-list:
--  	initializer-list
-- pseudo-destructor-name:
--  	::opt nested-name-specifier[opt] type-name :: ~ type-name
--  	::opt nested-name-specifier template simple-template-id :: ~ type-name     C++0x
--  	::opt nested-name-specifier[opt] ~ type-name
--  	~ decltype-specifier     C++0x
postfixExpression :: P Expression
postfixExpression = undefined

data ExpressionList = ExpressionList
  { _expressionListPos   :: SourcePos
  , _expressionListValue :: InitializerList
  } deriving (Show, Eq)

expressionList :: P [Expression]
expressionList = undefined

data PseudoDestructorName
  --  	::opt nested-name-specifier[opt] type-name :: ~ type-name
  = PseudoDestructorNameNested { _pseudoDestructorNameNestedPos :: SourcePos
                               , _pseudoDestructorNameNestedName :: Maybe NestedNameSpecifier
                               , _pseudoDestructorNameNestedType :: TypeName
                               , _pseudoDestructorNameNestedDestructorName :: TypeName }
  --  	::opt nested-name-specifier template simple-template-id :: ~ type-name     C++0x
  | PseudoDestructorNameTemplate { _pseudoDestructorNameTemplatePos :: SourcePos
                                 , _pseudoDestructorNameTemplateNestedName :: NestedNameSpecifier
                                 , _pseudoDestructorNameTemplateId :: SimpleTemplateId
                                 , _pseudoDestructorNameTemplateType :: TypeName }
  --  	::opt nested-name-specifier[opt] ~ type-name
  | PseudoDestructorNameTypeName { _pseudoDestructorNameTypeNamePos :: SourcePos
                                 , _pseudoDestructorNameTypeNameNested :: NestedNameSpecifier
                                 , _pseudoDestructorNameTypeName :: TypeName }
  --  	~ decltype-specifier     C++0x
  | PseudoDestructorNameDecltype { _pseudoDestructorNameDecltypePos :: SourcePos
                                 , _pseudoDestructorNameDecltypeSpecifier :: DecltypeSpecifier }
  deriving (Show, Eq)

pseudoDestructorName :: P PseudoDestructorName
pseudoDestructorName = undefined

-- expr.unary
-- unary-expression:
--  	postfix-expression
--  	++ cast-expression
--  	-- cast-expression
--  	unary-operator cast-expression
--  	sizeof unary-expression
--  	sizeof ( type-id )
--  	sizeof ... ( identifier )     C++0x
--  	alignof ( type-id )     C++0x
--  	noexcept-expression     C++0x
--  	new-expression
--  	delete-expression
-- unary-operator:
--  	*
--  	&
--  	+
--  	-
--  	!
--  	~
unaryExpression :: P Expression
unaryExpression = undefined

data UnaryOperator = UnaryOperator
  { _unaryOperatorPos  :: SourcePos
  , _unaryOperatorType :: UnaryOperatorType
  } deriving (Show, Eq)

data UnaryOperatorType
  = Ptr
  | Ref
  | UnaryPlus
  | UnaryMinus
  | Not
  | Tilda
  deriving (Show, Eq)

unaryOperator :: P UnaryOperator
unaryOperator = undefined

-- expr.new
-- new-expression:
--  	::opt new new-placement[opt] new-type-id new-initializer[opt]
--  	::opt new new-placement[opt] ( type-id ) new-initializer[opt]
-- new-placement:
--  	( expression-list )
-- new-type-id:
--  	type-specifier-seq new-declarator[opt]
-- new-declarator:
--  	ptr-operator new-declarator[opt]
--  	noptr-new-declarator     C++0x
-- noptr-new-declarator:
--  	[ expression ] attribute-specifier-seq[opt]     C++0x
--  	noptr-new-declarator [ constant-expression ] attribute-specifier-seq[opt]     C++0x
-- new-initializer:
--  	( expression-list[opt] )
--  	braced-init-list     C++0x
newExpression :: P Expression
newExpression = undefined

newPlacement :: P ExpressionList
newPlacement = undefined

data NewTypeId = NewTypeId
  { _newTypeIdPos              :: SourcePos
  , _newTypeIdTypeSpecifierSeq :: TypeSpecifierSeq
  , _newTypeIdNewDeclarator    :: Maybe NewDeclarator
  } deriving (Show, Eq)

newTypeId :: P NewTypeId
newTypeId = undefined

data NewDeclarator
  = NewDeclaratorPtr { _newDeclaratorPtrPos      :: SourcePos
                     , _newDeclaratorPtrOperator :: PtrOperator
                     , _newDeclaratorPtrSuffix   :: Maybe NewDeclarator}
  | NewDeclaratorNoptr { _newDeclaratorNoptrPos   :: SourcePos
                       , _newDeclaratorNoptrValue :: NoptrNewDeclarator }
  deriving (Show, Eq)

newDeclarator :: P NewDeclarator
newDeclarator = undefined

data NoptrNewDeclarator
  = NoptrNewDeclarator { _noptrNewDeclaratorPos        :: SourcePos
                       , _noptrNewDeclaratorExpession  :: Expression
                       , _noptrNewDeclaratorAttributes :: [AttributeSpecifier]}
  | NoptrNewDeclaratorPrefixed { _noptrNewDeclaratorPrefixedPos        :: SourcePos
                               , _noptrNewDeclaratorPrefix             :: NoptrNewDeclarator
                               , _noptrNewDeclaratorPrefixedExpression :: Expression
                               , _noptrNewDeclaratorPrefixedAttributes :: [AttributeSpecifier] }
  deriving (Show, Eq)

noptrNewDeclarator :: P NoptrNewDeclarator
noptrNewDeclarator = undefined

data NewInitializer
  = NewInitializerExpressionList { _newInitializerExpressionListPos   :: SourcePos
                                 , _newInitializerExpressionListValue :: Maybe ExpressionList }
  | NewInitializerBracedList { _newInitializerBracedListPos   :: SourcePos
                             , _newInitializerBracedListValue :: BracedInitList }
  deriving (Show, Eq)

newInitializer :: P NewInitializer
newInitializer = undefined

-- expr.delete
-- delete-expression:
--  	::opt delete cast-expression
--  	::opt delete [ ] cast-expression
deleteExpression :: P Expression
deleteExpression = undefined

-- expr.unary.noexcept
-- noexcept-expression:
--  	noexcept ( expression )     C++0x
noexceptExpression :: P Expression
noexceptExpression = undefined

-- expr.cast
-- cast-expression:
--  	unary-expression
--  	( type-id ) cast-expression
castExpression :: P Expression
castExpression = undefined

-- expr.mptr.oper
-- pm-expression:
--  	cast-expression
--  	pm-expression .* cast-expression
--  	pm-expression ->* cast-expression
pmExpression :: P Expression
pmExpression = undefined

-- expr.mul
-- multiplicative-expression:
--  	pm-expression
--  	multiplicative-expression * pm-expression
--  	multiplicative-expression / pm-expression
--  	multiplicative-expression % pm-expression
data BinaryOperator
  = BinaryOperator { _binaryOperatorPos :: SourcePos
                   , _binaryOperatorType :: BinaryOperatorType }
  deriving (Show, Eq)

data BinaryOperatorType
  = Multiply
  | Divide
  | Remain
  | Plus
  | Minus
  | LeftShift
  | RightShift
  | Greater
  | Lesser
  | GreaterEqual
  | LesserEqual
  | Equal
  | NotEqual
  | BitAnd
  | BitXor
  | BitOr
  | LogicalAnd
  | LogicalOr
  deriving (Show, Eq)

multiplicativeExpression :: P Expression
multiplicativeExpression = undefined

-- expr.add
-- additive-expression:
--  	multiplicative-expression
--  	additive-expression + multiplicative-expression
--  	additive-expression - multiplicative-expression
additiveExpression :: P Expression
additiveExpression = undefined

-- expr.shift
-- shift-expression:
--  	additive-expression
--  	shift-expression << additive-expression
--  	shift-expression >> additive-expression
shiftExpression :: P Expression
shiftExpression = undefined

-- expr.rel
-- relational-expression:
--  	shift-expression
--  	relational-expression < shift-expression
--  	relational-expression > shift-expression
--  	relational-expression <= shift-expression
--  	relational-expression >= shift-expression
relationalExpression :: P Expression
relationalExpression = undefined

-- expr.eq
-- equality-expression:
--  	relational-expression
--  	equality-expression == relational-expression
--  	equality-expression != relational-expression
equalityExpression :: P Expression
equalityExpression = undefined

-- expr.bit.and
-- and-expression:
--  	equality-expression
--  	and-expression & equality-expression
andExpression :: P Expression
andExpression = undefined

-- expr.xor
-- exclusive-or-expression:
--  	and-expression
--  	exclusive-or-expression ^ and-expression
exclusiveOrExpression :: P Expression
exclusiveOrExpression = undefined

-- expr.or
-- inclusive-or-expression:
--  	exclusive-or-expression
--  	inclusive-or-expression | exclusive-or-expression
inclusiveOrExpression :: P Expression
inclusiveOrExpression = undefined

-- expr.log.and
-- logical-and-expression:
--  	inclusive-or-expression
--  	logical-and-expression && inclusive-or-expression
logicalAndExpression :: P Expression
logicalAndExpression = undefined

-- expr.log.or
-- logical-or-expression:
--  	logical-and-expression
--  	logical-or-expression || logical-and-expression
logicalOrExpression :: P Expression
logicalOrExpression = undefined

-- expr.cond
-- conditional-expression:
--  	logical-or-expression
--  	logical-or-expression ? expression : assignment-expression
conditionalExpression :: P Expression
conditionalExpression = undefined

-- expr.ass
-- assignment-expression:
--  	conditional-expression
--  	logical-or-expression assignment-operator initializer-clause     C++0x
--  	throw-expression
-- assignment-operator:
--  	=
--  	*=
--  	/=
--  	%=
--  	+=
--  	-=
--  	>>=
--  	<<=
--  	&=
--  	^=
--  	|=
assignmentExpression :: P Expression
assignmentExpression = undefined

data AssignmentOperator = AssignmentOperator
  { _assignmentOperatorPos  :: SourcePos
  , _assignmentOperatorType :: AssignmentOperatorType
  } deriving (Show, Eq)

data AssignmentOperatorType
  = Assign
  | AssignMultiply
  | AssignDivision
  | AssignRemain
  | AssignAdd
  | AssignSubstract
  | AssignRightShift
  | AssignLeftShift
  | AssignAnd
  | AssignXor
  | AssignOr
  deriving (Show, Eq)

assignmentOperator :: P AssignmentOperator
assignmentOperator = undefined

-- expr.comma
-- expression:
--  	assignment-expression
--  	expression , assignment-expression
expression :: P Expression
expression = undefined

-- expr.const
-- constant-expression:
--  	conditional-expression
constantExpression :: P Expression
constantExpression = undefined

-- stmt.stmt
-- statement:
--  	labeled-statement
--  	attribute-specifier-seq[opt] expression-statement     C++0x
--  	attribute-specifier-seq[opt] compound-statement     C++0x
--  	attribute-specifier-seq[opt] selection-statement     C++0x
--  	attribute-specifier-seq[opt] iteration-statement     C++0x
--  	attribute-specifier-seq[opt] jump-statement     C++0x
--  	declaration-statement
--  	attribute-specifier-seq[opt] try-block
data Statement
  --  	attribute-specifier-seq[opt] identifier : statement
  = LabeledStatement { _labeledStatementPos :: SourcePos
                     , _labeledStatementAttrubutes :: [AttributeSpecifier]
                     , _labeledStatementId :: Id
                     , _labeledStatementBlock :: Statement }
  --  	attribute-specifier-seq[opt] case constant-expression : statement
  | LabeledCaseStatement { _labeledCaseStatementPos :: SourcePos
                         , _labeledCaseStatementAttributes :: [AttributeSpecifier]
                         , _labeledCaseStatementExpression :: Expression
                         , _labeledCaseStatementBlock :: Statement }
  --  	attribute-specifier-seq[opt] default : statement
  | LabeledDefaultStatement { _labeledDefaultStatementPos :: SourcePos
                            , _labeledDefaultStatementAttributes :: [AttributeSpecifier]
                            , _labeledDefaultStatementBlock :: Statement }
  --  	attribute-specifier-seq[opt] expression-statement     C++0x
  --  	expression[opt] ;
  | ExpressionStatement { _expressionStatementPos :: SourcePos
                        , _expressionStatementAttributes :: [AttributeSpecifier]
                        , _expressionStatementValue :: Maybe Expression }
  --  	attribute-specifier-seq[opt] compound-statement     C++0x
  --  	{ statement-seq[opt] }
  | CompoundStatement { _compoundStatementPos :: SourcePos
                      , _compoundStatementAttributes :: [AttributeSpecifier]
                      , _compoundStatementBlock :: [Statement] }
  --  	attribute-specifier-seq[opt] selection-statement     C++0x
  --  	if ( condition ) statement
  | IfStatement { _ifStatementPos :: SourcePos
                , _ifStatementAttributes :: [AttributeSpecifier]
                , _ifStatementCondition :: Condition
                , _ifStatementBlock :: Statement }
  --  	if ( condition ) statement else statement
  | IfElseStatement { _ifElseStatementPos :: SourcePos
                    , _ifElseStatementAttributes :: [AttributeSpecifier]
                    , _ifElseStatementCondition :: Condition
                    , _ifElseStatementIfBlock :: Statement
                    , _ifElseStatementElseBlock :: Statement }
  --  	switch ( condition ) statement
  | SwitchStatement { _switchStatementPos :: SourcePos
                    , _switchStatementAttributes :: [AttributeSpecifier]
                    , _switchStatementCondition :: Condition
                    , _switchStatementBlock :: Statement }
  --  	attribute-specifier-seq[opt] iteration-statement     C++0x
  --  	while ( condition ) statement
  | WhileStatement { _whileStatementPos :: SourcePos
                   , _whileStatementAttributes :: [AttributeSpecifier]
                   , _whileStatementCondition :: Condition
                   , _whileStatementBlock :: Statement }
  --  	do statement while ( expression ) ;
  | DoWhileStatement { _doWhileStatementPos :: SourcePos
                     , _doWhileStatementAttributes :: [AttributeSpecifier]
                     , _doWhileStatementBlock :: Statement
                     , _doWhileStatementExpression :: Expression }
  --  	for ( for-init-statement condition[opt] ; expression[opt] ) statement
  | ForStatement { _forStatementPos :: SourcePos
                 , _forStatementAttributes :: [AttributeSpecifier]
                 , _forStatementInit :: ForInitStatement
                 , _forStatementCondition :: Maybe Condition
                 , _forStatementExpression :: Maybe Expression
                 , _forStatementBlock :: Statement }
  --  	for ( for-range-declaration : for-range-initializer ) statement     C++0x
  | ForRangeStatement { _forRangeStatementPos :: SourcePos
                      , _forRangeStatementAttributes :: [AttributeSpecifier]
                      , _forRangeStatementDeclaration :: ForRangeDeclaration
                      , _forRangeStatementInitializer :: ForRangeInitializer
                      , _forRangeStatementBlock :: Statement }
  --  	attribute-specifier-seq[opt] jump-statement     C++0x
  --  	break ;
  | BreakStatement { _breakStatementPos :: SourcePos
                   , _breakStatementAttributes :: [AttributeSpecifier] }
  --  	continue ;
  | ContinueStatement { _continueStatementPos :: SourcePos
                      , _continueStatementAttributes :: [AttributeSpecifier] }
  --  	return expression[opt] ;
  --  	return braced-init-list[opt] ;     C++0x
  | ReturnStatement { _returnStatementPos :: SourcePos
                    , _returnStatementAttributes :: [AttributeSpecifier]
                    , _returnStatementValue :: Maybe (Either Expression BracedInitList) }
  --  	goto identifier ;
  | GotoStatement { _gotoStatementPos :: SourcePos
                  , _gotoStatementAttributes :: [AttributeSpecifier]
                  , _gotoStatementLabel :: Id }
  --  	declaration-statement
  | DeclarationStatement { _declarationStatementPos :: SourcePos
                         , _declarationStatementValue :: Declaration }
  --  	attribute-specifier-seq[opt] try-block
  --  	try compound-statement handler-seq
  | TryStatement { _tryStatementPos :: SourcePos
                 , _tryStatementAttributes :: [AttributeSpecifier]
                 , _tryStatementBlock :: Statement
                 , _tryStatementHandlers :: [Handler] }
  deriving (Show, Eq)

statement :: P Statement
statement = undefined

-- stmt.label
-- labeled-statement:
--  	attribute-specifier-seq[opt] identifier : statement
--  	attribute-specifier-seq[opt] case constant-expression : statement
--  	attribute-specifier-seq[opt] default : statement
labeledStatement :: P Statement
labeledStatement = undefined

-- stmt.expr
-- expression-statement:
--  	expression[opt] ;
expressionStatement :: P Statement
expressionStatement = undefined

-- stmt.block
-- compound-statement:
--  	{ statement-seq[opt] }
-- statement-seq:
--  	statement
--  	statement-seq statement
compoundStatement :: P Statement
compoundStatement = undefined

statementSeq :: P [Statement]
statementSeq = undefined

-- stmt.select
-- selection-statement:
--  	if ( condition ) statement
--  	if ( condition ) statement else statement
--  	switch ( condition ) statement
-- condition:
--  	expression
--  	attribute-specifier-seq[opt] decl-specifier-seq declarator = initializer-clause     C++0x
--  	attribute-specifier-seq[opt] decl-specifier-seq declarator braced-init-list     C++0x
selectionStatement :: P Statement
selectionStatement = undefined

data Condition
  --  	expression
  = ExpressionCondition { _expressionConditionPos :: SourcePos
                        , _expressionConditionValue :: Expression }
  --  	attribute-specifier-seq[opt] decl-specifier-seq declarator = initializer-clause     C++0x
  | InitializerCondition { _initializerConditionPos :: SourcePos
                         , _initializerConditionAttributes :: [AttributeSpecifier]
                         , _initializerConditionDeclSpecifiers :: DeclSpecifierSeq
                         , _initializerConditionDeclarator :: Declarator
                         , _initializerConditionInitializer :: InitializerClause }
  --  	attribute-specifier-seq[opt] decl-specifier-seq declarator braced-init-list     C++0x
  | InitBracedCondition { _initBracedConditionPos :: SourcePos
                        , _initBracedConditionAttributes :: [AttributeSpecifier]
                        , _initBracedConditionDeclSpecifiers :: DeclSpecifierSeq
                        , _initBracedConditionDeclarator :: Declarator
                        , _initBracedConditionBracedInitList :: BracedInitList }
  deriving (Show, Eq)

condition :: P Condition
condition = undefined

-- stmt.iter
-- iteration-statement:
--  	while ( condition ) statement
--  	do statement while ( expression ) ;
--  	for ( for-init-statement condition[opt] ; expression[opt] ) statement
--  	for ( for-range-declaration : for-range-initializer ) statement     C++0x
-- for-init-statement:
--  	expression-statement
--  	simple-declaration
-- for-range-declaration:
--  	attribute-specifier-seq[opt] type-specifier-seq declarator     C++0x
-- for-range-initializer:
--  	expression
--    braced-init-list     C++0x
iterationStatement :: P Statement
iterationStatement = undefined

data ForInitStatement = ForInitStatement
  { _forInitStatementPos :: SourcePos
  , _forInitStatementValue :: Either Statement Declaration
  } deriving (Show, Eq)

forInitStatement :: P ForInitStatement
forInitStatement = undefined

data ForRangeDeclaration = ForRangeDeclaration
  { _forRangeDeclarationPos :: SourcePos
  , _forRangeDeclarationAttributes :: [AttributeSpecifier]
  , _forRangeDeclarationTypeSpecifiers :: TypeSpecifierSeq
  , _forRangeDeclarationDeclarator :: Declarator
  } deriving (Show, Eq)

forRangeDeclaration :: P ForRangeDeclaration
forRangeDeclaration = undefined

data ForRangeInitializer = ForRangeInitializer
  { _forRangeInitializerPos :: SourcePos
  , _forRangeInitializerValue :: Either Expression BracedInitList
  } deriving (Show, Eq)

forRangeInitializer :: P ForRangeInitializer
forRangeInitializer = undefined

-- stmt.jump
-- jump-statement:
--  	break ;
--  	continue ;
--  	return expression[opt] ;
--  	return braced-init-list[opt] ;     C++0x
--  	goto identifier ;
jumpStatement :: P Statement
jumpStatement = undefined

-- stmt.dcl
-- declaration-statement:
--  	block-declaration
declarationStatement :: P Statement
declarationStatement = undefined

-- dcl.dcl
-- declaration-seq:
--  	declaration
--  	declaration-seq declaration
-- declaration:
--  	block-declaration
--  	function-definition
--  	template-declaration
--  	explicit-instantiation
--  	explicit-specialization
--  	linkage-specification
--  	namespace-definition
--  	empty-declaration     C++0x
--  	attribute-declaration     C++0x
-- block-declaration:
--  	simple-declaration
--  	asm-definition
--  	namespace-alias-definition
--  	using-declaration
--  	using-directive
--  	static_assert-declaration     C++0x
--  	alias-declaration     C++0x
--  	opaque-enum-declaration     C++0x
-- alias-declaration:
--  	using identifier = type-id ;     C++0x
-- simple-declaration:
--  	attribute-specifier-seq[opt] decl-specifier-seq[opt] init-declarator-list[opt] ;     C++0x
-- static_assert-declaration:
--  	static_assert ( constant-expression , string-literal ) ;     C++0x
-- empty-declaration:
--  	;     C++0x
-- attribute-declaration:
--  	attribute-specifier-seq ;     C++0x
data Declaration
  --  	block-declaration
  --  	simple-declaration
  --  	attribute-specifier-seq[opt] decl-specifier-seq[opt] init-declarator-list[opt] ;     C++0x
  = SimpleDeclation { _simpleDeclationPos :: SourcePos
                    , _simpleDeclationAttributes :: [AttributeSpecifier]
                    , _simpleDeclationDeclSpecifiers :: DeclSpecifierSeq
                    , _simpleDeclationInitDeclarators :: [InitDeclarator] }
  --  	asm-definition
  --  	asm ( string-literal ) ;
  | AsmDefinition { _asmDefinitionPos :: SourcePos
                  , _asmDefinitionString :: Literal }
  --  	namespace-alias-definition
  --  	namespace identifier = qualified-namespace-specifier ;
  | NamespaceAliasDefinition { _namespaceAliasDefinitionPos :: SourcePos
                             , _namespaceAliasDefinitionId :: Id
                             , _namespaceAliasDefinitionSpecifier :: QualifiedNamespaceSpecifier }
  --  	using-declaration
  --  	using typename[opt] ::opt nested-name-specifier unqualified-id ;
  | UsingNestedDeclaration { _usingNestedDeclarationPos :: SourcePos
                           , _usingNestedDeclarationTypename :: Maybe TypeName
                           , _usingNestedDeclarationNameSpecifier :: NestedNameSpecifier
                           , _usingNestedDeclarationId :: UnqualifiedId }
  --  	using :: unqualified-id ;
  | UsingDeclaration { _usingDeclarationPos :: SourcePos
                     , _usingDeclarationId :: Id }
  --  	using-directive
  --  	attribute-specifier-seq[opt] using namespace ::opt nested-name-specifier[opt] namespace-name ;
  | UsingDirective { _usingDirectivePos :: SourcePos
                   , _usingDirectiveAttributes :: [AttributeSpecifier]
                   , _usingDirectiveNameSpecifier :: Maybe NestedNameSpecifier
                   , _usingDirectiveName :: NamespaceName }
  --  	static_assert-declaration     C++0x
  --  	static_assert ( constant-expression , string-literal ) ;     C++0x
  | StaticAssertDeclaration { _staticAssertDeclarationPos :: SourcePos
                            , _staticAssertDeclarationExpression :: Expression
                            , _staticAssertDeclarationString :: Literal }
  --  	alias-declaration     C++0x
  --  	using identifier = type-id ;     C++0x
  | AliasDeclaration { _aliasDeclarationPos :: SourcePos
                     , _aliasDeclarationId :: Id
                     , _aliasDeclarationType :: TypeId }
  --  	opaque-enum-declaration     C++0x
  --  	enum-key attribute-specifier-seq[opt] identifier enum-base[opt] ;     C++0x
  | OpaqueEnumDeclaration { _opaqueEnumDeclarationPos :: SourcePos
                          , _opaqueEnumDeclarationKey :: EnumKey
                          , _opaqueEnumDeclarationAttributes :: [AttributeSpecifier]
                          , _opaqueEnumDeclarationId :: Id
                          , _opaqueEnumDeclarationBase :: Maybe EnumBase }
  --  	function-definition
  --  	attribute-specifier-seq[opt] decl-specifier-seq[opt] declarator function-body     C++0x
  | FunctionDefinition { _functionDefinitionPos :: SourcePos
                       , _functionDefinitionAttributes :: [AttributeSpecifier]
                       , _functionDefinitionDeclSpecifiers :: DeclSpecifierSeq
                       , _functionDefinitionDeclarator :: Declarator
                       , _functionDefinitionBody :: FunctionBody }
  --  	attribute-specifier-seq[opt] decl-specifier-seq[opt] declarator = default ;     C++0x
  | FunctionDefaultDefinition { _functionDefaultDefinitionPos :: SourcePos
                              , _functionDefaultDefinitionAttributes :: [AttributeSpecifier]
                              , _functionDefaultDefinitionDeclSpecifiers :: DeclSpecifierSeq
                              , _functionDefaultDefinitionDeclarator :: Declarator }
  --  	attribute-specifier-seq[opt] decl-specifier-seq[opt] declarator = delete ;     C++0x
  | FunctionDeleteDefinition { _functionDeleteDefinitionPos :: SourcePos
                             , _functionDeleteDefinitionAttributes :: [AttributeSpecifier]
                             , _functionDeleteDefinitionDeclSpecifiers :: DeclSpecifierSeq
                             , _functionDeleteDefinitionDeclarator :: Declarator }
  --  	template-declaration
  --  	template < template-parameter-list > declaration     C++0x - The export keyword is reserved for future use
  | TemplateDeclaration { _templateDeclarationPos :: SourcePos
                        , _templateDeclarationParameters :: [TemplateParameter]
                        , _templateDeclarationDeclarator :: Declarator }
  --  	explicit-instantiation
  --  	extern[opt] template declaration     C++0x
  | ExplicitInstantiation { _explicitInstantiationPos :: SourcePos
                          , _explicitInstantiationHasExtern :: Bool
                          , _explicitInstantiationDeclaration :: Declaration }
  --  	explicit-specialization
  --  	template < > declaration
  | ExplicitSpecialization { _explicitSpecializationPos :: SourcePos
                           , _explicitSpecializationDeclaration :: Declaration }
  --  	linkage-specification
  --  	extern string-literal { declaration-seq[opt] }
  --  	extern string-literal declaration
  | LinkageSpecification { _linkageSpecificationPos :: SourcePos
                         , _linkageSpecificationString :: Literal
                         , _linkageSpecificationDeclaration :: Either Declaration [Declaration] }
  --  	namespace-definition
  | NamespaceDefinition { _namespaceDefinitionPos :: SourcePos
                        , _namespaceDefinitionValue :: Either NamedNamespaceDefinition UnnamedNamespaceDefinition }
  --  	empty-declaration     C++0x
  | EmptyDeclaration { _emptyDeclarationPos :: SourcePos }
  --  	attribute-declaration     C++0x
  --  	attribute-specifier-seq ;     C++0x
  | AttributeDeclaration { _attributeDeclarationPos :: SourcePos
                         , _attributeDeclarationAttributes :: [AttributeSpecifier] }
  deriving (Show, Eq)

declarationSeq :: P [Declaration]
declarationSeq = undefined

declaration :: P Declaration
declaration = undefined

blockDeclaration :: P Declaration
blockDeclaration = undefined

aliasDeclaration :: P Declaration
aliasDeclaration = undefined

simpleDeclaration :: P Declaration
simpleDeclaration = undefined

staticAssertDeclaration :: P Declaration
staticAssertDeclaration = undefined

emptyDeclaration :: P Declaration
emptyDeclaration = undefined

attributeDeclaration :: P Declaration
attributeDeclaration = undefined

-- dcl.spec
-- decl-specifier:
--  	storage-class-specifier
--  	type-specifier
--  	function-specifier
--  	friend
--  	typedef
--  	constexpr     C++0x
-- decl-specifier-seq:
--  	decl-specifier attribute-specifier-seq[opt]     C++0x
--  	decl-specifier decl-specifier-seq     C++0x
data DeclSpecifier
  = StorageDeclSpecifier { _storageDeclSpecifierPos :: SourcePos
                         , _storageDeclSpecifierValue :: StorageClassSpecifier }
  | TypeDeclSpecifier { _typeDeclSpecifierPos :: SourcePos
                      , _typeDeclSpecifierValue :: TypeSpecifier }
  | FunctionDeclSpecifier { _functionDeclSpecifierPos :: SourcePos
                          , _functionDeclSpecifierValue :: FunctionSpecifier }
  | FriendDeclSpecifier { _friendDeclSpecifierPos :: SourcePos }
  | TypedefDeclSpecifier { _typedefDeclSpecifierPos :: SourcePos }
  | ConstexprDeclSpecifier { _constexprDeclSpecifierPos :: SourcePos }
  deriving (Show, Eq)

declSpecifier :: P DeclSpecifier
declSpecifier = undefined

data DeclSpecifierSeq
  = AttributedDeclSpecifier { _attributedDeclSpecifierPos :: SourcePos
                            , _attributedDeclSpecifierAttributes :: [AttributeSpecifier]
                            , _attributedDeclSpecifierValue :: DeclSpecifier }
  | SimpleDeclSpecifierSeq { _simpleDeclSpecifierSeqPos :: SourcePos
                           , _simpleDeclSpecifier :: DeclSpecifier
                           , _simpleDeclSpecifierSeq :: DeclSpecifierSeq }
  deriving (Show, Eq)

declSpecifierSeq :: P DeclSpecifierSeq
declSpecifierSeq = undefined

-- dcl.stc
-- storage-class-specifier:
--  	auto     Removed in C++0x
--  	register
--  	static
--  	thread_local     C++0x
--  	extern
--  	mutable
data StorageClassSpecifier
  = AutoSpecifier { _autoSpecifierPos :: SourcePos }
  | RegisterSpecifier { _registerSpecifierPos :: SourcePos }
  | StaticSpecifier { _staticSpecifierPos :: SourcePos }
  | ThreadLocalSpecifier { _threadLocalSpecifierPos :: SourcePos }
  | ExternSpecifier { _externSpecifierPos :: SourcePos }
  | MutableSpecifier { _mutableSpecifierPos :: SourcePos }
  deriving (Show, Eq)

storageClassSpecifier :: P StorageClassSpecifier
storageClassSpecifier = undefined

-- dcl.fct.spec
-- function-specifier:
--  	inline
--  	virtual
--  	explicit
data FunctionSpecifier
  = InlineSpecifier { _inlineSpecifierPos :: SourcePos }
  | VirtualSpecifier { _virtualSpecifierPos :: SourcePos }
  | ExplicitSpecifier { _explicitSpecifierPos :: SourcePos }
  deriving (Show, Eq)

functionSpecifier :: P FunctionSpecifier
functionSpecifier = undefined

-- dcl.typedef
-- typedef-name:
--  	identifier
data TypedefName = TypedefName
  { _typedefNamePos :: SourcePos
  , _typedefNameId :: Id
  } deriving (Show, Eq)

typedefName :: P TypedefName
typedefName = undefined

-- dcl.type
-- type-specifier:
--  	trailing-type-specifier
--  	class-specifier
--  	enum-specifier
-- trailing-type-specifier:
--  	simple-type-specifier
--  	elaborated-type-specifier
--  	typename-specifier
--  	cv-qualifier
-- type-specifier-seq:
--  	type-specifier attribute-specifier-seq[opt]     C++0x
--  	type-specifier type-specifier-seq
-- trailing-type-specifier-seq:
--  	trailing-type-specifier attribute-specifier-seq[opt]     C++0x
--  	trailing-type-specifier trailing-type-specifier-seq     C++0x
data TypeSpecifier
  = TypeSpecifierTrailing { _typeSpecifierTrailingPos :: SourcePos
                          , _typeSpecifierTrailingValue :: TrailingTypeSpecifier }
  | TypeSpecifierClass { _typeSpecifierClassPos :: SourcePos
                       , _typeSpecifierClassValue :: ClassSpecifier }
  | TypeSpecifierEnum { _typeSpecifierEnumPos :: SourcePos
                      , _typeSpecifierEnumValue :: EnumSpecifier }
  deriving (Show, Eq)

typeSpecifier :: P TypeSpecifier
typeSpecifier = undefined

data TrailingTypeSpecifier
  = TrailingTypeSpecifierSimple { _trailingTypeSpecifierSimplePos :: SourcePos
                                , _trailingTypeSpecifierSimpleValue :: SimpleTypeSpecifier }
  | TrailingTypeSpecifierElaborated { _trailingTypeSpecifierElaboratedPos :: SourcePos
                                    , _trailingTypeSpecifierElaboratedValue :: ElaboratedTypeSpecifier }
  | TrailingTypeSpecifierTypename { _trailingTypeSpecifierTypenamePos :: SourcePos
                                  , _trailingTypeSpecifierTypenameValue :: TypenameSpecifier }
  | TrailingTypeSpecifierCV { _trailingTypeSpecifierCVPos :: SourcePos
                            , _trailingTypeSpecifierCVValue :: CvQualifier }
  deriving (Show, Eq)

trailingTypeSpecifier :: P TrailingTypeSpecifier
trailingTypeSpecifier = undefined

data TypeSpecifierSeq
  = TypeSpecifierAttributed { _typeSpecifierAttributedPos :: SourcePos
                            , _typeSpecifierAttributes :: [AttributeSpecifier]
                            , _typeSpecifierAttributedValue :: TypeSpecifier }
  | TypeSpecifierSeq { _typeSpecifierSeqPos :: SourcePos
                     , _typeSpecifierSeq :: TypeSpecifierSeq
                     , _typeSpecifierValue :: TypeSpecifier }
  deriving (Show, Eq)

typeSpecifierSeq :: P TypeSpecifierSeq
typeSpecifierSeq = undefined

data TrailingTypeSpecifierSeq
  = TrailingTypeSpecifierAttributed { _trailingTypeSpecifierAttributedPos :: SourcePos
                                    , _trailingTypeSpecifierAttributes :: [AttributeSpecifier]
                                    , _trailingTypeSpecifierAttributedValue :: TrailingTypeSpecifier }
  | TrailingTypeSpecifierSeq { _trailingTypeSpecifierSeqPos :: SourcePos
                             , _trailingTypeSpecifierValue :: TrailingTypeSpecifier
                             , _trailingTypeSpecifierSeq :: TrailingTypeSpecifierSeq }
  deriving (Show, Eq)

trailingTypeSpecifierSeq :: P TrailingTypeSpecifierSeq
trailingTypeSpecifierSeq = undefined

-- dct.type.simple
-- simple-type-specifier:
--  	::opt nested-name-specifier[opt] type-name
--  	::opt nested-name-specifier template simple-template-id
--  	char
--  	char16_t     C++0x
--  	char32_t     C++0x
--  	wchar_t
--  	bool
--  	short
--  	int
--  	long
--  	signed
--  	unsigned
--  	float
--  	double
--  	void
--  	auto     C++0x
--  	decltype-specifier     C++0x
-- type-name:
--  	class-name
--  	enum-name
--  	typedef-name
--  	simple-template-id     C++0x
-- decltype-specifier:
--  	decltype ( expression )     C++0x
data SimpleTypeSpecifier
  = SimpleTypeSpecifier { _simpleTypeSpecifierPos :: SourcePos
                        , _simpleTypeSpecifierNestedName :: Maybe NestedNameSpecifier
                        , _simpleTypeSpecifierTypeName :: TypeName }
  | SimpleTypeSpecifierTemplate { _simpleTypeSpecifierTemplatePos :: SourcePos
                                , _simpleTypeSpecifierTemplateNestedName :: NestedNameSpecifier
                                , _simpleTypeSpecifierTemplateId :: SimpleTemplateId }
  | SimpleTypeSpecifierChar { _simpleTypeSpecifierCharPos :: SourcePos}
  | SimpleTypeSpecifierChar16T { _simpleTypeSpecifierChar16TPos :: SourcePos }
  | SimpleTypeSpecifierChar32T { _simpleTypeSpecifierChar32TPos :: SourcePos }
  | SimpleTypeSpecifierWcharT { _simpleTypeSpecifierWcharTPos :: SourcePos }
  | SimpleTypeSpecifierBool { _simpleTypeSpecifierBoolPos :: SourcePos }
  | SimpleTypeSpecifierShort { _simpleTypeSpecifierShortPos :: SourcePos }
  | SimpleTypeSpecifierInt { _simpleTypeSpecifierIntPos :: SourcePos }
  | SimpleTypeSpecifierLong { _simpleTypeSpecifierLongPos :: SourcePos }
  | SimpleTypeSpecifierSigned { _simpleTypeSpecifierSignedPos :: SourcePos }
  | SimpleTypeSpecifierUnsigned { _simpleTypeSpecifierUnsignedPos :: SourcePos }
  | SimpleTypeSpecifierFloat { _simpleTypeSpecifierFloatPos :: SourcePos }
  | SimpleTypeSpecifierDouble { _simpleTypeSpecifierDoublePos :: SourcePos }
  | SimpleTypeSpecifierVoid { _simpleTypeSpecifierVoidPos :: SourcePos }
  | SimpleTypeSpecifierAuto { _simpleTypeSpecifierAutoPos :: SourcePos }
  | SimpleTypeSpecifierDecltype { _simpleTypeSpecifierDecltypePos :: SourcePos
                                , _simpleTypeSpecifierDecltypeValue :: DecltypeSpecifier }
  deriving (Show, Eq)

simpleTypeSpecifier :: P SimpleTypeSpecifier
simpleTypeSpecifier = undefined

data TypeName
  = TypeNameClass { _typeNameClassPos :: SourcePos
                  , _typeNameClassValue :: ClassName }
  | TypeNameEnum { _typeNameEnumPos :: SourcePos
                 , _typeNameEnumValue :: EnumName }
  | TypeNameTypedef { _typeNameTypedefPos :: SourcePos
                    , _typeNameTypedefValue :: TypedefName }
  | TypeNameTemplate { _typeNameTemplatePos :: SourcePos
                     , _typeNameTemplateValue :: SimpleTemplateId }
  deriving (Show, Eq)

typeName :: P TypeName
typeName = undefined

data DecltypeSpecifier = DecltypeSpecifier
  { _decltypeSpecifierPos :: SourcePos
  , _decltypeSpecifierValue :: Expression
  } deriving (Show, Eq)

decltypeSpecifier :: P DecltypeSpecifier
decltypeSpecifier = undefined

-- dcl.type.elab
-- elaborated-type-specifier:
--  	class-key attribute-specifier-seq[opt] ::opt nested-name-specifier[opt] identifier
--  	class-key ::opt nested-name-specifier[opt] template[opt] simple-template-id
--  	enum ::opt nested-name-specifier[opt] identifier
data ElaboratedTypeSpecifier
  = ElaboratedTypeSpecifierId { _elaboratedTypeSpecifierIdPos :: SourcePos
                              , _elaboratedTypeSpecifierIdClassKey :: ClassKey
                              , _elaboratedTypeSpecifierIdAttributes :: [AttributeSpecifier]
                              , _elaboratedTypeSpecifierIdNestedName :: Maybe NestedNameSpecifier
                              , _elaboratedTypeSpecifierId :: Id }
  | ElaboratedTypeSpecifierTemplate { _elaboratedTypeSpecifierTemplatePos :: SourcePos
                                    , _elaboratedTypeSpecifierTemplateClassKey :: ClassKey
                                    , _elaboratedTypeSpecifierTemplateNestedName :: Maybe NestedNameSpecifier
                                    , _elaboratedTypeSpecifierHasTemplate :: Bool
                                    , _elaboratedTypeSpecifierTemplateId :: SimpleTemplateId }
  | ElaboratedTypeSpecifierEnum { _elaboratedTypeSpecifierEnumPos :: SourcePos
                                , _elaboratedTypeSpecifierEnumNestedName :: Maybe NestedNameSpecifier
                                , _elaboratedTypeSpecifierEnumId :: Id }
  deriving (Show, Eq)

elaboratedTypeSpecifier :: P ElaboratedTypeSpecifier
elaboratedTypeSpecifier = undefined

-- dcl.enum
-- enum-name:
--  	identifier
-- enum-specifier:
--  	enum-head { enumerator-list[opt] }     C++0x
--  	enum-head { enumerator-list , }     C++0x
-- enum-head:
--  	enum-key attribute-specifier-seq[opt] identifier[opt] enum-base[opt]     C++0x
--  	enum-key attribute-specifier-seq[opt] nested-name-specifier identifier enum-base[opt]     CD0x
-- opaque-enum-declaration:
--  	enum-key attribute-specifier-seq[opt] identifier enum-base[opt] ;     C++0x
-- enum-key:
--  	enum     C++0x
--  	enum class     C++0x
--  	enum struct     C++0x
-- enum-base:
--  	: type-specifier-seq     C++0x
-- enumerator-list:
--  	enumerator-definition     C++0x
--  	enumerator-list , enumerator-definition     C++0x
-- enumerator-definition:
--  	enumerator
--  	enumerator = constant-expression
-- enumerator:
--  	identifier
data EnumName = EnumName
  { _enumNamePos :: SourcePos
  , _enumNameValue :: Id
  } deriving (Show, Eq)

enumName :: P EnumName
enumName = undefined

data EnumSpecifier = EnumSpecifier
  { _enumSpecifierPos :: SourcePos
  , _enumSpecifierHead :: EnumHead
  , _enumSpecifierEnumerators :: [Enumerator]
  , _enumSpecifierHasDot :: Bool
  } deriving (Show, Eq)

enumSpecifier :: P EnumSpecifier
enumSpecifier = undefined

data EnumHead
  --  	enum-key attribute-specifier-seq[opt] identifier[opt] enum-base[opt]     C++0x
  = EnumHead { _enumHeadKey :: SourcePos
             , _enumHeadAttributes :: [AttributeSpecifier]
             , _enumHeadId :: Maybe Id
             , _enumHeadBase :: Maybe EnumBase }
  --  	enum-key attribute-specifier-seq[opt] nested-name-specifier identifier enum-base[opt]     CD0x
  | EnumHeadNested { _enumHeadNestedPos :: SourcePos
                   , _enumHeadNestedKey :: EnumKey
                   , _enumHeadNestedAttributes :: [AttributeSpecifier]
                   , _enumHeadNestedName :: NestedNameSpecifier
                   , _enumHeadNestedId :: Id
                   , _enumHeadNestedBase :: Maybe EnumBase }

  deriving (Show, Eq)

enumHead :: P EnumHead
enumHead = undefined

opaqueEnumDeclaration :: P Declaration
opaqueEnumDeclaration = undefined

data EnumKeyType
  = Enum
  | EnumClass
  | EnumStruct
  deriving (Show, Eq)

data EnumKey = EnumKey
  { _enumKeyPos :: SourcePos
  , _enumKeyType :: EnumKeyType
  } deriving (Show, Eq)

enumKey :: P EnumKey
enumKey = undefined

data EnumBase = EnumBase
  { _enumBasePos :: SourcePos
  , _enumBaseValue :: TypeSpecifierSeq
  } deriving (Show, Eq)

enumBase :: P EnumBase
enumBase = undefined

enumeratorList :: P [EnumeratorDefinition]
enumeratorList = undefined

data EnumeratorDefinition
  = EnumeratorDefinition { _enumeratorDefinitionPos :: SourcePos
                         , _enumeratorDefinition :: Enumerator }
  | EnumeratorDefinitionWithValue { _enumeratorDefinitionWithValuePos :: SourcePos
                                  , _enumeratorDefinitionWithValueId :: Enumerator
                                  , _enumeratorDefinitionValue :: Expression }
  deriving (Show, Eq)

enumeratorDefinition :: P EnumeratorDefinition
enumeratorDefinition = undefined

data Enumerator = Enumerator
  { _enumeratorPos :: SourcePos
  , _enumeratorValue :: Id
  } deriving (Show, Eq)

enumerator :: P Enumerator
enumerator = undefined

-- namespace.def
-- namespace-name:
--  	original-namespace-name
--  	namespace-alias
-- original-namespace-name:
--  	identifier
-- namespace-definition:
--  	named-namespace-definition
--  	unnamed-namespace-definition
-- named-namespace-definition:
--  	original-namespace-definition
--  	extension-namespace-definition
-- original-namespace-definition:
--  	inline[opt] namespace identifier { namespace-body }     C++0x
-- extension-namespace-definition:
--  	inline[opt] namespace original-namespace-name { namespace-body }     C++0xD
-- unnamed-namespace-definition:
--  	inline[opt] namespace { namespace-body }
-- namespace-body:
--  	declaration-seq[opt]
data NamespaceName = NamespaceName
  { _namespaceNamePos :: SourcePos
  , _namespaceNameValue :: Either OriginalNamespaceName NamespaceAlias
  } deriving (Show, Eq)

namespaceName :: P NamespaceName
namespaceName = undefined

data OriginalNamespaceName = OriginalNamespaceName
  { _originalNamespaceNamePos :: SourcePos
  , _originalNamespaceNameValue :: Id
  } deriving (Show, Eq)

originalNamespaceName :: P OriginalNamespaceName
originalNamespaceName = undefined

namespaceDefinition :: P Declaration
namespaceDefinition = undefined

data NamedNamespaceDefinition = NamedNamespaceDefinition
  { _namedNamespaceDefinitionPos :: SourcePos
  , _namedNamespaceDefinitionValue :: Either OriginalNamespaceDefinition ExtensionNamespaceDefinition
  } deriving (Show, Eq)

namedNamespaceDefinition :: P NamedNamespaceDefinition
namedNamespaceDefinition = undefined

data OriginalNamespaceDefinition = OriginalNamespaceDefinition
  { _originalNamespaceDefinitionPos :: SourcePos
  , _originalNamespaceDefinitionIsInline :: Bool
  , _originalNamespaceDefinitionId :: Id
  , _originalNamespaceDefinitionBody :: [Declaration]
  } deriving (Show, Eq)

originalNamespaceDefinition :: P OriginalNamespaceDefinition
originalNamespaceDefinition = undefined

data ExtensionNamespaceDefinition = ExtensionNamespaceDefinition
  { _extensionNamespaceDefinitionPos :: SourcePos
  , _extensionNamespaceDefinitionIsInline :: Bool
  , _extensionNamespaceDefinitionOriginal :: OriginalNamespaceName
  , _extensionNamespaceDefinitionBody :: [Declaration]
  } deriving (Show, Eq)

extensionNamespaceDefinition :: P ExtensionNamespaceDefinition
extensionNamespaceDefinition = undefined

data UnnamedNamespaceDefinition = UnnamedNamespaceDefinition
  { _unnamedNamespaceDefinitionPos :: SourcePos
  , _unnamedNamespaceDefinitionIsInline :: Bool
  , _unnamedNamespaceDefinitionBody :: [Declaration]
  } deriving (Show, Eq)

unnamedNamespaceDefinition :: P UnnamedNamespaceDefinition
unnamedNamespaceDefinition = undefined

namespaceBody :: P [Declaration]
namespaceBody = undefined

-- namespace.alias
-- namespace-alias:
--  	identifier
-- namespace-alias-definition:
--  	namespace identifier = qualified-namespace-specifier ;
-- qualified-namespace-specifier:
--  	::opt nested-name-specifier[opt] namespace-name
data NamespaceAlias = NamespaceAlias
  { _namespaceAliasPos :: SourcePos
  , _namespaceAliasValue :: Id
  } deriving (Show, Eq)

namespaceAlias :: P NamespaceAlias
namespaceAlias = undefined

namespaceAliasDefinition :: P Declaration
namespaceAliasDefinition = undefined

data QualifiedNamespaceSpecifier = QualifiedNamespaceSpecifier
  { _qualifiedNamespaceSpecifierPos :: SourcePos
  , _qualifiedNamespaceSpecifierNestedName :: Maybe NestedNameSpecifier
  , _qualifiedNamespaceSpecifierName :: NamespaceName
  } deriving (Show, Eq)

qualifiedNamespaceSpecifier :: P QualifiedNamespaceSpecifier
qualifiedNamespaceSpecifier = undefined

-- namespace.udecl
-- using-declaration:
--  	using typename[opt] ::opt nested-name-specifier unqualified-id ;
--  	using :: unqualified-id ;
usingDeclaration :: P Declaration
usingDeclaration = undefined

-- namespace.udir
-- using-directive:
--  	attribute-specifier-seq[opt] using namespace ::opt nested-name-specifier[opt] namespace-name ;
usingDirective :: P Declaration
usingDirective = undefined

-- dcl.asm
-- asm-definition:
--  	asm ( string-literal ) ;
asmDefinition :: P Declaration
asmDefinition = undefined

-- dcl.link
-- linkage-specification:
--  	extern string-literal { declaration-seq[opt] }
--  	extern string-literal declaration
linkageSpecification :: P Declaration
linkageSpecification = undefined

-- dcl.attr.grammar
-- attribute-specifier-seq:
--  	attribute-specifier     C++0x
--  	attribute-specifier-seq attribute-specifier     C++0x
-- attribute-specifier:
--  	[ [ attribute-list ] ]     C++0x
--  	alignment-specifier     C++0x
-- alignment-specifier:
--  	alignas ( type-id ...opt )     C++0x
--  	alignas ( alignment-expression ...opt )     C++0x
-- attribute-list:
--  	attribute[opt]     C++0x
--  	attribute-list , attribute[opt]     C++0x
--  	attribute ...     C++0x
--  	attribute-list , attribute ...     C++0x
-- attribute:
--  	attribute-token attribute-argument-clause[opt]     C++0x
-- attribute-token:
--  	identifier     C++0x
--  	attribute-scoped-token     C++0x
-- attribute-scoped-token:
--  	attribute-namespace :: identifier     C++0x
-- attribute-namespace:
--  	identifier     C++0x
-- attribute-argument-clause:
--  	( balanced-token-seq )     C++0x
-- balanced-token-seq:
--  	balanced-token     C++0x
--  	balanced-token-seq balanced-token     C++0x
-- balanced-token:
--  	( balanced-token-seq )     C++0x
--  	[ balanced-token-seq ]     C++0x
--  	{ balanced-token-seq }     C++0x
--  	token     C++0x - except a parenthesis, a bracket, or a brace
attributeSpecifierSeq :: P [AttributeSpecifier]
attributeSpecifierSeq = undefined

data AttributeSpecifier
  = AttributeSpecifier { _attributeSpecifierPos :: SourcePos
                       , _attributeSpecifierValue :: AttributeList }
  | AlignmentAttribute { _alignmentAttributePos :: SourcePos
                       , _alignmentAttributeValue :: AlignmentSpecifier }
  deriving (Show, Eq)

attributeSpecifier :: P AttributeSpecifier
attributeSpecifier = undefined

data AlignmentSpecifier
  = AlignAsType { _alignAsTypePos :: SourcePos
                , _alignAsTypeValue :: TypeId
                , _alignAsTypeThreeDot :: Bool }
  | AlignAsExpression { _alignAsExpressionPos :: SourcePos
                      , _alignAsExpressionValue :: Expression
                      , _alignAsExpressionThreeDot :: Bool }
  deriving (Show, Eq)

alignmentSpecifier :: P AlignmentSpecifier
alignmentSpecifier = undefined

data AttributeList = AttributeList
  { _attributeListPos :: SourcePos
  , _attributeListValue :: [Attribute]
  , _attributeListThreeDot :: Bool
  } deriving (Show, Eq)

attributeList :: P AttributeList
attributeList = undefined

data Attribute = Attribute
  { _attributePos :: SourcePos
  , _attributeToken :: AttributeToken
  , _attributeArgumentClause :: Maybe AttributeArgumentClause
  } deriving (Show, Eq)

attribute :: P Attribute
attribute = undefined

data AttributeToken = AttributeToken
  { _attributeTokenPos :: SourcePos
  , _attributeTokenValue :: Either Id AttributeScopedToken
  } deriving (Show, Eq)

attributeToken :: P AttributeToken
attributeToken = undefined

data AttributeScopedToken = AttributeScopedToken
  { _attributeScopedTokenPos :: SourcePos
  , _attributeScopedTokenNamespace :: AttributeNamespace
  , _attributeScopedTokenId :: Id
  } deriving (Show, Eq)

attributeScopedToken :: P AttributeScopedToken
attributeScopedToken = undefined

data AttributeNamespace = AttributeNamespace
  { _attributeNamespacePos :: SourcePos
  , _attributeNamespaceValue :: Id
  } deriving (Show, Eq)

attributeNamespace :: P AttributeNamespace
attributeNamespace = undefined

data AttributeArgumentClause = AttributeArgumentClause
  { _attributeArgumentClausePos :: SourcePos
  , _attributeArgumentClauseValue :: [BalancedToken]
  } deriving (Show, Eq)

attributeArgumentClause :: P AttributeArgumentClause
attributeArgumentClause = undefined

balancedTokenSeq :: P [BalancedToken]
balancedTokenSeq = undefined

data BalancedToken
  = ParencedTokenSeq { _parencedTokenSeqPos :: SourcePos
                     , _parencedTokenSeqValue :: [BalancedToken] }
  | SquaredTokenSeq { _squaredTokenSeqPos :: SourcePos
                    , _squaredTokenSeqValue :: [BalancedToken] }
  | BracedTokenSeq { _bracedTokenSeqPos :: SourcePos
                   , _bracedTokenSeqValue :: [BalancedToken] }
  | BalancedToken { _balancedTokenPos :: SourcePos
                  , _balancedTokenValue :: CppToken }
  deriving (Show, Eq)

balancedToken :: P BalancedToken
balancedToken = undefined

-- dcl.decl
-- init-declarator-list:
--  	init-declarator
--  	init-declarator-list , init-declarator
-- init-declarator:
--  	declarator initializer[opt]
-- declarator:
--  	ptr-declarator     C++0x
--  	noptr-declarator parameters-and-qualifiers trailing-return-type     C++0x
-- ptr-declarator:
--  	noptr-declarator     C++0x
--  	ptr-operator ptr-declarator     C++0x
-- noptr-declarator:
--  	declarator-id attribute-specifier-seq[opt]     C++0x
--  	noptr-declarator parameters-and-qualifiers     C++0x
--  	noptr-declarator [ constant-expression[opt] ] attribute-specifier-seq[opt]     C++0x
--  	( ptr-declarator )     C++0x
-- parameters-and-qualifiers:
--  	( parameter-declaration-clause ) attribute-specifier-seq[opt] cv-qualifier-seq[opt] ref-qualifier[opt] exception-specification[opt]     C++0x
-- trailing-return-type:
--  	-> trailing-type-specifier-seq abstract-declarator[opt]     C++0x
-- ptr-operator:
--  	* attribute-specifier-seq[opt] cv-qualifier-seq[opt]     C++0x
--  	& attribute-specifier-seq[opt]     C++0x
--  	&& attribute-specifier-seq[opt]     C++0x
--  	::opt nested-name-specifier * attribute-specifier-seq[opt] cv-qualifier-seq[opt]     C++0x
-- cv-qualifier-seq:
--  	cv-qualifier
--  	cv-qualifier cv-qualifier-seq
-- cv-qualifier:
--  	const
--  	volatile
-- ref-qualifier:
--  	&     C++0x
--  	&&     C++0x
-- declarator-id:
--  	...opt id-expression     C++0x
--  	::opt nested-name-specifier[opt] class-name     C++0x
initDeclaratorList :: P [InitDeclarator]
initDeclaratorList = undefined

data InitDeclarator = InitDeclarator
  { _initDeclaratorPos :: SourcePos
  , _initDeclaratorValue :: Declarator
  , _initDeclaratorInitializer :: Maybe Initializer
  } deriving (Show, Eq)

initDeclarator :: P InitDeclarator
initDeclarator = undefined

data Declarator
  = DeclaratorPtr { _declaratorPtrPos :: SourcePos
                  , _declaratorPtrValue :: PtrDeclarator }
  | DeclaratorNoptr { _declaratorNoptrPos :: SourcePos
                    , _declaratorNoptrValue :: NoptrDeclarator
                    , _declaratorNoptrParameters :: ParametersAndQualifiers
                    , _declaratorNoptrReturnType :: TrailingReturnType }
  deriving (Show, Eq)

declarator :: P Declarator
declarator = undefined

data PtrDeclarator
  = PtrDeclaratorNoptr { _ptrDeclaratorNoptrPos :: SourcePos
                       , _ptrDeclaratorNoptrValue :: NoptrDeclarator }
  | PtrDeclarator { _ptrDeclaratorPos :: SourcePos
                  , _ptrDeclaratorOperator :: PtrOperator
                  , _ptrDeclaratorValue :: PtrDeclarator }
  deriving (Show, Eq)

ptrDeclarator :: P PtrDeclarator
ptrDeclarator = undefined

data NoptrDeclarator
  = NoptrDeclarator { _noptrDeclaratorPos :: SourcePos
                    , _noptrDeclaratorValue :: DeclaratorId
                    , _noptrDeclaratorAttributes :: [AttributeSpecifier] }
  | NoptrDeclaratorParametred { _noptrDeclaratorParametredPos :: SourcePos
                              , _noptrDeclaratorParametredValue :: NoptrDeclarator
                              , _noptrDeclaratorParameters :: ParametersAndQualifiers }
  | NoptrDeclaratorIndexed { _noptrDeclaratorIndexedPos :: SourcePos
                           , _noptrDeclaratorIndexedValue :: NoptrDeclarator
                           , _noptrDeclaratorIndex :: Expression
                           , _noptrDeclaratorIndexedAttributes :: [AttributeSpecifier] }
  | NoptrDeclaratorParenced { _noptrDeclaratorParencedPos :: SourcePos
                            , _noptrDeclaratorParencedValue :: PtrDeclarator }
  deriving (Show, Eq)

noptrDeclarator :: P NoptrDeclarator
noptrDeclarator = undefined

data ParametersAndQualifiers = ParametersAndQualifiers
  { _parametersAndQualifiersPos :: SourcePos
  , _parametersAndQualifiersParameterDeclarations :: ParameterDeclarationClause
  , _parametersAndQualifiersAttributes :: [AttributeSpecifier]
  , _parametersAndQualifiersCv :: [CvQualifier]
  , _parametersAndQualifiersRef :: Maybe RefQualifier
  , _parametersAndQualifiersException :: Maybe ExceptionSpecification
  } deriving (Show, Eq)

parametersAndQualifiers :: P ParametersAndQualifiers
parametersAndQualifiers = undefined

data TrailingReturnType = TrailingReturnType
  { _trailingReturnType :: SourcePos
  , _trailingReturnTypeSpecifier :: TrailingTypeSpecifierSeq
  , _trailingReturnTypeDeclarator :: Maybe AbstractDeclarator
  } deriving (Show, Eq)

trailingReturnType :: P TrailingReturnType
trailingReturnType = undefined

data PtrOperator
  = StarOperator { _starOperatorPos :: SourcePos
                 , _starOperatorAttributes :: [AttributeSpecifier]
                 , _starOperatorQualifiers :: [CvQualifier] }
  | RefOperator { _refOperatorPos :: SourcePos
                , _refOperatorAttributes :: [AttributeSpecifier] }
  | DoubleRefOperator { _doubleRefOperatorPos :: SourcePos
                      , _doubleRefOperatorAttributes :: [AttributeSpecifier] }
  | NestedStarOperator { _nestedStarOperatorPos :: SourcePos
                       , _nestedStarOperatorNestedName :: NestedNameSpecifier
                       , _nestedStarOperatorAttributes :: [AttributeSpecifier]
                       , _nestedStarOperatorQualifiers :: [CvQualifier] }
  deriving (Show, Eq)

ptrOperator :: P PtrOperator
ptrOperator = undefined

cvQualifierSeq :: P [CvQualifier]
cvQualifierSeq = undefined

data CvQualifier
  = ConstQualifier { _constQualifierPos :: SourcePos }
  | VolatileQualifier { _volatileQualifierPos :: SourcePos }
  deriving (Show, Eq)

cvQualifier :: P CvQualifier
cvQualifier = undefined

data RefQualifier
  = RefQualifier { _refQualifierPos :: SourcePos }
  | RefQualifierDouble { _refQualifierDoublePos :: SourcePos }
  deriving (Show, Eq)

refQualifier :: P RefQualifier
refQualifier = undefined

data DeclaratorId
  = DeclaratorIdExpression { _declaratorIdExpressionPos :: SourcePos
                           , _declaratorIdExpressionHasThreeDot :: Bool
                           , _declaratorIdExpressionValue :: Expression }
  | DeclaratorIdClass { _declaratorIdClassPos :: SourcePos
                      , _declaratorIdClassHasSquareDot :: Bool
                      , _declaratorIdClassNestedName :: Maybe NestedNameSpecifier
                      , _declaratorIdClassName :: ClassName }
  deriving (Show, Eq)

declaratorId :: P DeclaratorId
declaratorId = undefined

-- dcl.name
-- type-id:
--  	type-specifier-seq abstract-declarator[opt]
-- abstract-declarator:
--  	ptr-abstract-declarator     C++0x
--  	noptr-abstract-declarator[opt] parameters-and-qualifiers trailing-return-type     C++0x
--  	...     C++0x
-- ptr-abstract-declarator:
--  	noptr-abstract-declarator     C++0x
--  	ptr-operator ptr-abstract-declarator[opt]     C++0x
-- noptr-abstract-declarator:
--  	noptr-abstract-declarator[opt] parameters-and-qualifiers     C++0x
--  	noptr-abstract-declarator[opt] [ constant-expression ] attribute-specifier-seq[opt]     C++0x
--  	( ptr-abstract-declarator )     C++0x
data TypeId = TypeId
  { _typeIdPos :: SourcePos
  , _typeIdSpecifiers :: TypeSpecifierSeq
  , _typeIdDeclarator :: Maybe AbstractDeclarator
  } deriving (Show, Eq)

typeId :: P TypeId
typeId = undefined

data AbstractDeclarator
  = AbstractDeclaratorPtr { _abstractDeclaratorPtrPos :: SourcePos
                          , _abstractDeclaratorPtrValue :: PtrAbstractDeclarator }
  | AbstractDeclaratorNoptr { _abstractDeclaratorNoptrPos :: SourcePos
                            , _abstractDeclaratorNoptrValue :: Maybe NoptrAbstractDeclarator
                            , _abstractDeclaratorNoptrParameters :: ParametersAndQualifiers
                            , _abstractDeclaratorNoptrTralingReturnType :: TrailingReturnType }
  | AbstractDeclaratorThreeDot { _abstractDeclaratorThreeDotPos :: SourcePos }
  deriving (Show, Eq)

abstractDeclarator :: P AbstractDeclarator
abstractDeclarator = undefined

data PtrAbstractDeclarator
  = PtrAbstractDeclaratorNoptr { _ptrAbstractDeclaratorNoptrPos :: SourcePos
                               , _ptrAbstractDeclaratorNoptrValue :: NoptrAbstractDeclarator }
  | PtrAbstractDeclarator { _ptrAbstractDeclaratorPos :: SourcePos
                          , _ptrAbstractDeclaratorOperator :: PtrOperator
                          , _ptrAbstractDeclaratorValue :: Maybe PtrAbstractDeclarator }
  deriving (Show, Eq)

ptrAbstractDeclarator :: P PtrAbstractDeclarator
ptrAbstractDeclarator = undefined

data NoptrAbstractDeclarator
  = NoptrAbstractDeclarator { _noptrAbstractDeclaratorPos :: SourcePos
                            , _noptrAbstractDeclaratorPref :: Maybe NoptrAbstractDeclarator
                            , _noptrAbstractDeclaratorParameters :: ParametersAndQualifiers }
  | ArrayNoptrAbstractDeclarator { _arrayNoptrAbstractDeclaratorPos :: SourcePos
                                 , _arrayNoptrAbstractDeclaratorPref :: Maybe NoptrAbstractDeclarator
                                 , _arrayNoptrAbstractDeclaratorExpression :: Expression
                                 , _arrayNoptrAbstractDeclaratorAttributes :: [AttributeSpecifier] }
  | ParensedPtrAbstractDeclarator { _parensedPtrAbstractDeclaratorPos :: SourcePos
                                  , _parensedPtrAbstractDeclaratorValue :: PtrAbstractDeclarator }
  deriving (Show, Eq)

noptrAbstractDeclarator :: P NoptrAbstractDeclarator
noptrAbstractDeclarator = undefined

-- dcl.fct
-- parameter-declaration-clause:
--  	parameter-declaration-list[opt] ...opt
--  	parameter-declaration-list , ...
-- parameter-declaration-list:
--  	parameter-declaration
--  	parameter-declaration-list , parameter-declaration
-- parameter-declaration:
--  	attribute-specifier-seq[opt] decl-specifier-seq declarator     C++0x
--  	attribute-specifier-seq[opt] decl-specifier-seq declarator = initializer-clause     C++0x
--  	attribute-specifier-seq[opt] decl-specifier-seq abstract-declarator[opt]     C++0x
--  	attribute-specifier-seq[opt] decl-specifier-seq abstract-declarator[opt] = initializer-clause     C++0x
data ParameterDeclarationClause = ParameterDeclarationClause
  { _parameterDeclarationClausePos :: SourcePos
  , _parameterDeclarationClauseList :: [ParameterDeclaration]
  , _parameterDeclarationClauseHasThreeDots :: Bool
  } deriving (Show, Eq)

parameterDeclarationClause :: P ParameterDeclarationClause
parameterDeclarationClause = undefined

parameterDeclarationList :: P [ParameterDeclaration]
parameterDeclarationList = undefined

data ParameterDeclaration
  = ParameterDeclaration { _parameterDeclarationPos :: SourcePos
                         , _parameterDeclarationAttributes :: [AttributeSpecifier]
                         , _parameterDeclarationDeclSpecifiers :: DeclSpecifierSeq
                         , _parameterDeclarationDeclarator :: Declarator }
  | ParameterDeclarationInitialized { _parameterDeclarationInitializedPos :: SourcePos
                                    , _parameterDeclarationInitializedAttributes :: [AttributeSpecifier]
                                    , _parameterDeclarationInitializedDeclSpecifiers :: DeclSpecifierSeq
                                    , _parameterDeclarationInitializedDeclarator :: Declarator
                                    , _parameterDeclarationInitializedInitializers :: InitializerClause }
  | AbstractParameterDeclaration { _abstractParameterDeclarationPos :: SourcePos
                                 , _abstractParameterDeclarationAttributes :: [AttributeSpecifier]
                                 , _abstractParameterDeclarationDeclSpecifiers :: DeclSpecifierSeq
                                 , _abstractParameterDeclarationDeclarator :: Maybe AbstractDeclarator }
  | AbstractParameterDeclarationInitialized { _abstractParameterDeclarationInitializedPos :: SourcePos
                                            , _abstractParameterDeclarationInitializedAttributes :: [AttributeSpecifier]
                                            , _abstractParameterDeclarationInitializedDeclSpecifiers :: DeclSpecifierSeq
                                            , _abstractParameterDeclarationInitializedDeclarator :: Maybe AbstractDeclarator
                                            , _abstractParameterDeclarationInitializedInitializers :: InitializerClause }
  deriving (Show, Eq)

parameterDeclaration :: P ParameterDeclaration
parameterDeclaration = undefined

-- dcl.fct.def.general
-- function-definition:
--  	attribute-specifier-seq[opt] decl-specifier-seq[opt] declarator function-body     C++0x
--  	attribute-specifier-seq[opt] decl-specifier-seq[opt] declarator = default ;     C++0x
--  	attribute-specifier-seq[opt] decl-specifier-seq[opt] declarator = delete ;     C++0x
-- function-body:
--  	ctor-initializer[opt] compound-statement     C++0x
--  	function-try-block     C++0x
functionDefinition :: P Declaration
functionDefinition = undefined

data FunctionBody
  = CtorBody { _ctorBodyPos :: SourcePos
             , _ctorBodyInitializer :: Maybe CtorInitializer
             , _ctorBodyValue :: Statement }
  | FunctionBody { _functionBodyPos :: SourcePos
                 , _functionBodyValue :: FunctionTryBlock }
  deriving (Show, Eq)

functionBody :: P FunctionBody
functionBody = undefined

-- dcl.init
-- initializer:
--  	brace-or-equal-initializer     C++0x
--  	( expression-list )     C++0x
-- brace-or-equal-initializer:
--  	= initializer-clause     C++0x
--  	braced-init-list     C++0x
-- initializer-clause:
--  	assignment-expression     C++0x
--  	braced-init-list     C++0x
-- initializer-list:
--  	initializer-clause ...opt     C++0x
--  	initializer-list , initializer-clause ...opt     C++0x
-- braced-init-list:
--  	{ initializer-list ,opt }     C++0x
--  	{ }     C++0x
data Initializer
  = Initializer { _initializerPos :: SourcePos
                , _initializerValue :: BraceOrEqualInitializer }
  | ParensedExpressionList { _parensedExpressionListPos :: SourcePos
                           , _parensedExpressionListValue :: ExpressionList }
  deriving (Show, Eq)

initializer :: P Initializer
initializer = undefined

data BraceOrEqualInitializer
  = EqualInitializer { _equalInitializerPos :: SourcePos
                     , _equalInitializerValue :: InitializerClause }
  | BraceInitializer { _braceInitializerPos :: SourcePos
                     , _braceInitializerValue :: BracedInitList }
  deriving (Show, Eq)

braceOrEqualInitializer :: P BraceOrEqualInitializer
braceOrEqualInitializer = undefined

data InitializerClause
  = ExpressionInitializerClause { _expressionInitializerClausePos :: SourcePos
                                , _expressionInitializerClauseValue :: Expression }
  | ListInitializerClause { _listInitializerClausePos :: SourcePos
                          , _listInitializerClauseValue :: BracedInitList }
  deriving (Show, Eq)

initializerClause :: P InitializerClause
initializerClause = undefined

data InitializerList = InitializerList
  { _initializerListPos :: SourcePos
  , _initializerListClauses :: [InitializerClause]
  , _initializerListHasThreeDots :: Bool
  } deriving (Show, Eq)

initializerList :: P InitializerList
initializerList = undefined

data BracedInitList
  = BracedInitList { _bracedInitListPos :: SourcePos
                   , _bracedInitListValue :: InitializerList
                   , _bracedInitListHasTrailingComma :: Bool }
  | EmptyBracedInitList { _emptyBracedInitListPos :: SourcePos }
  deriving (Show, Eq)

bracedInitList :: P BracedInitList
bracedInitList = undefined

-- class
-- class-name:
--  	identifier
--  	simple-template-id     C++0x
-- class-specifier:
--  	class-head { member-specification[opt] }
-- class-head:
--  	class-key attribute-specifier-seq[opt] class-head-name class-virt-specifier-seq[opt] base-clause[opt]     C++0x
--  	class-key attribute-specifier-seq[opt] base-clause[opt]     C++0x
-- class-head-name:
--  	nested-name-specifier[opt] class-name     C++0x
-- class-virt-specifier-seq:
--  	class-virt-specifier     C++0x
--  	class-virt-specifier-seq class-virt-specifier     C++0x
-- class-virt-specifier:
--  	final     C++0x
--  	explicit     C++0x
-- class-key:
--  	class
--  	struct
--  	union
data ClassName =
  ClassName
  deriving (Show, Eq)

className :: P ClassName
className = undefined

data ClassSpecifier =
  ClassSpecifier
  deriving (Show, Eq)

classSpecifier :: P ClassSpecifier
classSpecifier = undefined

data ClassHead =
  ClassHead
  deriving (Show, Eq)

classHead :: P ClassHead
classHead = undefined

data ClassHeadName =
  ClassHeadName
  deriving (Show, Eq)

classHeadName :: P ClassHeadName
classHeadName = undefined

classVirtSpecifierSeq :: P [ClassVirtSpecifier]
classVirtSpecifierSeq = undefined

data ClassVirtSpecifier =
  ClassVirtSpecifier
  deriving (Show, Eq)

classVirtSpecifier :: P ClassVirtSpecifier
classVirtSpecifier = undefined

data ClassKey =
  ClassKey
  deriving (Show, Eq)

classKey :: P ClassKey
classKey = undefined

-- class.mem
-- member-specification:
--  	member-declaration member-specification[opt]
--  	access-specifier : member-specification[opt]
-- member-declaration:
--  	attribute-specifier-seq[opt] decl-specifier-seq[opt] member-declarator-list[opt] ;     C++0x
--  	function-definition ;[opt]
--  	using-declaration
--  	static_assert-declaration     C++0x
--  	template-declaration
--  	alias-declaration     C++0x
-- member-declarator-list:
--  	member-declarator
--  	member-declarator-list , member-declarator
-- member-declarator:
--  	declarator virt-specifier-seq[opt] pure-specifier[opt]
--  	declarator virt-specifier-seq[opt] brace-or-equal-initializer[opt]     C++0x
--  	identifier[opt] attribute-specifier-seq[opt] virt-specifier-seq[opt] : constant-expression
-- virt-specifier-seq:
--  	virt-specifier
--  	virt-specifier-seq virt-specifier
-- virt-specifier:
--  	override
--  	final
--  	new
-- pure-specifier:
--  	= 0
data MemberSpecification =
  MemberSpecification
  deriving (Show, Eq)

memberSpecification :: P MemberSpecification
memberSpecification = undefined

data MemberDeclaration =
  MemberDeclaration
  deriving (Show, Eq)

memberDeclaration :: P MemberDeclaration
memberDeclaration = undefined

memberDeclaratorList :: P [MemberDeclarator]
memberDeclaratorList = undefined

data MemberDeclarator =
  MemberDeclarator
  deriving (Show, Eq)

memberDeclarator :: P MemberDeclarator
memberDeclarator = undefined

virtSpecifierSeq :: P [VirtSpecifier]
virtSpecifierSeq = undefined

data VirtSpecifier =
  VirtSpecifier
  deriving (Show, Eq)

virtSpecifier :: P VirtSpecifier
virtSpecifier = undefined

data PureSpecifier =
  PureSpecifier
  deriving (Show, Eq)

pureSpecifier :: P PureSpecifier
pureSpecifier = undefined

-- class.derived
-- base-clause:
--  	: base-specifier-list
-- base-specifier-list:
--  	base-specifier ...opt     C++0x
--  	base-specifier-list , base-specifier ...opt     C++0x
-- base-specifier:
--  	attribute-specifier-seq[opt] base-type-specifier     C++0x
--  	attribute-specifier-seq[opt] virtual access-specifier[opt] base-type-specifier     C++0x
--  	attribute-specifier-seq[opt] access-specifier virtual[opt] base-type-specifier     C++0x
-- class-or-decltype:
--  	::opt nested-name-specifier[opt] class-name     C++0x
--  	decltype-specifier     C++0x
-- base-type-specifier:
--  	class-or-decltype     C++0x
-- access-specifier:
--  	private
--  	protected
--  	public
data BaseClause =
  BaseClause
  deriving (Show, Eq)

baseClause :: P BaseClause
baseClause = undefined

data BaseSpecifierList =
  BaseSpecifierList
  deriving (Show, Eq)

baseSpecifierList :: P BaseSpecifierList
baseSpecifierList = undefined

data BaseSpecifier =
  BaseSpecifier
  deriving (Show, Eq)

baseSpecifier :: P BaseSpecifier
baseSpecifier = undefined

data ClassOrDecltype =
  ClassOrDecltype
  deriving (Show, Eq)

classOrDecltype :: P ClassOrDecltype
classOrDecltype = undefined

data BaseTypeSpecifier =
  BaseTypeSpecifier
  deriving (Show, Eq)

baseTypeSpecifier :: P BaseTypeSpecifier
baseTypeSpecifier = undefined

data AccessSpecifier =
  AccessSpecifier
  deriving (Show, Eq)

accessSpecifier :: P AccessSpecifier
accessSpecifier = undefined

-- class.conv.fct
-- conversion-function-id:
--  	operator conversion-type-id
-- conversion-type-id:
--  	type-specifier-seq conversion-declarator[opt]
-- conversion-declarator:
--  	ptr-operator conversion-declarator[opt]
data ConversionFunctionId =
  ConversionFunctionId
  deriving (Show, Eq)

conversionFunctionId :: P ConversionFunctionId
conversionFunctionId = undefined

data ConversionTypeId =
  ConversionTypeId
  deriving (Show, Eq)

conversionTypeId :: P ConversionTypeId
conversionTypeId = undefined

data ConversionDeclarator =
  ConversionDeclarator
  deriving (Show, Eq)

conversionDeclarator :: P ConversionDeclarator
conversionDeclarator = undefined

-- class.base.init
-- ctor-initializer:
--  	: mem-initializer-list
-- mem-initializer-list:
--  	mem-initializer ...opt     C++0x
--  	mem-initializer , mem-initializer-list ...opt     C++0x
-- mem-initializer:
--  	mem-initializer-id ( expression-list[opt] )
--  	mem-initializer-id braced-init-list     C++0x
-- mem-initializer-id:
--  	class-or-decltype
--  	identifier
data CtorInitializer =
  CtorInitializer
  deriving (Show, Eq)

ctorInitializer :: P CtorInitializer
ctorInitializer = undefined

data MemInitializerList =
  MemInitializerList
  deriving (Show, Eq)

memInitializerList :: P MemInitializerList
memInitializerList = undefined

data MemInitializer =
  MemInitializer
  deriving (Show, Eq)

memInitializer :: P MemInitializer
memInitializer = undefined

data MemInitializerId =
  MemInitializerId
  deriving (Show, Eq)

memInitializerId :: P MemInitializerId
memInitializerId = undefined

-- over.oper
-- operator-function-id:	See C++ Standard Core Language Issue n. 189
--  	operator overloadable-operator
--  	operator overloadable-operator < template-argument-list[opt] >
-- overloadable-operator:	See C++ Standard Core Language Issue n. 189
--  	new
--  	delete
--  	new [ ]
--  	delete [ ]
--  	+
--  	-
--  	*
--  	/
--  	%
--  	^
--  	&
--  	|
--  	~
--  	!
--  	=
--  	<
--  	>
--  	+=
--  	-=
--  	*=
--  	/=
--  	%=
--  	^=
--  	&=
--  	|=
--  	<<
--  	>>
--  	>>=
--  	<<=
--  	==
--  	!=
--  	<=
--  	>=
--  	&&
--  	||
--  	++
--  	--
--  	,
--  	->*
--  	->
--  	()
--  	[]
data OperatorFunctionId =
  OperatorFunctionId
  deriving (Show, Eq)

operatorFunctionId :: P OperatorFunctionId
operatorFunctionId = undefined

data OverloadableOperator =
  OverloadableOperator
  deriving (Show, Eq)

overloadableOperator :: P OverloadableOperator
overloadableOperator = undefined

-- over.literal
-- literal-operator-id:
--  	operator "" identifier     C++0x
data LiteralOperatorId =
  LiteralOperatorId
  deriving (Show, Eq)

literalOperatorId :: P LiteralOperatorId
literalOperatorId = undefined

-- temp
-- template-declaration:
--  	template < template-parameter-list > declaration     C++0x - The export keyword is reserved for future use
-- template-parameter-list:
--  	template-parameter
--  	template-parameter-list , template-parameter
templateDeclaration :: P Declaration
templateDeclaration = undefined

templateParameterList :: P [TemplateParameter]
templateParameterList = undefined

-- temp.param
-- template-parameter:
--  	type-parameter
--  	parameter-declaration
-- type-parameter:
--  	class ...opt identifier[opt]     C++0x
--  	class identifier[opt] = type-id
--  	typename ...opt identifier[opt]     C++0x
--  	typename identifier[opt] = type-id
--  	template < template-parameter-list > class ...opt identifier[opt]     C++0x
--  	template < template-parameter-list > class identifier[opt] = id-expression
data TemplateParameter =
  TemplateParameter
  deriving (Show, Eq)

templateParameter :: P TemplateParameter
templateParameter = undefined

data TypeParameter =
  TypeParameter
  deriving (Show, Eq)

typeParameter :: P TypeParameter
typeParameter = undefined

-- temp.names
-- simple-template-id:
--  	template-name < template-argument-list[opt] >     C++0x
-- template-id:
--  	simple-template-id     C++0x
--  	operator-function-id < template-argument-list[opt] >     C++0x
--  	literal-operator-id < template-argument-list[opt] >     C++0x
-- template-name:
--  	identifier
-- template-argument-list:
--  	template-argument ...opt     C++0x
--  	template-argument-list , template-argument ...opt     C++0x
-- template-argument:
--  	constant-expression     C++0x
--  	type-id     C++0x
--  	id-expression     C++0x
data SimpleTemplateId =
  SimpleTemplateId
  deriving (Show, Eq)

simpleTemplateId :: P SimpleTemplateId
simpleTemplateId = undefined

data TemplateId =
  TemplateId
  deriving (Show, Eq)

templateId :: P TemplateId
templateId = undefined

data TemplateName =
  TemplateName
  deriving (Show, Eq)

templateName :: P TemplateName
templateName = undefined

data TemplateArgumentList =
  TemplateArgumentList
  deriving (Show, Eq)

templateArgumentList :: P TemplateArgumentList
templateArgumentList = undefined

data TemplateArgument =
  TemplateArgument
  deriving (Show, Eq)

templateArgument :: P TemplateArgument
templateArgument = undefined

-- temp.res
-- typename-specifier:
--  	typename ::opt nested-name-specifier identifier     C++0x
--  	typename ::opt nested-name-specifier template[opt] simple-template-id     C++0x
data TypenameSpecifier =
  TypenameSpecifier
  deriving (Show, Eq)

typenameSpecifier :: P TypenameSpecifier
typenameSpecifier = undefined

-- temp.explicit
-- explicit-instantiation:
--  	extern[opt] template declaration     C++0x
explicitInstantiation :: P Declaration
explicitInstantiation = undefined

-- temp.expl.spec
-- explicit-specialization:
--  	template < > declaration
explicitSpecialization :: P Declaration
explicitSpecialization = undefined

-- except
-- try-block:
--  	try compound-statement handler-seq
-- function-try-block:
--  	try ctor-initializer[opt] compound-statement handler-seq     C++0x
-- handler-seq:
--  	handler
--  	handler handler-seq
-- handler:
--  	catch ( exception-declaration ) compound-statement
-- exception-declaration:
--  	attribute-specifier-seq[opt] type-specifier-seq declarator     C++0x
--  	attribute-specifier-seq[opt] type-specifier-seq abstract-declarator[opt]     C++0x
--  	...     C++0x
-- throw-expression:
--  	throw assignment-expression[opt]
data TryBlock =
  TryBlock
  deriving (Show, Eq)

tryBlock :: P TryBlock
tryBlock = undefined

data FunctionTryBlock =
  FunctionTryBlock
  deriving (Show, Eq)

functionTryBlock :: P FunctionTryBlock
functionTryBlock = undefined

handlerSeq :: P [Handler]
handlerSeq = undefined

data Handler =
  Handler
  deriving (Show, Eq)

handler :: P Handler
handler = undefined

data ExceptionDeclaration =
  ExceptionDeclaration
  deriving (Show, Eq)

exceptionDeclaration :: P ExceptionDeclaration
exceptionDeclaration = undefined

throwExpression :: P Expression
throwExpression = undefined

-- except.spec
-- exception-specification:
--  	dynamic-exception-specification     C++0x
--  	noexcept-specification     C++0x
-- dynamic-exception-specification:
--  	throw ( type-id-list[opt] )     C++0x
-- type-id-list:
--  	type-id ...opt     C++0x
--  	type-id-list , type-id ...opt     C++0x
-- noexcept-specification:
--  	noexcept ( constant-expression )     C++0x
--  	noexcept     C++0x
data ExceptionSpecification =
  ExceptionSpecification
  deriving (Show, Eq)

exceptionSpecification :: P ExceptionSpecification
exceptionSpecification = undefined

data DynamicExceptionSpecification =
  DynamicExceptionSpecification
  deriving (Show, Eq)

dynamicExceptionSpecification :: P DynamicExceptionSpecification
dynamicExceptionSpecification = undefined

data TypeIdList =
  TypeIdList
  deriving (Show, Eq)

typeIdList :: P TypeIdList
typeIdList = undefined

data NoexceptSpecification =
  NoexceptSpecification
  deriving (Show, Eq)

noexceptSpecification :: P NoexceptSpecification
noexceptSpecification = undefined
