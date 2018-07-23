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
  -- unary expression
  -- new expression
  -- delete expression
  -- noexcept expression
  -- cast expression
  -- binary expression
  -- conditional expression
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
  = PseudoDestructorName1 {}
  | PseudoDestructorName2 {}
  | PseudoDestructorName3 {}
  | PseudoDestructorName4 {}
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
  = LabeledStatement
  | ExpressionStatement
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
--  	attribute-specifier-seqopt decl-specifier-seq declarator = initializer-clause     C++0x
--  	attribute-specifier-seqopt decl-specifier-seq declarator braced-init-list     C++0x
selectionStatement :: P Statement
selectionStatement = undefined

data Condition =
  Condition
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
--  	expression braced-init-list     C++0x
iterationStatement :: P Statement
iterationStatement = undefined

data ForInitStatement =
  ForInitStatement
  deriving (Show, Eq)

forInitStatement :: P ForInitStatement
forInitStatement = undefined

data ForRangeDeclaration =
  ForRangeDeclaration
  deriving (Show, Eq)

forRangeDeclaration :: P ForRangeDeclaration
forRangeDeclaration = undefined

data ForRangeInitializer =
  ForRangeInitializer
  deriving (Show, Eq)

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
data Declaration =
  Declaration
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
data DeclSpecifier =
  DeclSpecifier
  deriving (Show, Eq)

declSpecifier :: P DeclSpecifier
declSpecifier = undefined

declSpecifierSeq :: P [DeclSpecifier]
declSpecifierSeq = undefined

-- dcl.stc
-- storage-class-specifier:
--  	auto     Removed in C++0x
--  	register
--  	static
--  	thread_local     C++0x
--  	extern
--  	mutable
data StorageClassSpecifier =
  StorageClassSpecifier
  deriving (Show, Eq)

storageClassSpecifier :: P StorageClassSpecifier
storageClassSpecifier = undefined

-- dcl.fct.spec
-- function-specifier:
--  	inline
--  	virtual
--  	explicit
data FunctionSpecifier =
  FunctionSpecifier
  deriving (Show, Eq)

functionSpecifier :: P FunctionSpecifier
functionSpecifier = undefined

-- dcl.typedef
-- typedef-name:
--  	identifier
data TypedefName =
  TypedefName
  deriving (Show, Eq)

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
data TypeSpecifier =
  TypeSpecifier
  deriving (Show, Eq)

typeSpecifier :: P TypeSpecifier
typeSpecifier = undefined

data TrailingTypeSpecifier =
  TrailingTypeSpecifier
  deriving (Show, Eq)

trailingTypeSpecifier :: P TrailingTypeSpecifier
trailingTypeSpecifier = undefined

data TypeSpecifierSeq =
  TypeSpecifierSeq
  deriving (Show, Eq)

typeSpecifierSeq :: P TypeSpecifierSeq
typeSpecifierSeq = undefined

data TrailingTypeSpecifierSeq =
  TrailingTypeSpecifierSeq
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
data SimpleTypeSpecifier =
  SimpleTypeSpecifier
  deriving (Show, Eq)

simpleTypeSpecifier :: P SimpleTypeSpecifier
simpleTypeSpecifier = undefined

data TypeName =
  TypeName
  deriving (Show, Eq)

typeName :: P TypeName
typeName = undefined

data DecltypeSpecifier =
  DecltypeSpecifier
  deriving (Show, Eq)

decltypeSpecifier :: P DecltypeSpecifier
decltypeSpecifier = undefined

-- dcl.type.elab
-- elaborated-type-specifier:
--  	class-key attribute-specifier-seq[opt] ::opt nested-name-specifier[opt] identifier
--  	class-key ::opt nested-name-specifier[opt] template[opt] simple-template-id
--  	enum ::opt nested-name-specifier[opt] identifier
data ElaboratedTypeSpecifier =
  ElaboratedTypeSpecifier
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
data EnumName =
  EnumName
  deriving (Show, Eq)

enumName :: P EnumName
enumName = undefined

data EnumSpecifier =
  EnumSpecifier
  deriving (Show, Eq)

enumSpecifier :: P EnumSpecifier
enumSpecifier = undefined

data EnumHead =
  EnumHead
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

data EnumKey =
  EnumKey
  deriving (Show, Eq)

enumKey :: P EnumKey
enumKey = undefined

data EnumBase =
  EnumBase
  deriving (Show, Eq)

enumBase :: P EnumBase
enumBase = undefined

enumeratorList :: P [EnumeratorDefinition]
enumeratorList = undefined

data EnumeratorDefinition =
  EnumeratorDefinition
  deriving (Show, Eq)

enumeratorDefinition :: P EnumeratorDefinition
enumeratorDefinition = undefined

data Enumerator =
  Enumerator
  deriving (Show, Eq)

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
data NamespaceName =
  NamespaceName
  deriving (Show, Eq)

namespaceName :: P NamespaceName
namespaceName = undefined

data OriginalNamespaceName =
  OriginalNamespaceName
  deriving (Show, Eq)

originalNamespaceName :: P OriginalNamespaceName
originalNamespaceName = undefined

namespaceDefinition :: P Declaration
namespaceDefinition = undefined

data NamedNamespaceDefinition =
  NamedNamespaceDefinition
  deriving (Show, Eq)

namedNamespaceDefinition :: P NamedNamespaceDefinition
namedNamespaceDefinition = undefined

data OriginalNamespaceDefinition =
  OriginalNamespaceDefinition
  deriving (Show, Eq)

originalNamespaceDefinition :: P OriginalNamespaceDefinition
originalNamespaceDefinition = undefined

data ExtensionNamespaceDefinition =
  ExtensionNamespaceDefinition
  deriving (Show, Eq)

extensionNamespaceDefinition :: P ExtensionNamespaceDefinition
extensionNamespaceDefinition = undefined

data UnnamedNamespaceDefinition =
  UnnamedNamespaceDefinition
  deriving (Show, Eq)

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
data NamespaceAlias =
  NamespaceAlias
  deriving (Show, Eq)

namespaceAlias :: P NamespaceAlias
namespaceAlias = undefined

namespaceAliasDefinition :: P Declaration
namespaceAliasDefinition = undefined

data QualifiedNamespaceSpecifier =
  QualifiedNamespaceSpecifier
  deriving (Show, Eq)

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

data AttributeSpecifier =
  AttributeSpecifier
  deriving (Show, Eq)

attributeSpecifier :: P AttributeSpecifier
attributeSpecifier = undefined

data AlignmentSpecifier =
  AlignmentSpecifier
  deriving (Show, Eq)

alignmentSpecifier :: P AlignmentSpecifier
alignmentSpecifier = undefined

data AttributeList =
  AttributeList
  deriving (Show, Eq)

attributeList :: P AttributeList
attributeList = undefined

data Attribute =
  Attribute
  deriving (Show, Eq)

attribute :: P Attribute
attribute = undefined

data AttributeToken =
  AttributeToken
  deriving (Show, Eq)

attributeToken :: P AttributeToken
attributeToken = undefined

data AttributeScopedToken =
  AttributeScopedToken
  deriving (Show, Eq)

attributeScopedToken :: P AttributeScopedToken
attributeScopedToken = undefined

data AttributeNamespace =
  AttributeNamespace
  deriving (Show, Eq)

attributeNamespace :: P AttributeNamespace
attributeNamespace = undefined

data AttributeArgumentClause =
  AttributeArgumentClause
  deriving (Show, Eq)

attributeArgumentClause :: P AttributeArgumentClause
attributeArgumentClause = undefined

balancedTokenSeq :: P [BalancedToken]
balancedTokenSeq = undefined

data BalancedToken =
  BalancedToken
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

data InitDeclarator =
  InitDeclarator
  deriving (Show, Eq)

initDeclarator :: P InitDeclarator
initDeclarator = undefined

data Declarator =
  Declarator
  deriving (Show, Eq)

declarator :: P Declarator
declarator = undefined

data PtrDeclarator =
  PtrDeclarator
  deriving (Show, Eq)

ptrDeclarator :: P PtrDeclarator
ptrDeclarator = undefined

data NoptrDeclarator =
  NoptrDeclarator
  deriving (Show, Eq)

noptrDeclarator :: P NoptrDeclarator
noptrDeclarator = undefined

data ParametersAndQualifiers =
  ParametersAndQualifiers
  deriving (Show, Eq)

parametersAndQualifiers :: P ParametersAndQualifiers
parametersAndQualifiers = undefined

data TrailingReturnType =
  TrailingReturnType
  deriving (Show, Eq)

trailingReturnType :: P TrailingReturnType
trailingReturnType = undefined

data PtrOperator =
  PtrOperator
  deriving (Show, Eq)

ptrOperator :: P PtrOperator
ptrOperator = undefined

cvQualifierSeq :: P [CvQualifier]
cvQualifierSeq = undefined

data CvQualifier =
  CvQualifier
  deriving (Show, Eq)

cvQualifier :: P CvQualifier
cvQualifier = undefined

data RefQualifier =
  RefQualifier
  deriving (Show, Eq)

refQualifier :: P RefQualifier
refQualifier = undefined

data DeclaratorId =
  DeclaratorId
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
data TypeId =
  TypeId
  deriving (Show, Eq)

typeId :: P TypeId
typeId = undefined

data AbstractDeclarator =
  AbstractDeclarator
  deriving (Show, Eq)

abstractDeclarator :: P AbstractDeclarator
abstractDeclarator = undefined

data PtrAbstractDeclarator =
  PtrAbstractDeclarator
  deriving (Show, Eq)

ptrAbstractDeclarator :: P PtrAbstractDeclarator
ptrAbstractDeclarator = undefined

data NoptrAbstractDeclarator =
  NoptrAbstractDeclarator
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
data ParameterDeclarationClause =
  ParameterDeclarationClause
  deriving (Show, Eq)

parameterDeclarationClause :: P ParameterDeclarationClause
parameterDeclarationClause = undefined

parameterDeclarationList :: P [ParameterDeclaration]
parameterDeclarationList = undefined

data ParameterDeclaration =
  ParameterDeclaration
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

data FunctionBody =
  FunctionBody
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
data Initializer =
  Initializer
  deriving (Show, Eq)

initializer :: P Initializer
initializer = undefined

data BraceOrEqualInitializer =
  BraceOrEqualInitializer
  deriving (Show, Eq)

braceOrEqualInitializer :: P BraceOrEqualInitializer
braceOrEqualInitializer = undefined

data InitializerClause =
  InitializerClause
  deriving (Show, Eq)

initializerClause :: P InitializerClause
initializerClause = undefined

data InitializerList =
  InitializerList
  deriving (Show, Eq)

initializerList :: P InitializerList
initializerList = undefined

data BracedInitList =
  BracedInitList
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
--  	externopt template declaration     C++0x
explicitinstantiation :: P Declaration
explicitinstantiation = undefined

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
