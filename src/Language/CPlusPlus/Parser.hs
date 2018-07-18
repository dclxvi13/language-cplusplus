module Language.CPlusPlus.Parser where

--import Language.CPlusPlus.AST
import Language.CPlusPlus.Token

import Text.Parsec

-- Hyperlinked C++ BNF Grammar
-- By Alessio Marchetti
--
-- Version 3.2
--
-- Last updated: 12-Feb-2016
--
-- Purpose of this document
-- If you are a C++ developer then you sometimes need to have a look at the C++ BNF grammar. At least I do.
--
-- But reading a set of BNF rules is difficult since you have to jump from one rule to the next one, maybe pages away from it.
--
-- With this document I tried to make it easier to navigate among the BNF rules of the C++ language grammar; you can find here a hyperlinked graph whose nodes are the BNF productions of the C++ language grammar.
--
-- If you find errors or inconsistencies, please drop me a note.
--
-- The BNF non-terminal for a C++ translation unit (translation-unit) is defined here.
-- The BNF non-terminal for a C++ preprocessing file (preprocessing-file) is defined here.
-- BNF Grammar Rules

-- basic.link
-- translation-unit:
--  	declaration-seq[opt]

data TranslationUnit = TU [Declaration]

translationUnit :: P TranslationUnit
translationUnit = option [] declarationSeq

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

type Literal = String
type Id      = String

data Expression
  = LiteralExpression
    { _literalExpressionPos   :: SourcePos
    , _literalExpressionValue :: Literal
    }
  | ThisExpression
    { _thisExpressionPos :: SourcePos
    }
  | ParensedExpression
    { _parensedExpressionPos   :: SourcePos
    , _parensedExpressionValue :: Expression
    }
  | IdExpression
    { _idExpressionPos :: SourcePos
    , _idExpressionValue :: Either UnqualifiedId QualifiedId
    }
  | LambdaExpression
    { _lambdaExpressionPos :: SourcePos
    }

primaryExpression :: P Expression
primaryExpression   = undefined

idExpression :: P Expression
idExpression        = undefined

data UnqualifiedId
  = UnqualifiedId

data QualifiedId
  = QualifiedId

unqualifiedId :: P UnqualifiedId
unqualifiedId       = undefined

qualifiedId :: P QualifiedId
qualifiedId         = undefined

data NestedNameSpecifier
  = NestedNameSpecifier

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

lambdaIntroducer = undefined
lambdaCapture    = undefined
captureDefault   = undefined
captureList      = undefined
capture          = undefined
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
postfixExpression    = undefined

expressionList :: P [Expression]
expressionList       = undefined

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

unaryOperator   = undefined

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
--  	( expression-listopt )
--  	braced-init-list     C++0x

newExpression :: P Expression
newExpression      = undefined

newPlacement       = undefined
newTypeId          = undefined
newDeclarator      = undefined
noptrNewDeclarator = undefined
newInitializer     = undefined

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

assignmentOperator   = undefined

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

data Statement = Statement

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
statementSeq      = undefined

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

data Condition = Condition

condition :: P Condition
condition          = undefined

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
iterationStatement  = undefined

forInitStatement    = undefined
forRangeDeclaration = undefined
forRangeInitializer = undefined

-- stmt.jump
-- jump-statement:
--  	break ;
--  	continue ;
--  	return expressionopt ;
--  	return braced-init-listopt ;     C++0x
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

data Declaration = Declaration

declarationSeq :: P [Declaration]
declarationSeq          = undefined

declaration :: P Declaration
declaration             = undefined

blockDeclaration :: P Declaration
blockDeclaration        = undefined

aliasDeclaration :: P Declaration
aliasDeclaration        = undefined

simpleDeclaration :: P Declaration
simpleDeclaration       = undefined

staticAssertDeclaration :: P Declaration
staticAssertDeclaration = undefined

emptyDeclaration :: P Declaration
emptyDeclaration        = undefined

attributeDeclaration :: P Declaration
attributeDeclaration    = undefined

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

data DeclSpecifier = DeclSpecifier

declSpecifier :: P DeclSpecifier
declSpecifier    = undefined

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

data StorageClassSpecifier = StorageClassSpecifier

storageClassSpecifier :: P StorageClassSpecifier
storageClassSpecifier = undefined

-- dcl.fct.spec
-- function-specifier:
--  	inline
--  	virtual
--  	explicit

data FunctionSpecifier = FunctionSpecifier

functionSpecifier :: P FunctionSpecifier
functionSpecifier = undefined

-- dcl.typedef
-- typedef-name:
--  	identifier

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

data TypeSpecifier = TypeSpecifier

typeSpecifier :: P TypeSpecifier
typeSpecifier            = undefined

trailingTypeSpecifier    = undefined
typeSpecifierSeq         = undefined
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

simpleTypeSpecifier = undefined
typeName            = undefined
decltypeSpecifier   = undefined

-- dcl.type.elab
-- elaborated-type-specifier:
--  	class-key attribute-specifier-seq[opt] ::opt nested-name-specifier[opt] identifier
--  	class-key ::opt nested-name-specifier[opt] template[opt] simple-template-id
--  	enum ::opt nested-name-specifier[opt] identifier

elaboratedTypeSpecifier = undefined

-- dcl.enum
-- enum-name:
--  	identifier
-- enum-specifier:
--  	enum-head { enumerator-listopt }     C++0x
--  	enum-head { enumerator-list , }     C++0x
-- enum-head:
--  	enum-key attribute-specifier-seqopt identifieropt enum-baseopt     C++0x
--  	enum-key attribute-specifier-seqopt nested-name-specifier identifier enum-baseopt     CD0x
-- opaque-enum-declaration:
--  	enum-key attribute-specifier-seqopt identifier enum-baseopt ;     C++0x
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

enumName              = undefined
enumSpecifier         = undefined
enumHead              = undefined
opaqueEnumDeclaration = undefined
enumKey               = undefined
enumBase              = undefined
enumeratorList        = undefined
enumeratorDefinition  = undefined
enumerator            = undefined

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
--  	inlineopt namespace identifier { namespace-body }     C++0x
-- extension-namespace-definition:
--  	inlineopt namespace original-namespace-name { namespace-body }     C++0xD- unnamed-namespace-definition:
--  	inlineopt namespace { namespace-body }
-- namespace-body:
--  	declaration-seqopt

namespaceName                = undefined
originalNamespaceName        = undefined
namespaceDefinition          = undefined
namedNamespaceDefinition     = undefined
originalNamespaceDefinition  = undefined
extensionNamespaceDefinition = undefined
unnamedNamespaceDefinition   = undefined
namespaceBody                = undefined

-- namespace.alias
-- namespace-alias:
--  	identifier
-- namespace-alias-definition:
--  	namespace identifier = qualified-namespace-specifier ;
-- qualified-namespace-specifier:
--  	::opt nested-name-specifieropt namespace-name

namespaceAlias              = undefined
namespaceAliasDefinition    = undefined
qualifiedNamespaceSpecifier = undefined

-- namespace.udecl
-- using-declaration:
--  	using typenameopt ::opt nested-name-specifier unqualified-id ;
--  	using :: unqualified-id ;

usingDeclaration = undefined

-- namespace.udir
-- using-directive:
--  	attribute-specifier-seqopt using namespace ::opt nested-name-specifieropt namespace-name ;

usingDirective = undefined

-- dcl.asm
-- asm-definition:
--  	asm ( string-literal ) ;

asmDefinition = undefined

-- dcl.link
-- linkage-specification:
--  	extern string-literal { declaration-seqopt }
--  	extern string-literal declaration

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
--  	attributeopt     C++0x
--  	attribute-list , attributeopt     C++0x
--  	attribute ...     C++0x
--  	attribute-list , attribute ...     C++0x
-- attribute:
--  	attribute-token attribute-argument-clauseopt     C++0x
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

attributeSpecifierSeq   = undefined
attributeSpecifier      = undefined
alignmentSpecifier      = undefined
attributeList           = undefined
attribute               = undefined
attributeToken          = undefined
attributeScopedToken    = undefined
attributeNamespace      = undefined
attributeArgumentClause = undefined
balancedTokenSeq        = undefined
balancedToken           = undefined

-- dcl.decl
-- init-declarator-list:
--  	init-declarator
--  	init-declarator-list , init-declarator
-- init-declarator:
--  	declarator initializeropt
-- declarator:
--  	ptr-declarator     C++0x
--  	noptr-declarator parameters-and-qualifiers trailing-return-type     C++0x
-- ptr-declarator:
--  	noptr-declarator     C++0x
--  	ptr-operator ptr-declarator     C++0x
-- noptr-declarator:
--  	declarator-id attribute-specifier-seqopt     C++0x
--  	noptr-declarator parameters-and-qualifiers     C++0x
--  	noptr-declarator [ constant-expressionopt ] attribute-specifier-seqopt     C++0x
--  	( ptr-declarator )     C++0x
-- parameters-and-qualifiers:
--  	( parameter-declaration-clause ) attribute-specifier-seqopt cv-qualifier-seqopt ref-qualifieropt exception-specificationopt     C++0x
-- trailing-return-type:
--  	-> trailing-type-specifier-seq abstract-declaratoropt     C++0x
-- ptr-operator:
--  	* attribute-specifier-seqopt cv-qualifier-seqopt     C++0x
--  	& attribute-specifier-seqopt     C++0x
--  	&& attribute-specifier-seqopt     C++0x
--  	::opt nested-name-specifier * attribute-specifier-seqopt cv-qualifier-seqopt     C++0x
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
--  	::opt nested-name-specifieropt class-name     C++0x

initDeclaratorList      = undefined
declarator              = undefined
ptrDeclarator           = undefined
noptrDeclarator         = undefined
parametersAndQualifiers = undefined
trailingReturnType      = undefined
ptrOperator             = undefined
cvQualifierSeq          = undefined
cvQualifier             = undefined
refQualifier            = undefined
declaratorId            = undefined

-- dcl.name
-- type-id:
--  	type-specifier-seq abstract-declaratoropt
-- abstract-declarator:
--  	ptr-abstract-declarator     C++0x
--  	noptr-abstract-declaratoropt parameters-and-qualifiers trailing-return-type     C++0x
--  	...     C++0x
-- ptr-abstract-declarator:
--  	noptr-abstract-declarator     C++0x
--  	ptr-operator ptr-abstract-declaratoropt     C++0x
-- noptr-abstract-declarator:
--  	noptr-abstract-declaratoropt parameters-and-qualifiers     C++0x
--  	noptr-abstract-declaratoropt [ constant-expression ] attribute-specifier-seqopt     C++0x
--  	( ptr-abstract-declarator )     C++0x

typeId                  = undefined
abstractDeclarator      = undefined
ptrAbstractDeclarator   = undefined
noptrAbstractDeclarator = undefined

-- dcl.fct
-- parameter-declaration-clause:
--  	parameter-declaration-listopt ...opt
--  	parameter-declaration-list , ...
-- parameter-declaration-list:
--  	parameter-declaration
--  	parameter-declaration-list , parameter-declaration
-- parameter-declaration:
--  	attribute-specifier-seqopt decl-specifier-seq declarator     C++0x
--  	attribute-specifier-seqopt decl-specifier-seq declarator = initializer-clause     C++0x
--  	attribute-specifier-seqopt decl-specifier-seq abstract-declaratoropt     C++0x
--  	attribute-specifier-seqopt decl-specifier-seq abstract-declaratoropt = initializer-clause     C++0x

parameterDeclarationClause = undefined
parameterDeclarationList = undefined
parameterDeclaration = undefined

-- dcl.fct.def.general
-- function-definition:
--  	attribute-specifier-seqopt decl-specifier-seqopt declarator function-body     C++0x
--  	attribute-specifier-seqopt decl-specifier-seqopt declarator = default ;     C++0x
--  	attribute-specifier-seqopt decl-specifier-seqopt declarator = delete ;     C++0x
-- function-body:
--  	ctor-initializeropt compound-statement     C++0x
--  	function-try-block     C++0x

functionDefinition = undefined
functionBody       = undefined

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

initializer             = undefined
braceOrEqualInitializer = undefined
initializerClause       = undefined
initializerList         = undefined
bracedInitList          = undefined

-- class
-- class-name:
--  	identifier
--  	simple-template-id     C++0x
-- class-specifier:
--  	class-head { member-specificationopt }
-- class-head:
--  	class-key attribute-specifier-seqopt class-head-name class-virt-specifier-seqopt base-clauseopt     C++0x
--  	class-key attribute-specifier-seqopt base-clauseopt     C++0x
-- class-head-name:
--  	nested-name-specifieropt class-name     C++0x
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

className             = undefined
classSpecifier        = undefined
classHead             = undefined
classHeadName         = undefined
classVirtSpecifierSeq = undefined
classVirtSpecifier    = undefined
classKey              = undefined

-- class.mem
-- member-specification:
--  	member-declaration member-specificationopt
--  	access-specifier : member-specificationopt
-- member-declaration:
--  	attribute-specifier-seqopt decl-specifier-seqopt member-declarator-listopt ;     C++0x
--  	function-definition ;opt
--  	using-declaration
--  	static_assert-declaration     C++0x
--  	template-declaration
--  	alias-declaration     C++0x
-- member-declarator-list:
--  	member-declarator
--  	member-declarator-list , member-declarator
-- member-declarator:
--  	declarator virt-specifier-seqopt pure-specifieropt
--  	declarator virt-specifier-seqopt brace-or-equal-initializeropt     C++0x
--  	identifieropt attribute-specifier-seqopt virt-specifier-seqopt : constant-expression
-- virt-specifier-seq:
--  	virt-specifier
--  	virt-specifier-seq virt-specifier
-- virt-specifier:
--  	override
--  	final
--  	new
-- pure-specifier:
--  	= 0

memberSpecification  = undefined
memberDeclaration    = undefined
memberDeclaratorList = undefined
memberDeclarator     = undefined
virtSpecifierSeq     = undefined
virtSpecifier        = undefined
pureSpecifier        = undefined

-- class.derived
-- base-clause:
--  	: base-specifier-list
-- base-specifier-list:
--  	base-specifier ...opt     C++0x
--  	base-specifier-list , base-specifier ...opt     C++0x
-- base-specifier:
--  	attribute-specifier-seqopt base-type-specifier     C++0x
--  	attribute-specifier-seqopt virtual access-specifieropt base-type-specifier     C++0x
--  	attribute-specifier-seqopt access-specifier virtualopt base-type-specifier     C++0x
-- class-or-decltype:
--  	::opt nested-name-specifieropt class-name     C++0x
--  	decltype-specifier     C++0x
-- base-type-specifier:
--  	class-or-decltype     C++0x
-- access-specifier:
--  	private
--  	protected
--  	public

baseClause = undefined
baseSpecifierList = undefined
baseSpecifier = undefined
classOrDecltype = undefined
baseTypeSpecifier = undefined
accessSpecifier = undefined

-- class.conv.fct
-- conversion-function-id:
--  	operator conversion-type-id
-- conversion-type-id:
--  	type-specifier-seq conversion-declaratoropt
-- conversion-declarator:
--  	ptr-operator conversion-declaratoropt

conversionFunctionId = undefined
conversionTypeId = undefined
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

ctorInitializer = undefined
memInitializerList = undefined
memInitializer = undefined
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

operatorFunctionId = undefined
overloadableOperator = undefined

-- over.literal
-- literal-operator-id:
--  	operator "" identifier     C++0x

literalOperatorId = undefined

-- temp
-- template-declaration:
--  	template < template-parameter-list > declaration     C++0x - The export keyword is reserved for future use
-- template-parameter-list:
--  	template-parameter
--  	template-parameter-list , template-parameter

templateDeclaration = undefined
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

templateParameter = undefined
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

simpleTemplateId = undefined
templateId = undefined
templateName = undefined
templateArgumentList = undefined
templateArgument = undefined

-- temp.res
-- typename-specifier:
--  	typename ::opt nested-name-specifier identifier     C++0x
--  	typename ::opt nested-name-specifier template[opt] simple-template-id     C++0x

typenameSpecifier = undefined

-- temp.explicit
-- explicit-instantiation:
--  	externopt template declaration     C++0x

explicitinstantiation = undefined

-- temp.expl.spec
-- explicit-specialization:
--  	template < > declaration

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

tryBlock = undefined
functionTryBlock = undefined
handlerSeq = undefined
handler = undefined
exceptionDeclaration = undefined
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

exceptionSpecification = undefined
dynamicExceptionSpecification = undefined
typeIdList = undefined
noexceptSpecification = undefined
