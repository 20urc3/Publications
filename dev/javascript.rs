//! # Abstract Syntax Tree: `JavaScript`
//!
//! **Description**
//! - This crate provides A comprehensive JavaScript AST with fixed recursive types. 
//! It covers many ECMAScript constructs including modules, statements, expressions, 
//! functions, classes, and modern language features.
//! - Each tag is associated with its textual value, a list of attributes, and a numeric weight.
//! - The weight can be used to guide transformations or drive weight-based code generation.
//!
//! **Features**
//! - Full coverage of ECMAScript 2024 tags.
//! - Built-in default tag weights, with support for user-defined overrides.
//! - Ready for transformations, rendering, or any custom code-generation pipeline.


#![allow(dead_code,unused)] /// Get rid of warnings

use std::collections::HashMap;

/// Represents a top-level JavaScript program, which can be either Script or Module.
/// This distinction is important because modules have different semantics,
/// such as strict mode enforcement, top-level `import`/`export` support,
/// and distinct scoping rules.
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum Program {

    /// A standalone script executed outside of a module system.
    Script(Script),

    /// An ECMAScript module, allowing `import` and `export` statements.
    Module(Module),
}

/// Represents a JavaScript script sequence.
/// Unlike modules, scripts do not enforce strict mode by default and do not support `import` or `export` statements.
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct Script {

    /// A list of directive prologues (e.g., `"use strict"`).
    pub directives: Vec<Directive>,

    /// The main body of the script, consisting of JavaScript statements.
    pub body: Vec<Statement>,
}

/// Represents an JavaScript module.
/// A `Module` consists of a sequence of top-level statements and import/export declarations.
/// Unlike scripts, modules:
/// - Always run in **strict mode**.
/// - Support `import` and `export` statements for module-based code organization.
/// - Have their own **module scope**, preventing accidental global variable pollution.
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct Module {

    /// The body of the module, containing top-level statements and module items.
    pub body: Vec<ModuleItem>,
}

/// Represents a directive prologue in JavaScript.
/// Directives are special string literals placed at the beginning of a script or function.
/// They alter execution semantics, such as enabling **strict mode** (`"use strict"`) or 
/// enabling experimental features in some JavaScript environments.
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct Directive {

    /// The string literal representing the directive (e.g., `"use strict"`).
    pub value: StringLiteral,
}

/// Represents an item inside an ECMAScript module.
/// Unlike scripts, modules require all import and export declarations to appear at the top level.
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum ModuleItem {

    ///  A regular JavaScript statement, such as variable declarations or function definitions.
    Statement(Statement),

    /// An `import` declaration, bringing binding for external module.
    ImportDeclaration(ImportDeclaration),

    /// An `export` declaration, exposing binding for external module.
    ExportDeclaration(ExportDeclaration),
}

/// Represents a JavaScript statement in the AST.
/// A statement is a fundamental unit of execution in JavaScript. It can be 
/// a control flow structure, an expression, a declaration, or a block.
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum Statement {

    /// A block of multiple statements enclosed in `{}`.
    Block(BlockStatement),

    /// A semicolon (`;`) that serves no operational purpose.
    Empty(EmptyStatement),

    /// A standalone expression used as a statement.
    Expression(ExpressionStatement),

    /// A conditional `if` statement.
    If(IfStatement),

    /// A labeled statement (e.g., `label: statement;`) used with `break` or `continue`.
    Labeled(LabeledStatement),

    /// Exits a loop or switch statement.
    Break(BreakStatement),

    /// Skips the rest of the current loop iteration and proceeds to the next.
    Continue(ContinueStatement),

    /// Alters the scope chain (deprecated and disallowed in strict mode).
    With(WithStatement),

    /// A multi-case conditional branching statement (`switch`).
    Switch(SwitchStatement),

    /// Exits a function and optionally returns a value.
    Return(ReturnStatement),

    /// Throws an exception to be caught by `try...catch`.
    Throw(ThrowStatement),

    /// Handles exceptions with `catch` and `finally` blocks.
    Try(TryStatement),

    /// Triggers a breakpoint in debugging tools (`debugger;`).
    Debugger(DebuggerStatement),

    /// Declares variables using `var`, `let`, or `const`.
    VariableDeclaration(VariableDeclaration),

    /// Defines a function declaration.
    FunctionDeclaration(FunctionDeclaration),

    /// Defines a class declaration.
    ClassDeclaration(ClassDeclaration),

    /// A traditional `for` loop with initialization, condition, and update.
    For(ForStatement),

    /// Iterates over an object's enumerable properties (`for...in`).
    ForIn(ForInStatement),

    /// Iterates over iterable values such as arrays (`for...of`).
    ForOf(ForOfStatement),
}

/// A block of statements enclosed in `{}`.
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct BlockStatement {

    /// A list of statements inside the block.
    pub body: Vec<Statement>,
}

/// A semicolon (`;`) with no operational effect.
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct EmptyStatement;

/// A standalone expression used as a statement.
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ExpressionStatement {

    /// The expression being evaluated.
    pub expression: Expression,
}

/// A conditional `if` statement with an optional `else` branch.
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct IfStatement {

    /// The condition being tested.
    pub test: Expression,
    
    /// The statement executed if the condition is `true`.
    pub consequent: Box<Statement>,
    
    /// The statement executed if the condition is `false` (optional).
    pub alternate: Option<Box<Statement>>,
}

/// A statement labeled with an identifier, used for `break` and `continue`.
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct LabeledStatement {

    /// The label identifier.
    pub label: Identifier,

    /// The statement associated with the label.
    pub body: Box<Statement>,
}

/// A `break` statement that exits a loop or switch case.
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct BreakStatement {

    /// The optional label to break out of a specific loop or switch case.
    pub label: Option<Identifier>,
}

/// A `continue` statement that skips to the next iteration of a loop.
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ContinueStatement {

    /// The optional label to continue a specific loop.
    pub label: Option<Identifier>,
}


/// Represents a with statement `with (obj) { ... }`
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct WithStatement {

    /// The object expression used in the with statement
    pub object: Expression,

    /// The body/block to execute with the object context
    pub body: Box<Statement>,
}

/// Represents a switch statement `switch (expr) { case x: ... }`
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct SwitchStatement {

    /// The expression to match against cases
    pub discriminant: Expression,

    /// List of case/default blocks
    pub cases: Vec<SwitchCase>,
}

/// Represents a single case in a switch statement
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct SwitchCase {

    /// The case value (None for default case)
    pub test: Option<Expression>,

    /// List of statements to execute when case matches
    pub consequent: Vec<Statement>,
}

/// Represents a return statement `return val`
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ReturnStatement {

    /// Optional return value (None for empty return)
    pub argument: Option<Expression>,
}

/// Represents a throw statement `throw err;`
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ThrowStatement {

    /// The expression to throw
    pub argument: Expression,
}

/// Represents a try-catch-finally statement
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct TryStatement {

    /// The try block
    pub block: BlockStatement,

    /// Optional catch clause
    pub handler: Option<CatchClause>,

    /// Optional finally block
    pub finalizer: Option<BlockStatement>,
}

/// Represents a catch clause in a try statement
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct CatchClause {

    /// Optional catch parameter (e.g., `catch(err) -> err is param`)
    pub param: Option<Pattern>,

    /// The catch block body
    pub body: BlockStatement,
}

/// Represents a debugger statement (debugger;)
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct DebuggerStatement;

/// Represents the initialization part of a for loop
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum ForStatementInit {

    /// Variable declaration `for(let i = 0;;)`
    VariableDeclaration(VariableDeclaration),
    /// Expression (for(i = 0;;))
    Expression(Expression),
}

/// Represents a standard for loop
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ForStatement {

    /// Optional initialization expression/declaration
    pub init: Option<ForStatementInit>,

    /// Optional test condition
    pub test: Option<Expression>,

    /// Optional update expression
    pub update: Option<Expression>,

    /// Loop body
    pub body: Box<Statement>,
}

/// Represents the left side of a for-in loop
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum ForInLeft {

    /// Variable declaration `for(let key in obj)`
    VariableDeclaration(VariableDeclaration),

    /// Expression `for(key in obj)`
    Expression(Expression),
}

/// Represents a for-in loop 
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ForInStatement {

    /// Left-hand side (variable or expression to assign)
    pub left: ForInLeft,

    /// Object to iterate over
    pub right: Expression,

    /// Loop body
    pub body: Box<Statement>,
}

/// Represents the left side of a for-of loop
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum ForOfLeft {

    /// Variable declaration `for(let val of array)`
    VariableDeclaration(VariableDeclaration),

    /// Expression (for(val of array))
    Expression(Expression),
}

/// Represents a for-of loop 
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ForOfStatement {
    /// Left-hand side: variable or expression to assign
    pub left: ForOfLeft,

    /// Iterable to loop through
    pub right: Expression,

    /// Loop body
    pub body: Box<Statement>,
}

/// Types of variable declarations
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum VariableDeclarationKind {

    /// var - function scoped
    Var,

    /// let - block scoped
    Let,

    /// const - block scoped, immutable
    Const,
}

/// Represents a variable declaration (var/let/const)
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct VariableDeclaration {

    /// Declaration type (`var`, `let`, `const`)
    pub kind: VariableDeclarationKind,

    /// List of variables being declared
    pub declarations: Vec<VariableDeclarator>,
}

/// Represents a single variable declarator
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct VariableDeclarator {

    /// The identifier or pattern being bound
    pub id: Pattern,

    /// Optional initializer
    pub init: Option<Expression>,
}

/// Represents a function declaration
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct FunctionDeclaration {

    /// Optional function name (required except in exports)
    pub id: Option<Identifier>,

    /// Function parameters
    pub params: Vec<Pattern>,

    /// Function body
    pub body: BlockStatement,

    /// Whether it's a generator function (`function*`)
    pub generator: bool,

    /// Whether it's an async function (`async function`)
    pub r#async: bool,
}

/// Represents a class declaration
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ClassDeclaration {

    /// Optional class name (required except in exports)
    pub id: Option<Identifier>,

    /// Optional parent class for inheritance
    pub super_class: Option<Expression>,

    /// Class definition body
    pub body: ClassBody,
}

/// Represents a class body
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ClassBody {

    /// List of class elements (methods, properties)
    pub body: Vec<ClassElement>,
}

/// Represents class members
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum ClassElement {

    /// Class method
    Method(ClassMethod),

    /// Class property
    Property(ClassProperty),

    /// Static initialization block
    StaticBlock(StaticBlock),
}

/// Represents a class method
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ClassMethod {

    /// Method name/key
    pub key: Expression,

    /// Method function definition
    pub value: FunctionExpression,

    /// Method type (`constructor`, `getter`, `setter`, etc.)
    pub kind: MethodKind,

    /// Whether the key is computed (e.g., `[expr]() {}`)
    pub computed: bool,

    /// Whether it's a static method
    pub is_static: bool,

    /// Whether it's an optional method (TS feature)
    pub optional: bool,
}

/// Types of class methods
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum MethodKind {

    /// `constructor() {}`
    Constructor,

    /// Regular `method() {}`
    Method,

    /// `get property() {}`
    Get,

    /// `set property() {}`
    Set,
}

/// Represents a class property
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ClassProperty {

    /// Property name/key
    pub key: Expression,

    /// Optional property value/initializer
    pub value: Option<Expression>,

    /// Whether the key is computed (e.g., [expr] = value)
    pub computed: bool,

    /// Whether it's a static property
    pub is_static: bool,

    /// Whether it's a readonly property (TS feature)
    pub readonly: bool,
}

/// Represents a static initialization block in a class
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct StaticBlock {

    /// Statements in the static block
    pub body: Vec<Statement>,
}

/// All possible expression types in JavaScript
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum Expression {
    /// `this` keyword
    This(ThisExpression),

    /// Variable reference
    Identifier(Identifier),
    
    /// `super` keyword
    Super(Super),

    /// Literal values (numbers, strings, etc.)
    Literal(Literal),

    /// Array literal ([1, 2, 3])
    Array(ArrayExpression),

    /// Object literal ({a: 1, b: 2})
    Object(ObjectExpression),

    /// Function expression
    Function(FunctionExpression),

    /// Arrow function
    ArrowFunction(Box<ArrowFunctionExpression>),

    /// Class expression
    Class(Box<ClassExpression>),

    /// await expression
    Await(AwaitExpression),

    /// yield expression
    Yield(YieldExpression),

    /// Unary operation (!, +, -, etc.)
    Unary(UnaryExpression),

    /// Binary operation (+, -, *, /, etc.)
    Binary(BinaryExpression),

    /// Logical operation (&&, ||)
    Logical(LogicalExpression),

    /// Assignment (=, +=, etc.)
    Assignment(Box<AssignmentExpression>),

    /// Update (++, --)
    Update(UpdateExpression),

    /// Conditional/ternary (a ? b : c)
    Conditional(ConditionalExpression),

    /// Function/method call
    Call(CallExpression),

    /// Constructor call (new X())
    New(NewExpression),

    /// Property access (obj.prop, obj['prop'])
    Member(MemberExpression),

    /// Comma-separated expressions
    Sequence(SequenceExpression),

    /// Template literals (`string ${expr}`)
    TemplateLiteral(TemplateLiteral),

    /// Tagged template literals (tag`string`)
    TaggedTemplate(TaggedTemplateExpression),

    /// Dynamic import (import())
    Import(ImportExpression),

    /// Meta properties (new.target, import.meta)
    MetaProperty(MetaProperty),
}

/// Represents the `this` keyword
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ThisExpression;

/// Represents an identifier/variable name
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct Identifier {

    /// The name of the identifier
    pub name: String,
}

/// Represents the `super` keyword
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct Super;

/// Represents literal values
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum Literal {

    /// null literal
    Null(NullLiteral),

    /// Boolean literal (true/false)
    Boolean(BooleanLiteral),

    /// Number literal
    Numeric(NumericLiteral),

    /// BigInt literal (123n)
    BigInt(BigIntLiteral),

    /// String literal
    String(StringLiteral),

    /// Regular expression literal (/pattern/flags)
    RegExp(RegExpLiteral),
}

/// Represents null literal
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct NullLiteral;

/// Represents boolean literal (true/false)
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct BooleanLiteral {

    /// The boolean value
    pub value: bool,
}

/// Represents numeric literal
#[derive(Hash, Eq, Debug, Clone, PartialEq)]
pub struct NumericLiteral {

    /// The numeric value
    pub value: i64,
}

/// Represents BigInt literal (123n)
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct BigIntLiteral {

    /// The BigInt value as string
    pub value: String,
}

/// Represents string literal
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct StringLiteral {

    /// The string value
    pub value: String,
}

/// Represents regular expression literal
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct RegExpLiteral {

    /// The regex pattern
    pub pattern: String,

    /// The regex flags
    pub flags: String,
}

/// Represents array literal/expression
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ArrayExpression {

    /// Array elements (Option for handling sparse arrays with holes)
    pub elements: Vec<Option<Expression>>,
}

/// Represents object literal/expression
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ObjectExpression {

    /// Object properties and spreads
    pub properties: Vec<ObjectProperty>,
}

/// Represents object properties/elements
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum ObjectProperty {

    /// Regular key-value property
    KeyValue(ObjectKeyValue),

    /// Spread property (...obj)
    Spread(SpreadElement),
}

/// Represents a key-value property in an object
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ObjectKeyValue {

    /// Property key
    pub key: Expression,

    /// Property value
    pub value: Expression,

    /// Whether the key is computed (e.g., [expr]: value)
    pub computed: bool,

    /// Whether it's a shorthand property ({x} instead of {x: x})
    pub shorthand: bool,
}

/// Represents spread syntax in arrays/objects
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct SpreadElement {

    /// The expression being spread
    pub argument: Expression,
}

/// Represents a function expression
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct FunctionExpression {

    /// Optional function name
    pub id: Option<Identifier>,

    /// Function parameters
    pub params: Vec<Pattern>,

    /// Function body
    pub body: BlockStatement,

    /// Whether it's a generator function (function*)
    pub generator: bool,

    /// Whether it's an async function (async function)
    pub r#async: bool,
}

/// Represents an arrow function
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ArrowFunctionExpression {

    /// Arrow function parameters
    pub params: Vec<Pattern>,

    /// Arrow function body (expression or block)
    pub body: Box<ArrowFunctionBody>,

    /// Whether it's an async arrow function
    pub r#async: bool,
}

/// Represents arrow function bodies (block or expression)
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum ArrowFunctionBody {

    /// Expression body (x => x * 2)
    Expression(Box<Expression>),

    /// Block body (x => { return x * 2; })
    Block(BlockStatement),
}

/// Represents a class expression
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ClassExpression {

    /// Optional class name
    pub id: Option<Identifier>,

    /// Optional parent class for inheritance
    pub super_class: Option<Box<Expression>>,

    /// Class definition body
    pub body: ClassBody,
}

/// Represents an await expression
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct AwaitExpression {

    /// The awaited expression
    pub argument: Box<Expression>,
}

/// Represents a yield expression
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct YieldExpression {

    /// Optional yielded value
    pub argument: Option<Box<Expression>>,

    /// Whether it's a yield* (delegate)
    pub delegate: bool,
}

/// Represents a unary operation
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct UnaryExpression {

    /// The operator (!, +, -, etc.)
    pub operator: UnaryOperator,

    /// The operand
    pub argument: Box<Expression>,

    /// Whether operator comes before operand (true for most unary ops)
    pub prefix: bool,
}

/// Unary operators in JavaScript
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum UnaryOperator {

    /// Unary minus (-x)
    Minus,

    /// Unary plus (+x)
    Plus,

    /// Logical not (!x)
    LogicalNot,

    /// Bitwise not (~x)
    BitwiseNot,

    /// typeof operator
    TypeOf,

    /// void operator
    Void,

    /// delete operator
    Delete,
}

/// Represents a binary operation
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct BinaryExpression {

    /// The operator (+, -, *, /, etc.)
    pub operator: BinaryOperator,

    /// Left operand
    pub left: Box<Expression>,

    /// Right operand
    pub right: Box<Expression>,

}

/// Binary operators in JavaScript
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum BinaryOperator {

    /// == (equality)
    Equal,

    /// != (inequality)
    NotEqual,

    /// === (strict equality)
    StrictEqual,

    /// !== (strict inequality)
    StrictNotEqual,

    /// < (less than)
    LessThan,

    /// <= (less than or equal)
    LessThanOrEqual,

    /// > (greater than)
    GreaterThan,

    /// >= (greater than or equal)
    GreaterThanOrEqual,

    /// << (left shift)
    LeftShift,

    /// >> (right shift)
    RightShift,
    
    /// >>> (unsigned right shift)
    UnsignedRightShift,

    /// + (addition)
    Add,

    /// - (subtraction)
    Subtract,

    /// * (multiplication)
    Multiply,

    /// / (division)
    Divide,

    /// % (modulus)
    Modulus,

    /// ** (exponentiation)
    Exponentiation,

    /// in operator
    In,

    /// instanceof operator
    InstanceOf,
}

/// Represents a logical operation
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct LogicalExpression {

    /// The logical operator (&&, ||)
    pub operator: LogicalOperator,

    /// Left operand
    pub left: Box<Expression>,

    /// Right operand
    pub right: Box<Expression>,
}

/// Logical operators in JavaScript
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum LogicalOperator {

    /// || (logical OR)
    Or,

    /// && (logical AND)
    And,
}

/// Represents an assignment expression
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct AssignmentExpression {

    /// Left side of assignment (target)
    pub left: AssignmentTarget,

    /// Right side of assignment (value)
    pub right: Box<Expression>,

    /// Assignment operator (=, +=, etc.)
    pub operator: AssignmentOperator,
}

/// Assignment operators in JavaScript
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum AssignmentOperator {

    /// = (simple assignment)
    Assign,

    /// += (add and assign)
    AddAssign,

    /// -= (subtract and assign)
    SubtractAssign,

    /// *= (multiply and assign)
    MultiplyAssign,

    /// /= (divide and assign)
    DivideAssign,

    /// %= (modulus and assign)
    ModulusAssign,

    /// <<= (left shift and assign)
    LeftShiftAssign,

    /// >>= (right shift and assign)
    RightShiftAssign,

    /// >>>= (unsigned right shift and assign)
    UnsignedRightShiftAssign,

    /// |= (bitwise OR and assign)
    BitOrAssign,

    /// ^= (bitwise XOR and assign)
    BitXorAssign,
    
    /// &= (bitwise AND and assign)
    BitAndAssign,

    /// **= (exponentiation and assign)
    ExponentiationAssign,
}

/// Represents an assignment target
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum AssignmentTarget {

    /// Target is an expression (typically member/identifier)
    Expression(Box<Expression>),

    /// Target is a destructuring pattern
    Pattern(Pattern),
}

/// Represents an update expression (increment/decrement)
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct UpdateExpression {

    /// The operator (++ or --)
    pub operator: UpdateOperator,

    /// The operand
    pub argument: Box<Expression>,

    /// Whether operator comes before operand (++x vs x++)
    pub prefix: bool,
}

/// Update operators in JavaScript
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum UpdateOperator {

    /// ++ (increment)
    Increment,

    /// -- (decrement)
    Decrement,
}

/// Represents a conditional/ternary expression
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ConditionalExpression {

    /// Condition expression
    pub test: Box<Expression>,

    /// Expression if condition is true
    pub consequent: Box<Expression>,

    /// Expression if condition is false
    pub alternate: Box<Expression>,
}

/// Represents a function call
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct CallExpression {

    /// The function being called
    pub callee: Box<Expression>,

    /// Arguments passed to the function
    pub arguments: Vec<CallArgument>,

    /// Whether it's an optional call (obj?.method())
    pub optional: bool,
}

/// Represents a function call argument
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum CallArgument {

    /// Regular expression argument
    Expression(Expression),

    /// Spread argument (...args)
    Spread(SpreadElement),
}

/// Represents a constructor call (new expression)
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct NewExpression {

    /// The constructor being called
    pub callee: Box<Expression>,

    /// Arguments passed to the constructor
    pub arguments: Vec<CallArgument>,
}

/// Represents property access
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct MemberExpression {

    /// The object being accessed
    pub object: Box<Expression>,

    /// The property being accessed
    pub property: Box<Expression>,

    /// Whether it's computed access (obj[expr] vs obj.prop)
    pub computed: bool,

    /// Whether it's optional chaining (obj?.prop)
    pub optional: bool,
}

/// Represents comma-separated expressions
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct SequenceExpression {

    /// The expressions in the sequence
    pub expressions: Vec<Expression>,
}

/// Represents a template literal
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct TemplateLiteral {

    /// The string parts
    pub quasis: Vec<TemplateElement>,

    /// The expressions between string parts
    pub expressions: Vec<Expression>,
}

/// Represents a part of a template literal
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct TemplateElement {

    /// The text content
    pub value: TemplateElementValue,

    /// Whether it's the last part
    pub tail: bool,
}

/// Represents the value of a template element
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct TemplateElementValue {

    /// The processed/cooked string (None if invalid escape)
    pub cooked: Option<String>,

    /// The raw unprocessed string
    pub raw: String,
}

/// Represents a tagged template literal
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct TaggedTemplateExpression {

    /// The tag function
    pub tag: Box<Expression>,

    /// The template literal
    pub quasi: TemplateLiteral,
}

/// Represents a dynamic import expression
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ImportExpression {

    /// The module specifier
    pub source: Box<Expression>,
}

/// Represents meta properties like new.target
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct MetaProperty {

    /// The meta object (new, import)
    pub meta: Identifier,

    /// The property (target, meta)
    pub property: Identifier,
}

/// Represents destructuring and binding patterns
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum Pattern {

    /// Simple identifier binding
    Identifier(Identifier),

    /// Object destructuring pattern
    Object(ObjectPattern),

    /// Array destructuring pattern
    Array(ArrayPattern),

    /// Rest pattern (...rest)
    Rest(RestElement),

    /// Assignment pattern with default (x = default)
    Assignment(AssignmentPattern),
}

/// Represents object destructuring pattern
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ObjectPattern {

    /// Properties in the pattern
    pub properties: Vec<ObjectPatternProperty>,
}

/// Represents properties in object pattern
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum ObjectPatternProperty {

    /// Regular key-value property
    KeyValue(ObjectPatternKeyValue),

    /// Rest property (...rest)
    Rest(RestElement),
}

/// Represents a key-value property in object pattern
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ObjectPatternKeyValue {

    /// Property key
    pub key: Expression,

    /// Property binding pattern
    pub value: Pattern,

    /// Whether the key is computed
    pub computed: bool,
}

/// Represents array destructuring pattern
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ArrayPattern {

    /// Elements in the pattern (Option for handling holes)
    pub elements: Vec<Option<Pattern>>,
}

/// Represents rest element in destructuring
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct RestElement {

    /// The target of the rest operation
    pub argument: Box<Pattern>,
}

/// Represents assignment pattern with default
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct AssignmentPattern {

    /// The target pattern
    pub left: Box<Pattern>,

    /// The default value
    pub right: Box<Expression>,
}

/// Represents an import declaration
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ImportDeclaration {

    /// The imported specifiers
    pub specifiers: Vec<ImportSpecifier>,

    /// The source module
    pub source: StringLiteral,
}

/// Types of import specifiers
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum ImportSpecifier {

    /// Default import (import name from 'mod')
    Default(ImportDefaultSpecifier),

    /// Named import (import {name} from 'mod')
    Named(ImportNamedSpecifier),

    /// Namespace import (import * as name from 'mod')
    Namespace(ImportNamespaceSpecifier),
}

/// Represents a default import specifier
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ImportDefaultSpecifier {

    /// Local binding name
    pub local: Identifier,
}

/// Represents a named import specifier
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ImportNamedSpecifier {

    /// The imported name
    pub imported: Identifier,

    /// Local binding name
    pub local: Identifier,
}

/// Represents a namespace import specifier
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ImportNamespaceSpecifier {

    /// Local binding name
    pub local: Identifier,
}

/// Types of export declarations
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum ExportDeclaration {

    /// export * from 'mod'
    ExportAll(ExportAllDeclaration),

    /// export {name}, export {name} from 'mod'
    ExportNamed(ExportNamedDeclaration),

    /// export default expr
    ExportDefault(ExportDefaultDeclaration),

    /// export declaration (function, class, etc)
    ExportDeclaration(ExportDeclarationDeclaration),
}

/// Represents an export all declaration
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ExportAllDeclaration {

    /// The source module
    pub source: StringLiteral,
}

/// Represents a named export declaration
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ExportNamedDeclaration {

    /// Optional declaration being exported
    pub declaration: Option<Box<Declaration>>,

    /// Export specifiers
    pub specifiers: Vec<ExportSpecifier>,

    /// Optional source module (for re-exports)
    pub source: Option<StringLiteral>,
}

/// Types of export specifiers
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum ExportSpecifier {

    /// Named export specifier
    Named(ExportNamedSpecifier),
}

/// Represents a named export specifier
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ExportNamedSpecifier {

    /// The exported name
    pub exported: Identifier,

    /// The local name
    pub local: Identifier,
}

/// Represents a default export declaration
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ExportDefaultDeclaration {

    /// The declaration being exported as default
    pub declaration: Box<Declaration>,
}

/// Represents a declaration export
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct ExportDeclarationDeclaration {

    /// The declaration being exported
    pub declaration: Box<Declaration>,
}

/// Types of declarations
#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum Declaration {

    /// Function declaration
    Function(FunctionDeclaration),

    /// Class declaration
    Class(ClassDeclaration),

    /// Variable declaration
    Variable(VariableDeclaration),
}

/// A map of JavaScript AST node types/variants to their probability weights
/// Used for controlling the relative frequency of different node types during code generation
#[derive(Debug, Clone)]
pub struct JsWeightMap {
    pub weights: HashMap<String, f64>,
}

/// Creates a new `JsWeightMap` with carefully calibrated default probabilities
/// for JavaScript AST node types and variants, then optionally applies user-defined overrides.
///
/// The weights influence how frequently each node type appears when generating
/// JavaScript code, resulting in more realistic and idiomatic output.
///
/// # Parameters
/// * `user_overrides` - Optional HashMap of custom weights to override defaults
///
/// # Returns
/// A new JsWeightMap instance with all weights initialized
impl JsWeightMap {
    pub fn new(user_overrides: Option<HashMap<String, f64>>) -> Self {
        let mut defaults = HashMap::new();

        // -- Program
        defaults.insert("program_script".into(), 0.7);
        defaults.insert("program_module".into(), 0.3);

        // -- Script
        defaults.insert("script_directives".into(), 0.2);
        defaults.insert("script_body".into(), 1.0);

        // -- Module
        defaults.insert("module_body".into(), 1.0);

        // -- Directive
        defaults.insert("directive_value".into(), 0.2);

        // -- ModuleItem
        defaults.insert("module_item_statement".into(), 0.7);
        defaults.insert("module_item_import_declaration".into(), 0.2);
        defaults.insert("module_item_export_declaration".into(), 0.1);

        // -- Statement
        defaults.insert("statement_block".into(), 0.8);
        defaults.insert("statement_empty".into(), 0.1);
        defaults.insert("statement_expression".into(), 0.9);
        defaults.insert("statement_if".into(), 0.7);
        defaults.insert("statement_labeled".into(), 0.1);
        defaults.insert("statement_break".into(), 0.3);
        defaults.insert("statement_continue".into(), 0.3);
        defaults.insert("statement_with".into(), 0.0);
        defaults.insert("statement_switch".into(), 0.4);
        defaults.insert("statement_return".into(), 0.5);
        defaults.insert("statement_throw".into(), 0.3);
        defaults.insert("statement_try".into(), 0.3);
        defaults.insert("statement_debugger".into(), 0.0);
        defaults.insert("statement_variable_declaration".into(), 0.8);
        defaults.insert("statement_function_declaration".into(), 0.7);
        defaults.insert("statement_class_declaration".into(), 0.4);
        defaults.insert("statement_for".into(), 0.6);
        defaults.insert("statement_for_in".into(), 0.2);
        defaults.insert("statement_for_of".into(), 0.3);

        // -- BlockStatement
        defaults.insert("block_statement_body".into(), 1.0);

        // -- ExpressionStatement
        defaults.insert("expression_statement_expression".into(), 1.0);

        // -- IfStatement
        defaults.insert("if_statement_test".into(), 1.0);
        defaults.insert("if_statement_consequent".into(), 1.0);
        defaults.insert("if_statement_alternate".into(), 0.4);

        // -- LabeledStatement
        defaults.insert("labeled_statement_label".into(), 0.1);
        defaults.insert("labeled_statement_body".into(), 1.0);

        // -- BreakStatement
        defaults.insert("break_statement_label".into(), 0.2);

        // -- ContinueStatement
        defaults.insert("continue_statement_label".into(), 0.2);

        // -- WithStatement
        defaults.insert("with_statement_object".into(), 1.0);
        defaults.insert("with_statement_body".into(), 1.0);

        // -- SwitchStatement
        defaults.insert("switch_statement_discriminant".into(), 1.0);
        defaults.insert("switch_statement_cases".into(), 1.0);

        // -- SwitchCase
        defaults.insert("switch_case_test".into(), 0.5);
        defaults.insert("switch_case_consequent".into(), 1.0);

        // -- ReturnStatement
        defaults.insert("return_statement_argument".into(), 0.6);

        // -- ThrowStatement
        defaults.insert("throw_statement_argument".into(), 1.0);

        // -- TryStatement
        defaults.insert("try_statement_block".into(), 1.0);
        defaults.insert("try_statement_handler".into(), 0.5);
        defaults.insert("try_statement_finalizer".into(), 0.3);

        // -- CatchClause
        defaults.insert("catch_clause_param".into(), 0.5);
        defaults.insert("catch_clause_body".into(), 1.0);

        // -- ForStatementInit
        defaults.insert("for_statement_init_variable_declaration".into(), 0.6);
        defaults.insert("for_statement_init_expression".into(), 0.4);

        // -- ForStatement
        defaults.insert("for_statement_init".into(), 0.7);
        defaults.insert("for_statement_test".into(), 0.7);
        defaults.insert("for_statement_update".into(), 0.7);
        defaults.insert("for_statement_body".into(), 1.0);

        // -- ForInLeft
        defaults.insert("for_in_left_variable_declaration".into(), 0.6);
        defaults.insert("for_in_left_expression".into(), 0.4);

        // -- ForInStatement
        defaults.insert("for_in_statement_left".into(), 1.0);
        defaults.insert("for_in_statement_right".into(), 1.0);
        defaults.insert("for_in_statement_body".into(), 1.0);

        // -- ForOfLeft
        defaults.insert("for_of_left_variable_declaration".into(), 0.6);
        defaults.insert("for_of_left_expression".into(), 0.4);

        // -- ForOfStatement
        defaults.insert("for_of_statement_left".into(), 1.0);
        defaults.insert("for_of_statement_right".into(), 1.0);
        defaults.insert("for_of_statement_body".into(), 1.0);

        // -- VariableDeclarationKind
        defaults.insert("var_decl_kind_var".into(), 0.8);
        defaults.insert("var_decl_kind_let".into(), 0.7);
        defaults.insert("var_decl_kind_const".into(), 0.5);

        // -- VariableDeclaration
        defaults.insert("variable_declaration_kind".into(), 1.0);
        defaults.insert("variable_declaration_declarations".into(), 1.0);

        // -- VariableDeclarator
        defaults.insert("variable_declarator_id".into(), 1.0);
        defaults.insert("variable_declarator_init".into(), 0.7);

        // -- FunctionDeclaration
        defaults.insert("function_declaration_id".into(), 0.4);
        defaults.insert("function_declaration_params".into(), 1.0);
        defaults.insert("function_declaration_body".into(), 1.0);
        defaults.insert("function_declaration_generator".into(), 0.2);
        defaults.insert("function_declaration_async".into(), 0.3);

        // -- ClassDeclaration
        defaults.insert("class_declaration_id".into(), 0.4);
        defaults.insert("class_declaration_super_class".into(), 0.3);
        defaults.insert("class_declaration_body".into(), 1.0);

        // -- ClassBody
        defaults.insert("class_body_body".into(), 1.0);

        // -- ClassElement
        defaults.insert("class_element_method".into(), 0.5);
        defaults.insert("class_element_property".into(), 0.4);
        defaults.insert("class_element_static_block".into(), 0.1);

        // -- ClassMethod
        defaults.insert("class_method_key".into(), 1.0);
        defaults.insert("class_method_value".into(), 1.0);
        defaults.insert("class_method_kind".into(), 1.0);
        defaults.insert("class_method_computed".into(), 0.2);
        defaults.insert("class_method_is_static".into(), 0.2);
        defaults.insert("class_method_optional".into(), 0.1);

        // -- MethodKind
        defaults.insert("method_kind_constructor".into(), 0.4);
        defaults.insert("method_kind_method".into(), 0.5);
        defaults.insert("method_kind_get".into(), 0.05);
        defaults.insert("method_kind_set".into(), 0.05);

        // -- ClassProperty
        defaults.insert("class_property_key".into(), 1.0);
        defaults.insert("class_property_value".into(), 0.3);
        defaults.insert("class_property_computed".into(), 0.2);
        defaults.insert("class_property_is_static".into(), 0.2);
        defaults.insert("class_property_readonly".into(), 0.1);

        // -- StaticBlock
        defaults.insert("static_block_body".into(), 1.0);

        // -- Expression (enum)
        defaults.insert("expression_this".into(), 0.4);
        defaults.insert("expression_identifier".into(), 1.0);
        defaults.insert("expression_super".into(), 0.2);
        defaults.insert("expression_literal".into(), 1.0);
        defaults.insert("expression_array".into(), 0.6);
        defaults.insert("expression_object".into(), 0.6);
        defaults.insert("expression_function".into(), 0.6);
        defaults.insert("expression_arrow_function".into(), 0.7);
        defaults.insert("expression_class".into(), 0.3);
        defaults.insert("expression_await".into(), 0.2);
        defaults.insert("expression_yield".into(), 0.2);
        defaults.insert("expression_unary".into(), 0.5);
        defaults.insert("expression_binary".into(), 0.8);
        defaults.insert("expression_logical".into(), 0.7);
        defaults.insert("expression_assignment".into(), 0.6);
        defaults.insert("expression_update".into(), 0.5);
        defaults.insert("expression_conditional".into(), 0.4);
        defaults.insert("expression_call".into(), 0.9);
        defaults.insert("expression_new".into(), 0.4);
        defaults.insert("expression_member".into(), 0.9);
        defaults.insert("expression_sequence".into(), 0.2);
        defaults.insert("expression_template_literal".into(), 0.5);
        defaults.insert("expression_tagged_template".into(), 0.3);
        defaults.insert("expression_import".into(), 0.2);
        defaults.insert("expression_meta_property".into(), 0.1);

        // -- Identifier
        defaults.insert("identifier_name".into(), 1.0);

        // -- Literal (enum)
        defaults.insert("literal_null".into(), 0.4);
        defaults.insert("literal_boolean".into(), 0.6);
        defaults.insert("literal_numeric".into(), 1.0);
        defaults.insert("literal_bigint".into(), 0.3);
        defaults.insert("literal_string".into(), 1.0);
        defaults.insert("literal_regexp".into(), 0.3);

        // -- BooleanLiteral
        defaults.insert("boolean_literal_value".into(), 1.0);

        // -- NumericLiteral
        defaults.insert("numeric_literal_value".into(), 1.0);

        // -- BigIntLiteral
        defaults.insert("bigint_literal_value".into(), 1.0);

        // -- StringLiteral
        defaults.insert("string_literal_value".into(), 1.0);

        // -- RegExpLiteral
        defaults.insert("regexp_literal_pattern".into(), 1.0);
        defaults.insert("regexp_literal_flags".into(), 1.0);

        // -- ArrayExpression
        defaults.insert("array_expression_elements".into(), 1.0);

        // -- ObjectExpression
        defaults.insert("object_expression_properties".into(), 1.0);

        // -- ObjectProperty (enum)
        defaults.insert("object_property_key_value".into(), 0.8);
        defaults.insert("object_property_spread".into(), 0.2);

        // -- ObjectKeyValue
        defaults.insert("object_key_value_key".into(), 1.0);
        defaults.insert("object_key_value_value".into(), 1.0);
        defaults.insert("object_key_value_computed".into(), 0.2);
        defaults.insert("object_key_value_shorthand".into(), 0.2);

        // -- SpreadElement
        defaults.insert("spread_element_argument".into(), 1.0);

        // -- FunctionExpression
        defaults.insert("function_expression_id".into(), 0.3);
        defaults.insert("function_expression_params".into(), 1.0);
        defaults.insert("function_expression_body".into(), 1.0);
        defaults.insert("function_expression_generator".into(), 0.2);
        defaults.insert("function_expression_async".into(), 0.3);

        // -- ArrowFunctionExpression
        defaults.insert("arrow_function_expression_params".into(), 1.0);
        defaults.insert("arrow_function_expression_body".into(), 1.0);
        defaults.insert("arrow_function_expression_async".into(), 0.3);

        // -- ArrowFunctionBody (enum)
        defaults.insert("arrow_function_body_expression".into(), 0.6);
        defaults.insert("arrow_function_body_block".into(), 0.4);

        // -- ClassExpression
        defaults.insert("class_expression_id".into(), 0.3);
        defaults.insert("class_expression_super_class".into(), 0.3);
        defaults.insert("class_expression_body".into(), 1.0);

        // -- AwaitExpression
        defaults.insert("await_expression_argument".into(), 1.0);

        // -- YieldExpression
        defaults.insert("yield_expression_argument".into(), 0.5);
        defaults.insert("yield_expression_delegate".into(), 0.1);

        // -- UnaryExpression
        defaults.insert("unary_expression_operator".into(), 1.0);
        defaults.insert("unary_expression_argument".into(), 1.0);
        defaults.insert("unary_expression_prefix".into(), 0.5);

        // -- UnaryOperator (enum)
        defaults.insert("unary_operator_minus".into(), 0.1);
        defaults.insert("unary_operator_plus".into(), 0.1);
        defaults.insert("unary_operator_logical_not".into(), 0.2);
        defaults.insert("unary_operator_bitwise_not".into(), 0.1);
        defaults.insert("unary_operator_typeof".into(), 0.2);
        defaults.insert("unary_operator_void".into(), 0.05);
        defaults.insert("unary_operator_delete".into(), 0.05);

        // -- BinaryExpression
        defaults.insert("binary_expression_operator".into(), 1.0);
        defaults.insert("binary_expression_left".into(), 1.0);
        defaults.insert("binary_expression_right".into(), 1.0);

        // -- BinaryOperator (enum)
        defaults.insert("binary_operator_equal".into(), 0.4);
        defaults.insert("binary_operator_not_equal".into(), 0.4);
        defaults.insert("binary_operator_strict_equal".into(), 0.7);
        defaults.insert("binary_operator_strict_not_equal".into(), 0.7);
        defaults.insert("binary_operator_less_than".into(), 0.5);
        defaults.insert("binary_operator_less_equal".into(), 0.4);
        defaults.insert("binary_operator_greater_than".into(), 0.4);
        defaults.insert("binary_operator_greater_equal".into(), 0.4);
        defaults.insert("binary_operator_left_shift".into(), 0.2);
        defaults.insert("binary_operator_right_shift".into(), 0.2);
        defaults.insert("binary_operator_unsigned_right_shift".into(), 0.1);
        defaults.insert("binary_operator_add".into(), 0.8);
        defaults.insert("binary_operator_subtract".into(), 0.7);
        defaults.insert("binary_operator_multiply".into(), 0.6);
        defaults.insert("binary_operator_divide".into(), 0.5);
        defaults.insert("binary_operator_modulus".into(), 0.4);
        defaults.insert("binary_operator_exponentiation".into(), 0.3);
        defaults.insert("binary_operator_in".into(), 0.1);
        defaults.insert("binary_operator_instanceof".into(), 0.2);

        // -- LogicalExpression
        defaults.insert("logical_expression_operator".into(), 1.0);
        defaults.insert("logical_expression_left".into(), 1.0);
        defaults.insert("logical_expression_right".into(), 1.0);

        // -- LogicalOperator (enum)
        defaults.insert("logical_operator_or".into(), 0.6);
        defaults.insert("logical_operator_and".into(), 0.6);

        // -- AssignmentExpression
        defaults.insert("assignment_expression_left".into(), 1.0);
        defaults.insert("assignment_expression_right".into(), 1.0);
        defaults.insert("assignment_expression_operator".into(), 1.0);

        // -- AssignmentOperator (enum)
        defaults.insert("assignment_operator_assign".into(), 0.8);
        defaults.insert("assignment_operator_add_assign".into(), 0.3);
        defaults.insert("assignment_operator_sub_assign".into(), 0.3);
        defaults.insert("assignment_operator_mul_assign".into(), 0.2);
        defaults.insert("assignment_operator_div_assign".into(), 0.2);
        defaults.insert("assignment_operator_mod_assign".into(), 0.2);
        defaults.insert("assignment_operator_left_shift_assign".into(), 0.1);
        defaults.insert("assignment_operator_right_shift_assign".into(), 0.1);
        defaults.insert("assignment_operator_unsigned_right_shift_assign".into(), 0.1);
        defaults.insert("assignment_operator_bit_or_assign".into(), 0.1);
        defaults.insert("assignment_operator_bit_xor_assign".into(), 0.1);
        defaults.insert("assignment_operator_bit_and_assign".into(), 0.1);
        defaults.insert("assignment_operator_exponentiation_assign".into(), 0.1);

        // -- AssignmentTarget (enum)
        defaults.insert("assignment_target_expression".into(), 0.8);
        defaults.insert("assignment_target_pattern".into(), 0.2);

        // -- UpdateExpression
        defaults.insert("update_expression_operator".into(), 1.0);
        defaults.insert("update_expression_argument".into(), 1.0);
        defaults.insert("update_expression_prefix".into(), 0.5);

        // -- UpdateOperator (enum)
        defaults.insert("update_operator_increment".into(), 0.6);
        defaults.insert("update_operator_decrement".into(), 0.4);

        // -- ConditionalExpression
        defaults.insert("conditional_expression_test".into(), 1.0);
        defaults.insert("conditional_expression_consequent".into(), 1.0);
        defaults.insert("conditional_expression_alternate".into(), 1.0);

        // -- CallExpression
        defaults.insert("call_expression_callee".into(), 1.0);
        defaults.insert("call_expression_arguments".into(), 1.0);
        defaults.insert("call_expression_optional".into(), 0.1);

        // -- CallArgument (enum)
        defaults.insert("call_argument_expression".into(), 0.8);
        defaults.insert("call_argument_spread".into(), 0.2);

        // -- NewExpression
        defaults.insert("new_expression_callee".into(), 1.0);
        defaults.insert("new_expression_arguments".into(), 1.0);

        // -- MemberExpression
        defaults.insert("member_expression_object".into(), 1.0);
        defaults.insert("member_expression_property".into(), 1.0);
        defaults.insert("member_expression_computed".into(), 0.2);
        defaults.insert("member_expression_optional".into(), 0.1);

        // -- SequenceExpression
        defaults.insert("sequence_expression_expressions".into(), 1.0);

        // -- TemplateLiteral
        defaults.insert("template_literal_quasis".into(), 1.0);
        defaults.insert("template_literal_expressions".into(), 1.0);

        // -- TemplateElement
        defaults.insert("template_element_value".into(), 1.0);
        defaults.insert("template_element_tail".into(), 0.5);

        // -- TemplateElementValue
        defaults.insert("template_element_value_cooked".into(), 0.7);
        defaults.insert("template_element_value_raw".into(), 1.0);

        // -- TaggedTemplateExpression
        defaults.insert("tagged_template_expression_tag".into(), 1.0);
        defaults.insert("tagged_template_expression_quasi".into(), 1.0);

        // -- ImportExpression
        defaults.insert("import_expression_source".into(), 1.0);

        // -- MetaProperty
        defaults.insert("meta_property_meta".into(), 1.0);
        defaults.insert("meta_property_property".into(), 1.0);

        // -- Pattern (enum)
        defaults.insert("pattern_identifier".into(), 0.7);
        defaults.insert("pattern_object".into(), 0.4);
        defaults.insert("pattern_array".into(), 0.4);
        defaults.insert("pattern_rest".into(), 0.2);
        defaults.insert("pattern_assignment".into(), 0.2);

        // -- ObjectPattern
        defaults.insert("object_pattern_properties".into(), 1.0);

        // -- ObjectPatternProperty (enum)
        defaults.insert("object_pattern_property_key_value".into(), 0.8);
        defaults.insert("object_pattern_property_rest".into(), 0.2);

        // -- ObjectPatternKeyValue
        defaults.insert("object_pattern_key_value_key".into(), 1.0);
        defaults.insert("object_pattern_key_value_value".into(), 1.0);
        defaults.insert("object_pattern_key_value_computed".into(), 0.2);

        // -- ArrayPattern
        defaults.insert("array_pattern_elements".into(), 1.0);

        // -- RestElement
        defaults.insert("rest_element_argument".into(), 1.0);

        // -- AssignmentPattern
        defaults.insert("assignment_pattern_left".into(), 1.0);
        defaults.insert("assignment_pattern_right".into(), 1.0);

        // -- ImportDeclaration
        defaults.insert("import_declaration_specifiers".into(), 1.0);
        defaults.insert("import_declaration_source".into(), 1.0);

        // -- ImportSpecifier (enum)
        defaults.insert("import_specifier_default".into(), 0.4);
        defaults.insert("import_specifier_named".into(), 0.5);
        defaults.insert("import_specifier_namespace".into(), 0.1);

        // -- ImportDefaultSpecifier
        defaults.insert("import_default_specifier_local".into(), 1.0);

        // -- ImportNamedSpecifier
        defaults.insert("import_named_specifier_imported".into(), 1.0);
        defaults.insert("import_named_specifier_local".into(), 1.0);

        // -- ImportNamespaceSpecifier
        defaults.insert("import_namespace_specifier_local".into(), 1.0);

        // -- ExportDeclaration (enum)
        defaults.insert("export_declaration_export_all".into(), 0.2);
        defaults.insert("export_declaration_export_named".into(), 0.5);
        defaults.insert("export_declaration_export_default".into(), 0.2);
        defaults.insert("export_declaration_export_declaration".into(), 0.1);

        // -- ExportAllDeclaration
        defaults.insert("export_all_declaration_source".into(), 1.0);

        // -- ExportNamedDeclaration
        defaults.insert("export_named_declaration_declaration".into(), 0.4);
        defaults.insert("export_named_declaration_specifiers".into(), 0.6);
        defaults.insert("export_named_declaration_source".into(), 0.2);

        // -- ExportSpecifier (enum)
        defaults.insert("export_specifier_named".into(), 1.0);

        // -- ExportNamedSpecifier
        defaults.insert("export_named_specifier_exported".into(), 1.0);
        defaults.insert("export_named_specifier_local".into(), 1.0);

        // -- ExportDefaultDeclaration
        defaults.insert("export_default_declaration_declaration".into(), 1.0);

        // -- ExportDeclarationDeclaration
        defaults.insert("export_declaration_declaration".into(), 1.0);

        // -- Declaration (enum)
        defaults.insert("declaration_function".into(), 0.6);
        defaults.insert("declaration_class".into(), 0.3);
        defaults.insert("declaration_variable".into(), 0.6);

        // If any user overrides are provided, apply them last
        if let Some(overrides) = user_overrides {
            defaults.extend(overrides);
        }

        Self { weights: defaults }
    }

    /// Retrieves the probability weight for a given AST node type.
    ///
    /// # Parameters
    /// * `key` - String identifier for the node type (e.g., "statement_if")
    ///
    /// # Returns
    /// The probability weight as a f64, defaulting to 0.5 if the key doesn't exist
    pub fn get_weight(&self, key: &str) -> f64 {
        *self.weights.get(key).unwrap_or(&0.5)
    }
}
