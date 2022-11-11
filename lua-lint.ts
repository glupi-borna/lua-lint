import { readFileSync } from 'fs';

type This<T extends new(...args: any) => any> = {
	new(...args: ConstructorParameters<T>): any
} & Pick<T, keyof T>;

type Literal<LIT, TYPE> = TYPE extends LIT ? never : LIT;

interface Char_Classifier {
    includes(code: number): boolean;
}

class Char_Range {
    start: number;
    end: number;
    constructor(range: string) {
        this.start = range.charCodeAt(0);
        this.end = range.charCodeAt(1);
    }
    includes(code: number) {
        return code >= this.start && code <= this.end;
    }
}

class Char_List {
    codes: number[];
    constructor(chars: string) {
        this.codes = chars.split("").map(c => c.charCodeAt(0));
    }
    includes(code: number) {
        return this.codes.includes(code);
    }
}

class Char_Classifiers {
    classifiers: Char_Classifier[];
    constructor(...ranges: (string|Char_Classifier)[]) {
        this.classifiers = ranges.map(r => typeof(r) === "string" ? new Char_Range(r) : r);
    }
    includes(char: string) {
        let code = char.charCodeAt(0);
        for (let range of this.classifiers) {
            if (range.includes(code)) return true;
        }
        return false;
    }
}

const LETTERS = new Char_Classifiers("az", "AZ");
const DIGITS = new Char_Classifiers("09");
const IDENT_CHARS = new Char_Classifiers(...LETTERS.classifiers, ...DIGITS.classifiers, "__");

class Position {
    readonly index: number;
    readonly line: number;

    constructor(idx: number, ln: number) {
        this.index = idx;
        this.line = ln;
    }

    copy() {
        return new Position(this.index, this.line);
    }

    set(pos: Position) {
        (this.index as number) = pos.index;
        (this.line as number) = pos.line;
    }

    move(amt=1) {
        (this.index as number) += amt;
    }

    nextLine() {
        (this.line as number)++;
    }
}

const KEYWORDS = [
    "and", "break", "do", "else", "elseif",
    "end", "false", "for", "function", "if",
    "in", "local", "nil", "not", "or",
    "repeat", "return", "then", "true", "until", "while"
];

const SIMPLE_SYMBOLS = new Char_Classifiers(new Char_List("+-*/%^#<>=(){}[];:,."));

enum AST_NODE {
    IDENTIFIER, NUMBER, STRING,
    DELETEME_NAME, VAR, PREFIX, SUFFIX, EXPR, VALUE,
    UNARY_OPERATOR, BINARY_OPERATOR,
    UNARY_OPERATION, BINARY_OPERATION,
    CHUNK, BLOCK, STATEMENT, LAST_STATEMENT,
    FUNC_NAME_NO_METHOD, VARLIST, NAMELIST,
    FUNC_CALL, ARGS, FUNC, FUNC_BODY,
    TABLE, FIELD, FIELD_SEP, DOT_ACCESS,
    INDEX, CALL, INDEX_ACCESS, METHOD_CALL,
    PARENS, BRACES, BRACKETS, NAMED_FIELD,
    RETURN, ASSIGN, SUFFIXES, METHOD_NAME, FUNC_NAME,
    VAR_ARGS, PARAMS, PARAM_LIST, FUNC_DECL, LOCAL_FUNC_DECL,
    ASSIGN_EXPRS, LOCAL_VAR_DECL, FOR, FOR_STEP, FOR_IN,

    STATEMENT_LIST, IDENTIFIER_LIST, VAR_LIST,
    EXPR_LIST, FIELD_LIST_WITH_TRAILER, FIELD_LIST,
    ELSEIF_LIST,

    DO_BLOCK, WHILE, REPEAT, IF, ELSE, ELSEIF,

    SYM_ADD, SYM_SUB, SYM_MUL, SYM_DIV, SYM_MOD,
    SYM_EXP, SYM_LEN, SYM_LESS, SYM_MORE, SYM_ASSIGN,
    SYM_EQUALS, SYM_NOT_EQUALS, SYM_LESS_EQUALS,
    SYM_MORE_EQUALS, SYM_PAR_OPEN, SYM_PAR_CLOSE,
    SYM_BRACE_OPEN, SYM_BRACE_CLOSE, SYM_BRACKET_OPEN,
    SYM_BRACKET_CLOSE, SYM_CONCAT, SYM_DOT, SYM_COMMA,
    SYM_COLON, SYM_SEMICOLON, SYM_DOTDOTDOT,

    KWD_AND, KWD_BREAK, KWD_DO, KWD_ELSE, KWD_ELSEIF,
    KWD_END, KWD_FALSE, KWD_FOR, KWD_FUNCTION, KWD_IF,
    KWD_IN, KWD_LOCAL, KWD_NIL, KWD_NOT, KWD_OR,
    KWD_REPEAT, KWD_RETURN, KWD_THEN, KWD_TRUE, KWD_UNTIL, KWD_WHILE,

    UNKNOWN,
};

type AST_CLASS_MAP = Record<string, AST_NODE>;

const AST_NODE_KEYWORD_MAP = {
    "and": AST_NODE.KWD_AND,
    "break": AST_NODE.KWD_BREAK,
    "do": AST_NODE.KWD_DO,
    "else": AST_NODE.KWD_ELSE,
    "elseif": AST_NODE.KWD_ELSEIF,
    "end": AST_NODE.KWD_END,
    "false": AST_NODE.KWD_FALSE,
    "for": AST_NODE.KWD_FOR,
    "function": AST_NODE.KWD_FUNCTION,
    "if": AST_NODE.KWD_IF,
    "in": AST_NODE.KWD_IN,
    "local": AST_NODE.KWD_LOCAL,
    "nil": AST_NODE.KWD_NIL,
    "not": AST_NODE.KWD_NOT,
    "or": AST_NODE.KWD_OR,
    "repeat": AST_NODE.KWD_REPEAT,
    "return": AST_NODE.KWD_RETURN,
    "then": AST_NODE.KWD_THEN,
    "true": AST_NODE.KWD_TRUE,
    "until": AST_NODE.KWD_UNTIL,
    "while": AST_NODE.KWD_WHILE,
} as const;

class AST_Node {
    static type: AST_NODE = AST_NODE.UNKNOWN;
    start: Position;
    end: Position;

    constructor(public parser: Parser) {
        this.parser = parser;
        this.start = parser.start.copy();
        this.end = parser.current.copy();
    }

    get type() {
        return (this.constructor as typeof AST_Node).type;
    }

    get kind() {
        return AST_NODE[(this.constructor as typeof AST_Node).type];
    }

    get text() {
        return this.parser.text.substring(this.start.index, this.end.index);
    }

    withPos(...tokens: (AST_Node|undefined)[]) {
        let min_pos = this.start;
        let max_pos = this.end;
        for (let token of tokens) {
            if (!token) continue;
            if (token.start.index < min_pos.index) min_pos = token.start;
            if (token.end.index > max_pos.index) max_pos = token.end;
        }
        this.start.set(min_pos);
        this.end.set(max_pos);
        return this;
    }

    keys() {
        return Object.keys(this).
            concat("text", "kind").
            filter(k => ![
                "parser", "start", "end", "type"
            ].includes(k)
        );
    }

    toString() {
        let n = this.constructor.name;
        let props = this.keys().map((p) => `${p}: ${(this as any)[p]}`).join(", ");
        return `${n}(${props})`;
    }
}

abstract class AST_Wrapper<T extends AST_Node> extends AST_Node {
    constructor(parser: Parser, public value: T) {
        super(parser);
        this.withPos(value);
    }
}

abstract class AST_Sequence<T extends Record<string, AST_Node>> extends AST_Node {
    constructor(
        parser: Parser,
        public fields: T
    ) {
        super(parser);
        this.withPos(...Object.values(fields));
    }
}

abstract class AST_Wrapped<
    OPEN extends AST_Node,
    T extends AST_Node,
    CLOSE extends AST_Node
> extends AST_Sequence<{
    open: OPEN,
    value: T,
    close: CLOSE
}> {}

abstract class AST_Prefixed<
    PREFIX extends AST_Node,
    T extends AST_Node
> extends AST_Sequence<{
    prefix: PREFIX,
    value: T
}> {}

abstract class AST_List<T extends AST_Node> extends AST_Node {
    constructor(
        parser: Parser,
        public list: T[]
    ) {
        super(parser);
        this.withPos(...list);
    }
}

abstract class AST_Delimited<T extends AST_Node, DELIMITER extends AST_Node> extends AST_List<T> {
    constructor(
        parser: Parser,
        first?: T,
        ...args: AST_Prefixed<DELIMITER, T>[]
    ) {
        let ts: T[] = [];
        if (first) ts.push(first);
        for (let arg of args) ts.push(arg.fields.value);
        super(parser, ts);
        this.withPos(first, ...args);
    }
}

function ast_class<
    TEXT extends string,
    AST extends AST_NODE
>(
    ast_text: Literal<TEXT, string>,
    ast_type: Literal<AST, AST_NODE>,
) {
    return class AST_Node_Simple extends AST_Node {
        static literal = ast_text;
        static type = ast_type;

        constructor(parser: Parser) { super(parser); }
        static match<T extends This<typeof AST_Node_Simple>>(this: T, obj: any): obj is InstanceType<T> {
            if (!(obj instanceof AST_Node)) return false;
            return obj.text == this.literal && obj.type == this.type;
        }
    };
}

function ast_class_map<
    T extends AST_CLASS_MAP
>(
    obj: Literal<T, AST_CLASS_MAP>
): {
    [KEY in keyof T]: KEY extends string ? ReturnType<typeof ast_class<KEY, T[KEY]>> : never
} {
    let out = {} as any;
    for (let key in obj) {
        out[key] = (ast_class as any)(key, obj[key]);
    }
    return out as ReturnType<typeof ast_class_map<T>>;
}


const AST_Keywords = ast_class_map({
    "and": AST_NODE.KWD_AND,
    "break": AST_NODE.KWD_BREAK,
    "do": AST_NODE.KWD_DO,
    "else": AST_NODE.KWD_ELSE,
    "elseif": AST_NODE.KWD_ELSEIF,
    "end": AST_NODE.KWD_END,
    "false": AST_NODE.KWD_FALSE,
    "for": AST_NODE.KWD_FOR,
    "function": AST_NODE.KWD_FUNCTION,
    "if": AST_NODE.KWD_IF,
    "in": AST_NODE.KWD_IN,
    "local": AST_NODE.KWD_LOCAL,
    "nil": AST_NODE.KWD_NIL,
    "not": AST_NODE.KWD_NOT,
    "or": AST_NODE.KWD_OR,
    "repeat": AST_NODE.KWD_REPEAT,
    "return": AST_NODE.KWD_RETURN,
    "then": AST_NODE.KWD_THEN,
    "true": AST_NODE.KWD_TRUE,
    "until": AST_NODE.KWD_UNTIL,
    "while": AST_NODE.KWD_WHILE,
});
type AST_KEYWORDS = typeof AST_Keywords;
type AST_KEYWORD<T extends keyof AST_KEYWORDS> = InstanceType<typeof AST_Keywords[T]>;

const AST_Symbols = ast_class_map({
    "+": AST_NODE.SYM_ADD,
    "-": AST_NODE.SYM_SUB,
    "*": AST_NODE.SYM_MUL,
    "/": AST_NODE.SYM_DIV,
    "%": AST_NODE.SYM_MOD,
    "^": AST_NODE.SYM_EXP,
    "#": AST_NODE.SYM_LEN,
    "<": AST_NODE.SYM_LESS,
    ">": AST_NODE.SYM_MORE,
    "=": AST_NODE.SYM_ASSIGN,
    "==": AST_NODE.SYM_EQUALS,
    "~=": AST_NODE.SYM_NOT_EQUALS,
    "<=": AST_NODE.SYM_LESS_EQUALS,
    ">=": AST_NODE.SYM_MORE_EQUALS,
    "(": AST_NODE.SYM_PAR_OPEN,
    ")": AST_NODE.SYM_PAR_CLOSE,
    "{": AST_NODE.SYM_BRACE_OPEN,
    "}": AST_NODE.SYM_BRACE_CLOSE,
    "[": AST_NODE.SYM_BRACKET_OPEN,
    "]": AST_NODE.SYM_BRACKET_CLOSE,
    "..": AST_NODE.SYM_CONCAT,
    ".": AST_NODE.SYM_DOT,
    ",": AST_NODE.SYM_COMMA,
    ":": AST_NODE.SYM_COLON,
    ";": AST_NODE.SYM_SEMICOLON,
    "...": AST_NODE.SYM_DOTDOTDOT,
});
type AST_SYMBOLS = typeof AST_Symbols;
type AST_SYMBOL<T extends keyof AST_SYMBOLS> = InstanceType<typeof AST_Symbols[T]>;

class Identifier extends AST_Node { static type = AST_NODE.IDENTIFIER; }
class Num extends AST_Node { static type = AST_NODE.NUMBER; }
class Str extends AST_Node { static type = AST_NODE.STRING; }

class Unary_Operator extends AST_Node { static type = AST_NODE.UNARY_OPERATOR; }
class Binary_Operator extends AST_Node { static type = AST_NODE.BINARY_OPERATOR; }

class Parens<T extends AST_Node> extends AST_Wrapped<AST_SYMBOL<"(">, T, AST_SYMBOL<")">> {
    static type = AST_NODE.PARENS;
}

class Braces<T extends AST_Node> extends AST_Wrapped<AST_SYMBOL<"{">, T, AST_SYMBOL<"}">> {
    static type = AST_NODE.BRACES;
}

class Brackets<T extends AST_Node> extends AST_Wrapped<AST_SYMBOL<"[">, T, AST_SYMBOL<"]">> {
    static type = AST_NODE.BRACKETS;
}

/**
nil | false | true | Number |
String | '...' | function |
tableconstructor | functioncall |
var | '(' exp ')'
*/
type Value = AST_KEYWORD<"nil"> | AST_KEYWORD<"false"> | AST_KEYWORD<"true"> |
    Num | Str | AST_SYMBOL<"..."> | Func | Table |
    Func_Call | Var | Parens<Expr>;


class Suffixes extends AST_List<Suffix> {
    static type = AST_NODE.SUFFIXES;
}

class Func_Call extends AST_Sequence<{
    prefix: Prefix,
    suffixes: Suffixes,
    call: Call
}> {
    static type = AST_NODE.FUNC_CALL;
}

class Func_Name_No_Method extends AST_Delimited<Identifier, AST_SYMBOL<".">> {
    static type = AST_NODE.FUNC_NAME_NO_METHOD;
}

class Method_Name extends AST_Sequence<{
    colon: AST_SYMBOL<":">;
    name: Identifier;
}> {
    static type = AST_NODE.METHOD_NAME;
}

class Func_Name extends AST_Sequence<{
    name: Func_Name_No_Method,
    method?: Method_Name
}> {
    static type = AST_NODE.FUNC_NAME;
}

/** prefix {suffix} index */
class Index_Access extends AST_Node {
    static type = AST_NODE.INDEX_ACCESS;
    constructor(
        parser: Parser,
        public prefix: Prefix,
        public suffixes: Suffix[],
        public index: Index
    ) {
        super(parser);
        this.withPos(this.prefix, this.index, ...this.suffixes);
    }
}

/** 'function' functionbody */
class Func extends AST_Sequence<{
    keyword: AST_KEYWORD<"function">,
    body: Func_Body
}> {
    static type = AST_NODE.FUNC;

}

/** {stat [`;´]} [laststat [`;´]] */
class Block extends AST_Sequence<{
    statements: Statement_List,
    last_statement?: Last_Statement
}> {
    static type = AST_NODE.BLOCK;
}

class Identifier_List extends AST_Delimited<Identifier, AST_SYMBOL<",">> {
    static type = AST_NODE.IDENTIFIER_LIST;
}

class Var_List extends AST_Delimited<Var, AST_SYMBOL<",">> {
    static type = AST_NODE.VAR_LIST;
}

class Expr_List extends AST_Delimited<Expr, AST_SYMBOL<",">> {
    static type = AST_NODE.EXPR_LIST;
}

class Var_Args extends AST_Prefixed<AST_SYMBOL<",">, AST_SYMBOL<"...">> {
    static type = AST_NODE.VAR_ARGS;
}

class Param_List extends AST_Sequence<{
    params: Identifier_List,
    varargs?: Var_Args
}> {
    static type = AST_NODE.PARAMS;
}

type Params = AST_SYMBOL<"..."> | Param_List;

/** `(´ [parlist] `)´ block end */
class Func_Body extends AST_Sequence<{
    params: Parens<Params>,
    body: Block,
    end: AST_KEYWORD<"end">
}> {
    static type = AST_NODE.FUNC_BODY;
}

class Var extends AST_Wrapper<Index_Access|Identifier> { static type = AST_NODE.VAR; }

class Binary_Operation extends AST_Node {
    static type = AST_NODE.BINARY_OPERATION;

    constructor(
        parser: Parser,
        public left: Value,
        public op: Binary_Operator,
        public right: Expr
    ) {
        super(parser);
        this.withPos(this.left, this.right);
    }
}

class Unary_Operation extends AST_Node {
    static type = AST_NODE.UNARY_OPERATION;

    constructor(
        parser: Parser,
        public op: Unary_Operator,
        public exp: Expr
    ) {
        super(parser);
        this.withPos(this.op, this.exp);
    }
}

/** '(' exp ')' | IDENTIFIER */
type Prefix = Parens<Expr|Identifier>;
type Suffix = Call|Index;

class Dot_Access extends AST_Prefixed<AST_SYMBOL<".">, Identifier> {
    static type = AST_NODE.DOT_ACCESS;
}

/** '[' exp ']' | '.' IDENTIFIER */
type Index = Brackets<Expr> | Dot_Access;

/** ':' IDENTIFIER args */
class Method_Call extends AST_Node {
    static type = AST_NODE.METHOD_CALL;
    constructor(
        parser: Parser,
        public method: Method_Name,
        public args: Args,
    ) {
        super(parser);
        this.withPos(method, args);
    }
}

/** args | ':' IDENTIFIER args */
type Call = Args | Method_Call;

/** `(´ [explist] `)´ | tableconstructor | String */
type Args = Parens<Expr_List|Table|Str>;
type Expr = Binary_Operation|Unary_Operation|Value;

/** `{´ [fieldlist] `}´ */
class Table extends Braces<Field_List_With_Trailer> { static type = AST_NODE.TABLE; }

class Named_Field extends AST_Node {
    static type = AST_NODE.NAMED_FIELD;

    constructor(
        parser: Parser,
        public name: Brackets<Expr> | Identifier,
        public equals: AST_SYMBOL<"=">,
        public expression: Expr
    ) {
        super(parser);
        this.withPos(name, equals, expression);
    }
}

type Field = Named_Field|Expr;
type Field_Sep = AST_SYMBOL<","> | AST_SYMBOL<";">;

class Field_List extends AST_Delimited<Field, Field_Sep> {
    static type = AST_NODE.FIELD_LIST;
}

class Field_List_With_Trailer extends AST_Sequence<{
    field_list: Field_List,
    trailer?: Field_Sep
}> {
    static type = AST_NODE.FIELD_LIST_WITH_TRAILER;
}

class Return_Statement extends AST_Prefixed<AST_KEYWORD<"return">, Expr> {
    static type = AST_NODE.RETURN;
}

class Last_Statement extends AST_Sequence<{
    statement: Return_Statement | AST_KEYWORD<"break">
    semicolon?: AST_SYMBOL<";">
}> {
    static type = AST_NODE.LAST_STATEMENT;
}

class Assign extends AST_Sequence<{
    vars: Var_List,
    exprs: Assign_Exprs
}> {
    static type = AST_NODE.ASSIGN;
}

class Do_Block extends AST_Sequence<{
    do: AST_KEYWORD<"do">,
    block: Block,
    end: AST_KEYWORD<"end">
}> {
    static type = AST_NODE.DO_BLOCK;
}

class While extends AST_Sequence<{
    while_keyword: AST_KEYWORD<"while">,
    condition: Expr,
    do_keyword: AST_KEYWORD<"do">,
    block: Block,
    end_keyword: AST_KEYWORD<"end">
}> {
    static type = AST_NODE.WHILE;
}

class Repeat extends AST_Sequence<{
    repeat_keyword: AST_KEYWORD<"repeat">,
    block: Block,
    until_keyword: AST_KEYWORD<"until">,
    condition: Expr
}> {
    static type = AST_NODE.REPEAT;
}

class If extends AST_Sequence<{
    if_keyword: AST_KEYWORD<"if">,
    condition: Expr,
    then_keyword: AST_KEYWORD<"then">,
    block: Block,
    elseifs?: ElseIf_List,
    Else?: Else
    end_keyword: AST_KEYWORD<"end">,

}> {
    static type = AST_NODE.IF;
}

class Else extends AST_Sequence<{
    else_keyword: AST_KEYWORD<"else">,
    block: Block
}> {
    static type = AST_NODE.ELSE;
}

class ElseIf extends AST_Sequence<{
    elseif_keyword: AST_KEYWORD<"elseif">,
    condition: Expr,
    then_keyword: AST_KEYWORD<"then">,
    block: Block
}> {
    static type = AST_NODE.ELSEIF;
}

class ElseIf_List extends AST_List<ElseIf> {
    static type = AST_NODE.ELSEIF_LIST;
}

class Func_Decl extends AST_Sequence<{
    func_keyword: AST_KEYWORD<"function">,
    name: Func_Name,
    body: Func_Body
}> {
    static type = AST_NODE.FUNC_DECL;
}

class Local_Func_Decl extends AST_Sequence<{
    local_keyword: AST_KEYWORD<"local">,
    func_keyword: AST_KEYWORD<"function">,
    name: Identifier,
    body: Func_Body
}> {
    static type = AST_NODE.LOCAL_FUNC_DECL;
}

class Assign_Exprs extends AST_Sequence<{
    "assign": AST_SYMBOL<"=">,
    "exprs": Expr_List
}> {
    static type = AST_NODE.ASSIGN_EXPRS;
}

class Local_Var_Decl extends AST_Sequence<{
    local_keyword: AST_KEYWORD<"local">,
    names: Identifier_List,
    exprs?: Assign_Exprs
}> {
    static type = AST_NODE.LOCAL_VAR_DECL;
}

class For extends AST_Sequence<{
    for_keyword: AST_KEYWORD<"for">,
    name: Identifier,
    assign: AST_SYMBOL<"=">,
    init: Expr,
    comma: AST_SYMBOL<",">,
    end: Expr,
    step?: For_Step,
    block: Do_Block
}> {
    static type = AST_NODE.FOR;
}

class For_Step extends AST_Sequence<{
    comma: AST_SYMBOL<",">,
    step: Expr
}> {
    static type = AST_NODE.FOR_STEP;
}

class For_In extends AST_Sequence<{
    for_keyword: AST_KEYWORD<"for">,
    names: Identifier_List,
    in_keyword: AST_KEYWORD<"in">,
    exprs: Expr_List,
    block: Do_Block
}> {
    static type = AST_NODE.FOR_IN;
}

class Statement_List extends AST_List<Statement> {
    static type = AST_NODE.STATEMENT_LIST;
}

type Statement = Assign | Func_Call | Do_Block | While | Repeat | If;

type DropFirst<T extends unknown[]> = T extends [any, ...infer U] ? U : never;
type ReturnTypes<T> = {
    [K in keyof T]: T[K] extends ()=>any ? ReturnType<T[K]> : never
}
type RequiredTypes<T> = {
    [K in keyof T]: NonNullable<T[K]>
}

class Parser {
    text: string;
    start = new Position(0, 0);
    current = new Position(0, 0);

    constructor(text: string) {
        this.text = text;
    }

    char(add=0) { return this.text[this.current.index+add]; }
    move(amt=1) { this.current.move(amt); }
    rollback(token?: AST_Node) { this.current.set((token||this).start); }
    at_end() { return this.current.index >= this.text.length; }

    ast_node<
        T extends AST_Node,
        FN extends new (...args: any) => T,
    >(
        cls: FN,
        ...args: DropFirst<ConstructorParameters<FN>>
    ): InstanceType<typeof cls> {
        let t = new cls(this, ...args);
        this.start = this.current;
        return t as any;
    }

    skip_ws() {
        while (true) {
            switch (this.char()) {
                case " ":
                case "\r":
                case "\t":
                    this.move();
                    break;

                case "\n":
                    this.current.nextLine();
                    this.move();
                    break;

                case "-":
                    if (this.char(1) == "-") {
                        this.move(2);
                        if (!this.long_bracket(AST_Node)) {
                            this.move(-2);
                            while (this.char() != "\n" && !this.at_end()) this.move();
                        }
                    } else {
                        this.start.set(this.current); return;
                    }
                    break;

                default: this.start.set(this.current); return;
            }
        }
    }

    ident() {
        this.skip_ws();
        if (this.at_end()) return undefined;
        let c = this.char();
        if (LETTERS.includes(c) || c == "_") {
            this.move();
            while (IDENT_CHARS.includes(this.char())) this.move();
            return this.ast_node(Identifier);
        } else {
            return undefined;
        }
    }

    num() {
        this.skip_ws();
        if (this.at_end()) return undefined;
        if (!DIGITS.includes(this.char()) && this.char() !== ".") { return undefined; }
        this.move();
        while (DIGITS.includes(this.char())) this.move();
        if (this.char() != ".") return this.ast_node(Num);
        this.move();
        while (DIGITS.includes(this.char())) this.move();
        return this.ast_node(Num);
    }

    symbol() {
        this.skip_ws();
        if (this.at_end()) return undefined;

        switch (this.char()) {
            case "=":
            case "~":
            case "<":
            case ">":
                if (this.char(1) == "=") {
                    this.move(2);
                    return this.ast_node(Sym);
                } else if (this.char() != "~") {
                    this.move(1);
                    return this.ast_node(Sym);
                }
                break;
            case ".":
                if (this.char(1) == ".") {
                    if (this.char(2) == ".") {
                        this.move(3);
                        return this.ast_node(Sym);
                    }
                    this.move(2);
                    return this.ast_node(Sym);
                }
                return this.ast_node(Sym);
        }

        if (!SIMPLE_SYMBOLS.includes(this.char())) return undefined;
        this.move();
        return this.ast_node(Sym);
    }

    long_bracket<T extends AST_Node>(token_type: new (parser: Parser) => T) {
        this.skip_ws();
        if (this.at_end()) return undefined;
        if (this.char() != "[") return undefined;

        this.move();
        let level = 0;
        while (this.char() == "=" && !this.at_end()) { level++; this.move(); }
        if (this.char() != "[") { this.rollback(); return undefined; }
        this.move();

        while (!this.at_end()) {
            if (this.char() == "\n") { this.current.nextLine(); }
            while (this.char() != "]" && !this.at_end()) { this.move(); }
            if (this.char() != "]") {
                this.rollback();
                return undefined;
            }

            this.move();
            let local_level = 0;
            while (local_level < level && this.char() == "=") {
                local_level++; this.move(); continue;
            }
            if (local_level != level) { continue; }
            if (this.char() != "]") { continue; }
            this.move();
            return this.ast_node(token_type);
        }

        this.rollback();
        return undefined;
    }

    str() {
        this.skip_ws();
        if (this.at_end()) return undefined;

        let lb = this.long_bracket(Str);
        if (lb) { return lb; }

        let ch = this.char();
        if (!"'\"".includes(ch)) { return undefined; }
        this.move();

        while (this.char() != ch && !this.at_end()) {
            if (this.char() == "\\") this.move();
            this.move();
        }

        if (this.char() != ch) {
            this.rollback();
            return undefined;
        }
        this.move();
        return this.ast_node(Str);
    }

    sequence<
        FN extends (() => (AST_Node | undefined))[]
    >(
        ...fns: FN
    ): RequiredTypes<ReturnTypes<typeof fns>>|undefined {
        let out = [];
        for (let fn of fns) {
            let res = fn.call(this);
            if (!res) { this.rollback(out[0]); return undefined }
            out.push(res);
        }
        return out as any;
    }

    either<
        FN extends (() => (AST_Node | undefined))[]
    >(
        ...fns: FN
    ): ReturnTypes<typeof fns>[number] | undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;
        for (let fn of fns) {
            let tok = fn.call(this);
            if (tok) return tok as any;
        }
        return undefined;
    }

    other() {
        this.skip_ws();
        if (this.at_end()) return undefined;
        this.move();
        return this.ast_node(AST_Node);
    }

    name() {
        this.skip_ws();
        if (this.at_end()) return undefined;
        let ident = this.ident();
        if (!ident) { return undefined; }
        if (ident.type === AST_NODE.KEYWORD) { this.rollback(ident); return undefined; }
        return ident;
    }

    unop(): Unary_Operator|undefined {
        let op = this.either(
            () => this.exact(this.symbol, "-"),
            () => this.exact(this.keyword, "not"),
            () => this.exact(this.symbol, "#"),
        );
        if (!op) return undefined;
        let u = new Unary_Operator(this);
        u.withPos(op);
        return u;
    }

    binop(): Binary_Operator|undefined {
        let op = this.either(
            () => this.exact(this.symbol, "+"),
            () => this.exact(this.symbol, "-"),
            () => this.exact(this.symbol, "*"),
            () => this.exact(this.symbol, "/"),
            () => this.exact(this.symbol, "^"),
            () => this.exact(this.symbol, "%"),
            () => this.exact(this.symbol, ".."),
            () => this.exact(this.symbol, "<"),
            () => this.exact(this.symbol, ">"),
            () => this.exact(this.symbol, "<="),
            () => this.exact(this.symbol, ">="),
            () => this.exact(this.symbol, "=="),
            () => this.exact(this.symbol, "~="),
            () => this.exact(this.keyword, "and"),
            () => this.exact(this.keyword, "or"),
        );
        if (!op) return undefined;
        let u = new Binary_Operator(this);
        u.withPos(op);
        return u;
    }

    prefix(): Prefix|undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;

        let pref = this.either(
            this.variable,
            () => {
                let s = this.sequence(
                    () => this.exact(this.symbol, "("),
                    this.expr,
                    () => this.exact(this.symbol, ")"),
                );
                if (s) return s[1];
                return undefined;
            }
        );

        if (!pref) { return undefined; }
        return this.ast_node(Prefix, pref);
    }

    value() {
        return this.either(
            () => this.exact(this.keyword, "nil"),
            () => this.exact(this.keyword, "false"),
            () => this.exact(this.keyword, "true"),
            this.num, this.str,
            () => this.exact(this.symbol, "...")
            // TODO: Function
            // TODO: TableConstructor
            // TODO: FunctionCall
            // TODO: var
            // TODO: ( expr )
        );
    }

    keyword(): Keyword|undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;

        let ident = this.ident();
        if (!ident) { return undefined; }
        if (!KEYWORDS.includes(ident.text)) {
            this.rollback(ident);
            return undefined;
        }
        let kwd = new Keyword(this);
        kwd.withPos(ident);
        return kwd;
    }

    expr(): Expr|undefined {
        let val = this.value();
        if (!val) { return undefined; }

        let e = this.ast_node(Expr, val);
        return e;
    }

    list<K extends AST_Node>(fn: () => K|undefined): K[] {
        let arr = [];
        let tok = fn.call(this);
        while (tok) {
            arr.push(tok);
            tok = fn();
        }
        return arr;
    }

    exact<K extends AST_Node>(fn: ()=>K|undefined, text: string): K|undefined {
        let tok = fn.call(this);
        if (!tok) return undefined;
        if (tok.text !== text) { this.rollback(tok); return undefined; }
        return tok;
    }

    variable(): Var|undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;

        let prefix: Identifier|Prefix|undefined = this.name();
        if (!prefix) {
            return undefined;
            prefix = this.prefix();
            if (!prefix) { return undefined; }
        }

        let sym = this.symbol();
        if (!sym || (sym.text != "." && sym.text != "[")) {
            if (sym) { this.rollback(sym); }
            return new Var(this, prefix);
        }

        let exp = this.name();
        if (!exp) { this.rollback(prefix); return undefined; }

        return new Var(this, prefix, exp);
    }

    parse() {
        let tokens = [];
        while (!this.at_end()) {
            let tok = this.either(this.num, this.variable, this.ident, this.str, this.symbol, this.other);
            if (tok) { tokens.push(tok); continue; }
            break;
        }
        return tokens;
    }
}

let file = readFileSync("tests/micro/user.lua", "utf8");
let parser = new Parser(file);
let tokens = parser.parse();

for (let token of tokens) {
    // if (token.type == TOKEN.VAR)
    console.log(token.toString());
}
