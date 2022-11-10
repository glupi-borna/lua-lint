import { readFileSync } from 'fs';

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
const SIMPLE_SYMBOLS = new Char_Classifiers(new Char_List("+-*/%^#<>=(){}[];:,."));

const KEYWORDS = [
    "and", "break", "do", "else", "elseif",
    "end", "false", "for", "function", "if",
    "in", "local", "nil", "not", "or",
    "repeat", "return", "then", "true", "until", "while"];

enum AST_NODE {
    IDENTIFIER, NUMBER, KEYWORD, STRING, SYMBOL,
    DELETEME_NAME, VAR, PREFIX, SUFFIX, EXPR, VALUE,
    UNARY_OPERATOR, BINARY_OPERATOR,
    UNARY_OPERATION, BINARY_OPERATION,
    CHUNK, BLOCK, STATEMENT, LAST_STATEMENT,
    FUNC_NAME, VARLIST, NAMELIST, EXPRLIST,
    FUNC_CALL, ARGS, FUNC, FUNC_BODY,
    PAR_LIST, TABLE, FIELDLIST, FIELD, FIELDSEP,
    INDEX, CALL, INDEX_ACCESS, PARENS,


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

class AST_Node {
    type: AST_NODE = AST_NODE.UNKNOWN;
    parser: Parser;
    start: Position;
    end: Position;

    constructor(parser: Parser) {
        this.parser = parser;
        this.start = parser.start.copy();
        this.end = parser.current.copy();
    }

    get kind() {
        return AST_NODE[this.type];
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

type This<T extends new(...args: any) => any> = {
	new(...args: ConstructorParameters<T>): any
} & Pick<T, keyof T>;

class Identifier extends AST_Node { type = AST_NODE.IDENTIFIER; }
class Num extends AST_Node { type = AST_NODE.NUMBER; }
class Str extends AST_Node { type = AST_NODE.STRING; }

class Sym extends AST_Node {
    type = AST_NODE.SYMBOL;
    static literal = "?";

    static match<T extends This<typeof Sym>>(this: T, obj: any): obj is InstanceType<T> {
        if (!(obj instanceof Sym)) return false;
        return obj.text == this.literal;
    }
}

class Sym_Add extends Sym { type = AST_NODE.SYM_ADD;  static literal = "+"; }
class Sym_Sub extends Sym { type = AST_NODE.SYM_SUB; static literal = "-"; }
class Sym_Mul extends Sym { type = AST_NODE.SYM_MUL; static literal = "*"; }
class Sym_Div extends Sym { type = AST_NODE.SYM_DIV; static literal = "/"; }
class Sym_Mod extends Sym { type = AST_NODE.SYM_MOD; static literal = "%"; }
class Sym_Exp extends Sym { type = AST_NODE.SYM_EXP; static literal = "^"; }
class Sym_Len extends Sym { type = AST_NODE.SYM_LEN; static literal = "#"; }
class Sym_Less extends Sym { type = AST_NODE.SYM_LESS; static literal = "<"; }
class Sym_More extends Sym { type = AST_NODE.SYM_MORE; static literal = ">"; }
class Sym_Assign extends Sym { type = AST_NODE.SYM_ASSIGN; static literal = "="; }
class Sym_Equals extends Sym { type = AST_NODE.SYM_EQUALS; static literal = "=="; }
class Sym_Not_Equals extends Sym { type = AST_NODE.SYM_NOT_EQUALS; static literal = "~="; }
class Sym_Less_Equals extends Sym { type = AST_NODE.SYM_LESS_EQUALS; static literal = "<="; }
class Sym_More_Equals extends Sym { type = AST_NODE.SYM_MORE_EQUALS; static literal = ">="; }
class Sym_Par_Open extends Sym { type = AST_NODE.SYM_PAR_OPEN; static literal = "("; }
class Sym_Par_Close extends Sym { type = AST_NODE.SYM_PAR_CLOSE; static literal = ")"; }
class Sym_Brace_Open extends Sym { type = AST_NODE.SYM_BRACE_OPEN; static literal = "{"; }
class Sym_Brace_Close extends Sym { type = AST_NODE.SYM_BRACE_CLOSE; static literal = "}"; }
class Sym_Bracket_Open extends Sym { type = AST_NODE.SYM_BRACKET_OPEN; static literal = "["; }
class Sym_Bracket_Close extends Sym { type = AST_NODE.SYM_BRACKET_CLOSE; static literal = "]"; }
class Sym_Concat extends Sym { type = AST_NODE.SYM_CONCAT; static literal = ".."; }
class Sym_Dot extends Sym { type = AST_NODE.SYM_DOT; static literal = "."; }
class Sym_Comma extends Sym { type = AST_NODE.SYM_COMMA; static literal = ","; }
class Sym_Colon extends Sym { type = AST_NODE.SYM_COLON; static literal = ":"; }
class Sym_Semicolon extends Sym { type = AST_NODE.SYM_SEMICOLON; static literal = ";"; }
class Sym_DotDotDot extends Sym { type = AST_NODE.SYM_DOTDOTDOT; static literal = "..."; }

class Unary_Operator extends Sym { type = AST_NODE.UNARY_OPERATOR; }
class Binary_Operator extends Sym { type = AST_NODE.BINARY_OPERATOR; }

class Keyword extends AST_Node {
    type = AST_NODE.KEYWORD;

    static text<T extends This<typeof Keyword>>(this: T): string {
        return this.constructor.name.split("_")[1].toLowerCase();
    }

    static match<T extends This<typeof Keyword>>(this: T, obj: any): obj is InstanceType<T> {
        if (!(obj instanceof Keyword)) return false;
        return obj.text == this.text();
    }
}

class Keyword_And extends Keyword { type = AST_NODE.KWD_AND; }
class Keyword_Break extends Keyword { type = AST_NODE.KWD_BREAK; }
class Keyword_Do extends Keyword { type = AST_NODE.KWD_DO; }
class Keyword_Else extends Keyword { type = AST_NODE.KWD_ELSE; }
class Keyword_ElseIf extends Keyword { type = AST_NODE.KWD_ELSEIF; }
class Keyword_End extends Keyword { type = AST_NODE.KWD_END; }
class Keyword_False extends Keyword { type = AST_NODE.KWD_FALSE; }
class Keyword_For extends Keyword { type = AST_NODE.KWD_FOR; }
class Keyword_Function extends Keyword { type = AST_NODE.KWD_FUNCTION; }
class Keyword_If extends Keyword { type = AST_NODE.KWD_IF; }
class Keyword_In extends Keyword { type = AST_NODE.KWD_IN; }
class Keyword_Local extends Keyword { type = AST_NODE.KWD_LOCAL; }
class Keyword_Nil extends Keyword { type = AST_NODE.KWD_NIL; }
class Keyword_Not extends Keyword { type = AST_NODE.KWD_NOT; }
class Keyword_Or extends Keyword { type = AST_NODE.KWD_OR; }
class Keyword_Repeat extends Keyword { type = AST_NODE.KWD_REPEAT; }
class Keyword_Return extends Keyword { type = AST_NODE.KWD_RETURN; }
class Keyword_Then extends Keyword { type = AST_NODE.KWD_THEN; }
class Keyword_True extends Keyword { type = AST_NODE.KWD_TRUE; }
class Keyword_Until extends Keyword { type = AST_NODE.KWD_UNTIL; }
class Keyword_While extends Keyword { type = AST_NODE.KWD_WHILE; }

abstract class AST_Wrapper<T extends AST_Node> extends AST_Node {
    value: T;
    constructor(parser: Parser, value: T) {
        super(parser);
        this.value = value;
        this.withPos(value);
    }
}

class Parens<T extends AST_Node> extends AST_Node {
    type = AST_NODE.PARENS;
    value: T;

    constructor(parser: Parser, open: Sym_Par_Open, value: T, close: Sym_Par_Close) {
        super(parser);
        this.value = value;
        this.withPos(open, value, close);
    }
}

/**
nil | false | true | Number |
String | '...' | function |
tableconstructor | functioncall |
var | '(' exp ')'
*/
class Value extends AST_Wrapper<
    Keyword_Nil | Keyword_False | Keyword_True |
    Num | Str |
    Sym | Func | Table |
    Func_Call | Var | Expr
> {
    type = AST_NODE.VALUE;
}

/** prefix {suffix} index */
class Index_Access extends AST_Node {
    type = AST_NODE.INDEX_ACCESS;
    prefix: Prefix;
    suffixes: Suffix[];
    index: Index;
    constructor(parser: Parser, prefix: Prefix, suffixes: Suffix[], index: Index) {
        super(parser);
        this.prefix = prefix;
        this.suffixes = suffixes;
        this.index = index;
        this.withPos(this.prefix, this.index, ...this.suffixes);
    }
}

/** 'function' functionbody */
class Func extends AST_Wrapper<Func_Body> { type = AST_NODE.FUNC; }

/** {stat [`;´]} [laststat [`;´]] */
class Chunk extends AST_Node {
    type = AST_NODE.CHUNK;
    statements: Statement[];
    last_statement?: Last_Statement;

    constructor(parser: Parser, statements: Statement[], last_statement?: Last_Statement) {
        super(parser);
        this.statements = statements;
        this.last_statement = last_statement;
        this.withPos(...statements, last_statement);
    }
}

class Block extends AST_Wrapper<Chunk> { type = AST_NODE.CHUNK; }

/** `(´ [parlist] `)´ block end */
class Func_Body extends AST_Node {
    type = AST_NODE.FUNC_BODY;
    params: Param_List;
    body: Block;
    constructor(parser: Parser, params: Param_List, body: Block) {
        super(parser);
        this.params = params;
        this.body = body;
        this.withPos(this.params, this.body);
    }
}

class Var extends AST_Wrapper<Index_Access|Identifier> { type = AST_NODE.VAR; }

class Binary_Operation extends AST_Node {
    type = AST_NODE.BINARY_OPERATION;
    left: Value;
    op: Binary_Operator;
    right: Expr;

    constructor(parser: Parser, left: Value, op: Binary_Operator, right: Expr) {
        super(parser);
        this.left = left;
        this.op = op;
        this.right = right;
        this.withPos(this.left, this.right);
    }
}

class Unary_Operation extends AST_Node {
    type = AST_NODE.UNARY_OPERATION;
    op: Unary_Operator;
    exp: Expr;

    constructor(parser: Parser, op: Unary_Operator, exp: Expr) {
        super(parser);
        this.op = op;
        this.exp = exp;
        this.withPos(this.op, this.exp);
    }
}

/** '(' exp ')' | IDENTIFIER */
class Prefix extends AST_Wrapper<Expr|Identifier> { type = AST_NODE.PREFIX; }
class Suffix extends AST_Wrapper<Call|Index> { type = AST_NODE.SUFFIX; }

/** '[' exp ']' | '.' IDENTIFIER */
class Index extends AST_Wrapper<Expr|Identifier> { type = AST_NODE.INDEX; }

/** args | ':' IDENTIFIER args */
class Call extends AST_Node {
    type = AST_NODE.CALL;
    args: Args;
    identifier?: Identifier;
    constructor(parser: Parser, args: Args, identifier?: Identifier) {
        super(parser);
        this.args = args;
        this.identifier = identifier;
        if (this.identifier) this.withPos(this.identifier, this.args);
        else this.withPos(this.args);
    }
}

/** `(´ [explist] `)´ | tableconstructor | String */
class Args extends AST_Wrapper<ExprList|Table|Str> { type = AST_NODE.ARGS; }
class Expr extends AST_Wrapper<Binary_Operation|Unary_Operation|Value> { type = AST_NODE.EXPR; }

/** `{´ [fieldlist] `}´ */
class Table extends AST_Wrapper<FieldList> { type = AST_NODE.TABLE; }

/** `[´ exp `]´ `=´ exp | IDENTIFIER `=´ exp | exp */
class Field extends AST_Node {
    type = AST_NODE.FIELD;
    name?: Expr | Identifier;
    value: Expr;
    constructor(parser: Parser, value: Expr, name?: Expr|Identifier) {
        super(parser);
        this.value = value;
        this.name = name;
        this.withPos(this.value, this.name);
    }
}

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
