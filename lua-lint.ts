import { default as assert } from 'assert';
import { readFileSync } from 'fs';
import { LETTERS, DIGITS, IDENT_CHARS, SIMPLE_SYMBOLS } from './char';
import { AST, Literal, Alternating_Array, No_Trailing_Array } from './ast_util';

let TRACE_EXECUTION = true;


if (process.argv.some(a => a === "--trace")) {
    TRACE_EXECUTION = true;
}


class Identifier extends AST.Node {
    static type = AST.NODE.IDENTIFIER;
    value = this.text;
}

class Num extends AST.Node {
    static type = AST.NODE.NUMBER;
    value = parseFloat(this.text);
}

class Str extends AST.Node {
    static type = AST.NODE.STRING;
    value = this.text;
}

class Unary_Operator extends AST.Node {
    static type = AST.NODE.UNARY_OPERATOR;
    private _ = undefined;
}

class Binary_Operator extends AST.Node {
    static type = AST.NODE.BINARY_OPERATOR;
    private _ = undefined;
}

class Parens<T extends AST.Node> extends AST.Wrapped<AST.SYMBOL<"(">, T, AST.SYMBOL<")">> {
    static type = AST.NODE.PARENS;
}

class Braces<T extends AST.Node> extends AST.Wrapped<AST.SYMBOL<"{">, T, AST.SYMBOL<"}">> {
    static type = AST.NODE.BRACES;
}

class Brackets<T extends AST.Node> extends AST.Wrapped<AST.SYMBOL<"[">, T, AST.SYMBOL<"]">> {
    static type = AST.NODE.BRACKETS;
}

/**
nil | false | true | Number |
String | '...' | function |
tableconstructor | functioncall |
var | '(' exp ')'
*/
type Value = AST.KEYWORD<"nil"> | AST.KEYWORD<"false"> | AST.KEYWORD<"true"> |
    Num | Str | AST.SYMBOL<"..."> | Func | Table |
    Func_Call | Var | Parens<Expr>;

function is_value(o: any): o is Value {
    return AST.is_any(o,
        AST.Keywords["nil"],
        AST.Keywords["false"],
        AST.Keywords["true"],
        AST.Symbols["..."],
        Func, Table, Var,
        Func_Call, Str, Num
    ) || (
        AST.is(o, Parens) && is_expr(o.fields.value)
    );
}


class Suffixes extends AST.List<Suffix> {
    static type = AST.NODE.SUFFIXES;
    static allow_empty = true;
}

class Func_Call extends AST.Sequence<{
    prefix: Prefix,
    suffixes: Suffixes,
    call: Call
}> {
    static type = AST.NODE.FUNC_CALL;
}

class Func_Name_No_Method extends AST.Delimited<Identifier, AST.SYMBOL<".">> {
    static type = AST.NODE.FUNC_NAME_NO_METHOD;
}

class Method_Name extends AST.Sequence<{
    colon: AST.SYMBOL<":">;
    name: Identifier;
}> {
    static type = AST.NODE.METHOD_NAME;
}

class Func_Name extends AST.Sequence<{
    name: Func_Name_No_Method,
    method?: Method_Name
}> {
    static type = AST.NODE.FUNC_NAME;
}

/** prefix {suffix} index */
class Index_Access extends AST.Node {
    static type = AST.NODE.INDEX_ACCESS;
    constructor(
        parser: Parser,
        public prefix: Prefix,
        public suffixes: Suffixes,
        public index: Index
    ) {
        super(parser, prefix.start, index.end);
    }
}

/** 'function' functionbody */
class Func extends AST.Sequence<{
    keyword: AST.KEYWORD<"function">,
    body: Func_Body
}> {
    static type = AST.NODE.FUNC;

}

/** {stat [`;´]} [laststat [`;´]] */
class Block extends AST.Sequence<{
    statements: Statement_List,
    last_statement?: Last_Statement
}> {
    static type = AST.NODE.BLOCK;
}

class Identifier_List extends AST.Delimited<Identifier, AST.SYMBOL<",">> {
    static type = AST.NODE.IDENTIFIER_LIST;
}

class Var_List extends AST.Delimited<Var, AST.SYMBOL<",">> {
    static type = AST.NODE.VAR_LIST;
}

class Expr_List extends AST.Delimited<Expr, AST.SYMBOL<",">> {
    static type = AST.NODE.EXPR_LIST;
    static allow_empty = true;
}

class Var_Args extends AST.Prefixed<AST.SYMBOL<",">, AST.SYMBOL<"...">> {
    static type = AST.NODE.VAR_ARGS;
}

class Param_List extends AST.Sequence<{
    params: Identifier_List,
    varargs?: Var_Args
}> {
    static type = AST.NODE.PARAMS;
}

type Params = AST.SYMBOL<"..."> | Param_List;

/** `(´ [parlist] `)´ block end */
class Func_Body extends AST.Sequence<{
    params: Parens<Params>,
    body: Block,
    end: AST.KEYWORD<"end">
}> {
    static type = AST.NODE.FUNC_BODY;
}

class Var extends AST.Wrapper<Index_Access|Identifier> { static type = AST.NODE.VAR; }

class Binary_Operation extends AST.Node {
    static type = AST.NODE.BINARY_OPERATION;

    constructor(
        parser: Parser,
        public left: Value,
        public op: Binary_Operator,
        public right: Expr
    ) {
        super(parser, left.start, right.end);
    }
}

class Unary_Operation extends AST.Node {
    static type = AST.NODE.UNARY_OPERATION;

    constructor(
        parser: Parser,
        public op: Unary_Operator,
        public exp: Expr
    ) {
        super(parser, op.start, exp.end);
    }
}

/** '(' exp ')' | IDENTIFIER */
type Prefix = Parens<Expr> | Identifier;
type Suffix = Call|Index;

class Dot_Access extends AST.Prefixed<AST.SYMBOL<".">, Identifier> {
    static type = AST.NODE.DOT_ACCESS;
}

/** '[' exp ']' | '.' IDENTIFIER */
type Index = Brackets<Expr> | Dot_Access;

/** ':' IDENTIFIER args */
class Method_Call extends AST.Node {
    static type = AST.NODE.METHOD_CALL;
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
function is_call(o: any): o is Call {
    return AST.is_any(o, Table, Str, Method_Call) ||
        (AST.is(o, Parens) && AST.is(o.fields.value, Expr_List));
}

/** `(´ [explist] `)´ | tableconstructor | String */
type Args = Parens<Expr_List>|Table|Str;
type Expr = Binary_Operation|Unary_Operation|Value;
function is_expr(o: any): o is Expr {
    return AST.is_any(o, Binary_Operation, Unary_Operation) || is_value(o);
}

/** `{´ [fieldlist] `}´ */
class Table extends Braces<Field_List> { static type = AST.NODE.TABLE; }

class Named_Field extends AST.Node {
    static type = AST.NODE.NAMED_FIELD;

    constructor(
        parser: Parser,
        public name: Brackets<Expr> | Identifier,
        public equals: AST.SYMBOL<"=">,
        public expression: Expr
    ) {
        super(parser);
        this.withPos(name, equals, expression);
    }
}

type Field = Named_Field|Expr;
type Field_Sep = AST.SYMBOL<","> | AST.SYMBOL<";">;

class Field_List extends AST.Delimited_Trailing<Field, Field_Sep> {
    static type = AST.NODE.FIELD_LIST;
    static allow_empty = true;
}

class Return_Statement extends AST.Prefixed<AST.KEYWORD<"return">, Expr_List> {
    static type = AST.NODE.RETURN;
}

class Last_Statement extends AST.Sequence<{
    statement: Return_Statement | AST.KEYWORD<"break">
    semicolon?: AST.SYMBOL<";">
}> {
    static type = AST.NODE.LAST_STATEMENT;
}

class Assign extends AST.Sequence<{
    vars: Var_List,
    equals: AST.SYMBOL<"=">,
    exprs: Expr_List
}> {
    static type = AST.NODE.ASSIGN;
}

class Do_Block extends AST.Sequence<{
    do: AST.KEYWORD<"do">,
    block: Block,
    end: AST.KEYWORD<"end">
}> {
    static type = AST.NODE.DO_BLOCK;
}

class While extends AST.Sequence<{
    while_keyword: AST.KEYWORD<"while">,
    condition: Expr,
    do_keyword: AST.KEYWORD<"do">,
    block: Block,
    end_keyword: AST.KEYWORD<"end">
}> {
    static type = AST.NODE.WHILE;
}

class Repeat extends AST.Sequence<{
    repeat_keyword: AST.KEYWORD<"repeat">,
    block: Block,
    until_keyword: AST.KEYWORD<"until">,
    condition: Expr
}> {
    static type = AST.NODE.REPEAT;
}

class If extends AST.Sequence<{
    if_keyword: AST.KEYWORD<"if">,
    condition: Expr,
    then_keyword: AST.KEYWORD<"then">,
    block: Block,
    elseifs?: ElseIf_List,
    Else?: Else
    end_keyword: AST.KEYWORD<"end">,

}> {
    static type = AST.NODE.IF;
}

class Else extends AST.Sequence<{
    else_keyword: AST.KEYWORD<"else">,
    block: Block
}> {
    static type = AST.NODE.ELSE;
}

class ElseIf extends AST.Sequence<{
    elseif_keyword: AST.KEYWORD<"elseif">,
    condition: Expr,
    then_keyword: AST.KEYWORD<"then">,
    block: Block
}> {
    static type = AST.NODE.ELSEIF;
}

class ElseIf_List extends AST.List<ElseIf> {
    static type = AST.NODE.ELSEIF_LIST;
}

class Func_Decl extends AST.Sequence<{
    func_keyword: AST.KEYWORD<"function">,
    name: Func_Name,
    body: Func_Body
}> {
    static type = AST.NODE.FUNC_DECL;
}

class Local_Func_Decl extends AST.Sequence<{
    local_keyword: AST.KEYWORD<"local">,
    func_keyword: AST.KEYWORD<"function">,
    name: Identifier,
    body: Func_Body
}> {
    static type = AST.NODE.LOCAL_FUNC_DECL;
}

class Assign_Exprs extends AST.Sequence<{
    "assign": AST.SYMBOL<"=">,
    "exprs": Expr_List
}> {
    static type = AST.NODE.ASSIGN_EXPRS;
}

class Local_Var_Decl extends AST.Sequence<{
    local_keyword: AST.KEYWORD<"local">,
    names: Identifier_List,
    exprs?: Assign_Exprs
}> {
    static type = AST.NODE.LOCAL_VAR_DECL;
}

class For extends AST.Sequence<{
    for_keyword: AST.KEYWORD<"for">,
    name: Identifier,
    assign: AST.SYMBOL<"=">,
    init: Expr,
    comma: AST.SYMBOL<",">,
    end: Expr,
    step?: For_Step,
    block: Do_Block
}> {
    static type = AST.NODE.FOR;
}

class For_Step extends AST.Sequence<{
    comma: AST.SYMBOL<",">,
    step: Expr
}> {
    static type = AST.NODE.FOR_STEP;
}

class For_In extends AST.Sequence<{
    for_keyword: AST.KEYWORD<"for">,
    names: Identifier_List,
    in_keyword: AST.KEYWORD<"in">,
    exprs: Expr_List,
    block: Do_Block
}> {
    static type = AST.NODE.FOR_IN;
}

class Statement_List extends AST.List<Statement> {
    static type = AST.NODE.STATEMENT_LIST;
}

type Statement = Assign | Func_Call | Do_Block | While | Repeat | If | For_In | For | Local_Var_Decl | Local_Func_Decl | Func_Decl;

type DropFirst<T extends unknown[]> = T extends [any, ...infer U] ? U : never;
type ReturnTypes<T> = {
    [K in keyof T]: T[K] extends ()=>any ? ReturnType<T[K]> : never
}
type RequiredTypes<T> = {
    [K in keyof T]: NonNullable<T[K]>
}

type Column<T extends any[], COL extends number> = {
    [K in keyof T]: T[K][COL]
}


function memoized<A, B>(fn: (a:A)=>B) {
    const cache = new Map<A, B>();

    return (a: A) => {
        if (cache.has(a)) return cache.get(a) as B;
        let b = fn(a);
        cache.set(a, b);
        return b;
    };
}

function replaceAt(str: string, index: number, replacement: string) {
    return str.substring(0, index) + replacement + str.substring(index + replacement.length);
}

// @TODO: Parsing errors
// Every function that returns a node should optionally
// also return an error.
// Then other functions can handle the error or
// propagate it upwards, perhaps with additional
// information.

class Parser {
    start = new AST.Position(0, 0);
    current = new AST.Position(0, 0);

    constructor(public text: string) {}

    char(add=0) { return this.text[this.current.index+add]; }
    move(amt=1) { this.current.move(amt); }
    rollback(token?: AST.Node) { this.current.set((token||this).start); }
    at_end() { return this.current.index >= this.text.length; }

    current_text() {
        return this.text.substring(this.start.index, this.current.index);
    }

    context() {
        let lines = this.text.split("\n");
        let firstchar = lines.slice(0, this.start.line).join("\n").length+1;
        let realchar = this.start.index;
        let offset = realchar - firstchar;
        return replaceAt(lines[this.start.line], offset, "^");
    }

    ast_node<
        T extends AST.Node,
        FN extends new (...args: any) => T,
    >(
        cls: FN,
        ...args: DropFirst<ConstructorParameters<FN>>
    ): InstanceType<typeof cls> {
        let t = new cls(this, ...args);
        this.start = this.current.copy();
        return t as any;
    }

    trace(msg: string) {
        let stack = (new Error()).stack;
        if (!stack) return;
        console.log(
            msg + ":\n" +
            "context: \n" + this.context() + "\n" +
            stack
                .split("\n")
                .filter(s => s.includes("at Parser"))
                .filter(s =>
                    !s.includes("either") &&
                    !s.includes("sequence") &&
                    !s.includes("no_trailing") &&
                    !s.includes("alternating")
                )
                .slice(1)
                .reverse()
                .map((s, i) => "| ".repeat(i) + s.trim().replaceAll("Parser.", "").split(" ")[1])
                .join("\n")
            + "\n"
        );
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
                        if (!this.long_bracket(AST.Node)) {
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

    ident(allow_keyword: boolean) {
        this.skip_ws();
        if (this.at_end()) return undefined;
        let c = this.char();
        if (LETTERS.includes(c) || c == "_") {
            this.move();
            while (IDENT_CHARS.includes(this.char())) this.move();
            let node = this.ast_node(Identifier);
            if (AST.Keywords[node.text as keyof AST.KEYWORDS] && !allow_keyword) {
                this.rollback(node);
                return undefined;
            }
            return node;
        } else {
            return undefined;
        }
    }

    ident_no_keyword() { return this.ident(false); }

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

    symbol(): InstanceType<AST.SYMBOLS[keyof AST.SYMBOLS]> | undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;

        let found = false;
        switch (this.char()) {
            case "=":
            case "~":
            case "<":
            case ">":
                if (this.char(1) == "=") {
                    this.move(2);
                    found = true;
                } else if (this.char() != "~") {
                    this.move(1);
                    found = true;
                }
                break;
            case ".":
                if (this.char(1) == ".") {
                    if (this.char(2) == ".") {
                        this.move(3);
                        found = true;
                        break;
                    }
                    this.move(2);
                    found = true;
                    break;
                }
        }

        if (!found && SIMPLE_SYMBOLS.includes(this.char())) {
            this.move();
            found = true;
        };

        if (!found) return undefined;
        return new AST.Symbols[this.current_text() as keyof AST.SYMBOLS](this);
    }

    long_bracket<T extends AST.Node>(token_type: new (parser: Parser) => T) {
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
        FN extends (() => (AST.Node | undefined))[]
    >(
        ...fns: FN
    ): RequiredTypes<ReturnTypes<typeof fns>>|undefined {
        let out = [];
        for (let fn of fns) {
            let res = fn.call(this);
            if (!res) {
                // this.trace("Sequence failed while trying to parse " + fn.name);
                if (out.length > 0) this.rollback(out[0]);
                return undefined;
            }
            out.push(res);
        }
        return out as any;
    }

    parens<
        FN extends (() => (AST.Node | undefined))
    >(fn: FN): Parens<Exclude<ReturnType<FN>, undefined>> | undefined {
        let seq = this.sequence(
            this.symbol_matcher("("),
            fn,
            this.symbol_matcher(")")
        );
        if (!seq) { return undefined; }
        return new Parens(this, {
            open: seq[0],
            value: seq[1] as Exclude<ReturnType<FN>, undefined>,
            close: seq[2]
        });
    }

    braces<
        FN extends (() => (AST.Node | undefined))
    >(fn: FN): Braces<Exclude<ReturnType<FN>, undefined>> | undefined {
        let seq = this.sequence(
            this.symbol_matcher("{"),
            fn,
            this.symbol_matcher("}")
        );
        if (!seq) { return undefined; }
        return new Braces(this, {
            open: seq[0],
            value: seq[1] as Exclude<ReturnType<FN>, undefined>,
            close: seq[2]
        });
    }

    brackets<
        FN extends (() => (AST.Node | undefined))
    >(fn: FN): Brackets<Exclude<ReturnType<FN>, undefined>> | undefined {
        let seq = this.sequence(
            this.symbol_matcher("["),
            fn,
            this.symbol_matcher("]")
        );
        if (!seq) { return undefined; }
        return new Brackets(this, {
            open: seq[0],
            value: seq[1] as Exclude<ReturnType<FN>, undefined>,
            close: seq[2]
        });
    }

    exact<K extends AST.Node>(fn: ()=>K|undefined, text: string): K|undefined {
        this.skip_ws();
        let tok = fn.call(this);
        if (!tok) return undefined;
        if (tok.text !== text) {
            this.rollback(tok);
            return undefined;
        }
        return tok;
    }

    exact_symbol<K extends keyof AST.SYMBOLS>(str: Literal<K, string>): AST.SYMBOL<K>|undefined {
        return this.exact(this.symbol, str) as AST.SYMBOL<K>|undefined;
    }

    exact_keyword<K extends keyof AST.KEYWORDS>(str: Literal<K, string>): AST.KEYWORD<K>|undefined {
        return this.exact(this.keyword, str) as AST.KEYWORD<K>|undefined;
    }

    symbol_matcher = memoized(<K extends keyof AST.SYMBOLS>(sym: Literal<K, string>) => {
        let fn = () => this.exact_symbol(sym);
        Object.defineProperty(fn, 'name', {
            value: "symbol_" + sym,
            configurable: true
        });
        return fn;
    });

    keyword_matcher = memoized(<K extends keyof AST.KEYWORDS>(kw: Literal<K, string>) => {
        let fn = () => this.exact_keyword(kw);
        Object.defineProperty(fn, 'name', {
            value: "keyword_" + kw,
            configurable: true
        });
        return fn;
    });


    either<
        FN extends (() => (AST.Node | undefined))[]
    >(
        ...fns: FN
    ): ReturnTypes<FN>[number] | undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;
        for (let fn of fns) {
            let tok = fn.call(this);
            if (tok) return tok as any;
        }
        // this.trace("Failed to parse any of " + fns.map(f => f.name).join(", "))
        return undefined;
    }

    preferential_either<
        FN extends [() => (AST.Node | undefined), ()=>number][]
    >(
        bail_on_all_zeroes: boolean,
        ...fns: FN
    ) {
        this.skip_ws();
        let nonzero = false;

        let sorted = fns.sort(
            (a, b) => {
                let ar = a[1]();
                let br = b[1]();
                nonzero = nonzero || ar != 0 || br != 0;
                return br - ar;
            }
        );

        if (bail_on_all_zeroes && !nonzero) { return undefined; }

        let mapped = sorted.map(a => a[0]) as unknown as Column<FN, 0>;
        return this.either(
            ...mapped
        );
    }

    alternating<
        FNA extends (() => (AST.Node | undefined)),
        FNB extends (() => (AST.Node | undefined))
    >(
        fn_a: FNA,
        fn_b: FNB,
        allow_empty: boolean
    ) {
        this.skip_ws();
        let first = fn_a.call(this);
        if (!first) {
            if (allow_empty) {
                return [] as unknown as Alternating_Array<
                    NonNullable<ReturnType<FNA>>,
                    NonNullable<ReturnType<FNB>>
                >;
            }
            return undefined;
        }        let args = [first];
        while (true) {
            let b = fn_b.call(this);
            if (!b) break;
            args.push(b);
            let a = fn_a.call(this);
            if (!a) break;
            args.push(a);
        }

        return args as unknown as Alternating_Array<
            NonNullable<ReturnType<FNA>>,
            NonNullable<ReturnType<FNB>>
        >;
    }

    no_trailing<
        FNA extends (() => (AST.Node | undefined)),
        FNB extends (() => (AST.Node | undefined))
    >(
        fn_a: FNA,
        fn_b: FNB,
        allow_empty: boolean
    ) {
        this.skip_ws();
        let first = fn_a.call(this);
        if (!first) {
            if (allow_empty) {
                return [] as unknown as No_Trailing_Array<
                    NonNullable<ReturnType<FNA>>,
                    NonNullable<ReturnType<FNB>>
                >;
            }
            return undefined;
        }
        let args = [first];
        while (true) {
            let b = fn_b.call(this);
            if (!b) break;
            let a = fn_a.call(this);
            if (!a) {
                this.rollback(first);
                return undefined
            };
            args.push(b, a);
        }

        return args as unknown as No_Trailing_Array<
            NonNullable<ReturnType<FNA>>,
            NonNullable<ReturnType<FNB>>
        >;
    }

    other() {
        this.skip_ws();
        if (this.at_end()) return undefined;
        this.move();
        return this.ast_node(AST.Node);
    }

    name() {
        this.skip_ws();
        if (this.at_end()) return undefined;
        let ident = this.ident(false);
        if (!ident) { return undefined; }
        if (ident.text in AST.Keywords) { this.rollback(ident); return undefined; }
        return ident;
    }

    unop(): Unary_Operator|undefined {
        let op = this.either(
            this.symbol_matcher("-"),
            this.keyword_matcher("not"),
            this.symbol_matcher("#"),
        );
        if (!op) return undefined;
        return new Unary_Operator(this, op.start, op.end);
    }

    text_start_preference(str: string, weight=1) {
        return () => {
            return this.text.startsWith(str, this.current.index) ? weight : 0;
        }
    }

    number_start_preference(weight=1) {
        return () => {
            return this.char().match(/[0-9]/) ? weight : 0
        }
    }

    string_start_preference(weight=1) {
        return () => {
            let char = this.char()
            return (char === '"' || char === "'") ? weight : 0;
        }
    }

    binop(): Binary_Operator|undefined {
        let op = this.preferential_either(
            true,
            [this.symbol_matcher("+"), this.text_start_preference("+")],
            [this.symbol_matcher("-"), this.text_start_preference("-")],
            [this.symbol_matcher("*"), this.text_start_preference("*")],
            [this.symbol_matcher("/"), this.text_start_preference("/")],
            [this.symbol_matcher("^"), this.text_start_preference("^")],
            [this.symbol_matcher("%"), this.text_start_preference("%")],
            [this.symbol_matcher(".."), this.text_start_preference("..")],
            [this.symbol_matcher("<"), this.text_start_preference("<")],
            [this.symbol_matcher(">"), this.text_start_preference(">")],
            [this.symbol_matcher("<="), this.text_start_preference("<=", 2)],
            [this.symbol_matcher(">="), this.text_start_preference(">=", 2)],
            [this.symbol_matcher("=="), this.text_start_preference("==", 2)],
            [this.symbol_matcher("~="), this.text_start_preference("~=")],
            [this.keyword_matcher("and"), this.text_start_preference("and")],
            [this.keyword_matcher("or"), this.text_start_preference("or")],
        );
        if (!op) return undefined;
        return new Binary_Operator(this, op.start, op.end);
    }

    prefix(): Prefix|undefined {
        return this.either(
            () => this.parens(this.expr),
            this.ident_no_keyword
        );
    }

    suffix(): Suffix|undefined {
        return this.either(
            this.call,
            this.index
        );
    }

    suffixes(): Suffixes {
        let suffixes = this.list(this.suffix);
        return new Suffixes(this, suffixes);
    }

    expr_list(allow_empty: boolean): Expr_List|undefined {
        let exprs = this.no_trailing(this.expr, this.symbol_matcher(","), allow_empty);
        if (!exprs) return undefined;
        return new Expr_List(this, exprs);
    }

    table(): Table|undefined {
        let b = this.braces(this.field_list);
        if (!b) return undefined;
        return new Table(this, b.fields);
    }

    field(): Field|undefined {
        return this.either(
            this.named_field,
            this.expr
        );
    }

    named_field(): Named_Field|undefined {
        let seq = this.sequence(
            () => this.either(
                this.ident_no_keyword,
                () => this.brackets(this.expr)
            ),
            this.symbol_matcher("="),
            this.expr
        );
        if (!seq) return undefined;
        return new Named_Field(this, ...seq);
    }

    field_sep(): Field_Sep|undefined {
        return this.preferential_either(
            true,
            [this.symbol_matcher(","), this.text_start_preference(",")],
            [this.symbol_matcher(";"), this.text_start_preference(";")]
        );
    }

    field_list(): Field_List|undefined {
        let fields = this.alternating(this.field, this.field_sep, true)
        if (!fields) return undefined;
        return new Field_List(this, fields);
    }

    args(): Args|undefined {
        return this.preferential_either(
            false,
            [this.str, this.string_start_preference()],
            [this.table, this.text_start_preference("{")],
            [() => this.parens(() => this.expr_list(true)), this.text_start_preference("(")]
        );
    }

    method_name(): Method_Name|undefined {
        let seq = this.sequence(
            this.symbol_matcher(":"),
            this.ident_no_keyword
        );
        if (!seq) return undefined;
        return new Method_Name(this, {
            colon: seq[0],
            name: seq[1]
        });
    }

    method_call(): Method_Call|undefined {
        let seq = this.sequence(
            this.method_name,
            this.args
        );
        if (!seq) return undefined;
        return new Method_Call(this, ...seq)
    }

    call(): Call|undefined {
        return this.either(
            this.args,
            this.method_call
        )
    }

    func(): Func|undefined {
        let seq = this.sequence(
            this.keyword_matcher("function"),
            this.func_body
        );
        if (!seq) return undefined;
        return new Func(this, {
            keyword: seq[0],
            body: seq[1]
        });
    }

    value(): Value|undefined {
        return this.preferential_either(
            false,
            [this.keyword_matcher("nil"), this.text_start_preference("nil")],
            [this.keyword_matcher("false"), this.text_start_preference("false")],
            [this.keyword_matcher("true"), this.text_start_preference("true")],
            [this.num, this.number_start_preference()],
            [this.str, this.string_start_preference()],
            [this.symbol_matcher("..."), this.text_start_preference("...")],
            [this.func, this.text_start_preference("function")],
            [this.table, this.text_start_preference("{")],
            [this.function_call, () => 0],
            [this.variable, () => 0],
            [() => this.parens(this.expr), this.text_start_preference("(")],
        );
    }

    keyword(): InstanceType<AST.KEYWORDS[keyof AST.KEYWORDS]> | undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;

        let ident = this.ident(true);
        if (!ident) { return undefined; }
        if (!(ident.text in AST.Keywords)) {
            this.rollback(ident);
            return undefined;
        }
        let kwd = new AST.Keywords[ident.text as keyof typeof AST.Keywords](this, ident);
        kwd.withPos(ident);
        return kwd;
    }

    binary_operation(): Binary_Operation|undefined {
        this.skip_ws();
        let seq = this.sequence(
            this.value,
            this.binop,
            this.expr
        );
        if (!seq) return undefined;
        return new Binary_Operation(this, ...seq);
    }

    unary_operation(): Unary_Operation|undefined {
        this.skip_ws();
        let seq = this.sequence(this.unop, this.expr);
        if (!seq) return undefined;
        return new Unary_Operation(this, ...seq);
    }

    expr(): Expr|undefined {
        return this.either(
            this.binary_operation,
            this.unary_operation,
            this.value
        );
    }

    list<K extends AST.Node>(fn: () => K|undefined): K[] {
        let arr = [];
        let tok = fn.call(this);
        while (tok) {
            arr.push(tok);
            tok = fn.call(this);
        }
        return arr;
    }

    dot_access(): Dot_Access|undefined {
        this.skip_ws();
        let seq = this.sequence(
            this.symbol_matcher("."),
            this.ident_no_keyword
        );
        if (!seq) return undefined;
        return new Dot_Access(this, {
            prefix: seq[0],
            value: seq[1]
        });
    }

    index(): Index|undefined {
        return this.either(
            () => this.brackets(this.expr),
            this.dot_access
        )
    }

    index_access(): Index_Access|undefined {
        this.skip_ws();
        let seq = this.sequence(
            this.prefix,
            this.suffixes
        );
        if (!seq) return undefined;
        let suffixes = seq[1];

        let last_suffix = suffixes.list.at(-1);
        if (last_suffix === undefined || is_call(last_suffix)) {
            this.rollback(seq[0]);
            return undefined;
        }

        let index = suffixes.list.pop() as Index;

        if (suffixes.list.length > 0) {
            suffixes.end.set(suffixes.list.at(-1)!.end);
        } else {
            suffixes.end.set(suffixes.start);
        }

        return new Index_Access(this, ...seq, index);
    }

    variable(): Var|undefined {
        let val = this.either(
            this.index_access,
            this.ident_no_keyword
        );
        if (!val) return undefined;
        return new Var(this, val);
    }

    var_list(allow_empty: boolean): Var_List|undefined {
        let args = this.no_trailing(this.variable, this.symbol_matcher(","), allow_empty);
        if (!args) return undefined;
        return new Var_List(this, args)
    }

    assign(): Assign|undefined {
        let seq = this.sequence(
            () => this.var_list(false),
            this.symbol_matcher("="),
            () => this.expr_list(false)
        );
        if (!seq) return undefined;
        return new Assign(this, {
            vars: seq[0],
            equals: seq[1],
            exprs: seq[2]
        });
    }

    function_call(): Func_Call|undefined {
        this.skip_ws();
        let seq = this.sequence(
            this.prefix,
            this.suffixes
        );
        if (!seq) return undefined;
        let suffixes = seq[1];

        let last_suffix = suffixes.list.at(-1);
        if (last_suffix === undefined || !is_call(last_suffix)) {
            this.rollback(seq[0]);
            return undefined;
        }

        let call = suffixes.list.pop() as Call;

        if (suffixes.list.length > 0) {
            suffixes.end.set(suffixes.list.at(-1)!.end);
        } else {
            suffixes.end.set(suffixes.start);
        }

        return new Func_Call(this, {
            prefix: seq[0],
            suffixes: suffixes,
            call: call
        });
    }

    block(): Block {
        this.skip_ws();
        let slist = this.statement_list();
        let last = this.last_statement();

        return new Block(this, {
            statements: slist,
            last_statement: last
        });
    }

    do_block(): Do_Block|undefined {
        this.skip_ws();
        let seq = this.sequence(
            this.keyword_matcher("do"),
            this.block,
            this.keyword_matcher("end")
        );
        if (!seq) return undefined;
        return new Do_Block(this, {
            do: seq[0],
            block: seq[1],
            end: seq[2]
        });
    }

    while_loop(): While|undefined {
        let seq = this.sequence(
            this.keyword_matcher("while"),
            this.expr,
            this.keyword_matcher("do"),
            this.block,
            this.keyword_matcher("end")
        );
        if (!seq) { return undefined; }
        return new While(this, {
            while_keyword: seq[0],
            condition: seq[1],
            do_keyword: seq[2],
            block: seq[3],
            end_keyword: seq[4]
        });
    }

    namelist(): Identifier_List | undefined {
        let l = this.no_trailing(this.ident_no_keyword, this.symbol_matcher(","), false);
        if (!l) return undefined;
        return new Identifier_List(this, l);
    }

    assign_exprs(): Assign_Exprs|undefined {
        let seq = this.sequence(
            this.symbol_matcher("="),
            () => this.expr_list(false)
        );
        if (!seq) return undefined;
        return new Assign_Exprs(this, {
            assign: seq[0],
            exprs: seq[1]
        });
    }

    local_var_decl(): Local_Var_Decl|undefined {
        let seq = this.sequence(
            this.keyword_matcher("local"),
            this.namelist
        );
        if (!seq) return undefined;
        return new Local_Var_Decl(this, {
            local_keyword: seq[0],
            names: seq[1],
            exprs: this.assign_exprs()
        });
    }

    func_name_no_method(): Func_Name_No_Method|undefined {
        let args = this.no_trailing(this.ident_no_keyword, this.symbol_matcher("."), false);
        if (!args) return undefined;
        return new Func_Name_No_Method(this, args);
    }

    func_name(): Func_Name|undefined {
        let fn = this.func_name_no_method();
        if (!fn) return undefined;
        return new Func_Name(this, {
            name: fn,
            method: this.method_name()
        });
    }

    varargs(): Var_Args|undefined {
        let seq = this.sequence(
            this.symbol_matcher(","),
            this.symbol_matcher("...")
        );
        if (!seq) return undefined;
        return new Var_Args(this, {
            prefix: seq[0],
            value: seq[1]
        });
    }

    param_list(): Param_List|undefined {
        let params = this.namelist();
        if (!params) return undefined;

        return new Param_List(this, {
            params: params,
            varargs: this.varargs()
        });
    }

    params(): Params|undefined {
        return this.either(
            this.param_list,
            this.symbol_matcher("...")
        );
    }

    func_body(): Func_Body|undefined {
        let seq = this.sequence(
            () => this.parens(this.params),
            this.block,
            this.keyword_matcher("end")
        );
        if (!seq) return undefined;
        return new Func_Body(this, {
            params: seq[0],
            body: seq[1],
            end: seq[2]
        });
    }

    func_decl(): Func_Decl|undefined {
        let seq = this.sequence(
            this.keyword_matcher("function"),
            this.func_name,
            this.func_body
        );
        if (!seq) return undefined;
        return new Func_Decl(this, {
            func_keyword: seq[0],
            name: seq[1],
            body: seq[2]
        });
    }

    elseif(): ElseIf|undefined {
        let seq = this.sequence(
            this.keyword_matcher("elseif"),
            this.expr,
            this.keyword_matcher("then"),
            this.block
        );
        if (!seq) return undefined;
        return new ElseIf(this, {
            elseif_keyword: seq[0],
            condition: seq[1],
            then_keyword: seq[2],
            block: seq[3]
        });
    }

    elseifs(): ElseIf_List|undefined {
        let else_ifs = this.list(this.elseif);
        if (else_ifs.length == 0) return undefined;
        return new ElseIf_List(this, else_ifs);
    }

    else_statement(): Else|undefined {
        let seq = this.sequence(
            this.keyword_matcher("else"),
            this.block
        );
        if (!seq) return undefined;
        return new Else(this, {
            else_keyword: seq[0],
            block: seq[1]
        });
    }

    if_statement(): If|undefined {
        let seq = this.sequence(
            this.keyword_matcher("if"),
            this.expr,
            this.keyword_matcher("then"),
            this.block
        );
        if (!seq) return undefined;

        let elseifs = this.elseifs();
        let else_stat = this.else_statement();

        let end = this.exact_keyword("end");
        if (!end) {
            this.rollback(seq[0]);
            return undefined;
        }

        return new If(this, {
            if_keyword: seq[0],
            condition: seq[1],
            then_keyword: seq[2],
            block: seq[3],
            elseifs: elseifs,
            Else: else_stat,
            end_keyword: end
        });
    }

    for_step(): For_Step|undefined {
        let seq = this.sequence(
            this.symbol_matcher(","),
            this.expr
        );
        if (!seq) return undefined;
        return new For_Step(this, {
            comma: seq[0],
            step: seq[1]
        });
    }

    for_loop(): For|undefined {
        let seq = this.sequence(
            this.keyword_matcher("for"),
            this.ident_no_keyword,
            this.symbol_matcher("="),
            this.expr,
            this.symbol_matcher(","),
            this.expr
        );

        if (!seq) return undefined;
        let step = this.for_step();

        let block = this.do_block();
        if (!block) {
            this.rollback(seq[0]);
            return undefined;
        }

        return new For(this, {
            for_keyword: seq[0],
            name: seq[1],
            assign: seq[2],
            init: seq[3],
            comma: seq[4],
            end: seq[5],
            step: step,
            block: block
        });
    }

    for_in_loop(): For_In|undefined {
        let seq = this.sequence(
            this.keyword_matcher("for"),
            this.namelist,
            this.keyword_matcher("in"),
            () => this.expr_list(false),
            this.do_block
        );
        if (!seq) return undefined;
        return new For_In(this, {
            for_keyword: seq[0],
            names: seq[1],
            in_keyword: seq[2],
            exprs: seq[3],
            block: seq[4]
        })
    }

    statement(): Statement|undefined {
        return this.preferential_either(
            false,
            [this.assign, () => 0],
            [this.function_call, () => 0],
            [this.do_block, this.text_start_preference("do")],
            [this.while_loop, this.text_start_preference("while")],
            // @TODO: repeat |
            [this.if_statement, this.text_start_preference("if")],
            [this.for_loop, this.text_start_preference("for")],
            [this.for_in_loop, this.text_start_preference("for", 2)],
            [this.func_decl, this.text_start_preference("function")],
            // @TODO: `local` `function` ident funcbody |
            [this.local_var_decl, this.text_start_preference("local")],
        );
    }

    statement_list(): Statement_List {
        return new Statement_List(this, this.list(this.statement));
    }

    return_statement(): Return_Statement | undefined {
        let seq = this.sequence(
            this.keyword_matcher("return"),
            () => this.expr_list(true)
        );
        if (!seq) return undefined;
        return new Return_Statement(this, {
            prefix: seq[0],
            value: seq[1]
        });
    }

    last_statement(): Last_Statement | undefined {
        let stat = this.either(
            this.return_statement,
            this.keyword_matcher("break")
        );
        if (!stat) return undefined;
        let semi = this.exact_symbol(";");
        return new Last_Statement(this, {
            statement: stat,
            semicolon: semi
        });
    }

    parse() {
        let tokens = [];
        while (!this.at_end()) {
            let tok = this.statement();
            if (tok) {
                tokens.push(tok);
                continue;
            }
            console.log("Failed to parse:\n" + this.context());
            break;
        }
        return tokens;
    }
}

const SKIP_FNS = [
    "proto", "char", "move",
    "at_end", "skip_ws", "either",
    "sequence", "context", "exact",
    "keyword", "symbol", "list",
    "rollback", "<anonymous>",
    "keyword_", "symbol_", "current_text",
    "ast_node",  "ident", "text_start_preference",
    "long_bracket", "preferential_either",
    "number_start_preference", "string_start_preference"
];

function includes_skip(str: string): boolean {
    for (let fn of SKIP_FNS) {
        if (str.includes("Parser." + fn + ".")) return true;
        if (str.includes("Parser." + fn + " ")) return true;
        if (str.includes("Parser.keyword_")) return true;
        if (str.includes("Parser.symbol_")) return true;
    }
    return false;
}

function get_depth() {
    let stack = (new Error()).stack;
    if (!stack) return 0;
    let f = stack.split("\n")
        .filter(s =>
            s !== "Error" &&
            !includes_skip(s) &&
            !s.includes("get_depth") &&
            !s.includes("Object.") &&
            !s.includes("Module.") &&
            !s.includes("Function.") &&
            !s.includes("node:internal/") &&
            true
        );
    // console.log(f);
    return f.length;
}

const TRACE_START = 0;
const TRACE_END = 999;

if (TRACE_EXECUTION) {
    Error.stackTraceLimit = Infinity;
    let proto = Parser.prototype as any;
    let keys = Object.getOwnPropertyNames(proto);
    let moved = false;
    for (let fn_name of keys) {
        let fn = proto[fn_name] as (...args: any[]) => any;
        if (fn_name === "move" || fn_name === "rollback") {
            proto[fn_name] = function(...args: any[]) {
                moved = true;
                return fn.call(this, ...args);
            }
        }
        if (fn_name === "Parser") continue;
        if (SKIP_FNS.includes(fn_name)) continue;
        proto[fn_name] = function(...args: any[]) {
            if (this.current.line < TRACE_START) {
                return fn.call(this, ...args);
            }

            if (this.current.line > TRACE_END) {
                throw new Error("OOPS");
            }

            let first_string_arg = args.length === 1 ? args[0] : "";
            if (typeof first_string_arg !== "string") first_string_arg = "";
            let depth = get_depth();
            let indent = "┊ ".repeat(depth);
            let ctx = "";
            if (depth > 3 && moved) {
                ctx = this.context();
                moved = false;
            }
            let str = (indent + fn_name + " " + first_string_arg).padEnd(60) + ctx;
            console.log(str);
            let res = fn.call(this, ...args);
            let ret = res instanceof AST.Node ? `${res.kind}` : "";

            if (res instanceof AST.List) ret += "[" + res.list.length + "]"
            if (ret !== "") {
                console.log(indent + "┕" + fn_name + ": " + ret)
            } else {
                console.log(indent + "┊" + fn_name)
            }

            return res;
        };
    }
}

let file = readFileSync("tests/micro/user.lua", "utf8");
let parser = new Parser(file);
let tokens = parser.parse();

for (let token of tokens) {
    // if (token.type == TOKEN.VAR)
    // console.log(token.toJSON());
}
