import { default as assert } from 'assert';
import { readFileSync } from 'fs';
import { LETTERS, DIGITS, IDENT_CHARS, SIMPLE_SYMBOLS } from './char';
import { AST, Literal, Alternating_Array, No_Trailing_Array } from './ast_util';

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
        public suffixes: Suffix[],
        public index: Index
    ) {
        super(parser);
        this.withPos(this.prefix, this.index, ...this.suffixes);
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
        super(parser);
        this.withPos(this.left, this.right);
    }
}

class Unary_Operation extends AST.Node {
    static type = AST.NODE.UNARY_OPERATION;

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
}

class Return_Statement extends AST.Prefixed<AST.KEYWORD<"return">, Expr> {
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

class Parser {
    text: string;
    start = new AST.Position(0, 0);
    current = new AST.Position(0, 0);

    constructor(text: string) {
        this.text = text;
    }

    char(add=0) { return this.text[this.current.index+add]; }
    move(amt=1) { this.current.move(amt); }
    rollback(token?: AST.Node) { this.current.set((token||this).start); }
    at_end() { return this.current.index >= this.text.length; }

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

    symbol(): InstanceType<AST.SYMBOLS[keyof AST.SYMBOLS]> | undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;

        let node: AST.Node|undefined = undefined;
        switch (this.char()) {
            case "=":
            case "~":
            case "<":
            case ">":
                if (this.char(1) == "=") {
                    this.move(2);
                    node = this.ast_node(AST.Node);
                } else if (this.char() != "~") {
                    this.move(1);
                    node = this.ast_node(AST.Node);
                }
                break;
            case ".":
                if (this.char(1) == ".") {
                    if (this.char(2) == ".") {
                        this.move(3);
                        node = this.ast_node(AST.Node);
                    }
                    this.move(2);
                    node = this.ast_node(AST.Node);
                }
                node = this.ast_node(AST.Node);
        }

        if (node === undefined && SIMPLE_SYMBOLS.includes(this.char())) {
            node = this.ast_node(AST.Node);
            this.move();
        };

        if (!node) return undefined;
        return new AST.Symbols[node.text as keyof AST.SYMBOLS](this);
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
            if (!res) { this.rollback(out[0]); return undefined }
            out.push(res);
        }
        return out as any;
    }

    parens<
        FN extends (() => (AST.Node | undefined))
    >(fn: FN): Parens<Exclude<ReturnType<FN>, undefined>> | undefined {
        let seq = this.sequence(
            () => this.exact_symbol("("),
            fn,
            () => this.exact_symbol(")")
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
            () => this.exact_symbol("{"),
            fn,
            () => this.exact_symbol("}")
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
            () => this.exact_symbol("["),
            fn,
            () => this.exact_symbol("]")
        );
        if (!seq) { return undefined; }
        return new Brackets(this, {
            open: seq[0],
            value: seq[1] as Exclude<ReturnType<FN>, undefined>,
            close: seq[2]
        });
    }

    exact<K extends AST.Node>(fn: ()=>K|undefined, text: string): K|undefined {
        let tok = fn.call(this);
        if (!tok) return undefined;
        if (tok.text !== text) { this.rollback(tok); return undefined; }
        return tok;
    }

    exact_symbol<K extends keyof AST.SYMBOLS>(str: Literal<K, string>): AST.SYMBOL<K>|undefined {
        return this.exact(this.symbol, str) as AST.SYMBOL<K>|undefined;
    }

    exact_keyword<K extends keyof AST.KEYWORDS>(str: Literal<K, string>): AST.KEYWORD<K>|undefined {
        return this.exact(this.keyword, str) as AST.KEYWORD<K>|undefined;
    }

    either<
        FN extends (() => (AST.Node | undefined))[]
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

    alternating<
        FNA extends (() => (AST.Node | undefined)),
        FNB extends (() => (AST.Node | undefined))
    >(
        fn_a: FNA,
        fn_b: FNB
    ) {
        this.skip_ws();
        let first = fn_a();
        if (!first) { return undefined; }
        let args = [first];
        while (true) {
            let b = fn_b();
            if (!b) break;
            args.push(b);
            let a = fn_a();
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
        fn_b: FNB
    ) {
        this.skip_ws();
        let first = fn_a();
        if (!first) { return undefined; }
        let args = [first];
        while (true) {
            let b = fn_b();
            if (!b) break;
            let a = fn_a();
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
        let ident = this.ident();
        if (!ident) { return undefined; }
        if (ident.text in AST.Keywords) { this.rollback(ident); return undefined; }
        return ident;
    }

    unop(): Unary_Operator|undefined {
        let op = this.either(
            () => this.exact_symbol("-"),
            () => this.exact_keyword("not"),
            () => this.exact_symbol("#"),
        );
        if (!op) return undefined;
        let u = new Unary_Operator(this);
        u.withPos(op);
        return u;
    }

    binop(): Binary_Operator|undefined {
        let op = this.either(
            () => this.exact_symbol("+"),
            () => this.exact_symbol("-"),
            () => this.exact_symbol("*"),
            () => this.exact_symbol("/"),
            () => this.exact_symbol("^"),
            () => this.exact_symbol("%"),
            () => this.exact_symbol(".."),
            () => this.exact_symbol("<"),
            () => this.exact_symbol(">"),
            () => this.exact_symbol("<="),
            () => this.exact_symbol(">="),
            () => this.exact_symbol("=="),
            () => this.exact_symbol("~="),
            () => this.exact_keyword("and"),
            () => this.exact_keyword("or"),
        );
        if (!op) return undefined;
        let u = new Binary_Operator(this);
        u.withPos(op);
        return u;
    }

    prefix(): Prefix|undefined {
        return this.either(
            () => this.parens(this.expr),
            this.ident
        );
    }

    suffix(): Suffix|undefined {
        return this.either(
            this.call,
            this.index
        );
    }

    expr_list(): Expr_List|undefined {
        let exprs = this.no_trailing(this.expr, () => this.exact_symbol(","));
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
                this.ident,
                () => this.brackets(this.expr)
            ),
            () => this.exact_symbol("="),
            this.expr
        );
        if (!seq) return undefined;
        return new Named_Field(this, ...seq);
    }

    field_sep(): Field_Sep|undefined {
        return this.either(
            () => this.exact_symbol(","),
            () => this.exact_symbol(";")
        );
    }

    field_list(): Field_List|undefined {
        let fields = this.alternating(this.field, this.field_sep)
        if (!fields) return undefined;
        return new Field_List(this, fields);
    }

    args(): Args|undefined {
        this.skip_ws();
        return this.either(
            this.str,
            this.table,
            () => this.parens(this.expr_list)
        )
    }

    method_name(): Method_Name|undefined {
        let seq = this.sequence(
            () => this.exact_symbol(":"),
            this.ident
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

    value(): Value|undefined {
        return this.either(
            () => this.exact_keyword("nil"),
            () => this.exact_keyword("false"),
            () => this.exact_keyword("true"),
            this.num, this.str,
            () => this.exact_symbol("..."),
            () => this.parens(this.expr),
            // TODO: Function
            this.table,
            // TODO: FunctionCall
            this.variable,
            () => this.parens(this.expr),
        );
    }

    keyword(): InstanceType<AST.KEYWORDS[keyof AST.KEYWORDS]> | undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;

        let ident = this.ident();
        if (!ident) { return undefined; }
        if (!(ident.text in AST.Keywords)) {
            this.rollback(ident);
            return undefined;
        }
        let kwd = new AST.Keywords[ident.text as keyof typeof AST.Keywords](this);
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
            tok = fn();
        }
        return arr;
    }

    dot_access(): Dot_Access|undefined {
        this.skip_ws();
        let seq = this.sequence(
            () => this.exact_symbol("."),
            this.ident
        );
        if (!seq) return undefined;
        return new Dot_Access(this, {
            prefix: seq[0],
            value: seq[1]
        });
    }

    index(): Index|undefined {
        this.skip_ws();
        return this.either(
            () => this.brackets(this.expr),
            this.dot_access
        )
    }

    index_access(): Index_Access|undefined {
        this.skip_ws();
        let prefix = this.prefix();
        if (!prefix) return undefined;
        let suffixes = this.list(this.suffix);
        let index = this.index();
        if (!index) {
            this.rollback(prefix);
            return undefined;
        }

        return new Index_Access(this, prefix, suffixes, index);
    }

    variable(): Var|undefined {
        this.skip_ws();
        let val = this.either(
            this.ident
        );
        if (!val) return undefined;
        return new Var(this, val);
    }

    var_list(): Var_List|undefined {
        let args = this.no_trailing(this.variable, ()=>this.exact_symbol(","));
        if (!args) return undefined;
        return new Var_List(this, args)
    }

    assign(): Assign|undefined {
        let seq = this.sequence(
            this.var_list,
            () => this.exact_symbol("="),
            this.expr_list
        );
        if (!seq) return undefined;
        return new Assign(this, {
            vars: seq[0],
            equals: seq[1],
            exprs: seq[2]
        });
    }

    statement(): Statement|undefined {
        return this.either(
            this.assign,
             // @TODO: functioncall |
             // @TODO: doblock |
             // @TODO: while |
             // @TODO: repeat |
             // @TODO: if |
             // @TODO: `for` ident `=´ exp `,´ exp [`,´ exp] doblock |
             // @TODO: `for` namelist `in` explist doblock |
             // @TODO: `function` funcname funcbody |
             // @TODO: `local` `function` ident funcbody |
             // @TODO: `local` namelist [`=´ explist]
        );
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
