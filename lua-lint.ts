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

enum TOKEN {
    IDENTIFIER, NUMBER, KEYWORD, STRING, SYMBOL,
    NAME, VAR, PREFIX, EXPR, VALUE,
    UNOP, BINOP,
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

class Token {
    type: TOKEN = TOKEN.UNKNOWN;
    parser: Parser;
    start: Position;
    end: Position;

    constructor(parser: Parser) {
        this.parser = parser;
        this.start = parser.start.copy();
        this.end = parser.current.copy();
    }

    get kind() {
        return TOKEN[this.type];
    }

    get text() {
        return this.parser.text.substring(this.start.index, this.end.index);
    }

    setPos(startt: Token, endt?: Token) {
        this.start.set(startt.start);
        this.end.set((endt || startt).end);
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

class Identifier extends Token { type = TOKEN.IDENTIFIER; }
class Num extends Token { type = TOKEN.NUMBER; }
class Str extends Token { type = TOKEN.STRING; }
class Name extends Token { type = TOKEN.NAME; }
class Keyword extends Token { type = TOKEN.KEYWORD; }
class Sym extends Token { type = TOKEN.SYMBOL; }
class UnOp extends Sym { type = TOKEN.UNOP; }
class BinOp extends Sym { type = TOKEN.BINOP; }

class Value extends Token {
    type = TOKEN.VALUE;
    value: Keyword | Num | Str | Sym;

    constructor(parser: Parser, val: Token) {
        super(parser);
        this.value = val;
        this.setPos(val);
    }
}

class Var extends Token {
    type = TOKEN.VAR;
    prefix: Name|Prefix;
    name: Name|Expr|undefined; // TODO: exp
    constructor(parser: Parser, prefix: Name|Prefix, name?: Name|Expr) {
        super(parser);
        this.prefix = prefix;
        this.name = name;
        this.setPos(this.prefix, this.name);
    }
}

class Prefix extends Token {
    type = TOKEN.PREFIX;
    prefix: Var | Expr; // TODO: functioncall, `(` exp `)`
    constructor(parser: Parser, prefix: Var|Expr) {
        super(parser);
        this.prefix = prefix;
        this.start = prefix.start.copy();
        this.end = prefix.end.copy();
    }
}

class Expr extends Token {
    type = TOKEN.EXPR;
    value: Value;

    constructor(parser: Parser, val: Value) {
        super(parser);
        this.value = val;
        this.setPos(val);
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
    rollback(token?: Token) { this.current.set((token||this).start); }
    at_end() { return this.current.index >= this.text.length; }

    token<
        T extends Token,
        FN extends new (p: Parser, ...args: DropFirst<ConstructorParameters<FN>>) => T
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
                        if (!this.long_bracket(Token)) {
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
            return this.token(Identifier);
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
        if (this.char() != ".") return this.token(Num);
        this.move();
        while (DIGITS.includes(this.char())) this.move();
        return this.token(Num);
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
                    return this.token(Sym);
                } else if (this.char() != "~") {
                    this.move(1);
                    return this.token(Sym);
                }
                break;
            case ".":
                if (this.char(1) == ".") {
                    if (this.char(2) == ".") {
                        this.move(3);
                        return this.token(Sym);
                    }
                    this.move(2);
                    return this.token(Sym);
                }
                return this.token(Sym);
        }

        if (!SIMPLE_SYMBOLS.includes(this.char())) return undefined;
        this.move();
        return this.token(Sym);
    }

    long_bracket<T extends Token>(token_type: new (parser: Parser) => T) {
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
            return this.token(token_type);
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
        return this.token(Str);
    }

    sequence<
        FN extends (() => (Token | undefined))[]
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
        FN extends (() => (Token | undefined))[]
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
        return this.token(Token);
    }

    name() {
        this.skip_ws();
        if (this.at_end()) return undefined;
        let ident = this.ident();
        if (!ident) { return undefined; }
        if (ident.type === TOKEN.KEYWORD) { this.rollback(ident); return undefined; }
        return this.token(Name);
    }

    unop(): UnOp|undefined {
        let op = this.either(
            () => this.exact(this.symbol, "-"),
            () => this.exact(this.keyword, "not"),
            () => this.exact(this.symbol, "#"),
        );
        if (!op) return undefined;
        let u = new UnOp(this);
        u.setPos(op);
        return u;
    }

    binop(): BinOp|undefined {
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
        let u = new BinOp(this);
        u.setPos(op);
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
        return this.token(Prefix, pref);
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
        kwd.setPos(ident);
        return kwd;
    }

    expr(): Expr|undefined {
        let val = this.value();
        if (!val) { return undefined; }

        let e = this.token(Expr, val);
        return e;
    }

    list<K extends Token>(fn: () => K|undefined): K[] {
        let arr = [];
        let tok = fn.call(this);
        while (tok) {
            arr.push(tok);
            tok = fn();
        }
        return arr;
    }

    exact<K extends Token>(fn: ()=>K|undefined, text: string): K|undefined {
        let tok = fn.call(this);
        if (!tok) return undefined;
        if (tok.text !== text) { this.rollback(tok); return undefined; }
        return tok;
    }

    variable(): Var|undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;

        let prefix: Name|Prefix|undefined = this.name();
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
