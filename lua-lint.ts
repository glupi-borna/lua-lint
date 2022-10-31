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
    NAME, VAR, PREFIX, EXP,
    UNKNOWN,
};

class Token {
    parser: Parser;
    type: TOKEN;
    start: number;
    end: number;
    line: number;

    constructor(parser: Parser, type: TOKEN) {
        this.parser = parser;
        this.type = type;
        this.start = parser.start;
        this.end = parser.current;
        this.line = parser.line;

        if (type == TOKEN.IDENTIFIER && KEYWORDS.includes(this.text)) {
            this.type = TOKEN.KEYWORD;
        }
    }

    get kind() {
        return TOKEN[this.type];
    }

    get text() {
        return this.parser.text.substring(this.start, this.end);
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

class Name extends Token {
    type = TOKEN.NAME;
    constructor(parser: Parser, id: Token) {
        super(parser, TOKEN.NAME);
        this.start = id.start;
        this.end = id.end;
        this.line = id.line;
    }
}

class Var extends Token {
    type = TOKEN.VAR;
    prefix: Name|Prefix;
    name: Name|undefined;
    constructor(parser: Parser, prefix: Name|Prefix, name?: Name) {
        super(parser, TOKEN.VAR);
        this.prefix = prefix;
        this.name = name;
        this.start = this.prefix.start;
        this.end = (this.name || this.prefix).end;
    }
}

class Prefix extends Token {
    type = TOKEN.PREFIX;
    prefix: Var;
    constructor(parser: Parser, prefix: Var) {
        super(parser, TOKEN.PREFIX);
        this.prefix = prefix;
        this.start = prefix.start;
        this.end = prefix.end;
        this.line = prefix.line;
    }
}

class Parser {
    text: string;
    start = 0;
    current = 0;
    line = 0;

    constructor(text: string) {
        this.text = text;
    }

    char(add=0) { return this.text[this.current+add]; }
    move(amt=1) { this.current+=amt; }
    rollback(token?: Token) { this.current = (token || this).start; }
    at_end() { return this.current >= this.text.length; }

    token(type: TOKEN) {
        let t = new Token(this, type);
        this.start = this.current;
        return t;
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
                    this.line++;
                    this.move();
                    break;

                case "-":
                    if (this.char(1) == "-") {
                        this.move(2);
                        if (!this.long_bracket()) {
                            this.move(-2);
                            while (this.char() != "\n" && !this.at_end()) this.move();
                        }
                    } else {
                        this.start = this.current; return;
                    }
                    break;

                default: this.start = this.current; return;
            }
        }
    }

    ident(): Token|undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;
        let c = this.char();
        if (LETTERS.includes(c) || c == "_") {
            this.move();
            while (IDENT_CHARS.includes(this.char())) this.move();
            return this.token(TOKEN.IDENTIFIER);
        } else {
            return undefined;
        }
    }

    num(): Token|undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;
        if (!DIGITS.includes(this.char()) && this.char() !== ".") { return undefined; }
        this.move();
        while (DIGITS.includes(this.char())) this.move();
        if (this.char() != ".") return this.token(TOKEN.NUMBER);
        this.move();
        while (DIGITS.includes(this.char())) this.move();
        return this.token(TOKEN.NUMBER);
    }

    symbol(): Token|undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;

        switch (this.char()) {
            case "=":
            case "~":
            case "<":
            case ">":
                if (this.char(1) == "=") {
                    this.move(2);
                    return this.token(TOKEN.SYMBOL);
                } else if (this.char() != "~") {
                    this.move(1);
                    return this.token(TOKEN.SYMBOL);
                }
                break;
        }

        if (!SIMPLE_SYMBOLS.includes(this.char())) return undefined;
        this.move();
        return this.token(TOKEN.SYMBOL);
    }

    long_bracket(): Token|undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;
        if (this.char() != "[") return undefined;
        let startline = this.line;

        this.move();
        let level = 0;
        while (this.char() == "=" && !this.at_end()) { level++; this.move(); }
        if (this.char() != "[") { this.rollback(); return undefined; }
        this.move();

        while (!this.at_end()) {
            if (this.char() == "\n") { this.line++; }
            while (this.char() != "]" && !this.at_end()) { this.move(); }
            if (this.char() != "]") {
                console.log("Not an LB1", this.text.substring(this.start, this.current), this.char());
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
            return this.token(TOKEN.UNKNOWN);
        }

        this.rollback();
        this.line = startline;
        return undefined;
    }

    str(): Token|undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;
        let startline = this.line;

        let lb = this.long_bracket();
        if (lb) { lb.type = TOKEN.STRING; return lb; }

        let ch = this.char();
        if (!"'\"".includes(ch)) { return undefined; }
        this.move();

        while (this.char() != ch && !this.at_end()) {
            if (this.char() == "\\") this.move();
            this.move();
        }

        if (this.char() != ch) {
            this.line = startline;
            this.rollback();
            return undefined;
        }
        this.move();
        return this.token(TOKEN.STRING);
    }

    either(...fns: (() => Token|undefined)[]): Token|undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;
        for (let fn of fns) {
            let tok = fn.call(this);
            if (tok) return tok;
        }
        return undefined;
    }

    other(): Token|undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;
        this.move();
        return this.token(TOKEN.UNKNOWN);
    }

    name(): Name|undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;
        let ident = this.ident();
        if (!ident) { return undefined; }
        if (ident.type === TOKEN.KEYWORD) { this.rollback(ident); return undefined; }
        return new Name(this, ident);
    }

    prefix(): Prefix|undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;

        let v = this.variable();
        if (!v) { return undefined; }
        return new Prefix(this, v);
    }

    keyword(): Token|undefined {
        this.skip_ws();
        if (this.at_end()) return undefined;

        let kwd = this.ident();
        if (!kwd) { return undefined; }
        if (kwd.type !== TOKEN.KEYWORD) {
            this.rollback(kwd);
            return undefined;
        }
        return kwd;
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
    if (token.type == TOKEN.VAR)
    console.log(token.toString());
}
