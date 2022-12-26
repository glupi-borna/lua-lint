import { default as assert } from 'assert';

export type Literal<LIT, TYPE> = TYPE extends LIT ? never : LIT;

interface Alternating_Brand { $alternating_brand: void };
export type Alternating_Array<A, B> = (A|B)[] & Alternating_Brand;
export function is_alternating<A, B>(
    arr: (A|B)[],
    a_validator: (o: any) => o is A,
    b_validator: (o: any) => o is B,
): arr is Alternating_Array<A, B> {
    for (let i=0; i<arr.length; i++) {
        if (i%2 === 0) {
            if (!a_validator(arr[i])) { return false; }
        } else {
            if (!b_validator(arr[i])) { return false; }
        }
    }
    return true;
}

interface No_Trailing_Brand { $no_trailing_brand: void };
export type No_Trailing_Array<A, B> = Alternating_Array<A, B> & No_Trailing_Brand;
export function is_not_trailing<A, B>(
    arr: Alternating_Array<A, B>,
    a_validator: (o: any) => o is A
): arr is No_Trailing_Array<A, B> {
    return arr.length == 0 || a_validator(arr[arr.length-1]);
}


interface Parser {
    text: string;
    start: AST.Position;
    current: AST.Position;
    move(amt?: number): void;
}

export namespace AST {
    export class Position {
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

    export enum NODE {
        IDENTIFIER, NUMBER, STRING, VAR, EXPR,

        UNARY_OPERATOR, BINARY_OPERATOR,
        UNARY_OPERATION, BINARY_OPERATION,

        BLOCK, LAST_STATEMENT,
        FUNC_NAME_NO_METHOD,
        FUNC_CALL, FUNC, FUNC_BODY,
        TABLE, DOT_ACCESS,
        INDEX_ACCESS, METHOD_CALL,
        PARENS, BRACES, BRACKETS, NAMED_FIELD,
        RETURN, ASSIGN, SUFFIXES, METHOD_NAME, FUNC_NAME,
        VAR_ARGS, PARAMS, FUNC_DECL, LOCAL_FUNC_DECL,
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

    export type CLASS_MAP = Record<string, NODE>;

    interface Node_Constructor<T> {
        new (...args: any): T;
        type: NODE;
    }


    export function is<T extends Node>(obj: any, node_type: Node_Constructor<T>): obj is T {
        if (!(obj instanceof Node)) return false;
        return obj.type === node_type.type;
    }

    export function is_any<Ts extends Node_Constructor<any>[]>(obj: any, ...args: Ts): obj is Ts[number] {
        if (!(obj instanceof Node)) return false;
        return args.some(c => obj.type === c.type);
    }

    export function matcher<T extends Node>(c: Node_Constructor<T>) {
        return function (o: any): o is T {
            return is(o, c);
        }
    }

    export function start_and_end(...nodes: Node[]): [Position, Position] {
        let min_pos = nodes[0].start;
        let max_pos = nodes[0].end;
        for (let node of nodes) {
            if (!node) continue;
            if (node.start.index < min_pos.index) min_pos = node.start;
            if (node.end.index > max_pos.index) max_pos = node.end;
        }
        return [min_pos.copy(), max_pos.copy()];
    }

    function nl_indent(depth=0) {
        return "\n" + "\t".repeat(depth);
    }

    export class Node {
        static type: NODE = NODE.UNKNOWN;
        static allow_empty = false;
        start: Position;
        end: Position;

        constructor(public parser: Parser, start?: Position, end?: Position) {
            this.parser = parser;

            start = start ?? parser.start;
            end = end ?? parser.current;

            assert(start !== end, "Start and current are the same object.");
            if (!this.allow_empty) {
                assert(
                    start.index !== end.index,
                    "Start and current are at the same index.");
            }
            this.start = start.copy();
            this.end = end.copy();
        }

        get allow_empty() {
            return (this.constructor as typeof Node).allow_empty;
        }

        get type() {
            return (this.constructor as typeof Node).type;
        }

        get kind() {
            return NODE[(this.constructor as typeof Node).type];
        }

        get text() {
            return this.parser.text.substring(this.start.index, this.end.index);
        }

        withPos(...tokens: Node[]) {
            let [min_pos, max_pos] = start_and_end(...tokens);
            this.start.set(min_pos);
            this.end.set(max_pos);
            return this;
        }

        keys() {
            return Object.keys(this).
                concat("text", "kind")
                .filter(k => ![
                    "parser", "start", "end", "type"
                ].includes(k)
            );
        }

        toString() {
            return JSON.stringify(this);
        }

        toJSON() {
            let props: any = {};
            for (let key of this.keys()) {
                props[key] = (this as any)[key];
            }
            return [
                this.constructor.name,
                props
            ];
        }
    }

    export abstract class Wrapper<T extends Node> extends Node {
        constructor(parser: Parser, public value: T) {
            super(parser, value.start, value.end);
        }
    }

    export abstract class Sequence<T extends Record<string, Node>> extends Node {
        constructor(
            parser: Parser,
            public fields: T
        ) {
            super(parser, ...start_and_end(...Object.values(fields)));
        }
    }

    export abstract class Wrapped<
        OPEN extends Node,
        T extends Node,
        CLOSE extends Node
    > extends Sequence<{
        open: OPEN,
        value: T,
        close: CLOSE
    }> {}

    export abstract class Prefixed<
        PREFIX extends Node,
        T extends Node
    > extends Sequence<{
        prefix: PREFIX,
        value: T
    }> {}

    export abstract class List<T extends Node> extends Node {
        constructor(
            parser: Parser,
            public list: T[]
        ) {
            let start = parser.start;
            let end = parser.current

            if (list.length > 0) {
                [start, end] = start_and_end(...list)
            }
            super(parser, start, end);
        }
    }

    export abstract class Delimited<
        T extends Node,
        DELIMITER extends Node
    > extends List<T|DELIMITER> {
        constructor(
            parser: Parser,
            args: No_Trailing_Array<T, DELIMITER>
        ) {
            super(parser, args);
        }
    }

    export abstract class Delimited_Trailing<
        T extends Node,
        DELIMITER extends Node
    > extends List<T|DELIMITER> {
        constructor(
            parser: Parser,
            args: Alternating_Array<T, DELIMITER>
        ) {
            super(parser, args);
            this.withPos(...args);
        }
    }

    function ast_class<
        TEXT extends string,
        AST extends NODE
    >(
        ast_text: Literal<TEXT, string>,
        ast_type: Literal<AST, NODE>,
    ) {
        return class _ extends Node {
            static literal = ast_text;
            static type = ast_type;
            get type(): AST {
                return _.type;
            }
            constructor(parser: Parser, node?: Node) { super(parser, node?.start, node?.end); }
        };
    }

    function ast_class_map<
        T extends CLASS_MAP
    >(
        obj: Literal<T, CLASS_MAP>
    ): {
        [KEY in keyof T]: KEY extends string ? ReturnType<typeof ast_class<KEY, T[KEY]>> : never
    } {
        let out = {} as any;
        for (let key in obj) {
            out[key] = (ast_class as any)(key, obj[key]);
        }
        return out as ReturnType<typeof ast_class_map<T>>;
    }


    export const Keywords = ast_class_map({
        "and": NODE.KWD_AND,
        "break": NODE.KWD_BREAK,
        "do": NODE.KWD_DO,
        "else": NODE.KWD_ELSE,
        "elseif": NODE.KWD_ELSEIF,
        "end": NODE.KWD_END,
        "false": NODE.KWD_FALSE,
        "for": NODE.KWD_FOR,
        "function": NODE.KWD_FUNCTION,
        "if": NODE.KWD_IF,
        "in": NODE.KWD_IN,
        "local": NODE.KWD_LOCAL,
        "nil": NODE.KWD_NIL,
        "not": NODE.KWD_NOT,
        "or": NODE.KWD_OR,
        "repeat": NODE.KWD_REPEAT,
        "return": NODE.KWD_RETURN,
        "then": NODE.KWD_THEN,
        "true": NODE.KWD_TRUE,
        "until": NODE.KWD_UNTIL,
        "while": NODE.KWD_WHILE,
    });
    export type KEYWORDS = typeof Keywords;
    export type KEYWORD<T extends keyof KEYWORDS> = InstanceType<typeof Keywords[T]>;

    export const Symbols = ast_class_map({
        "+": NODE.SYM_ADD,
        "-": NODE.SYM_SUB,
        "*": NODE.SYM_MUL,
        "/": NODE.SYM_DIV,
        "%": NODE.SYM_MOD,
        "^": NODE.SYM_EXP,
        "#": NODE.SYM_LEN,
        "<": NODE.SYM_LESS,
        ">": NODE.SYM_MORE,
        "=": NODE.SYM_ASSIGN,
        "==": NODE.SYM_EQUALS,
        "~=": NODE.SYM_NOT_EQUALS,
        "<=": NODE.SYM_LESS_EQUALS,
        ">=": NODE.SYM_MORE_EQUALS,
        "(": NODE.SYM_PAR_OPEN,
        ")": NODE.SYM_PAR_CLOSE,
        "{": NODE.SYM_BRACE_OPEN,
        "}": NODE.SYM_BRACE_CLOSE,
        "[": NODE.SYM_BRACKET_OPEN,
        "]": NODE.SYM_BRACKET_CLOSE,
        "..": NODE.SYM_CONCAT,
        ".": NODE.SYM_DOT,
        ",": NODE.SYM_COMMA,
        ":": NODE.SYM_COLON,
        ";": NODE.SYM_SEMICOLON,
        "...": NODE.SYM_DOTDOTDOT,
    });
    export type SYMBOLS = typeof Symbols;
    export type SYMBOL<T extends keyof SYMBOLS> = InstanceType<typeof Symbols[T]>;
}
