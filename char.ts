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

export const LETTERS = new Char_Classifiers("az", "AZ");
export const DIGITS = new Char_Classifiers("09");
export const IDENT_CHARS = new Char_Classifiers(...LETTERS.classifiers, ...DIGITS.classifiers, "__");
export const SIMPLE_SYMBOLS = new Char_Classifiers(new Char_List("+-*/%^#<>=(){}[];:,."));
