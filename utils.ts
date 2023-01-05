let print_buffer: string[] = [];

export function print(...args: any[]) {
    print_buffer.push(args.join(" "));
}

export function flush() {
    console.log(print_buffer.join("\n"));
    print_buffer = [];
}
