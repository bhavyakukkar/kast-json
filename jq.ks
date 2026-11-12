use (import "lib.ks").*;
const {.ArrayList, .escape_json_string, .uint32_to_int32, ...} = import "./src/utils.ks";

## JQ-like query tool that uses the provided Reader not just for input JSON but also for the query
## 
## - Queries are just multiple sub-queries separated by JSON whitespace
## - Sub-queries are either JSON strings or integer JSON numbers
## - JSON string subqueries will lookup and enter input JSON objects
## - Integer JSON number subqueries will lookup and enter input JSON arrays
## - At the end of the query the resulting JSON value is pretty-printed

module:
const run_query = (json :: &Value, reader :: &mut Reader, name :: String) => with_return (
    const eprint = [T] (msg :: &str) -> T => (
        panic(&format!("Error: \(msg)") |> as_str)
    );

    let sub_query = parse(reader) |> Result.unwrap_or_else({ .err, ... } => match err with (
        | :ImmediateEOF => (
            # query finished -> print resulting json
            println!("\(json^ |> Value.pretty_printer |> to_string)");
            return
        )
        | err => panic(&format!("bad query: \(err |> to_string)") |> as_str)
    ));

    match sub_query with (
        | :String key => if json is &(:Object ref pairs) then (
            pairs
                |> ArrayList.find_map((&{ obj_key, ref value } :: &Pair) => (
                    if obj_key == key then :Some value else :None
                ))
                |> Option.unwrap_or_else(() => (
                    eprint(&format!("object \(name) has no key \(escape_json_string(&key |> as_str))")
                        |> as_str)
                ))
                |> run_query(reader, format!("\(name)[\(sub_query |> to_string)]"));
        ) else (
            eprint(&format!("\(name) is not an object") |> as_str)
        )

        | :Number num => if json is &(:Array ref elems) then (
            let offset = &num
                |> Number.try_u32
                |> Result.unwrap_or_else(_ =>
                    eprint(&format!("\(num |> to_string) is not a valid offset") |> as_str))
                |> uint32_to_int32;
            if offset >= ArrayList.length(elems) then
                eprint(&format!("\(offset) is out of bounds for array \(name)") |> as_str);
            elems
                |> ArrayList.at(offset)
                |> run_query(reader, format!("\(name)[\(sub_query |> to_string)]"));
        ) else (
            eprint(&format!("\(name) is not an array") |> as_str)
        )

        | _ => panic("sub-queries can only be JSON strings or JSON numbers")
    );
);

const main = () => (
    if std.sys.argc() != 3 then (
        eprintln!("Usage: \(std.sys.argv_at(0)) (<file> or -) <query>");
        std.sys.exit(1);
    );
    let input = std.sys.argv_at(1);
    let file = if input == "-"
        then std.io.stdin.read_to_end()
        else std.fs.read_file(input);
    let query = std.sys.argv_at(2);

    match parse_one_total(&file |> as_str) with (
        | :Ok value => (
            let mut reader = Reader.create(query);
            unwindable block (
                with std.PanicHandler = {
                    .handle = msg => (
                        print!("\(msg)");
                        unwind block ()
                    ),
                };
                with error = (err => (
                    std.io.eprint("bad query");
                    unwind block ()
                ));
                &value |> run_query(&mut reader, String.from_str("$"));
            );
        )
        | :Error { .err, .pos } => (
            eprintln!("Error in input json at \(pos |> to_string): \(err |> to_string)");
        )
    );
);

main();

