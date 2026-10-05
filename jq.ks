const json = import "lib.ks";
use json.*;
use (import "stdplus.ks").*;

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
        eprintln("Error: \(msg)");
        return
    );

    let sub_query = parse(reader) |> Result.unwrap_or_else({ .err, ... } => match err with (
        | :ImmediateEOF => (
            # query finished -> print resulting json
            println!("\(json^ |> Value.pretty_printer)");
            return
        )
        | err => panic(&String.to_string(err) |> as_str)
    ));

    match sub_query with (
        | :String key => if json is &(:Object ref pairs) then (
            pairs
                |> List.find_map(&{ obj_key, ref value } =>
                    if obj_key == key then :Some value else :None)
                |> Option.unwrap_or_else(() =>
                    eprint(&format!("object \(name) has no key \(escape_json_string(key))") |> as_str))
                |> run_query(reader, format!("\(name)[\(sub_query)]"));
        ) else (
            eprint(name + " is not an object")
        )

        | :Number num => if json is &(:Array ref elems) then (
            let offset = num
                |> Number.try_u32
                |> Result.unwrap_or_else(_ =>
                    eprint(String.to_string(num) + " is not a valid offset"));
            if offset >= List.length(elems) then
                eprint(String.to_string(offset) + " is out of bounds for array " + name);
            elems
                |> List.at(offset)
                |> run_query(reader, format!("\(name)[\(sub_query)]"));
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
                        std.io.eprint("bad query: " + msg);
                        unwind block ()
                    ),
                };
                with error = (err => (
                    std.io.eprint("bad query");
                    unwind block ()
                ));
                &value |> run_query(&mut reader, "$");
            );
        )
        | :Error { .err, .pos } => (
            eprintln!("Error in input json at \(pos): \(err)");
        )
    );
);

main();

