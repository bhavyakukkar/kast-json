use (import "lib.ks").*;

const main = () => (
    if std.sys.argc() != 2 then (
        std.io.eprint(&format!("Usage: \(std.sys.argv_at(0)) (<file> or -)") |> as_str);
        std.sys.exit(1);
    );
    let input = std.sys.argv_at(1);
    let file = if input == "-"
        then std.io.stdin.read_to_end()
        else std.fs.read_file(input);

    match parse_one_total(&file |> as_str) with (
        | :Ok value => println!("\(value |> Value.pretty_printer |> to_string)")
        | :Error { .err, .pos } => (
            std.io.eprint(&format!("Error at \(pos |> to_string):\n\(err |> to_string)") |> as_str);
        )
    );
);

main();
