const json = include "lib.ks";
use json.*;

const main = () => (
    if std.sys.argc() != 2 then (
        std.io.eprint("Usage: " + std.sys.argv_at(0) + " (<file> or -)");
        std.sys.exit(1);
    );
    let input = std.sys.argv_at(1);
    let file = if input == "-"
        then std.io.stdin.read_to_end()
        else std.fs.read_file(input);

    match parse_one_total(&file) with (
        | :Ok value => value |> Value.pretty_printer |> String.to_string |> std.io.print
        | :Error { .err, .pos } => (
            std.io.eprint("Error at "
                + String.to_string(pos)
                + ":\n"
                + String.to_string(err));
        )
    );
);

main();
