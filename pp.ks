const json = import "lib.ks";

const main = () => (
    if std.sys.argc() != 2 then (
        std.io.eprint("Usage: " + std.sys.argv_at(0) + " <file>");
        std.sys.exit(1);
    );
    let file = std.fs.read_file(std.sys.argv_at(1));

    match json.parse_one_total(&file) with (
        | :Ok value => value |> json.Value.pretty_printer |> String.to_string |> std.io.print
        | :Error { .err, .pos } => (
            std.io.eprint("Error at " + String.to_string(pos));
            dbg.print(err);
        )
    );
);

main();
