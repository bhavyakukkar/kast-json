const json = import "lib.ks";

const main = () => (
    if std.sys.argc() != 2 then (
        std.io.eprint("Usage: " + std.sys.argv_at(0) + " <file>");
        std.sys.exit(1);
    );
    let file = std.fs.read_file(std.sys.argv_at(1));
    let mut reader = json.Reader.create(&file);
    match json.parse(&mut reader) with (
        | :Ok value => value |> String.to_string |> std.io.print
        | :Error { .err, .pos } => (
            std.io.eprint("Error at " + String.to_string(pos));
            dbg.print(err);
        )
    );
);

main();
