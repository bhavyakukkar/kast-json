## Use as pretty-printer
```console
$ kast run pp.ks test.json
```

## Use as library
```nim
const json = import "lib.ks";

let mut reader = json.Reader.create(&"[{\"some\": \"json\", \"\": true}, {}]");

match json.parse(&mut reader) with (
    | :Ok value => value |> json.Value.pretty_printer |> String.to_string |> std.io.print
    | :Error err => err |> dbg.print
);
# [
#     {
#         "some": "json",
#         "": true
#     },
#     {}
# ]
```
