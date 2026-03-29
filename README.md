# Kast-JSON

JSON parser in Kast


## Use as pretty-printer

```sh
$ kast run pp.ks test.json
# or, from stdin
$ kast run pp.ks -
```


## Use as JQ-like JSON query tool

- Currently only works on the JS target
- See [jq.ks](jq.ks)

```sh
$ kast compile --target js --output jq.js jq.ks

$ node jq.js test.json ""

$ node jq.js test.json '"entities" "user_mentions" 0'
# or, from stdin
$ cat test.json | node jq.js - '"entities" "user_mentions" 0'

## {
##     "screen_name": "aym0566x",
##     "name": "前田あゆみ",
##     "id": 866260188,
##     "id_str": "866260188",
##     "indices": [
##         0,
##         9
##     ]
## }

# chain pipes
$ cat test.json | node jq.js - '"entities" "user_mentions" 0' | node jq.js - '"indices" 1'
## 9
```


## Use as library

```nim
const json = import "lib.ks";

let mut reader = json.Reader.create(&"[{\"some\": \"json\", \"\": true}, {}]");

match json.parse(&mut reader) with (
    | :Ok value => value |> json.Value.pretty_printer |> String.to_string |> std.io.print
    | :Error err => err |> dbg.print
);

## [
##     {
##         "some": "json",
##         "": true
##     },
##     {}
## ]
```

- NOTE: [test.json](test.json) has been taken from within [twitter.json](https://github.com/miloyip/nativejson-benchmark/blob/master/data/twitter.json)
