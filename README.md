# Kast-JSON

JSON parser in Kast


## Use as pretty-printer

```sh
$ make pp

$ ./pp test.json
# or, from stdin
$ ./pp -
```


## Use as JQ-like JSON query tool

```sh
$ make jq

$ ./jq test.json ""

$ ./jq test.json '"entities" "user_mentions" 0'
# or, from stdin
$ cat test.json | ./jq - '"entities" "user_mentions" 0'

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
$ cat test.json | ./jq - '"entities" "user_mentions" 0' | ./jq - '"indices" 1'
## 9
```


## Use as library

```nim
const {.Reader, .parse, ...} = import "./path/to/this/library/src/_lib.ks";

let mut reader = Reader.new(&"[{\"some\": \"json\", \"\": true}, {}]");

let value = parse(&mut reader) |> Result.unwrap;
println!("\(value |> Value.pretty_printer |> to_string)");

## [
##     {
##         "some": "json",
##         "": true
##     },
##     {
## 
##     }
## ]
```


## Run tests

```sh
$ make test

$ ./test
```

- NOTE: [test.json](test.json) has been taken from within [twitter.json](https://github.com/miloyip/nativejson-benchmark/blob/478d5727c2a4048e835a29c65adecc7d795360d5/data/twitter.json)
- NOTE: You can specify a different target directory than `target` in any of the `make x` build commands above by setting `TARGET`. Eg:
  ```sh
  $ TARGET=out make pp
  $ file out/pp
  # out/pp: ELF 64-bit LSB pie executable
  ```
