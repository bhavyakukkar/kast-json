const {.escape_json_string, .string_repeat, ...} = import "./utils.ks";
use std.StringBuilder;
use std.fmt.write;

module:

use (import "./utils.ks").ArrayList;
use (import "./number.ks").Number;

const Pair = newtype { String, Value };

const Value = newtype (
    | :Null
    | :Bool Bool
    | :Number Number
    | :String String
    | :Array ArrayList.t[Value]
    | :Object ArrayList.t[Pair]
);

const PrettyPrinter = (
    const PRETTY_PRINTER_INDENT :: &str = "    ";

    module:

    const t = newtype {
        .value :: Value,
        .indent :: UInt32,
    };

    impl t as ToString = {
        .to_string = { .value, .indent } => match value with (
            | :Null => String.from_str("null")
            | :Bool b => String.from_str(if b then "true" else "false")
            | :Number num => String.to_string(num)
            | :String s => escape_json_string(&s |> as_str)
            | :Array ref values => (
                if ArrayList.is_empty(values) then String.from_str("[]")
                else (
                    let mut s = StringBuilder.new();
                    write!(&mut s, "[\n");
                    &mut s |> StringBuilder.add_String((string_repeat(PRETTY_PRINTER_INDENT, indent + 1)));
                    &mut s |> StringBuilder.add_String(
                        (t as ToString).to_string(
                            { .value = (values |> ArrayList.at(0))^, .indent = indent + 1 }
                        )
                    );
                    for { i, value } in ArrayList.iteri(values) do (
                        if i == 0 then continue;
                        write!(&mut s, ",\n");
                        &mut s |> StringBuilder.add_String(string_repeat(PRETTY_PRINTER_INDENT, indent + 1));
                        &mut s |> StringBuilder.add_String(
                            (t as ToString).to_string(
                                { .value = value^, .indent = indent + 1 }
                            )
                        );
                    );
                    write!(&mut s, "\n\(string_repeat(PRETTY_PRINTER_INDENT, indent))]");
                    s |> StringBuilder.into_string
                )
            )
            | :Object ref pairs => (
                let mut s = StringBuilder.new();
                write!(&mut s, "{\n");
                for { i, pair } in ArrayList.iteri(pairs) do (
                    if i != 0 then (
                        &mut s |> StringBuilder.add_str(",\n");
                    );
                    let { ref key, ref value } = pair^;
                    &mut s |> StringBuilder.add_String(
                        string_repeat(PRETTY_PRINTER_INDENT, indent + 1)
                    );
                    &mut s |> StringBuilder.add_String(escape_json_string(key |> as_str));
                    &mut s |> StringBuilder.add_str(": ");
                    &mut s |> StringBuilder.add_String(
                        (t as ToString).to_string(
                            { .value = value^, .indent = indent + 1 }
                        )
                    );
                );
                &mut s |> StringBuilder.add_str("\n");
                &mut s |> StringBuilder.add_String(
                    string_repeat(PRETTY_PRINTER_INDENT, indent)
                );
                write!(&mut s, "}");
                s |> StringBuilder.into_string
            )
        )
    };

#   impl t as std.fmt.Display = {
#       .display = (self, fmt) => match self with (
#           | :Null => fmt^.write_str("null")
#           | :Bool b => fmt^.write_str(if b then "true" else "false")
#           | :Number num => &to_string(num) |> as_str |> fmt^.write_str
#           | :String s => &escape_json_string(&s |> as_str) |> as_str |> fmt^.write_str
#           | :Array ref values => (
#               if ArrayList.is_empty(values) then fmt^.write_str("[]")
#               else (
#                   fmt^.write_str("[\n");
#                   &string_repeat(PRETTY_PRINTER_INDENT, indent + 1) |> as_str |> fmt^.write_str;
#                   (t as std.fmt.Display).to_string(
#                       { .value = (values |> ArrayList.at(0))^, .indent = indent + 1 }
#                   ) |> as_str
#                   for { i, value } in ArrayList.iteri(values) do (
#                       if i == 0 then continue;
#                       write!(&mut s, ",\n");
#                       &mut s |> StringBuilder.add_String(string_repeat(PRETTY_PRINTER_INDENT, indent + 1));
#                       &mut s |> StringBuilder.add_String(
#                           (t as ToString).to_string(
#                               { .value = value^, .indent = indent + 1 }
#                           )
#                       );
#                   );
#                   write!(&mut s, "\n\(string_repeat(PRETTY_PRINTER_INDENT, indent))]");
#                   s |> StringBuilder.into_string
#               )
#           )
#           | :Object ref pairs => (
#               let mut s = StringBuilder.new();
#               write!(&mut s, "{\n");
#               for { i, pair } in ArrayList.iteri(pairs) do (
#                   if i != 0 then (
#                       &mut s |> StringBuilder.add_str(",\n");
#                   );
#                   let { ref key, ref value } = pair^;
#                   &mut s |> StringBuilder.add_String(
#                       string_repeat(PRETTY_PRINTER_INDENT, indent + 1)
#                   );
#                   &mut s |> StringBuilder.add_String(escape_json_string(key |> as_str));
#                   &mut s |> StringBuilder.add_str(": ");
#                   &mut s |> StringBuilder.add_String(
#                       (t as ToString).to_string(
#                           { .value = value^, .indent = indent + 1 }
#                       )
#                   );
#               );
#               &mut s |> StringBuilder.add_str("\n");
#               &mut s |> StringBuilder.add_String(
#                   string_repeat(PRETTY_PRINTER_INDENT, indent)
#               );
#               write!(&mut s, "}");
#               s |> StringBuilder.into_string
#           )
#       ),
#   };
);

impl Value as module = (
    module:

    ## create a JSON pretty-printer of this value
    const pretty_printer = (self :: Value) -> PrettyPrinter.t => {
        .value = self,
        .indent = 0,
    };

    const clone = (v :: &Value) -> Value => match v^ with (
        | :Null => :Null
        | :Bool b => :Bool b
        | :Number (ref n) => :Number Number.clone(n)
        | :String (ref s) => :String (s |> as_str |> String.from_str)
        | :Array (ref a) => :Array (
            a
            |> ArrayList.iter
            |> std.iter.map(clone)
            |> ArrayList.from_iter
        )
        | :Object (ref o) => :Object (
            o
            |> ArrayList.iter
            |> std.iter.map(&{ref key, ref value} => {
                key |> as_str |> String.from_str,
                clone(value),
            })
            |> ArrayList.from_iter
        )
    );

    const eq = (lhs :: &Value, rhs :: &Value) -> Bool => (
        match ({lhs^, rhs^} :: {Value, Value}) with (
            | {:Null, :Null} => true
            | {:Null, _} => false

            | {:Bool l, :Bool r} => l == r
            | {:Bool _, _} => false

            | {:Number (ref l), :Number (ref r)} => Number.eq(l, r)
            | {:Number _, _} => false

            | {:String (ref l), :String (ref r)} => as_str(l) == as_str(r)
            | {:String _, _} => false

            | {:Array (ref l), :Array (ref r)} => with_return (
                let len = ArrayList.length(l);
                if (len != ArrayList.length(r)) then (
                    return false;
                );
                for i in 0..len do (
                    if (not eq(
                        l |> ArrayList.at(i),
                        r |> ArrayList.at(i),
                    )) then (
                        return false;
                    )
                );
                true
            )
            | {:Array _, _} => false

            | {:Object (ref l), :Object (ref r)} => with_return (
                let len = ArrayList.length(l);
                if (len != ArrayList.length(r)) then (
                    return false;
                );
                for i in 0..len do (
                    let le = l |> ArrayList.at(i);
                    let re = r |> ArrayList.at(i);
                    if (as_str(&le^.0) != as_str(&re^.0) or not eq(&le^.1, &re^.1)) then (
                        return false;
                    )
                );
                true
            )
            | {:Object _, _} => false
        )
    )
);

impl Value as ToString = {
    .to_string = value => match value with (
        | :Null => String.from_str("null")
        | :Bool b => String.from_str(if b then "true" else "false")
        | :Number num => String.to_string(num)
        | :String ref s => escape_json_string(s |> as_str)
        | :Array ref values => (
            if ArrayList.is_empty(values) then String.from_str("[]")
            else (
                let mut s = StringBuilder.new();
                write!(&mut s, "[\(to_string(values^.[0]))");
                for {i, value} in ArrayList.iteri(values) do (
                    if i == 0 then continue;
                    write!(&mut s, ",\(to_string(value^))");
                );
                write!(&mut s, "]");
                s |> StringBuilder.into_string
            )
        )
        | :Object ref pairs => (
            if ArrayList.is_empty(pairs) then String.from_str("{}")
            else (
                let { ref first_key, ref first_value } = ArrayList.at(pairs, 0)^;
                let mut s = StringBuilder.new();
                write!(&mut s, "{\(escape_json_string(first_key |> as_str)):\(to_string(first_value^))");
                for { i, pair } in ArrayList.iteri(pairs) do (
                    if i == 0 then continue;
                    let { ref key, ref value } = pair^;
                    write!(&mut s, ",\(escape_json_string(key |> as_str)):\(to_string(value^))");
                );
                write!(&mut s, "}");
                s |> StringBuilder.into_string
            )
        )
    )
};

