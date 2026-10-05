use (import "./stdplus.ks").*;

const json = import "./lib.ks";
use json.*;

const assert = (condition :: Bool, msg :: &str) => (
    if condition then (
        ()
    ) else (
        std.panic(&format!("assertion failed: \(msg)") |> as_str)
    )
);

const assert_eq = [T] (lhs :: T, rhs :: T) => (
    if std.repr.structurally_equal(&lhs, &rhs) then (
        ()
    ) else (
        std.dbg.print({.lhs = lhs, .rhs = rhs});
        std.panic("assertion failed: lhs != rhs")
    )
);

const test = (src :: &str, value :: Value) => (
    let mut reader = Reader.create(src);
    let parsed = parse(&mut reader) |> Result.unwrap;
    assert_eq(parsed, value);
);

test("null {", :Null);
test("true {", :Bool true);
test("false {", :Bool false);

test(
    "[{\"some\": \"json\", \"\": true}, {}]",
    :Array (
        const List = std.collections.ArrayList;
        let mut list = List.new();
        &mut list |> List.push_back(:Object (
            let mut list = List.new();
            &mut list |> List.push_back({ String.from_str("some"), :String String.from_str("json") });
            &mut list |> List.push_back({ String.from_str(""), :Bool true });
            list
        ));
        &mut list |> List.push_back(:Object List.new());
        list
    )
);

(
    let mut reader = Reader.create("-5.189e1");
    with error = (err => panic(&String.to_string(err) |> as_str));
    let num = Number.parse(&mut reader) |> Option.unwrap;
    assert_eq(num |> Number.into_f64, -51.89);
    assert_eq(
        num |> Number.try_u32,
        :Error String.from_str("Negative JSON number cannot be converted to UInt32")
    );
);

(
    let mut reader = Reader.create("5189");
    with error = (err => panic(&String.to_string(err) |> as_str));
    let num = Number.parse(&mut reader) |> Option.unwrap;
    assert_eq(num |> Number.try_u32, :Ok 5189);
);

assert_eq((UInt32 as IntoNumber).into(137803 :: UInt32), {
    .digits = String.from_str("137803"),
    .neg = false,
    .fraction_digits = String.from_str(""),
    .exponent = { .neg = true, .digits = String.from_str("") }
});
