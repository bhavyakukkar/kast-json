const json = include "./lib.ks";
use json.*;

const assert = (condition :: Bool, msg :: String) => (
    if condition then (
        ()
    ) else (
        std.panic("assertion failed: " + msg)
    )
);

const assert_eq = [T] (lhs :: T, rhs :: T) => (
    if std.repr.structurally_equal(lhs, rhs) then (
        ()
    ) else (
        std.dbg.print({.lhs = lhs, .rhs = rhs});
        std.panic("assertion failed: lhs != rhs")
    )
);

const test = (src :: &String, value :: Value) => (
    let mut reader = Reader.create(src);
    let parsed = parse(&mut reader) |> Result.unwrap;
    assert_eq(parsed, value);
);

test(&"null {", :Null);
test(&"true {", :Bool true);
test(&"false {", :Bool false);

test(
    &"[{\"some\": \"json\", \"\": true}, {}]",
    :Array (
        const List = std.collections.ArrayList;
        let mut list = List.new();
        &mut list |> List.push_back(:Object (
            let mut list = List.new();
            &mut list |> List.push_back({ "some", :String "json" });
            &mut list |> List.push_back({ "", :Bool true });
            list
        ));
        &mut list |> List.push_back(:Object List.new());
        list
    )
);

(
    let mut reader = Reader.create(&"-5.189e1");
    with error = ([T] (err => panic(String.to_string(err))));
    let num = Number.parse(&mut reader) |> Option.unwrap;
    assert_eq(num |> Number.into_f64, -51.89);
    assert_eq(
        num |> Number.try_u32,
        :Error "Negative JSON number cannot be converted to UInt32"
    );
);

(
    let mut reader = Reader.create(&"5189");
    with error = ([T] (err => panic(String.to_string(err))));
    let num = Number.parse(&mut reader) |> Option.unwrap;
    assert_eq(num |> Number.try_u32, :Ok 5189);
);

assert_eq((UInt32 as Into[Number]).into(137803 :: UInt32), {
    .digits = "137803",
    .neg = false,
    .fraction_digits = "",
    .exponent = { .neg = true, .digits = "" }
});
