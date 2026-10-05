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

const assert_err = [T] (
    v :: Result.t[T, String],
    exp_err :: String,
) => (
    match v with (
        | :Ok _ => (
            std.panic("assertion failed: not an error as unexpected")
        )
        | :Error e => (
            if as_str(&e) == as_str(&exp_err) then (
                ()
            ) else (
                std.panic("assertion failed: error message differs")
            )
        )
    )
);

const test = (src :: &str, value :: Value) => (
    let mut reader = Reader.create(src);
    let parsed = parse(&mut reader) |> Result.unwrap;
    if Value.eq(&parsed, &value) then (
        ()
    ) else (
        println!("test failed: json doesn't match");
        println!("parsed:\n\(to_string(parsed))");
        println!("\nexpected:\n\(to_string(value))");
        std.panic("tests failed")
    )
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
    assert(Number.to_f64(&num) == -51.89, "Number.into_f64 has a bug");
    assert_err(
        &num |> Number.try_u32,
        String.from_str("Negative JSON number cannot be converted to UInt32")
    );
);

(
    let mut reader = Reader.create("5189");
    with error = (err => panic(&String.to_string(err) |> as_str));
    let num = Number.parse(&mut reader) |> Option.unwrap;
    assert(
        &num |> Number.try_u32 |> Result.expect("shouldn't fail") == 5189,
        "Number.try_u32 has a bug",
    );
);

assert(
    Number.eq(
        &(UInt32 as IntoNumber).into(137803 :: UInt32),
        &{
            .digits = String.from_str("137803"),
            .neg = false,
            .fraction_digits = String.from_str(""),
            .exponent = { .neg = true, .digits = String.from_str("") }
        }
    ),
    "(UInt32 as IntoNumber).into has a bug",
);

println!("all tests pass");
