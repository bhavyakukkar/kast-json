use (import "./lib.ks").*;
const {.ArrayList, ...} = import "./src/utils.ks";
use std.thread.*;
use std.sync.Mutex;
use std.rc.Rc;
use std.Ast;

let stdout = Mutex.new(());
@defer Mutex.drop(stdout);

const TestCount = newtype {
    .num_passed :: UInt32,
    .failed :: Rc.t[ArrayList.t[type {UInt32, String}]],
};
let test_count = Mutex.new({.num_passed = 0, .failed = Rc.new(ArrayList.new())} :: TestCount);
@defer Mutex.drop(test_count);

let mut test_idx = 0;

let mut threads :: ArrayList.t[Id] = ArrayList.new();

const wait_for_tests_to_finish = (_ :: Ast) => `(
    for thread in threads |> ArrayList.into_iter do (
        join(thread);
    );
);
const print_test_results = (_ :: Ast) => `(
    let guard = Mutex.lock(&test_count);
    @defer Mutex.unlock(guard);

    if &guard.value^.failed^ |> ArrayList.is_empty then (
        std.io.println!("all \(guard.value^.num_passed) tests passed 👍");
    ) else (
        std.io.println!("failed tests:");
        for &{idx, msg} in &guard.value^.failed^ |> ArrayList.iter do (
            std.io.println!("\(idx): \(msg)");
        );
    );
);


const println = args => `(
    (
        let guard = Mutex.lock(&stdout);
        @defer Mutex.unlock(guard);
        std.io.println!($args);
    )
);
const panic = args => `(
    (
        let guard = Mutex.lock(&stdout);
        @defer Mutex.unlock(guard);
        std.panic(&format!($args) |> as_str)
    )
);
const assert = args => (
    let mut args = args |> Ast.get_comma_separated_list;
    let msg = &mut args |> ArrayList.pop_back();
    let condition = &mut args |> ArrayList.pop_back();

    `(
        if $condition then (
            ()
        ) else (
            panic!("assertion failed: \($msg)")
        )
    )
);
const assert_err = args => (
    let mut args = args |> Ast.get_comma_separated_list;
    let exp_err = &mut args |> ArrayList.pop_back();
    let v = &mut args |> ArrayList.pop_back();

    `(
        match $v with (
            | :Ok _ => (
                panic!("assertion failed: not an error as unexpected")
            )
            | :Error e => (
                if as_str(&e) == $exp_err then (
                    ()
                ) else (
                    panic!("assertion failed: error message differs")
                )
            )
        )
    )
);
const assert_expected = args => (
    let mut args = args |> Ast.get_comma_separated_list;
    let value = &mut args |> ArrayList.pop_back();
    let src = &mut args |> ArrayList.pop_back();

    `(
        let mut reader = Reader.new($src);
        let parsed = parse(&mut reader) |> Result.unwrap;
        if Value.eq(&parsed, &$value) then (
            ()
        ) else (
            println!("test failed: json doesn't match");
            println!("parsed:\n\(to_string(parsed))");
            println!("\nexpected:\n\(to_string($value :: Value))");
            panic!("tests failed")
        )
    )
);

const test = (body :: Ast) -> Ast => @comptime_only `(
    test_idx += 1;
    let thread = spawn(@move () => with_return (
        with std.PanicHandler = {
            .handle = @move msg => (
                let guard = Mutex.lock(&test_count);
                &mut guard.value^.failed^
                    |> ArrayList.push_back({test_idx, String.from_str(msg)});
                Mutex.unlock(guard);
                return
                # panic!("\(msg)")
            )
        };
        $body;
        (
            let guard = Mutex.lock(&test_count);
            @defer Mutex.unlock(guard);
            guard.value^.num_passed += 1;
        )
    ));
    &mut threads |> ArrayList.push_back(thread);
);

# 1
# longest test first so it can finish while other tests run
test!((
    let contents = std.fs.read_file("twitter.json");
    let mut reader = &contents |> as_str |> Reader.new;
    with error = (err => panic!("\(err |> to_string)"));
    match parse(&mut reader) with (
        | :Ok _ => ()
        | :Error { .err, .pos } => (
            panic!("failed to parse twitter.json: \(err |> to_string) at \(pos |> to_string)")
        )
    );
));

# 2
test!(assert_expected!("null {", :Null));
# 3
test!(assert_expected!("true {", :Bool true));
# 4
test!(assert_expected!("false {", :Bool false));

# 5
test!(assert_expected!(
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
));

# 6
test!((
    let mut reader = Reader.new("-5.189e1");
    with error = (err => panic!("\(err |> to_string)"));
    let num = Reader.parse_number(&mut reader) |> Option.unwrap;
    assert!(Number.to_f64(&num) == -51.89, "Number.into_f64 has a bug");
    assert_err!(
        &num |> Number.try_u32,
        "Negative JSON number cannot be converted to UInt32"
    );
));

# 7
test!((
    let mut reader = Reader.new("5189");
    with error = (err => panic!("\(err |> to_string)"));
    let num = Reader.parse_number(&mut reader) |> Option.unwrap;
    assert!(
        &num |> Number.try_u32 |> Result.expect("shouldn't fail") == 5189,
        "Number.try_u32 has a bug",
    );
));

# 8
test!(assert!(
    Number.eq(
        &(UInt32 as IntoNumber).into(137803 :: UInt32),
        &{
            .digits = String.from_str("137803"),
            .neg = false,
            .fraction_digits = String.from_str(""),
            .exponent = { .neg = false, .digits = String.from_str("") }
        }
    ),
    "(UInt32 as IntoNumber).into has a bug",
));

wait_for_tests_to_finish!();

print_test_results!();

