const {.ArrayList, .Option, ...} = import "./utils.ks";
const {.Value, .Pair, ...} = import "./value.ks";
const {.error, .ErrorPos, ...} = import "./error.ks";
use (import "./reader.ks").Reader;

const Context = newtype (
    | :Array {
        ArrayList.t[Value],
        .expecting_comma :: Bool
    }
    | :Object {
        ArrayList.t[Pair],
        .key :: Option.t[String],
        .expecting_comma_or_colon :: Bool
    }
);

impl Context as module = (
    module:

    const push_value = (context :: &mut Context, value :: Value) => (
        match context^ with (
            | :Array { ref mut arr, ... } => arr |> ArrayList.push_back(value)
            | :Object { ref mut pairs, .key = ref mut maybe_key, ... } => (
                if maybe_key^ is :Some ref mut key then (
                    pairs |> ArrayList.push_back({ key^, value });
                    maybe_key^ = :None;
                ) else if value is :String s then (
                    maybe_key^ = :Some s;
                )
                else (
                    (@current error)(:NonStrObjectKey) |> from_never
                )
            )
        )
    );

    const new_array = () -> Context => :Array {
        ArrayList.new(),
        .expecting_comma = false,
    };

    const new_object = () -> Context => :Object {
        ArrayList.new(),
        .key = :None,
        .expecting_comma_or_colon = false,
    };

    const as_array = (ctx :: Context) -> Option.t[type {
        ArrayList.t[Value], .expecting_comma :: Bool
    }] => match ctx with (
        | :Array arr => :Some arr
        | _ => :None
    );

    const as_object = (ctx :: Context) -> Option.t[type {
        ArrayList.t[Pair], .key :: Option.t[String], .expecting_comma_or_colon :: Bool
    }] => match ctx with (
        | :Object obj => :Some obj
        | _ => :None
    );

    const as_object_mut = (ctx :: &mut Context) -> Option.t[type (&mut {
        ArrayList.t[Pair], .key :: Option.t[String], .expecting_comma_or_colon :: Bool
    })] => match ctx^ with (
        | :Object ref mut obj => :Some obj
        | _ => :None
    );
);

module:

use Value;
use ErrorPos;

## parse the source-string as an individual JSON value
const parse_one_total = (source :: &str) -> Result.t[Value, ErrorPos] => with_return (
    let mut reader = Reader.new(source);
    parse_one(&mut reader) |> Result.and_then(value => (
        Reader.discard_ws(&mut reader);
        if Reader.peek(&reader) |> Option.is_some_and(c => not Reader.is_json_whitespace(&c))
        then :Error { .err = :TrailingChars, .pos = reader.pos }
        else :Ok value
    ))
);

## parse one JSON value from the reader
const parse_one = (reader :: &mut Reader) -> Result.t[Value, ErrorPos] => with_return (
    let ok = val => return :Ok val;
    with error = (err => return :Error { .err, .pos = reader^.pos });
    let error = @current error;

    let mut ctxs :: ArrayList.t[Context] = ArrayList.new();
    loop (
        let token = match Reader.next_token(reader) with (
            | :Ok token => token
            | :Error err => return :Error err
        );
        let last_ctx = ArrayList.last_mut(&mut ctxs);
        if last_ctx is :Some ctx then (
            match ctx^ with (
                # inside array
                | :Array {
                    ref mut arr, .expecting_comma = ref mut expecting_comma
                } => if expecting_comma^ then (
                    match token with (
                        | :ArrayClose => (
                            # drop `ctx` here

                            # SAFETY: `list_pop` won't panic because `ctxs` is not empty because
                            # `last_ctx` is :Some
                            let arr = :Array (
                                let { arr, ... } = &mut ctxs |>
                                    ArrayList.pop_back |>
                                    Context.as_array |>
                                    Option.expect("impossible, :Array match arm");
                                arr
                            );
                            if &mut ctxs |> ArrayList.last_mut is :Some ctx then (
                                ctx |> Context.push_value(arr)
                            ) else (
                                ok(arr);
                            )
                        )
                        | :Comma => (
                            expecting_comma^ = false;
                        )
                        | _ => error(:ExpectingComma) |> from_never
                    )
                ) else (
                    match token with (
                        | :Null => arr |> ArrayList.push_back(:Null)
                        | :Bool b => arr |> ArrayList.push_back(:Bool b)
                        | :Number num => arr |> ArrayList.push_back(:Number num)
                        | :String s => arr |> ArrayList.push_back(:String s)
                        | :ArrayOpen => (
                            expecting_comma^ = true;
                            # drop `ctx` here

                            &mut ctxs |> ArrayList.push_back(Context.new_array());
                            continue
                        )
                        | :ArrayClose => if &arr^ |> ArrayList.is_empty then (
                            expecting_comma^ = true;
                            # drop `ctx` here

                            # SAFETY: `list_pop` won't panic because `ctxs` is not empty because
                            # `last_ctx` is :Some
                            let arr = :Array (
                                let { arr, ... } = &mut ctxs |>
                                    ArrayList.pop_back |>
                                    Context.as_array |>
                                    Option.expect("impossible, :Array match arm");
                                arr
                            );
                            if &mut ctxs |> ArrayList.last_mut is :Some ctx then (
                                ctx |> Context.push_value(arr)
                            ) else (
                                ok(arr);
                            );
                            continue
                        ) else (
                            error(:TrailingCommaInArray) |> from_never
                        )
                        | :Comma => error(:EmptyArrayElem) |> from_never
                        | :ObjectOpen => (
                            expecting_comma^ = true;
                            # drop `ctx` here

                            &mut ctxs |> ArrayList.push_back(Context.new_object());
                            continue
                        )
                        | :ObjectClose => error(:MismatchedObjectClose) |> from_never
                        | :Colon => error(:UnexpectedColon) |> from_never
                    );
                    expecting_comma^ = true;
                )

                # inside object, waiting for key
                | :Object {
                    ref mut obj,
                    .key = :None,
                    .expecting_comma_or_colon = ref mut expecting_comma
                } => if expecting_comma^ then (
                    match token with (
                        | :ObjectClose => (
                            # drop `ctx` here

                            # SAFETY: `list_pop` won't panic because `ctxs` is not empty because
                            # `last_ctx` is :Some
                            let obj = :Object (
                                let { obj, ... } = &mut ctxs |>
                                    ArrayList.pop_back |>
                                    Context.as_object |>
                                    Option.expect("impossible, :Object match arm");
                                obj
                            );
                            if &mut ctxs |> ArrayList.last_mut is :Some ctx then (
                                ctx |> Context.push_value(obj)
                            ) else (
                                ok(obj);
                            )
                        )
                        | :Comma => (
                            expecting_comma^ = false
                        )
                        | _ => error(:ExpectingComma) |> from_never
                    )
                ) else (
                    match token with (
                        | :String s => (
                            let obj_ctx = ctx |>
                                Context.as_object_mut |>
                                Option.expect(":Object match arm");
                            obj_ctx^.key = :Some s;

                            let expecting_colon = expecting_comma;
                            expecting_colon^ = true;
                        )
                        | :ObjectClose => if &obj^ |> ArrayList.is_empty then (
                            expecting_comma^ = true;
                            # drop `ctx` here

                            # SAFETY: `list_pop` won't panic because `ctxs` is not empty because
                            # `last_ctx` is :Some
                            let obj = :Object (
                                let { obj, ... } = &mut ctxs |>
                                    ArrayList.pop_back |>
                                    Context.as_object |>
                                    Option.expect("impossible, :Object match arm");
                                obj
                            );
                            if &mut ctxs |> ArrayList.last_mut is :Some ctx then (
                                ctx |> Context.push_value(obj)
                            ) else (
                                ok(obj);
                            );
                            continue
                        ) else (
                            error(:TrailingCommaInObject) |> from_never
                        )
                        | :Comma => error(:EmptyObjectPair) |> from_never
                        | :ArrayClose => error(:MismatchedArrayClose) |> from_never
                        | :Colon => error(:EmptyObjectKey) |> from_never
                        | _ => error(:NonStrObjectKey) |> from_never
                    );
                    expecting_comma^ = true;
                )

                # inside object, waiting for value
                | :Object {
                    ref mut obj,
                    .key = :Some ref mut key,
                    .expecting_comma_or_colon = ref mut expecting_colon
                } => if expecting_colon^ then (
                    match token with (
                        | :ObjectClose => error(:MissingPairValue) |> from_never
                        | :Colon => (
                            expecting_colon^ = false
                        )
                        | _ => error(:ExpectingColon) |> from_never
                    )
                ) else (
                    let take_key = (ctx :: &mut Context) -> String => (
                        let obj_ctx = ctx |>
                            Context.as_object_mut |>
                            Option.expect(":Object match arm");
                        let key_was = obj_ctx^.key |> Option.expect("assert key exists failed");
                        obj_ctx^.key = :None;
                        key_was
                    );
                    match token with (
                        | :Null => (
                            # drop `ctx` here
                            let pair :: Pair = { take_key(ctx), :Null };
                            obj |> ArrayList.push_back(pair)
                        )
                        | :Bool b => (
                            # drop `ctx` here
                            let pair :: Pair = { take_key(ctx), :Bool b };
                            obj |> ArrayList.push_back(pair)
                        )
                        | :Number num => (
                            # drop `ctx` here
                            let pair :: Pair = { take_key(ctx), :Number num };
                            obj |> ArrayList.push_back(pair)
                        )
                        | :String s => (
                            # drop `ctx` here
                            let pair :: Pair = { take_key(ctx), :String s };
                            obj |> ArrayList.push_back(pair)
                        )
                        | :ArrayOpen => (
                            let expecting_comma = expecting_colon;
                            expecting_comma^ = true;
                            # drop `ctx` here

                            &mut ctxs |> ArrayList.push_back(Context.new_array());
                            continue
                        )
                        | :ArrayClose => error(:MismatchedArrayClose) |> from_never
                        | :Comma => error(:EmptyObjectValue) |> from_never
                        | :ObjectOpen => (
                            let expecting_comma = expecting_colon;
                            expecting_comma^ = true;
                            # drop `ctx` here

                            &mut ctxs |> ArrayList.push_back(Context.new_object());
                            continue
                        )
                        | :ObjectClose => if &obj^ |> ArrayList.is_empty then (
                            let expecting_comma = expecting_colon;
                            expecting_comma^ = true;
                            # drop `ctx` here

                            # SAFETY: `list_pop` won't panic because `ctxs` is not empty because
                            # `last_ctx` is :Some
                            let obj = :Object (
                                let { obj, ... } = &mut ctxs |>
                                    ArrayList.pop_back |>
                                    Context.as_object |>
                                    Option.expect("impossible, :Object match arm");
                                obj
                            );
                            if &mut ctxs |> ArrayList.last_mut is :Some ctx then (
                                ctx |> Context.push_value(obj)
                            ) else (
                                ok(obj);
                            );
                            continue
                        ) else (
                            error(:ExpectingValue) |> from_never
                        )
                        | :Colon => error(:UnexpectedColon) |> from_never
                    );
                    let expecting_comma = expecting_colon;
                    expecting_comma^ = true;
                )
            )
        ) else (
            match token with (
                | :Null => ok(:Null)
                | :Bool b => ok(:Bool b)
                | :Number n => ok(:Number n)
                | :String s => ok(:String s)
                | :ArrayOpen => (
                    &mut ctxs |> ArrayList.push_back(Context.new_array())
                )
                | :ArrayClose => error(:MismatchedArrayClose) |> from_never
                | :Comma => error(:UnexpectedComma) |> from_never
                | :ObjectOpen => (
                    &mut ctxs |> ArrayList.push_back(Context.new_object())
                )
                | :ObjectClose => error(:MismatchedObjectClose) |> from_never
                | :Colon => error(:UnexpectedColon) |> from_never
            )
        )
    );
    panic("unreachable")
);

