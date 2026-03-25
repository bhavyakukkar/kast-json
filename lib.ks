module:
use std.*;

# Notes
# - a variant of Iterable that can be stopped when you don't want it to iterate any more would be
#   helpful

const CharPlus = (
    module:

    # Unicode Control Characters
    # https://www.compart.com/en/unicode/category/Cc

    # First and last C0 Control characters
    const NUL :: Char = (@eval Char.from_code(0));
    const US :: Char = (@eval Char.from_code(31));

    # First and last C1 Control characters
    const DEL :: Char = (@eval Char.from_code(127));
    const APC :: Char = (@eval Char.from_code(159));

    const is_control = (c :: &Char) -> Bool => (
        let c = Char.code(c^);
        # C0 control codes
        (c >= (@eval Char.code(NUL)) and c <= (@eval Char.code(US))) or
            # C1 control codes
            (c >= (@eval Char.code(DEL)) and c <= (@eval Char.code(APC)))
    );
);

const Option = (
    module:

    use std.Option.*;

    # https://github.com/rust-lang/rust/pull/94317 :(
    const inspect_none = [T] (opt :: Option[T], f :: () -> ()) -> Option[T] => match opt with (
        | :Some x => :Some x
        | :None => (
            f();
            :None
        )
    );

    const map_or_else = [T, U] (opt :: Option[T], default :: () -> U, f :: T -> U) -> U => (
        match opt with (
            | :Some x => f(x)
            | :None => default()
        )
    );

    const is_some_and = [T] (opt :: Option[T], f :: T -> Bool) -> Bool => (
        match opt with (
            | :Some x => f(x)
            | :None => false
        )
    );

    const expect = [T] (opt :: Option[T], msg :: String) -> T => match opt with (
        | :Some x => x
        | :None => panic("unwrapped :None: " + msg)
    );
);

const StringPlus = (
    module:

    const of_char = (c :: Char) => (Char as ToString).to_string(c);
);

const BoolPlus = (
    module:

    # i just want to pattern match on booleans
    const t = newtype (
        | :True
        | :False
    );

    const to_t = (b :: Bool) -> t => if b then :True else :False;

    const then_some = [T] (b :: Bool, t :: T) -> Option.t[T] => (
        if b then (
            :Some t
        ) else (
            :None
        )
    );
);

const List = (
    module:

    use std.collections.ArrayList.*;

    const last = [T] (list :: &t[T]) -> Option.t[type (&T)] => (
        let len = length(list);
        if len == 0 then (
            :None
        ) else (
            :Some (list |> at(len - 1))
        )
    );

    const last_mut = [T] (list :: &mut t[T]) -> Option.t[type (&mut T)] => (
        let len = length(&list^);
        if len == 0 then (
            :None
        ) else (
            :Some (list |> at_mut(len - 1))
        )
    );

    const is_empty = [T] (list :: &t[T]) -> Bool => length(list) == 0;
);

const Tup = (
    module:

    const fst = [T] (tup :: {T, T}) -> T => tup.0;
);

const Error = newtype (
    | :ImmediateEOF
    | :UnexpectedEOF
    | :UnknownForm
    # Number errors
    | :LeadingZero
    | :NoDigitsAfterDecimal
    | :NoDigitsAfterExp
    | :MissingDigitsPart # character following `-` in a JSON Number must be a digit
    | :InvalidChar # control character in string
    | :InvalidUnicode
    | :InvalidEsc
    | :MismatchedArrayClose
    | :UnexpectedComma
    | :ExpectingComma
    | :EmptyArrayElem
    | :NonStrObjectKey
    | :MismatchedObjectClose
    | :UnexpectedColon
    | :ExpectingColon
    | :ExpectingValue
    | :EmptyObjectKey
    | :EmptyObjectValue
    | :EmptyObjectPair
    | :MissingPairValue
);

const is_json_whitespace = (c :: &Char) -> Bool => (
    c^ == ' ' or c^ == '\n' or c^ == '\r' or c^ == '\t'
);

const Pos = newtype {
    .line :: Int32,
    .col :: Int32,
    .byte :: Int32,
};

impl Pos as ToString = {
    .to_string = { .line, .col, ... } => (String.to_string(line) + ":" + String.to_string(col)),
};

const ErrorPos = newtype {
    .err :: Error,
    .pos :: Pos,
};

const Token = newtype (
    | :Null
    | :Bool Bool
    | :Number Number
    | :String String
    | :ArrayOpen # `[`
    | :ArrayClose # `]`
    | :Comma # `,`
    | :ObjectOpen # `{`
    | :ObjectClose # `}`
    | :Colon # `:`
);

const Reader = newtype {
    .ptr :: &String,
    .pos :: Pos,
};

const RaiseError = type ([T :: Type] Error -> T);
const error = @context RaiseError;

impl Reader as module = (
    module:

    const create = (s :: &String) -> Reader => {
        .ptr = s,
        .pos = {
            .line = 1,
            .col = 1,
            .byte = 0,
        },
    };

    const is_eof = (self :: &Reader) -> Bool => self^.pos.byte >= self^.ptr^ |> String.length;

    const next = (self :: &mut Reader) -> Option.t[Char] => (
        if is_eof(&self^) then (
            :None
        ) else (
            let c = self^.ptr^ |> String.at(self^.pos.byte);
            self^.pos.byte += Char.string_encoding_len(c);
            if c == '\n' then (
                self^.pos.line += 1;
                self^.pos.col = 1;
            ) else (
                self^.pos.col += 1;
            );
            :Some c
        )
    );

    const peek = (self :: &Reader) -> Option.t[Char] => if is_eof(self) then (
        :None
    ) else (
        :Some (self^.ptr^ |> String.at(self^.pos.byte))
    );

    const discard_ws = (self :: &mut Reader) => (
        while peek(&self^) is :Some c do (
            if is_json_whitespace(&c) then (
                next(self);
                continue
            ) else (
                break
            )
        )
    );

    const Str = (
        module:

        # parse 4 hex digits, used for unicode escapes in string literals
        const parse_unicode = (reader :: &mut Reader) -> Char => (
            use Option.*;

            let next_hex_digit = (reader :: &mut Reader) -> Option[UInt32] => (
                next(reader) |>
                    and_then(c => Char.is_ascii_alphanumeric(c) |> BoolPlus.then_some(
                        Char.to_digit_radix(c, 16)
                    ))
            );

            unwrap_or_else(
                next_hex_digit(reader) |> and_then(a =>
                next_hex_digit(reader) |> and_then(b =>
                next_hex_digit(reader) |> and_then(c =>
                next_hex_digit(reader) |> and_then(d => :Some (Char.from_code(
                    a * (@eval 16 * 16 * 16) +
                    b * (@eval 16 * 16) +
                    c * 16 +
                    d
                )))))),
                () => (@current error)(:InvalidUnicode)
            )
        );

        # parses a string literal excluding the enclosing double-quotes
        # can unwind to block `next_token` with value `:Error (e :: Error) :: Result.t[Token, Error]`
        const parse = (reader :: &mut Reader) -> String => with_return (
            let mut str = "";

            while peek(&reader^) is :Some c do (
                # `"` encountered without preceding `\`, end string
                if c == '"' then (
                    return str;
                )
                else if c == '\\' then (
                    next(reader); # pop slash

                    let mut consume_next = 1;
                    let esc = if peek(&reader^) is :Some esc then (
                        if esc == '"' or esc == '\\' or esc == '/' then (
                            esc
                        )
                        else if esc == 'b' then '\b'
                        # TODO: Kast doesn't support this escape (\f - formfeed)
                        # else if esc == 'f' then '\f'
                        else if esc == 'n' then '\n'
                        else if esc == 'r' then '\r'
                        else if esc == 't' then '\t'
                        else if esc == 'u' then (
                            consume_next = 0;
                            parse_unicode(reader)
                        )
                        else (@current error)(:InvalidEsc)
                    ) else (
                        (@current error)(:UnexpectedEOF)
                    );
                    for _ in 0..consume_next do next(reader); # pop escape char

                    str = str + StringPlus.of_char(esc);
                )
                else if CharPlus.is_control(&c) then (
                    (@current error)(:InvalidChar);
                )
                else (
                    next(reader);
                    str = str + StringPlus.of_char(c);
                )
            );
            str
        );
    );

    const next_token = (
        self :: &mut Reader
    ) -> Result.t[Token, ErrorPos] => with_return (
        discard_ws(self);

        if peek(&self^) is :Some c then (
            let mut consume_next = 1;
            let try_token :: Result.t[Token, Error] = unwindable token_block (
                with error = ([T] err => unwind token_block (:Error err));

                let token = (
                    if c == '['      then :ArrayOpen
                    else if c == ']' then :ArrayClose
                    else if c == ',' then :Comma
                    else if c == '{' then :ObjectOpen
                    else if c == '}' then :ObjectClose
                    else if c == ':' then :Colon
                    # parse literal string
                    else if c == '"' then (
                        next(self);
                        :String Str.parse(self)
                    )
                    else (
                        # for parsing `null` or `true` or `false`
                        let n_chars_eq = (n, str) => (
                            if (String.length(self^.ptr^) - self^.pos.byte >= n) then (
                                # TODO: if calling `substring` with invalid code-points becomes
                                # illegal, this will need some changes
                                String.substring(self^.ptr^, self^.pos.byte, n) == str
                            ) else (
                                false
                            )
                        );

                        if n_chars_eq(4, "null") then (
                            consume_next = 4;
                            :Null
                        )
                        else if n_chars_eq(4, "true") then (
                            consume_next = 4;
                            :Bool true
                        )
                        else if n_chars_eq(5, "false") then (
                            consume_next = 5;
                            :Bool false
                        )

                        else if parse_number(self) is :Some num then (
                            consume_next = 0;
                            :Number num
                        )
                        else (@current error)(:UnknownForm)
                    )
                );
                for _ in 0..consume_next do next(self);
                :Ok token
            );

            # add current position to error before reporting
            try_token |>
                Result.map_err(err => { .err, .pos = self^.pos })
        ) else (
            :Error { .err = :ImmediateEOF, .pos = self^.pos }
        )
    );
);

const Number = newtype {
    .neg :: Bool,
    .digits :: UInt32,
    .fraction_digits :: Option.t[UInt32],
    .exponent :: Option.t[type {.neg :: Bool, .digits :: UInt32}],
};

impl Number as ToString = {
    .to_string = { .neg, .digits, .fraction_digits, .exponent } => (
        let mut s = "";
        if neg then (
            s += "-";
        );
        s += String.to_string(digits);
        if fraction_digits is :Some digits then (
            s += "." + String.to_string(digits);
        );
        if exponent is :Some { .neg, .digits } then (
            s += "e" + (if neg then "-" else "+") + String.to_string(digits);
        );
        s
    )
};

impl Number as module = (
    module:

    const peek = Reader.peek;

    const next = Reader.next;

    const collect_zero_or_more_digits = (reader :: &mut Reader, num :: &mut UInt32) => (
        while peek(&reader^) is :Some c do (
            if not Char.is_ascii_digit(c) then break;
            next(reader);
            let digit = Char.to_digit(c);
            num^ = num^*10 + digit;
        )
    );

    const collect_one_or_more_digits = (reader :: &mut Reader) -> Option.t[UInt32] => with_return (
        peek(&reader^) |>
            Option.and_then(c => (
                if not Char.is_ascii_digit(c) then return :None;
                next(reader);

                let mut num = Char.to_digit(c);
                collect_zero_or_more_digits(reader, &mut num);
                :Some num
            ))
    );

    # parse whether the JSON number is negative (has a leading `-`)
    # returns :None if this doesn't resemble a number
    const parse_negativeness = (reader :: &mut Reader) -> Option.t[Bool] => (
        peek(&reader^) |>
            Option.map(c => if c == '-' then (
                next(reader);
                true
            ) else (
                false
            ))
    );

    # parse the digits part of the JSON number
    # returns :None if this doesn't resemble a number
    const parse_digits = (reader :: &mut Reader) -> Option.t[UInt32] => (
        peek(&reader^) |>
            Option.and_then(c => with_return (
                if not Char.is_ascii_digit(c) then return :None;
                next(reader);
                let mut num = Char.to_digit(c);

                :Some (
                    if num == 0 then (
                        if peek(&reader^) is :Some c then (
                            if Char.is_ascii_digit(c) then (@current error)(:LeadingZero);
                        );
                        0
                    ) else (
                        collect_zero_or_more_digits(reader, &mut num);
                        num
                    )
                )
            ))
    );

    # parse the fractional part of the JSON number
    # returns :None if there is no fractional part present (no '.')
    const parse_fractional = (reader :: &mut Reader) -> typeof ((_ :: Number).fraction_digits) => (
        peek(&reader^) |> Option.and_then(c => if c == '.' then (
            next(reader);

            if collect_one_or_more_digits(reader) is :Some num then (
                :Some num
            ) else (
                (@current error)(:NoDigitsAfterDecimal)
            )
        ) else (
            :None
        ))
    );

    # parse the exponent part of the JSON number
    # returns :None if there is no exponent part present (no 'e' or 'E')
    const parse_exponent = (reader :: &mut Reader) -> typeof ((_ :: Number).exponent) => (
        peek(&reader^) |> Option.and_then(c => if c == 'e' or c == 'E' then (
            next(reader);

            let is_neg = match peek(&reader^) with (
                | :Some c => (
                    if c == '-' then (
                        next(reader);
                        true
                    ) else if c == '+' then (
                        next(reader);
                        false
                    ) else (
                        false
                    )
                )
                | :None => false
            );

            if collect_one_or_more_digits(reader) is :Some num then (
                :Some { .neg = is_neg, .digits = num }
            ) else (
                (@current error)(:NoDigitsAfterExp)
            )
        ) else (
            :None
        ))
    );

    # parse a single `Number` from a `lib.Reader`
    # can unwind to block `next_token` with value `:Error (e :: Error) :: Result.t[Token, Error]`
    const parse = (reader :: &mut Reader) -> Option.t[Number] => (
        use Option.*;

        parse_negativeness(reader) |> map(neg => (
            parse_digits(reader) |> map_or_else(
                () => (@current error)(:MissingDigitsPart),
                digits => (
                    let fraction_digits = parse_fractional(reader);
                    let exponent = parse_exponent(reader);
                    {
                        .neg,
                        .digits,
                        .fraction_digits,
                        .exponent,
                    }
                )
            )
        ))
    );
);

const parse_number = Number.parse;

const Pair = newtype { String, Value };

const Value = newtype (
    | :Null
    | :Bool Bool
    | :Number Number
    | :String String
    | :Array List.t[Value]
    | :Object List.t[Pair]
);

impl Value as ToString = {
    .to_string = value => match value with (
        | :Null => "null"
        | :Bool b => if b then "true" else "false"
        | :Number num => String.to_string(num)
        | :String str => "\"" + str + "\""
        | :Array ref values => (
            if List.is_empty(values) then "[]"
            else (
                let mut str = "[" + (Value as ToString).to_string((values |> List.at(0))^);
                for i in 0..List.length(values) do (
                    let value = values |> List.at(i);
                    str += "," + (Value as ToString).to_string(value^);
                );
                str + "]"
            )
        )
        | :Object ref pairs => (
            if List.is_empty(pairs) then "{}"
            else (
                let { first_key, first_value } = List.at(pairs, 0)^;
                let mut str = "{\"" + first_key + "\":" + (Value as ToString).to_string(first_value);
                for i in 0..List.length(pairs) do (
                    let { key, value } = List.at(pairs, i)^;
                    str += "," + "\"" + key + "\":" + (Value as ToString).to_string(value);
                );
                str + "}"
            )
        )
    )
};

impl Value as module = (
    module:

    # if token is equivalent to a value (primitives null, bool, string, num), construct equivalent
    # value, else :None
    const from_value = (token :: Token) -> Option.t[Value] => (
        match token with (
            | :Null => :Some :Null
            | :Bool b => :Some :Bool b
            | :Number num => :Some :Number num
            | :String str => :Some :String str
            | _ => :None
        )
    );
);

const Context = newtype (
    | :Array {
        List.t[Value],
        .expecting_comma :: Bool
    }
    | :Object {
        List.t[Pair],
        .key :: Option.t[String],
        .expecting_comma_or_colon :: Bool
    }
);

impl Context as module = (
    module:

    const push_value = (context :: &mut Context, value :: Value) => (
        match context^ with (
            | :Array { ref mut arr, ... } => arr |> List.push_back(value)
            | :Object { ref mut pairs, .key = ref mut maybe_key, ... } => (
                if maybe_key^ is :Some ref mut key then (
                    pairs |> List.push_back({ key^, value });
                    maybe_key^ = :None;
                ) else if value is :String str then (
                    maybe_key^ = :Some str;
                )
                else (
                    (@current error)(:NonStrObjectKey)
                )
            )
        )
    );

    const new_array = () -> Context => :Array {
        List.new(),
        .expecting_comma = false,
    };

    const new_object = () -> Context => :Object {
        List.new(),
        .key = :None,
        .expecting_comma_or_colon = false,
    };

    const as_array = (ctx :: Context) -> Option.t[type {
        List.t[Value], .expecting_comma :: Bool
    }] => match ctx with (
        | :Array arr => :Some arr
        | _ => :None
    );

    const as_object = (ctx :: Context) -> Option.t[type {
        List.t[Pair], .key :: Option.t[String], .expecting_comma_or_colon :: Bool
    }] => match ctx with (
        | :Object obj => :Some obj
        | _ => :None
    );

    const as_object_mut = (ctx :: &mut Context) -> Option.t[type (&mut {
        List.t[Pair], .key :: Option.t[String], .expecting_comma_or_colon :: Bool
    })] => match ctx^ with (
        | :Object ref mut obj => :Some obj
        | _ => :None
    );
);

const parse = (reader :: &mut Reader) -> Result.t[Value, ErrorPos] => with_return (
    let ok = val => return :Ok val;
    let error = err => return :Error { .err, .pos = reader^.pos };

    let mut ctxs :: List.t[Context] = List.new();
    loop (
        let token = match Reader.next_token(reader) with (
            | :Ok token => token
            | :Error err => return :Error err
        );
        let last_ctx = List.last_mut(&mut ctxs);
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
                                    List.pop_back |>
                                    Context.as_array |>
                                    Option.expect("impossible, :Array match arm");
                                arr
                            );
                            if &mut ctxs |> List.last_mut is :Some ctx then (
                                ctx |> Context.push_value(arr)
                            ) else (
                                ok(arr);
                            )
                        )
                        | :Comma => (
                            expecting_comma^ = false;
                        )
                        | _ => error(:ExpectingComma)
                    )
                ) else (
                    match token with (
                        | :Null => arr |> List.push_back(:Null)
                        | :Bool b => arr |> List.push_back(:Bool b)
                        | :Number num => arr |> List.push_back(:Number num)
                        | :String str => arr |> List.push_back(:String str)
                        | :ArrayOpen => (
                            # drop `ctx` here
                            &mut ctxs |> List.push_back(Context.new_array())
                        )
                        | :ArrayClose => if &arr^ |> List.is_empty then (
                            # drop `ctx` here

                            # SAFETY: `list_pop` won't panic because `ctxs` is not empty because
                            # `last_ctx` is :Some
                            let arr = :Array (
                                let { arr, ... } = &mut ctxs |>
                                    List.pop_back |>
                                    Context.as_array |>
                                    Option.expect("impossible, :Array match arm");
                                arr
                            );
                            if &mut ctxs |> List.last_mut is :Some ctx then (
                                ctx |> Context.push_value(arr)
                            ) else (
                                ok(arr);
                            )
                        ) else (
                            error(:ExpectingValue)
                        )
                        | :Comma => error(:EmptyArrayElem)
                        | :ObjectOpen => (
                            # drop `ctx` here
                            &mut ctxs |> List.push_back(Context.new_object())
                        )
                        | :ObjectClose => error(:MismatchedObjectClose)
                        | :Colon => error(:UnexpectedColon)
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
                                    List.pop_back |>
                                    Context.as_object |>
                                    Option.expect("impossible, :Object match arm");
                                obj
                            );
                            if &mut ctxs |> List.last_mut is :Some ctx then (
                                ctx |> Context.push_value(obj)
                            ) else (
                                ok(obj);
                            )
                        )
                        | :Comma => (
                            expecting_comma^ = false
                        )
                        | _ => error(:ExpectingComma)
                    )
                ) else (
                    match token with (
                        | :String str => (
                            let obj_ctx = ctx |>
                                Context.as_object_mut |>
                                Option.expect(":Object match arm");
                            obj_ctx^.key = :Some str;

                            let expecting_colon = expecting_comma;
                            expecting_colon^ = true;
                        )
                        | :ObjectClose => if &obj^ |> List.is_empty then (
                            # drop `ctx` here

                            # SAFETY: `list_pop` won't panic because `ctxs` is not empty because
                            # `last_ctx` is :Some
                            let obj = :Object (
                                let { obj, ... } = &mut ctxs |>
                                    List.pop_back |>
                                    Context.as_object |>
                                    Option.expect("impossible, :Object match arm");
                                obj
                            );
                            if &mut ctxs |> List.last_mut is :Some ctx then (
                                ctx |> Context.push_value(obj)
                            ) else (
                                ok(obj);
                            )
                        ) else (
                            error(:ExpectingValue)
                        )
                        | :Comma => error(:EmptyObjectPair)
                        | :ArrayClose => error(:MismatchedArrayClose)
                        | :Colon => error(:EmptyObjectKey)
                        | _ => error(:NonStrObjectKey)
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
                        | :ObjectClose => error(:MissingPairValue)
                        | :Colon => (
                            expecting_colon^ = false
                        )
                        | _ => error(:ExpectingColon)
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
                            obj |> List.push_back(pair)
                        )
                        | :Bool b => (
                            # drop `ctx` here
                            let pair :: Pair = { take_key(ctx), :Bool b };
                            obj |> List.push_back(pair)
                        )
                        | :Number num => (
                            # drop `ctx` here
                            let pair :: Pair = { take_key(ctx), :Number num };
                            obj |> List.push_back(pair)
                        )
                        | :String str => (
                            # drop `ctx` here
                            let pair :: Pair = { take_key(ctx), :String str };
                            obj |> List.push_back(pair)
                        )
                        | :ArrayOpen => (
                            # drop `ctx` here
                            &mut ctxs |> List.push_back(Context.new_array())
                        )
                        | :ArrayClose => error(:MismatchedArrayClose)
                        | :Comma => error(:EmptyObjectValue)
                        | :ObjectOpen => (
                            # drop `ctx` here
                            &mut ctxs |> List.push_back(Context.new_object())
                        )
                        | :ObjectClose => if &obj^ |> List.is_empty then (
                            # drop `ctx` here

                            # SAFETY: `list_pop` won't panic because `ctxs` is not empty because
                            # `last_ctx` is :Some
                            let obj = :Object (
                                let { obj, ... } = &mut ctxs |>
                                    List.pop_back |>
                                    Context.as_object |>
                                    Option.expect("impossible, :Object match arm");
                                obj
                            );
                            if &mut ctxs |> List.last_mut is :Some ctx then (
                                ctx |> Context.push_value(obj)
                            ) else (
                                ok(obj);
                            )
                        ) else (
                            error(:ExpectingValue)
                        )
                        | :Colon => error(:UnexpectedColon)
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
                | :String str => ok(:String str)
                | :ArrayOpen => (
                    &mut ctxs |> List.push_back(Context.new_array())
                )
                | :ArrayClose => error(:MismatchedArrayClose)
                | :Comma => error(:UnexpectedComma)
                | :ObjectOpen => (
                    &mut ctxs |> List.push_back(Context.new_object())
                )
                | :ObjectClose => error(:MismatchedObjectClose)
                | :Colon => error(:UnexpectedColon)
            )
        )
    );
    panic("unreachable")
);
