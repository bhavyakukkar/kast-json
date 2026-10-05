const __private__ = (
module:

use std.*;
use std.fmt.write;
use (import "stdplus.ks").*;

# Notes
# - a variant of Iterable that can be stopped when you don't want it to iterate any more would be
#   helpful

const PRETTY_PRINTER_INDENT :: &str = "    ";

## non-exhaustive
const Error = newtype (
    | :ImmediateEOF
    | :UnexpectedEOF
    | :UnknownForm
    | :LeadingZero
    | :NoDigitsAfterDecimal
    | :NoDigitsAfterExp
    | :MissingDigitsPart
    | :InvalidChar
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
    | :TrailingCommaInArray
    | :TrailingCommaInObject
    | :EmptyObjectKey
    | :EmptyObjectValue
    | :EmptyObjectPair
    | :MissingPairValue
    | :TrailingChars
);

impl Error as ToString = {
    .to_string = err => String.from_str(match err with (
        | :ImmediateEOF => "unexpected end-of-file"
        | :UnexpectedEOF => "unexpected end-of-file"
        | :UnknownForm => "unknown form: neither null|true|false|number|string|array|object"
        | :LeadingZero => "leading zeroes in numbers are not allowed"
        | :NoDigitsAfterDecimal => "no digits found after decimal point in number"
        | :NoDigitsAfterExp => "no digits found after exponent symbol `e` in number"
        | :MissingDigitsPart => "no digit found after minus `-` in number"
        | :InvalidChar => "control characters are not allowed"
        | :InvalidUnicode => "invalid unicode sequence"
        | :InvalidEsc => "invalid escape sequence"
        | :MismatchedArrayClose => "unexpected array close character `]` found"
        | :UnexpectedComma => "unexpected comma found"
        | :ExpectingComma => "expected comma was not found"
        | :EmptyArrayElem => "array elements may not be empty"
        | :NonStrObjectKey => "object keys must be json strings"
        | :MismatchedObjectClose => "unexpected object close character `}` found"
        | :UnexpectedColon => "unexpected colon found"
        | :ExpectingColon => "expected colon was not found"
        | :ExpectingValue => "expecting value after colon in object"
        | :TrailingCommaInArray => "trailing commas are not allowed in arrays"
        | :TrailingCommaInObject => "trailing commas are not allowed in objects"
        | :EmptyObjectKey => "skipping object keys is not allowed"
        | :EmptyObjectValue => "skipping object values is not allowed"
        | :EmptyObjectPair => "object pairs may not be empty"
        | :MissingPairValue => "missing object value before closing object"
        | :TrailingChars => "trailing characters found after value"
    ))
};

const is_json_whitespace = (c :: &Char) -> Bool => (
    c^ == ' ' or c^ == '\n' or c^ == '\r' or c^ == '\t'
);

const Pos = newtype {
    .line :: Int32,
    .col :: Int32,
    .byte :: Int32,
};

impl Pos as ToString = {
    .to_string = { .line, .col, ... } => format!("\(line):\(col)"),
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
    .ptr :: &str,
    .pos :: Pos,
};

const RaiseError = type (Error -> Never);
const error = @context RaiseError;

impl Reader as module = (
    module:

    const create = (s :: &str) -> Reader => {
        .ptr = s,
        .pos = {
            .line = 1,
            .col = 1,
            .byte = 0,
        },
    };

    const is_eof = (self :: &Reader) -> Bool => (
        self^.pos.byte >= self^.ptr |> String.length
    );

    const next = (self :: &mut Reader) -> Option.t[Char] => (
        if is_eof(&self^) then (
            :None
        ) else (
            let c = self^.ptr |> String.at(self^.pos.byte);
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
        :Some (self^.ptr |> String.at(self^.pos.byte))
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

        ## parse 4 hex digits as a codepoint, used for unicode escapes in string literals
        const parse_unicode = (reader :: &mut Reader) -> Char => (
            use Option.*;

            let next_hex_digit = (reader :: &mut Reader) -> Option[UInt32] => (
                next(reader) |>
                    and_then(c => unwindable hex_digit (
                        with PanicHandler = {
                            .handle = _ => unwind hex_digit :None,
                        };
                        # `to_digit_radix` panics for invalid digits
                        :Some Char.to_digit_radix(c, 16)
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
                () => (
                    (@current error)(:InvalidUnicode) |> from_never
                )
            )
        );

        ## parse contents of a string literal (excluding the enclosing double-quotes)
        const parse = (reader :: &mut Reader) -> String => with_return (
            let mut result = StringBuilder.new();

            while peek(&reader^) is :Some c do (
                # `"` encountered without preceding `\`, end string
                if c == '"' then (
                    return result |> StringBuilder.into_string;
                )
                else if c == '\\' then (
                    next(reader); # pop slash

                    let mut consume_next = 1;
                    let esc = if peek(&reader^) is :Some esc then (
                        if esc == '"' or esc == '\\' or esc == '/' then (
                            esc
                        )
                        else if esc == 'b' then '\b'
                        else if esc == 'f' then '\f'
                        else if esc == 'n' then '\n'
                        else if esc == 'r' then '\r'
                        else if esc == 't' then '\t'
                        else if esc == 'u' then (
                            consume_next = 0;
                            next(reader);
                            parse_unicode(reader)
                        )
                        else (
                            (@current error)(:InvalidEsc) |> from_never
                        )
                    ) else (
                        (@current error)(:UnexpectedEOF) |> from_never
                    );
                    for _ in 0..consume_next do next(reader); # pop escape char

                    &mut result |> StringBuilder.add_String(StringPlus.of_char(esc));
                )
                else if CharPlus.is_control(&c) then (
                    (@current error)(:InvalidChar);
                )
                else (
                    next(reader);
                    &mut result |> StringBuilder.add_String(StringPlus.of_char(c));
                )
            );
            result |> StringBuilder.into_string
        );
    );

    const next_token = (
        self :: &mut Reader
    ) -> Result.t[Token, ErrorPos] => with_return (
        discard_ws(self);

        if peek(&self^) is :Some c then (
            let mut consume_next :: Int32 = 1;
            let try_token :: Result.t[Token, Error] = unwindable token_block (
                with error = (err => unwind token_block (:Error err));

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
                        let n_chars_eq = (n, s) => (
                            if (String.length(self^.ptr) - self^.pos.byte >= n) then (
                                # TODO: if calling `substring` with invalid code-points becomes
                                # illegal, this will need some changes
                                String.substring(self^.ptr, self^.pos.byte, n) == s
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
                        else (
                            (@current error)(:UnknownForm) |> from_never
                        )
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
    .digits :: String,
    .fraction_digits :: String, # empty string === no fractional part
    .exponent :: {.neg :: Bool, .digits :: String}, # empty `.digits` string === no exponent part
};

impl Number as ToString = {
    .to_string = { .neg, .digits, .fraction_digits, .exponent } => (
        let mut s = StringBuilder.new();
        if neg then (
            write!(&mut s, "-");
        );
        &mut s |> StringBuilder.add_String(digits);
        if not (&fraction_digits |> as_str |> StringPlus.is_empty) then (
            write!(&mut s, ".\(&fraction_digits |> as_str)");
        );
        if not (&exponent.digits |> as_str |> StringPlus.is_empty) then (
            write!(&mut s, "E\(if exponent.neg then "-" else "+")\(&exponent.digits |> as_str)");
        );
        s |> StringBuilder.into_string
    )
};

const IntoNumber = [Self] newtype {
    .into :: Self -> Number,
};

impl UInt32 as IntoNumber = {
    .into = mut num => (
        let mut digits = StringBuilder.new();
        while num > 0 do (
            &mut digits |> StringBuilder.add_String(num % 10 |> Char.from_digit |> StringPlus.of_char);
            num = num / 10;
        );
        let digits = StringPlus.rev(&(digits |> StringBuilder.into_string) |> as_str);
        {
            .digits,
            .neg = false,
            .fraction_digits = String.from_str(""),
            .exponent = { .neg = default(), .digits = String.from_str("") }
        }
    ),
};

impl Number as module = (
    module:

    const peek = Reader.peek;

    const next = Reader.next;

    ## convert (losslessly but without failure) a JSON number to a Float64
    const to_f64 = (&{
        .neg,
        .digits = ref digits,
        .fraction_digits = ref fraction_digits,
        .exponent = ref exponent,
    } :: type (&Number)) -> Float64 => (
        # consider digits
        let mut f = 0;
        for c in digits |> as_str |> String.iter do (
            f = f*10.0 + CharPlus.parse[Float64](c);
        );

        # consider fraction-digits
        let mut mult = 0.1;
        for c in fraction_digits |> as_str |> String.iter do (
            f += CharPlus.parse[Float64](c) * mult;
            mult *= 0.1;
        );

        # consider sign
        if neg then (
            f = -f;
        );

        if not (&exponent^.digits |> as_str |> StringPlus.is_empty) then (
            # consider exponent
            let exp = String.parse[UInt32](&exponent^.digits |> as_str);
            for i in 0..exp do (
                f *= if exponent^.neg then 0.1 else 10;
            )
        );
        
        f
    );

    const try_u32 = (&{
        .neg,
        .digits = ref digits,
        .fraction_digits = ref fraction_digits,
        .exponent = ref exponent,
    } :: &Number) -> Result.t[UInt32, String] => (
        if neg then (
            :Error String.from_str("Negative JSON number cannot be converted to UInt32")
        )
        else if String.length(fraction_digits |> as_str) > 0 then (
            :Error String.from_str("JSON number with fractional part cannot be converted to UInt32")
        )
        else if String.length(&exponent^.digits |> as_str) > 0 then (
            :Error String.from_str("JSON number with exponent part cannot be converted to UInt32")
        )
        else (
            unwindable parse_uint32 (
                with PanicHandler = {
                    .handle = msg => unwind parse_uint32 :Error String.from_str(msg),
                };
                :Ok String.parse[UInt32](digits |> as_str)
            )
        )
    );

    ## returns the first character of the JSON number if the reader is in-fact pointing at a JSON number
    const begin_json_number = (reader :: &Reader) -> Option.t[Char] => (
        peek(&reader^) |> Option.and_then(c => (c == '-' or Char.is_ascii_digit(c)) |> BoolPlus.then_some(c))
    );

    ## parse whether the JSON number is negative, based on whether the provided first-character is a minus (`-`)
    ##
    ## NOTE: expects that the reader is already pointing at a JSON number i.e. `Number.is_number(&reader)` is true
    const parse_negativeness = (first_char :: Char, reader :: &mut Reader) -> Bool => (
        if first_char == '-' then (
            next(reader);
            true
        ) else (
            false
        )
    );

    ## parse the digits part of the JSON number
    const parse_digits = (reader :: &mut Reader) -> String => (
        let error = @current error;

        let first_digit = next(reader)
            |> Option.and_then(c => c |> Char.is_ascii_digit |> BoolPlus.then_some(c))
            |> Option.unwrap_or_else(() => (
                error(:MissingDigitsPart) |> from_never
            ));

        if first_digit == '0' then (
            if peek(&reader^) |> Option.is_some_and(Char.is_ascii_digit) then (
                error(:LeadingZero) |> from_never
            ) else (
                String.from_str("0")
            )
        ) else (
            let mut digits = StringPlus.of_char(first_digit);
        
            while peek(&reader^) |> Option.is_some_and(Char.is_ascii_digit) do (
                digits = String.concat_owned(
                    digits,
                    next(reader) |> Option.expect("peek was :Some") |> StringPlus.of_char,
                );
            );

            digits
        )
    );

    ## parse the optional fractional part of the JSON number
    const parse_fractional = (reader :: &mut Reader) -> typeof ((_ :: Number).fraction_digits) => (
        if peek(&reader^) |> Option.is_some_and(c => c == '.') then (
            next(reader);

            let mut digits = next(reader)
                |> Option.and_then(c => c |> Char.is_ascii_digit |> BoolPlus.then_some(c))
                |> Option.unwrap_or_else(() => ((@current error)(:NoDigitsAfterDecimal)) |> from_never)
                |> StringPlus.of_char;

            while peek(&reader^) |> Option.is_some_and(Char.is_ascii_digit) do (
                digits = String.concat_owned(
                    digits,
                    next(reader) |> Option.expect("peek was :Some") |> StringPlus.of_char,
                );
            );

            digits
        ) else (
            String.from_str("")
        )
    );

    ## parse the optional exponent part of the JSON number
    const parse_exponent = (reader :: &mut Reader) -> typeof ((_ :: Number).exponent) => (
        if peek(&reader^) |> Option.is_some_and(c => c == 'e' or c == 'E') then (
            next(reader);

            let neg = match peek(&reader^) with (
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

            let mut digits = next(reader)
                |> Option.and_then(c => c |> Char.is_ascii_digit |> BoolPlus.then_some(c))
                |> Option.unwrap_or_else(() => ((@current error)(:NoDigitsAfterExp) |> from_never))
                |> StringPlus.of_char;

            while peek(&reader^) |> Option.is_some_and(Char.is_ascii_digit) do (
                digits = String.concat_owned(
                    digits,
                    next(reader) |> Option.expect("peek was :Some") |> StringPlus.of_char,
                );
            );

            { .neg, .digits }
        ) else (
            { .neg = default(), .digits = String.from_str("") }
        )
    );

    ## parse a JSON number, or return :None if the incoming form doesn't resemble a JSON number
    const parse = (reader :: &mut Reader) -> Option.t[Number] => (
        begin_json_number(&reader^) |> Option.map(first_char => {
            .neg = parse_negativeness(first_char, reader),
            .digits = parse_digits(reader),
            .fraction_digits = parse_fractional(reader),
            .exponent = parse_exponent(reader),
        })
    );

    const eq = (lhs :: &Number, rhs :: &Number) -> Bool => (
        lhs^.neg == rhs^.neg and
        as_str(&lhs^.digits) == as_str(&rhs^.digits) and
        as_str(&lhs^.fraction_digits) == as_str(&rhs^.fraction_digits) and
        lhs^.exponent.neg == rhs^.exponent.neg and
        as_str(&lhs^.exponent.digits) == as_str(&rhs^.exponent.digits)
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

const escape_json_string = (s :: &str) -> String => (
    let mut new = StringBuilder.new();
    write!(&mut new, "\"");
    for c in String.iter(s) do (
        if c == '"' then (
            write!(&mut new, "\\\"");
        ) else if c == '\\' then (
            write!(&mut new, "\\\\");
        ) else if c == '/' then (
            write!(&mut new, "/");
        ) else if c == '\b' then (
            write!(&mut new, "\\b");
        ) else if c == '\f' then (
            write!(&mut new, "\\f");
        ) else if c == '\n' then (
            write!(&mut new, "\\n");
        ) else if c == '\r' then (
            write!(&mut new, "\\r");
        ) else if c == '\t' then (
            write!(&mut new, "\\t");
        )
        # control-characters can only be represented via unicode notation (`\uxxxx`)
        else if CharPlus.is_control(&c) then (
            const pad_four_char_code = (c :: Char) => (
                let s = format!("000\(Char.code(c))");
                let s = &s |> as_str;
                String.from_str(s |> String.substring(String.length(s) - 4, 4))
            );
            write!(&mut new, "\\u\(pad_four_char_code(c))");
        ) else (
            write!(&mut new, "\(StringPlus.of_char(c))");
        )
    );
    write!(&mut new, "\"");
    new |> StringBuilder.into_string
);

impl Value as ToString = {
    .to_string = value => match value with (
        | :Null => String.from_str("null")
        | :Bool b => String.from_str(if b then "true" else "false")
        | :Number num => String.to_string(num)
        | :String ref s => escape_json_string(s |> as_str)
        | :Array ref values => (
            if List.is_empty(values) then String.from_str("[]")
            else (
                let mut s = StringBuilder.new();
                write!(&mut s, "[\(to_string(values^.[0]))");
                for {i, value} in List.iteri(values) do (
                    if i == 0 then continue;
                    write!(&mut s, ",\(to_string(value^))");
                );
                write!(&mut s, "]");
                s |> StringBuilder.into_string
            )
        )
        | :Object ref pairs => (
            if List.is_empty(pairs) then String.from_str("{}")
            else (
                let { ref first_key, ref first_value } = List.at(pairs, 0)^;
                let mut s = StringBuilder.new();
                write!(&mut s, "{\(escape_json_string(first_key |> as_str)):\(to_string(first_value^))");
                for { i, pair } in List.iteri(pairs) do (
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

const PrettyPrinter = newtype {
    .value :: Value,
    .indent :: UInt32,
};

impl PrettyPrinter as ToString = {
    .to_string = { .value, .indent } => match value with (
        | :Null => String.from_str("null")
        | :Bool b => String.from_str(if b then "true" else "false")
        | :Number num => String.to_string(num)
        | :String s => escape_json_string(&s |> as_str)
        | :Array ref values => (
            if List.is_empty(values) then String.from_str("[]")
            else (
                let mut s = StringBuilder.new();
                write!(&mut s, "[\n");
                &mut s |> StringBuilder.add_String((StringPlus.repeat(PRETTY_PRINTER_INDENT, indent + 1)));
                &mut s |> StringBuilder.add_String(
                    (PrettyPrinter as ToString).to_string(
                        { .value = (values |> List.at(0))^, .indent = indent + 1 }
                    )
                );
                for { i, value } in List.iteri(values) do (
                    if i == 0 then continue;
                    write!(&mut s, ",\n");
                    &mut s |> StringBuilder.add_String(StringPlus.repeat(PRETTY_PRINTER_INDENT, indent + 1));
                    &mut s |> StringBuilder.add_String(
                        (PrettyPrinter as ToString).to_string(
                            { .value = value^, .indent = indent + 1 }
                        )
                    );
                );
                write!(&mut s, "\n\(StringPlus.repeat(PRETTY_PRINTER_INDENT, indent))]");
                s |> StringBuilder.into_string
            )
        )
        | :Object ref pairs => (
            let mut s = StringBuilder.new();
            write!(&mut s, "{\n");
            for { i, pair } in List.iteri(pairs) do (
                if i != 0 then (
                    &mut s |> StringBuilder.add_str(",\n");
                );
                let { ref key, ref value } = pair^;
                &mut s |> StringBuilder.add_String(
                    StringPlus.repeat(PRETTY_PRINTER_INDENT, indent + 1)
                );
                &mut s |> StringBuilder.add_String(escape_json_string(key |> as_str));
                &mut s |> StringBuilder.add_str(": ");
                &mut s |> StringBuilder.add_String(
                    (PrettyPrinter as ToString).to_string(
                        { .value = value^, .indent = indent + 1 }
                    )
                );
            );
            &mut s |> StringBuilder.add_str("\n");
            &mut s |> StringBuilder.add_String(
                StringPlus.repeat(PRETTY_PRINTER_INDENT, indent)
            );
            write!(&mut s, "}");
            s |> StringBuilder.into_string
        )
    )
};

impl Value as module = (
    module:

    ## create a JSON pretty-printer of this value
    const pretty_printer = (self :: Value) -> PrettyPrinter => {
        .value = self,
        .indent = 0,
    };

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

# private
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

# private
impl Context as module = (
    module:

    const push_value = (context :: &mut Context, value :: Value) => (
        match context^ with (
            | :Array { ref mut arr, ... } => arr |> List.push_back(value)
            | :Object { ref mut pairs, .key = ref mut maybe_key, ... } => (
                if maybe_key^ is :Some ref mut key then (
                    pairs |> List.push_back({ key^, value });
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

## parse one JSON value from the reader
const parse_one = (reader :: &mut Reader) -> Result.t[Value, ErrorPos] => with_return (
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
                        | _ => error(:ExpectingComma) |> from_never
                    )
                ) else (
                    match token with (
                        | :Null => arr |> List.push_back(:Null)
                        | :Bool b => arr |> List.push_back(:Bool b)
                        | :Number num => arr |> List.push_back(:Number num)
                        | :String s => arr |> List.push_back(:String s)
                        | :ArrayOpen => (
                            expecting_comma^ = true;
                            # drop `ctx` here

                            &mut ctxs |> List.push_back(Context.new_array());
                            continue
                        )
                        | :ArrayClose => if &arr^ |> List.is_empty then (
                            expecting_comma^ = true;
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
                            );
                            continue
                        ) else (
                            error(:TrailingCommaInArray) |> from_never
                        )
                        | :Comma => error(:EmptyArrayElem) |> from_never
                        | :ObjectOpen => (
                            expecting_comma^ = true;
                            # drop `ctx` here

                            &mut ctxs |> List.push_back(Context.new_object());
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
                        | :ObjectClose => if &obj^ |> List.is_empty then (
                            expecting_comma^ = true;
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
                        | :String s => (
                            # drop `ctx` here
                            let pair :: Pair = { take_key(ctx), :String s };
                            obj |> List.push_back(pair)
                        )
                        | :ArrayOpen => (
                            let expecting_comma = expecting_colon;
                            expecting_comma^ = true;
                            # drop `ctx` here

                            &mut ctxs |> List.push_back(Context.new_array());
                            continue
                        )
                        | :ArrayClose => error(:MismatchedArrayClose) |> from_never
                        | :Comma => error(:EmptyObjectValue) |> from_never
                        | :ObjectOpen => (
                            let expecting_comma = expecting_colon;
                            expecting_comma^ = true;
                            # drop `ctx` here

                            &mut ctxs |> List.push_back(Context.new_object());
                            continue
                        )
                        | :ObjectClose => if &obj^ |> List.is_empty then (
                            let expecting_comma = expecting_colon;
                            expecting_comma^ = true;
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
                    &mut ctxs |> List.push_back(Context.new_array())
                )
                | :ArrayClose => error(:MismatchedArrayClose) |> from_never
                | :Comma => error(:UnexpectedComma) |> from_never
                | :ObjectOpen => (
                    &mut ctxs |> List.push_back(Context.new_object())
                )
                | :ObjectClose => error(:MismatchedObjectClose) |> from_never
                | :Colon => error(:UnexpectedColon) |> from_never
            )
        )
    );
    panic("unreachable")
);

## parse the source-string as an individual JSON value
const parse_one_total = (source :: &str) -> Result.t[Value, ErrorPos] => with_return (
    let mut reader = Reader.create(source);
    parse_one(&mut reader) |> Result.and_then(value => (
        Reader.discard_ws(&mut reader);
        if Reader.peek(&reader) |> Option.is_some_and(c => not is_json_whitespace(&c))
        then :Error { .err = :TrailingChars, .pos = reader.pos }
        else :Ok value
    ))
);

);

# Public API
(
module:

use __private__.Error;
use __private__.Pos;
use __private__.ErrorPos;
use __private__.Token;
use __private__.Reader;
use __private__.Number;
use __private__.Pair;
use __private__.Value;
use __private__.PrettyPrinter;
use __private__.parse_one;
const parse = __private__.parse_one; # for backwards compatibility
use __private__.parse_one_total;
use __private__.escape_json_string;
use __private__.error;
use __private__.IntoNumber;
)
