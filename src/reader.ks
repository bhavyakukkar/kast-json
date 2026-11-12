const {
    .int32_to_uint32,
    .is_control_char,
    .uint32_to_int32,
    .string_of_char,
    .then_some,
    .Option,
    ...
} = import "./utils.ks";
const {.error, .Error, .Pos, ...} = import "./error.ks";
use std.StringBuilder;

module:

use (import "./token.ks").Token;
use (import "./number.ks").Number;
use (import "./error.ks").ErrorPos;

const Reader = newtype {
    .ptr :: &str,
    .pos :: Pos,
};

impl Reader as module = (
    module:

    const is_json_whitespace = (c :: &Char) -> Bool => (
        c^ == ' ' or c^ == '\n' or c^ == '\r' or c^ == '\t'
    );

    const new = (s :: &str) -> Reader => {
        .ptr = s,
        .pos = {
            .line = 1,
            .col = 1,
            .byte = 0,
        },
    };

    # for backwards-compatibility
    const create = new;

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

    ## parse contents of a string literal (excluding the enclosing double-quotes)
    ## NOTE: requires `@current error` to be defined
    const parse_string_contents = (reader :: &mut Reader) -> String => with_return (

        ## parse 4 hex digits as a codepoint, used for unicode escapes in string literals
        const parse_unicode = (reader :: &mut Reader) -> Char => (
            let next_hex_digit = (reader :: &mut Reader) -> Option.t[Int32] => (
                next(reader) |>
                    Option.and_then(c => unwindable hex_digit (
                        with std.PanicHandler = {
                            .handle = _ => unwind hex_digit :None,
                        };
                        # `to_digit_radix` panics for invalid digits
                        :Some Char.to_digit_radix(c, 16)
                    ))
            );

            const and_then = Option.and_then;
            Option.unwrap_or_else(
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

        let mut result = StringBuilder.new();

        while peek(&reader^) is :Some c do (
            # `"` encountered without preceding `\`, end string
            if c == '"' then (
                return result |> StringBuilder.into_string;
            )
            else if c == '\\' then (
                next(reader); # pop slash

                let mut consume_next :: Int32 = 1;
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

                &mut result |> StringBuilder.add_String(string_of_char(esc));
            )
            else if is_control_char(&c) then (
                (@current error)(:InvalidChar);
            )
            else (
                next(reader);
                &mut result |> StringBuilder.add_String(string_of_char(c));
            )
        );
        result |> StringBuilder.into_string
    );

    ## parse a JSON number, or return :None if the incoming form doesn't resemble a JSON number
    ## NOTE: requires `@current error` to be defined
    const parse_number = (reader :: &mut Reader) -> Option.t[Number] => (

        ## returns the first character of the JSON number if the reader is in-fact pointing at a JSON number
        const begin_json_number = (reader :: &Reader) -> Option.t[Char] => (
            peek(&reader^) |> Option.and_then(c => (c == '-' or Char.is_ascii_digit(c)) |> then_some(c))
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
                |> Option.and_then(c => c |> Char.is_ascii_digit |> then_some(c))
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
                let mut digits = string_of_char(first_digit);
            
                while peek(&reader^) |> Option.is_some_and(Char.is_ascii_digit) do (
                    digits = String.concat_owned(
                        digits,
                        next(reader) |> Option.expect("peek was :Some") |> string_of_char,
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
                    |> Option.and_then(c => c |> Char.is_ascii_digit |> then_some(c))
                    |> Option.unwrap_or_else(() => ((@current error)(:NoDigitsAfterDecimal)) |> from_never)
                    |> string_of_char;

                while peek(&reader^) |> Option.is_some_and(Char.is_ascii_digit) do (
                    digits = String.concat_owned(
                        digits,
                        next(reader) |> Option.expect("peek was :Some") |> string_of_char,
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
                    |> Option.and_then(c => c |> Char.is_ascii_digit |> then_some(c))
                    |> Option.unwrap_or_else(() => ((@current error)(:NoDigitsAfterExp) |> from_never))
                    |> string_of_char;

                while peek(&reader^) |> Option.is_some_and(Char.is_ascii_digit) do (
                    digits = String.concat_owned(
                        digits,
                        next(reader) |> Option.expect("peek was :Some") |> string_of_char,
                    );
                );

                { .neg, .digits }
            ) else (
                { .neg = false, .digits = String.from_str("") }
            )
        );

        begin_json_number(&reader^) |> Option.map(first_char => {
            .neg = parse_negativeness(first_char, reader),
            .digits = parse_digits(reader),
            .fraction_digits = parse_fractional(reader),
            .exponent = parse_exponent(reader),
        })
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
                        :String parse_string_contents(self)
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

