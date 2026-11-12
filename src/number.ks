const {
    .parse_char,
    .string_is_empty,
    .string_of_char,
    .string_rev,
    .uint32_to_int32,
    ...
} = import "./utils.ks";
use std.PanicHandler;
use std.StringBuilder;
use std.fmt.write;

module:

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
        if not (&fraction_digits |> as_str |> string_is_empty) then (
            write!(&mut s, ".\(&fraction_digits |> as_str)");
        );
        if not (&exponent.digits |> as_str |> string_is_empty) then (
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
            &mut digits |> StringBuilder.add_String(
                num % 10 |> uint32_to_int32 |> Char.from_digit |> string_of_char
            );
            num = num / 10;
        );
        let digits = string_rev(&(digits |> StringBuilder.into_string) |> as_str);
        {
            .digits,
            .neg = false,
            .fraction_digits = String.from_str(""),
            .exponent = { .neg = false, .digits = String.from_str("") }
        }
    ),
};

impl Number as module = (
    module:

    const clone = (&{
        .neg,
        .digits = ref digits,
        .fraction_digits = ref fraction_digits,
        .exponent = ref exponent,
    } :: type (&Number)) -> Number => {
        .neg,
        .digits = digits |> as_str |> String.from_str,
        .fraction_digits = fraction_digits |> as_str |> String.from_str,
        .exponent = {
            .neg = exponent^.neg,
            .digits = &exponent^.digits |> as_str |> String.from_str,
        },
    };

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
            f = f*10.0 + parse_char[Float64](c);
        );

        # consider fraction-digits
        let mut mult = 0.1;
        for c in fraction_digits |> as_str |> String.iter do (
            f += parse_char[Float64](c) * mult;
            mult *= 0.1;
        );

        # consider sign
        if neg then (
            f = -f;
        );

        if not (&exponent^.digits |> as_str |> string_is_empty) then (
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

    const eq = (lhs :: &Number, rhs :: &Number) -> Bool => (
        lhs^.neg == rhs^.neg and
        as_str(&lhs^.digits) == as_str(&rhs^.digits) and
        as_str(&lhs^.fraction_digits) == as_str(&rhs^.fraction_digits) and
        lhs^.exponent.neg == rhs^.exponent.neg and
        as_str(&lhs^.exponent.digits) == as_str(&rhs^.exponent.digits)
    );
);

