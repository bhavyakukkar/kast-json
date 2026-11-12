module:

const RaiseError = type (Error -> Never);
const error = @context RaiseError;

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

