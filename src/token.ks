module:

use (import "./number.ks").Number;

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

