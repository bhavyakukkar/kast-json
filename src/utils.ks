module:

# First and last C0 Control characters
const CHAR_NUL :: Char = (@eval Char.from_code(0));
const CHAR_US :: Char = (@eval Char.from_code(31));

# First and last C1 Control characters
const CHAR_DEL :: Char = (@eval Char.from_code(127));
const CHAR_APC :: Char = (@eval Char.from_code(159));

const is_control_char = (c :: &Char) -> Bool => (
    let c = Char.code(c^);
    # C0 control codes
    (c >= (@eval Char.code(CHAR_NUL)) and c <= (@eval Char.code(CHAR_US))) or
        # C1 control codes
        (c >= (@eval Char.code(CHAR_DEL)) and c <= (@eval Char.code(CHAR_APC)))
);

const escape_json_string = (s :: &str) -> String => (
    use std.fmt.write;
    use std.StringBuilder;

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
        else if is_control_char(&c) then (
            const pad_four_char_code = (c :: Char) => (
                let s = format!("000\(Char.code(c))");
                let s = &s |> as_str;
                String.from_str(s |> String.substring(String.length(s) - 4, 4))
            );
            write!(&mut new, "\\u\(pad_four_char_code(c))");
        ) else (
            write!(&mut new, "\(string_of_char(c))");
        )
    );
    write!(&mut new, "\"");
    new |> StringBuilder.into_string
);

const string_of_char = (c :: Char) => (Char as ToString).to_string(c);

const parse_char = [T] (c :: Char) -> T => (
    String.parse[T](&string_of_char(c) |> as_str)
);

const string_is_empty = (s :: &str) => String.length(s) == 0;

const string_rev = (s :: &str) -> String => (
    use std.fmt.write;
    use std.StringBuilder;

    let mut new = StringBuilder.new();
    for i in 0..String.length(s) do (
        write!(&mut new, "\(String.at(s, String.length(s) - i - 1) |> string_of_char)");
    );
    new |> StringBuilder.into_string
);

const string_repeat = (self :: &str, times :: UInt32) -> String => (
    use std.StringBuilder;

    if times == 0 then (
        String.from_str("")
    ) else (
        let mut new_str = StringBuilder.new();
        for _ in 0..times do (
            &mut new_str |> StringBuilder.add_str(self);
        );
        new_str |> StringBuilder.into_string
    )
);

const uint32_to_int32 = (i :: UInt32) -> Int32 => (
    let s = i |> String.to_string;
    &s |> String.as_str |> String.parse
);

const int32_to_uint32 = (i :: Int32) -> UInt32 => (
    let s = i |> String.to_string;
    &s |> String.as_str |> String.parse
);

const then_some = [T] (b :: Bool, t :: T) -> Option.t[T] => (
    if b then (
        :Some t
    ) else (
        :None
    )
);

const Option = (
    module:

    use std.Option.*;

    # https://github.com/rust-lang/rust/pull/94317 :(
    const inspect_none = [T] (opt :: t[T], f :: () -> ()) -> t[T] => match opt with (
        | :Some x => :Some x
        | :None => (
            f();
            :None
        )
    );

    const map_or_else = [T, U] (opt :: t[T], default :: () -> U, f :: T -> U) -> U => (
        match opt with (
            | :Some x => f(x)
            | :None => default()
        )
    );

    const is_some_and = [T] (opt :: t[T], f :: T -> Bool) -> Bool => (
        match opt with (
            | :Some x => f(x)
            | :None => false
        )
    );

    const expect = [T] (opt :: t[T], msg :: &str) -> T => match opt with (
        | :Some x => x
        | :None => panic(&format!("unwrapped :None: \(msg)") |> as_str)
    );
);

const ArrayList = (
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

    const iteri = [T] (
        a :: &ArrayList.t[T]
    ) -> std.iter.Iterable[type { UInt32, type (&T) }] => (
        let mut i = 0;
        {
            .iter = @move f => (
                iter(a).iter(item => (
                    let ret = f({i, item});
                    i += 1;
                    ret
                ))
            ),
        }
    );

    const is_empty = [T] (list :: &t[T]) -> Bool => length(list) == 0;

    const find_map = [T, U] (list :: &t[T], f :: type (&T) -> Option.t[U]) -> Option.t[U] => with_return (
        for item in iter(list) do (
            if f(item) is :Some u then (
                return :Some u
            );
        );
        :None
    );
);

