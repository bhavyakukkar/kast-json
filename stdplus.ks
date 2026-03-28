module:

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

    const parse = [T] (c :: Char) -> T => (
        String.parse[T](StringPlus.of_char(c))
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

    const repeat = (self :: String, times :: UInt32) -> String => (
        let mut new_str = self;
        if times == 0 then (
            ""
        ) else (
            for _ in 0..(times - 1) do (
                new_str += self;
            );
            new_str
        )
    );

    const is_empty = (s :: String) => String.length(s) == 0;
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

    const iteri = [T] (
        a :: &ArrayList.t[T]
    ) -> std.iter.Iterable[type { UInt32, type (&T) }] => (
        let mut i = 0;
        {
            .iter = f => (
                iter(a).iter(item => (
                    let ret = f({i, item});
                    i += 1;
                    ret
                ))
            ),
        }
    );

    const is_empty = [T] (list :: &t[T]) -> Bool => length(list) == 0;
);

const Tup = (
    module:

    const fst = [T] (tup :: {T, T}) -> T => tup.0;
);

const Default = [Self] newtype {
    .default :: () -> Self,
};

const default = [T] () -> T => (T as Default).default();

impl Bool as Default = {
    .default = () => true,
};
