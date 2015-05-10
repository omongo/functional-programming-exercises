datatype 'a mytype =
    Val of 'a
  | Lst of 'a mytype list

fun my_flatten xs =
    let
        fun aux (xs, acc) =
          case xs of
              [] => acc
            | (Val v) :: xs' => aux (xs', (acc @ [(Val v)]))
            | (Lst l) :: xs' => aux (xs', (aux (l, acc)))
    in
        aux (xs, [])
    end

val x = [(Val #"a"), (Lst [(Val #"b"), (Lst [(Val #"c"), (Val #"d")]), (Val #"e")])];
my_flatten x = [(Val #"a"), (Val #"b"), (Val #"c"), (Val #"d"), (Val #"e")];
