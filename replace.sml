datatype 'a mytype = Val of 'a | Lst of 'a mytype list;

fun my_flatten xs =
    let
        fun aux xs acc =
          case xs of
              [] => acc
            | (Val v) :: xs' => aux xs' (acc @ [v])
            | (Lst l) :: xs' => aux xs' (aux l acc)
    in
        aux xs []
    end;

fun replace_string rep_f s = (implode o rep_f o explode) s;

fun replace xs a b = map (fn x => if x = a then b else x) xs;

fun replace2 xs cond_f = map cond_f xs;

fun replace3 xs rep_f c =
  let
      fun cons_replace_list v c acc =
          if c = 0
          then Lst acc
          else cons_replace_list v (c - 1) ((Val v) :: acc)
  in
      my_flatten (map (fn x => if x = rep_f x
                               then Val x
                               else cons_replace_list (rep_f x) c [])
                      xs)
  end;

replace [1, 2, 3, 4, 5, 4, 3, 2, 1] 3 9 = [1, 2, 9, 4, 5, 4, 9, 2, 1];
replace_string (fn xs => replace xs #"e" #"a") "hello, how are you?" = "hallo, how ara you?";

replace2 [1, 2, 3, 4, 5, 4, 3, 2, 1] (fn x => if x = 3 then 9 else x) = [1, 2, 9, 4, 5, 4, 9, 2, 1];
replace_string (fn xs => replace2 xs (fn x => if x = #"e" then #"a" else x)) "hello, how are you?" = "hallo, how ara you?";

replace3 [1, 2, 3, 4, 5, 4, 3, 2, 1] (fn x => if x = 3 then 9 else x) 3 = [1, 2, 9, 9, 9, 4, 5, 4, 9, 9, 9, 2, 1];
replace_string (fn xs => replace3 xs (fn x => if x = #"e" then #"a" else x) 3) "hello, how are you?" = "haaallo, how araaa you?";
