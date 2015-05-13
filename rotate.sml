fun rotate xs n =
    let
        val length_xs = length xs
    in
        List.@ (foldr (fn (x, (y, z)) => if length y < length_xs - (if n > 0 then n else n mod length_xs) then (x::y, z) else (y, x::z)) ([], []) xs)
    end;

fun rotate_string s n = (implode o (fn xs => rotate xs n) o explode) s;

rotate [1,2,3,4] 1 = [2,3,4,1];
rotate_string "hello" 2 = "llohe";
rotate_string "negative ok" ~2 = "oknegative ";
