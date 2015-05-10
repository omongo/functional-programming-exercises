fun my_rev xs =
    let
        fun aux (xs, acc) =
            case xs of
                [] => acc
              | x::xs' => aux (xs', (x::acc)) 
    in
        aux (xs, [])
    end
 
fun palindrome_detector xs = xs = my_rev xs;
 
palindrome_detector [1, 2, 3, 4, 5] = false;
palindrome_detector ["r", "a", "c", "e", "c", "a", "r"] = true;
palindrome_detector [1, 1, 3, 3, 1, 1] = true;
