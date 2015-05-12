fun compress xs = foldr (fn (x, y) => if ((y = []) orelse (x <> hd y)) then x::y else y) [] xs;

compress [] = [];
compress [1, 1, 2, 3, 3, 3, 2, 2, 3] = [1, 2, 3, 2, 3];
compress [[1, 2], [1, 2], [3, 4], [1, 2]] = [[1, 2], [3, 4], [1, 2]];
