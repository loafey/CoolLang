# Replaced in interpreter
plus = \x -> \y -> x;

id = \x -> x;

main = let p = (\y -> 10) in (\x -> p x) 5;