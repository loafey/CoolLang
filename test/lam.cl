# Replaced in interpreter
plus = \x -> \y -> x;

id = \x -> x;

main = (\x -> (\y -> y) x) 5;