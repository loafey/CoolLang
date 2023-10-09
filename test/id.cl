#print = \x -> x;
#plus  = \x -> \y -> x; 

main = (\x -> \y -> \z -> z) 1 2 3;
# returns "x" instead of 1