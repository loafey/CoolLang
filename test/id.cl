#print = \x -> x;
#plus  = \x -> \y -> x; 

main = (\x -> \y -> x) 1 2;
# returns "x" instead of 1