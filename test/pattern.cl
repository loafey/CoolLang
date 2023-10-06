main : io ;
main = \x -> case x of {
    just (just nothing) => 1 ;
    nothing => 0 ;
} ;
