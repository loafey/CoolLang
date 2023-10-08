main : forall io. io ;
main = \x -> case x of {
    just just => 1 ;
    nothing => 0 ;
} ;
