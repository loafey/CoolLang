data maybe a = nothing | just a ;

fromJust : maybe a -> a ;
fromJust = \x -> case x of {
    just (just x) => x ;
};

main = fromJust (just (just 5));