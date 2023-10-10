data maybe a = nothing | just a ;

fromJust : maybe a -> a ;
fromJust = \x -> case x of {
    just x => x ;
};

main = fromJust (just 5);