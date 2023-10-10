data test a b = test a b ;

fromTest = \x -> case x of {
    test a b => b;
};

main = fromTest (test 1 4);