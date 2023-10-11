data bool = true | false;

data maybe = nothing | just bool;

toBool : maybe -> bool;
toBool = \x -> case x of {
    nothing => false;
    just b => b;
};
