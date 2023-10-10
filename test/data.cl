data forall a. maybe a = nothing | just a ;

fromJust : forall a . maybe a -> a ;
fromJust = \a -> case a of {
    just a => a ;
};
