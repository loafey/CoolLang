grammar: 
    bnfc "src/Lang.bnfc" -o "src" -m -d --functor && cd src && make

run: 
    just grammar && cabal run