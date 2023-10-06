grammar:
    bnfc "src/Lang.bnfc" -o "src" -m -d --functor && cd src && make

runTest: 
    just grammar && cabal run CoolLang -- test/test.cl
run file: 
    just grammar && cabal run CoolLang -- {{file}}
ngrammar:
    nix-shell --run "just grammar"