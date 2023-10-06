grammar:
    bnfc "src/Lang.bnfc" -o "src" -m -d --functor && cd src && make

runTest:
    cabal run CoolLang -- test/test.cl

run file:
    cabal run CoolLang -- {{file}}

nrun file:
    nix-shell --run "just run {{file}}"

ngrammar:
    nix-shell --run "just grammar"
