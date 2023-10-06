{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
           basePackages = pkgs.haskellPackages;
           devShell = {
            enable = true;
            tools = hp: {
                BNFC = hp.BNFC;
                alex = hp.alex;
                fourmolu = hp.fourmolu;
                ghcid = hp.ghcid;
                happy = hp.happy;
            };
            hlsCheck.enable = true;
           };
           autoWire = [
               "packages"
               "apps"
               "checks"
           ];
        };
        devShells.default = pkgs.mkShell {
            name = "CoolLang development shell";
            inputsFrom = [
                config.haskellProjects.default.outputs.devShell
            ];
            nativeBuildInputs = with pkgs; [
                just
            ];
        };
      };
    };
}
