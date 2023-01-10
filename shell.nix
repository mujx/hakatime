{ pkgs ? (import ./nix/upstream.nix) }:

pkgs.stdenv.mkDerivation {
  name = "hakatime-dev";

  buildInputs = [
    pkgs.haskell.compiler.ghc924
    pkgs.cabal-install
    pkgs.zlib
    pkgs.postgresql
    pkgs.hlint
    pkgs.pgcli
    pkgs.cabal2nix
    pkgs.ormolu
  ];

  shellHook = ''
    export HISTCONTROL=ignoreboth:erasedups
  '';
}
