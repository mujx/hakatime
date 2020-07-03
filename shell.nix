{ pkgs ? (import ./nix/upstream.nix) }:

pkgs.stdenv.mkDerivation {
  name = "hakatime-dev";

  buildInputs = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.zlib
    pkgs.postgresql
    pkgs.hlint
    pkgs.pgcli
    pkgs.cabal2nix
  ];

  shellHook = ''
    export HISTCONTROL=ignoreboth:erasedups
  '';
}
