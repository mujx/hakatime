let
  rev    = "72cdc142b93cffbedc91001ae5c6e9059b03c60c";
  sha256 = "1nxma713balaxwp52vjklbn7p53m4cr43qphh6bdv9pzymdyjsmd";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

  pkgs = import nixpkgs {};

in pkgs

