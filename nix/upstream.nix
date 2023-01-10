let
  rev    = "dfdaa0ce26e1e29967a01e1900aba5b112fc087e";
  sha256 = "0a2bdy44dx9v5fa8j13ys82g9jv3cl7qljsxhf1j75l2gcr0smls";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

  pkgs = import nixpkgs {};

in pkgs

