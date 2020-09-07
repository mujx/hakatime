let
  rev    = "7b537f99c0937655d0e67646da62380548c02523";
  sha256 = "0jz8m1p45sqmsnfjwvqzgapv1mk29myy8yawi5p21s7dalhxs62b";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          servant             = haskellPackagesNew.callPackage ./servant.nix {};
          servant-server      = haskellPackagesNew.callPackage ./servant-server.nix {};
          servant-client      = haskellPackagesNew.callPackage ./servant-client.nix {};
          servant-client-core = haskellPackagesNew.callPackage ./servant-client-core.nix {};
        };
      };
    };
  };

  pkgs = import nixpkgs { inherit config; };

in pkgs

