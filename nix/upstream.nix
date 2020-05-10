let
  rev    = "10100a97c8964e82b30f180fda41ade8e6f69e41";
  sha256 = "011f36kr3c1ria7rag7px26bh73d1b0xpqadd149bysf4hg17rln";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          servant        = haskellPackagesNew.callPackage ./servant.nix {};
          servant-server = haskellPackagesNew.callPackage ./servant-server.nix {};
        };
      };
    };
  };

  pkgs = import nixpkgs { inherit config; };

in pkgs

