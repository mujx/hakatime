let
  rev    = "20.09";
  sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          hasql-queue         = haskellPackagesNew.callPackage ./hasql-queue.nix {};
          servant             = haskellPackagesNew.callPackage ./servant.nix {};
          servant-client      = haskellPackagesNew.callPackage ./servant-client.nix {};
          servant-client-core = haskellPackagesNew.callPackage ./servant-client-core.nix {};
          servant-server      = haskellPackagesNew.callPackage ./servant-server.nix {};
          tmp-postgres        = haskellPackagesNew.callPackage ./tmp-postgres.nix {};
        };
      };
    };
  };

  pkgs = import nixpkgs { inherit config; };

in pkgs

