{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, indexed, indexed-free, lens
      , lens-aeson, stdenv, text
      }:
      mkDerivation {
        pname = "json-assertions";
        version = "1.0.7";
        src = ./.;
        libraryHaskellDepends = [
          aeson base indexed indexed-free lens lens-aeson text
        ];
        homepage = "http://github.com/ocharles/json-assertions.git";
        description = "Test that your (Aeson) JSON encoding matches your expectations";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
