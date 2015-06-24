with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, aeson, base, indexed, indexed-free, lens
             , lens-aeson, stdenv, text
             }:
             mkDerivation {
               pname = "json-assertions";
               version = "1.0.5";
               src = ./.;
               buildDepends = [
                 aeson base indexed indexed-free lens lens-aeson text
               ];
               homepage = "http://github.com/ocharles/json-assertions.git";
               description = "Test that your (Aeson) JSON encoding matches your expectations";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
