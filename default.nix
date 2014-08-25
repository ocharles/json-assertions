{}:
with import <nixpkgs> {};
let
  inherit (haskellPackages) aeson ariadne attoparsec
    cabal cabalInstall doctest filepath genericDeriving lens lensAeson
    semigroups simpleReflect snap tasty tastyHunit tastySmallcheck tastyQuickcheck tastyAntXml
    text unorderedContainers utf8String vector webRoutesBoomerang free
    quickcheckInstances indexed indexedFree;

in cabal.mkDerivation (self: {
  pname = "json-assertions";
  version = "0.1.0";
  src = ./.;
  buildDepends = [ aeson indexed indexedFree lens lensAeson free text ];
  buildTools = [ cabalInstall ];
})
