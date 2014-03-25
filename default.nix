{}:
with import <nixpkgs> {};
let
  inherit (haskellPackages) ariadne attoparsec
    cabal cabalInstall_1_18_0_3 doctest filepath genericDeriving lens
    semigroups simpleReflect snap tasty tastyHunit tastySmallcheck tastyQuickcheck tastyAntXml
    text unorderedContainers utf8String vector webRoutesBoomerang free
    quickcheckInstances indexed indexedFree;

in cabal.mkDerivation (self: {
  pname = "json-assertions";
  version = "0.1.0";
  src = ./.;
  buildDepends = [ indexed indexedFree lens free text ];
  buildTools = [ cabalInstall_1_18_0_3 ];
})
