{}:
with import <nixpkgs> {};
let
  inherit (haskellPackages) aeson ariadne attoparsec
    cabal cabalInstall_1_18_0_2 doctest filepath genericDeriving lens
    semigroups simpleReflect snap tasty tastyHunit tastySmallcheck tastyQuickcheck tastyAntXml
    text unorderedContainers utf8String vector webRoutesBoomerang
    quickcheckInstances;

  indexed = cabal.mkDerivation (self: {
    pname = "indexed";
    version = "0.1";
    sha256 = "1dx5pyi5psjd2l26hc3wfsapnywdl0kqpw98b3jwc0xq4406ax12";
    meta = {
      homepage = "https://github.com/reinerp/indexed";
      description = "Haskell98 indexed functors, monads, comonads";
      license = self.stdenv.lib.licenses.bsd3;
      platforms = self.ghc.meta.platforms;
    };
  });

  indexedFree = cabal.mkDerivation (self: {
    pname = "indexed-free";
    src = /home/ollie/work/indexed-free;
    version = "0.3.0";
    buildDepends = [ indexed ];
    meta = {
      homepage = "https://github.com/fumieval/indexed-free";
      description = "indexed monads for free";
      license = self.stdenv.lib.licenses.bsd3;
      platforms = self.ghc.meta.platforms;
    };
  });

  lensAeson = cabal.mkDerivation (self: {
    pname = "lens-aeson";
    version = "0.1.2";
    sha256 = "1h0w8p227r8gzvgqjl210i0z7xxv3435vwyi3j7vkm7a05cdk03l";
    buildDepends = [
      aeson attoparsec lens text unorderedContainers utf8String vector
    ];
    testDepends = [
      doctest filepath genericDeriving semigroups simpleReflect
    ];
    meta = {
      homepage = "http://github.com/lens/lens-aeson/";
      description = "Law-abiding lenses for aeson";
      license = self.stdenv.lib.licenses.bsd3;
      platforms = self.ghc.meta.platforms;
    };
  });

in cabal.mkDerivation (self: {
  pname = "json-assertions";
  version = "0.1.0";
  src = ./.;
  buildDepends = [ aeson indexed indexedFree lensAeson ];
  buildTools = [ cabalInstall_1_18_0_2 ];
})
