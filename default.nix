(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = { abacus = ./.; };
  shells = {
    ghc = ["abacus"];
    ghcjs = ["abacus"];
  };
  overrides = self: super: {
    double-conversion = pkgs.haskell.lib.dontCheck super.double-conversion;
  };
})