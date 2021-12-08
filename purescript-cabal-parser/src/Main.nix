
{ # This Nix file ends up in
  # cabal2nixWithoutIFD/purescript-cabal-parser/output/Main/foreign.nix.  The cabal file
  # is located at cabal2nixWithoutIFD/example-cabal-library/example-cabal-library.cabal.
  cabalFilePath = ./../../../example-cabal-library/example-cabal-library.cabal;

  haskellPackagePath = ./../../../example-cabal-library/.;

  # :: forall a. Array String -> (AttrSet -> a) -> FunctionWithArgs
  mkFunctionWithArgs = argList: f:
    { __functor = _: f;
      __functionArgs = builtins.listToAttrs (map (k: { name = k; value = false; }) argList);
    };
}
