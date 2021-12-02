let upstream =
      https://raw.githubusercontent.com/purenix-org/temp-package-set/main/packages.dhall sha256:35a43b497d0e850f2ea201726d267132b1e66149d7f80d43a64fc8318ca783b9

in  upstream
  with cabal-parser = ./purescript-cabal-parser/spago.dhall as Location
  with parser-combinator =
      ./purescript-parser-combinator/spago.dhall as Location
