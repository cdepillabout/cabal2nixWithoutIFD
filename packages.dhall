
let upstream =
      https://raw.githubusercontent.com/purenix-org/temp-package-set/main/packages.dhall

in  upstream
    with cabal-parser = ./purescript-cabal-parser/spago.dhall as Location
    with parser-combinator = ./purescript-parser-combinator/spago.dhall as Location
