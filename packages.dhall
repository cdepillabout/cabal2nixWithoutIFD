let upstream =
      https://raw.githubusercontent.com/purenix-org/temp-package-set/main/packages.dhall sha256:870e084327dc7673a5313cd5a61fedc3504d7e36570317b91adf321c5cb4091d

in  upstream
  with cabal-parser = ./purescript-cabal-parser/spago.dhall as Location
  with parser-combinator =
      ./purescript-parser-combinator/spago.dhall as Location
