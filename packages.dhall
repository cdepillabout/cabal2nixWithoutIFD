let upstream =
      https://raw.githubusercontent.com/purenix-org/temp-package-set/main/packages.dhall sha256:1460c7eb214a7c44a54e2030409b89ca2e383da2710b6a52577054639540fe0c

in  upstream
  with cabal-parser = ./purescript-cabal-parser/spago.dhall as Location
  with parser-combinator =
      ./purescript-parser-combinator/spago.dhall as Location
