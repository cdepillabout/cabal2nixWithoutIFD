
rec {
  # This is taken from lib/attrsets.nix in Nixpkgs.
  #
  # :: forall a. Array String  -> a -> AttrSet -> a
  attrByPath = attrPath: default: e:
    let
      attr = builtins.head attrPath;
    in
    if attrPath == [] then
      e
    else
      if e ? ${attr} then
        attrByPath (builtins.tail attrPath) default e.${attr}
      else
        default;

  concatStringsSep = builtins.concatStringsSep;

  getAttr = builtins.getAttr;

  # This is taken from lib/attrsets.nix in Nixpkgs.
  #
  # :: forall a. Array String  -> AttrSet -> a
  getAttrFromPath = attrPath: set:
    let errorMsg = "cannot find attribute `" + builtins.concatStringsSep "." attrPath + "'";
    in attrByPath attrPath (abort errorMsg) set;

  readFile = builtins.readFile;

  stringLength = builtins.stringLength;

  substring = builtins.substring;

  trace = builtins.trace;
}
