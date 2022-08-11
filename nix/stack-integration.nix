# from https://www.tweag.io/blog/2022-06-02-haskell-stack-nix-shell/
let
  sources = import ./source.nix;
  pkgs = sources.nixpkgs;
in

# See https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file
{ ghc }:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "haskell-stack-nix";
  # System dependencies needed at compilation time
  buildInputs = [
    pkgs.postgresql
    pkgs.zlib
  ];
}

