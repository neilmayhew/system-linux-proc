{
  description = "A library for accessing the /proc filesystem in Linux";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      name = "system-linux-proc";
      compiler = "ghc910";
    in
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs name;
      overlay = final: prev: {
        "${name}".defaultPackage =
          final.haskell.lib.justStaticExecutables
            (final.haskell.packages.${compiler}.callPackage ./default.nix {});
      };
      shell = { pkgs }: pkgs.${name}.defaultPackage.env;
    };
}
