{
  inputs = { flake-utils.url = "github:numtide/flake-utils"; };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with import nixpkgs { inherit system; };
      with self.packages.${system};
      let
        name = "system-linux-proc";
        compiler = "ghc910";
      in
      {
        packages.default =
          haskell.lib.justStaticExecutables
            (haskell.packages.${compiler}.callCabal2nix "" ./. {});
        devShells.default = default.env;
      }
    );
}
