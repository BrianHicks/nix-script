{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import inputs.nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          packages = [
            # rust
            pkgs.rustc
            pkgs.cargo
            pkgs.clippy
            pkgs.rustPackages.rustfmt

            # system
            pkgs.libiconv
          ];
        };
      });
}
