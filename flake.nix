{
description = "A simple Haskell development environment";

inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

outputs = { self, nixpkgs }:
    let
    supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in
    {
    devShells = forAllSystems (system:
        let
        pkgs = nixpkgsFor.${system};
        in
        {
        default = pkgs.haskellPackages.developPackage {
            root = ./.;
            modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
                [
                haskell-language-server
                ]);
        };
        });
    };
}
