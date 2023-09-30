{
    description = "A simple tool to display the contents of a Git repository in the terminal";

    inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";

    outputs = { self, nixpkgs }:
    let
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        haskellPackages = pkgs.haskellPackages;
    in {
        defaultPackage.x86_64-linux =
            with import nixpkgs { system = "x86_64-linux"; };
            stdenv.mkDerivation {
                name = "summr";
                src = self;
                buildInputs = [
                    (haskellPackages.ghcWithPackages (hpkgs: [
                        hpkgs.regex
                        hpkgs.optparse-applicative
                    ]))
                ];
                buildPhase = "make";
                installPhase = "mkdir -p $out/bin; install -t $out/bin summr";
            };
    };
}