{
  description = "Scala Template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";
  };

  outputs = { self, nixpkgs, flake-utils }: 
      flake-utils.lib.eachDefaultSystem
        (system:
          let pkgs = nixpkgs.legacyPackages.${system}; in
          {
            inherit pkgs;
            devShells.default = pkgs.mkShell {
                buildInputs = [
                  pkgs.sbt
                  pkgs.metals
                  pkgs.openjdk17
                  pkgs.coursier
                ];
              };
          }
        );
}
