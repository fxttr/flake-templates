{
  description = "NixOS flake with CUDA support, JupyterLab, NumPy, Pandas, scikit-learn, and PyTorch";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";  # Verwende den unstable channel für aktuelle Pakete
  };

  outputs = { self, nixpkgs }: {
    # Für Entwicklungsumgebung
    devShells.x86_64-linux.default = let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        config = {
          allowUnfree = true;  # Erforderlich für CUDA und NVIDIA-Treiber
        };
      };
      
      pythonEnv = pkgs.python312.withPackages (ps: with ps; [
        numpy
        pandas
        scikit-learn
        pytorch
        jupyterlab
      ]);

    in pkgs.mkShell {
      buildInputs = [
        pythonEnv
        pkgs.cudaPackages.cudatoolkit  # CUDA Toolkit für GPU-Unterstützung
        pkgs.cudaPackages.cudnn  # CUDA Deep Neural Network library
      ];

      # CUDA-Umgebungsvariablen
      CUDA_HOME = "${pkgs.cudatoolkit}";
      LD_LIBRARY_PATH = "${pkgs.cudaPackages.cudatoolkit.lib}/lib64:${pkgs.cudaPackages.cudnn}/lib";
      PATH = "${pkgs.cudatoolkit}/bin:${pkgs.path}";

      shellHook = ''
        echo "CUDA_HOME set to $CUDA_HOME"
        echo "LD_LIBRARY_PATH set to $LD_LIBRARY_PATH"
        echo "NVIDIA and CUDA environment initialized"
      '';
    };
  };
}
