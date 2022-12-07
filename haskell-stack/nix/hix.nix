{pkgs, ...}: {
  name = "ChangeMe";
  compiler-nix-name = "ghc8107";

  shell = {
    tools = {
      cabal = "3.8.1.0";
      hlint = "3.4";
      haskell-language-server = "1.8.0.0";
      ormolu = "0.5.0.0";
      happy = "1.20.0";
      stack = "2.9.1";
    };

    exactDeps = true;
  };
}
