{
  description = "A collection of flake templates for my projects";

  outputs = { self }: {

    templates = {
      scala = {
        path = ./scala;
        description = "Scala template using the sbt buildsystem";
      };

      haskell-stack = {
        path = ./haskell-stack;
        description = "Haskell template using haskellNix and stack";
      };

      ocaml = {
        path = ./ocaml;
        description = "OCaml template using the dune buildsystem"
      };

      trivial = {
        path = ./trivial;
        description = "A very basic flake";
      };
    };

    defaultTemplate = self.templates.trivial;
  };
}
