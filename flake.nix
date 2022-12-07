{
  description = "A collection of flake templates";

  outputs = { self }: {

    templates = {
      scala = {
        path = ./scala-sbt;
        description = "Scala sbt template";
      };

      haskell = {
        path = ./haskell-stack;
        description = "Haskell stack template";
      };

      trivial = {
        path = ./trivial;
        description = "A very basic flake";
      };
    };

    defaultTemplate = self.templates.trivial;

  };
}
