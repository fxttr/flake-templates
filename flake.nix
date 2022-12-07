{
  description = "A collection of flake templates";

  outputs = { self }: {

    templates = {
      scala = {
        path = ./scala;
        description = "Scala templates";
      };

      haskell = {
        path = ./haskell;
        description = "Haskell templates";
      };

      trivial = {
        path = ./trivial;
        description = "A very basic flake";
      };
    };

    defaultTemplate = self.templates.trivial;

  };
}
