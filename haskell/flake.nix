{
  description = "Haskell Templates";

  outputs = { self }: {
    templates = {
      stack = {
        path = ./stack;
        description = "Haskell stack template";
      };

      default = self.templates.stack;
    };
  };
}
