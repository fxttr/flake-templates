{
  description = "Scala Template";

  outputs = { self }: {
    templates = {
      sbt = {
        path = ./sbt;
        description = "Scala SBT Template";
      };

      default = self.templates.sbt;
    };
  };
}
