{
  perSystem = {
    treefmt.config = {
      # Suppress autoformatting of frontend dhall files.
      settings.formatter.dhall.excludes = [
        "Frontend/packages.dhall"
      ];
    };
  };
}
