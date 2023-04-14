{ inputs, ... }:
{
  imports = [
  ];
  perSystem = { config, self', pkgs, lib, ... }:
      let easy-ps = import inputs.easy-purescript-nix { inherit pkgs; };
      in {
        devShells.frontend = pkgs.mkShell {
          name = "ps-dev-shell";
          packages = [
              easy-ps.purs-0_15_4
              easy-ps.spago
          ];
        };
  };
}
