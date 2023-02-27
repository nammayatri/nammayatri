{ self, ... }:

let
  cachixName = "srid-nammayatri";
in
{
  perSystem = { pkgs, ... }: {
    apps.cachix = pkgs.writeShellApplication {
      name = "cachix-push";
      buildInputs = with pkgs; [
        nix
        cachix
        jq
      ];
      text = ''
        set -e
        # Push shell
        echo 'Pushing nix shell ...'
        nix develop --profile $TMP/dev-profile 
        cachix push ${cachixName} $TMP/dev-profile
        # Push packages
        echo 'Pushing nix packages ...'
        nix build .#all --json | \
         jq -r '.[].outputs | to_entries[].value' | \
         cachix push ${cachixName}
        # Push docker image
        echo 'Pushing docker image drv ...'
        nix build .#dockerImage --json | \
         jq -r '.[].outputs | to_entries[].value' | \
         cachix push ${cachixName}
      '';
    };
  }
