{ self, ... }:

let
  cachixName = "nammayatri";
in
{
  perSystem = { pkgs, lib, ... }: {
    # A script to push to cachix, until we configure CI to do it automatically
    # for both Linux and macOS.
    apps.cachix = {
      type = "app";
      program = lib.getExe
        (pkgs.writeShellApplication {
          name = "cachix-push";
          runtimeInputs = with pkgs; [
            nix
            cachix
            jq
          ];
          text = ''
            # Push packages
            echo '## Pushing nix packages ...'
            nix build .#all --json | \
              jq -r '.[].outputs | to_entries[].value' | \
              cachix push ${cachixName}
          '' + (if pkgs.stdenv.hostPlatform.isLinux && lib.hasAttr "rev" self then ''
            # Push docker image
            if [[ "$(git branch --show-current)" == "main" ]]; then
              echo '## Pushing docker image drv ...'
              nix build --no-allow-dirty .#dockerImage --json | \
                jq -r '.[].outputs | to_entries[].value' | \
                cachix push ${cachixName}
            fi
          '' else "") + ''
            # Push shell
            echo '## Pushing nix shell ...'
            tmpfile=$(mktemp /tmp/dev-profile.XXXXXX)
            rm "$tmpfile"
            nix develop --profile "$tmpfile" -c echo > /dev/null
            cachix push ${cachixName} "$tmpfile"
          '';
        });
    };
  };
}

