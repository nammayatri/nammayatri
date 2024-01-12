{ inputs, self, ... }:
{
  debug = inputs.debug.value;

  perSystem = { pkgs, ... }: {
    apps.trace.program = pkgs.writeShellApplication {
      name = "trace";
      meta.description = ''
        Evaluate the backend devShell whilst tracing verbose logging from haskell-flake.

        Useful to spot performance issues on the user's system.
      '';
      runtimeInputs = with pkgs; [
        moreutils
      ];
      text = ''
        set -x
        nix --no-eval-cache develop --trace-verbose ${self}#backend -c echo \
          2>&1 \
          | ts '[%H:%M:%S]' \
          | sed 's/^\([^ ]*\)/\x1b[31m\1\x1b[0m/'
      '';
    };
  };
}
