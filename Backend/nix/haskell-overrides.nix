{ self, pkgs, lib, ... }:

let
  inputs = self.inputs;
  # A function that enables us to write `foo = [ dontCheck ]` instead of `foo =
  # lib.pipe super.foo [ dontCheck ]` in haskell-flake's `overrides`.
  compilePipe = f: self: super:
    lib.mapAttrs
      (name: value:
        if lib.isList value then
          lib.pipe super.${name} value
        else
          value
      )
      (f self super);
in
{
  source-overrides = {
    # Dependencies from flake inputs.
    # NOTE: The below boilerplate can be automated once
    # https://github.com/srid/haskell-flake/issues/84 is done.
    inherit (inputs)
      beam-mysql
      euler-hs
      hedis
      mysql-haskell
      sequelize
      ;
    beam-core = inputs.beam + /beam-core;
    beam-migrate = inputs.beam + /beam-migrate;
    beam-postgres = inputs.beam + /beam-postgres;
    beam-sqlite = inputs.beam + /beam-sqlite;
    beckn-gateway = inputs.beckn-gateway + /app/gateway;
    mobility-core = inputs.shared-kernel + /lib/mobility-core;
    mock-registry = inputs.beckn-gateway + /app/mock-registry;
    passetto-client = inputs.passetto-hs + /client;
    passetto-core = inputs.passetto-hs + /core;

  };
  overrides = compilePipe (self: super: with pkgs.haskell.lib.compose; {
    beam-core = [ doJailbreak ];
    beam-migrate = [ doJailbreak ];
    beam-mysql = [ dontCheck doJailbreak ];
    beam-postgres = [ dontCheck doJailbreak ];
    beam-sqlite = [ dontCheck doJailbreak ];
    euler-hs = [ dontCheck dontHaddock doJailbreak (appendPatch ./euler-hs.patch) ];
    hedis = [ dontCheck ];
    mysql-haskell = [ dontCheck doJailbreak ];
    sequelize = [ dontCheck ];
  });
}
