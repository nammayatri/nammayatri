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
    mobility-core = inputs.beckn-shared-kernel + /lib/mobility-core;
    mock-registry = inputs.beckn-gateway + /app/mock-registry;
    passetto-client = inputs.passetto-hs + /client;
    passetto-core = inputs.passetto-hs + /core;

    # Dependencies from Hackage
    aeson = "1.5.6.0";
    dhall = "1.35.0";
    geojson = "4.0.4";
    http2 = "3.0.2";
    jwt = "0.10.0";
    lens = "4.19.2";
    megaparsec = "9.0.0";
    mmorph = "1.1.3";
    openapi3 = "3.1.0";
    optparse-applicative = "0.15.1.0";
    servant = "0.18.3";
    servant-client = "0.18.1";
    servant-client-core = "0.18.1";
    servant-docs = "0.11.7";
    servant-foreign = "0.15.2";
    servant-mock = "0.8.7";
    servant-multipart = "0.12";
    servant-openapi3 = "2.0.1.2";
    servant-server = "0.18.1";
    singletons = "2.6";
    streamly = "0.7.3.1";
    th-desugar = "1.10";
    universum = "1.6.1";
  };
  overrides = compilePipe (self: super: with pkgs.haskell.lib.compose; {
    # NOTE: A lot of these overrides try to match
    # https://www.stackage.org/lts-16.31 because that's what the project is
    # mostly using. As we migrate to GHC 9.2, we can remove most of these.
    aeson = [ doJailbreak ];
    aeson-casing = [ dontCheck ];
    amazonka-core = [ unmarkBroken dontCheck doJailbreak ];
    beam-core = [ doJailbreak ];
    beam-migrate = [ doJailbreak ];
    beam-mysql = [ dontCheck doJailbreak ];
    beam-postgres = [ dontCheck doJailbreak ];
    beam-sqlite = [ dontCheck doJailbreak ];
    binary-parsers = [ unmarkBroken ];
    dhall = [ dontCheck doJailbreak ];
    euler-hs = [ dontCheck dontHaddock doJailbreak (appendPatch ./euler-hs.patch) ];
    geojson = [ dontCheck ];
    hedis = [ dontCheck ];
    http2 = [ dontCheck ];
    jwt = [ dontCheck doJailbreak ];
    lens = [ dontCheck doJailbreak ];
    lrucaching = [ unmarkBroken ];
    megaparsec = [ dontCheck doJailbreak ];
    mmorph = [ doJailbreak ];
    mysql-haskell = [ dontCheck doJailbreak ];
    openapi3 = [ dontCheck doJailbreak ];
    optparse-applicative = [ doJailbreak ];
    prometheus-proc = [ unmarkBroken ];
    sequelize = [ dontCheck ];
    servant = [ doJailbreak ];
    servant-client = [ dontCheck doJailbreak ];
    servant-client-core = [ dontCheck doJailbreak ];
    servant-docs = [ dontCheck doJailbreak ];
    servant-foreign = [ dontCheck doJailbreak ];
    servant-mock = [ dontCheck doJailbreak ];
    servant-multipart = [ dontCheck doJailbreak ];
    servant-openapi3 = [ dontCheck doJailbreak ];
    servant-server = [ dontCheck doJailbreak ];
    singletons = [ dontCheck doJailbreak ];
    streamly = [ dontCheck doJailbreak ];
    th-desugar = [ dontCheck doJailbreak ];
    tinylog = [ unmarkBroken ];
    universum = [ dontCheck doJailbreak ];
    word24 = [ unmarkBroken ];
  });
}
