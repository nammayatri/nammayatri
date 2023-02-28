{ self, pkgs, ... }:

let
  inputs = self.inputs;
in
{
  packageSettings =
    let
      defaults = {
        jailbreak = true;
        doHaddock = false;
        doCheck = false;
      };
    in
    # NOTE: Some of the 'input' boilerplace could be automated with
      # https://github.com/srid/haskell-flake/issues/84 
    {
      # Nammayatri umbrella dependencies
      mobility-core.input.path = inputs.beckn-shared-kernel + /lib/mobility-core;
      beckn-gateway.input.path = inputs.beckn-gateway + /app/gateway;
      mock-registry.input.path = inputs.beckn-gateway + /app/mock-registry;

      # Packages coming from flake inputs
      euler-hs = {
        input.path = inputs.euler-hs;
        overrides = { old, ... }: {
          inherit (defaults) jailbreak doHaddock doCheck;
          patches = (old.patches or [ ]) ++ [ ./euler-hs.patch ];
        };
      };
      passetto-client.input.path = inputs.passetto-hs + /client;
      passetto-core.input.path = inputs.passetto-hs + /core;
      hedis.input.path = inputs.hedis;
      hedis.overrides.doCheck = false;
      sequelize.input.path = inputs.sequelize;
      sequelize.overrides.doCheck = false;
      beam-core.input.path = inputs.beam + /beam-core;
      beam-core.overrides.jailbreak = true;
      beam-migrate.input.path = inputs.beam + /beam-migrate;
      beam-migrate.overrides.jailbreak = true;
      beam-sqlite.input.path = inputs.beam + /beam-sqlite;
      beam-sqlite.overrides = {
        inherit (defaults) jailbreak doCheck;
      };
      beam-postgres.input.path = inputs.beam + /beam-postgres;
      beam-postgres.overrides = {
        inherit (defaults) jailbreak doCheck;
      };
      beam-mysql.input.path = inputs.beam-mysql;
      beam-mysql.overrides = {
        inherit (defaults) jailbreak doCheck;
      };
      mysql-haskell.input.path = inputs.mysql-haskell;
      mysql-haskell.overrides = {
        inherit (defaults) jailbreak doCheck;
      };
    };
  overrides = self: super: with pkgs.haskell.lib; {
    # NOTE: A lot of these overrides try to match
    # https://www.stackage.org/lts-16.31 because that's what the project is
    # mostly using.
    prometheus-proc = unmarkBroken super.prometheus-proc; # self.callHackage "prometheus-proc" "0.1.4.0" { };
    openapi3 = doJailbreak (dontCheck (self.callHackage "openapi3" "3.1.0" { }));
    servant-openapi3 = doJailbreak (dontCheck (self.callHackage "servant-openapi3" "2.0.1.2" { }));
    servant-multipart = doJailbreak (dontCheck (self.callHackage "servant-multipart" "0.12" { }));
    lens = doJailbreak (dontCheck (self.callHackage "lens" "4.19.2" { }));
    tinylog = unmarkBroken super.tinylog;
    universum = doJailbreak (dontCheck (self.callHackage "universum" "1.6.1" { }));
    jwt = doJailbreak (dontCheck (self.callHackage "jwt" "0.10.0" { }));
    dhall = doJailbreak (dontCheck (self.callHackage "dhall" "1.35.0" { }));
    optparse-applicative = doJailbreak super.optparse-applicative_0_15_1_0;
    megaparsec = doJailbreak (dontCheck (self.callHackage "megaparsec" "9.0.0" { }));
    aeson-casing = dontCheck super.aeson-casing;
    amazonka-core = doJailbreak (dontCheck (unmarkBroken super.amazonka-core));
    singletons = doJailbreak (dontCheck (self.callHackage "singletons" "2.6" { }));
    th-desugar = doJailbreak (dontCheck (self.callHackage "th-desugar" "1.10" { }));
    streamly = doJailbreak (dontCheck (self.callHackage "streamly" "0.7.3.1" { }));

    # These may not be necessary once we upgrade to GHC 9.2
    aeson = super.aeson_1_5_6_0;
    geojson = dontCheck (self.callHackage "geojson" "4.0.4" { });
    http2 = dontCheck (self.callHackage "http2" "3.0.2" { });
    binary-parsers = unmarkBroken super.binary-parsers;
    word24 = unmarkBroken super.word24;
    mmorph = doJailbreak super.mmorph_1_1_3;
    servant = doJailbreak (self.callHackage "servant" "0.18.3" { });
    servant-mock = doJailbreak (dontCheck (self.callHackage "servant-mock" "0.8.7" { }));
    servant-server = doJailbreak (dontCheck (self.callHackage "servant-server" "0.18.1" { }));
    servant-client = doJailbreak (dontCheck (self.callHackage "servant-client" "0.18.1" { }));
    servant-client-core = doJailbreak (dontCheck (self.callHackage "servant-client-core" "0.18.1" { }));
    servant-docs = doJailbreak (dontCheck (self.callHackage "servant-docs" "0.11.7" { }));
    servant-foreign = doJailbreak (dontCheck (self.callHackage "servant-foreign" "0.15.2" { }));
    lrucaching = unmarkBroken super.lrucaching;
  };
}
