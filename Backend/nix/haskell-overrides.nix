{ self, pkgs, ... }:

let
  inputs = self.inputs;
in
{
  # NOTE: Some of the 'input' boilerplace could be automated with
  # https://github.com/srid/haskell-flake/issues/84 
  packageSettings = {
    # Nammayatri umbrella dependencies
    mobility-core.input.path = inputs.beckn-shared-kernel + /lib/mobility-core;
    beckn-gateway.input.path = inputs.beckn-gateway + /app/gateway;
    mock-registry.input.path = inputs.beckn-gateway + /app/mock-registry;

    # Packages coming from flake inputs
    euler-hs = {
      input.path = inputs.euler-hs;
      overrides = { old, ... }: {
        jailbreak = true;
        doCheck = false;
        doHaddock = false;
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
      jailbreak = true;
      doCheck = false;
    };
    beam-postgres.input.path = inputs.beam + /beam-postgres;
    beam-postgres.overrides = {
      jailbreak = true;
      doCheck = false;
    };
    beam-mysql.input.path = inputs.beam-mysql;
    beam-mysql.overrides = {
      jailbreak = true;
      doCheck = false;
    };
    mysql-haskell.input.path = inputs.mysql-haskell;
    mysql-haskell.overrides = {
      jailbreak = true;
      doCheck = false;
    };

    # Packages from nixpkgs 
    aeson.input = { super, ... }: { drv = super.aeson_1_5_6_0; };
    aeson-casing.overrides.doCheck = false;
    prometheus-proc.overrides.broken = false;
    tinylog.overrides.broken = false;

    # Trying to match Stack LTS 16.31 (of stack.yaml)
    # These should be removed once we upgrade to GHC 9.2
    openapi3.input.hackageVersion = "3.1.0";
    openapi3.overrides = { jailbreak = true; doCheck = false; };

  };
  overrides = self: super: with pkgs.haskell.lib; {
    # TODO: port these to packageSettings
    servant-openapi3 = doJailbreak (dontCheck (self.callHackage "servant-openapi3" "2.0.1.2" { }));
    servant-multipart = doJailbreak (dontCheck (self.callHackage "servant-multipart" "0.12" { }));
    lens = doJailbreak (dontCheck (self.callHackage "lens" "4.19.2" { }));
    universum = doJailbreak (dontCheck (self.callHackage "universum" "1.6.1" { }));
    jwt = doJailbreak (dontCheck (self.callHackage "jwt" "0.10.0" { }));
    dhall = doJailbreak (dontCheck (self.callHackage "dhall" "1.35.0" { }));
    optparse-applicative = doJailbreak super.optparse-applicative_0_15_1_0;
    megaparsec = doJailbreak (dontCheck (self.callHackage "megaparsec" "9.0.0" { }));
    amazonka-core = doJailbreak (dontCheck (unmarkBroken super.amazonka-core));
    singletons = doJailbreak (dontCheck (self.callHackage "singletons" "2.6" { }));
    th-desugar = doJailbreak (dontCheck (self.callHackage "th-desugar" "1.10" { }));
    streamly = doJailbreak (dontCheck (self.callHackage "streamly" "0.7.3.1" { }));

    # These may not be necessary once we upgrade to GHC 9.2
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
