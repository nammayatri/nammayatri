{ self, pkgs, ... }:

let
  inputs = self.inputs;
in
{
  source-overrides = {
    # NOTE: The below boilerplate can be automated once
    # https://github.com/srid/haskell-flake/issues/84 is done.
    inherit (inputs)
      euler-hs
      sequelize
      beam-mysql
      mysql-haskell
      hedis
      ;
    beam-core = inputs.beam + /beam-core;
    beam-migrate = inputs.beam + /beam-migrate;
    beam-sqlite = inputs.beam + /beam-sqlite;
    beam-postgres = inputs.beam + /beam-postgres;
    mobility-core = inputs.beckn-shared-kernel + /lib/mobility-core;
    beckn-gateway = inputs.beckn-gateway + /app/gateway;
    mock-registry = inputs.beckn-gateway + /app/mock-registry;
    passetto-client = inputs.passetto-hs + /client;
    passetto-core = inputs.passetto-hs + /core;
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
    mysql-haskell = doJailbreak (dontCheck super.mysql-haskell); # memory constraint too narrow
    beam-core = doJailbreak super.beam-core;
    beam-migrate = doJailbreak super.beam-migrate;
    beam-mysql = doJailbreak (dontCheck super.beam-mysql);
    beam-sqlite = doJailbreak (dontCheck super.beam-sqlite);
    beam-postgres = doJailbreak (dontCheck super.beam-postgres);

    sequelize = dontCheck super.sequelize;
    euler-hs = appendPatch
      (doJailbreak (dontHaddock (dontCheck super.euler-hs)))
      ./euler-hs.patch;
    hedis = dontCheck super.hedis;
  };
}
