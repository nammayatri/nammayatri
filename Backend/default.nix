{ ... }:
let
  sources = import ../nix/sources.nix;
in
{
  imports = [
    ./nix/debug.nix
    ./nix/docker.nix
    ./nix/scripts.nix
    ./nix/run-mobility-stack.nix
    ./nix/arion-configuration.nix
    ./nix/osrm.nix
    ./load-test
  ];
  perSystem = { config, self', pkgs, lib, system, ... }:
    let
      # Packages to exclude from CI builds (not needed for production deployment)
      ciExcludedPackages = [
        "hunit-tests"
        "beckn-test"
        "image-api-helper"
        "route-extractor"
        "mock-public-transport-provider-platform"
        "public-transport-rider-platform"
        "public-transport-search-consumer"
        "mock-idfy"
        "mock-payment"
        "mock-google"
        "mock-rider-platform"
        "sdk-event-pipeline"
        "special-zone"
        "example-service"
        "safety-dashboard"
        "search-result-aggregator"
        "beckn-cli"
        "alchemist"
        "arion"
        "load-test-dev"
        "load-test-prepare"
        "mock-fcm"
        "mock-sms"
        "osrm-server"
        "osrm-data"
        "run-mobility-stack-dev"
        "run-mobility-stack-nix"
      ];
      # Apps to exclude from CI builds (devour-flake via om ci run)
      ciExcludedApps = [
        "hunit-tests"
        "beckn-cli-exe"
        "image-api-helper-exe"
        "route-extractor-exe"
        "mock-public-transport-provider-platform-exe"
        "public-transport-rider-platform-exe"
        "public-transport-search-consumer-exe"
        "mock-idfy-exe"
        "mock-payment-exe"
        "mock-google-exe"
        "mock-rider-platform-exe"
        "sdk-event-pipeline-exe"
        "special-zone-exe"
        "example-service-exe"
        "safety-dashboard-exe"
        "search-result-aggregator-exe"
        "mock-fcm-exe"
        "mock-sms-exe"
        "trace"
        "alchemist-generator-exe"
      ];
      cacConfig = p: p.overrideAttrs (oa: {
        inherit (config.haskellProjects.default.outputs.finalPackages) cac_client;
        preBuild = ''
          export ${if pkgs.stdenv.isLinux then "LD_LIBRARY_PATH" else "DYLD_LIBRARY_PATH"}="$cac_client/lib";
        '';
        nativeBuildInputs = (oa.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
        postFixup = (oa.postFixup or "") + ''
          if [-d $bin/bin/* ]; then
            wrapProgram $bin/bin/* \
              --set ${if pkgs.stdenv.isLinux then "LD_LIBRARY_PATH" else "DYLD_LIBRARY_PATH"} "$cac_client/lib"
          fi
        '';
      });
      gtfstidy = with pkgs; buildGoModule rec {
        pname = "gtfstidy";
        version = "deceaaaea84c61392642bff40468ba79ef2fc9dc";
        src = pkgs.fetchFromGitHub {
          owner = "patrickbr";
          repo = "gtfstidy";
          rev = "${version}";
          hash = "sha256-PsGENVqSelmpXkVtMy9MdJALFAv2mTW7A11QzuZ/E8w=";
        };
        doCheck = false;
        vendorHash = "sha256-TH8oCyZ5ZThKwwraa/qgr0Jf4kQNOz2PZxvv1dn/yVA=";
        meta = {
          description = "A tool for checking, sanitizing and minimizing GTFS feeds";
          homepage = "https://github.com/patrickbr/gtfstidy";
          license = lib.licenses.gpl2;
          maintainers = with lib.maintainers; [ patrickbr ];
        };
      };
      cac_client = pkgs.rustPlatform.buildRustPackage {
        pname = "cac_client";
        version = "0.0.1";
        src = sources.haskell-cac + "/rust/cac_client";
        cargoLock.lockFile = sources.haskell-cac + "/rust/cac_client/Cargo.lock";
        nativeBuildInputs = [ pkgs.pkg-config ];
        buildInputs = [ pkgs.openssl pkgs.libiconv ]
          ++ lib.optionals pkgs.stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
            Security SystemConfiguration
          ]);
      };
      localBuild = builtins.getEnv "NIX_LOCAL_BUILD" != "";
    in
    {
      pre-commit.settings.imports = [
        ./nix/pre-commit.nix
      ];
      pre-commit.settings.hooks.treefmt.enable = lib.mkForce false;
      treefmt.config.settings.formatter.ormolu.excludes = [
        "vira.hs"
      ];

      haskellProjects.default = {
        projectRoot = ./.;
        # Inlined from beckn-gateway, shared-kernel, euler-hs, and haskell-cac
        # haskellFlakeProjectModules (previously imported as flake inputs).
        # "packages" and "apps" are excluded from autoWire so we can filter out
        # ciExcludedPackages from the flake's top-level outputs (used by devour-flake in om ci run).
        autoWire = [ "checks" ];
        devShell.tools = _: { };
        # NOTE: aarch64-linux builds are currently experiencing an assembly error:
        # "/build/ghc612_0/ghc_329.s:160652:0: error: conditional branch out of range"
        # This is a known issue with GHC's code generation on ARM architectures.
        # Temporary fix: Optimization level has been reduced for ARM Linux builds.
        # TODO: Monitor does this optimization cause any perf issue
        defaults.settings.defined = {
          extraConfigureFlags =
            (lib.optional (system == "aarch64-linux") "--ghc-options=-O1")
            ++ (lib.optionals localBuild [
              "--ghc-options=-O0"
              "--ghc-options=-funfolding-use-threshold20"
              "--ghc-options=-fno-cse"
              "--ghc-options=-fmax-simplifier-iterations1"
              "--ghc-options=-fno-specialise-aggressively"
            ]);
        };
        packages = {
          # Local to nammayatri
          amazonka.source = sources.amazonka-git + "/lib/amazonka";
          amazonka-core.source = sources.amazonka-git + "/lib/amazonka-core";
          amazonka-test.source = sources.amazonka-git + "/lib/amazonka-test";
          amazonka-sso.source = sources.amazonka-git + "/lib/services/amazonka-sso";
          amazonka-sts.source = sources.amazonka-git + "/lib/services/amazonka-sts";
          amazonka-ses.source = sources.amazonka-git + "/lib/services/amazonka-ses";
          streamly.source = "0.8.3";
          unicode-data.source = "0.3.1";
          namma-dsl.source = sources.namma-dsl + "/lib/namma-dsl";
          json-logic-hs.source = sources.json-logic-hs;
          google-cloud-pubsub.source = sources.google-cloud-haskell + "/lib/google-cloud-pubsub";
          google-cloud-common.source = sources.google-cloud-haskell + "/lib/google-cloud-common";

          # From haskell-cac
          haskell-cac.source = sources.haskell-cac + "/haskell";

          # From beckn-gateway
          beckn-gateway.source = sources.beckn-gateway + "/app/gateway";
          mock-registry.source = sources.beckn-gateway + "/app/mock-registry";

          # From shared-kernel
          mobility-core.source = sources.shared-kernel + "/lib/mobility-core";
          passetto-client.source = sources.passetto-hs + "/client";
          passetto-core.source = sources.passetto-hs + "/core";

          # From euler-hs
          euler-hs.source = sources.euler-hs;
          hedis.source = sources.hedis;
          cereal.source = sources.cereal;
          servant-mock.source = sources.servant-mock;
          tinylog.source = sources.tinylog;

          # From euler-events-hs
          euler-events-hs.source = sources.euler-events-hs;

          # From juspay-extra
          juspay-extra.source = sources.juspay-extra;

          # From sequelize (+ beam deps)
          sequelize.source = sources.sequelize;
          beam-core.source = sources.beam + "/beam-core";
          beam-migrate.source = sources.beam + "/beam-migrate";
          beam-sqlite.source = sources.beam + "/beam-sqlite";
          beam-postgres.source = sources.beam + "/beam-postgres";
          beam-mysql.source = sources.beam-mysql;
          mysql-haskell.source = sources.mysql-haskell;
          bytestring-lexing.source = sources.bytestring-lexing;
          word24.source = sources.word24;

          # From clickhouse-haskell
          clickhouse-haskell.source = sources.clickhouse-haskell;

          # From prometheus-haskell
          prometheus-client.source = sources.prometheus-haskell + "/prometheus-client";
          prometheus-metrics-ghc.source = sources.prometheus-haskell + "/prometheus-metrics-ghc";
          prometheus-proc.source = sources.prometheus-haskell + "/prometheus-proc";
          wai-middleware-prometheus.source = sources.prometheus-haskell + "/wai-middleware-prometheus";
        };
        settings = {
          alchemist.custom = cacConfig;
          beckn-cli.custom = cacConfig;
          provider-dashboard.custom = cacConfig;
          example-service.custom = cacConfig;
          mock-fcm.custom = cacConfig;
          mock-google.custom = cacConfig;
          mock-public-transport-provider-platform.custom = cacConfig;
          mock-rider-platform.custom = cacConfig;
          mock-sms.custom = cacConfig;
          mock-payment.custom = cacConfig;
          public-transport-rider-platform.custom = cacConfig;
          public-transport-search-consumer.custom = cacConfig;
          rider-app.custom = cacConfig;
          search-result-aggregator.custom = cacConfig;
          rider-app-drainer.custom = cacConfig;
          special-zone.custom = cacConfig;
          image-api-helper.custom = cacConfig;
          route-extractor.custom = cacConfig;
          dynamic-offer-driver-app.custom = cacConfig;
          producer.custom = cacConfig;
          dynamic-offer-driver-drainer.custom = cacConfig;
          lib-dashboard.custom = cacConfig;
          kafka-consumers.custom = cacConfig;
          driver-offer-allocator.custom = cacConfig;
          beckn-test.custom = cacConfig;
          rider-dashboard.custom = cacConfig;


          namma-dsl.libraryProfiling = false;
          location-updates.check = false;
          beckn-test.check = false;
          singletons-th.jailbreak = true;
          singletons-base = {
            jailbreak = true;
            check = false;
          };
          streamly = {
            extraBuildDepends = lib.optional pkgs.stdenv.isDarwin pkgs.darwin.apple_sdk.frameworks.Cocoa;
            jailbreak = true;
          };
          base32.jailbreak = true;
          amazonka-core.check = false;
          cryptostore.check = false;
          google-cloud-pubsub.check = false;
          google-cloud-common.check = false;

          # From haskell-cac: cac_client C library
          cac_client.custom = _: cac_client;

          # From shared-kernel settings
          generic-deriving.check = false;

          # From euler-hs settings
          hedis = { jailbreak = true; check = false; };
          tinylog = { jailbreak = true; check = false; };
          servant-mock.check = false;
          cereal.check = false;

          # From sequelize/beam deps
          bytestring-lexing = { jailbreak = true; check = false; };
          beam-core.jailbreak = true;
          beam-migrate.jailbreak = true;
          beam-mysql.jailbreak = true;
          beam-sqlite = { jailbreak = true; check = false; };
          beam-postgres = { jailbreak = true; check = false; };
          mysql-haskell = { jailbreak = true; check = false; };
          word24 = { jailbreak = true; check = false; broken = false; };
          tcp-streams = { jailbreak = true; check = false; };
          wire-streams = { jailbreak = true; check = false; };
          binary-parsers = { jailbreak = true; check = false; };

          # From prometheus-haskell
          prometheus-client.check = false;
          wai-middleware-prometheus.check = false;

          # Disable checks for upstream packages with version bound issues
          servant.check = false;
          servant-server.check = false;
          servant-client.check = false;
          servant-client-core.check = false;
          euler-hs.check = false;
          euler-events-hs.check = false;
          passetto-client.check = false;
          passetto-core.check = false;
          sequelize.check = false;
          clickhouse-haskell.check = false;
        };
      };

      packages =
        let
          # Manually wire haskell packages, excluding ciExcludedPackages.
          # This ensures devour-flake (via om ci run) does not build them.
          allHaskellPackages = config.haskellProjects.default.outputs.packages;
          filteredHaskellPackages = lib.mapAttrs
            (_: p: p.package)
            (lib.filterAttrs
              (name: _: !(builtins.elem name ciExcludedPackages))
              allHaskellPackages);

          localCabalPackages = builtins.map
            (p: if p.exes != { } then lib.getBin p.package else null)
            (lib.filter
              (p: !(builtins.elem p.package.pname ciExcludedPackages))
              (lib.attrValues allHaskellPackages));
        in
        filteredHaskellPackages // {
          # The final nammayatri package containing the various executables and
          # configuration files.
          nammayatri = pkgs.symlinkJoin {
            name = "nammayatri";
            paths = localCabalPackages;
            postBuild = ''
              # Prepare /opt/app layout for Docker image.
              # Rationale: Our k8s deployment config is hardcoded to look for exes
              # under /opt/app.
              mkdir -p $out/opt/app
              for f in `${lib.getExe pkgs.fd} . $out/bin/`; do
                ln -s $f $out/opt/app/
              done
              cp -r ${./dhall-configs} $out/opt/app/dhall-configs
              cp -r ${./swagger} $out/opt/app/swagger
            '';
          };
        };

      apps =
        let
          allHaskellApps = config.haskellProjects.default.outputs.apps;
        in
        lib.filterAttrs (name: _: !(builtins.elem name ciExcludedApps)) allHaskellApps;

      devShells.backend = pkgs.mkShell {
        name = "ny-backend";
        meta.description = "Backend development environment for nammayatri";
        packages = with pkgs; [
          redis # redis-cli is used in scripts.nix
          openssh # sftp binary for SFTP upload support
          sshpass # For password-based SFTP authentication
          jq
          gdal
          postgis
          zbar
          newman
          config.mission-control.wrapper
          gtfstidy
        ] ++ (lib.optionals pkgs.stdenv.isLinux [
          # TODO: Use wkhtmltopdf  (with macOS support) after updating common/nixpkgs-latest
          # common/nixpkgs-latest update requires https://github.com/NixOS/nixpkgs/pull/465567
          pkgs.wkhtmltopdf-bin
        ]);
        # cf. https://haskell.flake.page/devshell#composing-devshells
        inputsFrom = [
          config.pre-commit.devShell
          config.haskellProjects.default.outputs.devShell
          config.flake-root.devShell
        ];
        shellHook =
          let cacLib = "${config.haskellProjects.default.outputs.finalPackages.cac_client}/lib"; in
          ''
            export LIBRARY_PATH="${cacLib}"
            export LD_LIBRARY_PATH="${cacLib}"
            export DYLD_LIBRARY_PATH="${cacLib}"
          '';
      };
    };
}
