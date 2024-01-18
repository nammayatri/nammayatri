# process-compose module for running the nammayatri stack
ny:
{ config, pkgs, lib, ... }:
let
  inherit (lib) types;
in
{
  options = {
    services.nammayatri = lib.mkOption {
      type = types.submodule {
        options = {
          enable = lib.mkEnableOption "Enable nammayatri stack";
          useCabal = lib.mkEnableOption "Use cabal instead of Nix";
        };
      };
    };
  };

  config =
    let
      pcLib = import ny.inputs.common.inputs.process-compose-flake.lib { inherit lib; };
      cfg = config.services.nammayatri;
      # The cabal executables we want to run as part of the nammayatri service
      # group.
      cabalExecutables = [
        "driver-offer-allocator-exe"
        "dynamic-offer-driver-app-exe"
        "dynamic-offer-driver-drainer-exe"
        "rider-app-drainer-exe"
        "image-api-helper-exe"
        "kafka-consumers-exe"
        "mock-fcm-exe"
        "mock-google-exe"
        "mock-idfy-exe"
        "mock-sms-exe"
        "provider-dashboard-exe"
        "producer-exe"
        "public-transport-rider-platform-exe"
        "public-transport-search-consumer-exe"
        "rider-app-exe"
        "rider-dashboard-exe"
        "scheduler-example-app-exe"
        # "scheduler-example-scheduler-exe"
        "search-result-aggregator-exe"
        "special-zone-exe"
      ];

      haskellProcessFor = name:
        if cfg.useCabal
        then
          let
            # Which Haskell package contains the given cabal executable?
            cabalTargetForExe = lib.listToAttrs (lib.flatten (lib.mapAttrsToList
              (name: info: map (exe: lib.nameValuePair exe "${name}:exe:${exe}") (lib.attrNames info.exes))
              ny.config.haskellProjects.default.outputs.packages));
          in
          {
            command =
              "set -x; cabal run ${cabalTargetForExe.${name}}";
            environment = {
              CABAL_TARGET = cabalTargetForExe.${name};
            };
          }
        else {
          command = ny.config.apps.${name}.program;
        };

      haskellProcesses = {
        processes = lib.listToAttrs (builtins.map
          (name: {
            inherit name;
            value = {
              imports = [ (haskellProcessFor name) ];
              log_location = "${name}.log";
              working_dir = "Backend";
              depends_on."nammayatri-init".condition = "process_completed_successfully";
            };
          })
          cabalExecutables);
      };

      initProcesses = {
        processes = {
          nammayatri-init = {
            working_dir = "Backend";
            depends_on."cabal-build".condition = "process_completed_successfully";
            command = pkgs.writeShellApplication {
              name = "run-mobility-stack-init";
              runtimeInputs = with pkgs; [
                redis
              ];
              text = ''
                set -x
                rm -f ./*.log # Clean up the log files
                redis-cli -p 30001 -c XGROUP CREATE Available_Jobs myGroup  0 MKSTREAM # TODO: remove this once cluster funtions from euler are fixed
                redis-cli XGROUP CREATE Available_Jobs myGroup 0 MKSTREAM
              '';
            };
          };

          cabal-build = {
            working_dir = "Backend";
            disabled = !cfg.useCabal;
            command = pkgs.writeShellApplication {
              name = "cabal-build";
              text =
                let
                  # The cabal target of all Haskell processes.
                  cabalTargets =
                    lib.filter (x: x != null)
                      (builtins.map
                        (p:
                          pcLib.lookupEnv "CABAL_TARGET" (p.environment or null))
                        (lib.attrValues config.settings.processes));
                in
                ''
                  set -x
                  cabal build ${builtins.concatStringsSep " " cabalTargets}
                '';
            };
          };
        };
      };
    in
    {
      settings = {
        imports = [
          initProcesses
          haskellProcesses
        ];
        processes = {
          kafka-consumers-exe = {
            environment = {
              CONSUMER_TYPE = "AVAILABILITY_TIME";
            };
          };
        };
      };
    };
}
