# Add a process-compose based package for running the entire backend stack.
nammayatriConfig:
{ config, lib, ... }:
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
      cfg = config.services.nammayatri;
      cabalTargetForExe = lib.listToAttrs (lib.flatten (lib.mapAttrsToList
        (name: info: map (exe: lib.nameValuePair exe "${name}:exe:${exe}") (lib.attrNames info.exes))
        nammayatriConfig.haskellProjects.default.outputs.packages));
      # A process-compose process representing the local Haskell package app.
      haskellApp = { name, ... }: {
        log_location = "${name}.log";
        environment = lib.optionalAttrs cfg.useCabal {
          CABAL_TARGET = cabalTargetForExe.${name};
        };
        command =
          if cfg.useCabal
          then "set -x; cabal run ${cabalTargetForExe.${name}}"
          else nammayatriConfig.apps.${name}.program;
      };
    in
    {
      settings = {
        processes = {
          # Local Haskell processes
          driver-offer-allocator-exe = {
            imports = [ haskellApp ];
          };
          dynamic-offer-driver-app-exe = {
            imports = [ haskellApp ];
          };
          dynamic-offer-driver-drainer-exe = {
            imports = [ haskellApp ];
          };
          rider-app-drainer-exe = {
            imports = [ haskellApp ];
          };
          image-api-helper-exe = {
            imports = [ haskellApp ];
          };
          kafka-consumers-exe = {
            imports = [ haskellApp ];
            environment = {
              CONSUMER_TYPE = "AVAILABILITY_TIME";
            };
          };
          mock-fcm-exe = {
            imports = [ haskellApp ];
          };
          mock-google-exe = {
            imports = [ haskellApp ];
          };
          mock-idfy-exe = {
            imports = [ haskellApp ];
          };
          mock-sms-exe = {
            imports = [ haskellApp ];
          };
          provider-dashboard-exe = {
            imports = [ haskellApp ];
          };
          producer-exe = {
            imports = [ haskellApp ];
          };
          public-transport-rider-platform-exe = {
            imports = [ haskellApp ];
          };
          public-transport-search-consumer-exe = {
            imports = [ haskellApp ];
          };
          rider-app-exe = {
            imports = [ haskellApp ];
          };
          rider-dashboard-exe = {
            imports = [ haskellApp ];
          };
          scheduler-example-app-exe = {
            imports = [ haskellApp ];
          };
          # scheduler-example-scheduler-exe = {
          #   imports = [ haskellApp ];
          # };
          search-result-aggregator-exe = {
            imports = [ haskellApp ];
          };
          special-zone-exe = {
            imports = [ haskellApp ];
          };
        };
      };
    };
}
