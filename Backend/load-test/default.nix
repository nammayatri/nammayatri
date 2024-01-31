{ inputs, ... }:
{
  perSystem = perSystem@{ inputs', self', pkgs, lib, ... }:
    let
      myPython = pkgs.python310.withPackages (ps: with ps; [
        locust
        roundrobin
        flask
        requests
        psycopg2
      ]);
      common = { name, ... }: {
        namespace = "load-test";
        log_location = "${name}.log";
      };
      waitForNammayatri = {
        depends_on."load-test-init".condition = "process_completed_successfully";
      };
    in
    {
      packages.load-test-prepare = pkgs.writeShellApplication {
        name = "load-test-prepare";
        runtimeInputs = [ ];
        text = ''
          cp Backend/dhall-configs/dev/secrets/top-secret-template.dhall Backend/dhall-configs/dev/secrets/top-secret.dhall
        '';
      };

      process-compose.load-test-dev = {
        imports = [
          (import ../nix/services/nammayatri.nix { inherit (perSystem) config self' inputs'; inherit inputs; })
        ];
        apiServer = false;
        services.nammayatri.enable = true;

        preHook = ''
          rm -rf Backend/load-test/output
          rm -rf data
        '';

        postHook = ''
          echo "Load test completed"
          cat Backend/load-test/output/riderApp/riderApp.csv_stats.csv
          cat Backend/load-test/output/driverApp/driverOffer.csv_stats.csv
        '';

        settings.processes = {
          # Temporarily disable
          driver-offer-allocator-exe.disabled = true;
          dynamic-offer-driver-drainer-exe.disabled = true;
          rider-app-drainer-exe.disabled = true;
          rider-app-scheduler-exe.disabled = true;
          image-api-helper-exe.disabled = true;
          kafka-consumers-exe.disabled = true;
          mock-fcm-exe.disabled = false;
          mock-google-exe.disabled = false;
          mock-idfy-exe.disabled = false;
          mock-sms-exe.disabled = false;
          provider-dashboard-exe.disabled = true;
          producer-exe.disabled = true;
          public-transport-rider-platform-exe.disabled = true;
          public-transport-search-consumer-exe.disabled = true;
          rider-app-exe.disabled = false;
          dynamic-offer-driver-app-exe.disabled = false;
          rider-dashboard-exe.disabled = true;
          search-result-aggregator-exe.disabled = true;
          special-zone-exe.disabled = true;

          load-test-init = {
            imports = [ common ];
            command = "echo Waiting for load-test dependencies to start";
            depends_on = {
              "rider-app-exe".condition = "process_healthy";
              "dynamic-offer-driver-app-exe".condition = "process_healthy";
            };
          };
          create-drivers = {
            imports = [
              common
              waitForNammayatri
            ];
            command = pkgs.writeShellApplication {
              name = "create-drivers";
              runtimeInputs = [ myPython ];
              text = ''
                sleep 5
                python3 Backend/load-test/createDrivers.py
                sleep 3
              '';
            };
          };
          auth = {
            imports = [
              common
              waitForNammayatri
            ];
            command = pkgs.writeShellApplication {
              name = "auth";
              runtimeInputs = [ myPython ];
              text = ''
                sleep 5
                python3 Backend/load-test/setup/auth.py
                sleep 3
              '';
            };
            depends_on."create-drivers".condition = "process_completed";
          };
          share-otp = {
            imports = [
              common
              waitForNammayatri
            ];
            command = pkgs.writeShellApplication {
              name = "share-otp";
              runtimeInputs = [
                myPython
              ];
              text = ''
                python3 Backend/load-test/services/shareOTP.py
              '';
            };
          };
          update-location-service = {
            imports = [
              common
              waitForNammayatri
            ];
            command = pkgs.writeShellApplication {
              name = "location-update";
              runtimeInputs = [ myPython ];
              text = ''
                python3 Backend/load-test/setup/locationUpdateService.py
              '';
            };
            depends_on."auth".condition = "process_completed_successfully";
          };
          load-test-rider = {
            imports = [
              common
              waitForNammayatri
            ];
            command = pkgs.writeShellApplication {
              name = "load-test-rider";
              runtimeInputs = [ myPython ];
              text = ''
                locust --headless --run-time 2m --users 50  --only-summary --html Backend/load-test/output/riderApp/riderApp.html --csv  Backend/load-test/output/riderApp/riderApp.csv -f Backend/load-test/scripts/riderApp.py
              '';
            };
            depends_on."auth".condition = "process_completed_successfully";
          };
          load-test-driver = {
            imports = [
              common
              waitForNammayatri
            ];
            command = pkgs.writeShellApplication {
              name = "load-test-driver";
              runtimeInputs = [ myPython ];
              text = ''
                locust --headless --run-time 2m --users 50 --only-summary  --html Backend/load-test/output/driverApp/driverOffer.html --csv Backend/load-test/output/driverApp/driverOffer.csv -f Backend/load-test/scripts/driverOffer.py
              '';
            };
            depends_on."auth".condition = "process_completed_successfully";
          };
          load-test-exit = {
            imports = [ common ];
            command = "echo Waiting for load-test processes to end";
            depends_on = {
              "load-test-driver".condition = "process_completed_successfully";
              "load-test-rider".condition = "process_completed_successfully";
            };
            availability.exit_on_end = true;
          };
        };
      };
    };
}
