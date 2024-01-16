_:
{
  perSystem = { self', pkgs, lib, ... }:
    let
      myPython = pkgs.python310.withPackages (ps: with ps; [
        locust
        roundrobin
        flask
        requests
        psycopg2
      ]);
    in
    {
      process-compose.load-test-dev = {
        settings.processes = {
          create-drivers = {
            command = pkgs.writeShellApplication {
              name = "create-drivers";
              runtimeInputs = [ myPython ];
              text = ''
                python3 Backend/load-test/createDrivers.py
                sleep 3
              '';
            };
            log_location = "Backend/create-drivers.log";
          };
          auth = {
            command = pkgs.writeShellApplication {
              name = "auth";
              runtimeInputs = [ myPython ];
              text = ''
                python3 Backend/load-test/setup/auth.py
                sleep 3
              '';
            };
            log_location = "Backend/auth.log";
            depends_on."create-drivers".condition = "process_completed";
          };
          share-otp = {
            command = pkgs.writeShellApplication {
              name = "hare-otp";
              runtimeInputs = [
                myPython
              ];
              text = ''
                python3 Backend/load-test/services/shareOTP.py
              '';
            };
            log_location = "Backend/share-otp.log";
          };
          update-location-service = {
            command = pkgs.writeShellApplication {
              name = "location-update";
              runtimeInputs = [ myPython ];
              text = ''
                python3 Backend/load-test/setup/locationUpdateService.py
              '';
            };
            log_location = "Backend/update-location-service.log";
            depends_on."auth".condition = "process_completed_successfully";
          };
          load-test-rider = {
            command = pkgs.writeShellApplication {
              name = "load-test-rider";
              runtimeInputs = [ myPython ];
              text = ''
                locust --headless --run-time 2m --users 50  --only-summary --html Backend/load-test/output/riderApp.html --csv  Backend/load-test/output/riderApp.csv -f Backend/load-test/scripts/riderApp.py
              '';
            };
            depends_on."auth".condition = "process_completed_successfully";
            log_location = "Backend/load-test-rider.log";
          };
          load-test-driver = {
            command = pkgs.writeShellApplication {
              name = "load-test-rider";
              runtimeInputs = [ myPython ];
              text = ''
                locust --headless --run-time 2m --users 50 --only-summary  --html Backend/load-test/output/driverOffer.html --csv Backend/load-test/output/driverOffer.csv -f Backend/load-test/scripts/driverOffer.py
              '';
            };
            depends_on."auth".condition = "process_completed_successfully";
            log_location = "Backend/load-test-driver.log";
          };
        };
      };
    };
}
