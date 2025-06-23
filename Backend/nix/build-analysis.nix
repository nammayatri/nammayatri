# Run the python scripts from ./scripts to generate build analysis reports in `nammayatriMetadata/<package-name>` output of the package using this setting
{ pkgs, name, self, lib, mkCabalSettingOptions, ... }:
let
  inherit (lib) types;
  scriptsDir = ./scripts;
in
{
  options = mkCabalSettingOptions {
    name = "buildAnalysis";
    type = types.bool;
    description = "Enable build analysis powered by juspay/spider";
    impl = enable: drv:
      if enable then
        drv.overrideAttrs
          (oldAttrs: {
            outputs = oldAttrs.outputs ++ [ "nammayatriMetadata" ];
            buildInputs = (oldAttrs.buildInputs or [ ]) ++ (with pkgs; [
              (python311.withPackages (p: with p; [ websockets aiohttp boto3 ]))
            ]);

            preBuildHooks = [
              ''
                # Create cleanup function
                cleanup() {
                  if [ -f server.pid ]; then
                    echo "Stopping fdep server..."
                    kill $(cat server.pid) 2>/dev/null || true
                    rm -f server.pid
                  fi
                  rm -f fdep_port
                }

                # Set trap for cleanup
                trap cleanup EXIT INT TERM

                echo "Running fdep server..."
                python3 @server@ &
                echo $! > server.pid

                # Wait for port file with timeout
                TIMEOUT=30
                COUNTER=0
                while [ ! -f fdep_port ]; do
                  if [ $COUNTER -ge $TIMEOUT ]; then
                    echo "Timeout waiting for fdep server to start"
                    exit 1
                  fi
                  sleep 1
                  COUNTER=$((COUNTER + 1))
                done

                if [ ! -f fdep_port ]; then
                  echo "Failed to create fdep_port file"
                  exit 1
                fi

                export FDEP_SOCKET_PATH=$(cat fdep_port)
                echo $FDEP_SOCKET_PATH
                if [ -z "$FDEP_SOCKET_PATH" ]; then
                  echo "Failed to get valid server port"
                  exit 1
                fi

                export NIX_BUILD_API_CONTRACT=True

              ''
            ];

            postBuildHooks = [
              ''
                if [ -f server.pid ]; then
                  echo "Creating fdep zip..."
                  python3 @tmpzip@ --output-path $juspayMetadata/@packageName@/fdep.zip || {
                    echo "Failed to create fdep.zip"
                    exit 1
                  }
                  echo "fdep output zipping complete - output saved to $juspayMetadata/@packageName@"

                  # Server cleanup is handled by the trap
                  kill $(cat server.pid) 2>/dev/null || true
                fi

              ''
            ];

            preInstallHooks = [
              ''
                mkdir -p $nammayatriMetadata/${name}
              ''
            ];
          })
      else drv;
  };
}
