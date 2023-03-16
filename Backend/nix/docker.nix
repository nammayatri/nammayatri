{ self, ... }:

let
  imageName = "ghcr.io/nammayatri/nammayatri";
  # self.rev will be non-null only when the working tree is clean
  # This is equivalent to `git rev-parse --short HEAD`
  imageTag = builtins.substring 0 6 (self.rev or "dev");
in
{
  config = {
    flake.dockerImageName = imageName + ":" + imageTag;
    flake.dockerImageTag = imageTag;
    perSystem = { self', pkgs, lib, ... }: {
      packages = lib.optionalAttrs pkgs.stdenv.isLinux {
        dockerImage =
          # TODO: Build a layered image, separating tools and packages
          pkgs.dockerTools.buildImage {
            name = imageName;
            created = "now";
            tag = imageTag;
            copyToRoot = pkgs.buildEnv {
              paths = with pkgs; [
                cacert
                awscli
                coreutils
                bash
                # Add project root to paths to copy dhall-configs and swagger dirs
                self
              ] ++ (with self'.packages;
                let inherit (pkgs.haskell.lib) justStaticExecutables;
                # TODO: Refactor
                in builtins.map justStaticExecutables [
                  rider-app
                  dynamic-offer-driver-app
                  static-offer-driver-app
                  static-offer-driver-app-allocator
                  static-offer-driver-app-scheduler
                  driver-offer-allocator
                  rider-dashboard
                  provider-dashboard
                  driver-tracking-healthcheck
                ]);
              name = "beckn-root";
              pathsToLink = [ "/dhall-configs" "/swagger" "/bin" ];
              postBuild = ''
                # Legacy k8s deployment expects the app to be in /opt/app
                mkdir $out/opt
                cp -r $out/bin $out/opt/app
              '';
            };
            config = {
              Env = [ "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" ];
              Cmd = [ "${pkgs.lib.getExe self'.packages.default}" ];
            };
          };
      };
    };
  };
}
