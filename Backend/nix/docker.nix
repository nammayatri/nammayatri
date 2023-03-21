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
          let
            # Our k8s deployment config is hardcoded to look for exes under
            # /opt/app
            nammayatri-in-opt = pkgs.symlinkJoin {
              name = "nammayatri-exes-opt";
              paths = [ self'.packages.nammayatri ];
              postBuild = ''
                mkdir $out/opt && mv $out/bin $out/opt/app
              '';
            };
          in
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
                nammayatri-in-opt
              ];
              name = "beckn-root";
              pathsToLink = [
                "/Backend/dhall-configs"
                "/Backend/swagger"
                "/bin"
                "/opt"
              ];
            };
            config = {
              Env = [
                "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
                # Ref: https://hackage.haskell.org/package/x509-system-1.6.7/docs/src/System.X509.Unix.html#getSystemCertificateStore
                "SYSTEM_CERTIFICATE_PATH=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
              ];
              Cmd = [ "${self'.packages.nammayatri}/bin/rider-app-exe" ];
            };
          };
      };
    };
  };
}
