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
    perSystem = { self', pkgs, lib, ... }:
      let
        exes = builtins.map pkgs.haskell.lib.justStaticExecutables (with self'.packages; [
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
      in
      {
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
                ] ++ exes;
                name = "beckn-root";
                pathsToLink = [ "/Backend/dhall-configs" "/Backend/swagger" "/bin" ];
                postBuild = ''
                  # Legacy k8s deployment expects the app to be in /opt/app
                  mkdir $out/opt
                  cp -r $out/bin $out/opt/app
                '';
              };
              config = {
                Env = [
                  "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
                  # Ref: https://hackage.haskell.org/package/x509-system-1.6.7/docs/src/System.X509.Unix.html#getSystemCertificateStore
                  "SYSTEM_CERTIFICATE_PATH=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
                ];
                Cmd = [ "${pkgs.lib.getExe self'.packages.default}" ];
              };
            };
        };
      };
  };
}
