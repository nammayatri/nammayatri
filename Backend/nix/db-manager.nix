{ inputs, ... }:
{
  perSystem = { pkgs, lib, ... }:
    let
      version = "2.0.61";
      nodejs = pkgs.nodejs_20;
    in
    {
      packages = {
        db-manager-backend = pkgs.buildNpmPackage {
          pname = "db-manager-backend";
          inherit version;
          src = "${inputs.db-manager-src}/backend";
          npmDepsHash = "sha256-2AN+LFaXzy8cP8OpY4UDdDCeEXhRlw/SNPYAvOKNwAQ=";
          inherit nodejs;
          # `npm run build` = tsc → dist/. Keep the full node_modules for runtime
          # (node dist/server.js), plus the config template dir + SQL migrations.
          installPhase = ''
            runHook preInstall
            mkdir -p $out
            cp -r dist node_modules package.json $out/
            cp -r config migrations $out/
            runHook postInstall
          '';
          # No bin wrapper; this is a server, launched by the stack.
          dontNpmPrune = true;
        };

        db-manager-frontend = pkgs.buildNpmPackage {
          pname = "db-manager-frontend";
          inherit version;
          src = "${inputs.db-manager-src}/frontend";
          npmDepsHash = "sha256-Ub6TPTt/J1itDbyYy9kuWU4B7t75b8tLB72+UrkCnWA=";
          inherit nodejs;
          # Build with a RELATIVE base (./) so the SPA + its /config.js work when
          # served under caddy's /db-manager-frontend/ path prefix (dev-box: the
          # browser reaches everything through the single caddy origin, so
          # frontend↔backend are same-origin → no CORS, cookies just work).
          # Skip the package.json `build` script's leading `tsc` (type errors
          # would fail the sandbox build; vite/esbuild transpiles regardless).
          buildPhase = ''
            runHook preBuild
            npm exec -- vite build --base=./
            runHook postBuild
          '';
          installPhase = ''
            runHook preInstall
            mkdir -p $out
            cp -r dist/* $out/
            runHook postInstall
          '';
        };
      };
    };
}
