{ inputs, ... }:
{
  perSystem = { pkgs, lib, system, ... }:
    let
      version = "2.0.61";
      # Node/npm packaging comes from the repo's existing nixpkgs-unstable input
      # (already there for tooling that needs a modern Node) — used ONLY here, so
      # the haskell packages, devshell and every other service keep building
      # against the main pin exactly as before.
      #
      # Why not the main pin: it is from Oct 2023, which predates importNpmLock.
      # importNpmLock takes the dependency hashes from the upstream
      # package-lock.json, so no npmDepsHash is maintained in this repo — the
      # db-manager-src rev in flake.lock pins the lockfile, which pins every
      # dependency. It also fetches each dependency as its own fixed-output
      # derivation, so a dropped connection retries one package instead of
      # restarting the whole prefetch.
      pkgsNpm = import inputs.nixpkgs-unstable { inherit system; };
      nodejs = pkgsNpm.nodejs_20;
      srcOf = sub: inputs.db-manager-src + "/${sub}";
      # npmRoot is where package-lock.json lives; importNpmLock reads the
      # integrity hashes from it and turns each entry into a fetch.
      npmDepsFor = sub: {
        npmDeps = pkgsNpm.importNpmLock { npmRoot = srcOf sub; };
        inherit (pkgsNpm.importNpmLock) npmConfigHook;
      };
    in
    {
      packages = {
        db-manager-backend = pkgsNpm.buildNpmPackage ({
          pname = "db-manager-backend";
          inherit version;
          src = srcOf "backend";
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
        } // npmDepsFor "backend");

        db-manager-frontend = pkgsNpm.buildNpmPackage ({
          pname = "db-manager-frontend";
          inherit version;
          src = srcOf "frontend";
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
        } // npmDepsFor "frontend");
      };
    };
}
