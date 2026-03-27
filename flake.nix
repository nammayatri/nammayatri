{
  nixConfig = {
    # Workaround https://github.com/nammayatri/nammayatri/pull/9493#issuecomment-2506672419
    max-call-depth = "1000000";
    # Nix cache
    extra-substituters = "https://cache.nixos.asia/oss";
    extra-trusted-public-keys = "oss:KO872wNJkCDgmGN3xy9dT89WAhvv13EiKncTtHDItVU=";
  };

  inputs = {
    common.url = "github:nammayatri/common";
    nixpkgs.follows = "common/nixpkgs";
    haskell-flake.follows = "common/haskell-flake";

    # Backend inputs
    shared-kernel = {
      url = "github:nammayatri/shared-kernel";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        common.follows = "common";
      };
    };

    namma-dsl = {
      url = "github:nammayatri/namma-dsl";
      inputs.common.follows = "common";
    };

    haskell-cac = {
      url = "github:piyushKumar-1/haskell_cac_client/Testing";
      inputs = {
        common.follows = "common";
        nixpkgs.follows = "common/nixpkgs"; # nix eval is failing in pipeline without giving proper error message #36 for nix update https://github.com/srid/nixci/issues/36
        crane.follows = "common/crane";
      };
    };

    beckn-gateway = {
      url = "github:nammayatri/beckn-gateway";
      inputs = {
        common.follows = "common";
        haskell-flake.follows = "haskell-flake";
        nixpkgs.follows = "nixpkgs";
        shared-kernel.follows = "shared-kernel";
      };
    };

    location-tracking-service = {
      url = "github:nammayatri/location-tracking-service";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "common/flake-parts";
        treefmt-nix.follows = "common/treefmt-nix";
        process-compose-flake.follows = "common/process-compose-flake";
        pre-commit-hooks-nix.follows = "common/pre-commit-hooks-nix";
        crane.follows = "common/crane";
        rust-overlay.follows = "common/rust-overlay";
        services-flake.follows = "services-flake";
      };
    };

    # https://github.com/nammayatri/passetto/pull/8
    passetto = {
      url = "github:nammayatri/passetto/use-crypton";
      inputs = {
        nixpkgs.follows = "common/nixpkgs";
        flake-parts.follows = "common/flake-parts";
        haskell-flake.follows = "common/haskell-flake";
        process-compose-flake.follows = "common/process-compose-flake";
        services-flake.follows = "services-flake";
      };
    };
    # Question: move this to common?
    services-flake.url = "github:juspay/services-flake";

    # Non-flake source dependencies (amazonka, google-cloud-haskell, json-logic-hs,
    # osrm-pbf) are pinned in Backend/nix/sources.nix via fetchTarball/fetchurl
    # to avoid the ~1.5s per-input verification overhead.
  };

  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      imports = [
        ./Backend/default.nix
        ./Frontend/default.nix
      ];
    };
}
