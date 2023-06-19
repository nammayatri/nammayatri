{
  inputs = {
    # Note: Note: change to upstream when upstream merged
    # common.url = "github:nammayatri/common";
    common.url = "github:arjunkathuria/common/Mobility-GHC927-Rebased";
    nixpkgs.follows = "common/nixpkgs";
    flake-parts.follows = "common/flake-parts";
    # Backend inputs
    # Note: Note: change to upstream when upstream merged
    # shared-kernel.url = "github:nammayatri/shared-kernel";
    shared-kernel.url = "github:arjunkathuria/shared-kernel/GHC-927-testing-snapshot";
    shared-kernel.inputs.nixpkgs.follows = "nixpkgs";

    # Note: Note: change to upstream when upstream merged
    # beckn-gateway.url = "github:nammayatri/beckn-gateway";
    beckn-gateway.url = "github:arjunkathuria/beckn-gateway/GHC-927-testing-snapshot";
    beckn-gateway.inputs.shared-kernel.follows = "shared-kernel";

    easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix";
    easy-purescript-nix.flake = false;

    # Amazonka 2.0 master branch
    amazonka-git.url = "github:brendanhay/amazonka?rev=3e66cc1b07f2818d0b4fc346310ecdb1c3c27599";
    amazonka-git.flake = false;
  };
  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      imports = [
        ./Backend/default.nix
        ./Frontend/default.nix
      ];
    };
}
