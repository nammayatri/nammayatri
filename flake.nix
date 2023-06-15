{
  inputs = {
    common.url = "github:nammayatri/common";

    # Backend inputs
    shared-kernel.url = "github:nammayatri/shared-kernel/updatedEulerHS";
    beckn-gateway.url = "github:nammayatri/beckn-gateway/updatedEulerHS";
    beckn-gateway.inputs.common.follows = "common";
    beckn-gateway.inputs.shared-kernel.follows = "shared-kernel";
    beckn-gateway.inputs.flake-parts.follows = "common/flake-parts";
    beckn-gateway.inputs.nixpkgs.follows = "common/nixpkgs";


    easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix";
    easy-purescript-nix.flake = false;
  };
  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      imports = [
        ./Backend/default.nix
        ./Frontend/default.nix
      ];
    };
}
