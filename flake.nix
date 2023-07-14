{
  inputs = {
    common.url = "github:nammayatri/common";

    # Backend inputs
    shared-kernel.url = "github:nammayatri/shared-kernel/9e6d7fdb1b093ad29c029c7c843b5414a57ce718";
    beckn-gateway.url = "github:nammayatri/beckn-gateway";
    beckn-gateway.inputs.common.follows = "common";
    beckn-gateway.inputs.shared-kernel.follows = "shared-kernel";

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
