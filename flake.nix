{
  inputs = {
    common.url = "github:nammayatri/common";

    # Backend inputs
    shared-kernel.url = "github:nammayatri/shared-kernel";
    beckn-gateway.url = "github:nammayatri/beckn-gateway";
    beckn-gateway.inputs.common.follows = "common";
    beckn-gateway.inputs.shared-kernel.follows = "shared-kernel";
    purifix.url = "github:purifix/purifix";
  };
  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      imports = [
        ./Backend/default.nix
        ./Frontend/default.nix
      ];
    };
}
