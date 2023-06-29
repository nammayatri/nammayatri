{
  inputs = {
    common.url = "github:nammayatri/common";

    # Backend inputs
    shared-kernel.url = "github:nammayatri/shared-kernel/89219e9da12fe237676f3eefddb2f0e283de8ed4";
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
