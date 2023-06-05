{
  inputs = {
    common.url = "github:nammayatri/common";

    # Backend inputs
    shared-kernel.url = "github:nammayatri/shared-kernel";
    beckn-gateway.url = "github:nammayatri/beckn-gateway";
    beckn-gateway.inputs.common.follows = "common";
    beckn-gateway.inputs.shared-kernel.follows = "shared-kernel";

    # Frontend inputs
    purifix.url = "github:purifix/purifix";
    dream2nix.url = "github:nix-community/dream2nix";
    dream2nix.inputs.nixpkgs.follows = "dream2nix-nixpkgs-fixed";
    dream2nix-nixpkgs-fixed.url = "nixpkgs/c1989c17e2658f721df7b0027cbc3d8959f914cb"; # when this fix gets upstreamed remove
    android-nixpkgs.url = "github:tadfisher/android-nixpkgs/stable";
  };
  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      imports = [
        ./Backend/default.nix
        ./Frontend/default.nix
      ];
    };
}
