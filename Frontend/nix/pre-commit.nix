# https://pre-commit.com/ hooks defined in Nix
# cf. https://github.com/cachix/pre-commit-hooks.nix
{ lib, ... }:

{
  hooks = {
    eslint = {
      enable = true;
      files = lib.mkForce "(Frontend/ui-common/src/.*\\.js$)|(Frontend/ui-customer/src/.*\\.js$)|(Frontend/ui-driver/src/.*\\.js$)";
    };
  };
}
