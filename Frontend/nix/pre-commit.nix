# https://pre-commit.com/ hooks defined in Nix
# cf. https://github.com/cachix/pre-commit-hooks.nix
{ lib, ... }:

{
  hooks = {
    eslint = {
      enable = true;
      files = lib.mkForce "Frontend/.*\\.js$";
    };
    purty.enable = true;
  };
}
