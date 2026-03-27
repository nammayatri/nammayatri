# Zero flake inputs — all dependencies pinned in nix/sources.nix via fetchTarball.
# Each flake input costs ~1.5s of verification overhead; with zero inputs,
# nix develop evaluates in <1s (warm).
# See: https://github.com/juspay/kolu/pull/152
#
# WARNING: Do not add flake inputs here. Pin sources in nix/sources.nix instead.
{
  nixConfig = {
    extra-substituters = "https://cache.nixos.asia/oss";
    extra-trusted-public-keys = "oss:KO872wNJkCDgmGN3xy9dT89WAhvv13EiKncTtHDItVU=";
  };

  inputs = { };

  outputs = { self, ... }:
    let
      sources = import ./nix/sources.nix;
      mk = import ./nix/mk-flake.nix sources;
    in
    mk.mkFlake { inherit self; } {
      imports = [
        ./Backend/default.nix
        ./Frontend/default.nix
      ];
    };
}
