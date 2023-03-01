# Nix

The Nammayatri project is in the process of using Nix instead of Stack. At the moment, developers still use Stack, but Nix is used in CI. If you are modifying `stack.yaml` be sure to also update the "inputs" in `flake.lock` (see the section further below).

## Installing Nix

https://haskell.flake.page/nix

## Keeping stack.yaml and flake.lock in sync

If you `stack.yaml` has the following input,

```yaml
  - git: https://github.com/nammayatri/shared-kernel.git
    commit: 28bae0f39eb3d2158c74dd3aac7ed685805c5cee
```

you can specify the corresponding input in `flake.nix` as follows:

```nix
{
  inputs = {
    # You can also use branch name instead of revision here.
    shared-kernel.url = "github:nammayatri/shared-kernel/28bae0f39eb3d2158c74dd3aac7ed685805c5cee";
    shared-kernel.flake = false;
  };
}
```

Every time you update the `flake.nix`, be sure to run the `nix flake lock` command.
