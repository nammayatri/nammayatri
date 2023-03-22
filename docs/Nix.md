# Nix

The Nammayatri project is in the process of migrating from Stack to Nix. Nix is now used in CI, and very soon we'll ask developers to use Nix while removing all Stack files. In the meanwhile, to ensure that CI passes, if you are modifying `stack.yaml` be sure to also update the "inputs" in `flake.lock` (see the section further below).

## Installing Nix

https://haskell.flake.page/nix

Note: installing Nix requires root access.

## Keeping stack.yaml and flake.lock in sync

If your `stack.yaml` has the following input,

```yaml
  - git: https://github.com/nammayatri/shared-kernel.git
    commit: 28bae0f39eb3d2158c74dd3aac7ed685805c5cee
```

you can specify the corresponding input in `flake.nix` as follows:

```nix
{
  inputs = {
    # You can also use branch name instead of revision here.
    # ie., `github:nammayatri/shared-kernel/my-branch`
    shared-kernel.url = "github:nammayatri/shared-kernel/28bae0f39eb3d2158c74dd3aac7ed685805c5cee";
  };
}
```

**Important:** Each time you update the `flake.nix` file, be sure to run the `nix flake lock` command.

### Transitive dependencies

Keep in mind that, with Nix transitive dependency overrides need not be copied over to all repos. So a dependency like `euler-hs` need only be overriden in one repo (`shared-kernel.git` in this case), and all other repos will use the override indirectly.

## Building using Nix

Nix support is still a work-in-progress, but you can test it out as follows:

```sh
# Setup nix cache (do this once)
nix run nixpkgs#cachix use nammayatri

# Build the nammayatri executables
nix build .#nammayatri

# Build the docker image
docker load -i $(nix build .#dockerImage --print-out-paths)

# Enter the nix shell
nix develop # or `direnv allow`; see https://haskell.flake.page/direnv
```
