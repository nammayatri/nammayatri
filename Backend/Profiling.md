# Profiling Setup for NammaYatri Project

<!-- Author: Vijay Gupta -->

This guide explains how to enable profiling for Haskell projects, specifically for the `NammaYatri` project, and other Haskell projects using `haskell-flake`.

## Step-by-Step Guide to Enable Profiling

### 1. Modify the `default.nix` File

Navigate to the `Backend/default.nix` file. In the `haskellProjects.default` section, add the following line to force profiling:

```nix
defaults.settings.defined = {
  libraryProfiling = lib.mkForce true;
};
```

### 2. Re-enter the Nix Development Shell

After making changes to `default.nix`, re-enter the Nix development shell and run the following command to generate the `.prof` file:

```bash
cabal <package_name> run --enable-profiling -- +RTS -p -RTS
```

Once you run this command, your application will compile and execute with profiling enabled, and a `.prof` file will be generated in the root directory of the project.

### 3. Advanced Profiling

For advanced profiling (such as heap profiling), you can use the following command:

```bash
cabal <package_name> run --enable-profiling -- +RTS -hc -Pa -RTS
```

## Running Services with Profiling

The current setup has issues running all services with profiling enabled simultaneously. As a workaround, run the necessary services and manually profile the application you want. Here's an example:

### Example:

To profile `dynamic-offer-driver-app` and `rider-app`, use the following commands:

```bash
cabal run dynamic-offer-driver-app --enable-profiling -- +RTS -p -RTS
cabal run rider-app --enable-profiling -- +RTS -p -RTS
```

For advanced profiling:

```bash
cabal run dynamic-offer-driver-app --enable-profiling -- +RTS -hc -Pa -RTS
cabal run rider-app --enable-profiling -- +RTS -hc -Pa -RTS
```

---

## Profiling Setup for Other Projects

If you're working on other projects and using a different version of `haskell-flake`, follow these steps:

### 1. For `haskell-flake < 0.5`

In your `default.nix` file where `haskellProjects.default` is defined, add the following:

```nix
defaults.settings.default = {
  libraryProfiling = true;
};
```

### 2. For `haskell-flake >= 0.5`

Use the following in the `haskellProjects.default` section of your `default.nix` file:

```nix
defaults.settings.defined = {
  libraryProfiling = true;
};
```

### Example:

```nix
haskellProjects.default = {
  packages = {
    my-project = {
      # ...
    };
  };
  defaults.settings.defined = {
    libraryProfiling = true;
  };
};
```

### 3. Forcing Profiling in Other Projects

If your project uses a library where profiling isn't enabled by default, add the following in your `default.nix`:

```nix
defaults.settings.defined = {
  libraryProfiling = lib.mkForce true;
};
```

Then run the project with:

```bash
cabal <package_name> run --enable-profiling -- +RTS -p -RTS
```

---

By following these steps, you can enable profiling for your Haskell applications and troubleshoot any performance issues with detailed profiling information.

---

### Notes:
- Profiling can generate different insights, such as time or heap usage, depending on the profiling flags (`-p`, `-hc`, etc.).
- The setup might vary slightly depending on your project’s structure and dependencies.

