
This is the sub-project containing backend code written in [haskell] powering [nammayatri] servers.

## Getting Started

### Pre-requisites

To build or develop the project, you need to install the following.

#### Nix

We use [Nix](https://nixos.asia/en/nix) to build and develop the [Namamayatri][nammayatri] project. To prepare your system for a pleasant [Nix-based development](https://nixos.asia/en/dev), follow these four steps:

1. [Install **Nix**](https://nixos.asia/en/install)
1. [Install **direnv**](https://github.com/juspay/nixos-unified-template)[^direnv] (select `home-manager` template)
1. Enter the Nix devshell by running `ln -s .envrc.backend .envrc && direnv allow` in the project directory.

[^direnv]: You want this to facilitate a nice Nix develoment environment. Read more about direnv [here](https://nixos.asia/en/direnv). Even though `direnv` is not strictly required to develop nammayatri, if you do not use `direnv` you would have to remember to manually restart the `nix develop` shell, and know when exactly to do this each time. Also, you need setup the binary cache manually.

### Building

**🚨 Attention 🚨**: You do not need to run `nix build` if you are developing the project locally (`nix build` is run in CI). Skip to the [Development](#development) section below. You should prefer [cabal] over Nix when building locally because cabal is faster.

Once you have installed all the necessary tools and dependencies, we can proceed with building the project for development.

To compile the backend, use the following command:

```sh
nix build .#nammayatri
```

This should produce a `./result` symlink in the current directory, containing all backend binaries under `./result/bin`.

**🚧 Warning 🚧**: The `nix build` command should _only_ build the nammayatri project and it should finish in a matter of minutes. If not, you must not have setup the Nix cache properly. Consult [the steps further above](#nix).

#### Building the docker image

**🚨 Attention 🚨**: You can skip this step if you intend to run locally only.

```sh
docker load -i $(nix build .#dockerImage --no-link --print-out-paths)
```

### Development

#### Setting up a development environment

**🚨 Attention 🚨**: If you were using *stack* to develop Nammayatri in the past, you must **completely erase** that git working copy, and start from a fresh clone of this repository before working with Nix. You might also want to remove your cache folders `~/.cache/cabal` and `~/.cabal/hie-bios`.

To set up your development environment for backend, you should setup and use direnv[^de-ns] by running the following from repository root:

```sh
cd ~/Projects/nammayatri
ln -sf .envrc.backend .envrc  # Run this only once.
direnv allow                 # Run this only once.
```

[^de-ns]: If you are not using `direnv` and if you know what you are doing, you could manually start the [nix shell][nix-shell] using `nix develop .#backend`.

**🚧 Warning 🚧**: Entering the nix develop shell (using `direnv allow`, for example) should not compile anything and it should finish in a matter of minutes (after downloading the binaries from our Nix cache). If not, you must not have setup the Nix cache properly. Consult [the steps further above](#nix).

This will drop you into a [shell environment][nix-shell] containing all project dependencies. Inside the nix shell, run `,` to see the available commands specific to nammayatri development.

To compile the project, use [cabal]:

```sh
# Do this in nix develop shell activated direnv:
cd ./Backend
# Build all packages
cabal build all
# Once the build is complete, you will be able to run backend services directly
# Run a cabal package (by path to the directory containing .cabal file)
cabal run lib/location-updates
# Run ghcid (for fast compile feedback)
, ghcid lib/location-updates
```

#### Faster Local Development builds

For much shorter compile times and quicker feedback cycles while developing, you may want to use the builds optimized for local development.

This is now easily & quickly achieved by simply un-commenting the flags under ["DEVELOPMENT FLAGS" section in cabal.project file](cabal.project) when developing.

#### Linker errors

If you get `Segmentation fault` during linking (see https://github.com/NixOS/nixpkgs/issues/149692), you can workaround it by increasing your stack size limit:

```sh
ulimit -s 9999
```

#### Parallel Jobs
To speed up the compilation times, we use 6 parallel jobs by default. If you have a powerful computer with lots of cores and memory, you can increment the `jobs` setting in [cabal.project](cabal.project) file to run more parallel jobs for faster results. Inversely, if its a low-powered machine, you may consider lowering that number.

#### Running backend services

To run the backend either use:

```sh
# From nix develop shell
, run-mobility-stack-dev
```

This will run the mobility stack using `cabal run` which is the recommend approach for development environment.

You can also use Nix to run the mobility stack, but this is slower compared to the cabal way because it involves doing a full Nix build.

```sh
, run-mobility-stack-nix
# Or (if you are not in the git repo):
nix run github:nammayatri/nammyatri#run-mobility-stack-nix
```

##### External services

The above command will also run some services (databae, redis, passetto, osrm-server, kafka). These are provided via [services-flake].

More services, if needed, can be run with the following commands.

For running pgadmin run this command:

```sh
, run-pgadmin
```

For running monitoring services like prometheus and grafana use this command:

```sh
, run-monitoring
```

#### Updating flake inputs

External dependencies of the project are usually specified in [`inputs`](https://nixos.wiki/wiki/Flakes#Input_schema) attribute of the `flake.nix` file. They usually point to external Git repos, but they can also point to local directories (which is useful during development)

The specific revisions of these Git repos are pinned in the `flake.lock` file. If you want to update, say, shared-kernel to a particular commit, run:

```sh
nix flake lock --update-input shared-kernel --override-input shared-kernel github:nammayatri/shared-kernel/f8a79646de474d1a103365e27677778f278b100f
```

If you just want to advance the pinned commit to the HEAD of the `main` branch, run instead:

```sh
nix flake lock --update-input shared-kernel
```

You can also change the flake input to point a local checkout. To do this, change the `flake.nix` to be like:

```nix
{
  inputs = {
    shared-kernel.url = "path:/Users/myname/Projects/shared-kernel";
  };
}
```

Now, if you run `nix build` or any of the other nix commands, it will use the local shared-kernel to compile nammayatri against. Whenever you change the contents of `/Users/myname/Projects/shared-kernel`,  you **must** run `nix flake lock --update-input shared-kernel` again, so the current project will use the new contents.

#### Visual Studio Code

Also see: https://nixos.asia/en/vscode

Once you have cloned the repo and have been successfully able to build the project using `cabal build all`, you can use [Visual Studio Code](https://code.visualstudio.com/) to develop the project.

- Launch [VSCode](https://code.visualstudio.com/), and open the `git clone`’ed project directory [as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces)
    - NOTE: If you are on Windows, you must use the [Remote - WSL extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-wsl) to open the folder in WSL.
- When prompted by VSCode, install the [workspace recommended](https://code.visualstudio.com/docs/editor/extension-marketplace#_workspace-recommended-extensions) extensions.
    - If it doesn’t prompt, press Cmd+Shift+X and search for `@recommended` to install them all manually.
- Ensure that the direnv extension is fully activated. You should expect to see this in the footer of VSCode: ![image](https://user-images.githubusercontent.com/3998/235459201-f0442741-294b-40bc-9c65-77500c9f4f1c.png)
- Once direnv is activated (and only then) open a Haskell file (`.hs`). You should expect haskell-language-server to startup, as seen in the footer: ![image](https://user-images.githubusercontent.com/3998/235459551-7c6c0c61-f4e8-41f3-87cf-6a834e2cdbc7.png)
    - Once this processing is complete, all IDE features should work.

### Testing

The project comes with a range of tests in its test-suites. These tests should pass for each correct build.

To run the test-suite for the project,

- first ensure you have the services running (see [running external services section](#running-external-services)).
- Then, run the osrm-server using `nix run .#osrm-server`

Run the following command in `./Backend` folder after the services are up and running:

```sh
cabal test all
```

#### Running integration tests

See Documentation [README.md](newman-tests/README.md)

## Usage

Each of the application has particular set of defined APIs and Schemas. To get available APIs/Schemas of any particular application, follow these steps

1. Copy the swagger json from `http://localhost:<port>/swagger` and use the relevant port (enlisted below)

| Application                              | Port   |
| -----------------------------------------|--------|
| rider-app                                | `8013` |
| beckn-gateway                            | `8015` |
| dynamic-offer-driver-app                 | `8016` |
| mock-registry                            | `8020` |
| transporter-scheduler                    | `8053` |
| allocation-service                       | `9996` |

2. To run the requests one can use the Postman or any other API platform.

## Project Structure

The top level of the project has a very descriptive folder structure with helpful names.

The entire project is structured as a collection of smaller focused packages, which can be found listed in the top level `cabal.project` file, under the _packages_ section.

Each package has clear separation of focuses w.r.t the functionality it provides, which helps with maintenance and development and provides clear designated areas to look at for a specific desired behavior and functionality. A good overview of the app structure be found in the table below:-

```text
├── rider-platform                                  : encapsulates all the rider side microservices
|   ├── rider-app (rider-app-exe)                   : Frontend facing APIs, rider app
|   └── public-transport
|       ├── Main (public-transport-rider-platform-exe)
|       └── search-consumer	(public-transport-search-consumer-exe)
├── provider-platform                               : encapsulates all the provider side microservices
|   ├── dynamic-offer-driver-app                    : Microservices that power dynamic pricing,
|   |   |                                             as quoted by the driver, service
|   |   ├── Allocator (driver-offer-allocator-exe)  : Allocation service that matches a driver
|   |   |                                             to a ride
|   |   └── Main (dynamic-offer-driver-app-exe)     : Frontend facing APIs, driver app
|   ├── driver-tracking-health-check
├── dashboard
|   ├── rider-dashboard (rider-dashboard-exe)       : Rider specific ops dashboard APIs
|   └── provider-dashboard (provider-dashboard-exe) : Provider specific ops dashboard APIs
├── kafka-consumers                                 : Microservices that consume messages from kafka
|                                                     to perform various tasks
├── mocks                                           : Mock servers that mock various
|                                                     third party APIs, used for local testing
└── utils
    ├── image-api-helper (image-api-helper-exe)
    └── route-extractor	(route-extractor-exe)
```

## FAQs

### I can't figure out the project structure.

Please refer to the [Project Structure Section](#project-structure)

### In Visual Studio Code Terminal, I get the error ```Assertion `path != ""' failed```.

Try the following steps:
1. `sudo -i nix upgrade-nix` (This will upgrade nix on your machine)
2. Ensure that you have VS Code installed from this link (choose the installer correctly): (https://code.visualstudio.com/insiders/)
3. Open the project in the newly installed VS Code Insiders.

If the above steps don't solve the issue, this could be due to [a bug in the VSCode direnv extension](https://github.com/NixOS/nix/issues/6409#issuecomment-1407799718). Run `unset NIX_STORE && direnv reload` in the VSCode terminal to fix it.

### How to find out where a library dependency is specified in Nix?

Run `nix run github:nix-community/nix-melt` to navigate and find that transitive flake input specifying the dependency you are looking for. You can also inspect the `flake.lock` file.

[nammayatri]: https://www.nammayatri.in/
[haskell]: https://www.haskell.org/
[arion]: https://github.com/hercules-ci/arion
[services-flake]: https://github.com/juspay/services-flake
[cabal]: https://cabal.readthedocs.io/
[nix-shell]: https://nixos.wiki/wiki/Development_environment_with_nix-shell

### `, run-mobility-stack-*` not responding to `Ctrl-C` or [external-services](running-external-services) running in the background even after exiting `, run-mobility-stack-*`

Run `, kill-svc-ports`

## Running Load Test

See Documentation [README.md](load-test/README.md)
