This is the sub-project containing backend code written in [haskell] powering [nammayatri] servers.

## Getting Started

### Pre-requisites

To build or develop the project, you need to install the following.

#### Nix

Nix is central to building and developing the Namamayatri project. Install and setup Nix as follows:

1. [Install **Nix**](https://github.com/DeterminateSystems/nix-installer#the-determinate-nix-installer)
    - Then, run `nix run github:srid/nix-health` to check that everything is green.
1. Setup the **binary cache** (to avoid compiling locally):
    ```sh
    nix run nixpkgs#cachix use nammayatri
    ```
    - For this command to succed, you must have added yourself to the `trusted-users` list of `nix.conf`
1. If you are also developing the backend, we recommend that you install **nix-direnv** and **starship**. See the [explanation](https://haskell.flake.page/direnv)[ here](https://haskell.flake.page/direnv); here is a [home-manager template](https://github.com/juspay/nix-dev-home) that you can use to get started easily.
    - While this is not strictly required, it is recommended for better IDE integration in VSCode and other editors.

#### Other tools

Aside from Nix, you also need:

1. Install [Docker](https://www.docker.com/products/docker-desktop/) (we use docker-compose for running external services dependencies).
    - If you are on macOS, open *Docker -> Preferences... -> Resources -> File Sharing* in Docker Desktop and add `/nix/store` to the list of shared folders.
1. Install [Xcode](https://developer.apple.com/xcode/), if you are on macOS.


### Building

Once you have installed all the necessary tools and dependencies, we can proceed with building the project for development.

To compile the backend, use the following command:

```sh
nix build .#nammayatri
```

This should produce a `./result` symlink in the current directory, containing all backend binaries under `./result/bin`.

#### Building the docker image

```sh
docker load -i $(nix build .#dockerImage --print-out-paths)
```

### Development

NOTE: The `Backend/dev/` folder contains all the relevant files and configs for local development, should you need to change or inspect them.

#### Setting up a development environment

To set up your development environment, you should run `direnv allow` from the project root. If you do not have nix-direnv setup (as per the pre-requisites above), run instead:

```sh
nix develop # If you cannot do `direnv allow`.
```

This will drop you into a shell environment containing all project dependencies. In side the nix shell, run `,` to see the available commands specific to nammayatri development.

To compile the project, use [cabal]:

```sh
# In nix develop shell:
cd ./Backend
# Build all packages
cabal build all
# Run a cabal package (by path to the directory containing .cabal file)
cabal run lib/location-updates
# Run ghcid
, backend-ghcid lib/location-updates
```

#### Running the services

To run the project, we'd first need to run some services. These are provided via docker images (built in Nix).

For running the database, redis, passetto and kafka run this command:

```sh
# NOTE: You must run this from inside nix shell.
, backend-run-svc
```

That should run most of the services required.

More services, if needed, can be run with the following commands.

For running pgadmin run this command:

```sh
, backend-run-pgadmin
```

For running monitoring services like prometheus and grafana use this command:

```sh
, backend-run-monitoring
```

To run osrm-server, run:

```sh
nix run .#osrm-server
```

#### Running backend

```sh
, backend-run-mobility-stack
```

This will run nammayatri components using `cabal run`. If you wish to run it using Nix instead, run:

```sh
nix run .#run-mobility-stack
```

#### Updating flake inputs

Nix dependencies specified in `inputs` of the `flake.nix` file. They usually point to external Git repos. The specific revisions of these Git repos are pinned in the `flake.lock` file. To update the `shared-kernel` input, for instance, run:

```sh
nix flake lock --update-input shared-kernel
```

If you update the `inputs` section of `flake.nix` file, be sure to run `nix flake lock` so as to also update the `flake.lock` file.

### Testing

The project comes with a range of tests in its test-suites. These tests should pass for each correct build.

To run the test-suite for the project,

- first ensure you have the services running (see [running servcies section](#running-the-services)).
- Then, run the osrm-server using `nix run .#osrm-server`

Run the following command in `./Backend` folder after the services are up and running:

```sh
cabal test all
```


## Usage

Each of the application has particular set of defined APIs and Schemas. To get available APIs/Schemas of any particular application, follow these steps

1. Copy the swagger json from `http://localhost:<port>/swagger` and use the relevant port (enlisted below)

| Application                              | Port   |
| -----------------------------------------|--------|
| rider-app                                | `8013` |
| static-offer-driver-app                  | `8014` |
| beckn-gateway                            | `8015` |
| dynamic-offer-driver-app                 | `8016` |
| mock-registry                            | `8020` |
| transporter-scheduler                    | `8053` |
| allocation-service                       | `9996` |

2. To run the requests one can use the Postman or any other API platform.

## Project Structure

The top level of the project has a very descriptive folder structure with helpful names.

The entire project is structured as a collection of smaller focused packages, which can be found listed in the top level `stack.yaml` file, under the _packages_ section.

Each package has clear separation of focuses w.r.t the functionality it provides, which helps with maintenance and development and provides clear designated areas to look at for a specific desired behavior and functionality. A good overview of the app structure be found in the table below:-

```text
├── rider-platform                                  : encapsulates all the rider side microservices
|   ├── rider-app (rider-app-exe)                   : Frontend facing APIs, rider app
|   └── public-transport
|       ├── Main (public-transport-rider-platform-exe)
|       └── search-consumer	(public-transport-search-consumer-exe)
├── provider-platform                               : encapsulates all the provider side microservices
|   ├── static-offer-driver-app                     : Microservices that power fixed price ride
|   |   |                                             hailing service
|   |   ├── Allocator (allocation-service-exe)      : Allocation service that matches a driver
|   |   |                                             to a ride
|   |   ├── Main (static-offer-driver-app-exe)      : Frontend facing APIs, driver app
|   |   └── Scheduler (transporter-scheduler-exe)   : Job scheduler for scheduling rental rides
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

1. I can't figure out the project structure.

    Please refer to the [Project Structure Section](#project-structure)

2. TBD...

[nammayatri]: https://www.nammayatri.in/
[haskell]: https://www.haskell.org/