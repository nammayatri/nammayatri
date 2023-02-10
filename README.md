# Juspay Mobility

## Introduction

This is the code repository for the open mobility platform developed by Juspay.
The mobility platform is powering the next-generation of mobility applications in India, it is based on open protocols with the goal of empowering and enabling the end-users.

The code in this repository powers the entire "Namma Yatri" platform, application & services.

## Getting Started

Getting Started with building and running the project.

### Pre-requisites

Before we can build the mobility project, there are some pre-requisites in the form of external dependencies which you must install, depending on you OS.

#### Haskell language toolchain

You'd need the Haskell language toolchain (GHC, cabal) installed in order build the project.

[GHCup](https://https://www.haskell.org/ghcup) is the preferred method to install Haskell.


#### Tools

These tools are required when working with the mobility repository:-

1. [Docker](https://www.docker.com/products/docker-desktop/) - we use docker and docker-compose for containers.
2. [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) - we use the stack build tool for building and running the entire project. ([GHCup](https://https://www.haskell.org/ghcup) can also be used to install Stack)

For Mac users, some additional tools may be required:-

1. [Xcode](https://developer.apple.com/xcode/)
2. [Home brew](https://brew.sh)

#### Linters and formatters

Install haskell linter and formatter by running this command in home directory, after you have stack installed.

```
$ stack install hlint ormolu
```

#### Extra Dependencies

Depending on your OS, you'd need install these dependencies or their equivalents.

For Mac

```
$ brew install libpq
$ brew install librdkafka
$ brew install postgres
$ brew install dhall
$ brew install jq
$ brew install parallel
```

For M1 or newer Macs (other dependencies)

```
arch -x86_64 /usr/local/bin/brew install libpq
arch -x86_64 /usr/local/bin/brew install librdkafka
arch -x86_64 /usr/local/bin/brew install postgres
arch -x86_64 /usr/local/bin/brew install dhall
arch -x86_64 /usr/local/bin/brew install jq
arch -x86_64 /usr/local/bin/brew install parallel
```

For Linux (other dependencies) or your package-manager equivalents

```
$ sudo apt-get install libpq-dev
$ sudo apt-get install librdkafka-dev
$ sudo apt-get install postgresql
$ sudo apt-get install dhall
$ sudo apt-get install jq
$ sudo apt-get install parallel
```

### Building and Development

After you've all the pre-requisite tools & dependencies installed, we can build the project for development.

#### Building

To build the project for development, we should compile the project with the command

```
$ stack build --fast
```

The `--fast` flag disables some compile-time optimizations for faster compile times and should only be used for development builds.

> **_Note:_**  For deployment, `stack build` command should be used to compile with optimizations.

This should start building the project and all it's dependencies.

#### Development

Once the above build command completes successfully, we can run the project for development.

The `dev/` folder at the project top-level contains all the relevant files and configs, should you need to change or inspect them.

##### Setting up development environment

To set up your development environment, from project root run

```
$ ./dev/setup.sh
```


##### Running the services
To run the project, we'd first need to run some services. These are provided via helpful `make` commands.


For running the database, redis, passetto and kafka run this command
```
$ make run-svc
```

That should run most of the services required.

More services, if needed, can be run with the following commands.

For running pgadmin run this command
```
$ make run-pgadmin
```

For running monitoring services like prometheus and grafana use this command
```
$ make run-monitoring
```


### Testing

The project comes with a range of tests in it's test-suites. These tests should pass for each correct build.

To run the test-suite for the project, first ensure you have the services running (see [running servcies section](#running-the-services)).

Run the following command in the project root folder after the services are up and running:-

```
$ stack test
```


## Usage

Each of the application has particular set of defined APIs and Schemas. To get available APIs/Schemas of any particular application, follow these steps

1. Copy the swagger json from `http://localhost:<port>/swagger` and use the relevant port (enlisted below)

| Application                              | Port   |
| -----------------------------------------|--------|
| rider-app                                | `8013` |
| static-offer-driver-app                  | `8014` |
| beckn-gateway                            | `8015` |
| mock-registry                            | `8020` |
| public-transport-rider-platform          | `8023` |
| public-transport-search-consumer         | `8024` |
| search-result-aggregator                 | `8025` |
| scheduler-example-scheduler              | `8051` |
| transporter-scheduler                    | `8053` |
| allocation-service                       | `9996` |

2. Paste the copied swagger json inside https://editor.swagger.io/
3. To run the requests one can use the Postman or any other API platform.

## Project Structure

The top level of the project has a very descriptive folder structure with helpful names.

The entire project is structured as a collection of smaller focused packages, which can be found listed in the top level `stack.yaml` file, under the _packages_ section.

Each package has clear separation of focuses w.r.t the functionality it provides, which helps with maintenance and development and provides clear designated areas to look at for a specific desired behavior and functionality. A good overview of the app structure be found in the table below:- 

| S.no | Module                         | Executable                           | Function                                                               |
|:-----|:-------------------------------|--------------------------------------|:-----------------------------------------------------------------------|
| 1.   | bap                            |                                      |                                                                        |
| 1.1  | - app-backend -> rider-app     | app-backend-exe                      | Frontend facing APIs (BAP), has more end-user specific implementations |
|      |                                |                                      |                                                                        |
| 1.2  | - public-transport             |                                      |                                                                        |
|      | * Main                         | public-transport-bap-exe             |                                                                        |
|      | * search-consumer              | public-transport-search-consumer-exe | This app receives and processes search requests from app-backend       |
|      |                                |                                      |                                                                        |
| 2.   | bpp                            |                                      |                                                                        |
| 2.1  | - beckn-transport              |                                      |                                                                        |
|      | * Allocator                    | allocation-service-exe               |                                                                        |
|      | * Main                         | beckn-transport-exe                  |                                                                        |
|      | * Scheduler                    | transporter-scheduler-exe            |                                                                        |
|      |                                |                                      |                                                                        |
| 2.2  | - driver-offer-bpp             |                                      |                                                                        |
|      | * Allocator                    | driver-offer-allocator-exe           |                                                                        |
|      | * Main                         | driver-offer-bpp-exe                 |                                                                        |
|      |                                |                                      |                                                                        |
| 2.3  | - driver-tracking-health-check | driver-tracking-healthcheck-exe      |                                                                        |
|      |                                |                                      |                                                                        |
| 3.   | dashboard                      |                                      |                                                                        |
| 3.1  | - BAPDashboard                 | bap-dashboard-exe                    | BAP specific dashboard                                                 |
| 3.2  | - BPPDashboard                 | bpp-dashboard-exe                    | BPP specific dashboard                                                 |
|      |                                |                                      |                                                                        |
| 4.   | example-service                | example-service-exe                  | Example (template) of a service for faster service creation.           |
|      |                                |                                      |                                                                        |
| 5.   | kafka-consumers                | kafka-consumers-exe                  |                                                                        |
|      |                                |                                      |                                                                        |
| 6.   | mocks                          |                                      |                                                                        |
| 6.1  | - bap                          | mock-bap-exe                         | Mock bap to trigger bpp endpoints, receive responces and log them      |
| 6.2  | - fcm                          | mock-fcm-exe                         | Mock FCM                                                               |
| 6.3  | - google                       | mock-google-exe                      | Mock for Google with hardcoded values for using in stack test          |
| 6.4  | - idfy                         | mock-idfy-exe                        |                                                                        |
| 6.5  | - public-transport-bpp         | mock-public-transport-bpp-exe        |                                                                        |
| 6.6  | - sms                          | mock-sms-exe                         | Mock Sms                                                               |
|      |                                |                                      |                                                                        |
| 7.   | scheduler-example              | scheduler-example-app-exe            | example applications that uses the scheduler library                   |
|      |                                | scheduler-example-scheduler-exe      |                                                                        |
| 8.   | utils                          |                                      |                                                                        |
| 8.1  | - image-api-helper             | image-api-helper-exe                 |                                                                        |
| 8.2  | - route-extractor              | route-extractor-exe                  |                                                                        |
|      |                                |                                      |                                                                        |


## Contributing

We strongly believe in the power of open-source. The main purpose of this repository is that the development of the mobility stack happens at an open-to-all platform, where anyone can inspect and contribute. The repository should evolve, adding more features and make existing features faster, correct and easy to use.
We welcome contributions to the repository in the form of bug-reports, code patches, documentation updates and feature requests or heads-up for breaking changes in any of the dependencies.

### Code of Conduct

TBD (add a link to the complete code of conduct)

### Guidelines

In order to keep the project healthy and the load on the maintainers in check, please have a look at our general contribution guidelines.

[TBD a link to a full guidelines & CONTRIBUTING list, but general points can include:- ]

#### When opening an issue

    1. When opening an issue, please mention the problem encountered in detail, steps to reproduce the issue, tooling & compiler versions used as well as your OS or linux distro version.

    2. Please ensure that you have all the dependencies installed as per the instructions before opening a build-error issue.

#### When opening a pull-request

When contributing to mobility, it's a good idea to follow the following general guidelines.

1. Fork the repository.
2. Create your branch from `master`.
3. If you've added some code which would need to be tested or fixed a bug, consider adding tests for it if required.
4. Please ensure that all tests in the test-suite pass after running `stack test`.
5. Consider fixing all linting issues as suggested by the linter we use - hlint.
6. Format your code with the code formatter we use for the project - Ormolu.
7. Raise a pull request, referring the issue it solves.
8. More points TBD ...

## License

TBD: The type of license for mobility.

## FAQs

1. My project doesn't build.

    This could be because of a number of reasons. Most commonly though, we've found that more often than not it's because of the following reasons:-

    * We're missing a (OS specific) dependency.
    * Have an older or really new version of a required dependency.
    * Missing a required tool like stack or GHC.
    * Have incompatible version of the above tools like stack or GHC (though using stack minimizes this)
    * Our PATH environment/shell variable is not correctly configured.
    * We're running an incompatible, older or unsupported OS or OS version.

2. I can't run the services correctly.

    To make sure you can run the services correctly for development and testing, please ensure that you have Docker & Docker-compose installed correctly, are configured correctly in you PATH variable and you have the required permissions to run them.

3. I can't figure out the project structure.

    Please refer to the [Project Structure Section](#project-structure)

4. TBD...
