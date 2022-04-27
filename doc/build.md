## Pre-requisite  to get started
1. [Docker](https://www.docker.com/products/docker-desktop/)

2. [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

For Mac

1. [Xcode](https://developer.apple.com/xcode/)

2. [Home brew](https://brew.sh)

## Setup

Set up your development environment, from project root run

```
./dev/setup.sh
```
For Mac                       
```
brew install libpq                                  

brew install librdkafka

brew install postgres

brew install dhall
```

For Linux
```

sudo apt-get install libpq-dev

sudo apt-get install librdkafka-dev

sudo apt-get install postgresql

sudo apt-get install dhall
```

## Run this command in home directory
```
stack install hlint-3.2.7 ormolu-0.1.4.1
```

## Compile the project with

```
stack build --fast
```
Note: For deployment, `stack build` command should be used to compile with optimizations.

## Run the services 

 For running the database, redis, passetto and kafka run this command
```
make run-svc
```

### Additional services
For running pgadmin run this command
```
make run-pgadmin 
```

For running monitoring services like prometheus and grafana use this command
```
make run-monitoring
```

## Running the applications 
For running all the applications use this command 
```
make run-mobility-stack
```
### Run load testing

Beckn project uses K6 framework for initiating load tests. Before running them make sure you have k6 CLI installed on your local machine. Refer to https://k6.io/docs/getting-started/installation.

Once install start BPP server:

```
stack run beckn-transport-exe
```

And then run Beckn CLI command for testing:

```
stack run beckn-cli-exe -- --private-key *BASE64_ENCODED_PRIVATE_KEY* --requests *NUMBER_OF_REQUESTS_TO_RUN*
```

Keep in mind that you need to use the same private key for signing requests that is available for BPP instance. You can use the default key that is intended for DEV environment which is set in `dhall-configs/dev/secrets/common.dhall`