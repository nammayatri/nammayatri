# Building and running

## Setup

Set up your development environment, from project root run:

```
./dev/setup.sh
```

## Compile the project with


```
stack build --fast
```
Note: For deployment, `stack build` command should be used to compile with optimizations.

## Run the databases and Redis inside Docker

```
cd dev && docker-compose up
```

## Run gateway:

```
stack exec beckn-gateway-exe
```
## Running the applications

### Mobility
```
stack exec app-backend-exe
stack exec search-result-aggregator-exe
stack exec beckn-transport-exe
stack exec beckn-transport-btm-exe
stack exec parking-bap-exe
stack exec public-transport-bap-exe
```

### FMD wrapper:

```
stack exec fmd-wrapper-exe
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