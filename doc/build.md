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
stack exec beckn-transport-exe
stack exec beckn-transport-btm-exe
```

### FMD wrapper:

```
stack exec fmd-wrapper-exe
```