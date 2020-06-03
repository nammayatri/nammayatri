# beckn

## This contains two libraries namely (beckn-epass, beckn-core)
* `beckn-core` is the common library where the API types, unified data models will be there

## How to run Beckn backends in a dev environment:

### Setup

Set up your development environment

```
stack build
```

### Compile the project with

```
stack build
```

### Run the databases inside Docker

```
cd dev && docker-compose up
```

### Start the app-backend server

```
stack exec app-backend-exe
```

### Start the transporter server

```
BECKN_GATEWAY_BASE_URL="localhost" BECKN_GATEWAY_PORT="8013" stack exec beckn-transport-exe
```
