# beckn
Beckn is an open protocol that enables location-aware, local commerce across industries.

More about beckn:
* [whitepaper](https://beckn.org/wp-content/uploads/2020/04/WhatIsBeckn.pdf)
* [mobility](https://beckn.org/wp-content/uploads/2020/04/ImaginingMobilityWithBeckn.pdf)
* [spec](https://github.com/beckn/protocol-specifications/)
* [website](https://beckn.org/)

## This contains two modules
* `app-backend` - frontend facing APIs (BA), has more end-user specific implementations
* `beckn-transport` - beckn provider (BP), implmenting mobility spec
* `epass-backend` - e-pass for travelling, has consumer and approver flows

## Shared libraries
* `beckn-core` - is the common library where the API types, unified data models will be there

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
