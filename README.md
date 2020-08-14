# beckn
Beckn is an open protocol that enables location-aware, local commerce across industries.

More about beckn:

* [whitepaper](https://beckn.org/wp-content/uploads/2020/04/WhatIsBeckn.pdf)
* [mobility](https://beckn.org/wp-content/uploads/2020/04/ImaginingMobilityWithBeckn.pdf)
* [spec](https://github.com/beckn/protocol-specifications/)
* [website](https://beckn.org/)

FMD module and related types conform to beckn spec [fmd-0.8.0](https://github.com/beckn/protocol-specifications/tree/fmd-0.8.0)


## This project contains three modules
* `app-backend` - frontend facing APIs (BA), has more end-user specific implementations
* `beckn-transport` - beckn provider (BP), implmenting mobility spec

## Shared libraries
* `beckn-core` - is the common library where the API types, unified data models will be there

## How to run Beckn backends in a dev environment:

### Setup

Set up your development environment, from project root run:

```
./dev/setup.sh
```

### Compile the project with

```
stack build
```

### Run the databases and redis inside Docker

```
cd dev && docker-compose up
```

### Start the app-backend server

```
stack exec app-backend-exe
```

### Start the transporter server

```
stack exec beckn-transport-exe
```