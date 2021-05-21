# Project structure
## Modules
### `app-backend` 
Frontend facing APIs (BAP), has more end-user specific implementations.
### `beckn-transport` 
Beckn provider (BPP), implementing mobility spec. This module contains two executables:
 * `beckn-transport-exe`:  BPP server
 * `beckn-transport-btm-exe`:  background task manager. This component runs the allocation service responsible for allocating rides to drivers.
### `beckn-gateway` 
Beckn gateway (BG)
### `fmd-wrapper` 
Beckn provider (BPP) for Dunzo

### `beckn-core`
Common library where the API types, unified data models will be there

## Spec versions

Mobility module and related types conform to beckn spec [mobility-0.8.2](https://github.com/beckn/protocol-specifications/tree/mobility-v0.8.2)

FMD module and related types conform to beckn spec [fmd-0.8.3](https://github.com/beckn/protocol-specifications/tree/fmd-0.8.3)

