# Atlas
<hr style="height: 1px; margin-top:-15px;">

Atlas is a set of mobility application running on the top of beckn. Beckn is an open protocol that enables location-aware, local commerce across industries. Know more about [beckn](https://beckn.org/).


## Table of Contents
<hr style="height: 1px; margin-top:-15px;"></hr>

  * [Getting Started](#getting-started)
    + [Pre-requisite to get started](#pre-requisite-to-get-started)
    + [Installation](#installation)
    + [Compilation](#compilation)
    + [Running the services](#running-the-services)
    + [Running the applications](#running-the-applications)
  * [Usage](#usage)
  * [Contribution](#contribution)
  * [Project structure](#project-structure)
    + [Modules](#modules)
    + [Flow](#flow)
  * [Troubleshooting](#troubleshooting)
  * [Contact](#contact)
  * [License](#license)

## Getting Started
<hr style="height: 1px; margin-top:-15px;"></hr>

### Pre-requisite to get started

1. [Docker](https://www.docker.com/products/docker-desktop/)

2. [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

For Mac

1. [Xcode](https://developer.apple.com/xcode/)

2. [Home brew](https://brew.sh)

### Installation

To set up your development environment, from project root run

```
$ ./dev/setup.sh
```

Install haskell linter and formatter by running this command in home directory

```
$ stack install hlint-3.2.7 ormolu-0.1.4.1
```

Mac (other dependencies)

```
$ brew install libpq
$ brew install librdkafka
$ brew install postgres
$ brew install dhall
$ brew install jq
$ brew install parallel
```

Linux (other dependencies)

```
$ sudo apt-get install libpq-dev
$ sudo apt-get install librdkafka-dev
$ sudo apt-get install postgresql
$ sudo apt-get install dhall
$ sudo apt-get install jq
$ sudo apt-get install parallel
```

### Compilation

Compile the project with
```
$ stack build --fast
```
> **_Note:_**  For deployment, `stack build` command should be used to compile with optimizations.

### Running the services

 For running the database, redis, passetto and kafka run this command
```
$ make run-svc
```

For running pgadmin run this command
```
$ make run-pgadmin
```

For running monitoring services like prometheus and grafana use this command
```
$ make run-monitoring
```

### Running the applications
For running all the applications use this command
```
$ make run-mobility-stack
```

## Usage
<hr style="height: 1px; margin-top:-15px;"></hr>

Each of the application has particular set of defined APIs and Schemas. To get available APIs/Schemas of any particular application, follow these steps

1. Copy the swagger json from `http://localhost:<port>/swagger` and use the relevant port (enlisted below)

| Application                              | Port   |
| -----------------------------------------|--------|
| app-backend                              | `8013` |
| beckn-transport                          | `8014` |
| beckn-gateway                            | `8015` |
| mock-registry                            | `8020` |
| public-transport-bap                     | `8023` |
| public-transport-search-consumer         | `8024` |
| search-result-aggregator                 | `8025` |
| scheduler-example-scheduler              | `8051` |
| transporter-scheduler                    | `8053` |
| allocation-service                       | `9996` |

2. Paste the copied swagger json inside https://editor.swagger.io/
3. To run the requests one can use the Postman or any other API platform.

## Contribution
<hr style="height: 1px; margin-top:-15px;"></hr>

To contribute, follow these steps:

1. Fork this repository.
2. Create a branch: `git checkout -b <branch_name>`. Branch name should consists of two or three parts, separated by /:
   1. Either of three: _feature_, _fix_, _refactor_
   2. Task code, e.g. BKN-123
   3. Human readable description in kebab case
3. Make your changes and commit them: `git commit -m '<commit_message>'`. A good practice is to prefix commit with task code, like `[BKN-1037] Replace Proxy-Authorization -> X-Gateway-Authorization` it also becomes clickable and this commit is added to task even if it is in some branch that is not directly related to a PR. But this is not mandatory.
4. Push to the original branch: `git push origin <project_name>/<location>`
5. Create the pull request.

Alternatively see the GitHub documentation on [creating a pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request).

## Project structure
<hr style="height: 1px; margin-top:-15px;"></hr>

### Modules
<hr style="height: 1px; margin-top:-10px;"></hr>

#### `app-backend`
Frontend facing APIs (BAP), has more end-user specific implementations.
#### `beckn-transport`
Beckn provider (BPP), implementing mobility spec. This module contains two executables:
 * `beckn-transport-exe`:  BPP server
 * `beckn-transport-btm-exe`:  background task manager. This component runs the allocation service responsible for allocating rides to drivers.
#### `beckn-gateway`
Beckn gateway (BG)

#### `beckn-core`
Common library where the API types, unified data models will be there

## Troubleshooting
<hr style="height: 1px; margin-top:-15px;"></hr>

If you are facing any issue with the application, you can debug it through logs

#### Application logs

Every log line consists of the following sections - timestamp, log level (INFO, WARNING, ERROR, DEBUG), host name, log tags and log data.

Application logs are under beckn index in Kibana

<i>Sample application log line</i>
```
2022-06-06 10:55:24.854380462 UTC INFO> @beckn-transport-sandbox-fd6c557fc-qp5xs [requestId-6d567ca3-3a86-424c-8a59-55335eb8e605,
driverLocationUpdate] |> got location updates: 6f7ecd55-0be2-45aa-8fa3-779c6e07b0d9 [{"ts":"2022-06-06T10:55:24.938Z","pt":{"lat":9.9816358,"lon":76.2998842},"acc":6}]
```

Details of sections in the above log line example

* <b>Timestamp</b>: `2022-06-06 10:55:24.854380462 UTC`
* <b>Log level</b>: `INFO`
* <b>Host name</b>: `beckn-transport-sandbox-fd6c557fc-qp5xs`
* <b>Log tags</b>: `requestId-6d567ca3-3a86-424c-8a59-55335eb8e605, driverLocationUpdate`
* <b>Log data</b>:
    ```
    got location updates: 6f7ecd55-0be2-45aa-8fa3-779c6e07b0d9
    [{"ts":"2022-06-06T10:55:24.938Z","pt":{"lat":9.9816358,"lon":76.2998842},"acc":6}]
    ```

Depending on the log defined in code, log tags and log data will change, rest of the sections values will depend on the runtime environment.

For logs related to incoming requests, log tags will include request-id generated by the load balancer, using which we can track the logs for a particular request. In the above example, we can search for requestId-6d567ca3-3a86-424c-8a59-55335eb8e605 in Kibana (beckn index) to get logs related to that API.

Some logs are tagged with transaction-ids or person-ids. You can search for relevant log tags and view logs related to that. To see all tags being used, check for withLogTag function usage in code.

**Commonly used log search keywords**

| Keyword | Detail |
|---------|--------|
|`Beckn-[deployment-name]-[master|sandbox]` | To view all logs related to that deployment. Replace [deployment-name] with its value and [master\|sandbox] depending on which environment you are looking for
|`"UTC ERROR"` | To look for all errors
|`"Request&Response"` | Application incoming API log. Contains all header information along with response status code
|`txnId` |  To get transactionIds tagged to incoming Beckn APIs. Useful to check APIs related to one transaction.

Any of the above keywords can be combined with operators like AND, NOT, OR etc. (see KQL doc for more info) to refine logs further.

## Contact
<hr style="height: 1px; margin-top:-15px;"></hr>

// TODO

## License
<hr style="height: 1px; margin-top:-15px;"></hr>

// TODO

hello
