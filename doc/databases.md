# Databases and Redis

Each application uses its own database. In addition, there is one instance of Redis that is shared by all the applications.

In the local development environment, the databases are running in the docker containers.
To look at the database data, one can, for example use:
- psql
- pgadmin
- vscode plugins

Connection details can be found in the individual `.dhall` configuration files.

