# Authentication

Frontend APIs use api key authenication, and Beckn APIs use [signature authentication](doc/signatures.md).

Both types of authentication are implemented in a way so that the result of the authentication (for example, a person or an organization) is passed as the first parameter to the endpoint handler.
