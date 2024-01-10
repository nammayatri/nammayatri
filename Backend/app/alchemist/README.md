# Namma DSL Tutorial

This tutorial provides a comprehensive guide to understanding and working with the Domain Specific Language (NammaDSL) in the NammaYatri. It covers the creation of YAML files, code generation, and compilation, as well as the syntax for defining APIs and storage.

## Creating YAML Files

1. **Location**: Create a YAML file inside the `spec` folder of the module you are working on. For instance, if you are working on the `rider-app`, the path would be:
    ```
    rider-platform/rider-app/spec
    ```
    Depending on the type of specification you are defining, you may place the file inside either the `API` or `Storage` folder.

2. **Code Generation**: After defining the YAML files, execute the following command to generate the code:
    ```
    , run-generator
    ```
    This command generates Haskell Beam, query, and domain files in the `src-read-only` directory, as well as the SQL queries.

3. **Compilation**: Compile the code using:
    ```
    cabal build all
    ```

## Syntax for API DSL

- `imports`: Used for importing predefined types.
- `module`: Specifies the name of the module.
- `types`: Defines the request and response types for your APIs. This field is optional. Types are defined in the following format:
    ```
    {TypeName}:
      {field1}: {field1Type}
      {field2}: {field2Type}
    ```
    Enum types can be defined as:
    ```
    {TypeName}:
      enum: {enum1}|{enum2}
    ```
- `apis`: Contains all the APIs in the following format:
    - `{httpMethod}` (HTTP method GET|POST|PUT|DELETE)
        - `endpoint`: API path
        - `response`:
          - `type`: Type of response
        - `request`:
          - `type`: Type of request (optional)
        - `auth`: Authentication method (default: TokenAuth)
        - `query`: List of query parameters
          ```
          {queryParam1}: {queryParam1Type}
          ```
        - `mandatoryQuery`: List of mandatory query parameters
          ```
          {mandatoryQueryParam1}: {mandatoryQueryParam1Type}
          ```
        - `params`: List of path parameters
          ```
          {pathParam1}: {pathParam1Type}
          ```

## Syntax for Storage DSL

- `imports`: Used for importing predefined types.
- `{moduleName}`: Specifies the name of the module.
  - `tableName`: Name of the table.
  - `fields`: Lists all fields of the table with Haskell type.
      ```
      {field1}: {field1Type}
      ```
  - `types`: User-defined types, similar to API types.
  - `default`: Default value for fields, if any.
  - `queries`: All Beam queries for the table.
  - `fromTType`: `FromTType` of fields, if applicable.
  - `toTType`: `ToTType` of fields, if applicable.
