### Run load testing

Beckn project uses K6 framework for initiating load tests. Before running them make sure you have k6 CLI installed on your local machine. Refer to https://k6.io/docs/getting-started/installation.

Once install start provider platform server:

```
stack run beckn-transport-exe
```

And then run Beckn CLI command for testing:

```
stack run beckn-cli-exe -- --private-key *BASE64_ENCODED_PRIVATE_KEY* --requests *NUMBER_OF_REQUESTS_TO_RUN*
```

Keep in mind that you need to use the same private key for signing requests that is available for provider platform instance. You can use the default key that is intended for DEV environment which is set in `dhall-configs/dev/secrets/common.dhall`

Full option description:

```
Usage: beckn-cli-exe (--private-key PRIVATEKEY [--requests INT] [--url URL]
                       [--file-path FILEPATH] |
                       --generate-key-pair)

Available options:
  --private-key PRIVATEKEY Private key for signing requests
  --requests INT           How many requests to generate (default: 100)
  --url URL                URL to
                           test (default: "http://127.0.0.1:8014/v1/7f7896dd-787e-4a0b-8675-e9e6fe93bb8f")
  --file-path FILEPATH     Path to file with generated
                           data. (default: "/tmp/req-data.json")
  --generate-key-pair      Generate public/private key pair.
  -h,--help                Show this help text
```

### Generate public/private key pair

You can use the following command to generate public/private key pair that is used for Registry. Currently it uses `Ed25519` algorithm for keys.

```
stack run beckn-cli -- --generate-key-pair
```
