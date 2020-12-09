This document describes the process of signing and verifying HTTP Messages. The spec is described here: https://tools.ietf.org/id/draft-cavage-http-signatures-12.html

BECKN project involves 3 entities that use HTTP Messages: BAP, BPP, BG.

## Common info

* The implementation uses `ed25519` algorithm for calculating the resulting signature value that is passed into headers.
* For calculating digest the implementation uses raw request as an input usign `Blake2b_512` hashing algorithm. The result of hash is then encoded with `Base64`.
* The Signature message is composed of `(created)`, `(expires)` and `digest` with their values and separated with `\n`
* Public and Private keys are encoded in `Base64`. Before using them they will need to be decoded first.

## Signing

For example let's take the raw input of `"{\"hello\": \"world\"}"` as a body and sign it:

1. Generate a `Blake2b_512` hex string from input body: `20cb8f1175aaa3f23f020b3962300c483ba33dda3f1ae32734605db4d834419f874f19963636ff0c79d45a054af895b20fdac745f354c865d938ef6e801b8e33`
2. Encode the hex string using `Base64` and concatenate `BLAKE-512=` prefix: `BLAKE-512=MjBjYjhmMTE3NWFhYTNmMjNmMDIwYjM5NjIzMDBjNDgzYmEzM2RkYTNmMWFlMzI3MzQ2MDVkYjRkODM0NDE5Zjg3NGYxOTk2MzYzNmZmMGM3OWQ0NWEwNTRhZjg5NWIyMGZkYWM3NDVmMzU0Yzg2NWQ5MzhlZjZlODAxYjhlMzM=`
3. Apply `(created)` and `(expires)` to final signature message with `digest`: 
```
(created): 1402170695\n
(expires): 1402170699\n
digest: BLAKE-512=MjBjYjhmMTE3NWFhYTNmMjNmMDIwYjM5NjIzMDBjNDgzYmEzM2RkYTNmMWFlMzI3MzQ2MDVkYjRkODM0NDE5Zjg3NGYxOTk2MzYzNmZmMGM3OWQ0NWEwNTRhZjg5NWIyMGZkYWM3NDVmMzU0Yzg2NWQ5MzhlZjZlODAxYjhlMzM=
```
4. Sign the message with `ed25519` algorithm and encode with `Base64`: `uMU5BXPKi2SunoIhHnpKeiSKY9Ekq5uiM7luxb51ztAtx62EqUWqGlYbhsnBNICgedEG+38w+m8n64/+BvBtAg==`
5. Form the header value for request:
```
Signature keyId="example-bg.com|bg432|ed25519",algorithm="ed25519",\n
created=1402170695,expires=1402170699,\n
headers="(created) (expires) digest",\n
signature="uMU5BXPKi2SunoIhHnpKeiSKY9Ekq5uiM7luxb51ztAtx62EqUWqGlYbhsnBNICgedEG+38w+m8n64/+BvBtAg=="
```

## Verifying

The verification process repeats same steps for signing process (1-3). After recreating the signing message it passes the payload to `ed25519` verification function with the signature and public key to verify the validity of the signature.