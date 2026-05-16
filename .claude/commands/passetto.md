# Passetto Encrypt / Decrypt / Hash

Helper for the NammaYatri dashboard local-testing flow. Generates the encrypted
blobs (`*_encrypted` columns) and SHA-256 hashes (`*_hash` columns) that match
production schema for `atlas_bpp_dashboard.person`, `atlas_bap_dashboard.person`,
etc., using the local `passetto` service plus the dev `encHashSalt`.

## Service

- Endpoint: `http://127.0.0.1:8079`
- Started by: `, run-mobility-stack-dev`
- Source: `Backend/lib/passetto-lib/`, `Backend/dev/passetto-dev/`

## Wire format

- The encrypt/decrypt API expects the value pre-wrapped as Haskell `show` of the
  raw text — i.e. the literal `S"..."` (capital S followed by a double-quoted
  string). Without the `S"..."` wrapper, the encrypted payload will not round-trip
  via the Haskell server.
- The response `value` is `"<version>|<keyId>|<base64-ciphertext>"`
  (e.g. `0.1.0|0|...`). Store it verbatim in the `*_encrypted` column.

## Encrypt

```bash
curl -s --location 'http://127.0.0.1:8079/encrypt' \
  --header 'Content-Type: application/json' \
  --data-raw '{"value":"S\"fleet@dashboard.com\""}'
# -> {"value":"0.1.0|0|FmlW..."}
```

## Decrypt

```bash
curl -s --location 'http://127.0.0.1:8079/decrypt' \
  --header 'Content-Type: application/json' \
  --data '{"value":"0.1.0|0|FmlW..."}'
# -> {"value":"S\"fleet@dashboard.com\""}
```

The decrypted payload still has the `S"..."` Haskell wrapper — strip it to get
the plaintext.

## Hash (for `*_hash` columns)

Hash = `sha256(encHashSalt || plaintext)`, stored as `\x<hex>` bytea literal.

Dev `encHashSalt` (from `Backend/dhall-configs/dev/secrets/*.dhall`):

```
How wonderful it is that nobody need wait a single moment before starting to improve the world
```

One-liner:

```bash
python3 -c "
import hashlib, sys
salt = b'How wonderful it is that nobody need wait a single moment before starting to improve the world'
val = sys.argv[1].encode()
print('\\\\x' + hashlib.sha256(salt + val).hexdigest())
" 'fleet@dashboard.com'
# -> \x4d383194f7abb5422eed9d10feb4b30ac2ba06f88ff0e0aa35262d487db86d4a
```

Same algorithm is used for `email_hash`, `mobile_number_hash`, and
`password_hash` — only the input plaintext differs.

## Reference: full row generation

For a dashboard `person` row, you need:

| column                    | source                                          |
|---------------------------|-------------------------------------------------|
| `email_encrypted`         | `/encrypt` of `S"<email>"`                      |
| `email_hash`              | `sha256(salt + <email>)`, prefix `\x`           |
| `mobile_number_encrypted` | `/encrypt` of `S"<mobile>"`                     |
| `mobile_number_hash`      | `sha256(salt + <mobile>)`, prefix `\x`          |
| `password_hash`           | `sha256(salt + <password>)`, prefix `\x`        |

Existing examples in `Backend/dev/local-testing-data/provider-dashboard.sql`
(juspay_admin, fleet) demonstrate the final SQL shape.

## Gotchas

- Quoting: the JSON body's `value` is itself a JSON string, so the Haskell
  `S"..."` becomes `"S\"...\""` after escaping. Mismatched quoting is the
  most common cause of `decrypt` returning a parse error.
- Key id: `0.1.0|0|...` (key index `0`) is the only key in the local dev
  passetto setup; production rotates this. Do not hand-edit the index.
- Per-deployment salt: the `encHashSalt` differs between rider / provider /
  unified dashboards' `secrets/*.dhall` only by file path — they all use the
  same literal in dev. In production each environment has its own salt; never
  reuse a dev-generated hash against a prod DB.
