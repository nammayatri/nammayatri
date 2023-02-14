# Registry encryption

## Algorithm
For encryption/decryption process system uses X25519 keys and AES encryption algorithm:

1. Registry takes its private key and our public key and calculates symmetric key with Curve25519 Diffie-Hellman function.
2. Registry encrypts challenge with calculated symmetric key using AES algorithm.
3. We take our private key and registry public key and calculate symmetric key with Curve25519 Diffie-Hellman function. Our symmetric key must be the same as symmetric key calculated on registry side.
4. We decrypt challenge with symmetric key and give it to registry.

## Keys generating
To generate keys you can use OpenSSL. There are some specifics however. You may try to find a more convenient way of work with OpenSSL and apply changes to the instructions below.

The first specific thing is that OpenSSL generates X25519 keys in PEM format and this is not what we want. You can open PEM files with text editor and see that it says that it contains private or public key. The problem here is that the actual key firstly was converted into ASN.1 format. This ASN.1 format also brings some metadata. After that key with metadata in ASN.1 format was converted into PEM format and this is what you see in PEM files. We need to get the actual key from PEM file somehow.

Generate and get an actual private key in Base16 format:
1. Run in terminal (this will create PEM file): openssl genpkey -algorithm x25519 -out x25519-priv.pem
2. Run in terminal: openssl asn1parse -in x25519-priv.pem -offset 14 | sed 's/.*\[HEX DUMP\]:\(.*\)/\1/'
3. Copy the output and paste it to configs. This is your private key in Base16 format. For now we convert it during run-time from Base16 format.

Generate and get an actual public key in Base64 format:
1. Run in terminal: openssl pkey -outform DER -pubout -in x25519-priv.pem | tail -c +13 | openssl base64
2. Copy the output. This is your public key in Base64 format that you can share with the registry.

Sources that led to these keys generation algotithms:
[Generating key pair in PEM format](https://stackoverflow.com/questions/43546712/how-to-generate-a-curve25519-key-pair-in-terminal)
[Getting private key in hex format without metadata from PEM file with private key](https://stackoverflow.com/questions/60689653/openssl-eddsa-specify-key-size)
[Getting public key in correct format from PEM file with private key](https://mta.openssl.org/pipermail/openssl-users/2018-March/007777.html)
