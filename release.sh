#!/usr/bin/env bash

set -eu

echo $PGP_SECRET | base64 --decode > gpg_key

gpg --import  --no-tty --batch --yes gpg_key

rm gpg_key

# ./mill fastparse.jvm[2.13.4].publish --sonatypeCreds "$SONATYPE_USERNAME:$SONATYPE_PASSWORD" --release true --gpgArgs --passphrase=$PGP_PASSPHRASE,--batch,--yes,-a,-b
# ./mill fastparse.jvm[2.12.13].publish --sonatypeCreds "$SONATYPE_USERNAME:$SONATYPE_PASSWORD" --release true --gpgArgs --passphrase=$PGP_PASSPHRASE,--batch,--yes,-a,-b
# ./mill fastparse.jvm[2.11.12].publish --sonatypeCreds "$SONATYPE_USERNAME:$SONATYPE_PASSWORD" --release true --gpgArgs --passphrase=$PGP_PASSPHRASE,--batch,--yes,-a,-b

./mill fastparse.js[2.13.4,1.4.0].publish --sonatypeCreds "$SONATYPE_USERNAME:$SONATYPE_PASSWORD" --release true --gpgArgs --passphrase=$PGP_PASSPHRASE,--batch,--yes,-a,-b
./mill fastparse.js[2.12.13,1.4.0].publish --sonatypeCreds "$SONATYPE_USERNAME:$SONATYPE_PASSWORD" --release true --gpgArgs --passphrase=$PGP_PASSPHRASE,--batch,--yes,-a,-b

./mill fastparse.native[2.13.4,0.4.0].publish --sonatypeCreds "$SONATYPE_USERNAME:$SONATYPE_PASSWORD" --release true --gpgArgs --passphrase=$PGP_PASSPHRASE,--batch,--yes,-a,-b
./mill fastparse.native[2.12.13,0.4.0].publish --sonatypeCreds "$SONATYPE_USERNAME:$SONATYPE_PASSWORD" --release true --gpgArgs --passphrase=$PGP_PASSPHRASE,--batch,--yes,-a,-b

