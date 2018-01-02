#!/bin/sh
# Script for deploying the package via Travis CI
set -ex
mkdir -p ~/.stack/upload
echo "{\"username\":\"${HACKAGE_USERNAME}\",\"password\":\"${HACKAGE_PASSWD}\"}" > ~/.stack/upload/credentials.json
stack upload . --no-signature
