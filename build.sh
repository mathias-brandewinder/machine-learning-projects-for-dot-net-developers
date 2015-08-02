#!/usr/bin/env bash
set -e

echo 'Restoring nugets'
[ -f paket.exe ] || mono paket.bootstrapper.exe
mono paket.exe install

# echo 'Building'
# find . -type f -name "*.sln" -exec xbuild {} \;