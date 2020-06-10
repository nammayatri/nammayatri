#!/bin/bash

set -e

ROOTDIR="$(dirname ${0})/.."

ln -sf ../../dev/pre-commit ${ROOTDIR}/.git/hooks/pre-commit
