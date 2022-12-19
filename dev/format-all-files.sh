#!/bin/bash

HS_FILES=$(find . -type d \( -path "*/.stack-work" -o -path "*/.hie" \) -prune -o -name '*.hs' -print)

if ! which ormolu >& /dev/null; then
  echo "Could not find 'ormolu', is it installed?"
  exit 1
fi

BAD_HS_FILES=""

for file in $HS_FILES; do
  if ! ormolu -m check -o '-XTypeApplications' -o '-fplugin=RecordDotPreprocessor' "${file}"; then
    BAD_HS_FILES+=" ${file}"
    FAIL="true"
  fi
done

if [ -n "$FAIL" ]; then
  ORMOLU_CMD="ormolu -m inplace -o -fplugin=RecordDotPreprocessor ${BAD_HS_FILES}"

  echo "Code style issues found in:"
  echo "${BAD_HS_FILES}"

  if [ -n "$BECKN_DISABLE_AUTOFORMAT" ]; then
    echo "Fix them with:"
    echo " ${ORMOLU_CMD}"
  else
    echo "Running Ormolu, please check and git add the changes"
    ${ORMOLU_CMD}
  fi
fi


DHALL_FILES=$(find . -name '*.dhall' -print)

if ! which dhall >& /dev/null; then
  echo "Could not find 'dhall', is it installed?"
  exit 1
fi

for file in $DHALL_FILES; do
  DHALL_CMD="dhall format ${file}"
  ${DHALL_CMD}
  if ! dhall --file "${file}" 1>/dev/null; then
    BAD_DHALL_FILES+=" ${file}"
    DHALL_FAIL="true"
  fi
done

if [ -n "$DHALL_FAIL" ]; then
  echo "Invalid dhall files:"
  echo "${BAD_DHALL_FILES}"
  FAIL="true"
fi

if [ -n "$FAIL" ]; then
  exit 1
else
  exit 0
fi