#!/usr/bin/bash
# Used for applying changes in secrets of dhall files

DRY_RUN_FILE="dhall-secrets.yaml"
DHALL_CONFIG_DIR="$(dirname $0)"
if [[ -z "$@" || !($# == 1 || $# == 2) ]];
then
  echo """Usage: $0 [branch-name] [--apply]
  using without --apply will dry-run the create $DRY_RUN_FILE file for review.
  using with --apply will apply the secrets
  """
  exit 0
fi
APPLY_CONFIG=false
BRANCH_NAME=""
for opt in $@
do
  case $opt in
  --apply)
    APPLY_CONFIG=true
    ;;
  *)
    BRANCH_NAME=$opt
    ;;
  esac
done

SECRET_NAME="beckn-dhall-secret-$BRANCH_NAME"
NS="atlas"
if [[ "$APPLY_CONFIG" == true ]];
then
  kubectl create secret generic $SECRET_NAME --from-file=$DHALL_CONFIG_DIR/secrets --from-file=$DHALL_CONFIG_DIR/generic/secrets -o yaml -n $NS && \
  echo "Applied $SECRET_NAME config in $NS namespace"
else
  kubectl create secret generic $SECRET_NAME --from-file=$DHALL_CONFIG_DIR/secrets --from-file=$DHALL_CONFIG_DIR/generic/secrets -o yaml -n $NS --dry-run=client > $DRY_RUN_FILE && \
  echo "Created $DRY_RUN_FILE in $(pwd)"
fi