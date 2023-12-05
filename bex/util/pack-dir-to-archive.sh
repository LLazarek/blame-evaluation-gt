#!/bin/bash

if [[ "$1" = "-h" || "$1" = "--help" || "$1" = "" ]]; then
    echo "Usage: pack.sh <directory> [<archive-name>]"
    exit 1
fi

DIRNAME="$1"
NEW_NAME="$2"

if [ "$NEW_NAME" != "" ]; then
  if [ -d "$NEW_NAME" ]; then
    rm -r "$NEW_NAME"
  fi
  mv "$DIRNAME" "$NEW_NAME"
  mkdir "$DIRNAME"
  DIRNAME="$NEW_NAME"
fi

if [ -f "$DIRNAME".tar.gz ]; then
  rm "$DIRNAME".tar.gz
fi

tar -czf "$DIRNAME".tar.gz "$DIRNAME"

