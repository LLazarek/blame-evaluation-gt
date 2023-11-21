#!/bin/bash

## ASSUMPTIONS:
# 1. All of the arguments to this script are base64-encoded strings

declare -a args
for arg in "$@"; do
        args+=("$(echo "$arg" | base64 --decode)")
done

timeout -k 5 ${args[0]} "${args[@]:1}"

