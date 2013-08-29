#!/bin/bash
HOOKDIR="$(git rev-parse --git-dir)/hooks"
HOOKS="pre-commit"

for HOOK in ${HOOKS} ; do
  ln -s $(readlink -f hooks/${HOOK}) ${HOOKDIR}/${HOOK}
done

