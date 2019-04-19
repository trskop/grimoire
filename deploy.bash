#!/usr/bin/env bash

set -ex

function main() {
    local repoUrl='git@github.com:trskop/trskop.github.io.git'
    local repoCloneDir='./trskop.github.io'

    if [[ ! -d '_site' ]]; then
        exit 1
    fi

    if [[ ! -d "${repoCloneDir}/.git" ]]; then
        git clone "${repoUrl}" "${repoCloneDir}"
    fi

    git -C "${repoCloneDir}" ls-files -z \
    | xargs --no-run-if-empty -0 git -C "${repoCloneDir}" rm

    cp -r _site/* "${repoCloneDir}/"
    git -C "${repoCloneDir}" add --all

    if (( $(git -C "${repoCloneDir}" status -s | wc -l) )); then
        git -C "${repoCloneDir}" commit -m "Release $(date --iso-8601=minutes)"
        git -C "${repoCloneDir}" push
    fi
}

main "$@"
