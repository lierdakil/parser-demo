#!/bin/bash

sr="$(stack path --local-install-root)/bin/parser-demo-exe.jsexe"

git worktree add out gh-pages
closure-compiler -O ADVANCED_OPTIMIZATIONS --externs "$sr/all.js.externs" "$sr/all.js" > out/all.min.js || exit $?
pushd out
git commit -am 'deploy'
git push origin HEAD
popd
rm -rf out
git worktree prune
