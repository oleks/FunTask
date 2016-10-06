#!/usr/bin/env bash
#
# Exhibit how to call a shell function in a subshell. As a side-effect, exhibit
# how to respond to a signal with a function.

# The typical use for a subshell is temporarily change the working directory,
# so let's set up a temporary directory to change to.

# Set up a temporary directory and purge it on exit.
tmpdir=$(mktemp -d -p .)
function finish {
  rm -rf "$tmpdir"
}
trap finish EXIT

function f {
  cd "${tmpdir}"
  pwd
}

pwd
(f)
pwd
