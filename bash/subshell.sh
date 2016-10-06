#!/usr/bin/env bash
#
# Exhibit how to call a shell function in a subshell. As a side-effect, exhibit
# how to respond to a signal with a function, also from within a subshell.

set -euo pipefail

# The typical use for a subshell is temporarily change the working directory,
# so let's set up a temporary directory to change to.

# Set up a temporary directory and purge it on exit.
fdir=$(mktemp -d -p .)
echo "Created $(readlink -f "$fdir")"
function finish {
  echo "Removing $(readlink -f "$fdir")"
  rmdir "$fdir"
}
trap finish EXIT

function f {
  cd "${fdir}"

  # Show that trap is subshell-sensitive.
  gdir=$(mktemp -d -p .)
  echo "Created $(readlink -f "$gdir")"
  function finish {
    echo "Removing $(readlink -f "$gdir")"
    rmdir "$gdir"
  }
  trap finish EXIT
}

(f)
