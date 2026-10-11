#!/usr/bin/env bash
# Checks that terminating tadka-ghc itself also stops the build it started.
#
# The build runs in its own process group, so a signal sent to tadka-ghc no
# longer reaches it automatically; tadka-ghc must stop the group itself.
# This runs the real executable against a fake `cabal` whose child process
# would run for 37 seconds, terminates tadka-ghc with SIGTERM and SIGHUP, and
# fails if the build is still running afterwards or the exit status is wrong.
#
# Usage: scripts/check-signal-cleanup.sh /path/to/tadka-ghc
set -u
BIN="${1:?usage: check-signal-cleanup.sh /path/to/tadka-ghc}"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/tadka-signal.XXXXXX")"
trap 'pkill -fx "sleep 37" 2>/dev/null; rm -rf "$WORK"' EXIT

mkdir -p "$WORK/bin" "$WORK/proj"
: > "$WORK/proj/p.cabal"
cat > "$WORK/bin/cabal" <<'FAKE'
#!/bin/sh
sleep 37 &
echo "Building library for pkg-0.1..." >&2
wait
FAKE
chmod +x "$WORK/bin/cabal"

FAILS=0
check() { # signal-name  expected-exit-status
  PATH="$WORK/bin:$PATH" "$BIN" --cabal --narratable "$WORK/proj" > /dev/null 2>&1 &
  local pid=$!
  sleep 1.5                                  # the fake build and its child are running
  local before; before=$(pgrep -fx 'sleep 37' | wc -l)
  kill -"$1" "$pid"
  wait "$pid"; local status=$?
  sleep 1
  local after; after=$(pgrep -fx 'sleep 37' | wc -l)
  if [ "$before" -ge 1 ] && [ "$after" = 0 ] && [ "$status" = "$2" ]; then
    echo "PASS  SIG$1: tadka-ghc exited $status and the build was stopped (running before: $before, after: $after)"
  else
    echo "FAIL  SIG$1: exit status $status (expected $2); build processes before=$before after=$after (expected >=1 and 0)"
    FAILS=$((FAILS + 1))
  fi
  pkill -fx 'sleep 37' 2>/dev/null
}
# (SIGINT is not tested here: a non-interactive shell starts background jobs
# with SIGINT ignored. GHC's runtime turns an interactive Ctrl-C into an
# exception, and the library's tests cover an exception in the caller.)
check TERM 143
check HUP 129
exit "$FAILS"
