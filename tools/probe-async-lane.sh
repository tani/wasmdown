#!/bin/bash
# tools/probe-async-lane.sh — the ONLY sanctioned way to meter the component lane.
#
# Exists because every false verdict this project has produced came from an
# ad-hoc shell loop: a `git switch` that aborted mid-script (one "master" batch
# actually ran on a feature branch), a comparison between two batches taken
# minutes apart on a machine whose rate drifted 14 -> 22 fails/30 with no code
# change, and runs whose wall time was never recorded so a contaminated batch
# looked like a real difference.
#
# What it enforces:
#   1. TREE GUARD   — refuses to run unless the checked-out branch is the one you
#                     named, and re-checks it before EVERY run.
#   2. CLEAN GUARD  — refuses a dirty tree (only .envrc may be modified), and
#                     checks out the two generated artifacts first, because the
#                     lane rewrites them and a leftover diff aborts `git switch`.
#   3. QUIET GUARD  — refuses to run while any other sbcl is alive: a concurrent
#                     SBCL distorts this timing-pinned lane by CPU contention.
#   4. WALL RECORD  — times every run itself (the runner exits via sb-ext:exit and
#                     prints no wall line), so a contaminated batch is visible as
#                     a wall outlier instead of being read as a code difference.
#   5. NO SUMMARISING — one directory per batch with every per-run jsonl and a
#                     summary line; publish the batches side by side, never as an
#                     absolute number. Two identical-content batches here have
#                     measured 20 and 42 fail commands out of 30 runs, so ANY
#                     single-batch comparison is unresolved by construction.
#
# Usage:
#   tools/probe-async-lane.sh --branch master --runs 30 --label master-A
#   tools/probe-async-lane.sh --branch wip/type-view --runs 30 --label tv-A
#   tools/probe-async-lane.sh --branch master --runs 6 --label wip-diff --allow-dirty
#     (--allow-dirty meters an UNLANDED diff and prints its sha256 prefix, so a
#      subagent's working tree can be measured before it is committed. It is the
#      only way to measure work in progress; never use it to compare a landed
#      branch against a dirty tree.)
# then compare the two labels' fail-command totals. A difference smaller than
# ~22 fails/30 is not a result; it is the instrument's width.
#
# Exit code: 0 when the batch completed (regardless of lane outcome — this is a
# measurement, not a gate), 2 on a guard refusal, 3 if a run timed out or hung.
set -u
cd "$(dirname "$0")/.." || exit 2
ROOT=$(pwd)

BRANCH=""
RUNS=30
ALLOW_DIRTY=0
LABEL=""
TIMEOUT=200
GENERATED="tests/component-results.jsonl docs/conformance/component.md"

while [ $# -gt 0 ]; do
  case "$1" in
    --branch) BRANCH=$2; shift 2 ;;
    --runs)   RUNS=$2; shift 2 ;;
    --label)  LABEL=$2; shift 2 ;;
    --timeout) TIMEOUT=$2; shift 2 ;;
    --allow-dirty) ALLOW_DIRTY=1; shift ;;
    *) echo "unknown option: $1" >&2; exit 2 ;;
  esac
done

[ -n "$BRANCH" ] || { echo "REFUSING: --branch is required (no implicit tree)" >&2; exit 2; }
[ -n "$LABEL" ]  || LABEL="$BRANCH-$(date +%H%M%S)"
OUT="$ROOT/tests/lane-batches/$LABEL"

# ---- guard 1: the tree ------------------------------------------------------
NOW=$(git branch --show-current)
if [ "$NOW" != "$BRANCH" ]; then
  echo "REFUSING: on branch '$NOW', you asked for '$BRANCH'. Switch deliberately, then rerun." >&2
  exit 2
fi

# ---- guard 2: the working tree ---------------------------------------------
DIRTY=$(git status --porcelain \
          | grep -v '^ M \.envrc$' \
          | grep -v '^?? tools/probe-async-lane.sh$' \
          | grep -v '^?? tests/lane-batches/' || true)
if [ -n "$DIRTY" ] && [ "$ALLOW_DIRTY" = "0" ]; then
  echo "REFUSING: working tree is not clean (only .envrc may be modified):" >&2
  echo "$DIRTY" >&2
  echo "Check out generated artifacts with:" >&2
  echo "  git checkout -- $GENERATED" >&2
  exit 2
fi
if [ -n "$DIRTY" ] && [ "$ALLOW_DIRTY" = "1" ]; then
  echo "NOTE: metering an UNLANDED diff (--allow-dirty): the batch describes this"
  echo "      working tree, not the branch tip. Record the diff hash with the result:"
  git diff | sha256sum | cut -c1-16
fi

# ---- guard 3: machine quietness --------------------------------------------
OTHER=$(pgrep -c sbcl 2>/dev/null || true)
if [ "${OTHER:-0}" != "0" ]; then
  echo "REFUSING: $OTHER other sbcl process(es) alive — this lane is timing-pinned." >&2
  pgrep -a sbcl >&2
  exit 2
fi

mkdir -p "$OUT"
echo "batch label : $LABEL"
echo "branch      : $BRANCH ($(git rev-parse --short HEAD))"
echo "runs        : $RUNS   timeout/run: ${TIMEOUT}s"
echo "output      : $OUT"

RED=0
TOTAL=0
WALLMIN=""
WALLMAX=""
for i in $(seq 1 "$RUNS"); do
  # guard 1 again, per run: a branch switch inside the loop is the exact bug
  # that produced a fabricated verdict once.
  CUR=$(git branch --show-current)
  if [ "$CUR" != "$BRANCH" ]; then
    echo "ABORT at run $i: tree is '$CUR', not '$BRANCH'." >&2
    exit 2
  fi

  START=$(date +%s.%N)
  timeout "$TIMEOUT" sbcl --control-stack-size 16 --dynamic-space-size 2048 \
      --script tools/run-component-suite.lisp > "$OUT/run-$i.log" 2>&1
  RC=$?
  END=$(date +%s.%N)
  WALL=$(awk "BEGIN{printf \"%.2f\", $END-$START}")

  if [ "$RC" = "124" ]; then
    echo "HANG at run $i (timeout ${TIMEOUT}s) — see $OUT/run-$i.log" >&2
    echo "hang $i wall=$WALL" >> "$OUT/summary.txt"
    exit 3
  fi

  cp tests/component-results.jsonl "$OUT/run-$i.jsonl"
  read -r K IDS <<EOF
$(python3 - "$OUT/run-$i.jsonl" <<'PY'
import json,sys
rows=[json.loads(l) for l in open(sys.argv[1])]
bad=[r['case_id'].split('/')[-1] for r in rows if r.get('outcome')=='fail']
print(len(bad), ','.join(bad) if bad else '-')
PY
)
EOF
  TOTAL=$((TOTAL + K))
  [ "$K" != "0" ] && RED=$((RED + 1))
  case "$WALL" in
    "") ;;
    *) if [ -z "$WALLMIN" ] || awk "BEGIN{exit !($WALL < $WALLMIN)}"; then WALLMIN=$WALL; fi
       if [ -z "$WALLMAX" ] || awk "BEGIN{exit !($WALL > $WALLMAX)}"; then WALLMAX=$WALL; fi ;;
  esac
  printf 'run %2d  fails=%-2s wall=%-6s %s\n' "$i" "$K" "$WALL" "$IDS"
  printf 'run %s fails=%s wall=%s rows=%s\n' "$i" "$K" "$WALL" "$IDS" >> "$OUT/summary.txt"
done

echo
echo "== batch $LABEL: red_runs=$RED/$RUNS fail_commands=$TOTAL walls=${WALLMIN:-?}..${WALLMAX:-?}s"
echo "red_runs=$RED runs=$RUNS fail_commands=$TOTAL wall_min=${WALLMIN:-?} wall_max=${WALLMAX:-?}" >> "$OUT/summary.txt"
echo "== READ THE DIFFERENCE, NOT THE NUMBER. Two identical-content batches have"
echo "   measured 20 and 42 fail commands / 30 runs here, so a delta below ~22 is"
echo "   the instrument's width, not a result. Compare against the master batch"
echo "   taken in the SAME window."
