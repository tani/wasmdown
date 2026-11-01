#!/usr/bin/env bash
# Component-lane CENSUS: run the lane N times and require every run to
# be green (docs/design/PLAN.md §12.7's repeat-run stability requirement).
#
# The component lane carries the async/concurrency scenarios, whose
# failure MECHANISMS are scheduling lotteries: a single green run does
# not prove the lane, and a single red run must be attributable. This
# census therefore records, per run:
#   * the executed command count (a shrinking denominator is a fail),
#   * fail / harness-error / timeout / skip counts,
#   * the wall time,
# and then prints one table plus a verdict. Any run that is not
# GATE: GREEN fails the census with its full log preserved.
#
# Usage:
#   tools/run-component-census.sh [RUNS]      # RUNS defaults to 30
# Environment:
#   SBCL          compiler (default sbcl)
#   KEEP_LOGS=0   delete per-run logs on success (default: keep)
set -u

RUNS="${1:-30}"
SBCL="${SBCL:-sbcl}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
LOGDIR="${ROOT}/tmp-census"
mkdir -p "${LOGDIR}"

EXPECTED=1631        # pinned executed count (docs/design/PLAN.md §12.9 baseline)
failures=0
printf '%-6s %-10s %-8s %-6s %-6s %-8s\n' run executed fail herr tout wall
printf '%-6s %-10s %-8s %-6s %-6s %-8s\n' ---- -------- ---- ---- ---- ----

for i in $(seq 1 "${RUNS}"); do
    log="${LOGDIR}/run-${i}.log"
    start=$(date +%s%N)
    "${SBCL}" --control-stack-size 16 --dynamic-space-size 2048 \
        --script "${ROOT}/tools/run-component-suite.lisp" > "${log}" 2>&1
    rc=$?
    end=$(date +%s%N)
    wall=$(awk -v s="${start}" -v e="${end}" 'BEGIN{printf "%.2f", (e-s)/1000000000}')

    # the suite prints its per-outcome counts with UPERCASE keywords
    # (:FAIL -> "FAIL: 0"), so match case-insensitively
    executed=$(grep -o 'executed (denominator): [0-9]*' "${log}" | awk '{print $3}')
    failures_n=$(grep -io 'fail: [0-9]*' "${log}" | head -1 | awk '{print $2}')
    herr=$(grep -io 'harness-error: [0-9]*' "${log}" | head -1 | awk '{print $2}')
    tout=$(grep -io 'timeout: [0-9]*' "${log}" | head -1 | awk '{print $2}')
    green=no
    grep -q 'GATE: GREEN' "${log}" && green=yes

    printf '%-6s %-10s %-8s %-6s %-6s %-8s' "${i}" "${executed:-?}" \
        "${failures_n:-?}" "${herr:-?}" "${tout:-?}" "${wall}"
    if [ "${rc}" -eq 0 ] && [ "${green}" = yes ] \
       && [ "${executed:-0}" = "${EXPECTED}" ] \
       && [ "${failures_n:-x}" = "0" ] \
       && [ "${herr:-x}" = "0" ] && [ "${tout:-x}" = "0" ]; then
        printf ' ok\n'
    else
        printf ' RED (rc=%s, see %s)\n' "${rc}" "${log}"
        failures=$((failures + 1))
    fi
done

echo
if [ "${failures}" -eq 0 ]; then
    echo "CENSUS: GREEN (${RUNS}/${RUNS} runs, ${EXPECTED} commands each)"
    if [ "${KEEP_LOGS:-1}" = "0" ]; then rm -rf "${LOGDIR}"; fi
    exit 0
fi
echo "CENSUS: RED (${failures}/${RUNS} runs failed)"
exit 1
