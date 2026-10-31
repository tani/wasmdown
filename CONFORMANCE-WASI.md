# WASI conformance lane (all, interpret)

- runtime: `wasmdown 0.0.1` on `SBCL 2.6.4`
- repetitions: 30; cases per run: 15; total results: 450
- result records: `tests/wasi-results/final/results.jsonl` (wasmdown.test-result.v1)

| ABI | planned | executed | pass | fail |
| --- | --- | --- | --- | --- |
| preview1 | 120 | 120 | 120 | 0 |
| preview2 | 330 | 330 | 330 | 0 |

**450/450 pass**

