"""wasi-testsuite runtime adapter for wasmdown (PLAN.md §9.3 F1).

The upstream runner (`run-tests --runtime-adapter adapters/wasmdown.py`)
imports this file as a module and calls `compute_argv` once per test. Each
guest becomes one fresh `sbcl --script tools/wasmdown.lisp run ...` process,
so a crash/trap cannot leak state into the next test.

Contract (wasi-testsuite `test-runner/wasi_test_runner/runtime_adapter.py`):
  get_name / get_version
  get_wasi_versions / get_wasi_worlds
  get_timeout_seconds (optional; upstream default is 5 s)
  compute_argv(test_path, (args, env, root), proposals, wasi_world, wasi_version)

Declared profile: `wasi:cli/command` only, `wasm32-wasip1` + `wasm32-wasip3`.
Proposal-tagged tests (`http`, `sockets`) are excluded by design and are
skipped by tools/run-wasi-testsuite.py's generated expectations file; the
assertion below keeps an accidental execution an explicit error instead of a
silent unsupported run.

Environment:
  WASMDOWN_ROOT                    repository root (default: parent of adapters/)
  SBCL                             SBCL command line (default: "sbcl")
  WASMDOWN_WASI_VERSIONS           comma-separated claim (default: both ABIs)
  WASMDOWN_WASI_TESTSUITE_TIMEOUT  per-test wall timeout in seconds (default: 60)
"""

import os
import shlex
from pathlib import Path
from typing import Dict, List, Optional, Tuple

ROOT = Path(os.environ.get(
    "WASMDOWN_ROOT", Path(__file__).resolve().parent.parent))
SBCL = shlex.split(os.getenv("SBCL", "sbcl"), posix=os.name != "nt")
TIMEOUT_SECONDS = float(os.getenv("WASMDOWN_WASI_TESTSUITE_TIMEOUT", "60"))

_DEFAULT_VERSIONS = "wasm32-wasip1,wasm32-wasip3"
WASI_VERSIONS = [
    v.strip() for v in
    os.getenv("WASMDOWN_WASI_VERSIONS", _DEFAULT_VERSIONS).split(",")
    if v.strip()
]


def get_name() -> str:
    return "wasmdown"


def get_version() -> str:
    return "0.0.1"


def get_wasi_versions() -> List[str]:
    return list(WASI_VERSIONS)


def get_wasi_worlds() -> List[str]:
    return ["wasi:cli/command"]


def get_timeout_seconds() -> float:
    # SBCL image startup dominates the older 5 s default; the runtime is an
    # interpreter, so server-less guests still finish in a few seconds.
    return TIMEOUT_SECONDS


def compute_argv(test_path: str,
                 args_env_root: Tuple[List[str], Dict[str, str], Optional[str]],
                 proposals: List[str],
                 wasi_world: str,
                 wasi_version: str) -> List[str]:
    if proposals:
        raise RuntimeError(
            "wasmdown declares no WASI proposals, but test "
            f"{test_path} requested {proposals}; the driver must exclude it "
            "by a declared rule (wasi:http / wasi:sockets exclusions)")
    args, env, root = args_env_root
    argv = SBCL + [
        "--script", str(ROOT / "tools" / "wasmdown.lisp"),
        "run", test_path, "--wasi",
    ]
    for key in sorted(env):
        argv += ["--env", f"{key}={env[key]}"]
    if root:
        argv += ["--dir", f"{Path(root).resolve()}::/"]
    argv += ["--"] + list(args)
    return argv
