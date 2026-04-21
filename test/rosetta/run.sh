#!/bin/bash
# End-to-end rosetta: C snippet -> ccrh bytes+asm -> plugin disasm -> side-by-side.
# Usage: run.sh fixtures/<name>.c
set -euo pipefail
src="$1"
HERE="$(cd "$(dirname "$0")" && pwd)"
HARNESS="${HARNESS:-$HOME/src/binaryninja-api/out/bin/rosetta_harness}"
"$HERE/compile.sh" "$src"
obj="${src%.c}.obj"
recs="$(python3 "$HERE/extract.py" "$obj")"
echo "=== $(basename "$src") ==="
python3 - "$recs" "$HARNESS" <<'PY'
import json, subprocess, sys
recs = json.loads(sys.argv[1])
harness = sys.argv[2]
for r in recs:
    print(f"\n--- {r['name']} @ 0x{r['addr']:x} ({r['size']}B) ---")
    out = subprocess.run([harness, r['bytes_hex'], hex(r['addr'])],
                         capture_output=True, text=True, check=True).stdout.splitlines()
    # Side-by-side: ccrh (oracle) vs plugin
    for i, (ccrh, plug) in enumerate(zip(r['asm'] + ['']*len(out), out)):
        mark = '  ' if ccrh.split()[0:1] and plug.split()[3:4] and ccrh.split()[0] == plug.split()[3] else 'X '
        print(f"{mark}{plug:<55} | ccrh: {ccrh}")
PY
