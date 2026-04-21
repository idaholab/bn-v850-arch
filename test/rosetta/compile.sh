#!/bin/bash
# Compile a C snippet via CC-RH (RH850/V850E3v5) and emit .obj (ELF V800) + .asm.
# Usage: compile.sh <name>.c [ccrh flags...]
set -euo pipefail
src="$1"; shift || true
dir="$(cd "$(dirname "$src")" && pwd)"
base="$(basename "$src" .c)"
docker run --rm --platform linux/amd64 -v "$dir":/w -w /w ccrh:2.07 \
    ccrh -Xcpu=g3kh -Xasm_path=. -Osize -Xobj_path=. "$@" "$(basename "$src")" >/dev/null 2>&1 || true
ls "$dir/$base.obj" "$dir/$base.asm" >/dev/null
echo "$dir/$base.obj"
