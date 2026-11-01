#!/usr/bin/env bash
# Build a minimal JRE into target/jre for bundling with the Electron app.
set -euo pipefail
cd "$(dirname "$0")/.."
JAR=target/clj-browse.jar
[ -f "$JAR" ] || { echo "missing $JAR; run: clojure -T:build uber" >&2; exit 1; }
FALLBACK=java.base,java.desktop,java.logging,java.management,java.naming,java.net.http,java.sql,jdk.unsupported,java.xml,jdk.crypto.ec,jdk.zipfs,jdk.management
MODS=$(jdeps --print-module-deps --ignore-missing-deps --multi-release 21 "$JAR" 2>/dev/null || true)
if [ -z "$MODS" ]; then MODS=$FALLBACK; else MODS="$MODS,$FALLBACK"; fi
echo "modules: $MODS"
rm -rf target/jre
jlink --add-modules "$MODS" --strip-debug --no-header-files --no-man-pages --compress=zip-6 --output target/jre
echo "JRE at target/jre ($(du -sh target/jre | cut -f1))"
