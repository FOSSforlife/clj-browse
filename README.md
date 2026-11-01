# clj-browse

A code browser for Clojure that works across many repos. It indexes every namespace, file and top-level form, plus their docstrings and usages, into one database, and keeps a record for each commit. You browse the result in an outliner styled after Logseq, can edit docstrings in place, and can run any function through the repo's nREPL from a dialog with one input per parameter.

It runs as a local web app (`clj-browse serve`) or as a desktop app (Electron around the same server).

See [PLAN.md](PLAN.md) for the full design and the decisions behind it.

## Requirements

- Java 21+ (developed on Java 27), Clojure CLI, `git`
- Node 20+ (frontend build and the Electron shell)
- Babashka, for the dev tasks and the bb-tasks panel
- Optional: `lein` / `npx shadow-cljs` if you start REPLs for those project types

## Quick start (development)

```bash
npm install
bb dev          # Tailwind watch + shadow-cljs watch + server on http://127.0.0.1:7878
```

Open http://127.0.0.1:7878, then click **+ / Add repos**. You can scan a folder that contains repos (nothing is added until you choose) or add a single repo. Indexing starts automatically.

Other tasks (`bb tasks`):

| Task | What it does |
|---|---|
| `bb test` | Run the test suite: Kaocha, with real git fixture repos and real nREPL processes |
| `bb lint` | Run clj-kondo |
| `bb uber` | Build `target/clj-browse.jar`, which includes the release frontend |
| `bb jre` | Build a minimal JRE with jlink in `target/jre` |
| `bb electron:dev` | Run the desktop shell against the uberjar |
| `bb package` | Build AppImage/deb (Linux) or dmg (macOS) into `electron/dist/` |

## CLI

```
clj-browse serve [--host 127.0.0.1] [--port 7878] [--open] [--token T] [--data-dir DIR]
clj-browse add <repo-dir>...     # register and index repos headlessly, then exit
clj-browse scan <dir>            # list git repos under dir (doesn't add them)
```

`java -jar target/clj-browse.jar …` runs the same commands. Add `--add-opens=java.base/java.nio=ALL-UNNAMED --add-opens=java.base/sun.nio.ch=ALL-UNNAMED --enable-native-access=ALL-UNNAMED --sun-misc-unsafe-memory-access=allow` to silence Datalevin's JVM warnings.

The server only listens on `127.0.0.1`. If you expose it with `--host 0.0.0.0`, pass `--token`. Anyone who can reach the server can run code through the REPL and bb tasks.

## Using it

- **Outline:** repo → project (`deps.edn` / `project.clj` / `shadow-cljs.edn`) → source paths → folders → files → top-level forms. Each form shows its kind, name, arglists and how many places use it, with the Markdown docstring underneath.
  - **Compact** puts each form on one line.
  - **Namespaces** lists files by namespace instead of by folder.
  - Folders with a single child are collapsed into one row.
- **Keyboard:**

  | Key | Action |
  |---|---|
  | ↑ ↓ | move |
  | → ← | expand / collapse |
  | Enter | edit docstring |
  | `r` | Run |
  | `c` | copy form |
  | `v` | paste |
  | `u` | usages |
  | `e` | open the file in your editor |
  | Ctrl+Enter | send to REPL |
  | Ctrl+K | search everything |

  Every key can be rebound in Settings.
- **Run (`r`)** opens a dialog with one syntax-highlighted input per parameter, a tab per arity and a list for variadic arguments. Inputs are evaluated in the function's namespace. Recent invocations are kept so you can re-run them. On a `def` the action is *inspect*, and on a macro it's *macroexpand*.
- **REPL panel:** connect to a running nREPL (it reads `.nrepl-port`) or start one for a project with the aliases or profiles you pick. cider-nrepl is injected. For shadow-cljs, choose a build to get a cljs session.
- **Copy / paste:** copy a form, select any file or form in any repo, and press `v`. The paste dialog offers the requires the form needs, flags alias conflicts, and warns when the source repo isn't a dependency of the target.
- **bb tasks:** a panel listing the repo's `bb.edn` tasks. You can run them with arguments, stream their colored output and stop them.
- **Settings** are stored in `~/.config/clj-browse/config.edn` (macOS: `~/Library/Application Support/clj-browse/`). You can edit the file directly; the app reloads it.

## Architecture

| Layer | Where |
|---|---|
| Schema (shared clj/cljs) | `src/cljc/clj_browse/schema.cljc`, documented in [docs/data-model.md](docs/data-model.md) |
| Git CLI (`cat-file --batch`, trees parsed in-process) | `src/clj/clj_browse/git.clj` |
| Project discovery (deps/lein/shadow, never evaluated) | `src/clj/clj_browse/projects.clj` |
| Analysis (clj-kondo as a library + rewrite-clj, batched) | `src/clj/clj_browse/analyze.clj` |
| Indexing (worktree snapshot, history window, idempotent) | `src/clj/clj_browse/index.clj`, `jobs.clj`, `watch.clj`, `gc.clj` |
| Cross-repo usage resolution | `src/clj/clj_browse/resolve.clj` and [ADR 0001](docs/decisions/0001-usage-resolution.md) |
| Source edits (rewrite-clj, conflict-checked) | `src/clj/clj_browse/edit.clj` |
| nREPL manager | `src/clj/clj_browse/repl.clj` |
| WebSocket API (transit) | `src/clj/clj_browse/server.clj`, `api/*.clj` |
| UI (re-frame, CodeMirror 6, Tailwind) | `src/cljs/clj_browse/ui/**` |
| Extension points (form actions, result viewers, panels, …) | `src/cljs/clj_browse/ui/registry.cljs` |
| Desktop shell | `src/cljs/clj_browse/electron/main.cljs`, `electron/` |
