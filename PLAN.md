# clj-browse — Plan

A Clojure code browser that spans multiple repos. It indexes namespaces, files, top-level forms, docstrings and usages into a single database, keeps a record for each commit, and presents everything in an outliner styled after Logseq, with REPL integration.

---

## Part 1 — Decisions (settled during design review)

### Runtime and deployment
- **Local, single-user.** The server listens on `127.0.0.1` by default. An optional token allows LAN or SSH access. There is no multi-tenancy.
- **The backend is JVM Clojure.** The frontend is ClojureScript with re-frame (Reagent, React 19). Code shared between them (schema, message formats, specs) lives in `.cljc`.
- **CLI first:** `clj-browse serve [--port] [--open]`. Later, an **Electron** shell (thin cljs) starts the server as a sidecar process using a JRE bundled with jlink.
- **Platforms:** Linux and macOS only.
- **Libraries:** babashka.fs for all filesystem work. nextjournal/beholder for file watching. http-kit, reitit, and transit over WebSocket for pushed updates (indexing progress, REPL and task output).
- **Paths:**

  | | Linux | macOS |
  |---|---|---|
  | Config | `~/.config/clj-browse/config.edn` | `~/Library/Application Support/clj-browse/config.edn` |
  | Data | `~/.local/share/clj-browse/` | `~/Library/Application Support/clj-browse/` |

  `XDG_CONFIG_HOME` and `XDG_DATA_HOME` are honored when set.

### Database
- **Datalevin**, embedded in the server. There is no database in the client: re-frame's app-db holds the slices the server sends.
- **Git-shaped model.** Commits point to trees, trees to blobs, and blobs to forms. Each git tree is stored once, keyed by its SHA, so unchanged directories are shared between commits. Each blob is parsed once.
- **Identity:** a var's stable identity is `:var/fqn`. Each version is a form entity keyed by `:form/hash`. The hash covers the normalized form (comments and whitespace removed) plus the calls it resolves to. A **docstring change counts as a new version**. Reformatting alone does not.
- **Positions** (row and col) belong to the link between a blob and a form, not to the form itself.
- **Every blob records its extractor version.** When the extractor improves, only blobs with an older version are re-analyzed.
- **Parse failures** are stored as facts (`:blob/error`). A file that fails to parse never stops indexing.
- **Settings:** user preferences go in `config.edn`, which the settings UI edits and the app reloads when the file changes. Per-repo state lives in the database: tracked branches, chosen aliases, run history, the full-history opt-in, and scan roots.

### Repos, projects and indexing
- **Adding repos:** repos are **never added automatically**. Scan roots are remembered. Re-scanning one lists the new repos it finds, and you choose which to add. You can also add a repo directly.
- **Commits and file changes are picked up automatically:**
  - A watcher on the working tree updates a per-repo **worktree snapshot**, a virtual commit whose blobs are hashed with `git hash-object`.
  - A watcher on `.git/HEAD` and the refs triggers indexing of new commits.
- **Tracked branches:** the default branch (`origin/HEAD`, falling back to `main` or `master`) plus the checked-out branch. You can pin more, including remote branches. Tags are recorded as refs.
- **The first scan is bounded:** `git rev-list --topo-order -n 100` on each tracked branch, configurable. The oldest commits in that window are stored as **boundary commits**, so the UI can say "history before X isn't indexed" instead of giving a wrong answer. History grows as new commits arrive. Full-history indexing is an opt-in per repo.
- **Git access goes through the `git` CLI:** one long-running `git cat-file --batch` per repo, plus `rev-list`, `ls-tree` and `for-each-ref`.
- **Indexing order:** worktree first, then the HEAD commit, then history in the background. Every step is idempotent and can resume after a crash.
- **Finding projects:** `deps.edn`, `project.clj` and `shadow-cljs.edn` are found anywhere in a repo. These directories are skipped: `node_modules`, `target`, `.git`, `.cpcache`, `.shadow-cljs`, `out`, `.clj-kondo`.
  - **deps.edn:** `:paths` plus every alias's `:extra-paths`, each tagged with its alias.
  - **project.clj:** read with edamame and **never evaluated**. Uses `:source-paths`, `:test-paths` and the same keys inside profiles (tagged with the profile), falling back to Leiningen's defaults.
  - **shadow-cljs.edn:** `:source-paths`.
  - **Overlap:** when two projects list the same directory, each file is indexed once and appears under both projects.
  - **Files shown:** only `.clj`, `.cljs`, `.cljc` and `.bb`. Resource paths are excluded.
- **Analysis:**
  - clj-kondo, used as a library, provides definitions, usages, arglists and docstrings. It respects the repo's `.clj-kondo` config, hooks and `:lint-as`.
  - rewrite-clj provides source positions and does all editing.
  - Results are cached per blob SHA.
  - `.cljc` files are analyzed once per language. Usages are tracked per language.
- **Removing a repo** deletes its commits, refs and worktree snapshot. Blobs, forms and vars that no other repo uses are then garbage-collected.
- **Libraries that aren't indexed** (`clojure.core`, jars, npm packages) become **stub** vars that carry only the name. When a REPL is connected, their docs come live from cider-nrepl's `info` op. Indexing jars is a possible later opt-in.

### Usage resolution (rules are recorded in an ADR so they can change later)
These rules live in one namespace (`clj-browse.resolve`) and are explained in `docs/decisions/0001-usage-resolution.md`:
1. Usages are matched by fully qualified name.
2. If exactly one indexed repo defines the name, the usage links to it.
3. If more than one does, the repo whose project depends on the other through `:local/root` or a git dependency wins. Otherwise every candidate is shown, tagged with its repo.
4. Usage badges and the Usages list reflect each repo's **current worktree snapshot**. Usages in past commits only appear in the History view.

### Outline UI
- **Repo navigation:** tabs or a sidebar for switching repos (a setting). One repo is shown at a time.
- **Tree:** project → source paths → directories → files → top-level forms.
  - Folders with a single child are collapsed (compact folders).
  - Each repo has a toggle to show namespaces instead of directories.
- **Every top-level form is shown**, including `ns`, side-effect calls and `comment` blocks. The inner forms of a `comment` block appear as children, each of which can be sent to the REPL.
- **Definitions that get names:** the def family, re-frame registrations (`reg-event-db`/`-fx`, `reg-sub`, `reg-fx`, `reg-cofx`, named by their keyword), and anything mapped with `:lint-as`.
- **Form row:** name, kind badge, arglists in grey, and the docstring below, rendered as Markdown and truncated to about 3 lines. A **compact view** toggle puts each form on one line with its docstring truncated inline.
- **`.cljc` forms:** shown once, with `clj` / `cljs` badges when they exist in only one language. Forms that differ by reader conditional get an expandable clj/cljs split.
- **Look and controls:**
  - Tailwind CSS, no component library. Light and dark themes, following the system setting.
  - Everything works with both keyboard and mouse, and keybindings can be changed: ↑↓ move, →← expand/collapse, Enter edit docstring, `r` Run, `c` Copy, `v` Paste, `u` Usages, `e` open the file externally, Ctrl+Enter send to REPL.
- **Command palette (Ctrl+K):** fuzzy search of var and namespace names across all repos, plus full-text search of docstrings using Datalevin's built-in search.

### Form actions
| Action | Applies to | Behavior |
|---|---|---|
| **Run** | `defn`, `defn-` (through `#'`), `defmulti` | Opens the Run dialog |
| **Eval / Inspect** | `def` | Evaluates and shows the value |
| **Macroexpand** | `defmacro` | |
| **Copy** | all forms | Copies to the system clipboard and to an internal clipboard that remembers the source namespace and the aliases the form uses |
| **Paste here** | on a file or a form | Inserts the form, offers to add missing requires to the target `ns`, flags alias conflicts, and warns when the source isn't a dependency of the target project |
| **Usages** | named definitions | Count badge and a list across repos (resolved as described above) |
| **Copy FQN / require** | named definitions | For example `[my.app.db :as db]` and `db/save!` |
| **Open in editor** | **whole file** | `$EDITOR`, `code -g`, `emacsclient`, configurable |

**Later:** History (list of versions, "introduced" and "last changed", with boundary caveats), Run tests, and Move (rewriting callers).

### Editing
- **Docstrings:**
  - Saving writes to disk immediately through rewrite-clj. Continuation lines get a 2-space indent.
  - The save is refused if the file's hash changed on disk since indexing.
  - Adding a docstring to a form without one puts it in the right place for that kind of form.
  - Undo is per-session.
- **Inline form editor:** CodeMirror 6 with nextjournal clojure-mode (highlighting and paredit), clj-kondo diagnostics, and completion and hover docs from cider-nrepl when a REPL is connected. Ctrl+S saves, with the same conflict check. Formatting and refactoring tools come after v1.

### REPL (one per repo, in its tab)
- **Connect** to an existing `.nrepl-port`, or **Start** one using the project and aliases or profiles chosen for the repo (remembered):
  - deps.edn: `clojure -Sdeps … -M:<aliases> -m nrepl.cmdline`
  - Leiningen: `lein repl :headless`
  - shadow-cljs: `npx shadow-cljs server`, then pick a build
- cider-nrepl middleware is injected.
- The REPL's output streams into its panel.
- REPLs the app started are stopped when it quits. REPLs it only connected to are left running.
- `.cljc` files get a clj/cljs session toggle.
- Code can be sent as one form (evaluated in its namespace), a form inside a `comment`, or a whole file (`load-file`).
- **Run modal:** the **Run** button (or `r`) opens a modal dialog over the outline. Its header shows the function's name, namespace and docstring. The body has **one labeled input element per parameter**, and Run / Cancel buttons sit at the bottom. Ctrl+Enter runs, Esc closes.
  - Each input is a small CodeMirror 6 editor with Clojure syntax highlighting, styled as a form field and labeled with the parameter's name, so values can be numbers, strings or whole data structures. Multi-arity functions get a tab per arity. Variadic `& more` gets a list of inputs you can add to. Destructured maps start with a template.
  - Inputs are **evaluated** in the function's namespace.
  - Results show as pretty-printed, collapsible EDN. Exceptions show their `ex-data`.
  - Each function keeps a history of invocations that can be re-run.

### bb tasks
When a repo has a `bb.edn`, a panel lists its tasks with their `:doc`, hiding names that start with `-`. Each task has an optional arguments field. Output streams with ANSI colors, several tasks can run at once, each has a Stop button, and the last run's output is kept.

### Extensions
v1 has an **internal registry of extension points**: form actions, result viewers, form renderers, outline decorations and panels. The app's own features register through it. Loading third-party extensions comes later, as cljs evaluated with SCI (frontend, plus optional backend code) with no compile step.

### This repo
- One repo: `deps.edn` for the backend, `shadow-cljs.edn` for the frontend and Electron shell, `bb.edn` for dev tasks.
- Source layout: `src/clj`, `src/cljs`, `src/cljc`.
- Tests: Kaocha, with fixture git repos generated by a test helper.
- **Dogfooding:** clj-browse must be able to index itself.

---

## Part 2 — Implementation phases

Each phase ends with something that runs and has tests. Phases 1–6 are backend only and are tested against fixture repos and the REPL. Phase 7 is the first time there's a UI to look at.

### Phase 0 — Scaffolding
- Run `git init` and set up `deps.edn`, `shadow-cljs.edn`, `bb.edn`, `package.json`, Tailwind, Kaocha, and the source directories.
- Config and data directories resolved per OS, plus loading and reloading of `config.edn`.
- Server skeleton: http-kit, reitit, a transit WebSocket channel, and serving the compiled frontend.
- CLI entry point `clj-browse serve --port --open`.
- A re-frame shell that connects over the WebSocket and renders a placeholder.
- bb tasks: `dev`, `test`, `build`.
- **Done when:** `bb dev` gives a live-reloading app in the browser, and `bb test` passes.

### Phase 1 — Schema and fixture repos
- Datalevin schema in `.cljc`: scan-root, repo, ref, commit (parents, tree, boundary flag), tree, tree-entry, blob (SHA, extractor version, error), blob→form link (positions, language), form (hash, kind, name, doc, arglists, calls), var (FQN, stub flag), project, source path (tags), worktree snapshot, run history.
- Write `docs/data-model.md`.
- A test helper that builds fixture repos: deps.edn, Leiningen, shadow-cljs, a nested monorepo, a merge commit, a file that doesn't parse, `.cljc` files with reader conditionals, and re-frame registrations.
- **Done when:** a transaction and query round-trip works, and the fixture repos build.

### Phase 2 — Discovering repos and projects
- Scanning a root to find candidate repos (directories containing `.git`). Adding and removing repos, and remembering scan roots.
- Finding project files with the skip list. Parsers for deps.edn, project.clj (edamame) and shadow-cljs.edn, producing source paths tagged by alias or profile, with defaults.
- **Done when:** every fixture repo produces the expected projects and paths, including overlapping paths.

### Phase 3 — Analyzing one blob
- clj-kondo as a library, run on a blob's text with the repo's config, hooks and `:lint-as`.
- rewrite-clj for positions and the children of `comment` blocks.
- Extracting every top-level form: def kinds, re-frame registrations, docstrings, arglists, and usages per language for `.cljc`.
- Normalized form hash. Parse errors recorded. Extractor version.
- **Done when:** golden tests pass for every fixture file.

### Phase 4 — Indexing git history
- Git CLI wrapper with a long-running `cat-file --batch` process.
- Choosing tracked branches. The `rev-list` window (default 100) with boundary commits.
- Storing trees keyed by SHA. Analyzing only blobs that haven't been seen.
- A background job queue (worktree first, then HEAD, then history) that is idempotent and resumable, with progress sent over the WebSocket.
- Full-history opt-in. A watcher on refs and `HEAD` that indexes new commits.
- **Done when:** indexing a fixture repo, interrupting it, and resuming gives the same database. Making a new commit in a fixture repo gets it indexed automatically. A clean run on clj-browse itself works.

### Phase 5 — Worktree snapshot and garbage collection
- Debounced beholder watcher per repo, using the same skip list, re-hashing and re-analyzing only the files that changed.
- Repo removal with garbage collection of unshared blobs, forms and vars.
- **Done when:** editing a fixture file updates the snapshot within about 1 s, and after a repo is removed nothing is left of it in the database.

### Phase 6 — Usage resolution and query API
- `clj-browse.resolve` implementing the resolution rules, plus ADR `docs/decisions/0001-usage-resolution.md`.
- Stub vars.
- Query API over the WebSocket: repo outline, form details, usages, search (fuzzy plus Datalevin full-text).
- **Done when:** tests pass for one definition, the dependency tie-break, ambiguous candidates and stubs, and every endpoint is tested.

### Phase 7 — Read-only outliner
- The extension-point registry comes first, and all actions below register through it.
- Tabs or sidebar for repos. The project → path → directory → file → form tree, with compact folders and the namespace-view toggle.
- Form rows: kind badge, arglists, Markdown docstring truncated, clj/cljs badges, and the reader-conditional split.
- Compact view toggle, light and dark themes, keyboard and mouse navigation.
- Usages badge and panel. Ctrl+K palette. Indexing progress indicator. A "history not indexed before X" notice.
- **Done when:** clj-browse and two other repos can be browsed, and searching jumps to a form.

### Phase 8 — Editing
- Inline docstring editor: rewrite-clj write, disk-conflict check, insertion rules per form kind, undo.
- Inline CodeMirror 6 form editor with clj-kondo diagnostics and saving.
- Open the file in an external editor.
- **Done when:** edits go to disk and preserve formatting (golden tests), a conflicting edit is refused, and the worktree watcher re-indexes the change.

### Phase 9 — Copy and paste
- The internal clipboard (source namespace, aliases used).
- "Paste here" on a file or form, adding missing requires, flagging alias conflicts, and warning about missing cross-repo dependencies.
- Copy FQN / require snippet.
- **Done when:** a form copied between namespaces in the same repo, and between repos, gets the right requires added.

### Phase 10 — nREPL
- Connecting to `.nrepl-port`. Starting a REPL for deps.edn, Leiningen and shadow-cljs with chosen aliases (remembered) and cider-nrepl injected. Streaming output. Lifecycle handling. The clj/cljs toggle.
- Sending a form, a form inside a `comment`, or a whole file. Eval/Inspect for `def`, macroexpand for macros.
- Completion and hover docs in the editor. Live docs for stub vars.
- **Done when:** each project type can start a REPL and evaluate code against the fixture repos.

### Phase 11 — Run modal
- The Run button (or `r`) opens a **modal** with the function's name, namespace and docstring at the top, **one labeled, syntax-highlighted input element per parameter**, and Run / Cancel buttons. Ctrl+Enter runs, Esc closes.
- Parsing arglists into input models: arity tabs, variadic lists, templates for destructured parameters.
- CodeMirror inputs evaluated in the function's namespace. The result appears inside the modal, below the inputs. Result viewer, registered as the default result viewer extension. Exceptions shown with `ex-data`.
- Saved invocation history that can be re-run.
- **Done when:** functions with multiple arities, variadic parameters and destructured parameters all run correctly.

### Phase 12 — bb tasks panel
- Listing tasks with `:doc` and hiding private ones. Running with arguments, streamed ANSI output, several at once, Stop, and keeping the last output.

### Phase 13 — Settings
- Settings UI that writes `config.edn` (theme, tabs or sidebar, compact default, keybindings, editor command, commit window, nREPL middleware).
- Per-repo settings: tracked branches, full-history opt-in, aliases.

### Phase 14 — Electron and packaging
- Thin cljs Electron main process: start the JVM sidecar with a jlink JRE, wait until it's healthy, open a window, add native menus, a folder picker for scan roots and repos, and a tray icon. Shut down cleanly.
- Packages for Linux (AppImage and .deb) and macOS (.dmg). Notarization is deferred.

### After v1 (recorded, not scheduled)
- History view for each form (versions, "introduced" and "last changed").
- Form-level diffs between commits. Browsing the outline at a given commit.
- Run tests for a function. Move with caller rewriting. cljfmt and refactoring tools.
- Loading third-party extensions with SCI (Reagent component previewer, table viewer for a vector of maps).
- Opt-in indexing of dependency jars. A DataScript mirror in the client if extensions need to query locally.
