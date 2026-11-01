# Data model

The schema is defined in `src/cljc/clj_browse/schema.cljc`. Everything lives in one embedded Datalevin database, at `~/.local/share/clj-browse/db` on Linux and `~/Library/Application Support/clj-browse/db` on macOS.

The model follows git's object model, so content is stored once no matter how many commits, branches or repos share it:

```
repo ──< ref ──> commit ──> tree ──< entry ──> blob ──< occurrence ──> form ──< def   ──> var
 │                  │                                                      ├──< usage ──> var
 │                  └──> parents (commits)                                 └──< child (comment-block forms)
 ├──< file (worktree snapshot) ──> blob
 └──< project (deps.edn / project.clj / shadow-cljs.edn) — paths with alias/profile tags
```

| Entity | Identity | Notes |
|---|---|---|
| `repo` | `:repo/path` (absolute) | Per-repo settings: commit window, full-history opt-in, pinned branches, REPL config |
| `ref` | `:ref/key` = `"<repo>\|refs/heads/x"` | `:ref/tracked?` is true for the default branch, the checked-out branch and pinned refs |
| `commit` | `:commit/sha` | Shared between repos (`:commit/repos`). `:commit/boundary?` marks a commit whose parents are outside the indexed window |
| `tree` | `:tree/sha` | Only Clojure files and the directories leading to them are stored. Skipped directories (`node_modules`, `target`, hidden directories, …) are left out |
| `blob` | `:blob/key` = `"<sha>.<ext>@<kondo-config-hash>"` | An *analysis* of a git blob. Extension and clj-kondo config are part of the key because both change the analysis result |
| `occurrence` | component of blob | Position of a form within one file version (row/col/end) |
| `form` | `:form/hash` | One version of a top-level form. The hash covers ns + kind + whitespace/comment-insensitive tokens + resolved usages, so a docstring edit makes a new version and reformatting doesn't |
| `def` | component of form | One definition inside a form: usually one, two for a `#?(:clj … :cljs …)` split, several for `defprotocol` |
| `usage` | component of form | A reference to a var, with a row offset relative to the form |
| `var` | `:var/fqn` | Stable identity: `"ns/name"`, or `":ns/kw"` for re-frame registrations. A var that no worktree defines is a *stub* (a library or a missing definition) |
| `file` | `:file/key` = `"<repo>\|<path>"` | The **worktree snapshot**: what's on disk now. The outline, usages and search all read from this |
| `run` | — | Invocation history for the Run modal |

## Freshness

- **Worktree:** beholder watches every source path. Changed files are re-hashed with the `git hash-object` algorithm, computed in-process, and only blobs that haven't been seen are analyzed (`clj-browse.watch` → `clj-browse.jobs` → `clj-browse.index/index-worktree!`).
- **History:** a 2-second poller watches `HEAD`, refs, `packed-refs`, project files and the clj-kondo config. `index-history!` walks `git log --topo-order -n <window>` for each tracked ref, reads trees through one long-running `git cat-file --batch` process, and analyzes new blobs in batched clj-kondo runs.
- **Re-analysis:** bumping `schema/extractor-version` makes every blob with an older version re-analyze the next time it's seen.
- **Removing a repo:** `clj-browse.gc/remove-repo!` deletes the repo's own entities, then mark-and-sweeps trees, blobs, forms and vars that are no longer referenced.

## Usage resolution

See [decisions/0001-usage-resolution.md](decisions/0001-usage-resolution.md).
