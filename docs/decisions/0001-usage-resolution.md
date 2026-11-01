# ADR 0001: How usages are matched to definitions across repos

- **Status:** Accepted (v1). Expected to change; keep this file and `clj-browse.resolve` in sync.
- **Code:** `src/clj/clj_browse/resolve.clj` is the only place these rules are implemented. Everything else (usage badges, the Usages panel, jump-to-definition) calls `resolve-fqn` or `usages-of`.

## Context

clj-browse indexes many repos into one database. A usage is stored as a reference to a var by its fully-qualified name (`:var/fqn`, for example `"my.lib/save!"` or `":user/login"` for re-frame ids). Repos can share namespaces: a library and your fork of it, two checkouts of the same project, or a vendored copy. When repo B calls `lib.core/f`, we need to decide which repo's `lib.core/f` it means.

## Decision

When repo `using` refers to fqn F, the candidates are the repos whose **current worktree snapshot** defines F.

| # | Condition | Result (`:status`) |
|---|---|---|
| 0 | `using` defines F itself | its own definition (`:own`) |
| 1 | exactly one repo defines F | that repo (`:unique`) |
| 2 | several define F, and `using` depends on some of them | those repos (`:dependency`) |
| 3 | otherwise | all defining repos, shown as candidates tagged with their repo (`:ambiguous`) |
| – | nobody defines F | `:unresolved`: a stub var (library, `clojure.core`, …) |

Rule 0 is an addition to the rules agreed during design. Without it, a repo's own calls to its own functions would count as ambiguous whenever a fork is also indexed.

"Depends on" (rule 2) comes from project files:
- a `:local/root` in any `deps.edn` of `using` that equals, or lies inside, the other repo's path
- a `:git/url`, or an inferred `io.github.x/y` lib, in `using`'s deps that matches the other repo's `origin` URL after normalization (`git@host:a/b.git` and `https://host/a/b` are treated as the same)

Usages in historical commits are not resolved. They only appear in the History view.

## Consequences

- The rules are cheap to evaluate. `dependency-graph` is computed per query, and `resolve-fqn` is memoized per using repo.
- Leiningen `:checkouts` and Maven coordinates are not used for rule 2. A lein project depending on an indexed repo through a jar falls through to rule 1 or rule 3.
- Usages of `clojure.core` and `cljs.core` are not recorded at all (`clj-browse.analyze/ignored-usage-nses`).

## Changing this

Edit `resolve-fqn` and update this table. Tests live in `test/clj/clj_browse/resolve_test.clj`.
