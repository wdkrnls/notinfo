# notinfo

Indexed full-text search over the Info manuals on your system.

Info manuals are already structured: each manual holds a sequence of nodes, and each node has a name, a parent and a place in reading order. 

The indexing system heavily leverages the `info` shell command. It asks it for a list of manuals, and then for each manual it asks `info --subnodes` for it's whole text, splits that by `\x1f` node separators, and indexes one document per node — storing that node's rendered text, so it can be viewed with the `view` subcommand. `notinfo search` commands with `--point` options give character position numbers relative to the beginning of the node buffer.

## Requirements

* Guile 3.0 and guile-xapian
* texinfo (`info`), coreutils (`cksum`), findutils (`find`, `xargs`), gawk

## Installing

With Guix, from the notman channel:

    guix install notinfo

Without Guix, from a checkout:

    make install

Note that this will put the default database inside of:

```
~/.config/
```

## Usage

    notinfo index  [--rebuild] [--lang=LANG] [--manual=NAME[,NAME...]]
    notinfo search [--context[=N]] [--point] [--debug] QUERY...
    notinfo render MANUAL
    notinfo view '(MANUAL)NODE'

`--db-path=DIR` on any subcommand puts the database elsewhere.

## Indexing

`notinfo index` is incremental: a manual is re-indexed only if its contents
size or cksum changed.  `--rebuild` discards the database first and starts from scratch.

`--manual=emacs,elisp` restricts the indexing run to just those manuals.  This is a complete description of what you want in the database, so manuals outside the list are purged as stale — `notinfo index --rebuild --manual=emacs` leaves an emacs-only database, while `notinfo index --manual=emacs` updates changes only for that manual.

## Query syntax

Three fields, combinable with `AND`, `OR`, parentheses and quotes:

* `manual:tar` — the file's name, minus `.info`, `.gz` and any language
  suffix
* `node:changes` — this node's name, or any ancestor nodes name
* `title:` — an alias for `node:`
* `chapter:12` — the node's section number, or any decendant of it
* `section:12` — an alias for `chapter:`
* `menu:` — the text of a node's menu sections
* `body:globbing` — this node's own text

Bare words search a whole manual, including node names, their body text, and menu text.

    notinfo search manual:tar AND node:changes AND body:globbing

Because a node title prefix carries its ancestors' names and numbers, `node:` reaches down the tree: `node:regexp` searches through every node which has an ancestor node with Regexp as a substring of it's name, even when that node's own title says nothing about regexps.

## Reading a node

    notinfo view '(tar)Changes'

prints the node's stored text in the Xapian exactly as indexed.  Position 0 is the first column of the node's heading line. So in Emacs, with that text in a buffer and point on the beginning of the header line:

    (forward-char POINT)

with POINT standing in for the match reported by `--point`.  `notinfo render tar` prints the whole manual as `info --subnodes` produces it, which is what was indexed.

## Output

    (tar)Changes: ... matching text ...

With `--point`, the offset within the node instead of the context:

    (tar)Changes: 388

## Where the database lives

`$XDG_DATA_HOME/notinfo` (that is, `~/.local/share/notinfo`), overridable
with `--db-path=DIR` on indexing or `$NOTINFO_DB_DIR`.

## Notes on the design

Text comes from `info --subnodes --output=-`, which resolves a manual's split
files (`foo.info` plus `foo.info-1`, `foo.info-2`, …), decompresses, walks
the menu tree in reading order, and emits an `Up:` field for every node.  Its
output does not depend on the terminal width — `COLUMNS=100 info tar
--subnodes --output=- | cksum` is unchanged — so offsets into it are stable
and are stored once rather than recomputed.

Node boundaries are `\x1f`, and a node's name is in the `Node:` field of the
header line that follows.  A field's value runs until the next `, Known:`
field rather than the next comma, because node names may themselves contain
`, `.

Each node stores its text, so the database is roughly the size of the manuals
it indexes.  That buys a `view` and a `--point` that need no `info` run.

`body:` excludes the header line and any text inside `* Menu:` blocks, since menus are for navigation and their text only repeats node titles.  Menu stripping is conservative — a wrapped menu description is not `* `-prefixed and so ends the block — because indexing a stray line is better than eating body text.
