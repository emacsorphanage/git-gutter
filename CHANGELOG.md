# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

Releases up to 0.90 are described in [Changes](Changes). Releases 0.91 to
0.93 have no entries; see the git history.

## [Unreleased]

### Fixed

- Live update ignored `git-gutter:start-revision`: it compared the buffer
  with the index (git), the parent of the working copy (jj) or the base
  revision (svn, hg, bzr), and replaced the signs of the full update,
  which compares with the start revision. After
  `git-gutter:set-start-revision`, the signs for that revision showed
  until the next live update. Live update now compares with the start
  revision when it is set.
- `git-gutter:set-start-revision` did not show the new revision's signs
  when a diff was running: `git-gutter` starts no new diff while one
  runs, so the signs stayed those of the old revision until the next
  update. It now kills the running diff first.

## [0.94.4] - 2026-09-25

### Fixed

- A new value of `git-gutter:update-interval` did not take effect: the
  timer kept the interval it was started with, and setting the variable
  to nil did not stop live updates. The timer now restarts when the
  variable is set, also with `setq`.
- A new value of `git-gutter:update-hooks` did not apply to buffers that
  already had `git-gutter-mode` on, and turning the mode off in such a
  buffer left `git-gutter` in the hooks that had been removed from the
  list, so it still drew signs. The mode now installs the new hooks when
  the variable is set, and turning it off removes the hooks it added.
- Deleting whole lines that had signs, for example empty added lines
  removed with backspace, moved their signs onto the next line, where
  they showed on top of each other until the next update. A sign
  overlay now spans its line and its newline, and deleting the line
  deletes the overlay.
- On Emacs 27, with `git-gutter:visual-line` on, drawing the signs took
  time proportional to the number of signed lines times the number of
  overlays in the buffer: a full update of 20000 lines took ten times as
  long as with the option off.

## [0.94.3] - 2026-09-24

### Changed

- Live update writes the buffer to one temporary file per buffer,
  created at the buffer's first live update, instead of a new file for
  each update. It no longer starts a `diff` while the buffer's previous
  one is still running; it runs again when that one finishes.

### Fixed

- A new major mode, for example after `revert-buffer` or `normal-mode`,
  left live update's temporary files: the new mode removes all local
  variables, and with them the names of these files.
- Live update in a buffer stopped the live update of another buffer
  whose file has the same name in another directory, so that buffer's
  signs were not updated until its next live update.
- Killing an indirect buffer made with `clone-indirect-buffer` deleted
  the base buffer's live update files, whose names the clone had copied,
  and the base buffer's live updates failed until its next full update.
- Killing an indirect buffer stopped a running full update of its base
  buffer, whose signs then stayed as they were until the next one.
- Edits made in an indirect buffer could leave wrong signs in its base
  buffer when the advice on `make-indirect-buffer` did not run, for
  example when natively compiled code called it: the base buffer did not
  know it had an indirect buffer, and kept signs of lines that the edits
  had changed.
- When an edit joined lines that had signs, for example an undo of
  inserted newlines, the sign of each joined line moved into the joined
  line and showed as an extra sign on its screen row, until the next
  update. An edit now deletes the signs that no longer start a line.

## [0.94.2] - 2026-09-23

### Fixed

- A full update that finished after a later live update, for example
  when `git diff` is slow after a save, replaced the live update's signs
  with those of the saved file. The result of an update is now shown
  only if no other update has started since.
- A live update that started while the previous one was still running
  stopped it, and the stopped one left its copy of the buffer in the
  temporary directory. It now deletes that copy.
- Exiting Emacs left live update's temporary files: the original
  version of each buffer and the copy of a buffer whose `diff` was
  still running. `kill-emacs-hook` now deletes them.

## [0.94.1] - 2026-09-23

### Fixed

- `git-gutter:statistic` counted one line too few for each added hunk
  and no lines for deleted hunks. It now counts the lines that
  `git diff --numstat` counts.
- The signs flickered on every update, for example while typing with
  `git-gutter:update-interval` set, because each update removed all
  signs and drew them again. An update now changes only the signs that
  changed, and skips the hunks in which no text was edited, when
  `git-gutter:view-diff-function` and `git-gutter:clear-function` have
  their default values.
- In a narrowed buffer, live update compared only the visible part with
  the original version, so the lines outside it showed as deleted and
  the signs were on the wrong lines. It now writes the whole buffer.
- Live update wrote the buffer in another coding system when the buffer
  had a character that `buffer-file-coding-system` cannot encode, so
  every line with a non-ASCII character showed as changed.
- Byte-compilation warning: `define-global-minor-mode`, obsolete since
  Emacs 31.1.

## [0.94.0] - 2026-09-23

### Added

- `git-gutter:popup-hunk-inline-at-point`, which shows the current hunk
  above the current line until the next key press
  ([#230](https://github.com/emacsorphanage/git-gutter/pull/230)).
- The signs are updated when a window showing the buffer is selected,
  for example with `other-window` (`window-selection-change-functions`).
- `git-gutter:staged-sign` and the face `git-gutter:staged` mark lines
  whose changes are staged, for git
  ([#241](https://github.com/emacsorphanage/git-gutter/pull/241)). Off by
  default.
- Support for [Jujutsu](https://github.com/jj-vcs/jj) (`jj`), with the
  user option `git-gutter:jj-diff-option`. Add `jj` to
  `git-gutter:handled-backends` to use it
  ([#244](https://github.com/emacsorphanage/git-gutter/pull/244)).
- README: theming the gutter column background
  ([#247](https://github.com/emacsorphanage/git-gutter/pull/247)).

### Changed

- The minimum Emacs version is 27.1; it was 25.1. Since 0.92
  ([#232](https://github.com/emacsorphanage/git-gutter/pull/232)),
  git-gutter calls `executable-find` with a second argument, REMOTE,
  which Emacs 27.1 added. On Emacs 26 every git call signals
  `wrong-number-of-arguments`, so git-gutter has not worked there since
  0.92. Declaring 27.1 makes package.el refuse to install it on Emacs 26,
  instead of installing a version that fails.
- The signs are updated when a window starts showing the buffer
  (`window-buffer-change-functions`), instead of through pre- and
  post-command hooks and advice on `switch-to-buffer` and `quit-window`
  ([#235](https://github.com/emacsorphanage/git-gutter/pull/235)).
- With `git-gutter:visual-line`, the signs are drawn on the continuation
  rows of a wrapped line with a `wrap-prefix`, instead of one overlay per
  visual row ([#248](https://github.com/emacsorphanage/git-gutter/pull/248)).
- Live update (`git-gutter:update-interval` above 0) reads the original
  version of the file once, and again after each full update, instead of
  on every update; Emacs waits 4.4 ms per live update on a 2000-line
  file instead of 24.6 ms. `git-gutter:update-interval` accepts
  fractions of a second, such as 0.1, and `nil`, its new default, for no
  live updates; 0 still means no live updates.

### Deprecated

- `git-gutter:linum-setup` does nothing except show a warning. Use
  `display-line-numbers-mode`.

### Removed

- Support for `linum-mode`. Emacs 26.1 added the built-in
  `display-line-numbers-mode` and announced that `linum-mode` would
  become obsolete; Emacs 29.1 made linum.el obsolete. Every Emacs version
  git-gutter supports has `display-line-numbers-mode`.
- `git-gutter:update-commands` and `git-gutter:update-windows-commands`,
  which listed the commands after which the signs were updated
  ([#235](https://github.com/emacsorphanage/git-gutter/pull/235)).
  Remove them from your configuration: `add-to-list` on either one now
  signals `void-variable`.
- `git-gutter:next-visual-line`
  ([#248](https://github.com/emacsorphanage/git-gutter/pull/248)).

### Fixed

- With `git-gutter:visual-line`, a sign now appears on every row of a
  wrapped line, a sign of a changed line is drawn over the separator, and
  the sign of a deleted hunk appears only on the first row of the line
  above the deletion
  ([#248](https://github.com/emacsorphanage/git-gutter/pull/248)).
- `git-gutter:stage-hunk` failed on Windows when `core.autocrlf` is
  `true`, the default of Git for Windows: the patch was written with CRLF
  line endings, and `git apply` rejected it
  ([#210](https://github.com/emacsorphanage/git-gutter/pull/210)).
- Files whose encoding differs from the Emacs default: diffs, temporary
  files, staging patches and the popup buffer now use the buffer's
  `buffer-file-coding-system`
  ([#210](https://github.com/emacsorphanage/git-gutter/pull/210)).
- `git-gutter:stage-hunk` staged nothing when the repository is a bare
  repository used through `GIT_DIR` and `GIT_WORK_TREE`
  ([#250](https://github.com/emacsorphanage/git-gutter/pull/250)).
- Killing an indirect buffer whose base buffer no longer exists, or a
  clone of an indirect buffer, signalled `wrong-type-argument`
  ([#240](https://github.com/emacsorphanage/git-gutter/pull/240)).
- With `git-gutter:unchanged-sign` or `git-gutter:separator-sign` set,
  updating the signs took time proportional to the square of the number
  of lines: 1.5 s for a 20000-line file with one hunk, 20.4 s with 2000
  hunks. It now takes 0.016 s and 0.028 s
  ([#243](https://github.com/emacsorphanage/git-gutter/pull/243)).
- Live update showed no changes when the file's directory was reached
  through a symbolic link, for example under `/tmp` or `/var` on macOS.
- Live update removed all signs when `diff` failed.
- Byte-compilation warnings: missing `lexical-binding` cookie, and
  `when-let`, obsolete since Emacs 31.1
  ([#236](https://github.com/emacsorphanage/git-gutter/pull/236)).

[Unreleased]: https://github.com/alberti42/fork-git-gutter/compare/0.94.4...HEAD
[0.94.4]: https://github.com/alberti42/fork-git-gutter/compare/0.94.3...0.94.4
[0.94.3]: https://github.com/alberti42/fork-git-gutter/compare/0.94.2...0.94.3
[0.94.2]: https://github.com/alberti42/fork-git-gutter/compare/0.94.1...0.94.2
[0.94.1]: https://github.com/alberti42/fork-git-gutter/compare/0.94.0...0.94.1
[0.94.0]: https://github.com/alberti42/fork-git-gutter/compare/0.93...0.94.0
