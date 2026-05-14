# git-gutter.el

[![melpa badge][melpa-badge]][melpa-link]
[![melpa stable badge][melpa-stable-badge]][melpa-stable-link]
[![gh actions badge][gh-actions-badge]][gh-actions-link]

## Introduction

`git-gutter.el` is an Emacs port of the Sublime Text plugin
[GitGutter](https://github.com/jisaacks/GitGutter).


## Features

- Asynchronous updating
- [Live updating](#live-updating)
- [Support multiple VCS](#backends)
- Support Tramp
- Work without `vc-mode`


## Screenshot

![Screenshot of git-gutter.el](image/git-gutter1.png)


## Requirements

* Emacs 25.1 or higher
* [Git](http://git-scm.com/)(1.7.0 or higher)


## git-gutter.el vs [git-gutter-fringe.el](https://github.com/syohex/emacs-git-gutter-fringe)

|                      | git-gutter.el     | git-gutter-fringe.el |
|:---------------------|:-----------------:|:--------------------:|
| Work in tty frame    | OK                | unsupported          |
| Work with linum-mode | OK(experimental)  | OK                   |
| Show on right side   | unsupported       | OK                   |
| configurable         | more configurable | less configurable    |


## Installation

You can install `git-gutter.el` from [MELPA](http://melpa.org) with package.el
(`M-x package-install git-gutter`), with [el-get](https://github.com/dimitri/el-get),
or with another package manager of your choice


## Global Minor Mode and Minor Mode

`git-gutter.el` provides a global minor-mode(`global-git-gutter-mode`)
and minor-mode(`git-gutter-mode`).

If you want to use `git-gutter` for files in git repository.  You add
following s-exp in your configuration file(`~/.emacs.d/init.el` or
`~/.emacs`).

```lisp
(global-git-gutter-mode +1)
```

Other case, you want to use `git-gutter` for some files, you can use `git-gutter-mode`.
Following example of enabling `git-gutter` for some mode.

```lisp
(add-hook 'ruby-mode-hook 'git-gutter-mode)
(add-hook 'python-mode-hook 'git-gutter-mode)
```

## Commands

`git-gutter.el` provides following commands.
**Obsoleted interfaces will be removed when 1.0 released.**

#### `git-gutter:next-hunk`

Jump to next hunk

#### `git-gutter:previous-hunk`

Jump to previous hunk

#### `git-gutter:end-of-hunk`

Move to end of current hunk

#### `git-gutter:mark-hunk`

Mark current hunk.

#### `git-gutter:set-start-revision`

Set the start revision from which git-gutter performs the diffs.

You can also set the variable `git-gutter:start-revision` as a
directory-local variable.

#### `git-gutter:popup-hunk`

Popup current diff hunk(alias `git-gutter:popup-diff`)

`git-gutter:next-hunk` and `git-gutter:previous-hunk` update content
of buffer popuped by `git-gutter:popup-diff` to current hunk.

#### `git-gutter:stage-hunk`

Stage current hunk. You can use this command like `git add -p`.
This command is supported only for `git`.

#### `git-gutter:revert-hunk`

Revert current hunk

#### `git-gutter`

Show changes from last commit or Update change information.
Please execute this command if diff information is not be updated.

#### `git-gutter:linum-setup`

Setup for working with `linum-mode`.

#### `git-gutter:update-all-windows`

Update git-gutter information of buffers in all visible window.


## Sample Configuration

```lisp
(require 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)

;; If you enable git-gutter-mode for some modes
(add-hook 'ruby-mode-hook 'git-gutter-mode)

(global-set-key (kbd "C-x C-g") 'git-gutter)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

;; Mark current hunk
(global-set-key (kbd "C-x v SPC") #'git-gutter:mark-hunk)
```

## Directory-local variables

### Set starting revision for diffs

Using directory-local variables, you can set the start revision for diffs
for any file in the current directory:

``` lisp
;;; .dir-locals.el
((prog-mode . ((git-gutter:start-revision . "my-branch"))))
```

## Customize

### Live updating

If you set `git-gutter:update-interval` seconds larger than 0, `git-gutter` updates
diff information in real-time by idle timer.

```lisp
(custom-set-variables
 '(git-gutter:update-interval 2))
```

You can stop timer by `git-gutter:cancel-update-timer` and starts by `git-gutter:start-update-timer`.

### Look and feel

![Screenshot of multiple characters in gutter](image/git-gutter-multichar.png)

You can change the signs and those faces.

```lisp
(custom-set-variables
 '(git-gutter:modified-sign "  ") ;; two space
 '(git-gutter:added-sign "++")    ;; multiple character is OK
 '(git-gutter:deleted-sign "--"))

(set-face-background 'git-gutter:modified "purple") ;; background color
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")
```

You can change minor-mode name in mode-line to set `git-gutter:lighter`.
Default is " GitGutter"

```lisp
;; first character should be a space
(custom-set-variables
 '(git-gutter:lighter " GG"))
```


### Using full width characters

![Screenshot of using full-width character as diff sign](image/git-gutter-fullwidth.png)

Emacs has `char-width` function which returns character width.
`git-gutter.el` uses it for calculating character length of the signs.
But `char-width` does not work for some full-width characters.
So you should explicitly specify window width, if you use full-width
character.

```lisp
(custom-set-variables
 '(git-gutter:window-width 2)
 '(git-gutter:modified-sign "☁")
 '(git-gutter:added-sign "☀")
 '(git-gutter:deleted-sign "☂"))
```

### Backends

`git-gutter.el` supports following version control systems

- [Git](http://git-scm.com/)(1.7.0 or higher)
- [Mercurial](https://www.mercurial-scm.org/)
- [Subversion](https://subversion.apache.org/)(1.8 or higher)
- [Bazaar](http://bazaar.canonical.com/en/)


You can set backends which `git-gutter.el` will be used.
Default value of `git-gutter:handled-backends` is `'(git)`. If you want to use
`git-gutter.el` for other VCS, please change value of `git-gutter:handled-backends` as below.

```lisp
;; Use for 'Git'(`git`), 'Mercurial'(`hg`), 'Bazaar'(`bzr`), and 'Subversion'(`svn`) projects
(custom-set-variables
 '(git-gutter:handled-backends '(git hg bzr svn)))
```

### Updates hooks

diff information is updated at hooks in `git-gutter:update-hooks`.

```lisp
(add-to-list 'git-gutter:update-hooks 'focus-in-hook)
```

### Updates commands

diff information is updated after command in `git-gutter:update-commands` executed.

```lisp
(add-to-list 'git-gutter:update-commands 'other-window)
```

### Disabled modes

If you use `global-git-gutter-mode`, you may want some modes to disable
`git-gutter-mode`. You can make it by setting `git-gutter:disabled-modes`
to `non-nil`.

```lisp
;; inactivate git-gutter-mode in asm-mode and image-mode
(custom-set-variables
 '(git-gutter:disabled-modes '(asm-mode image-mode)))
```

Default is `nil`.

### Show signs at gutter by visual lines

Emacs folds long line if `truncate-lines` is `nil`. If `git-gutter:visual-line` is
non-nil, `git-gutter` puts sign by visual lines.

```lisp
(custom-set-variables
 '(git-gutter:visual-line t))
```

Default bahavior is that signs are put by logical lines.
value of `git-gutter:visual-line` is `nil`.

### Show Unchanged Information

![Screenshot of highlighting unchanged lines](image/git-gutter-unchanged.png)

`git-gutter.el` can view unchanged information by setting `git-gutter:unchanged-sign`.
Like following.

```lisp
(custom-set-variables
 '(git-gutter:unchanged-sign " "))
(set-face-background 'git-gutter:unchanged "yellow")
```

Default value of `git-gutter:unchanged-sign` is `nil`.

### Show a separator column

![Screenshot of showing separator between buffer and gutter](image/git-gutter-separator.png)

`git-gutter.el` can display an additional separator character at the right of the changed
signs. This is mostly useful when running emacs in a console.

```lisp
(custom-set-variables
 '(git-gutter:separator-sign "|"))
(set-face-foreground 'git-gutter:separator "yellow")
```

Default value of `git-gutter:separator-sign` is `nil`.

Please set `git-gutter:always-show-separator` to non-nil, if you want to show
separator always.

### Theming the gutter column background

Many modern themes — for example the built-in `modus-operandi` and
`modus-vivendi` (see https://github.com/protesilaos/modus-themes) — give the
line-number column a background color that differs from the main buffer
background.  git-gutter places its change indicators in that same left-margin
area, so when a theme colors the line numbers, the gutter should share that
background for the column to look uniform.

git-gutter uses a dedicated face for each type of change: `git-gutter:added`
for added lines, `git-gutter:modified` for modified lines, and
`git-gutter:deleted` for deleted lines.  On those rows, the sign character
configured under [Look and feel](#look-and-feel) (e.g. `git-gutter:added-sign`)
is drawn in the foreground color of the corresponding face, against that face's
background color.

How the *remaining* rows — those with no changes — get their background
depends on your Emacs version.

#### Emacs 31.1 and later: use the built-in `margin` face

Emacs 31.1 introduced a new basic face called `margin` (see
[bug #80693](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=80693)).  The
display engine fills every empty cell in the left and right window margins
with this face's background.  Content written into the margin (such as a
git-gutter sign) is drawn with its own face, but where that face does not
specify a background of its own, it inherits from the `margin` face. So,
configuring `margin` once yields a visually uniform gutter column background,
without any per-package further customization.

Note that the line-number area is a separate display column with its own
face (`line-number`); the `margin` face does not affect it.  Most users will
want the two backgrounds to match visually, when the line numbers are displayed.
This is something themes can arrange and the example below does explicitly.
The `margin` face was added in May 2026, so it will take time before third-party
themes ship explicit support — until then, customizing it yourself is the way to go.

To match the line-number column without hardcoding a color:

```lisp
(defun my/margin-update-face ()
  (let ((bg (face-background 'line-number nil t)))
    (when bg
      (set-face-background 'margin bg))))

(add-hook 'enable-theme-functions (lambda (&rest _) (my/margin-update-face)))
(my-margin-update-face)
```

With this in place, you do **not** need to set `git-gutter:unchanged-sign` or
`git-gutter:always-show-separator` just to fill the gutter background — and
on large files you should actively *avoid* them for that purpose, because
each requires git-gutter to install an overlay on every line of the buffer.
That cost becomes prohibitive on files with tens of thousands of lines (e.g.
Emacs' own `xdisp.c`); the `margin` face approach moves the work to the C
display loop, where it costs nothing per line.

If you want a visual gap between the change indicator and the buffer text,
the right approach on 31.1+ is to widen the gutter — for example
`(setq git-gutter:window-width 2)` — and let the `margin` face fill the
extra column for free.  `git-gutter:unchanged-sign` and
`git-gutter:separator-sign` retain their original meaning only when you
actually want a visible character drawn on every unchanged line.

#### Emacs 30.x and earlier: fill via git-gutter's own faces

Before the `margin` face existed, an unwritten margin cell fell back to the
default frame or terminal background.  git-gutter had no way to influence
cells it did not render into, so a uniform gutter background required filling
those cells via git-gutter itself, using `git-gutter:unchanged` (when
`git-gutter:unchanged-sign` is set) or `git-gutter:separator` (when
`git-gutter:always-show-separator` is non-nil).

**Important gotcha:** `git-gutter:window-width` reserves space for the gutter
column in every buffer, but on unchanged rows nothing is written into that space
unless one of the two signs above is configured.  A cell that is reserved but
never written falls back to the default frame or terminal background — so
setting a background color on `git-gutter:unchanged` or `git-gutter:separator`
has no visible effect unless the corresponding sign is also set (even a single
space `" "`).  Once a sign is in place, those two faces control the background
of every cell not covered by a diff sign, making them the key to a visually
uniform column.

To keep the gutter background in sync with the active theme without hardcoding
a color, derive it from the `line-number` face:

```lisp
(defun my-git-gutter-update-faces ()
  (let ((bg (face-background 'line-number nil t)))
    (when bg
      ;; All gutter cells share the same background as the line-number column.
      (dolist (face '(git-gutter:added git-gutter:modified git-gutter:deleted
                                       git-gutter:unchanged git-gutter:separator))
        (set-face-background face bg))
      ;; Make unchanged and separator invisible by matching foreground to background.
      (dolist (face '(git-gutter:unchanged git-gutter:separator))
        (set-face-foreground face bg)))))

;; Re-run after every theme change (Emacs 29+).
(add-hook 'enable-theme-functions (lambda (&rest _) (my-git-gutter-update-faces)))
(my-git-gutter-update-faces)
```

#### Recommendation for theme designers

On Emacs 31.1+, themes should set the background of the `margin` face —
typically matching `line-number`'s background, or a related accent.  This
single setting covers every margin cell in every buffer.

For backward compatibility with Emacs ≤30.x, themes that include explicit
support for git-gutter should additionally set the background of
`git-gutter:unchanged` and `git-gutter:separator` to the same color.  Leaving
these backgrounds unspecified is the most common cause of a broken-looking
gutter column on those older Emacs versions: an empty stripe drawn in the
frame's default background, mismatched against the line-number column.

### Hide gutter if there are no changes

Hide gutter when there are no changes if `git-gutter:hide-gutter` is non-nil.
(Default is nil)

```lisp
(custom-set-variables
 '(git-gutter:hide-gutter t))
```

### Pass option to 'git diff' command

You can pass `git diff` option to set `git-gutter:diff-option`.

```lisp
;; ignore all spaces
(custom-set-variables
 '(git-gutter:diff-option "-w"))
```

### Don't ask whether commit/revert or not

`git-gutter.el` always asks you whether commit/revert or not. If you don't want,
please set `git-gutter:ask-p` to `nil`.

```lisp
;; Don't ask me!!
(custom-set-variables
 '(git-gutter:ask-p nil))
```

### Log/Message Level

```lisp
;; Don't need log/message.
(custom-set-variables
 '(git-gutter:verbosity 0))
```

Default value is 4(`0` is lowest, `4` is highest).

### Run hook

Run hook `git-gutter-mode-on-hook` when `git-gutter-mode` is turn on, and
run hook `git-gutter-mode-off-hook` when `git-gutter-mode` is turn off.

## Statistic

`git-gutter.el` provides some statistic API. This is useful for knowing how much
code you changed etc. To display them in mode-line is also useful.

#### `(git-gutter:buffer-hunks)`

Count unstaged hunks in current buffer.

#### `(git-gutter:all-hunks)`

Count unstaged hunks in all buffers

#### `(git-gutter:statistic)`

Return statistic unstaged hunks in current buffer. Return value is dot-list.
First element is total added lines, second element is total deleted lines.

## See Also

### [GitGutter](https://github.com/jisaacks/GitGutter)

GitGutter is Sublime Text plugin.

### [vim-gitgutter](https://github.com/airblade/vim-gitgutter)

Vim version of GitGutter

### [diff-hl](https://github.com/dgutov/diff-hl)

`diff-hl` is similar tool based on `vc`.

### [git-gutter-plus](https://github.com/nonsequitur/git-gutter-plus)

Fork of `git-gutter.el`. Some features which are not provided `git-gutter.el` provides.
However git-gutter-plus updates diff information synchronously.

[melpa-link]: http://melpa.org/#/git-gutter
[melpa-stable-link]: http://stable.melpa.org/#/git-gutter
[gh-actions-link]: https://github.com/emacsorphanage/git-gutter/actions
[melpa-badge]: http://melpa.org/packages/git-gutter-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/git-gutter-badge.svg
[gh-actions-badge]: https://github.com/emacsorphanage/git-gutter/workflows/CI/badge.svg
