;;; git-gutter.el --- Port of Sublime Text plugin GitGutter -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2020 Syohei YOSHIDA <syohex@gmail.com>
;; Copyright (C) 2020-2022 Neil Okamoto <neil.okamoto+melpa@gmail.com>
;; Copyright (C) 2020-2024 Shen, Jen-Chieh <jcs090218@gmail.com>
;; Copyright (C) 2024-2025 Seven Beep <ebn@entreparentheses.xyz>

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Maintainer: Neil Okamoto <neil.okamoto+melpa@gmail.com>
;;             Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacsorphanage/git-gutter
;; Version: 0.94.5
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Port of GitGutter which is a plugin of Sublime Text

;;; Code:

(require 'cl-lib)

(defgroup git-gutter nil
  "Port GitGutter"
  :prefix "git-gutter:"
  :group 'vc)

;; Defined by `define-minor-mode' below; declared here for the byte compiler.
(defvar git-gutter-mode)

(defcustom git-gutter:window-width nil
  "Character width of gutter window.  Emacs mistakes width of some characters.
It is better to explicitly assign width to this variable, if you use full-width
character for signs of changes"
  :type 'integer
  :group 'git-gutter)

(defcustom git-gutter:diff-option ""
  "Option of `git diff\'."
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:subversion-diff-option ""
  "Option of `svn diff\'."
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:mercurial-diff-option ""
  "Option of `hg diff\'."
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:bazaar-diff-option ""
  "Option of `bzr diff\'."
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:jj-diff-option ""
  "Option of `jj diff\'."
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:update-hooks
  '(after-save-hook
    after-revert-hook
    find-file-hook
    after-change-major-mode-hook
    text-scale-mode-hook)
  "Hook points of updating gutter."
  :type '(list (hook :tag "HookPoint")
               (repeat :inline t (hook :tag "HookPoint")))
  :group 'git-gutter)

(defcustom git-gutter:always-show-separator nil
  "Show separator even if there are no changes."
  :type 'boolean
  :group 'git-gutter)

(defcustom git-gutter:separator-sign nil
  "Separator sign."
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:modified-sign "="
  "Modified sign."
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:added-sign "+"
  "Added sign."
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:deleted-sign "-"
  "Deleted sign."
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:staged-sign nil
  "Sign for lines whose changes are staged, or nil to show no staged signs.
Staged signs are shown only for git, and only when
`git-gutter:start-revision' is not set."
  :type '(choice (const :tag "No staged signs" nil) string)
  :group 'git-gutter)

(defcustom git-gutter:unchanged-sign nil
  "Unchanged sign."
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:hide-gutter nil
  "Hide gutter if there are no changes."
  :type 'boolean
  :group 'git-gutter)

(defcustom git-gutter:lighter " GitGutter"
  "Minor mode lighter in mode-line."
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:verbosity 0
  "Log/message level.  4 means all, 0 nothing."
  :type 'integer
  :group 'git-gutter)

(defcustom git-gutter:visual-line nil
  "Show sign at gutter by visual line."
  :type 'boolean
  :group 'git-gutter)

(defface git-gutter:separator
  '((t (:foreground "cyan" :weight bold :inherit default)))
  "Face of separator")

(defface git-gutter:modified
  '((t (:foreground "magenta" :weight bold :inherit default)))
  "Face of modified")

(defface git-gutter:added
  '((t (:foreground "green" :weight bold :inherit default)))
  "Face of added")

(defface git-gutter:deleted
  '((t (:foreground "red" :weight bold :inherit default)))
  "Face of deleted")

(defface git-gutter:unchanged
  '((t (:background "yellow" :inherit default)))
  "Face of unchanged")

(defface git-gutter:staged
  '((t (:foreground "cyan" :weight bold :inherit default)))
  "Face of staged")

(defcustom git-gutter:disabled-modes nil
  "A list of modes which `global-git-gutter-mode' should be disabled."
  :type '(repeat symbol)
  :group 'git-gutter)

(defcustom git-gutter:handled-backends '(git)
  "List of version control backends for which `git-gutter.el` will be used.
`git', `svn', `hg', `bzr' and `jj' are supported."
  :type '(repeat symbol)
  :group 'git-gutter)

(defvar git-gutter:view-diff-function #'git-gutter:view-diff-infos
  "Function of viewing changes.")

(defvar git-gutter:clear-function #'git-gutter:clear-diff-infos
  "Function of clear changes.")

(defvar git-gutter:init-function 'nil
  "Function of initialize.")

(defcustom git-gutter-mode-on-hook nil
  "Hook run when git-gutter mode enable."
  :type 'hook
  :group 'git-gutter)

(defcustom git-gutter-mode-off-hook nil
  "Hook run when git-gutter mode disable."
  :type 'hook
  :group 'git-gutter)

(defcustom git-gutter:update-interval nil
  "Idle time in seconds before a live update, or nil for no live updates.
A live update compares the unsaved buffer with the original version
and runs each time Emacs has been idle for this many seconds, for
example after you stop typing.  A value such as 0.1 shows changes while
you edit.  A new value takes effect at once, also when set with
`setq'.  The value 0 also means no live updates."
  :type '(choice (const :tag "No live updates" nil)
                 (number :tag "Idle seconds"))
  :group 'git-gutter)

(defun git-gutter:live-update-interval ()
  "Return `git-gutter:update-interval' if live updates are on, else nil."
  (and (numberp git-gutter:update-interval)
       (> git-gutter:update-interval 0)
       git-gutter:update-interval))

(defcustom git-gutter:ask-p t
  "Ask whether commit/revert or not."
  :type 'boolean
  :group 'git-gutter)

(defcustom git-gutter:display-p t
  "Display diff information or not."
  :type 'boolean
  :group 'git-gutter)

(defvar git-gutter:start-revision nil
  "Starting revision for vc diffs.
Can be a directory-local variable in your project.")

(make-variable-buffer-local 'git-gutter:start-revision)
(put 'git-gutter:start-revision 'safe-local-variable
     (lambda (x) (or (booleanp x) (stringp x))))

(cl-defstruct git-gutter-hunk
  type content start-line end-line)

(defvar-local git-gutter:enabled nil)
(defvar git-gutter:diffinfos nil)
(defvar git-gutter:has-indirect-buffers nil)

(defun git-gutter--has-indirect-buffers-p ()
  "Non-nil if an indirect buffer of the current buffer exists.
`git-gutter:has-indirect-buffers' counts them through advice on
`make-indirect-buffer', which does not run when natively compiled code
calls it, so also look for them."
  (or git-gutter:has-indirect-buffers
      (let ((base (current-buffer)))
        (cl-some (lambda (buf) (eq (buffer-base-buffer buf) base))
                 (buffer-list)))))
(defvar git-gutter:vcs-type nil)
(defvar git-gutter:revision-history nil)
(defvar git-gutter:update-timer nil)
(defvar-local git-gutter--installed-update-hooks nil
  "The hooks in which `git-gutter-mode' added `git-gutter' in this buffer.")
(defvar-local git-gutter:last-chars-modified-tick nil)
(defvar-local git-gutter--live-update-file nil
  "Temporary file that live update writes the buffer to, or nil.
The first live update in the buffer creates it; later ones overwrite it.")
(defvar-local git-gutter--temp-files-owner nil
  "The buffer whose live update made the temporary files named here.
A clone copies the local variables of its base buffer, and with them the
names of the base buffer's files; see `git-gutter--own-temp-files'.")
(defvar-local git-gutter--live-update-process nil
  "The diff process of the live update running in the current buffer, or nil.")
(defvar-local git-gutter--live-update-pending nil
  "Non-nil if a live update was due while the previous one's diff ran.")

(defvar-local git-gutter--last-update 0
  "Number of the last update started in the current buffer.
Each full or live update takes the next number when it starts, and shows
its result only if no other update has started since.")
(defvar-local git-gutter:live-update-cache nil
  "Cons (ROOT . ORIGINAL) that live update reuses, or nil.
ROOT is the true name of the repository root.  ORIGINAL is a temporary
file with the version live update compares the buffer with, or nil if
the file has no such version.  A full update clears the cache.")

(defvar git-gutter:popup-buffer "*git-gutter:diff*")
(defmacro git-gutter:awhen (test &rest body)
  "Anaphoric when.
Argument TEST is the case before BODY execution."
  (declare (indent 1))
  `(let ((it ,test))
     (when it ,@body)))

(defsubst git-gutter:execute-command (cmd output &rest args)
  (apply #'process-file cmd nil output nil args))

(defun git-gutter:in-git-repository-p ()
  (when (executable-find "git" t)
    (with-temp-buffer
      (when-let* ((exec-result (git-gutter:execute-command
                                "git" t "rev-parse" "--is-inside-work-tree")))
        (when (zerop exec-result)
          (goto-char (point-min))
          (looking-at-p "true"))))))

(defun git-gutter:in-repository-common-p (cmd check-subcmd repodir)
  (and (executable-find cmd t)
       (locate-dominating-file default-directory repodir)
       (zerop (apply #'git-gutter:execute-command cmd nil check-subcmd))
       (not (string-match-p (regexp-quote (concat "/" repodir "/"))
                            default-directory))))

(defun git-gutter:vcs-check-function (vcs)
  (cl-case vcs
    (git (git-gutter:in-git-repository-p))
    (svn (git-gutter:in-repository-common-p "svn" '("info") ".svn"))
    (hg (git-gutter:in-repository-common-p "hg" '("root") ".hg"))
    (bzr (git-gutter:in-repository-common-p "bzr" '("root") ".bzr"))
    (jj (git-gutter:in-repository-common-p "jj" '("root") ".jj"))))


(defun git-gutter:in-repository-p ()
  (cl-loop for vcs in git-gutter:handled-backends
           when (git-gutter:vcs-check-function vcs)
           return (setq-local git-gutter:vcs-type vcs)))

(defsubst git-gutter:changes-to-number (str)
  (if (string= str "")
      1
    (string-to-number str)))

(defsubst git-gutter:base-file ()
  (buffer-file-name (buffer-base-buffer)))

(defun git-gutter:diff-content ()
  (save-excursion
    (goto-char (line-beginning-position))
    (let ((curpoint (point)))
      (forward-line 1)
      (if (re-search-forward "^@@" nil t)
          (backward-char 3) ;; for '@@'
        (goto-char (point-max)))
      (buffer-substring curpoint (point)))))

(defvar git-gutter:diff-output-regexp
  "^@@ -\\(?:[0-9]+\\),?\\([0-9]*\\) \\+\\([0-9]+\\),?\\([0-9]*\\) @@"
  "Parse diff output.")

(defun git-gutter:process-diff-output (buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (goto-char (point-min))
      (cl-loop while (re-search-forward git-gutter:diff-output-regexp nil t)
               for new-line  = (string-to-number (match-string 2))
               for orig-changes = (git-gutter:changes-to-number (match-string 1))
               for new-changes = (git-gutter:changes-to-number (match-string 3))
               for type = (cond ((zerop orig-changes) 'added)
                                ((zerop new-changes) 'deleted)
                                (t 'modified))
               for end-line = (if (eq type 'deleted)
                                  new-line
                                (1- (+ new-line new-changes)))
               for content = (git-gutter:diff-content)
               collect
               (let ((start (if (zerop new-line) 1 new-line))
                     (end (if (zerop end-line) 1 end-line)))
                 (make-git-gutter-hunk
                  :type type :content content :start-line start :end-line end))))))

(defsubst git-gutter:window-margin ()
  (or git-gutter:window-width (git-gutter:longest-sign-width)))

(defun git-gutter:set-window-margin (width)
  (when (>= width 0)
    (let ((curwin (get-buffer-window)))
      (set-window-margins curwin width (cdr (window-margins curwin))))))

(defsubst git-gutter:revision-set-p ()
  (and git-gutter:start-revision (not (string= git-gutter:start-revision ""))))

(defun git-gutter:git-diff-arguments (file)
  (let (args)
    (unless (string= git-gutter:diff-option "")
      (setq args (nreverse (split-string git-gutter:diff-option))))
    (when (git-gutter:revision-set-p)
      (push git-gutter:start-revision args))
    (push "--" args)
    (nreverse (cons file args))))

(defun git-gutter:start-git-diff-process (file proc-buf)
  (let ((arg (git-gutter:git-diff-arguments file)))
    (apply #'start-file-process "git-gutter" proc-buf
           "git" "--no-pager" "-c" "diff.autorefreshindex=0"
           "diff" "--no-color" "--no-ext-diff" "--relative" "-U0"
           arg)))

(defun git-gutter:start-git-head-diff-process (file proc-buf)
  "Diff FILE in the working tree against HEAD, with output to PROC-BUF."
  (let ((git-gutter:start-revision "HEAD"))
    (git-gutter:start-git-diff-process file proc-buf)))

(defun git-gutter:svn-diff-arguments (file)
  (let (args)
    (unless (string= git-gutter:subversion-diff-option "")
      (setq args (nreverse (split-string git-gutter:subversion-diff-option))))
    (when (git-gutter:revision-set-p)
      (push "-r" args)
      (push git-gutter:start-revision args))
    (nreverse (cons file args))))

(defsubst git-gutter:start-svn-diff-process (file proc-buf)
  (let ((args (git-gutter:svn-diff-arguments file)))
    (apply #'start-file-process "git-gutter" proc-buf "svn" "diff" "--diff-cmd"
           "diff" "-x" "-U0" args)))

(defun git-gutter:hg-diff-arguments (file)
  (let (args)
    (unless (string= git-gutter:mercurial-diff-option "")
      (setq args (nreverse (split-string git-gutter:mercurial-diff-option))))
    (when (git-gutter:revision-set-p)
      (push "-r" args)
      (push git-gutter:start-revision args))
    (nreverse (cons file args))))

(defsubst git-gutter:start-hg-diff-process (file proc-buf)
  (let ((args (git-gutter:hg-diff-arguments file))
        (process-environment (cons "HGPLAIN=1" process-environment)))
    (apply #'start-file-process "git-gutter" proc-buf "hg" "diff" "-U0" args)))

(defun git-gutter:bzr-diff-arguments (file)
  (let (args)
    (unless (string= git-gutter:bazaar-diff-option "")
      (setq args (nreverse (split-string git-gutter:bazaar-diff-option))))
    (when (git-gutter:revision-set-p)
      (push "-r" args)
      (push git-gutter:start-revision args))
    (nreverse (cons file args))))

(defsubst git-gutter:start-bzr-diff-process (file proc-buf)
  (let ((args (git-gutter:bzr-diff-arguments file)))
    (apply #'start-file-process "git-gutter" proc-buf
           "bzr" "diff" "--context=0" args)))

(defun git-gutter:jj-diff-arguments (file)
  (let (args)
    (unless (string= git-gutter:jj-diff-option "")
      (setq args (nreverse (split-string git-gutter:jj-diff-option))))
    (when (git-gutter:revision-set-p)
      (push "--from" args)
      (push git-gutter:start-revision args))
    ;; jj reads a path as a fileset expression; quote it, so that names
    ;; with spaces or parentheses are not parsed as fileset syntax.
    (nreverse (cons (format "file:%S" file) args))))

(defsubst git-gutter:start-jj-diff-process (file proc-buf)
  (let ((args (git-gutter:jj-diff-arguments file)))
    (apply #'start-file-process "git-gutter" proc-buf
           "jj" "--config=ui.diff-formatter=:git"
           "--no-pager" "--quiet" "--color" "never" "diff" "--context" "0"
           args)))

(defun git-gutter:start-diff-process1 (file proc-buf)
  (let ((coding-system-for-read buffer-file-coding-system))
    (cl-case git-gutter:vcs-type
      (git (git-gutter:start-git-diff-process file proc-buf))
      (svn (git-gutter:start-svn-diff-process file proc-buf))
      (hg (git-gutter:start-hg-diff-process file proc-buf))
      (bzr (git-gutter:start-bzr-diff-process file proc-buf))
      (jj (git-gutter:start-jj-diff-process file proc-buf)))))

(defun git-gutter:show-staged-p ()
  "Non-nil when staged signs are shown in the current buffer."
  (and git-gutter:staged-sign
       (eq git-gutter:vcs-type 'git)
       (not (git-gutter:revision-set-p))))

(defun git-gutter:start-diff-process (curfile proc-buf)
  (if (git-gutter:show-staged-p)
      (git-gutter:start-combined-git-diff-process curfile proc-buf)
    (let* ((file (git-gutter:base-file))
           (curbuf (current-buffer))
           (update (cl-incf git-gutter--last-update))
           (process (git-gutter:start-diff-process1 curfile proc-buf)))
      (set-process-query-on-exit-flag process nil)
      (set-process-sentinel
       process
       (lambda (proc _event)
         (when (eq (process-status proc) 'exit)
           (let ((diffinfos (git-gutter:process-diff-output (process-buffer proc))))
             (when (and (buffer-live-p curbuf)
                        (= update (buffer-local-value 'git-gutter--last-update curbuf)))
               (with-current-buffer curbuf
                 (setq git-gutter:enabled nil)
                 (git-gutter:update-diffinfo diffinfos)
                 (when (git-gutter--has-indirect-buffers-p)
                   (git-gutter:update-indirect-buffers file))
                 (setq git-gutter:enabled t)))
             (kill-buffer proc-buf))))))))

(defun git-gutter:staged-hunks (head-hunks unstaged-hunks)
  "Return the parts of HEAD-HUNKS that are not in UNSTAGED-HUNKS.
HEAD-HUNKS are the changes since HEAD and UNSTAGED-HUNKS the changes not
in the index.  Both come from diffs against the working tree, so their
line numbers are the buffer's.  A line changed since HEAD but not
unstaged is staged.  The result has type `staged'."
  (let ((unstaged-lines (make-hash-table))
        staged)
    (dolist (hunk unstaged-hunks)
      (cl-loop for line from (git-gutter-hunk-start-line hunk)
               to (git-gutter-hunk-end-line hunk)
               do (puthash line t unstaged-lines)))
    (dolist (hunk head-hunks)
      (let (run-start)
        (cl-loop for line from (git-gutter-hunk-start-line hunk)
                 to (1+ (git-gutter-hunk-end-line hunk))
                 do (if (and (<= line (git-gutter-hunk-end-line hunk))
                             (not (gethash line unstaged-lines)))
                        (unless run-start (setq run-start line))
                      (when run-start
                        (push (make-git-gutter-hunk
                               :type 'staged
                               :content (git-gutter-hunk-content hunk)
                               :start-line run-start :end-line (1- line))
                              staged)
                        (setq run-start nil))))))
    (nreverse staged)))

(defun git-gutter:start-combined-git-diff-process (curfile proc-buf)
  "Diff CURFILE against the index and against HEAD, and show both.
PROC-BUF stays alive until both processes finish, so that `git-gutter'
does not start another pair of processes meanwhile."
  (let* ((file (git-gutter:base-file))
         (curbuf (current-buffer))
         (update (cl-incf git-gutter--last-update))
         (unstaged-buf (get-buffer-create (concat (buffer-name proc-buf) "-unstaged")))
         (head-buf (get-buffer-create (concat (buffer-name proc-buf) "-head")))
         (unstaged :pending)
         (head :pending)
         (finish
          (lambda ()
            (unless (or (eq unstaged :pending) (eq head :pending))
              (when (and (buffer-live-p curbuf)
                         (= update (buffer-local-value 'git-gutter--last-update curbuf)))
                (with-current-buffer curbuf
                  (setq git-gutter:enabled nil)
                  (git-gutter:update-diffinfo
                   (sort (append unstaged (git-gutter:staged-hunks head unstaged))
                         (lambda (a b)
                           (< (git-gutter-hunk-start-line a)
                              (git-gutter-hunk-start-line b)))))
                  (when (git-gutter--has-indirect-buffers-p)
                    (git-gutter:update-indirect-buffers file))
                  (setq git-gutter:enabled t)))
              (when (buffer-live-p proc-buf)
                (kill-buffer proc-buf))))))
    (dolist (spec (list (list unstaged-buf #'git-gutter:start-git-diff-process
                              (lambda (hunks) (setq unstaged hunks)))
                        (list head-buf #'git-gutter:start-git-head-diff-process
                              (lambda (hunks) (setq head hunks)))))
      (let ((process (funcall (nth 1 spec) curfile (nth 0 spec)))
            (store (nth 2 spec)))
        (set-process-query-on-exit-flag process nil)
        (set-process-sentinel
         process
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (funcall store (git-gutter:process-diff-output (process-buffer proc)))
             (kill-buffer (process-buffer proc))
             (funcall finish))))))))

(defsubst git-gutter:gutter-seperator ()
  (when git-gutter:separator-sign
    (propertize git-gutter:separator-sign 'face 'git-gutter:separator)))

(defun git-gutter:before-string (sign)
  (let ((gutter-sep (concat sign (git-gutter:gutter-seperator))))
    (propertize " " 'display `((margin left-margin) ,gutter-sep))))

(defun git-gutter:propertized-sign (type)
  (let (sign face)
    (cl-case type
      (added (setq sign git-gutter:added-sign
                   face 'git-gutter:added))
      (modified (setq sign git-gutter:modified-sign
                      face 'git-gutter:modified))
      (deleted (setq sign git-gutter:deleted-sign
                     face 'git-gutter:deleted))
      (staged (setq sign git-gutter:staged-sign
                    face 'git-gutter:staged)))
    (when (get-text-property 0 'face sign)
      (setq face (append
                  (get-text-property 0 'face sign)
                  `(:inherit ,face))))
    (propertize sign 'face face)))

(defun git-gutter:wrap-prefix-for-sign (sign pos)
  "Return a `wrap-prefix' string that renders SIGN in the left margin.
Prepends the gutter sign to any existing `wrap-prefix' text property at POS
so that continuation indentation (e.g. from `visual-wrap-prefix-mode') is
preserved on wrapped rows."
  (let ((existing (get-text-property pos 'wrap-prefix)))
    (concat (git-gutter:before-string sign) (if (stringp existing) existing ""))))

(cl-defstruct (git-gutter--group
               (:constructor git-gutter--make-group (key lines overlays)))
  "The sign overlays drawn for one hunk or one range of unchanged lines.
KEY identifies the signs, LINES is the number of lines, and OVERLAYS
are the overlays in buffer order."
  key lines overlays)

(defvar-local git-gutter--groups nil
  "The `git-gutter--group's of the sign overlays in the current buffer.")

(defvar-local git-gutter--edited t
  "The text edited since `git-gutter:view-diff-infos' last drew the signs.
nil if no text was edited, t for the whole buffer, or a cons of two
markers around the edited text.")

(defvar git-gutter--old-groups nil
  "Hash table from positions to the old `git-gutter--group' that starts there.
`git-gutter:view-diff-infos' binds it while it draws the signs.")

(defvar git-gutter--old-overlays nil
  "Hash table from positions to the old sign overlays that start there.
These overlays belong to no group any more; `git-gutter:put-signs'
reuses them.  `git-gutter:view-diff-infos' binds it while it draws the
signs.")

(defvar git-gutter--new-groups nil
  "The groups drawn so far by `git-gutter:view-diff-infos'.")

(defun git-gutter--after-change (beg end _len)
  "Add the text from BEG to END to `git-gutter--edited'.
Delete the sign overlays there that no longer start a line."
  (cond ((eq git-gutter--edited t))
        ((null git-gutter--edited)
         (setq git-gutter--edited (cons (copy-marker beg) (copy-marker end t))))
        (t
         (when (< beg (car git-gutter--edited))
           (set-marker (car git-gutter--edited) beg))
         (when (> end (cdr git-gutter--edited))
           (set-marker (cdr git-gutter--edited) end))))
  ;; When an edit joins two lines, the second line's overlay moves to the
  ;; join and would show a second sign on that screen row until the next
  ;; update.
  (save-excursion
    (dolist (ov (overlays-in beg (min (1+ end) (point-max))))
      (when (and (overlay-get ov 'git-gutter)
                 (progn (goto-char (overlay-start ov)) (not (bolp))))
        (delete-overlay ov)))))

(defun git-gutter--set-edited (value)
  "Set `git-gutter--edited' to VALUE, and release its old markers."
  (when (consp git-gutter--edited)
    (set-marker (car git-gutter--edited) nil)
    (set-marker (cdr git-gutter--edited) nil))
  (setq git-gutter--edited value))

(defun git-gutter--sign-key (sign)
  (list (substring-no-properties sign) (get-text-property 0 'face sign)
        git-gutter:separator-sign))

(defun git-gutter--release-group (group)
  "Move the overlays of GROUP to `git-gutter--old-overlays'."
  (dolist (ov (git-gutter--group-overlays group))
    (when (overlay-buffer ov)
      (push ov (gethash (overlay-start ov) git-gutter--old-overlays)))))

(defun git-gutter--take-group (pos)
  "Remove from `git-gutter--old-groups' the group at POS and return it."
  (when git-gutter--old-groups
    (let ((group (gethash pos git-gutter--old-groups)))
      (when group
        (remhash pos git-gutter--old-groups))
      group)))

(defun git-gutter--old-overlay (pos)
  "Remove from `git-gutter--old-overlays' an overlay at POS and return it.
If a group starts at POS, release its overlays first."
  (when git-gutter--old-overlays
    (git-gutter:awhen (git-gutter--take-group pos)
      (git-gutter--release-group it))
    (let ((ovs (gethash pos git-gutter--old-overlays)))
      (when ovs
        (puthash pos (cdr ovs) git-gutter--old-overlays)
        (car ovs)))))

(defvar git-gutter--edited-lines nil
  "The lines of `git-gutter--edited': t, nil, or (BEG . END).
BEG is the start of the first edited line, END the end of the last.
`git-gutter:view-diff-infos' binds it while it draws the signs.")

(defun git-gutter--edited-lines ()
  (if (consp git-gutter--edited)
      (save-excursion
        (cons (progn (goto-char (car git-gutter--edited)) (line-beginning-position))
              (progn (goto-char (cdr git-gutter--edited)) (line-end-position))))
    git-gutter--edited))

(defun git-gutter--group-edited-p (group)
  "Non-nil if text in the lines of GROUP was edited.
A group whose first or last overlay `git-gutter--after-change' deleted
counts as edited."
  (or (eq git-gutter--edited-lines t)
      (and git-gutter--edited-lines
           (let* ((ovs (git-gutter--group-overlays group))
                  (first (car ovs))
                  (last (car (last ovs))))
             (or (not (overlay-buffer first))
                 (not (overlay-buffer last))
                 ;; The overlays start at the beginning of their lines,
                 ;; so comparing the starts is enough.
                 (and (<= (overlay-start first) (cdr git-gutter--edited-lines))
                      (>= (overlay-start last) (car git-gutter--edited-lines))))))))

(defun git-gutter:put-signs (sign points &optional wrap-sign)
  "Put SIGN at each position in POINTS, and return the overlays.
When `git-gutter:visual-line' is non-nil, continuation rows show WRAP-SIGN,
or SIGN if WRAP-SIGN is nil.  An overlay from `git-gutter--old-overlays'
that already shows the same sign at the same position is kept unchanged."
  ;; SIGN is the same for all POINTS: build its display string once, and
  ;; let every overlay share it.
  (let ((gutter-sign (git-gutter:before-string sign))
        ;; Ensure changed signs win over separator/unchanged overlays.
        (priority (when (string-match-p "\\S-" (substring-no-properties sign))
                    10))
        (wrap-sign (or wrap-sign sign))
        (sign-key (git-gutter--sign-key sign))
        overlays)
    (dolist (pos points)
      (let* ((visual git-gutter:visual-line)
             ;; Span the line and its newline: an edit that deletes the
             ;; line deletes the overlay (`evaporate'), instead of moving
             ;; it onto the next line.  With `git-gutter:visual-line',
             ;; `wrap-prefix' then applies to every continuation row.
             ;; `line-end-position' respects fields, which on Emacs 27
             ;; takes time proportional to the number of overlays.
             (end (save-excursion (goto-char pos) (forward-line 1) (point)))
             (key (vector sign-key
                          (when visual (git-gutter--sign-key wrap-sign))
                          (when visual (get-text-property pos 'wrap-prefix))))
             (ov (git-gutter--old-overlay pos)))
        (if (not ov)
            (progn
              (setq ov (make-overlay pos end))
              (when (< pos end)
                (overlay-put ov 'evaporate t)))
          (unless (= (overlay-end ov) end)
            ;; `move-overlay' deletes an empty overlay that evaporates.
            (overlay-put ov 'evaporate (< pos end))
            (move-overlay ov pos end)))
        (unless (equal-including-properties (overlay-get ov 'git-gutter-key) key)
          (overlay-put ov 'before-string gutter-sign)
          (overlay-put ov 'priority priority)
          (overlay-put ov 'wrap-prefix
                       (when visual (git-gutter:wrap-prefix-for-sign wrap-sign pos)))
          (overlay-put ov 'git-gutter-key key))
        (overlay-put ov 'git-gutter t)
        (push ov overlays)))
    (nreverse overlays)))

(defun git-gutter--signs-key (sign &optional wrap-sign)
  (list (git-gutter--sign-key sign)
        (when wrap-sign (git-gutter--sign-key wrap-sign))
        git-gutter:visual-line))

(defun git-gutter--put-group (sign lines &optional wrap-sign key)
  "Put SIGN on LINES lines from point, and move to the line after them.
Keep the old group that starts at point if it shows the same signs on
the same number of lines and no text in them was edited.  A non-nil
WRAP-SIGN marks a deleted hunk: one sign, see `git-gutter:put-signs'.
KEY is `git-gutter--signs-key' of SIGN and WRAP-SIGN, or nil to compute it."
  (let ((key (or key (git-gutter--signs-key sign wrap-sign)))
        (old (git-gutter--take-group (point))))
    (if (and old
             (equal (git-gutter--group-key old) key)
             (= (git-gutter--group-lines old) lines)
             (not (git-gutter--group-edited-p old)))
        (progn
          (push old git-gutter--new-groups)
          (forward-line lines))
      (when old
        (git-gutter--release-group old))
      (let (points)
        (if wrap-sign
            (progn (push (point) points) (forward-line 1))
          (dotimes (_ lines)
            (unless (eobp)
              (push (point) points)
              (forward-line 1))))
        (when points
          (let ((overlays (git-gutter:put-signs sign (nreverse points) wrap-sign)))
            (when git-gutter--old-groups
              (push (git-gutter--make-group key lines overlays)
                    git-gutter--new-groups))))))))

(defsubst git-gutter:sign-width (sign)
  (cl-loop for s across sign
           sum (char-width s)))

(defun git-gutter:longest-sign-width ()
  (let ((signs (list git-gutter:modified-sign
                     git-gutter:added-sign
                     git-gutter:deleted-sign)))
    (when git-gutter:unchanged-sign
      (push git-gutter:unchanged-sign signs))
    (when git-gutter:staged-sign
      (push git-gutter:staged-sign signs))
    (+ (apply #'max (mapcar 'git-gutter:sign-width signs))
       (git-gutter:sign-width git-gutter:separator-sign))))

(defun git-gutter:propertized-unchanged-sign ()
  (if git-gutter:unchanged-sign
      (propertize git-gutter:unchanged-sign 'face 'git-gutter:unchanged)
    " "))

(defun git-gutter:build-unchanged-ranges (diffinfos max-line)
  "Build list of unchanged line ranges [start, end] from diff hunks.
Returns list of (start-line . end-line) pairs for unchanged regions."
  (if (null diffinfos)
      (list (cons 1 max-line))
    (let ((ranges '())
          (last-end 0))
      ;; Process gaps between hunks
      (dolist (hunk diffinfos)
        (let ((hunk-start (git-gutter-hunk-start-line hunk))
              (hunk-end (git-gutter-hunk-end-line hunk)))
          ;; Add unchanged range before this hunk
          (when (> hunk-start (1+ last-end))
            (push (cons (1+ last-end) (1- hunk-start)) ranges))
          (setq last-end hunk-end)))
      ;; Add final unchanged range after last hunk
      (when (< last-end max-line)
        (push (cons (1+ last-end) max-line) ranges))
      (nreverse ranges))))

(defun git-gutter:view-for-unchanged (diffinfos)
  "Put the unchanged sign on every line outside the hunks in DIFFINFOS.
Without `git-gutter:unchanged-sign', put a blank, which is followed by
`git-gutter:separator-sign'."
  (save-excursion
    (let* ((sign (git-gutter:propertized-unchanged-sign))
           (key (git-gutter--signs-key sign))
           (max-line (line-number-at-pos (point-max)))
           (line 1))
      (goto-char (point-min))
      (dolist (range (git-gutter:build-unchanged-ranges diffinfos max-line))
        ;; Move from the end of the previous range, not from `point-min'.
        (forward-line (- (car range) line))
        (git-gutter--put-group sign (1+ (- (cdr range) (car range))) nil key)
        (setq line (1+ (cdr range)))))))

(defsubst git-gutter:check-file-and-directory ()
  (and (git-gutter:base-file)
       default-directory (file-directory-p default-directory)))

(defun git-gutter:window-buffer-change-function (window)
  "Function to hook into `window-buffer-change-functions' to update `git-gutter'."
  (with-selected-window (window-normalize-window window)
    (when git-gutter-mode
      (git-gutter))))

(defun git-gutter:window-selection-change-function (window)
  "Update the signs when WINDOW becomes the selected window.
Emacs calls this from `window-selection-change-functions' also when
WINDOW is deselected; then it does nothing."
  (when (eq window (selected-window))
    (with-selected-window window
      (when git-gutter-mode
        (git-gutter)))))

(defsubst git-gutter:diff-process-buffer (curfile)
  (concat " *git-gutter-" curfile "-*"))

(defun git-gutter--kill-diff-process-buffers (proc-buf)
  "Kill the buffer named PROC-BUF and the buffers of its diff processes.
`git-gutter:start-combined-git-diff-process' runs its two processes in
the buffers PROC-BUF-unstaged and PROC-BUF-head.  Killing a buffer kills
its process."
  (dolist (name (list proc-buf
                      (concat proc-buf "-unstaged")
                      (concat proc-buf "-head")))
    (git-gutter:awhen (get-buffer name)
      (kill-buffer it))))

(defun git-gutter:kill-buffer-hook ()
  (git-gutter--delete-buffer-temp-files)
  ;; In an indirect buffer, the process buffer is the base buffer's.
  (unless (buffer-base-buffer)
    (git-gutter--kill-diff-process-buffers
     (git-gutter:diff-process-buffer (git-gutter:base-file)))))

;;;###autoload
(defun git-gutter:linum-setup ()
  "Do nothing; support for `linum-mode' has been removed.
Use `display-line-numbers-mode' instead."
  (display-warning 'git-gutter
                   "`git-gutter:linum-setup' does nothing; use `display-line-numbers-mode'"))
(make-obsolete 'git-gutter:linum-setup 'display-line-numbers-mode "0.95")

(defun git-gutter:show-backends ()
  (mapconcat (lambda (backend)
               (capitalize (symbol-name backend)))
             git-gutter:handled-backends "/"))

(defun git-gutter--install-update-hooks (hooks)
  "Run `git-gutter' from HOOKS in this buffer, and from no other hook."
  (dolist (hook git-gutter--installed-update-hooks)
    (remove-hook hook 'git-gutter t))
  (dolist (hook hooks)
    (add-hook hook 'git-gutter nil t))
  (setq git-gutter--installed-update-hooks (copy-sequence hooks)))

(defun git-gutter--update-hooks-watcher (_symbol newval operation where)
  "Install NEWVAL, the new `git-gutter:update-hooks', where the mode is on.
A buffer-local value (WHERE) applies to its buffer only.  Ignore `let'
bindings (OPERATION)."
  (when (eq operation 'set)
    (dolist (buf (if where (list where) (buffer-list)))
      (with-current-buffer buf
        (when (and git-gutter-mode
                   (or where (not (local-variable-p 'git-gutter:update-hooks))))
          (git-gutter--install-update-hooks newval))))))

(add-variable-watcher 'git-gutter:update-hooks
                      #'git-gutter--update-hooks-watcher)

;;;###autoload
(define-minor-mode git-gutter-mode
  "Git-Gutter mode"
  :init-value nil
  :global     nil
  :lighter    git-gutter:lighter
  (if git-gutter-mode
      (if (and (git-gutter:check-file-and-directory)
               (git-gutter:in-repository-p))
          (progn
            (when git-gutter:init-function
              (funcall git-gutter:init-function))
            (make-local-variable 'git-gutter:diffinfos)
            ;;(setq-local git-gutter:start-revision nil)
            (add-hook 'kill-buffer-hook 'git-gutter:kill-buffer-hook nil t)
            ;; A new major mode kills all local variables, the cache too.
            (add-hook 'change-major-mode-hook
                      #'git-gutter--delete-buffer-temp-files nil t)
            (add-hook 'kill-emacs-hook #'git-gutter--delete-temp-files)
            (add-hook 'after-change-functions #'git-gutter--after-change nil t)
            (add-hook 'window-buffer-change-functions
                      #'git-gutter:window-buffer-change-function nil t)
            (add-hook 'window-selection-change-functions
                      #'git-gutter:window-selection-change-function nil t)
            (git-gutter--install-update-hooks git-gutter:update-hooks)
            (git-gutter)
            (unless git-gutter:update-timer
              (git-gutter--start-update-timer git-gutter:update-interval)))
        (when (> git-gutter:verbosity 2)
          (message "Here is not %s work tree" (git-gutter:show-backends)))
        (git-gutter-mode -1))
    (git-gutter--delete-buffer-temp-files)
    (remove-hook 'kill-buffer-hook 'git-gutter:kill-buffer-hook t)
    (remove-hook 'change-major-mode-hook #'git-gutter--delete-buffer-temp-files t)
    (remove-hook 'after-change-functions #'git-gutter--after-change t)
    (git-gutter--set-edited t)
    (git-gutter--install-update-hooks nil)
    (remove-hook 'window-buffer-change-functions
                 #'git-gutter:window-buffer-change-function t)
    (remove-hook 'window-selection-change-functions
                 #'git-gutter:window-selection-change-function t)
    (git-gutter:clear-gutter)))

(defun git-gutter--turn-on ()
  (when (and (buffer-file-name)
             (not (memq major-mode git-gutter:disabled-modes)))
    (git-gutter-mode +1)))

;;;###autoload
(define-globalized-minor-mode global-git-gutter-mode git-gutter-mode git-gutter--turn-on)

(defsubst git-gutter:show-gutter-p (diffinfos)
  (if git-gutter:hide-gutter
      (or diffinfos git-gutter:unchanged-sign)
    (or global-git-gutter-mode git-gutter:unchanged-sign diffinfos)))

(defun git-gutter:show-gutter (diffinfos)
  (when (git-gutter:show-gutter-p diffinfos)
    (git-gutter:set-window-margin (git-gutter:window-margin))))

(defun git-gutter:view-set-overlays (diffinfos)
  (when (or git-gutter:unchanged-sign git-gutter:separator-sign)
    (git-gutter:view-for-unchanged diffinfos))
  (save-excursion
    (goto-char (point-min))
    (cl-loop with curline = 1
             ;; (TYPE SIGN WRAP-SIGN KEY) for each type met so far.
             with signs = nil
             for info in diffinfos
             for start-line = (git-gutter-hunk-start-line info)
             for end-line = (git-gutter-hunk-end-line info)
             for type = (git-gutter-hunk-type info)
             for (sign wrap-sign key)
             = (or (cdr (assq type signs))
                   (let* ((sign (git-gutter:propertized-sign type))
                          ;; The line of a deleted hunk is unchanged; mark
                          ;; only its first row.
                          (wrap-sign (when (eq type 'deleted)
                                       (git-gutter:propertized-unchanged-sign)))
                          (entry (list sign wrap-sign
                                       (git-gutter--signs-key sign wrap-sign))))
                     (push (cons type entry) signs)
                     entry))
             do
             (forward-line (- start-line curline))
             (git-gutter--put-group sign (if wrap-sign 1 (1+ (- end-line start-line)))
                                    wrap-sign key)
             (setq curline (1+ end-line)))))

(defsubst git-gutter:reset-window-margin-p ()
  (or git-gutter:hide-gutter (not global-git-gutter-mode)))

(defun git-gutter:view-diff-infos (diffinfos)
  "Show the signs for DIFFINFOS.
Skip the hunks whose signs are still right, keep the sign overlays that
still show the right sign, and delete the others."
  (let ((git-gutter--old-groups (make-hash-table))
        (git-gutter--old-overlays (make-hash-table))
        (git-gutter--new-groups nil)
        (git-gutter--edited-lines nil))
    ;; Edits in an indirect buffer do not run this buffer's
    ;; `after-change-functions'.
    (unless (and (memq #'git-gutter--after-change after-change-functions)
                 (not (buffer-base-buffer))
                 (not (git-gutter--has-indirect-buffers-p)))
      (git-gutter--set-edited t))
    (setq git-gutter--edited-lines (git-gutter--edited-lines))
    (if (eq git-gutter--edited-lines t)
        ;; No group can be kept.  Collect every sign overlay, also those
        ;; in no group, for example from before the package was reloaded.
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (overlay-get ov 'git-gutter)
            (push ov (gethash (overlay-start ov) git-gutter--old-overlays))))
      (dolist (group git-gutter--groups)
        (let ((first (car (git-gutter--group-overlays group))))
          (if (and (overlay-buffer first)
                   (not (gethash (overlay-start first) git-gutter--old-groups)))
              (puthash (overlay-start first) group git-gutter--old-groups)
            (git-gutter--release-group group)))))
    (when (or diffinfos git-gutter:always-show-separator)
      (git-gutter:view-set-overlays diffinfos))
    (maphash (lambda (_pos group) (mapc #'delete-overlay (git-gutter--group-overlays group)))
             git-gutter--old-groups)
    (maphash (lambda (_pos ovs) (mapc #'delete-overlay ovs))
             git-gutter--old-overlays)
    (setq git-gutter--groups git-gutter--new-groups)
    (git-gutter--set-edited nil))
  (if (git-gutter:show-gutter-p diffinfos)
      (git-gutter:set-window-margin (git-gutter:window-margin))
    (when (git-gutter:reset-window-margin-p)
      (git-gutter:set-window-margin 0))))

(defun git-gutter:clear-diff-infos ()
  (when (git-gutter:reset-window-margin-p)
    (git-gutter:set-window-margin 0))
  (remove-overlays (point-min) (point-max) 'git-gutter t)
  (setq git-gutter--groups nil))

(defun git-gutter--reset-state ()
  (setq git-gutter:enabled nil
        git-gutter:last-chars-modified-tick nil
        git-gutter:diffinfos nil))

(defun git-gutter:clear-gutter ()
  (save-restriction
    (widen)
    (when git-gutter:clear-function
      (funcall git-gutter:clear-function)))
  (git-gutter--reset-state))

(defun git-gutter:update-diffinfo (diffinfos)
  (save-restriction
    (widen)
    (if (and git-gutter:display-p
             (eq git-gutter:view-diff-function #'git-gutter:view-diff-infos)
             (eq git-gutter:clear-function #'git-gutter:clear-diff-infos))
        ;; `git-gutter:view-diff-infos' replaces only the signs that
        ;; changed.  Clearing all of them first makes the signs flicker.
        (git-gutter--reset-state)
      (git-gutter:clear-gutter))
    (setq git-gutter:diffinfos diffinfos)
    (when (and git-gutter:display-p git-gutter:view-diff-function)
      (funcall git-gutter:view-diff-function diffinfos))))

(defun git-gutter:search-near-diff-index (diffinfos is-reverse)
  (cl-loop with current-line = (line-number-at-pos)
           with cmp-fn = (if is-reverse #'> #'<)
           for diffinfo in (if is-reverse (reverse diffinfos) diffinfos)
           for index = 0 then (1+ index)
           for start-line = (git-gutter-hunk-start-line diffinfo)
           when (funcall cmp-fn current-line start-line)
           return (if is-reverse
                      (1- (- (length diffinfos) index))
                    index)))

(defun git-gutter:search-here-diffinfo (diffinfos)
  (save-restriction
    (widen)
    (cl-loop with current-line = (line-number-at-pos)
             for diffinfo in diffinfos
             for start = (git-gutter-hunk-start-line diffinfo)
             for end   = (or (git-gutter-hunk-end-line diffinfo) (1+ start))
             when (and (>= current-line start) (<= current-line end))
             return diffinfo
             finally do (error "Here is not changed!!"))))

(defun git-gutter:collect-deleted-line (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (cl-loop while (re-search-forward "^-\\(.*?\\)$" nil t)
             collect (match-string 1) into deleted-lines
             finally return deleted-lines)))

(defun git-gutter:delete-added-lines (start-line end-line)
  (forward-line (1- start-line))
  (let ((start-point (point)))
    (forward-line (1+ (- end-line start-line)))
    (delete-region start-point (point))))

(defun git-gutter:insert-deleted-lines (content)
  (dolist (line (git-gutter:collect-deleted-line content))
    (insert (concat line "\n"))))

(defsubst git-gutter:delete-from-first-line-p (start-line end-line)
  (and (not (= start-line 1)) (not (= end-line 1))))

(defun git-gutter:do-revert-hunk (diffinfo)
  (save-excursion
    (goto-char (point-min))
    (let ((start-line (git-gutter-hunk-start-line diffinfo))
          (end-line (git-gutter-hunk-end-line diffinfo))
          (content (git-gutter-hunk-content diffinfo)))
      (cl-case (git-gutter-hunk-type diffinfo)
        (added (git-gutter:delete-added-lines start-line end-line))
        (deleted (when (git-gutter:delete-from-first-line-p start-line end-line)
                   (forward-line start-line))
                 (git-gutter:insert-deleted-lines content))
        (modified (git-gutter:delete-added-lines start-line end-line)
                  (git-gutter:insert-deleted-lines content))))))

(defsubst git-gutter:popup-buffer-window ()
  (get-buffer-window (get-buffer git-gutter:popup-buffer)))

(defun git-gutter:query-action (action action-fn update-fn)
  (git-gutter:awhen (git-gutter:search-here-diffinfo git-gutter:diffinfos)
    (if (eq (git-gutter-hunk-type it) 'staged)
        (message "Hunk is already staged")
      (save-window-excursion
        (when git-gutter:ask-p
          (git-gutter:popup-hunk it))
        (when (or (not git-gutter:ask-p)
                  (yes-or-no-p (format "%s current hunk? " action)))
          (funcall action-fn it)
          (funcall update-fn))
        (if git-gutter:ask-p
            (delete-window (git-gutter:popup-buffer-window))
          (message "%s current hunk." action))))))

(defun git-gutter:revert-hunk ()
  "Revert current hunk."
  (interactive)
  (git-gutter:query-action "Revert" #'git-gutter:do-revert-hunk #'save-buffer))

(defun git-gutter:extract-hunk-header ()
  (git-gutter:awhen (git-gutter:base-file)
    (with-temp-buffer
      (when (zerop (git-gutter:execute-command
                    "git" t "--no-pager" "-c" "diff.autorefreshindex=0"
                    "diff" "--no-color" "--no-ext-diff"
                    (file-name-nondirectory it)))
        (goto-char (point-min))
        (forward-line 4)
        (buffer-substring-no-properties (point-min) (point))))))

(defvar git-gutter:git-hunk-header-regexp
  "^@@ -\\([0-9]+\\),?\\([0-9]*\\) \\+\\([0-9]+\\),?\\([0-9]*\\) @@"
  "Parse git hunk header.")

(defun git-gutter:read-hunk-header (header)
  (when (string-match git-gutter:git-hunk-header-regexp header)
    (list (string-to-number (match-string 1 header))
          (git-gutter:changes-to-number (match-string 2 header))
          (string-to-number (match-string 3 header))
          (git-gutter:changes-to-number (match-string 4 header)))))

(defun git-gutter:convert-hunk-header (type)
  (let ((header (buffer-substring-no-properties (point) (line-end-position))))
    (delete-region (point) (line-end-position))
    (cl-destructuring-bind
        (orig-line orig-changes new-line new-changes)
        (git-gutter:read-hunk-header header)
      (cl-case type
        (added (setq new-line (1+ orig-line)))
        (t (setq new-line orig-line)))
      (let ((new-header (format "@@ -%d,%d +%d,%d @@"
                                orig-line orig-changes new-line new-changes)))
        (insert new-header)))))

(defun git-gutter:insert-staging-hunk (hunk type)
  (save-excursion
    (insert hunk "\n"))
  (git-gutter:convert-hunk-header type))

(defun git-gutter:do-stage-hunk (diff-info)
  (let ((content (git-gutter-hunk-content diff-info))
        (type (git-gutter-hunk-type diff-info))
        (header (git-gutter:extract-hunk-header))
        (patch (make-temp-name "git-gutter"))
        (coding buffer-file-coding-system))
    (when header
      (with-temp-file patch
        ;; patch must match target file encoding, but unix eols are mandatory
        (setq buffer-file-coding-system
              (coding-system-change-eol-conversion coding 'unix))
        (insert header)
        (git-gutter:insert-staging-hunk content type))
      (unless (zerop (git-gutter:execute-command "git" nil
                                                 "apply" "--unidiff-zero"
                                                 "--cached" patch))
        (message "Failed: staging this hunk"))
      (delete-file patch))))

(defun git-gutter:stage-hunk ()
  "Stage this hunk like `git add -p'."
  (interactive)
  (git-gutter:query-action "Stage" #'git-gutter:do-stage-hunk #'git-gutter))

(defsubst git-gutter:line-point (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun git-gutter:mark-hunk ()
  (interactive)
  (git-gutter:awhen (git-gutter:search-here-diffinfo git-gutter:diffinfos)
    (let ((start (git-gutter:line-point (git-gutter-hunk-start-line it)))
          (end (git-gutter:line-point (1+ (git-gutter-hunk-end-line it)))))
      (goto-char start)
      (push-mark end nil t))))

(defun git-gutter:update-popuped-buffer (diffinfo)
  (let ((coding buffer-file-coding-system))
    (with-current-buffer (get-buffer-create git-gutter:popup-buffer)
      (setq buffer-file-coding-system (coding-system-base coding))
      (view-mode -1)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (git-gutter-hunk-content diffinfo))
      (insert "\n")
      (goto-char (point-min))
      (diff-mode)
      (view-mode +1)
      (current-buffer))))

(defun git-gutter:popup-hunk (&optional diffinfo)
  "Popup current diff hunk."
  (interactive)
  (git-gutter:awhen (or diffinfo
                        (git-gutter:search-here-diffinfo git-gutter:diffinfos))
    (save-selected-window
      (display-buffer (git-gutter:update-popuped-buffer it)))))

(defun git-gutter:popup-hunk-inline-at-point ()
  "Show hunk by temporarily expanding it at point"
  (interactive)
  (when-let* ((diffinfo (git-gutter:search-here-diffinfo git-gutter:diffinfos)))
    (let ((diff (with-temp-buffer
                  (insert (git-gutter-hunk-content diffinfo) "\n")
                  (diff-mode)
                  ;; Force-fontify the invisible temp buffer
                  (font-lock-ensure)
                  (buffer-string))))
      (momentary-string-display diff (line-beginning-position)))))

(defun git-gutter:next-hunk (arg)
  "Move to next diff hunk"
  (interactive "p")
  (if (not git-gutter:diffinfos)
      (when (> git-gutter:verbosity 3)
        (message "There are no changes!!"))
    (save-restriction
      (widen)
      (let* ((is-reverse (< arg 0))
             (diffinfos git-gutter:diffinfos)
             (len (length diffinfos))
             (index (git-gutter:search-near-diff-index diffinfos is-reverse))
             (real-index (if index
                             (let ((next (if is-reverse (1+ index) (1- index))))
                               (mod (+ arg next) len))
                           (if is-reverse (1- len) 0)))
             (diffinfo (nth real-index diffinfos)))
        (goto-char (point-min))
        (forward-line (1- (git-gutter-hunk-start-line diffinfo)))
        (when (> git-gutter:verbosity 0)
          (message "Move to %d/%d hunk" (1+ real-index) len))
        (when (buffer-live-p (get-buffer git-gutter:popup-buffer))
          (git-gutter:update-popuped-buffer diffinfo))))))

(defun git-gutter:previous-hunk (arg)
  "Move to previous diff hunk"
  (interactive "p")
  (git-gutter:next-hunk (- arg)))

(defun git-gutter:end-of-hunk ()
  "Move to end of current diff hunk"
  (interactive)
  (git-gutter:awhen (git-gutter:search-here-diffinfo git-gutter:diffinfos)
    (let ((lines (- (git-gutter-hunk-end-line it) (line-number-at-pos))))
      (forward-line lines))))

(defalias 'git-gutter:next-diff 'git-gutter:next-hunk)
(make-obsolete 'git-gutter:next-diff 'git-gutter:next-hunk "0.60")
(defalias 'git-gutter:previous-diff 'git-gutter:previous-hunk)
(make-obsolete 'git-gutter:previous-diff 'git-gutter:previous-hunk "0.60")
(defalias 'git-gutter:popup-diff 'git-gutter:popup-hunk)
(make-obsolete 'git-gutter:popup-diff 'git-gutter:popup-hunk "0.60")

(defun git-gutter:update-indirect-buffers (orig-file)
  (cl-loop with diffinfos = git-gutter:diffinfos
           for win in (window-list)
           for buf  = (window-buffer win)
           for base = (buffer-base-buffer buf)
           when (and base (string= (buffer-file-name base) orig-file))
           do
           (with-current-buffer buf
             (git-gutter:update-diffinfo diffinfos))))

;;;###autoload
(defun git-gutter ()
  "Show diff information in gutter"
  (interactive)
  ;; The index or the commit may have changed since live update last read
  ;; the original version.
  (git-gutter:clear-live-update-cache)
  ;; Draw every sign again: a `wrap-prefix' that a sign overlay copied
  ;; may have changed without an edit.
  (git-gutter--set-edited t)
  (when (or git-gutter:vcs-type (git-gutter:in-repository-p))
    (let* ((file (git-gutter:base-file))
           (proc-buf (git-gutter:diff-process-buffer file)))
      (when (called-interactively-p 'interactive)
        (git-gutter--kill-diff-process-buffers proc-buf))
      (when (and file (file-exists-p file) (not (get-buffer proc-buf)))
        (git-gutter:start-diff-process (file-name-nondirectory file)
                                       (get-buffer-create proc-buf))))))

(defun git-gutter:kill-indirect-buffer ()
  (when (buffer-live-p (buffer-base-buffer))
    (with-current-buffer (buffer-base-buffer)
      (when git-gutter:has-indirect-buffers
        (if (< 1 git-gutter:has-indirect-buffers)
            (setq git-gutter:has-indirect-buffers (1- git-gutter:has-indirect-buffers))
          (kill-local-variable 'git-gutter:has-indirect-buffers))))))

(defun git-gutter:make-indirect-buffer (oldfun base-buffer &rest args)
  (with-current-buffer (or (buffer-base-buffer (window-normalize-buffer base-buffer))
                           base-buffer)
    (if git-gutter:has-indirect-buffers
        (setq git-gutter:has-indirect-buffers (1+ git-gutter:has-indirect-buffers))
      (setq-local git-gutter:has-indirect-buffers 1))
    (with-current-buffer (apply oldfun base-buffer args)
      (add-hook 'kill-buffer-hook #'git-gutter:kill-indirect-buffer nil t)
      ;; A clone copies the local variables of BASE-BUFFER.  It must not
      ;; delete the base buffer's files, nor draw on its overlays.
      (setq git-gutter:live-update-cache nil
            git-gutter--live-update-file nil
            git-gutter--live-update-process nil
            git-gutter--live-update-pending nil
            git-gutter--groups nil)
      (current-buffer))))
(advice-add 'make-indirect-buffer :around #'git-gutter:make-indirect-buffer)

(defun git-gutter:vc-revert (&rest _args)
  (when git-gutter-mode
    (run-with-idle-timer 0.1 nil 'git-gutter)))
(advice-add 'vc-revert :after #'git-gutter:vc-revert)

(defun git-gutter:toggle-truncate-lines (&rest _args)
  (when (and git-gutter-mode git-gutter:visual-line)
    (run-with-idle-timer 0.1 nil 'git-gutter)))
(advice-add 'toggle-truncate-lines :after #'git-gutter:toggle-truncate-lines)

(defun git-gutter:clear ()
  "Clear diff information in gutter."
  (interactive)
  (git-gutter-mode -1))
(make-obsolete 'git-gutter:clear #'git-gutter-mode "0.86")

;;;###autoload
(defun git-gutter:toggle ()
  "Toggle to show diff information."
  (interactive)
  (if git-gutter-mode
      (git-gutter-mode -1)
    (git-gutter-mode +1)))
(make-obsolete 'git-gutter:toggle #'git-gutter-mode "0.86")

(defun git-gutter:revision-valid-p (revision)
  (zerop (cl-case git-gutter:vcs-type
           (git (git-gutter:execute-command "git" nil
                                            "rev-parse" "--quiet" "--verify"
                                            revision))
           (svn (git-gutter:execute-command "svn" nil "info" "-r" revision
                                            (file-relative-name (buffer-file-name))))
           (hg (git-gutter:execute-command "hg" nil "id" "-r" revision))
           (bzr (git-gutter:execute-command "bzr" nil
                                            "revno" "-r" revision))
           (jj (git-gutter:execute-command "jj" nil
                                           "log" "--ignore-working-copy" "--no-graph"
                                           "-r" revision "-T" "commit_id.short()")))))

(defun git-gutter:set-start-revision (start-rev)
  "Set start revision. If `start-rev' is nil or empty string then reset
start revision."
  (interactive
   (list (read-string "Start Revision: "
                      nil 'git-gutter:revision-history)))
  (when (and start-rev (not (string= start-rev "")))
    (unless (git-gutter:revision-valid-p start-rev)
      (error "Revision '%s' is not valid." start-rev)))
  (setq git-gutter:start-revision start-rev)
  ;; A running diff compares with the old revision, and `git-gutter'
  ;; starts no new diff while it runs.
  (git-gutter--kill-diff-process-buffers
   (git-gutter:diff-process-buffer (git-gutter:base-file)))
  (git-gutter))

(defun git-gutter:update-all-windows ()
  "Update git-gutter information for all visible buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (when (get-buffer-window buf 'visible)
      (with-current-buffer buf
        (when git-gutter-mode
          (git-gutter))))))

(defun git-gutter--start-update-timer (interval)
  "Start the live-update timer if INTERVAL is a number above 0."
  (let ((git-gutter:update-interval interval))
    (when (git-gutter:live-update-interval)
      (setq git-gutter:update-timer
            (run-with-idle-timer (git-gutter:live-update-interval) t
                                 'git-gutter:live-update)))))

(defun git-gutter:start-update-timer ()
  (interactive)
  (when git-gutter:update-timer
    (error "Update timer is already running."))
  (unless (git-gutter:live-update-interval)
    (user-error "Set `git-gutter:update-interval' to a number above 0 first"))
  (git-gutter--start-update-timer git-gutter:update-interval))

(defun git-gutter:cancel-update-timer ()
  (interactive)
  (unless git-gutter:update-timer
    (error "Timer is no running."))
  (cancel-timer git-gutter:update-timer)
  (setq git-gutter:update-timer nil))

(defun git-gutter--update-interval-watcher (_symbol newval operation where)
  "Restart the live-update timer with NEWVAL, the new `git-gutter:update-interval'.
Start it only if a buffer has `git-gutter-mode' on; otherwise the mode
starts it.  Ignore `let' bindings and buffer-local values (OPERATION and
WHERE)."
  (when (and (eq operation 'set) (null where))
    (when git-gutter:update-timer
      (cancel-timer git-gutter:update-timer)
      (setq git-gutter:update-timer nil))
    (when (cl-some (lambda (buf) (buffer-local-value 'git-gutter-mode buf))
                   (buffer-list))
      (git-gutter--start-update-timer newval))))

(add-variable-watcher 'git-gutter:update-interval
                      #'git-gutter--update-interval-watcher)

(defsubst git-gutter:write-current-content (tmpfile)
  "Write the whole buffer to TMPFILE, also when it is narrowed."
  ;; `write-region' with START nil ignores the narrowing.  Writing from
  ;; the buffer copies no string.  With `coding-system-for-write' bound, a
  ;; character that `buffer-file-coding-system' cannot encode changes only
  ;; its own line; without it, Emacs may write the whole file in another
  ;; coding system, and every line with a non-ASCII character differs.
  (let ((coding-system-for-write buffer-file-coding-system)
        (buffer-file-format nil)
        (write-region-annotate-functions nil)
        (write-region-inhibit-fsync t))
    (write-region nil nil tmpfile nil 'silent)))

(defun git-gutter:original-file-content (file vcs)
  "Return the version of FILE that the full update diffs against.
That is `git-gutter:start-revision' if it is set; otherwise the index
for git, the parent of the working copy for jj, and the base revision
for svn, hg and bzr."
  (let ((coding-system-for-read (coding-system-base buffer-file-coding-system))
        ;; `git-gutter:start-revision' is buffer-local; read it before
        ;; `with-temp-buffer'.
        (revision (and (git-gutter:revision-set-p) git-gutter:start-revision)))
    (with-temp-buffer
      (cl-case vcs
        (git
         (when (zerop (process-file "git" nil t nil "show"
                                    (concat revision ":" file)))
           (buffer-substring-no-properties (point-min) (point-max))))
        ((svn hg bzr)
         (let ((command (symbol-name vcs)))
           (when (zerop (apply #'process-file command nil t nil "cat"
                               (append (and revision (list "-r" revision))
                                       (list file))))
             (buffer-substring-no-properties (point-min) (point-max)))))
        (jj
         ;; FILE is relative to the repository root.  Reading @- does not
         ;; need a snapshot of the working copy.
         (when (zerop (process-file "jj" nil t nil "--ignore-working-copy"
                                    "file" "show" "-r" (or revision "@-")
                                    (format "root-file:%S" file)))
           (buffer-substring-no-properties (point-min) (point-max))))))))

(defun git-gutter:write-original-content (tmpfile filename)
  (git-gutter:awhen (git-gutter:original-file-content filename git-gutter:vcs-type)
    (let ((coding buffer-file-coding-system))
      (with-temp-file tmpfile
        (setq buffer-file-coding-system coding)
        (insert it)
        t))))

(defsubst git-gutter:start-raw-diff-process (proc-buf original now)
  (let ((coding-system-for-read buffer-file-coding-system))
    (start-file-process "git-gutter:update-timer" proc-buf
                        "diff" "-U0" original now)))

(defun git-gutter:start-live-update (_file original now)
  "Start diff of ORIGINAL and NOW, the original and current versions.
The process is stored in `git-gutter--live-update-process'.  The first
argument, the file name, is not used."
  (let* ((curbuf (current-buffer))
         (update (cl-incf git-gutter--last-update))
         (proc-buf (generate-new-buffer " *git-gutter-live*"))
         (process (git-gutter:start-raw-diff-process proc-buf original now)))
    (setq git-gutter--live-update-process process)
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel
     process
     (lambda (proc _event)
       (unless (process-live-p proc)
         ;; diff exits with 0 or 1; 2 means it failed, for example
         ;; because a temporary file is gone.  Keep the signs then.
         (when (and (eq (process-status proc) 'exit)
                    (<= (process-exit-status proc) 1)
                    (buffer-live-p curbuf)
                    (= update (buffer-local-value 'git-gutter--last-update curbuf)))
           (let ((diffinfos (git-gutter:process-diff-output (process-buffer proc))))
             (with-current-buffer curbuf
               (setq git-gutter:enabled nil)
               (git-gutter:update-diffinfo diffinfos)
               (setq git-gutter:enabled t))))
         (when (buffer-live-p proc-buf)
           (kill-buffer proc-buf))
         (when (buffer-live-p curbuf)
           (with-current-buffer curbuf
             (when (eq git-gutter--live-update-process proc)
               (setq git-gutter--live-update-process nil))
             ;; The buffer changed while diff ran: update again.
             (when git-gutter--live-update-pending
               (setq git-gutter--live-update-pending nil)
               (git-gutter:live-update)))))))))

(defun git-gutter:should-update-p ()
  (let ((chars-modified-tick (buffer-chars-modified-tick)))
    (unless (equal chars-modified-tick git-gutter:last-chars-modified-tick)
      (setq git-gutter:last-chars-modified-tick chars-modified-tick))))

(defun git-gutter:vcs-root (vcs)
  (with-temp-buffer
    (cl-case vcs
      (git
       (when (zerop (process-file "git" nil t nil "rev-parse" "--show-toplevel"))
         (goto-char (point-min))
         (file-name-as-directory
          (buffer-substring-no-properties (point) (line-end-position)))))
      (svn
       (when (zerop (process-file "svn" nil t nil "info"))
         (goto-char (point-min))
         (when (re-search-forward "^Working Copy Root Path: \(.+\)$" nil t)
           (file-name-as-directory (match-string-no-properties 1)))))
      ((hg bzr jj)
       (let ((command (symbol-name vcs)))
         (when (zerop (process-file command nil t nil "root"))
           (goto-char (point-min))
           (file-name-as-directory
            (buffer-substring-no-properties (point) (line-end-position)))))))))

(defun git-gutter--own-temp-files ()
  "Forget live update's files and process if another buffer made them.
This happens in a clone, which copies its base buffer's local variables.
Advice on `make-indirect-buffer' does not run when natively compiled
code calls it, so the clone cannot rely on that to clear them."
  (when (and git-gutter--temp-files-owner
             (not (eq git-gutter--temp-files-owner (current-buffer))))
    (setq git-gutter:live-update-cache nil
          git-gutter--live-update-file nil
          git-gutter--live-update-process nil
          git-gutter--live-update-pending nil
          git-gutter--temp-files-owner nil)))

(defun git-gutter:clear-live-update-cache ()
  "Delete the file of `git-gutter:live-update-cache' and clear the cache."
  (git-gutter--own-temp-files)
  (let ((original (cdr git-gutter:live-update-cache)))
    (when (and original (file-exists-p original))
      (delete-file original)))
  (setq git-gutter:live-update-cache nil))

(defun git-gutter--delete-temp-files ()
  "Delete the temporary files of live update in every buffer.
`kill-emacs' runs no `kill-buffer-hook' and no process sentinels, so
this function in `kill-emacs-hook' deletes the files that they would."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (git-gutter--delete-buffer-temp-files))))

(defun git-gutter--delete-buffer-temp-files ()
  "Delete the temporary files of live update in the current buffer.
Stop the buffer's running live update first: its diff reads them."
  (git-gutter--own-temp-files)
  (when (process-live-p git-gutter--live-update-process)
    (delete-process git-gutter--live-update-process))
  (setq git-gutter--live-update-process nil)
  (when git-gutter:live-update-cache
    (git-gutter:clear-live-update-cache))
  (when git-gutter--live-update-file
    (git-gutter--delete-live-update-file)))

(defun git-gutter--delete-live-update-file ()
  "Delete `git-gutter--live-update-file' and forget it."
  (git-gutter--own-temp-files)
  (when (and git-gutter--live-update-file
             (file-exists-p git-gutter--live-update-file))
    (delete-file git-gutter--live-update-file))
  (setq git-gutter--live-update-file nil))

(defun git-gutter:live-update-cache (file)
  "Return `git-gutter:live-update-cache' for FILE, filling it if empty.
Filling it runs the version control system twice, synchronously: once
for the repository root and once for the original version of FILE."
  (git-gutter--own-temp-files)
  (or git-gutter:live-update-cache
      (setq git-gutter--temp-files-owner (current-buffer)
            git-gutter:live-update-cache
            (let* ((root (file-truename (git-gutter:vcs-root git-gutter:vcs-type)))
                   (original (make-temp-file "git-gutter-orig")))
              ;; ROOT is a true name; the file name must be one too, or a
              ;; path through a symbolic link becomes "../../..." relative
              ;; to ROOT.
              (unless (git-gutter:write-original-content
                       original (file-relative-name (file-truename file) root))
                (delete-file original)
                (setq original nil))
              (cons root original)))))

(defun git-gutter:live-update ()
  (git-gutter--own-temp-files)
  (git-gutter:awhen (git-gutter:base-file)
    (if (process-live-p git-gutter--live-update-process)
        ;; The previous diff still reads `git-gutter--live-update-file';
        ;; its sentinel runs this function again.
        (setq git-gutter--live-update-pending t)
      (when (and git-gutter:enabled
                 (git-gutter:should-update-p))
        (git-gutter:awhen (cdr (git-gutter:live-update-cache it))
          (unless (and git-gutter--live-update-file
                       (file-exists-p git-gutter--live-update-file))
            (setq git-gutter--temp-files-owner (current-buffer)
                  git-gutter--live-update-file (make-temp-file "git-gutter-cur")))
          (git-gutter:write-current-content git-gutter--live-update-file)
          (git-gutter:start-live-update (git-gutter:base-file)
                                        it git-gutter--live-update-file))))))

(defun git-gutter:all-hunks ()
  "Cound unstaged hunks in all buffers"
  (let ((sum 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when git-gutter-mode
          (cl-incf sum (git-gutter:buffer-hunks)))))
    sum))

(defun git-gutter:buffer-hunks ()
  "Count unstaged hunks in current buffer."
  (length git-gutter:diffinfos))

(defun git-gutter:stat-hunk (hunk)
  "Return (ADDED . DELETED), the number of lines HUNK adds and deletes.
Count the lines of the hunk's diff content that start with + and -."
  (with-temp-buffer
    (insert (git-gutter-hunk-content hunk))
    (goto-char (point-min))
    (let ((added 0)
          (deleted 0))
      (while (not (eobp))
        (cond ((looking-at-p "\\+") (cl-incf added))
              ((looking-at-p "-") (cl-incf deleted)))
        (forward-line 1))
      (cons added deleted))))

(defun git-gutter:statistic ()
  "Return statistic unstaged hunks in current buffer."
  (interactive)
  (cl-loop for hunk in (cl-remove 'staged git-gutter:diffinfos
                                  :key #'git-gutter-hunk-type)
           for (add . del) = (git-gutter:stat-hunk hunk)
           sum add into added
           sum del into deleted
           finally
           return (progn
                    (when (called-interactively-p 'interactive)
                      (message "Added %d lines, Deleted %d lines" added deleted))
                    (cons added deleted))))

(provide 'git-gutter)

;;; git-gutter.el ends here

;; Local Variables:
;; fill-column: 85
;; indent-tabs-mode: nil
;; elisp-lint-indent-specs: ((git-gutter:awhen . 1))
;; End:
