;;; git-gutter.el --- Port of Sublime Text 2 plugin GitGutter

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-git-gutter
;; Version: 0.08

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
;; Port of GitGutter which is a plugin of Sublime Text2

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup git-gutter nil
  "Port GitGutter"
  :prefix "git-gutter:"
  :group 'vc)

(defcustom git-gutter:window-width nil
  "Character width of gutter window. Emacs mistakes width of some characters.
It is better to explicitly assign width to this variable, if you use full-width
character for signs of changes"
  :type 'integer
  :group 'git-gutter)

(defcustom git-gutter:diff-option ""
  "Option of 'git diff'"
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:modified-sign "="
  "Modified sign"
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:added-sign "+"
  "Added sign"
  :type 'string
  :group 'git-gutter)

(defcustom git-gutter:deleted-sign "-"
  "Deleted sign"
  :type 'string
  :group 'git-gutter)

(defface git-gutter:modified
    '((t (:foreground "magenta" :weight bold)))
  "Face of modified"
  :group 'git-gutter)

(defface git-gutter:added
    '((t (:foreground "green" :weight bold)))
  "Face of added"
  :group 'git-gutter)

(defface git-gutter:deleted
    '((t (:foreground "red" :weight bold)))
  "Face of deleted"
  :group 'git-gutter)

(defvar git-gutter:overlays nil)
(make-variable-buffer-local 'git-gutter:overlays)

(defun git-gutter:in-git-repository-p ()
  (call-process-shell-command "git rev-parse --is-inside-work-tree"))

(defun git-gutter:root-directory ()
  (with-temp-buffer
    (let* ((cmd "git rev-parse --show-toplevel")
           (ret (call-process-shell-command cmd nil t)))
      (unless (zerop ret)
        (error "Here is not git repository!!"))
      (goto-char (point-min))
      (file-name-as-directory
       (buffer-substring-no-properties (point) (line-end-position))))))

(defun git-gutter:changes-to-number (str)
  (if (string= str "")
      1
    (string-to-number str)))

(defun git-gutter:make-diffinfo (type start &optional end)
  (list :type type :start-line start :end-line end))

(defun git-gutter:diff (curfile)
  (let ((cmd (format "git diff -U0 %s %s" git-gutter:diff-option curfile))
        (regexp "^@@ -\\([0-9]+\\),?\\([0-9]*\\) \\+\\([0-9]+\\),?\\([0-9]*\\) @@"))
    (with-temp-buffer
      (let ((ret (call-process-shell-command cmd nil t)))
        (unless (or (zerop ret))
          (error (format "Failed '%s'" cmd))))
      (goto-char (point-min))
      (loop while (re-search-forward regexp nil t)
            for orig-line = (string-to-number (match-string 1))
            for new-line  = (string-to-number (match-string 3))
            for orig-changes = (git-gutter:changes-to-number (match-string 2))
            for new-changes = (git-gutter:changes-to-number (match-string 4))
            for end-line = (1- (+ new-line new-changes))
            collect
            (cond ((zerop orig-changes)
                   (git-gutter:make-diffinfo 'added new-line end-line))
                  ((zerop new-changes)
                   (git-gutter:make-diffinfo 'deleted (1- orig-line)))
                  (t
                   (git-gutter:make-diffinfo 'modified new-line end-line)))))))

(defun git-gutter:line-to-pos (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defmacro git-gutter:before-string (sign)
  `(propertize " " 'display `((margin left-margin) ,sign)))

(defun git-gutter:select-face (type)
  (case type
    (added 'git-gutter:added)
    (modified 'git-gutter:modified)
    (deleted 'git-gutter:deleted)))

(defun git-gutter:select-sign (type)
  (case type
    (added git-gutter:added-sign)
    (modified git-gutter:modified-sign)
    (deleted git-gutter:deleted-sign)))

(defun git-gutter:propertized-sign (type)
  (let ((sign (git-gutter:select-sign type))
        (face (git-gutter:select-face type)))
    (propertize sign 'face face)))

(defun git-gutter:view-region (sign start-line end-line)
  (let ((beg (git-gutter:line-to-pos start-line)))
    (goto-char beg)
    (while (and (<= (line-number-at-pos) end-line) (not (eobp)))
      (git-gutter:view-at-pos sign (point))
      (forward-line 1))))

(defun git-gutter:view-at-pos (sign pos)
  (let ((ov (make-overlay pos pos)))
    (overlay-put ov 'before-string (git-gutter:before-string sign))
    (push ov git-gutter:overlays)))

(defun git-gutter:view-diff-info (diffinfo)
  (let* ((start-line (plist-get diffinfo :start-line))
         (end-line (plist-get diffinfo :end-line))
         (type (plist-get diffinfo :type))
         (sign (git-gutter:propertized-sign type)))
    (case type
      (modified (git-gutter:view-region sign start-line end-line))
      (added (git-gutter:view-region sign start-line end-line))
      (deleted (git-gutter:view-at-pos
                sign (git-gutter:line-to-pos start-line))))))

(defun git-gutter:sign-width (sign)
  (loop for s across sign
        sum (char-width s)))

(defun git-gutter:longest-sign-width ()
  (let ((signs (list git-gutter:modified-sign
                     git-gutter:added-sign
                     git-gutter:deleted-sign)))
    (apply #'max (mapcar #'git-gutter:sign-width signs))))

(defun git-gutter:view-diff-infos (diffinfos)
  (let ((curwin (get-buffer-window))
        (win-width (or git-gutter:window-width
                       (git-gutter:longest-sign-width))))
    (save-excursion
      (mapc #'git-gutter:view-diff-info diffinfos)
      (set-window-margins curwin win-width (cdr (window-margins curwin))))))

(defun git-gutter:delete-overlay ()
  (mapc #'delete-overlay git-gutter:overlays)
  (setq git-gutter:overlays nil)
  (let ((curwin (get-buffer-window)))
    (set-window-margins curwin 0 (cdr (window-margins curwin)))))

(defvar git-gutter:view-diff-function #'git-gutter:view-diff-infos
  "Function of viewing changes")

(defvar git-gutter:clear-function #'git-gutter:clear-overlays
  "Function of clear changes")

(defun git-gutter:process-diff (curfile)
  (let ((diffinfos (git-gutter:diff curfile)))
    (funcall git-gutter:view-diff-function diffinfos)))

(defun git-gutter:clear-overlays ()
  (git-gutter:delete-overlay))

(defvar git-gutter:enabled nil)
(make-variable-buffer-local 'git-gutter:enabled)

;;;###autoload
(defun git-gutter ()
  (interactive)
  (git-gutter:delete-overlay)
  (when (buffer-file-name)
    (let* ((gitroot (git-gutter:root-directory)))
      (let ((default-directory gitroot)
            (current-file (file-relative-name (buffer-file-name) gitroot)))
        (git-gutter:process-diff current-file)
        (setq git-gutter:enabled t)))))

;;;###autoload
(defun git-gutter:clear ()
  (interactive)
  (funcall git-gutter:clear-function)
  (setq git-gutter:enabled nil))

;;;###autoload
(defun git-gutter:toggle ()
  (interactive)
  (if git-gutter:enabled
      (git-gutter:clear)
    (git-gutter)))

(defvar git-gutter:lighter " GitGutter")

;;;###autoload
(define-minor-mode git-gutter-mode ()
  "Git-Gutter mode"
  :group      'git-gutter
  :init-value nil
  :global     nil
  :lighter    git-gutter:lighter
  (if git-gutter-mode
      (if (zerop (git-gutter:in-git-repository-p))
          (progn
            (add-hook 'after-save-hook 'git-gutter nil t)
            (add-hook 'after-revert-hook 'git-gutter nil t)
            (run-with-idle-timer 0 nil 'git-gutter))
        (message "Here is not Git Repository!!")
        (git-gutter-mode -1))
    (remove-hook 'after-save-hook 'git-gutter t)
    (remove-hook 'after-revert-hook 'git-gutter t)
    (git-gutter:clear)))

;;;###autoload
(define-global-minor-mode global-git-gutter-mode
  git-gutter-mode
  (lambda ()
    (unless (minibufferp)
      (when (zerop (git-gutter:in-git-repository-p))
        (git-gutter-mode 1))))
  :group 'git-gutter)

(provide 'git-gutter)

;;; git-gutter.el ends here
