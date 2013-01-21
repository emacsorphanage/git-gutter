;;; git-gutter.el --- Port of Sublime Text 2 plugin GitGutter

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-git-gutter
;; Version: 0.01

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

(defstruct git-gutter:repoinfo root gitdir)
(defstruct git-gutter:diffinfo type start-line end-line)

(defun git-gutter:root-directory ()
  (with-temp-buffer
    (let* ((cmd "git rev-parse --show-toplevel")
           (ret (call-process-shell-command cmd nil t)))
      (unless (zerop ret)
        (error "Here is not git repository!!"))
      (goto-char (point-min))
      (file-name-as-directory
       (buffer-substring-no-properties (point) (line-end-position))))))

(defun git-gutter:repo-info ()
  (let* ((rootdir (git-gutter:root-directory))
         (gitdir (concat rootdir ".git")))
    (make-git-gutter:repoinfo :root rootdir :gitdir gitdir)))

(defun git-gutter:construct-diffinfo (type start end)
  (cond ((string= type "c")
         (make-git-gutter:diffinfo :type 'modified
                                   :start-line start :end-line end))
        ((string= type "a")
         (make-git-gutter:diffinfo :type 'added
                                   :start-line start :end-line end))
        ((string= type "d")
         (make-git-gutter:diffinfo :type 'deleted :start-line (1+ start)))
        (t
         (error (format "Unknown diff type '%s'" type)))))

(defun git-gutter:diff (orig cur)
  (let ((cmd (format "diff %s %s" orig cur))
        (regexp "^\\([0-9]+\\),?\\([0-9]*\\)\\(.\\)\\([0-9]+\\),?\\([0-9]*\\)"))
    (with-temp-buffer
      (let ((ret (call-process-shell-command cmd nil t)))
        (unless (or (zerop ret) (= ret 1)) ;; `diff' returns 1 if it succeeds
          (error (format "Failed '%s'" cmd))))
      (goto-char (point-min))
      (loop while (re-search-forward regexp nil t)
            collect
            (let* ((type (match-string 3))
                   (start-line (string-to-number (match-string 4)))
                   (temp-end (match-string 5))
                   (end-line (or (and (string= temp-end "") start-line)
                                 (string-to-number temp-end))))
              (git-gutter:construct-diffinfo type start-line end-line))))))

(defun git-gutter:remove-tempfiles (files)
  (dolist (file files)
    (delete-file file)))

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
  (let ((start-line (git-gutter:diffinfo-start-line diffinfo))
        (end-line (git-gutter:diffinfo-end-line diffinfo)))
    (case (git-gutter:diffinfo-type diffinfo)
      (modified
       (git-gutter:view-region
        (git-gutter:propertized-sign 'modified) start-line end-line))
      (added
       (git-gutter:view-region
        (git-gutter:propertized-sign 'added) start-line end-line))
      (deleted
       (git-gutter:view-at-pos
        (git-gutter:propertized-sign 'deleted)
        (git-gutter:line-to-pos start-line))))))

(defun git-gutter:longest-sign-length ()
  (let ((signs (list git-gutter:modified-sign
                     git-gutter:added-sign
                     git-gutter:deleted-sign)))
    (apply #'max
           (mapcar (lambda (c)
                     (char-width (string-to-char c))) signs))))

(defun git-gutter:view-diff-infos (diffinfos)
  (let ((curwin (get-buffer-window))
        (winlen (git-gutter:longest-sign-length)))
    (save-excursion
      (loop for diffinfo in diffinfos
            do
            (git-gutter:view-diff-info diffinfo))
      (set-window-margins curwin winlen (cdr (window-margins curwin))))))

(defun git-gutter:delete-overlay ()
  (mapc #'delete-overlay git-gutter:overlays)
  (setq git-gutter:overlays nil)
  (let ((curwin (get-buffer-window)))
    (set-window-margins curwin 0 (cdr (window-margins curwin)))))

(defun git-gutter:filepath-to-gitpath (path root)
  (file-relative-name path root))

(defun git-gutter:show-original-content-command (repoinfo file)
  (let ((gitdir (git-gutter:repoinfo-gitdir repoinfo))
        (rootdir (git-gutter:repoinfo-root repoinfo)))
    (format "git --git-dir=%s --work-tree=%s show HEAD:%s"
            gitdir rootdir (git-gutter:filepath-to-gitpath file rootdir))))

(defun git-gutter:original-content (repoinfo)
  (let ((cmd (git-gutter:show-original-content-command
              repoinfo (expand-file-name (buffer-file-name)))))
    (with-temp-buffer
      (let ((ret (call-process-shell-command cmd nil t)))
        (unless (zerop ret)
          (error (format "Did you add '%s' to Git repository ?"
                         (buffer-file-name))))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun git-gutter:write-file (filename content)
  (with-temp-file filename
    (insert content)))

(defun git-gutter:prepare-diff (repoinfo original current)
  (let ((orig-content (git-gutter:original-content repoinfo))
        (cur-content (buffer-substring-no-properties (point-min) (point-max))))
    (git-gutter:write-file original orig-content)
    (git-gutter:write-file current cur-content)))

(defvar git-gutter:view-diff-function #'git-gutter:view-diff-infos
  "Function of viewing diff information")

(defun git-gutter:process-diff (original current)
  (let ((diffinfos (git-gutter:diff original current)))
    (funcall git-gutter:view-diff-function diffinfos)))

;;;###autoload
(defun git-gutter ()
  (interactive)
  (git-gutter:delete-overlay)
  (let ((repoinfo (git-gutter:repo-info))
        (original (make-temp-name "gutter_orig"))
        (current  (make-temp-name "gutter_cur")))
    (git-gutter:prepare-diff repoinfo original current)
    (git-gutter:process-diff original current)
    (git-gutter:remove-tempfiles (list original current))))

(provide 'git-gutter)

;;; git-gutter.el ends here
