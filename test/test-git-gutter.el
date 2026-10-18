;;; test-git-gutter.el --- Test for git-gutter.el

;; Copyright (C) 2016-2020 Syohei YOSHIDA and Neil Okamoto

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Maintainer: Neil Okamoto <neil.okamoto+melpa@gmail.com>

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

;;; Code:

(require 'ert)
(require 'vc)
(require 'git-gutter)

;; suppress log message
(setq git-gutter:verbosity 0)

(ert-deftest git-gutter:sign-width ()
  "helper function `git-gutter:sign-width'"
  (let ((got1 (git-gutter:sign-width "a"))
        (got2 (git-gutter:sign-width "0123456789")))
    (should (= got1 1))
    (should (= got2 10))))

(ert-deftest git-gutter:propertized-sign ()
  "helper function `git-gutter:propertized-sign'"
  (should (string= (git-gutter:propertized-sign 'added) "+")))

(ert-deftest git-gutter:changes-to-number ()
  "helper function `git-gutter:changes-to-number'"
  (should (= (git-gutter:changes-to-number "") 1))
  (should (= (git-gutter:changes-to-number "123") 123)))

(defun with-temporary-directory (test-fn)
  "Create a temp directory and execute TEST-FN within."
  (let (tmpdir)
    (unwind-protect
        (progn
          (setq tmpdir (make-temp-file "git-gutter-test-" t))
          (let* ((default-directory tmpdir))
            (funcall test-fn)))
      (delete-directory tmpdir t))))

(ert-deftest git-gutter:in-git-repository-p ()
  "True only if default-directory is a git repo."

  (with-temporary-directory
   (lambda ()
     ;; not in a git repository
     (should-not (git-gutter:in-git-repository-p))

     ;; now create the repo and check again
     (vc-create-repo 'Git)
     (should (git-gutter:in-git-repository-p))
     
     ;; ...but not inside the .git subdirectory itself #36
     (with-current-buffer (find-file-noselect ".git/config")
       (should-not (git-gutter:in-git-repository-p))))))

(ert-deftest git-gutter ()
  "Should return nil if buffer does not related with file or file is not existed"
  (with-current-buffer (get-buffer-create "*not-related-file*")
    (should-not (git-gutter)))
  (let ((buf (find-file-noselect "not-found")))
    (with-current-buffer buf
      (should-not (git-gutter)))))

(ert-deftest git-gutter:collect-deleted-line ()
  "Should return lines which start with '-'"
  (let* ((input (mapconcat 'identity
                           (list "-apple" "-melon" "+orange")
                           "\n"))
         (got (git-gutter:collect-deleted-line input)))
    (should (equal got '("apple" "melon")))))

(ert-deftest git-gutter:insert-deleted-lines ()
  "Should insert deleted line"
  (let ((input (mapconcat 'identity
                          (list "-apple" "-melon" "+orange")
                          "\n")))
    (with-temp-buffer
      (git-gutter:insert-deleted-lines input)
      (should (string= (buffer-string)
                       "apple\nmelon\n")))))

(ert-deftest git-gutter:diff-content ()
  "Should return diff hunk"
  (let* ((input "@@-1,1+1,1@@
foo
bar
@@ -2,2 +2,2 @@")
         (got (with-temp-buffer
                (insert input)
                (goto-char (point-min))
                (goto-char (line-end-position))
                (git-gutter:diff-content))))
    (should (string= got "@@-1,1+1,1@@\nfoo\nbar"))))

(ert-deftest git-gutter:set-window-margin ()
  "Should change window margin"
  (git-gutter:set-window-margin 4)
  (let ((got (car (window-margins))))
    (should (= got 4))))

(ert-deftest git-gutter-mode-success ()
  "Case git-gutter-mode enabled"
  (with-temporary-directory
   (lambda ()
     (vc-create-repo 'Git)
     (let ((buf (find-file-noselect "test.el")))
       (unwind-protect
           (with-current-buffer buf
             (git-gutter-mode 1)
             (should git-gutter-mode))
         (kill-buffer buf))))))

(ert-deftest git-gutter-mode-failed ()
  "Case git-gutter-mode disabled"
  (with-temp-buffer
    (git-gutter-mode 1)
    (should-not git-gutter-mode))

  (let ((default-directory nil))
    (git-gutter-mode 1)
    (should-not git-gutter-mode))

  (let ((default-directory "foo"))
    (git-gutter-mode 1)
    (should-not git-gutter-mode))

  (when (file-directory-p ".git") ;; #36
    (with-current-buffer (find-file-noselect ".git/config")
      (git-gutter-mode 1)
      (should-not git-gutter-mode))))

(ert-deftest global-git-gutter-mode-success ()
  "Case global-git-gutter-mode enabled"
  (with-temporary-directory
   (lambda ()
     (vc-create-repo 'Git)
     (let ((buf (find-file-noselect "test.el")))
       (unwind-protect
           (with-current-buffer buf
             (global-git-gutter-mode 1)
             (should git-gutter-mode))
         (kill-buffer buf))))))

(ert-deftest global-git-gutter-mode-failed ()
  "Case global-git-gutter-mode disabled"

  (with-temp-buffer
    (global-git-gutter-mode t)
    (should-not git-gutter-mode))

  (let* ((git-gutter:disabled-modes '(emacs-lisp-mode))
         (buf (find-file-noselect "test-git-gutter.el")))
    (unwind-protect
        (with-current-buffer buf
          (global-git-gutter-mode t)
          (should-not git-gutter-mode))
      (kill-buffer buf))))

(ert-deftest git-gutter-git-diff-arguments ()
  "Command line options of `git diff'"

  (let ((git-gutter:diff-option "-a -b -c")
        (file "git-gutter.el"))
    (let ((got (git-gutter:git-diff-arguments file)))
      (should (equal got '("-a" "-b" "-c" "--" "git-gutter.el"))))

    (let* ((git-gutter:start-revision "HEAD")
           (got (git-gutter:git-diff-arguments file)))
      (should (equal got '("-a" "-b" "-c" "HEAD" "--" "git-gutter.el"))))))

(ert-deftest git-gutter-hg-diff-arguments ()
  "Command line options of `hg diff'"

  (let ((git-gutter:mercurial-diff-option "-a -b -c")
        (file "git-gutter.el"))
    (let ((got (git-gutter:hg-diff-arguments file)))
      (should (equal got '("-a" "-b" "-c" "git-gutter.el"))))

    (let* ((git-gutter:start-revision "30000")
           (got (git-gutter:hg-diff-arguments file)))
      (should (equal got '("-a" "-b" "-c" "-r" "30000" "git-gutter.el"))))))

(ert-deftest git-gutter-bzr-diff-arguments ()
  "Command line options of `bzr diff'"

  (let ((git-gutter:bazaar-diff-option "-a -b -c")
        (file "git-gutter.el"))
    (let ((got (git-gutter:bzr-diff-arguments file)))
      (should (equal got '("-a" "-b" "-c" "git-gutter.el"))))

    (let* ((git-gutter:start-revision "30000")
           (got (git-gutter:bzr-diff-arguments file)))
      (should (equal got '("-a" "-b" "-c" "-r" "30000" "git-gutter.el"))))))

(ert-deftest git-gutter-jj-diff-arguments ()
  "Command line options of `jj diff'"

  (let ((git-gutter:jj-diff-option "-a -b -c")
        (file "git-gutter.el"))
    (let ((got (git-gutter:jj-diff-arguments file)))
      (should (equal got '("-a" "-b" "-c" "file:\"git-gutter.el\""))))

    (let* ((git-gutter:start-revision "30000")
           (got (git-gutter:jj-diff-arguments file)))
      (should (equal got '("-a" "-b" "-c" "--from" "30000" "file:\"git-gutter.el\""))))))

(ert-deftest git-gutter-read-header ()
  "Read header of diff hunk"

  (let ((got (git-gutter:read-hunk-header "@@ -658,31 +688,30 @@")))
    (should (= (nth 0 got) 658))
    (should (= (nth 1 got) 31))
    (should (= (nth 2 got) 688))
    (should (= (nth 3 got) 30)))

  (let ((got (git-gutter:read-hunk-header "@@ -100 +200 @@")))
    (should (= (nth 0 got) 100))
    (should (= (nth 1 got) 1))
    (should (= (nth 2 got) 200))
    (should (= (nth 3 got) 1))))

(ert-deftest git-gutter-show-backends ()
  "Show only handled backends."

  (let ((git-gutter:handled-backends '(bzr))
        (expected "Bzr"))
    (should (string= (git-gutter:show-backends) expected)))

  (let ((git-gutter:handled-backends '(git hg bzr))
        (expected "Git/Hg/Bzr"))
    (should (string= (git-gutter:show-backends) expected))))

;; `clone-buffer' copies the buffer-local `kill-buffer-hook' of an
;; indirect buffer into a buffer that has no base buffer.
(ert-deftest git-gutter:kill-indirect-buffer-without-base ()
  "Killing a clone of an indirect buffer does not signal an error."
  (let* ((base (generate-new-buffer "git-gutter-base"))
         (indirect (make-indirect-buffer
                    base (generate-new-buffer-name "git-gutter-indirect")))
         (clone (with-current-buffer indirect (clone-buffer))))
    (unwind-protect
        (progn
          (should-not (buffer-base-buffer clone))
          (kill-buffer clone)
          (should-not (buffer-live-p clone)))
      (kill-buffer base))))

;; Staging tests: commit a file with lines 1..10, change line 5,
;; stage that hunk with `git-gutter:stage-hunk', and check the index.

(defun git-gutter-test:git (&rest args)
  "Run git with ARGS in `default-directory'; signal an error on failure."
  (with-temp-buffer
    (unless (zerop (apply #'process-file "git" nil t nil
                          "-c" "user.name=test" "-c" "user.email=test@example.com"
                          args))
      (error "git %S failed: %s" args (buffer-string)))
    (buffer-string)))

(defun git-gutter-test:stage-line-5 (file)
  "Change line 5 of FILE, stage the hunk with git-gutter, return `git diff --cached'."
  (let ((buf (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (forward-line 4)
          (delete-region (point) (line-end-position))
          (insert "FIVE")
          (save-buffer)
          (git-gutter-mode 1)
          (git-gutter)
          (with-timeout (10 (error "git-gutter did not finish"))
            (while (not git-gutter:enabled)
              (accept-process-output nil 0.1)))
          (goto-char (point-min))
          (forward-line 4)
          (let ((git-gutter:ask-p nil))
            (git-gutter:stage-hunk))
          (git-gutter-test:git "diff" "--cached" "--no-color"))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))))

(ert-deftest git-gutter:stage-hunk-in-subdirectory ()
  "Stage a hunk of a file that is not in the top directory of the repository."
  (with-temporary-directory
   (lambda ()
     (git-gutter-test:git "init" "-q")
     (make-directory "sub")
     (with-temp-file "sub/f.txt"
       (dotimes (i 10) (insert (format "%d\n" (1+ i)))))
     (git-gutter-test:git "add" "sub/f.txt")
     (git-gutter-test:git "commit" "-q" "-m" "init")
     (let ((cached (git-gutter-test:stage-line-5
                    (expand-file-name "sub/f.txt"))))
       (should (string-match-p "^\\+FIVE\r?$" cached))))))

(ert-deftest git-gutter:stage-hunk-in-bare-repository ()
  "Stage a hunk when GIT_DIR and GIT_WORK_TREE point to a bare repository."
  (with-temporary-directory
   (lambda ()
     (let ((git-dir (expand-file-name "repo.git"))
           (work-tree (file-name-as-directory (expand-file-name "home")))
           (process-environment (copy-sequence process-environment)))
       (git-gutter-test:git "init" "-q" "--bare" git-dir)
       (setenv "GIT_DIR" git-dir)
       (setenv "GIT_WORK_TREE" work-tree)
       (make-directory (expand-file-name ".config/x" work-tree) t)
       (let ((default-directory work-tree))
         (with-temp-file ".config/x/f.txt"
           (dotimes (i 10) (insert (format "%d\n" (1+ i)))))
         (git-gutter-test:git "add" ".config/x/f.txt")
         (git-gutter-test:git "commit" "-q" "-m" "init")
         (let ((cached (git-gutter-test:stage-line-5
                        (expand-file-name ".config/x/f.txt"))))
           (should (string-match-p "^\\+FIVE\r?$" cached))))))))

;; Window change hooks.  Emacs runs these hooks only during redisplay,
;; which batch mode does not do, so the tests call the hook functions
;; directly.

(defmacro git-gutter-test:with-file-in-repo (&rest body)
  "Run BODY in a buffer with `git-gutter-mode' on, visiting a committed file."
  (declare (indent 0))
  `(with-temporary-directory
    (lambda ()
      (git-gutter-test:git "init" "-q")
      (with-temp-file "f.txt" (insert "1\n"))
      (git-gutter-test:git "add" "f.txt")
      (git-gutter-test:git "commit" "-q" "-m" "init")
      (let ((buf (find-file-noselect (expand-file-name "f.txt"))))
        (unwind-protect
            (with-current-buffer buf
              (git-gutter-mode 1)
              ;; Wait for the diff process: on Windows, a running process
              ;; keeps the temporary directory from being deleted.
              (with-timeout (10 (error "git-gutter did not finish"))
                (while (not git-gutter:enabled)
                  (accept-process-output nil 0.1)))
              ,@body)
          (kill-buffer buf))))))

(defmacro git-gutter-test:count-git-gutter-calls (&rest body)
  "Run BODY with `git-gutter' replaced by a counter; return the count."
  (declare (indent 0))
  `(let ((calls 0))
     (cl-letf (((symbol-function 'git-gutter)
                (lambda () (setq calls (1+ calls)))))
       ,@body)
     calls))

(ert-deftest git-gutter:window-change-hooks-installed ()
  "`git-gutter-mode' adds and removes its buffer-local window change hooks."
  (git-gutter-test:with-file-in-repo
    (should (memq #'git-gutter:window-buffer-change-function
                  window-buffer-change-functions))
    (should (memq #'git-gutter:window-selection-change-function
                  window-selection-change-functions))
    (git-gutter-mode -1)
    (should-not (memq #'git-gutter:window-buffer-change-function
                      window-buffer-change-functions))
    (should-not (memq #'git-gutter:window-selection-change-function
                      window-selection-change-functions))))

(ert-deftest git-gutter:window-selection-change-function ()
  "Update only for the selected window, and only with `git-gutter-mode' on."
  (git-gutter-test:with-file-in-repo
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (let* ((selected (selected-window))
             ;; The batch frame of Emacs 27 is too small to split with the
             ;; default minimum window size.
             (other (let ((window-min-width 1)
                          (window-min-height 1))
                      (split-window-right))))
        (set-window-buffer other (current-buffer))
        (should (= 1 (git-gutter-test:count-git-gutter-calls
                       (git-gutter:window-selection-change-function selected))))
        (should (= 0 (git-gutter-test:count-git-gutter-calls
                       (git-gutter:window-selection-change-function other))))
        (git-gutter-mode -1)
        (should (= 0 (git-gutter-test:count-git-gutter-calls
                       (git-gutter:window-selection-change-function selected))))))))

(ert-deftest git-gutter:window-buffer-change-function ()
  "Update when a window starts showing a buffer with `git-gutter-mode' on."
  (git-gutter-test:with-file-in-repo
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (should (= 1 (git-gutter-test:count-git-gutter-calls
                     (git-gutter:window-buffer-change-function (selected-window)))))
      (git-gutter-mode -1)
      (should (= 0 (git-gutter-test:count-git-gutter-calls
                     (git-gutter:window-buffer-change-function (selected-window))))))))

;; `git-gutter:view-for-unchanged' puts an overlay on every line outside
;; the hunks: the unchanged sign, or a blank followed by the separator.

(defun git-gutter-test:unchanged-overlay-lines (hunks)
  "Lines 1..10 of a buffer that get an unchanged overlay for HUNKS."
  (with-temp-buffer
    (dotimes (i 10) (insert (format "%d\n" (1+ i))))
    (git-gutter:view-for-unchanged
     (mapcar (lambda (range)
               (make-git-gutter-hunk :type 'modified :content ""
                                     :start-line (car range)
                                     :end-line (cdr range)))
             hunks))
    (sort (mapcar (lambda (ov) (line-number-at-pos (overlay-start ov)))
                  (seq-filter (lambda (ov) (overlay-get ov 'git-gutter))
                              (overlays-in (point-min) (point-max))))
          #'<)))

(ert-deftest git-gutter:view-for-unchanged-lines ()
  "The unchanged sign goes on the lines outside the hunks."
  (let ((git-gutter:unchanged-sign ".")
        (git-gutter:separator-sign nil))
    (should (equal (git-gutter-test:unchanged-overlay-lines '((3 . 4) (7 . 7)))
                   '(1 2 5 6 8 9 10)))
    (should (equal (git-gutter-test:unchanged-overlay-lines '((1 . 1) (10 . 10)))
                   '(2 3 4 5 6 7 8 9)))
    (should (equal (git-gutter-test:unchanged-overlay-lines nil)
                   '(1 2 3 4 5 6 7 8 9 10)))))

(ert-deftest git-gutter:view-for-unchanged-separator-only ()
  "With only a separator, unchanged lines still get an overlay."
  (let ((git-gutter:unchanged-sign nil)
        (git-gutter:separator-sign "|"))
    (should (equal (git-gutter-test:unchanged-overlay-lines '((3 . 4)))
                   '(1 2 5 6 7 8 9 10)))))

;; `git-gutter:update-diffinfo' keeps the overlays whose sign did not
;; change, so that the signs do not flicker.

(defun git-gutter-test:hunk (type start end)
  (make-git-gutter-hunk :type type :content "" :start-line start :end-line end))

(defun git-gutter-test:sign-overlays ()
  "The sign overlays of the current buffer, sorted by position."
  (sort (seq-filter (lambda (ov) (overlay-get ov 'git-gutter))
                    (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun git-gutter-test:sign-lines ()
  "(LINE SIGN) for each sign overlay of the current buffer."
  (mapcar (lambda (ov)
            (list (line-number-at-pos (overlay-start ov))
                  (substring-no-properties
                   (cadr (get-text-property 0 'display (overlay-get ov 'before-string))))))
          (git-gutter-test:sign-overlays)))

(defmacro git-gutter-test:with-lines (&rest body)
  "Run BODY in a buffer with lines 1..10 and the default signs."
  (declare (indent 0))
  `(let ((git-gutter:unchanged-sign ".")
         (git-gutter:separator-sign nil)
         (git-gutter:visual-line nil)
         (git-gutter:view-diff-function #'git-gutter:view-diff-infos)
         (git-gutter:clear-function #'git-gutter:clear-diff-infos))
     (with-temp-buffer
       (dotimes (i 10) (insert (format "%d\n" (1+ i))))
       (add-hook 'after-change-functions #'git-gutter--after-change nil t)
       ,@body)))

(ert-deftest git-gutter:update-diffinfo-keeps-overlays ()
  "An update with the same hunks keeps every overlay unchanged."
  (git-gutter-test:with-lines
    (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'modified 3 4)))
    (let ((before (git-gutter-test:sign-overlays))
          (strings (mapcar (lambda (ov) (overlay-get ov 'before-string))
                           (git-gutter-test:sign-overlays))))
      (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'modified 3 4)))
      (should (equal (git-gutter-test:sign-overlays) before))
      (should (cl-every #'eq strings
                        (mapcar (lambda (ov) (overlay-get ov 'before-string))
                                (git-gutter-test:sign-overlays)))))))

(ert-deftest git-gutter:update-diffinfo-changes-only-changed-signs ()
  "An update changes the signs of the changed lines and keeps the others."
  (git-gutter-test:with-lines
    (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'modified 3 4)))
    (let ((line-8 (nth 7 (git-gutter-test:sign-overlays)))
          (string-8 (overlay-get (nth 7 (git-gutter-test:sign-overlays)) 'before-string)))
      (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'modified 3 3)
                                        (git-gutter-test:hunk 'added 6 6)))
      (should (equal (git-gutter-test:sign-lines)
                     '((1 ".") (2 ".") (3 "=") (4 ".") (5 ".") (6 "+")
                       (7 ".") (8 ".") (9 ".") (10 "."))))
      (should (eq (nth 7 (git-gutter-test:sign-overlays)) line-8))
      (should (eq (overlay-get line-8 'before-string) string-8)))
    ;; The same signs as drawing into an empty buffer.
    (let ((incremental (git-gutter-test:sign-lines)))
      (remove-overlays (point-min) (point-max) 'git-gutter t)
      (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'modified 3 3)
                                        (git-gutter-test:hunk 'added 6 6)))
      (should (equal (git-gutter-test:sign-lines) incremental)))))

(ert-deftest git-gutter:update-diffinfo-deletes-unused-overlays ()
  "Overlays that no line needs any more are deleted."
  (git-gutter-test:with-lines
    (let ((git-gutter:unchanged-sign nil))
      (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'modified 3 4)))
      (should (equal (git-gutter-test:sign-lines) '((3 "=") (4 "="))))
      (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'deleted 7 7)))
      (should (equal (git-gutter-test:sign-lines) '((7 "-"))))
      (git-gutter:update-diffinfo nil)
      (should (null (git-gutter-test:sign-overlays))))))

(ert-deftest git-gutter:update-diffinfo-visual-line-moves-overlay ()
  "With `git-gutter:visual-line', a longer line keeps its overlay."
  (git-gutter-test:with-lines
    (let ((git-gutter:visual-line t)
          (git-gutter:unchanged-sign nil))
      (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'modified 3 3)))
      (let ((ov (car (git-gutter-test:sign-overlays))))
        (goto-char (overlay-start ov))
        (end-of-line)
        (insert "abc")
        (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'modified 3 3)))
        (should (equal (git-gutter-test:sign-overlays) (list ov)))
        (should (= (overlay-end ov) (save-excursion (goto-char (overlay-start ov))
                                                    (1+ (line-end-position)))))))))

(ert-deftest git-gutter:deleted-lines-leave-no-sign ()
  "Deleting whole lines deletes their overlays instead of stacking them."
  (dolist (visual '(nil t))
    ;; Delete three empty lines at once, and one by one with backspace.
    (dolist (delete '(region backspace))
      (git-gutter-test:with-lines
        (let ((git-gutter:visual-line visual))
          (goto-char (point-min))
          (forward-line 2)
          (insert "\n\n\n")
          (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'added 3 5)))
          (if (eq delete 'region)
              (delete-region (save-excursion (forward-line -3) (point)) (point))
            (dotimes (_ 3)
              (delete-char -1)))
          ;; Line 3 shows the sign of the old line 6, which it is now.
          (should (equal (git-gutter-test:sign-lines)
                         (mapcar (lambda (line) (list line "."))
                                 (number-sequence 1 10)))))))))

;; `git-gutter:view-diff-infos' skips the hunks and ranges of unchanged
;; lines whose signs are still right.
(defmacro git-gutter-test:counting-put-signs (&rest body)
  "Run BODY and return the positions `git-gutter:put-signs' got."
  (declare (indent 0))
  `(let ((points nil))
     (cl-letf* ((put (symbol-function 'git-gutter:put-signs))
                ((symbol-function 'git-gutter:put-signs)
                 (lambda (sign pts &optional wrap-sign)
                   (setq points (append points pts))
                   (funcall put sign pts wrap-sign))))
       ,@body)
     points))

(ert-deftest git-gutter:update-diffinfo-skips-unchanged-hunks ()
  "Only the hunk with an edit and the ranges beside it are drawn again."
  (git-gutter-test:with-lines
    (let ((hunks (list (git-gutter-test:hunk 'modified 3 3)
                       (git-gutter-test:hunk 'added 7 8))))
      (git-gutter:update-diffinfo hunks)
      (should (null (git-gutter-test:counting-put-signs
                      (git-gutter:update-diffinfo hunks))))
      ;; Edit line 7.
      (goto-char (point-min))
      (forward-line 6)
      (insert "x")
      (should (equal (mapcar #'line-number-at-pos
                             (git-gutter-test:counting-put-signs
                               (git-gutter:update-diffinfo hunks)))
                     '(7 8)))
      ;; A new line above moves the hunks down; their signs moved with the text.
      (goto-char (point-min))
      (insert "new\n")
      (should (equal (sort (mapcar #'line-number-at-pos
                                   (git-gutter-test:counting-put-signs
                                     (git-gutter:update-diffinfo
                                      (list (git-gutter-test:hunk 'added 1 1)
                                      (git-gutter-test:hunk 'modified 4 4)
                                            (git-gutter-test:hunk 'added 8 9)))))
                           #'<)
                     '(1 2 3)))
      (should (equal (git-gutter-test:sign-lines)
                     '((1 "+") (2 ".") (3 ".") (4 "=") (5 ".") (6 ".") (7 ".")
                       (8 "+") (9 "+") (10 ".") (11 ".")))))))

(ert-deftest git-gutter:update-diffinfo-without-edit-tracking ()
  "Without `git-gutter--after-change', every sign is drawn again."
  (git-gutter-test:with-lines
    (remove-hook 'after-change-functions #'git-gutter--after-change t)
    (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'modified 3 3)))
    (should (= 10 (length (git-gutter-test:counting-put-signs
                           (git-gutter:update-diffinfo
                            (list (git-gutter-test:hunk 'modified 3 3)))))))))

(ert-deftest git-gutter:update-diffinfo-overlays-in-no-group ()
  "The first update deletes sign overlays that belong to no group."
  (git-gutter-test:with-lines
    (let ((git-gutter:unchanged-sign nil))
      (git-gutter:put-signs "=" (list (point-min)))
      (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'added 3 3)))
      (should (equal (git-gutter-test:sign-lines) '((3 "+")))))))

(ert-deftest git-gutter:update-diffinfo-separator-change ()
  "Changing `git-gutter:separator-sign' draws the signs again."
  (git-gutter-test:with-lines
    (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'modified 3 3)))
    (let ((git-gutter:separator-sign "|"))
      (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'modified 3 3)))
      (should (equal (cadr (assq 3 (git-gutter-test:sign-lines))) "=|")))))

(defun git-gutter-test:sign-state ()
  "The sign overlays of the current buffer as comparable lists."
  (mapcar (lambda (ov)
            (list (overlay-start ov) (overlay-end ov)
                  (cadr (get-text-property 0 'display (overlay-get ov 'before-string)))
                  (overlay-get ov 'wrap-prefix)
                  (overlay-get ov 'priority)))
          (git-gutter-test:sign-overlays)))

(defun git-gutter-test:random-hunks (lines)
  "Random sorted, disjoint hunks in a buffer of LINES lines."
  (let ((line 1) hunks)
    (while (< line lines)
      (setq line (+ line (random 4)))
      (let* ((type (nth (random 3) '(added modified deleted)))
             (end (if (eq type 'deleted) line (min lines (+ line (random 3))))))
        (when (<= line lines)
          (push (git-gutter-test:hunk type line end) hunks))
        (setq line (+ end 1 (random 3)))))
    (nreverse hunks)))

(ert-deftest git-gutter:update-diffinfo-random-edits ()
  "After random edits, the signs are those drawn into a fresh buffer."
  (random "git-gutter")
  (dolist (visual '(nil t))
    (dolist (unchanged '(nil "."))
      (git-gutter-test:with-lines
        (let ((git-gutter:visual-line visual)
              (git-gutter:unchanged-sign unchanged)
              (hunks nil)
              (kept 0))
          (dotimes (_ 200)
            (dotimes (_ (random 3))
              (goto-char (1+ (random (buffer-size))))
              (pcase (random 4)
                (0 (insert "ab"))
                (1 (insert "\n"))
                (2 (insert "x\ny\n"))
                (3 (delete-region (point) (min (point-max) (+ (point) (random 6)))))))
            ;; Keep the hunks half of the time, so that groups are kept.
            (let ((hunks (if (and hunks (zerop (random 2)))
                             hunks
                           (git-gutter-test:random-hunks
                            (line-number-at-pos (point-max)))))
                  (text (buffer-string))
                  (groups git-gutter--groups))
              (git-gutter:update-diffinfo hunks)
              (cl-incf kept (cl-count-if (lambda (g) (memq g groups))
                                         git-gutter--groups))
              (let ((incremental (git-gutter-test:sign-state)))
                (with-temp-buffer
                  (insert text)
                  (git-gutter:update-diffinfo hunks)
                  ;; Compare the printed forms: before Emacs 29,
                  ;; `equal-including-properties' compares property
                  ;; values with `eq'.
                  (should (equal (prin1-to-string incremental)
                                 (prin1-to-string (git-gutter-test:sign-state))))))))
          ;; The test reached the code that keeps groups.
          (should (> kept 20)))))))

(ert-deftest git-gutter:joined-lines-leave-one-sign ()
  "Joining lines deletes the overlays that no longer start a line."
  (dolist (visual '(nil t))
    (git-gutter-test:with-lines
      (let ((git-gutter:visual-line visual)
            (git-gutter:unchanged-sign nil))
        (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'modified 3 6)))
        ;; Join lines 3 to 6 into one line, as an undo of splits would.
        (goto-char (point-min))
        (forward-line 2)
        (dotimes (_ 3)
          (end-of-line)
          (delete-char 1))
        (should (equal (git-gutter-test:sign-lines) '((3 "="))))
        (dolist (ov (git-gutter-test:sign-overlays))
          (goto-char (overlay-start ov))
          (should (bolp)))
        ;; The next update draws the joined line once.
        (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'modified 3 3)))
        (should (equal (git-gutter-test:sign-lines) '((3 "="))))))
    ;; Only the group's last overlay goes: join lines 5 and 6.
    (git-gutter-test:with-lines
      (let ((git-gutter:visual-line visual)
            (git-gutter:unchanged-sign nil))
        (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'modified 3 6)))
        (goto-char (point-min))
        (forward-line 4)
        (end-of-line)
        (delete-char 1)
        ;; Same start, sign and number of lines: the old group is asked
        ;; whether it was edited, with its last overlay deleted.
        (git-gutter:update-diffinfo (list (git-gutter-test:hunk 'modified 3 6)))
        (should (equal (git-gutter-test:sign-lines)
                       '((3 "=") (4 "=") (5 "=") (6 "="))))))))

(ert-deftest git-gutter:update-diffinfo-other-view-function ()
  "With another view function, the clear function runs first."
  (git-gutter-test:with-lines
    (let* ((calls nil)
           (git-gutter:clear-function (lambda () (push 'clear calls)))
           (git-gutter:view-diff-function (lambda (_) (push 'view calls))))
      (git-gutter:update-diffinfo nil)
      (should (equal calls '(view clear))))))

;; Staged signs.

(defun git-gutter-test:write-lines (file lines)
  "Write LINES, a list of strings, to FILE, one per line."
  (with-temp-file file
    (dolist (line lines) (insert line "\n"))))

(defun git-gutter-test:hunks-with-staged (setup)
  "Commit f.txt with lines 1..10, call SETUP, and return the hunks.
SETUP changes, stages and commits files in `default-directory'.  The
result lists (TYPE START-LINE END-LINE) for each hunk, with staged signs
on."
  (let (result)
    (with-temporary-directory
     (lambda ()
       (git-gutter-test:git "init" "-q")
       (git-gutter-test:write-lines "f.txt" (mapcar #'number-to-string (number-sequence 1 10)))
       (git-gutter-test:git "add" "f.txt")
       (git-gutter-test:git "commit" "-q" "-m" "init")
       (funcall setup)
       (let* ((git-gutter:staged-sign "*")
              (buf (find-file-noselect (expand-file-name "f.txt"))))
         (unwind-protect
             (with-current-buffer buf
               (git-gutter-mode 1)
               (with-timeout (10 (error "git-gutter did not finish"))
                 (while (not git-gutter:enabled)
                   (accept-process-output nil 0.1)))
               (setq result
                     (mapcar (lambda (h)
                               (list (git-gutter-hunk-type h)
                                     (git-gutter-hunk-start-line h)
                                     (git-gutter-hunk-end-line h)))
                             git-gutter:diffinfos)))
           (kill-buffer buf)))))
    result))

(ert-deftest git-gutter:staged-sign-with-unstaged-lines-above ()
  "A staged change keeps its line when unstaged lines are added above it."
  (should (equal (git-gutter-test:hunks-with-staged
                  (lambda ()
                    (git-gutter-test:write-lines
                     "f.txt" '("1" "2" "3" "4" "5" "6" "7" "EIGHT" "9" "10"))
                    (git-gutter-test:git "add" "f.txt")
                    (git-gutter-test:write-lines
                     "f.txt" '("new1" "new2" "new3"
                               "1" "2" "3" "4" "5" "6" "7" "EIGHT" "9" "10"))))
                 '((added 1 3) (staged 11 11)))))

(ert-deftest git-gutter:staged-sign-line-changed-again ()
  "A staged line that is changed again shows the unstaged sign only."
  (should (equal (git-gutter-test:hunks-with-staged
                  (lambda ()
                    (git-gutter-test:write-lines
                     "f.txt" '("1" "2" "3" "4" "FIVE" "6" "7" "8" "9" "10"))
                    (git-gutter-test:git "add" "f.txt")
                    (git-gutter-test:write-lines
                     "f.txt" '("1" "2" "3" "4" "five" "6" "7" "8" "9" "10"))))
                 '((modified 5 5)))))

(ert-deftest git-gutter:staged-sign-off-by-default ()
  "Without `git-gutter:staged-sign', a staged change shows no sign."
  (let (hunks)
    (with-temporary-directory
     (lambda ()
       (git-gutter-test:git "init" "-q")
       (git-gutter-test:write-lines "f.txt" '("1" "2" "3"))
       (git-gutter-test:git "add" "f.txt")
       (git-gutter-test:git "commit" "-q" "-m" "init")
       (git-gutter-test:write-lines "f.txt" '("1" "TWO" "3"))
       (git-gutter-test:git "add" "f.txt")
       (let ((buf (find-file-noselect (expand-file-name "f.txt"))))
         (unwind-protect
             (with-current-buffer buf
               (should-not git-gutter:staged-sign)
               (git-gutter-mode 1)
               (with-timeout (10 (error "git-gutter did not finish"))
                 (while (not git-gutter:enabled)
                   (accept-process-output nil 0.1)))
               (setq hunks git-gutter:diffinfos))
           (kill-buffer buf)))))
    (should-not hunks)))

(ert-deftest git-gutter:staged-hunks ()
  "Lines changed since HEAD but not unstaged become staged hunks."
  (let ((staged (git-gutter:staged-hunks
                 (list (make-git-gutter-hunk :type 'modified :content ""
                                             :start-line 5 :end-line 10))
                 (list (make-git-gutter-hunk :type 'modified :content ""
                                             :start-line 7 :end-line 8)))))
    (should (equal (mapcar (lambda (h)
                             (list (git-gutter-hunk-type h)
                                   (git-gutter-hunk-start-line h)
                                   (git-gutter-hunk-end-line h)))
                           staged)
                   '((staged 5 6) (staged 9 10))))))

(ert-deftest git-gutter:staged-hunk-commands ()
  "Hunk commands leave a staged hunk alone; statistics skip it."
  (with-temp-buffer
    (insert "1\n2\n3\n")
    (setq-local git-gutter:diffinfos
                (list (make-git-gutter-hunk :type 'staged :content "@@ -2 +2 @@\n-two\n+2"
                                            :start-line 2 :end-line 2)))
    (goto-char (point-min))
    (forward-line 1)
    (let ((git-gutter:ask-p nil)
          (called nil))
      (should (equal (git-gutter:query-action
                      "Revert" (lambda (_) (setq called t)) #'ignore)
                     "Hunk is already staged"))
      (should-not called))
    (should (equal (git-gutter:statistic) '(0 . 0)))))

(ert-deftest git-gutter:live-update-through-symlink ()
  "Live update works when the file's directory is reached through a symlink."
  (with-temporary-directory
   (lambda ()
     (make-directory "real/repo" t)
     (skip-unless (ignore-errors (make-symbolic-link "real" "link") t))
     (let ((default-directory (file-name-as-directory (expand-file-name "real/repo"))))
       (git-gutter-test:git "init" "-q")
       (git-gutter-test:write-lines "f.txt" '("1" "2" "3"))
       (git-gutter-test:git "add" "f.txt")
       (git-gutter-test:git "commit" "-q" "-m" "init"))
     (let* ((file (expand-file-name "link/repo/f.txt"))
            (buf (find-file-noselect file)))
       (unwind-protect
           (with-current-buffer buf
             (git-gutter-mode 1)
             (with-timeout (10 (error "git-gutter did not finish"))
               (while (not git-gutter:enabled)
                 (accept-process-output nil 0.1)))
             (goto-char (point-min))
             (delete-region (point) (line-end-position))
             (insert "ONE")
             (git-gutter:live-update)
             (with-timeout (10 (error "live update did not finish"))
               (while git-gutter--live-update-process
                 (accept-process-output nil 0.1)))
             (should (equal (mapcar #'git-gutter-hunk-start-line git-gutter:diffinfos)
                            '(1))))
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))))))

;; Live update reuses the repository root and the original version.

(defun git-gutter-test:live-update-and-wait ()
  "Change the first line, run a live update, and wait for its diff."
  (goto-char (point-min))
  (insert "x")
  (git-gutter:live-update)
  (with-timeout (10 (error "live update did not finish"))
    (while git-gutter--live-update-process
      (accept-process-output nil 0.1))))

(ert-deftest git-gutter:live-update-cache ()
  "Live update runs `git show' once, until a full update clears the cache."
  (git-gutter-test:with-file-in-repo
    (let ((shows 0))
      (cl-letf* ((process-file-orig (symbol-function 'process-file))
                 ((symbol-function 'process-file)
                  (lambda (program &rest args)
                    (when (and (equal program "git") (member "show" args))
                      (setq shows (1+ shows)))
                    (apply process-file-orig program args))))
        (git-gutter-test:live-update-and-wait)
        (git-gutter-test:live-update-and-wait)
        (should (= shows 1))
        (should (equal (mapcar #'git-gutter-hunk-start-line git-gutter:diffinfos) '(1)))
        (let ((original (cdr git-gutter:live-update-cache)))
          (should (file-exists-p original))
          (git-gutter)
          (should-not git-gutter:live-update-cache)
          (should-not (file-exists-p original)))
        (with-timeout (10 (error "git-gutter did not finish"))
          (while (not git-gutter:enabled)
            (accept-process-output nil 0.1)))
        (git-gutter-test:live-update-and-wait)
        (should (= shows 2)))
      (set-buffer-modified-p nil))))

(ert-deftest git-gutter:live-update-start-revision ()
  "With a start revision, live update compares the buffer with that revision."
  (git-gutter-test:with-file-in-repo
    (git-gutter-test:write-lines "f.txt" '("1" "2" "3" "4" "5"))
    (git-gutter-test:git "commit" "-q" "-a" "-m" "init")
    (git-gutter-test:write-lines "f.txt" '("1" "TWO" "3" "4" "5"))
    (git-gutter-test:git "commit" "-q" "-a" "-m" "second")
    (revert-buffer t t)
    (git-gutter-test:wait-for-full-update)
    (git-gutter:set-start-revision "HEAD^")
    (git-gutter-test:wait-for-full-update)
    (goto-char (point-min))
    (forward-line 3)
    (delete-region (point) (line-end-position))
    (insert "FOUR")
    (git-gutter:live-update)
    (with-timeout (10 (error "live update did not finish"))
      (while git-gutter--live-update-process
        (accept-process-output nil 0.1)))
    (should (equal (mapcar (lambda (h)
                             (list (git-gutter-hunk-type h)
                                   (git-gutter-hunk-start-line h)))
                           git-gutter:diffinfos)
                   '((modified 2) (modified 4))))
    (set-buffer-modified-p nil)))

(defun git-gutter-test:wait-for-full-update ()
  "Wait until the diff process of `git-gutter' has finished."
  (with-timeout (10 (error "git-gutter did not finish"))
    (while (get-buffer (git-gutter:diff-process-buffer (git-gutter:base-file)))
      (accept-process-output nil 0.1))))

(ert-deftest git-gutter:live-update-narrowed ()
  "Live update diffs the whole buffer, also when it is narrowed."
  (git-gutter-test:with-file-in-repo
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "1\n2\n3\n4\n5\n")
      (save-buffer))
    ;; `save-buffer' started a full update.  Its result would replace the
    ;; live update's if it came later.
    (git-gutter-test:wait-for-full-update)
    (git-gutter-test:git "commit" "-q" "-a" "-m" "five lines")
    (goto-char (point-min))
    (forward-line 2)
    (narrow-to-region (point) (progn (forward-line 2) (point)))
    (git-gutter-test:live-update-and-wait)
    (should (equal (mapcar (lambda (hunk)
                             (list (git-gutter-hunk-type hunk)
                                   (git-gutter-hunk-start-line hunk)
                                   (git-gutter-hunk-end-line hunk)))
                           git-gutter:diffinfos)
                   '((modified 3 3))))
    (widen)
    (should (equal (buffer-string) "1\n2\nx3\n4\n5\n"))
    (set-buffer-modified-p nil)))

(ert-deftest git-gutter:late-full-update-ignored ()
  "A full update that finishes after a later live update is ignored."
  (git-gutter-test:with-file-in-repo
    (cl-letf (((symbol-function 'git-gutter:start-diff-process1)
               ;; A diff that finishes after 1 s with a hunk for lines 1-2.
               (lambda (_file proc-buf)
                 (start-file-process
                  "git-gutter" proc-buf
                  (expand-file-name invocation-name invocation-directory)
                  "-Q" "--batch" "--eval"
                  "(progn (sleep-for 1) (princ \"@@ -1 +1,2 @@\\n\"))"))))
      (git-gutter)
      (git-gutter-test:live-update-and-wait)
      (git-gutter-test:wait-for-full-update))
    (should (equal (mapcar (lambda (hunk)
                             (list (git-gutter-hunk-type hunk)
                                   (git-gutter-hunk-start-line hunk)
                                   (git-gutter-hunk-end-line hunk)))
                           git-gutter:diffinfos)
                   '((modified 1 1))))
    (set-buffer-modified-p nil)))

(defun git-gutter-test:hunk-list ()
  "(TYPE START-LINE END-LINE) for each hunk in `git-gutter:diffinfos'."
  (mapcar (lambda (hunk)
            (list (git-gutter-hunk-type hunk)
                  (git-gutter-hunk-start-line hunk)
                  (git-gutter-hunk-end-line hunk)))
          git-gutter:diffinfos))

(defun git-gutter-test:wait-for-live-update ()
  "Wait until no live update is running or due."
  (with-timeout (10 (error "live update did not finish"))
    (while (or git-gutter--live-update-pending git-gutter--live-update-process)
      (accept-process-output nil 0.1))))

(ert-deftest git-gutter:live-update-one-temp-file ()
  "Live update writes the buffer to one file, and runs again if it was due."
  (let ((temporary-file-directory
         (file-name-as-directory (make-temp-file "git-gutter-tmp" t))))
    (unwind-protect
        (progn
          (git-gutter-test:with-file-in-repo
            (should-not git-gutter--live-update-file)
            (goto-char (point-min))
            (insert "x")
            (git-gutter:live-update)
            ;; The first diff is still running: this update is only noted.
            (goto-char (point-max))
            (insert "new\n")
            (git-gutter:live-update)
            (should git-gutter--live-update-pending)
            (git-gutter-test:wait-for-live-update)
            ;; The first diff alone gives ((modified 1 1)).
            (should (equal (git-gutter-test:hunk-list) '((modified 1 2))))
            (should (equal (directory-files temporary-file-directory t
                                            "\\`git-gutter-cur")
                           (list git-gutter--live-update-file)))
            (set-buffer-modified-p nil))
          ;; `git-gutter-test:with-file-in-repo' killed the buffer.
          (should-not (directory-files temporary-file-directory nil
                                       "\\`git-gutter-")))
      (delete-directory temporary-file-directory t))))

(ert-deftest git-gutter:kill-emacs-deletes-temp-files ()
  "`kill-emacs-hook' deletes the cached original and the buffer's copy."
  (git-gutter-test:with-file-in-repo
    (should (memq #'git-gutter--delete-temp-files kill-emacs-hook))
    (git-gutter-test:live-update-and-wait)
    (let ((original (cdr git-gutter:live-update-cache))
          (copy git-gutter--live-update-file))
      (should (file-exists-p original))
      (should (file-exists-p copy))
      (git-gutter--delete-temp-files)
      (should-not (file-exists-p original))
      (should-not (file-exists-p copy))
      (should-not git-gutter:live-update-cache)
      (should-not git-gutter--live-update-file))
    (set-buffer-modified-p nil)))

(ert-deftest git-gutter:change-major-mode-deletes-temp-files ()
  "A new major mode deletes live update's files before it kills the cache."
  (git-gutter-test:with-file-in-repo
    (git-gutter-test:live-update-and-wait)
    (let ((original (cdr git-gutter:live-update-cache))
          (copy git-gutter--live-update-file))
      (should (file-exists-p original))
      (should (file-exists-p copy))
      (fundamental-mode)
      (should-not (file-exists-p original))
      (should-not (file-exists-p copy)))
    (set-buffer-modified-p nil)))

(ert-deftest git-gutter:live-update-same-file-name ()
  "Live updates of two files with the same name do not wait for each other."
  (with-temporary-directory
   (lambda ()
     (git-gutter-test:git "init" "-q")
     (make-directory "a")
     (make-directory "b")
     (with-temp-file "a/f.txt" (insert "1\n"))
     (with-temp-file "b/f.txt" (insert "1\n"))
     (git-gutter-test:git "add" ".")
     (git-gutter-test:git "commit" "-q" "-m" "init")
     (let ((bufs (list (find-file-noselect (expand-file-name "a/f.txt"))
                       (find-file-noselect (expand-file-name "b/f.txt")))))
       (unwind-protect
           (progn
             (dolist (buf bufs)
               (with-current-buffer buf
                 (git-gutter-mode 1)
                 (with-timeout (10 (error "git-gutter did not finish"))
                   (while (not git-gutter:enabled)
                     (accept-process-output nil 0.1)))))
             (dolist (buf bufs)
               (with-current-buffer buf
                 (goto-char (point-min))
                 (insert "x")
                 ;; B's live update starts while A's diff still runs.
                 (git-gutter:live-update)
                 (should-not git-gutter--live-update-pending)))
             (dolist (buf bufs)
               (with-current-buffer buf
                 (git-gutter-test:wait-for-live-update)
                 (should (equal (git-gutter-test:hunk-list) '((modified 1 1))))
                 (set-buffer-modified-p nil))))
         (mapc #'kill-buffer bufs))))))

(ert-deftest git-gutter:killing-clone-keeps-full-update ()
  "Killing a clone does not stop the base buffer's full update."
  (git-gutter-test:with-file-in-repo
    (let ((clone (clone-indirect-buffer nil nil)))
      (goto-char (point-min))
      (insert "x")
      (save-buffer)
      ;; `save-buffer' started a full update.
      (should (get-buffer (git-gutter:diff-process-buffer (git-gutter:base-file))))
      (kill-buffer clone)
      (should (get-buffer (git-gutter:diff-process-buffer (git-gutter:base-file))))
      (git-gutter-test:wait-for-full-update)
      (should (equal (git-gutter-test:hunk-list) '((modified 1 1)))))))

(defmacro git-gutter-test:with-and-without-indirect-advice (&rest body)
  "Run BODY twice: with the advice on `make-indirect-buffer', and without.
Natively compiled callers of `make-indirect-buffer' may skip the advice."
  (declare (indent 0))
  `(dolist (advice '(t nil))
     (unless advice
       (advice-remove 'make-indirect-buffer #'git-gutter:make-indirect-buffer))
     (unwind-protect
         (progn ,@body)
       (advice-add 'make-indirect-buffer :around #'git-gutter:make-indirect-buffer))))

(ert-deftest git-gutter:clone-has-own-temp-files ()
  "Killing a clone does not delete the base buffer's live update files."
  (git-gutter-test:with-and-without-indirect-advice
    (git-gutter-test:with-file-in-repo
      (git-gutter-test:live-update-and-wait)
      (let ((original (cdr git-gutter:live-update-cache))
            (copy git-gutter--live-update-file))
        (kill-buffer (clone-indirect-buffer nil nil))
        (should (file-exists-p original))
        (should (file-exists-p copy)))
      (goto-char (point-max))
      (insert "new\n")
      (git-gutter:live-update)
      (git-gutter-test:wait-for-live-update)
      (should (equal (git-gutter-test:hunk-list) '((modified 1 2))))
      (set-buffer-modified-p nil))))

(ert-deftest git-gutter:clone-live-update-does-not-wait ()
  "A base buffer and its clone run their live updates at the same time.
Each has its own files."
  (git-gutter-test:with-and-without-indirect-advice
    (git-gutter-test:with-file-in-repo
      (git-gutter-test:live-update-and-wait)
      (let* ((base (current-buffer))
             (clone (clone-indirect-buffer nil nil)))
        (unwind-protect
            (progn
              (goto-char (point-min))
              (insert "x")
              (git-gutter:live-update)
              (with-current-buffer clone
                (setq git-gutter:enabled t
                      git-gutter:last-chars-modified-tick nil)
                (git-gutter:live-update)
                (should-not git-gutter--live-update-pending)
                ;; The diff may already have exited: test only that the
                ;; clone started its own.
                (should (processp git-gutter--live-update-process))
                (should-not (eq git-gutter--live-update-process
                                (buffer-local-value 'git-gutter--live-update-process
                                                    base))))
              (should-not (equal (buffer-local-value 'git-gutter--live-update-file clone)
                                 git-gutter--live-update-file))
              (should-not (equal (buffer-local-value 'git-gutter:live-update-cache clone)
                                 git-gutter:live-update-cache))
              (dolist (buf (list (current-buffer) clone))
                (with-current-buffer buf
                  (git-gutter-test:wait-for-live-update)
                  (should (equal (git-gutter-test:hunk-list) '((modified 1 1)))))))
          (kill-buffer clone)))
      (should (file-exists-p (cdr git-gutter:live-update-cache)))
      (set-buffer-modified-p nil))))

(ert-deftest git-gutter:update-diffinfo-random-edits-through-clone ()
  "Edits made in a clone do not leave wrong signs in the base buffer.
The base buffer does not see them in its `after-change-functions'."
  (random "git-gutter-clone")
  (git-gutter-test:with-and-without-indirect-advice
    (dolist (visual '(nil t))
      (git-gutter-test:with-lines
        (let ((git-gutter:visual-line visual)
              (git-gutter:unchanged-sign ".")
              (clone (clone-indirect-buffer nil nil))
              (hunks nil))
          (unwind-protect
              (dotimes (_ 100)
                (with-current-buffer clone
                  (dotimes (_ (random 3))
                    (goto-char (1+ (random (buffer-size))))
                    (pcase (random 4)
                      (0 (insert "ab"))
                      (1 (insert "\n"))
                      (2 (insert "x\ny\n"))
                      (3 (delete-region (point) (min (point-max) (+ (point) (random 6))))))))
                (setq hunks (if (and hunks (zerop (random 2)))
                                hunks
                              (git-gutter-test:random-hunks
                               (line-number-at-pos (point-max)))))
                (let ((text (buffer-string)))
                  (git-gutter:update-diffinfo hunks)
                  (let ((incremental (prin1-to-string (git-gutter-test:sign-state))))
                    (with-temp-buffer
                      (insert text)
                      (git-gutter:update-diffinfo hunks)
                      (should (equal incremental
                                     (prin1-to-string (git-gutter-test:sign-state))))))))
            (kill-buffer clone)))))))

(ert-deftest git-gutter:write-current-content-coding ()
  "The current content is written in `buffer-file-coding-system'."
  (let ((file (make-temp-file "git-gutter-test")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-coding-system 'iso-latin-1-unix)
          (insert "\u00e9\n\u6f22\n")
          (git-gutter:write-current-content file)
          (should (equal (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally file)
                           (buffer-string))
                         "\351\n \n")))
      (delete-file file))))

(ert-deftest git-gutter:live-update-cache-deleted-on-kill ()
  "Killing the buffer deletes the cached original version."
  (let (original)
    (git-gutter-test:with-file-in-repo
      (git-gutter-test:live-update-and-wait)
      (setq original (cdr git-gutter:live-update-cache))
      (should (file-exists-p original))
      (set-buffer-modified-p nil))
    (should-not (file-exists-p original))))

(ert-deftest git-gutter:live-update-keeps-signs-when-diff-fails ()
  "When diff fails, live update keeps the signs it has."
  (git-gutter-test:with-file-in-repo
    (git-gutter-test:live-update-and-wait)
    (should (equal (mapcar #'git-gutter-hunk-start-line git-gutter:diffinfos) '(1)))
    ;; Remove the original version behind the cache's back: diff exits 2.
    (delete-file (cdr git-gutter:live-update-cache))
    (git-gutter-test:live-update-and-wait)
    (should (equal (mapcar #'git-gutter-hunk-start-line git-gutter:diffinfos) '(1)))
    (set-buffer-modified-p nil)))

(ert-deftest git-gutter:live-update-interval ()
  "nil, 0 and negative values of `git-gutter:update-interval' mean off."
  (dolist (case '((nil . nil) (0 . nil) (-1 . nil) (0.1 . 0.1) (2 . 2)))
    (let ((git-gutter:update-interval (car case)))
      (should (equal (git-gutter:live-update-interval) (cdr case))))))

(ert-deftest git-gutter:update-interval-nil ()
  "With the default nil, the mode turns on and starts no timer."
  (should-not (default-value 'git-gutter:update-interval))
  (let ((git-gutter:update-interval nil)
        (git-gutter:update-timer nil))
    (git-gutter-test:with-file-in-repo
      (should git-gutter-mode)
      (should-not git-gutter:update-timer)
      (should-error (git-gutter:start-update-timer) :type 'user-error))))

(ert-deftest git-gutter:update-interval-watcher ()
  "Setting `git-gutter:update-interval' restarts or cancels the timer."
  (let ((old (default-value 'git-gutter:update-interval))
        (git-gutter:update-timer nil))
    (unwind-protect
        (progn
          ;; No buffer has the mode on: the mode starts the timer later.
          (setq git-gutter:update-interval 2)
          (should-not git-gutter:update-timer)
          (git-gutter-test:with-file-in-repo
            (let ((timer git-gutter:update-timer))
              (should (timerp timer))
              (should (= (float-time (timer--time timer)) 2))
              (setq git-gutter:update-interval 0.5)
              (should (timerp git-gutter:update-timer))
              (should (= (float-time (timer--time git-gutter:update-timer)) 0.5))
              (should-not (memq timer timer-idle-list))
              (setq timer git-gutter:update-timer)
              (setq git-gutter:update-interval nil)
              (should-not git-gutter:update-timer)
              (should-not (memq timer timer-idle-list))
              ;; A `let' binding leaves the timer alone.
              (let ((git-gutter:update-interval 1))
                (should-not git-gutter:update-timer)))))
      (when git-gutter:update-timer
        (cancel-timer git-gutter:update-timer))
      (setq git-gutter:update-timer nil)
      (setq git-gutter:update-interval old))))

(ert-deftest git-gutter:update-hooks-watcher ()
  "Setting `git-gutter:update-hooks' changes the hooks of enabled buffers."
  (let ((old (default-value 'git-gutter:update-hooks)))
    (unwind-protect
        (git-gutter-test:with-file-in-repo
          (should (memq 'git-gutter after-revert-hook))
          (setq git-gutter:update-hooks '(after-save-hook))
          (should (memq 'git-gutter after-save-hook))
          (should-not (memq 'git-gutter after-revert-hook))
          (setq git-gutter:update-hooks '(after-revert-hook))
          (should-not (memq 'git-gutter after-save-hook))
          (should (memq 'git-gutter after-revert-hook))
          ;; Turning the mode off removes the hooks that it added.
          (git-gutter-mode -1)
          (should-not (memq 'git-gutter after-revert-hook)))
      (setq git-gutter:update-hooks old))))

(ert-deftest git-gutter:statistic-matches-git ()
  "`git-gutter:statistic' counts the lines that `git diff --numstat' counts."
  (with-temporary-directory
   (lambda ()
     (git-gutter-test:git "init" "-q")
     (git-gutter-test:write-lines "f.txt" (mapcar #'number-to-string (number-sequence 1 10)))
     (git-gutter-test:git "add" "f.txt")
     (git-gutter-test:git "commit" "-q" "-m" "init")
     ;; Delete lines 2-4, change line 6, add two lines after line 8.
     (git-gutter-test:write-lines "f.txt" '("1" "5" "SIX" "7" "8" "new1" "new2" "9" "10"))
     (let* ((numstat (split-string (git-gutter-test:git "diff" "--numstat" "f.txt")))
            (expected (cons (string-to-number (nth 0 numstat))
                            (string-to-number (nth 1 numstat))))
            (buf (find-file-noselect (expand-file-name "f.txt"))))
       (should (equal expected '(3 . 4)))
       (unwind-protect
           (with-current-buffer buf
             (git-gutter-mode 1)
             (with-timeout (10 (error "git-gutter did not finish"))
               (while (not git-gutter:enabled)
                 (accept-process-output nil 0.1)))
             (should (equal (mapcar #'git-gutter-hunk-type git-gutter:diffinfos)
                            '(deleted modified added)))
             (should (equal (git-gutter:statistic) expected)))
         (kill-buffer buf))))))

;; jj backend.  These tests need the jj program and skip without it,
;; unless GIT_GUTTER_TEST_REQUIRE_JJ is set, as in the CI jobs that
;; install jj; then a missing jj fails the tests.

(defun git-gutter-test:require-jj ()
  "Skip the current test if jj is missing, or fail if CI requires jj."
  (unless (executable-find "jj")
    (when (getenv "GIT_GUTTER_TEST_REQUIRE_JJ")
      (error "GIT_GUTTER_TEST_REQUIRE_JJ is set, but jj is not installed"))
    (ert-skip "jj is not installed")))

(defmacro git-gutter-test:with-jj-repo (&rest body)
  "Run BODY in a new jj repository, with jj configured only by the test."
  (declare (indent 0))
  `(with-temporary-directory
    (lambda ()
      (let ((process-environment (copy-sequence process-environment)))
        (with-temp-file "jj-config.toml")
        (setenv "JJ_CONFIG" (expand-file-name "jj-config.toml"))
        (setenv "JJ_USER" "test")
        (setenv "JJ_EMAIL" "test@example.com")
        (make-directory "repo")
        (let ((default-directory (file-name-as-directory (expand-file-name "repo"))))
          (git-gutter-test:jj "git" "init" ".")
          ,@body)))))

(defun git-gutter-test:jj (&rest args)
  "Run jj with ARGS in `default-directory'; signal an error on failure."
  (with-temp-buffer
    (unless (zerop (apply #'process-file "jj" nil t nil args))
      (error "jj %S failed: %s" args (buffer-string)))
    (buffer-string)))

(defun git-gutter-test:jj-hunks (file &optional change-buffer)
  "Open FILE with the jj backend and return its hunks as (TYPE START-LINE).
With CHANGE-BUFFER, call it in the buffer and run a live update."
  (let* ((git-gutter:handled-backends '(jj))
         (buf (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buf
          (git-gutter-mode 1)
          (with-timeout (10 (error "git-gutter did not finish"))
            (while (not git-gutter:enabled)
              (accept-process-output nil 0.1)))
          (when change-buffer
            (funcall change-buffer)
            (git-gutter:live-update)
            (with-timeout (10 (error "live update did not finish"))
              (while git-gutter--live-update-process
                (accept-process-output nil 0.1))))
          (mapcar (lambda (h)
                    (list (git-gutter-hunk-type h) (git-gutter-hunk-start-line h)))
                  git-gutter:diffinfos))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))))

(ert-deftest git-gutter:jj-diff-saved-change ()
  "A saved change shows, also in a subdirectory file with parentheses."
  (git-gutter-test:require-jj)
  (git-gutter-test:with-jj-repo
    (make-directory "sub dir")
    (git-gutter-test:write-lines "sub dir/a (1).txt" '("1" "2" "3" "4" "5"))
    (git-gutter-test:jj "commit" "-m" "init")
    (git-gutter-test:write-lines "sub dir/a (1).txt" '("1" "2" "THREE" "4" "5"))
    (should (equal (git-gutter-test:jj-hunks (expand-file-name "sub dir/a (1).txt"))
                   '((modified 3))))))

(ert-deftest git-gutter:jj-start-revision ()
  "With a start revision, the file is compared with that revision."
  (git-gutter-test:require-jj)
  (git-gutter-test:with-jj-repo
    (git-gutter-test:write-lines "f.txt" '("1" "2" "3" "4" "5"))
    (git-gutter-test:jj "commit" "-m" "init")
    (git-gutter-test:write-lines "f.txt" '("1" "TWO" "3" "4" "5"))
    (git-gutter-test:jj "commit" "-m" "second")
    (git-gutter-test:write-lines "f.txt" '("1" "TWO" "3" "FOUR" "5"))
    (let ((git-gutter:start-revision "@--"))
      (should (equal (git-gutter-test:jj-hunks (expand-file-name "f.txt"))
                     '((modified 2) (modified 4)))))))

(ert-deftest git-gutter:jj-live-update ()
  "Live update shows an unsaved change."
  (git-gutter-test:require-jj)
  (git-gutter-test:with-jj-repo
    (make-directory "sub")
    (git-gutter-test:write-lines "sub/f.txt" '("1" "2" "3" "4" "5"))
    (git-gutter-test:jj "commit" "-m" "init")
    (should (equal (git-gutter-test:jj-hunks
                    (expand-file-name "sub/f.txt")
                    (lambda ()
                      (goto-char (point-min))
                      (delete-region (point) (line-end-position))
                      (insert "ONE")))
                   '((modified 1))))))

(ert-deftest git-gutter:jj-live-update-start-revision ()
  "With a start revision, live update compares the buffer with that revision."
  (git-gutter-test:require-jj)
  (git-gutter-test:with-jj-repo
    (git-gutter-test:write-lines "f.txt" '("1" "2" "3" "4" "5"))
    (git-gutter-test:jj "commit" "-m" "init")
    (git-gutter-test:write-lines "f.txt" '("1" "TWO" "3" "4" "5"))
    (git-gutter-test:jj "commit" "-m" "second")
    (let ((git-gutter:start-revision "@--"))
      (should (equal (git-gutter-test:jj-hunks
                      (expand-file-name "f.txt")
                      (lambda ()
                        (goto-char (point-min))
                        (forward-line 3)
                        (delete-region (point) (line-end-position))
                        (insert "FOUR")))
                     '((modified 2) (modified 4)))))))

;;; test-git-gutter.el end here
