;;; test-git-gutter.el ---

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL:
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

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'ert)
(require 'git-gutter)

(ert-deftest git-gutter:root-directory ()
  "helper function `git-gutter:root-directory'"
  (let ((expected (expand-file-name default-directory))
        (got (git-gutter:root-directory)))
    (should (string= expected got))))

(ert-deftest git-gutter:filepath-to-gitpath ()
  "helper function `git-gutter:filepath-to-gitpath'"
  (let* ((filename "test-git-gutter.el")
         (expected (file-name-nondirectory filename))
         (got (git-gutter:filepath-to-gitpath
               filename (git-gutter:root-directory))))
    (should (string= expected got))))

(ert-deftest git-gutter:repo-info ()
  "helper function `git-gutter:repo-info'"
  (flet ((buffer-file-name ()
          (concat default-directory "test-git-gutter.el")))
    (let ((repoinfo (git-gutter:repo-info))
          (cwd (expand-file-name default-directory)))
      (should (string= (git-gutter:repoinfo-root repoinfo) cwd))
      (should (string= (git-gutter:repoinfo-gitdir repoinfo)
                       (concat (expand-file-name cwd) ".git"))))))

(ert-deftest git-gutter:show-original-file-command ()
  "helper function `git-gutter:show-original-file-command"
  (let* ((repoinfo (make-git-gutter:repoinfo :root "/foo/" :gitdir "/foo/.git"))
         (got (git-gutter:show-original-content-command repoinfo "/foo/bar.txt"))
         (expected "git --git-dir=/foo/.git --work-tree=/foo/ show HEAD:bar.txt"))
    (should (string= got expected))))

;;; test-git-gutter.el end here
