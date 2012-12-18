# git-gutter.el

## Introduction
`git-gutter.el` is port of [GitGutter](https://github.com/jisaacks/GitGutter)
which is a plugin of Sublime Text2.


## Screenshot

![git-gutter.el](https://github.com/syohex/emacs-git-gutter/raw/master/image/git-gutter1.png)


## Requirements

* Emacs 24 or higher


## Basic Usage

`git-gutter.el` provides following commands.

    M-x git-gutter


## Sample Configuration

```` elisp
(require 'git-gutter)

(add-hook 'after-save-hook
          (lambda ()
            (if (zerop (call-process-shell-command "git rev-parse --show-toplevel"))
                (git-gutter))))

````
