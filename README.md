# git-gutter.el

## Introduction
`git-gutter.el` is port of [GitGutter](https://github.com/jisaacks/GitGutter)
which is a plugin of Sublime Text2.


## Screenshot

### linum-mode style
![git-gutter.el](https://github.com/syohex/emacs-git-gutter/raw/master/image/git-gutter1.png)

### View git information in fringe
![git-gutter-fringe.el](https://github.com/syohex/emacs-git-gutter/raw/master/image/git-gutter-fringe.png)


## Requirements

* Emacs 24 or higher
* [fringe-helper](http://www.emacswiki.org/emacs/FringeHelper) for using `git-gutter-fringe`


## Basic Usage

`git-gutter.el` provides following commands.

Show changes from last commit

    M-x git-gutter

Clear changes

    M-x git-gutter:clear

Toggle git-gutter

    M-x git-gutter:toggle


## Sample Configuration

### linum style setting

```` elisp
(require 'git-gutter)

;; Update changes information after save buffer
(add-hook 'after-save-hook
          (lambda ()
            (when (zerop (call-process-shell-command "git rev-parse --show-toplevel"))
              (git-gutter))))
````

### fringe version setting

```` elisp
;; You need to install fringe-helper.el
(require 'git-gutter-fringe)
````

## Customize

You can change change signes and those faces in linum style.

```` elisp
(setq git-gutter:modified-sign "==")
(setq git-gutter:added-sign "++")
(setq git-gutter:deleted-sign "--")

(set-face-foreground 'git-gutter:modified "cyan")
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")
````


### Using full width characters

Emacs has `char-width` function which returns character width.
`git-gutter.el` uses it for calculating character length of the signs.
But `char-width` does not work for some full-width characters.
So you should explicitly specify window width, if you use full-width
character.

```` elisp
(setq git-gutter:window-width 2)
(setq git-gutter:modified-sign "☁")
(setq git-gutter:added-sign "☀")
(setq git-gutter:deleted-sign "☂")
````

### Screenshot of above customization
![git-gutter-fullwidth](https://github.com/syohex/emacs-git-gutter/raw/master/image/git-gutter-fullwidth.png)
