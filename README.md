# emacs-config
My emacs configuration

Following `.git` directories has been removed from sub repositories with the following commands:

    $ find . -mindepth 2 -type d -name .git -print0 | xargs -n1 -0 echo
    ./.emacs.d/powerline/.git
    ./.emacs.d/themes/idea-darkula-theme/.git
    ./.emacs.d/themes/tao-theme-emacs/.git
    ./.emacs.d/themes/foggy-night-theme/.git
    ./.emacs.d/multiple-cursors.el/.git
    ./.emacs.d/emacs-jedi/.git

    $ find . -mindepth 2 -type d -name .git -print0 | xargs -n1 -0 rm

## Installation

After clonig the repo, copy `.emacs` and `.emacs.d/` to `$HOME` folder.

## Quicklisp

Also, I'd advise to install SLIME through quicklisp-slime-helper

You would need to install some Lisp you like (let it be CCL for this purpose, as described above), then, in the same shell do this:

(Suppose you are on a Debian-like Linux)

    $ sudo apt-get install wget
    $ cd ~/Downloads
    $ wget http://beta.quicklisp.org/quicklisp.lisp
    $ sbcl --load ./quicklisp.lisp

wait until you see Lisp shell prompt,

    * (quicklisp-quickstart:install)
    * (ql:add-to-init-file)
    * (ql:quickload "quicklisp-slime-helper")
    * (quit)

now you are back in the regular shell. Launch Emacs, if not open yet. C-f x~/.emacs. Add the lines below to it (instead of what you posted above):

    ;; SLIME setup for CCL
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    (setq inferior-lisp-program "/home/kkielak/opt/ccl/lx86cl64")
    (slime-setup '(slime-fancy))

