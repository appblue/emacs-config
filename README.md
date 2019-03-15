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
