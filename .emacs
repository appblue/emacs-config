(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#252525" "#D9D9D9" "#C2C2C2" "#F6F6F6" "#E8E8E8" "#D9D9D9" "#F0F0F0" "#F6F6F6"])
 '(custom-safe-themes
   (quote
    ("603a9c7f3ca3253cb68584cb26c408afcf4e674d7db86badcfe649dd3c538656" "02591317120fb1d02f8eb4ad48831823a7926113fa9ecfb5a59742420de206e0" "40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" default)))
 '(fci-rule-color "#161616")
 '(inhibit-startup-screen t)
 '(neo-theme (quote ascii))
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(vc-annotate-background "#0E0E0E")
 '(vc-annotate-color-map
   (quote
    ((20 . "#616161")
     (40 . "#9D9D9D")
     (60 . "#9D9D9D")
     (80 . "#C2C2C2")
     (100 . "#C2C2C2")
     (120 . "#D9D9D9")
     (140 . "#D9D9D9")
     (160 . "#E8E8E8")
     (180 . "#E8E8E8")
     (200 . "#E8E8E8")
     (220 . "#F0F0F0")
     (240 . "#F0F0F0")
     (260 . "#F0F0F0")
     (280 . "#F6F6F6")
     (300 . "#F6F6F6")
     (320 . "#F6F6F6")
     (340 . "#F9F9F9")
     (360 . "#F9F9F9"))))
 '(vc-annotate-very-old-color "#D9D9D9"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq visible-bell nil)
(menu-bar-mode -1)

;; Once this is enabled, you can make the text in a region
;; lowercase with C-x C-l or uppercase with C-x C-u.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; MELPA
(require 'package)

;; package archives
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; Clojure setup
(load "~/.emacs.d/init.el")

;; powerline support
(add-to-list 'load-path "~/.emacs.d/powerline/")
(require 'powerline)
(powerline-default-theme)

;; IDEA Dracula Theme !!!
(push (substitute-in-file-name "~/.emacs.d/themes/idea-darkula-theme/") custom-theme-load-path)
(load-theme 'idea-darkula t)

;; Tao Themes
;;(push (substitute-in-file-name "~/.emacs.d/themes/tao-theme-emacs/") custom-theme-load-path)
;;(add-to-list 'load-path "~/.emacs.d/themes/tao-theme-emacs/")
;; (load-theme 'tao-yin t)

;; Jedi support - TODO!!
;; (add-to-list 'load-path "~/.emacs.d/emacs-jedi/")
;; (setq jedi:server-command '("~/.emacs.d/emacs-jedi/PATH/TO/jediepcserver.py"))
;; (autoload 'jedi:setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'jedi:setup)

;; Multiple cursors
(add-to-list 'load-path "~/.emacs.d/multiple-cursors.el/")
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; Disable Scrollbars
(scroll-bar-mode -1)

;; Turn on auto-complete
(ac-config-default)

;; Switch to Fullscreen mode
(toggle-frame-fullscreen)

;; SLIME setup for CCL
;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;;(setq inferior-lisp-program "sbcl --noinform --no-linedit")
;;(slime-setup '(slime-fancy))
;; SLIME setup for SBCL 
(defun slime-sbcl ()
  (interactive)
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl --noinform --no-linedit")
  (slime-setup '(slime-fancy))
  (slime))

(defun slime-ccl ()
  (interactive)
  (load (expand-file-name "~/quicklisp-ccl/slime-helper.el"))
  (setq inferior-lisp-program "ccl")
  (slime-setup '(slime-fancy))
  (slime))

;; Hyperspec
(global-set-key [(f2)] 'slime-hyperspec-lookup) 
(setq common-lisp-hyperspec-root (expand-file-name "~/.emacs.d/HyperSpec/"))

;; on MacOSX extend the exec-path to include binaries installed via MacPorts
;; (add-to-list 'exec-path "/opt/local/bin")

(require 'w3m)
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
 ;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)

;; Use the opam installed utop
(setq utop-command "opam config exec -- utop -emacs")
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)

;; Haskell mode with stack util for REPL
(custom-set-variables '(haskell-process-type 'stack-ghci))
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
