(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(
    use-package
    helm
    company
    magit
    mic-paren
    jetbrains-darcula-theme
    gruber-darker-theme
    
    sly
;;    slime

    ;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; merlin
    ;; tuareg
    ;; utop

    ;; Standard ML
    sml-mode

    ;; Support for SQLFormatter
    sqlformat

    ;; Haskell
    haskell-mode

    ;; W3M
    w3m
    ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Do not show the startup screen.
(setq inhibit-startup-message t)

;; Disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight current line.
(global-hl-line-mode t)

(setq make-backup-files nil) ; stop creating ~ files

;; Do not use `init.el` for `custom-*` code - use `custom-file.el`.
(setq custom-file "~/.emacs.d/custom-file.el")

;; Assuming that the code in custom-file is execute before the code
;; ahead of this line is not a safe assumption. So load this file
;; proactively.
(load-file custom-file)

(require 'use-package)
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-<f5>" . helm-find-files)
         ([f10] . helm-buffers-list)
         ([S-f10] . helm-recentf)))

(use-package company
  :bind (:map company-active-map
         ("C-n"       . company-select-next)
         ("C-p"       . company-select-previous)
	 ([tab]       . company-complete-selection))
  :config
  (setq company-idle-delay nil)
  (global-company-mode t))

(global-set-key (kbd "<backtab>") 'company-complete)
(global-set-key (kbd "<C-return>") 'other-window)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; load both color themes
;; -----------------------------------------
(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

(use-package jetbrains-darcula-theme
  :config
  (load-theme 'jetbrains-darcula t))

(defvar current-theme 'jetbrains-darcula)
(defun switch-themes ()
  (interactive)
  (if (eq current-theme 'jetbrains-darcula)
      (progn
	(setq current-theme 'gruber-darker)
	(load-theme 'gruber-darker t))
    (setq current-theme 'jetbrains-darcula)
    (load-theme 'jetbrains-darcula t)))

(global-set-key (kbd "<f9>") 'switch-themes)
;; -----------------------------------------

;; Better handling of paranthesis when writing Lisps.
(use-package paredit
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  ;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  :config
  (show-paren-mode t)
  :bind (("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly))
  :diminish nil)

(use-package haskell-mode
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package mic-paren)

(use-package sly
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(use-package sqlformat
  :init
  ;; pg_format tool needs to in the path
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g")))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'ocaml-ts-mode)
(add-hook 'ocaml-ts-mode-hook
          (lambda ()
            (eglot-ensure)
            ;; (company-mode)
            ;; (flyspell-prog-mode)
            (local-set-key (kbd "C-<tab>") 'company-complete)
            (add-hook 'before-save-hook 'ocaml-ts-mode-ocamlformat-before-save)
            (setq indent-tabs-mode nil)
            (setq truncate-lines t)
            (setq whitespace-line-column 100)
            (setq whitespace-style '(face trailing lines-tail))
            (whitespace-mode)))

(add-hook 'ocamli-ts-mode-hook
          (lambda ()
            (eglot-ensure)
            ;; (company-mode)
            ;; (flyspell-prog-mode)
            (local-set-key (kbd "C-<tab>") 'company-complete)
            (add-hook 'before-save-hook 'ocaml-ts-mode-ocamlformat-before-save)
            (setq indent-tabs-mode nil)
            (setq truncate-lines t)
            (setq whitespace-line-column 100)
            (setq whitespace-style '(face trailing lines-tail))
            (whitespace-mode)))

(setq treesit-language-source-alist
      '((astro "https://github.com/virchau13/tree-sitter-astro")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "grammars/ocaml/src")
        (ocaml-interface "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "grammars/interface/src")
        (ocaml-type "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "grammars/type/src")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (css-mode . css-ts-mode)
        (go-mode . go-ts-mode)
        (js2-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (tuareg-mode . ocaml-ts-mode)))

(use-package eglot
  :bind (:map eglot-mode-map
              ("s-l a" . eglot-code-actions)
              ("s-l c" . flymake-start)
              ("s-l er" . eglot-reconnect)
              ("s-l fd" . eglot-find-declaration)
              ("s-l fi" . eglot-find-implementation)
              ("s-l fr" . xref-find-references)
              ("s-l ft" . eglot-find-typeDefinition)
              ("s-l h" . eldoc)
              ("s-l n" . flymake-goto-next-error)
              ("s-l o" . eglot-code-action-organize-imports)
              ("s-l r" . eglot-rename)
              ("s-l lr" . imenu-list-refresh)
              ("s-l ll" . imenu-list)
              ("s-l lc" . consult-imenu))
  :config
  (add-to-list 'eglot-server-programs
               '(tuareg-mode . ("ocamllsp" "--fallback-read-dot-merlin")))
  (add-to-list 'eglot-server-programs
               '(ocaml-ts-mode . ("ocamllsp" "--fallback-read-dot-merlin")))
  (add-to-list 'eglot-server-programs
               '(ocamli-ts-mode . ("ocamllsp" "--fallback-read-dot-merlin"))))


(setq treesit-language-source-alist
      '((astro "https://github.com/virchau13/tree-sitter-astro")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "grammars/ocaml/src")
        (ocaml-interface "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "grammars/interface/src")
        (ocaml-type "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "grammars/type/src")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (css-mode . css-ts-mode)
        (go-mode . go-ts-mode)
        (js2-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (tuareg-mode . ocaml-ts-mode)))

;; OCAML TREESITTER CONFI - execute manually once
;; (treesit-install-language-grammar 'ocaml)
;; (treesit-install-language-grammar 'ocaml-interface)
;; (treesit-install-language-grammar 'ocaml-type)

;; slime option
;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
;; (setq inferior-lisp-program "/usr/bin/sbcl")

;; turn off visual and sound bell notifications
(setq ring-bell-function 'ignore)

; set fonts
(ignore-errors
    (set-frame-font "Hurmit Nerd Font Mono 13" nil t))

;; Ocaml setup
;; (setq utop-command "opam config exec -- utop -emacs")
;; (setq utop-command "opam config exec -- dune utop . -- -emacs")

;; (autoload 'utop-minor-mod "utop" "Minor mode for utop" t)
;; (add-hook 'tuareg-mode-hook 'utop-minor-mode)

;; Merlin setup
;; (let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
;;   (when (and opam-share (file-directory-p opam-share))
;;     ;; Register Merlin
;;     (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
;;     (autoload 'merlin-mode "merlin" nil t nil)
;;     ;; Automatically start it in OCaml buffers
;;     (add-hook 'tuareg-mode-hook 'merlin-mode t)
;;     (add-hook 'caml-mode-hook 'merlin-mode t)
;;     ;; Use opam switch to lookup ocamlmerlin binary
;;     (setq merlin-command 'opam)))

;; Make company aware of merlin
;; (with-eval-after-load 'company
;;  (add-to-list 'company-backends 'merlin-company-backend))

;; make and relaod utop
;; (TODO: issue with TAB completion after reload)
(defun make-and-reload ()
  (interactive)
  (let ((cbuff (current-buffer))
	(makefile-path (locate-dominating-file "." "Makefile")))
    (if makefile-path
	(progn
	  (compile (concat "cd " makefile-path " && make -k"))
	  (utop-kill)

	  ;; restart utop
	  (let ((buf (get-buffer-create utop-buffer-name))
		(cmd utop-command))
	    (pop-to-buffer buf)
	    (setq utop-command cmd)
	    (with-current-buffer buf (utop-mode))))
      (message "No Makefile found"))))

(global-set-key (kbd "C-c C-v") 'make-and-reload)

;; Hyperspec
;; (global-set-key [(f2)] 'slime-hyperspec-lookup)
(global-set-key [(f2)] 'sly-hyperspec-lookup) 
(setq common-lisp-hyperspec-root (expand-file-name "~/.emacs.d/HyperSpec/"))

(require 'w3m)
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
 ;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)

;; remap Home and End keys
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "<home>") 'beginning-of-line)
