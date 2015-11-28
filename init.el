;;; init.el --- Håkan Råberg's Emacs configuration.

;;; Commentary:

;; This is my portable .emacs.d which uses use-package.
;; Tested with Emacs 24.5.1 on Linux.

;;; Code:

(eval-when-compile (require 'cl))

(setq custom-file "~/.emacs.d/custom.el"
      inhibit-splash-screen t
      use-dialog-box nil
      make-backup-files nil
      confirm-nonexistent-file-or-buffer nil
      ido-create-new-buffer 'always)

(load custom-file :noerror)

(column-number-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)

(package-initialize)

(eval-and-compile
  (case emacs-major-version
    (24 (setq-default tramp-ssh-controlmaster-options
                      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"))
    (25 (require 'url-handlers)))

  (dolist (archive '(("melpa" . "https://melpa.org/packages/")
                     ("melpa-stable" . "https://stable.melpa.org/packages/")))
    (add-to-list 'package-archives archive :append)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose t)

(use-package autorevert
  :config
  (global-auto-revert-mode)
  (setq auto-revert-interval 1))

(use-package cua-base
  :config (cua-mode))

(use-package delsel
  :config (delete-selection-mode))

(use-package eldoc
  :diminish eldoc-mode
  :config (add-hook 'prog-mode-hook 'eldoc-mode))

(use-package flyspell
  :diminish flyspell-mode
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package better-defaults
  :config (setq visible-bell nil))

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 2.0)
  (which-key-mode))

(use-package paredit
  :diminish paredit-mode
  :config
  (dolist (hook '(clojure-mode-hook
                  cider-repl-mode-hook
                  emacs-lisp-mode-hook
                  lisp-interaction-mode-hook
                  lisp-mode-hook
                  scheme-mode-hook
                  ielm-mode-hook
                  css-mode
                  js-mode
                  json-mode-hook))
    (add-hook hook 'enable-paredit-mode)))

(use-package paredit-everywhere
  :diminish paredit-everywhere-mode
  :config (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

(use-package tagedit
  :diminish tagedit-mode
  :init
  (eval-when-compile
    (defun te/maybe-expand-tag ()))
  :config
  (tagedit-add-paredit-like-keybindings)
  (tagedit-add-experimental-features)
  (add-hook 'sgml-mode-hook 'tagedit-mode))

(use-package company
  :diminish company-mode
  :bind (("TAB" . company-indent-or-complete-common))
  :config (global-company-mode))

(use-package json-mode
  :defer t)

(use-package cider
  :pin melpa-stable
  :defer 1
  :init
  (eval-when-compile
    (defun org-bookmark-jump-unhide ()))
  :config (setq cider-prompt-for-symbol nil))

(defun init/go-get (package)
  "Use `go get' to install Go package PACKAGE."
  (apply 'call-process-shell-command (list "go" nil nil nil "get" package)))

(use-package go-mode
  :defer t
  :init
  (init/go-get "github.com/rogpeppe/godef")
  (init/go-get "golang.org/x/tools/cmd/goimports")
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (bind-keys :map go-mode-map
             ("M-." . godef-jump)
             ("M-," . pop-tag-mark)))

(use-package gotest
  :defer t
  :config
  (bind-keys :map go-test-mode-map
             ("C-c ," . go-test-current-file)
             ("C-c C-," . go-test-current-file)))

(use-package go-eldoc
  :defer t
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package company-go
  :defer t
  :init (init/go-get "github.com/nsf/gocode"))

(use-package markdown-mode
  :mode ("\\.markdown\\'"
         "\\.md\\'"
         ("README\\.md\\'" . gfm-mode)))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package elisp-slime-nav
  :when (= 24 emacs-major-version)
  :diminish elisp-slime-nav-mode
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-'" . er/expand-region)
         ("C-M-'" . er/contract-region)))

(use-package idle-highlight-mode
  :config (add-hook 'prog-mode-hook 'idle-highlight-mode))

(use-package hl-sexp
  :config (global-hl-sexp-mode))

(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :config
  (setq hl-paren-colors
        '("orange1" "yellow1" "greenyellow" "green1"
          "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

(use-package browse-kill-ring
  :config (browse-kill-ring-default-keybindings))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :config (smex-initialize))

(use-package zenburn-theme
  :config (load-theme 'zenburn :no-confirm))

(set-face-attribute 'default nil :height 150)
(toggle-frame-fullscreen)

(provide 'init)

;;; init.el ends here
