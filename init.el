(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'cl)
  (require 'use-package)
  (require 'url-handlers))
(require 'diminish)
(require 'bind-key)

(use-package flyspell
  :diminish flyspell-mode
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package better-defaults
  :ensure t
  :config
  (setq inhibit-splash-screen t
        column-number-mode t
        global-font-lock-mode t
        visible-bell nil)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (prefer-coding-system 'utf-8)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (global-auto-revert-mode)
  (cua-mode))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'json-mode-hook 'enable-paredit-mode))

(use-package company
  :ensure t
  :diminish company-mode
  :config (global-company-mode))

(use-package cider
  :ensure t
  :defer 1
  :pin melpa-stable
  :init (setq cider-prompt-for-symbol nil))

(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'"
         "\\.md\\'"
         ("README\\.md\\'" . gfm-mode)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C-M-=" . er/contract-region)))

(use-package hl-sexp
  :ensure t
  :config (global-hl-sexp-mode))

(use-package browse-kill-ring
  :ensure t
  :config (browse-kill-ring-default-keybindings))

(use-package smex
  :ensure t
  :config (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

(set-face-attribute 'default nil :height 150)
(toggle-frame-fullscreen)
