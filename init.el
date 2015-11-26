(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

(package-initialize)

(dolist (archive '(("melpa" . "https://melpa.org/packages/")
                   ("melpa-stable" . "https://stable.melpa.org/packages/")))
  (add-to-list 'package-archives archive :append))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'cl)
  (require 'use-package)
  (setq use-package-always-ensure t))
(require 'diminish)
(require 'bind-key)

(use-package flyspell
  :diminish flyspell-mode
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package better-defaults
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
  :diminish paredit-mode
  :config
  (dolist (hook '(clojure-mode-hook
                  cider-repl-mode-hook
                  lisp-mode-hook
                  emacs-lisp-mode-hook
                  lisp-interaction-mode-hook
                  json-mode-hook))
    (add-hook hook 'enable-paredit-mode)))

(use-package company
  :diminish company-mode
  :bind (("TAB" . company-indent-or-complete-common))
  :config (global-company-mode))

(use-package cider
  :defer 1
  :pin melpa-stable
  :init (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  :config (setq cider-prompt-for-symbol nil))

(use-package elisp-slime-nav
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode)))

(use-package markdown-mode
  :mode ("\\.markdown\\'"
         "\\.md\\'"
         ("README\\.md\\'" . gfm-mode)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-M-=" . er/contract-region)))

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
