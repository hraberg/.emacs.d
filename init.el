;;; init.el --- Håkan Råberg's Emacs configuration.

;;; Commentary:

;; This is my portable .emacs.d which uses use-package.
;; Tested with Emacs 24.3.1, 24.5.1 and 25.1.50.2 on Linux.

;;; Code:

(eval-when-compile (require 'cl))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      inhibit-splash-screen t
      use-dialog-box nil
      make-backup-files nil
      confirm-nonexistent-file-or-buffer nil
      ido-create-new-buffer 'always)

(load custom-file :noerror)

(column-number-mode)
(electric-pair-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)

(package-initialize)

(eval-and-compile
  (case emacs-major-version
    (24 (setq-default tramp-ssh-controlmaster-options
                      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"))
    (25 (require 'url-handlers)))

  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") :append))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose t)

(use-package autorevert
  :config
  (setq auto-revert-interval 1)
  (global-auto-revert-mode))

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

(use-package asm-mode
  :config (setq asm-comment-char ?#))

(use-package better-defaults
  :config (setq visible-bell nil))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (bind-keys :map projectile-mode-map
             ("s-D" . projectile-find-dir)
             ("s-P" . projectile-switch-project)
             ("s-F" . projectile-find-file)
             ("s-G" . projectile-grep)))

(use-package auto-complete
  :diminish auto-complete-mode
  :config
  (ac-config-default)
  (eval '(progn (ac-set-trigger-key "TAB")
                (ac-flyspell-workaround)))
  (setq ac-auto-start nil))

(use-package ac-capf
  :config (ac-capf-setup))

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

(eval-when-compile (defun te/maybe-expand-tag ()))

(use-package tagedit
  :defer t
  :diminish tagedit-mode
  :commands tagedit-mode
  :init
  (add-hook 'sgml-mode-hook 'tagedit-mode)
  :config
  (tagedit-add-paredit-like-keybindings)
  (tagedit-add-experimental-features))

(use-package scss-mode
  :defer t
  :config (setq scss-compile-at-save nil))

(use-package json-mode
  :defer t)

(use-package scheme
  :defer t
  :config
  (dolist (r7rs-macro '(when unless guard parameterize))
    (font-lock-add-keywords 'scheme-mode `((,(symbol-name r7rs-macro) . font-lock-keyword-face)))
    (put r7rs-macro 'scheme-indent-function 1))
  (font-lock-add-keywords 'scheme-mode '(("case-lambda" . font-lock-keyword-face)))
  (put 'dynamic-wind 'scheme-indent-function 0)
  (setq scheme-program-name "~/dev/akeem/akeem"))

(use-package cider
  :defer 1
  :config
  (setq cider-prompt-for-symbol nil
        cider-prompt-for-project-on-connect nil
        cider-repl-display-help-banner nil)
  (bind-keys :map cider-repl-mode-map
             ("C-c M-o" . cider-repl-clear-buffer)))

(use-package ac-cider
  :defer t
  :init
  (dolist (hook '(cider-mode-hook cider-repl-mode-hook))
    (add-hook hook 'ac-cider-setup))
  (dolist (mode '(cider-mode cider-repl-mode))
    (add-to-list 'ac-modes mode)))

(use-package etags)

(use-package arc-mode)

(use-package inf-clojure
  :config
  (dolist (minor-mode '(eldoc-mode paredit-mode ac-capf-setup))
    (add-hook 'inf-clojure-mode-hook minor-mode)
    (add-hook 'inf-clojure-minor-mode-hook minor-mode))

  (dolist (map (list inf-clojure-minor-mode-map
                     inf-clojure-mode-map))
    (bind-keys :map map
               ("M-." . inf-clojure-navigate)
               ("M-," . pop-tag-mark))))

(defun inf-clojure-connect (&optional port name)
  "Connect to socket repl at PORT using NAME for the buffer.

The REPL can be started like this:

  java -Dclojure.server.repl=\"{:port 5555 :accept clojure.core.server/repl}\" -jar clojure.jar"
  (interactive)
  (with-current-buffer
      (make-comint-in-buffer (or name "inf-clojure")
                             nil (cons "localhost" (or port 5555)))
    (inf-clojure-mode)
    (process-send-string (inf-clojure-proc)
                         "(try (require 'complete.core) (catch Exception _))\n")
    (setq inf-clojure-buffer (current-buffer))
    (pop-to-buffer-same-window (current-buffer))))

(defun inf-clojure-find-file (url)
  "Opens the URL in a buffer.  Based on cider-common.el."
  (cond ((string-match "^file:\\(.+\\)" url)
         (find-file-noselect (match-string 1 url)))

        ((string-match "^\\(jar\\):file:\\(.+\\)!/\\(.+\\)" url)
         (let* ((entry (match-string 3 url))
                (path  (match-string 2 url))
                (name  (format "%s:%s" path entry)))
           (or (find-buffer-visiting name)
               (progn
                 (find-file path)
                 (goto-char (point-min))
                 (search-forward (concat entry "\n"))
                 (forward-line -1)
                 (archive-extract)
                 (current-buffer)))))))

(defvar inf-clojure-navigate-command
  "(when-let [m (meta (resolve '%s))]
     (require 'clojure.java.io)
     (->> (-> (select-keys m [:file :line :column :name :ns])
              (update :ns (comp symbol str))
              (update :file (comp str (resolve 'clojure.java.io/resource))))
          (sort-by key)
          flatten))\n")

(defun inf-clojure-navigate ()
  "Try navigate to the symbol at point and open it up in the same window."
  (interactive)
  (make-local-variable 'thing-at-point-file-name-chars)
  (setq thing-at-point-file-name-chars "-[:alnum:]_.:/*<>")

  (let* ((symbol (or (thing-at-point 'filename) ""))
         (proc (inf-clojure-proc))
         (comint-filt (process-filter proc))
         (kept "")
         var-info)

    (when symbol
      (set-process-filter proc (lambda (_proc string)
                                 (setq kept (concat kept string))))
      (unwind-protect
          (let ((snippet (format inf-clojure-navigate-command symbol)))
            (process-send-string proc snippet)
            (while (and (not (string-match inf-clojure-prompt kept))
                        (accept-process-output proc 2)))
            (setq var-info (and (string-match "(.+)" kept) (match-string 0 kept))))
        (set-process-filter proc comint-filt))

      (when var-info
        (let* ((result (read var-info))
               (file (nth 3 result))
               (line (nth 5 result)))

          (if (and (stringp file) (numberp line))
              (progn
                (ring-insert find-tag-marker-ring (point-marker))
                (with-current-buffer (inf-clojure-find-file file)
                  (inf-clojure-minor-mode)
                  (goto-char (point-min))
                  (forward-line (1- line))
                  (pop-to-buffer-same-window (current-buffer)
                                             '((display-buffer-reuse-window display-buffer-same-window)))))

            (message (format "Don't know how to find '%s'" (symbol-name symbol)))))))))

(defun init/go-get (package)
  "Use `go get' to install Go package PACKAGE."
  (call-process "go" nil nil nil "get" package))

(use-package go-mode
  :defer t
  :init
  (dolist (package '("github.com/rogpeppe/godef"
                     "golang.org/x/tools/cmd/goimports"))
    (init/go-get package))
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
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-autocomplete
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
  :diminish idle-highlight-mode
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

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :config (smex-initialize))

(use-package dockerfile-mode)

(use-package el-get
  :config
  (let ((el-get-local-recipes (expand-file-name "el-get/recipes" user-emacs-directory)))
    (make-directory el-get-local-recipes :parents)
    (add-to-list 'el-get-recipe-path el-get-local-recipes)
    (el-get :sync)))

(load (expand-file-name "local-init.el" user-emacs-directory) :noerror)

(use-package zenburn-theme
  :config (load-theme 'zenburn :no-confirm))

(when (equal 'x window-system)
  (set-face-attribute 'default nil :height
                      (if (= 3200 (x-display-pixel-width)) 120 150)))
(set-face-attribute 'mode-line nil :box)
(set-face-attribute 'mode-line-inactive nil :box)
(set-face-attribute 'vertical-border nil :foreground "#444")
(set-face-attribute 'hl-sexp-face nil :background "#383838")

(fringe-mode '(4 . 0))
(when (fboundp 'toggle-frame-fullscreen)
  (toggle-frame-fullscreen))

(provide 'init)

;;; init.el ends here
