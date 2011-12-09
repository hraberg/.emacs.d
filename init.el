(let* ((my-lisp-dir "~/.emacs.d/")
        (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))

(setq frame-title-format "%b")
(column-number-mode t)
(setq inhibit-splash-screen t)

(menu-bar-mode -1)

(when (boundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(if window-system
    (progn
      (set-frame-height (selected-frame) 43)
      (set-frame-width (selected-frame) 132)))

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)

(transient-mark-mode t)
(delete-selection-mode 1)
(setq x-select-enable-clipboard t)
(savehist-mode t)
(global-auto-revert-mode t)
(winner-mode 1)
;; (desktop-save-mode 1)

(require 'layout-restore)
(global-set-key [?\C-c ?l] 'layout-save-current)
(global-set-key [?\C-c ?\C-l ?\C-l] 'layout-restore)
(global-set-key [?\C-c ?\C-l ?\C-c] 'layout-delete-current)
(setq layout-restore-after-killbuffer nil)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-q") 'delete-other-windows)
(global-set-key (kbd "C-?") 'comment-or-uncomment-region)
(global-set-key (kbd "C-<f11>") 'slime)

(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "<M-C-left>") 'previous-buffer)
(global-set-key (kbd "<M-C-right>") 'next-buffer)

(windmove-default-keybindings 'meta)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "C-x f") 'find-file-in-project)

(cua-mode)
(ido-mode)
(setq ido-enable-flex-matching t)
;; (global-set-key
;;  "\M-x"
;;  (lambda ()
;;    (interactive)
;;    (call-interactively
;;     (intern
;;      (ido-completing-read
;;       "M-x "
;;       (all-completions "" obarray 'commandp))))))
;; (dolist (hook '(text-mode-hook markdown-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))
;; (dolist (hook '(emacs-lisp-mode-hook clojure-mode-hook))
;;   (add-hook hook (lambda () (flyspell-prog-mode 1))))
(require 'tramp)

(require 'recentf)
(recentf-mode)
(setq recentf-max-saved-items 50)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(require 'magit)
(require 'projectile)
(projectile-global-mode)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(require 'zenburn)
(zenburn)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;; Adapted from https://github.com/purcell/emacs.d/blob/master/init-auto-complete.el
(setq tab-always-indent 'complete)  ;; use 'complete when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)

;; hook AC into completion-at-point
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(global-auto-complete-mode t)

;; Adapted from https://github.com/scottjad/dotfiles/blob/master/.emacs
(defun jsj-ac-show-help ()
  "show docs for symbol at point or at beginning of list if not on a symbol"
  (interactive)
  (let ((s (save-excursion
             (or (symbol-at-point)
                 (progn (backward-up-list)
                        (forward-char)
                        (symbol-at-point))))))
    (popup-tip (if (or (equal major-mode 'emacs-lisp-mode)
                       (equal major-mode 'lisp-interaction-mode))
                   (ac-symbol-documentation s)
                 (ac-slime-documentation (symbol-name s)))
               :point (point)
               :around t
               :scroll-bar t
               :margin t)))

(define-key lisp-mode-shared-map (kbd "C-c C-q") 'jsj-ac-show-help)

(require 'hl-sexp)
(global-hl-sexp-mode)
(set-face-attribute 'hl-sexp-face nil :background "#181818")
(show-paren-mode)

(add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
(add-hook 'slime-repl-mode-hook 'highlight-parentheses-mode)
(add-hook 'emacs-lisp-mode-hook  'highlight-parentheses-mode)
(setq hl-paren-colors
      '("orange1" "yellow1" "greenyellow" "green1"
        "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))

(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

(global-set-key [pause] 'toggle-current-window-dedication)

(set-face-attribute 'default (selected-frame) :height 120)
