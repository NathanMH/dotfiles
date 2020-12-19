(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;;;;; Speed Enhancements
(setq gc-cons-threshold (* 50 1000 1000))

;;; Theme/Font
; doom-vibrant, doom-molokai, ample-theme, doom-tomorrow-night,
; doom-dark+, doom-acario-dark, doom-Iosvkem, doom-moonlight
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-acario-dark t))
					;(add-to-list 'default-frame-alist '(font . "Iosevka Sparkle"))
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))

;;; Use-Package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;;; Navigation / Tips / General
;;;; Which Key
(use-package which-key
  ;;;; Which Key
  :diminish
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.05))

;;;; Company
(use-package company
  :hook((org-mode . company-mode)
	(c++-mode . company-mode)
	(python-mode . company-mode)))

(use-package company-org-roam
  :defer t
  :config
  (push 'company-org-roam company-backends))

;;;; Helm
(use-package helm
  :config
  (helm-mode)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-ff-skip-boring-files t)
  (setq helm-mini-default-sources '(helm-source-buffers-list
				    helm-source-recentf
				    helm-source-bookmarks))
  (setq helm-boring-buffer-regexp-list '("\\` " "\\*.+\\*"))

  (use-package helm-org-rifle
    :config
    (setq helm-org-rifle-show-path t)
  ))

;;;; Avy and Ace-Window
(use-package avy
  :config
  (setq avy-keys '(?t ?n ?s ?e ?f ?u ?d ?h ?r ?i))
  )

(use-package ace-window) ; Used only for ace-swap-window since built in is not good

;;;; FZF
(use-package fzf
  :init
  ;; (setenv "FZF_DEFAULT_COMMAND" "fd --type f --hidden")
  (setenv "FZF_DEFAULT_COMMAND" "rg --files --no-ignore --hidden --follow")
  (setq fzf/window-height 90))

					; Set default directory for fzf to ~/
(defun find-file-from-home ()
  (interactive)
  (let ((default-directory "~/"))
    (call-interactively 'fzf)))

(defun sidebar-ms ()
  (interactive)
  (dired-sidebar-toggle-sidebar "/mnt/c/Users/natha/Documents"))

;;;; Dired-Sidebar
(use-package dired-sidebar
  :ensure t
  :config
  (setq dired-sidebar-close-sidebar-on-file-open 't
	dired-sidebar-pop-to-sidebar-on-toggle-open 't)
  (setq dired-sidebar-one-instance-p 1) ; Only keep one buffer for sidebar
  (define-key dired-sidebar-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-sidebar-mode-map (kbd "^") 'dired-sidebar-up-directory)
  :commands (dired-sidebar-toggle-sidebar))

;;;; Outshine Folding (for init.el)
(use-package outshine
  ;; Easier navigation, headlines for source code files
  :bind (:map outshine-mode-map
	      ("<S-iso-lefttab>" . outshine-kbd-TAB)
	      )
  :hook (emacs-lisp-mode . outshine-mode)
  :config (setq outshine-cycle-emulate-tab t)
  )

;;; Programming Modes
;;;; Yaml
(use-package yaml-mode)

;;;; Python
;;;;; General
(setq python-shell-interpreter "python3")
(setq gud-pdb-command-name "python -m pdb")

;;;;; Formatting
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package blacken
  :hook (python-mode . blacken-mode))


;;;;; Python LSP
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . lsp)
  :defer t
  :config (setq lsp-python-ms-python-executable-cmd python-shell-interpreter)
  )

(use-package lsp-mode
  :ensure t
  :defer t
  :config

  (use-package lsp-ui
    :ensure t
    :defer t
    :config
    (setq lsp-ui-sideline-ignore-duplicate t)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))

  (use-package company-lsp
    :defer t
    :config
    (push 'company-capf company-backends))
  )

;;;; CSV
(use-package csv-mode)

;;; PDF

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0))))

(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

;;; Org
;;;; General
(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)))
  (setq org-want-todo-bindings t
	org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-src-window-setup 'current-window
	org-src-strip-leading-and-trailing-blank-lines t
	org-src-preserve-indentation t
	org-babel-python-command "python3"
	org-startup-with-inline-images t
	org-image-actual-width '(600) ; Allows for resizing inline images
	org-agenda-files '("/home/natha/Documents/notes/"))
  (setq org-roam-capture-templates
	'(("d" "default" plain (function org-roam--capture-get-point)
	   "%?"
	   :file-name "${slug}"
	   :head "#+TITLE: ${title}
#+ROAM_ALIAS: 
#+AUTHOR: Nathan Mador-House
#+STARTUP: showall
#+ROAM_TAGS: "
	   :unnarrowed t)))
  :hook
  (org-babel-after-execute . org-redisplay-inline-images)
  (org-mode . (lambda () (define-key org-mode-map (kbd "C-j") nil))) ; unbind C-j and C-k so windmove works
  (org-mode . (lambda () (define-key org-mode-map (kbd "C-k") nil)))
  )

;;;; Roam
(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "/home/natha/Documents/notes/"))

(use-package org-roam-server ; For visualizing note connections
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
	org-roam-server-port 8080
	org-roam-server-export-inline-images t
	org-roam-server-authenticate nil
	org-roam-server-label-truncate t
	org-roam-server-label-truncate-length 60
	org-roam-server-label-wrap-length 20))

(setq org-roam-dailies-capture-templates
      (quote (("t" "Table" table-line (function org-roam--capture-get-point)
               :file-name "journal/%(format-time-string \"%Y-%m-%d\" (current-time) t)"
               :head "#+TITLE: %(format-time-string \"%Y-%m-%d\" (current-time) t)\n#+ROAM_TAGS: journal
	       \n| screen time  | 0 |\n| sleep        | 0 |\n| alcohol      | 0 |\n| caffeine     | 0 |\n| exercise     | 0 |\n| stress level | 0 |\n| hydration    | 0 |\n| advil        | 0 |\n| anxiety meds | 0 |\n| outside      | 0 |\n| reading      | 0 |\n| shower       | 0 |\n\nNotes: "
	       ))))

;;;; Addons
(use-package interleave
  ;; For pdf annotations
  :config (setq interleave-disable-narrowing t)
  :defer t)

(use-package org-bullets
  :hook ((org-mode) . org-bullets-mode))

;;; Aesthetics
;;;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode text-mode) . rainbow-delimiters-mode))

;;;; HTML Rainbow Colours
(use-package rainbow-mode
  :hook ((html-mode css-mode xml-mode text-mode) . rainbow-mode))

;;;; Telephone Line (Powerline)
(use-package telephone-line ; Powerline status bar
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
	telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
	telephone-line-primary-right-separator 'telephone-line-cubed-right
	telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (telephone-line-mode 1))

;;;; Highlight Indent
(use-package highlight-indent-guides
  :hook
  ((prog-mode python-mode) . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;;; EVIL
;;;; General
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-fine-undo 'fine)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd ",O") 'show-all)
  (define-key evil-normal-state-map (kbd "H") "^")
  (define-key evil-normal-state-map (kbd "L") "$")
  (define-key evil-normal-state-map (kbd "RET") 'org-open-at-point)
  (define-key evil-normal-state-map (kbd "TAB") 'hs-toggle-hiding)

  (defun minibuffer-keyboard-quit ()  ;; Esc quits everything
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
	(setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (global-set-key [escape] 'evil-exit-emacs-state)
  )

;;;; Surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;;;; Evil-Collection
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;;;; Leader
(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "q" 'kill-this-buffer
    "k" 'kill-buffer-and-window
    "d" 'delete-window
    "tt" 'save-buffer
    "m" 'bookmark-set
    ;"ne" 'dired-sidebar-toggle-sidebar
    "ne" 'sidebar-ms
    "s" 'avy-goto-char
    "b" 'helm-mini
    "r" 'org-roam
    "a" 'org-todo-list
    "h" 'find-file-from-home
    "|" 'split-window-right ; Split window vertically
    "-" 'split-window-below ; Split window horizontally
    "[" 'toggle-window-split
    "]" 'ace-swap-window ; window-swap-state messes up pdf evil bindings so use ace-window instead
    "=" 'balance-windows
    "fl" 'font-lock-mode
    "tl" 'toggle-truncate-lines
    "full" 'toggle-frame-fullscreen
    "which" 'which-key-show-major-mode
    "term" 'term-other-window
    "comm" 'comment-line
    "note" 'org-roam-find-file
    "time" 'org-time-stamp
    "i" 'org-roam-jump-to-index
    "cc" 'org-toggle-checkbox
    "cn" 'org-insert-todo-heading
    "obs" 'org-insert-structure-template
    "j" 'json-pretty-print-buffer
    "p" 'compile
    "log" 'org-roam-dailies-find-today
    )
  )

;;;; Escape
(use-package evil-escape
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "nn")
  (setq evil-escape-inhibit-functions '(evil-visual-state-p))
  (setq-default evil-escape-delay 0.2)
  )

;;;; Evil Org
(use-package evil-org
  :hook ((org-mode-hook) . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  )

;;; Custom Modes
;;;; Centered Cursor
(define-minor-mode centered-point-mode
  "Always center the cursor in the middle of the screen."
  :lighter "..."
  (cond (centered-point-mode (add-hook 'post-command-hook 'line-change))
	(t (remove-hook 'post-command-hook 'line-change)))
  )
(defun line-change ()
  (recenter)
  )
;;(centered-point-mode 1)

;;;; Web Dev 
(use-package web-mode
  :mode ("\\.html$" . web-mode)
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq js-indent-level 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t)
  (add-hook 'web-mode-hook 'electric-pair-mode))

;;;; HTML Preview Mode
;; Starts the `simple-httpd' server if it is not already running
;; Turns on `impatient-mode' for the current buffer."
(defun my-html-mode-hook ()
  (unless (get-process "httpd")
    (message "starting httpd server...")
    (httpd-start))
  (impatient-mode))
(add-hook #'html-mode-hook #'my-html-mode-hook)

;;; Emacs Settings
;;;; Extra Keybindings
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)

;;;; Parens
(use-package highlight-parentheses) ; Use this instead of show-paren-mode due to highlight font

;;;; Undo/Redo compat with EVIL
(global-undo-tree-mode 1)
(use-package undo-tree
  :init
  (undo-tree-mode))

;;;; General Settings
(add-hook 'prog-mode-hook 'hs-minor-mode)
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-attribute 'show-paren-match nil :foreground "black" :background "grey")
(blink-cursor-mode 1)
(electric-pair-mode 1)
(menu-bar-mode -1) 
(toggle-scroll-bar -1) 
(tool-bar-mode -1) 
(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq-default explicit-shell-file-name "/bin/bash")
(setq-default shell-file-name "/bin/bash")
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(global-hl-line-mode t)
(set-default-coding-systems 'utf-8)
(setq compilation-ask-about-save nil)

;; Remember cursor location for files
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; Remove svg files made from org-roam from the recent files list
(setq recentf-exclude '("\.svg$")) 

;; No backups please
(setq create-lockfiles nil)
(setq make-backup-files nil)


;;; Custom-set-variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   (quote
    (yaml-mode org-drill which-key use-package telephone-line rainbow-mode rainbow-delimiters powerline org-sticky-header org-bullets markdown-mode impatient-mode helm-org-rifle fzf evil-surround evil-org evil-leader evil-escape dashboard avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Post speedup enhancements
(setq gc-cons-threshold (* 2 1000 1000))
