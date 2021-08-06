(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;;; STARTUP SPEED ENHANCEMENTS
(setq gc-cons-threshold (* 50 1000 1000))

;;; USE-PACKAGE

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;;; THEME/FONT
; doom-vibrant, doom-molokai, ample-theme, doom-tomorrow-night,
; doom-dark+, doom-acario-dark, doom-Iosvkem, doom-moonlight, doom-one-light
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-acario-dark t))
;(add-to-list 'default-frame-alist '(font . "Iosevka Sparkle"))
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))

;;; NAVIGATION / TIPS
;;;; AVY / ACE-WINDOW
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

;; Set default directory for fzf to ~/
(defun find-file-from-home ()
  (interactive)
  (let ((default-directory "~/"))
    (call-interactively 'fzf)))

(setq evil-want-keybinding nil)
;;;; TREEMACS SIDEBAR
(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-follow-mode 't
	treemacs-filewatch-mode 't
	treeview-display-in-side-window 't))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

;;;;; FORMATTING
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;; PROGRAMMING MODES
;;;; YAML
(use-package yaml-mode)

;;;; PYTHON
(setq python-shell-interpreter "python3")
(setq gud-pdb-command-name "python -m pdb")
(add-hook 'python-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 (concat "python3 " buffer-file-name))))

(use-package blacken
  :hook (python-mode . blacken-mode))

;;;;; PYTHON LSP
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . lsp)
  :defer t
  :config 
  (setq lsp-python-ms-python-executable-cmd python-shell-interpreter))

(use-package lsp-mode
  :ensure t
  :defer t
  :config

  (use-package lsp-ui
    :ensure t
    :defer t
    :config
    (setq lsp-ui-sideline-ignore-duplicate t)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)))

;;;; WEB DEV 
(use-package web-mode
  :mode ("\\.html$" . web-mode)
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
)

;;;; HTML PREVIEW MODE
;; Starts the `simple-httpd' server if it is not already running
;; Turns on `impatient-mode' for the current buffer."

(use-package impatient-mode
  :ensure t
  :defer t)

(defun my-html-mode-hook ()
  (unless (get-process "httpd")
    (message "starting httpd server...")
    (httpd-start))
  (impatient-mode))
(add-hook #'html-mode-hook #'my-html-mode-hook)

;;;; CSV
(use-package csv-mode)

;;;; WHICH KEY
(use-package which-key
  :diminish
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.05))

;;;; COMPANY
(use-package company
  :hook ((org-mode . company-mode)
	(c++-mode . company-mode)
	(python-mode . company-mode))
  :config
  (setq company-backends '(company-capf)))

;;;; HELM
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

;;; ORG
;;;; GENERAL
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
	org-agenda-files '("/home/natha/Documents/notes/")
	org-startup-indented t
	left-margin-width 2
	right-margin-width 2)

(setq org-roam-capture-templates
      '(("d" "default" plain
	 "%?"
         :if-new (file+head "test.org"
			    "#+TITLE: ${title}
#+AUTHOR: Nathan Mador-House
#+STARTUP: showall")
         :unnarrowed t)))

  (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 100)
  (set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono")
  (set-face-attribute 'variable-pitch nil :family "LiberationSerif-Bold")
  (set-face-attribute 'org-level-1 nil :height 170 :family "DejaVu Serif")
  (set-face-attribute 'org-level-2 nil :height 160 :family "DejaVu Serif")
  (set-face-attribute 'org-level-3 nil :height 150 :family "DejaVu Serif")
  (set-face-attribute 'org-level-4 nil :height 140 :family "DejaVu Serif")
  (set-face-attribute 'org-level-5 nil :height 130 :family "DejaVu Serif")
  (set-face-attribute 'org-level-6 nil :height 120 :family "DejaVu Serif")
  (set-face-attribute 'org-level-7 nil :height 120 :family "DejaVu Serif")
  (set-face-attribute 'org-level-8 nil :height 120 :family "DejaVu Serif")
  (set-face-attribute 'org-document-title nil :height 220 :family "DejaVu Serif")
  
  ; Makes code blocks and tables use fixed-pitch (mono spaced) font
  (defun my-adjoin-to-list-or-symbol (element list-or-symbol)
    (let ((list (if (not (listp list-or-symbol))
		    (list list-or-symbol)
                list-or-symbol)))
      (require 'cl-lib)
      (cl-adjoin element list)))

  (eval-after-load "org"
    '(mapc
      (lambda (face)
	(set-face-attribute
	 face nil
	 :inherit
	 (my-adjoin-to-list-or-symbol
	  'fixed-pitch
	  (face-attribute face :inherit))))
      (list 'org-code 'org-block 'org-table ))) 

  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  (org-babel-after-execute . org-redisplay-inline-images)
  (org-mode . (lambda () (define-key org-mode-map (kbd "C-j") nil))) ; unbind C-j and C-k so windmove works
  (org-mode . (lambda () (define-key org-mode-map (kbd "C-k") nil))))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-node-display-template "${title:50} ${tags:*}")
  (add-hook 'completion-at-point-functions 'org-roam-complete-at-point nil)
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "/home/natha/Documents/notes/"))

;;;;; ADDONS

(use-package org-noter
  :ensure t
  :defer t
  :config)

(use-package org-bullets
  :hook ((org-mode) . org-bullets-mode))

;;; CODE AESTHETICS
;;;; HTML RAINBOW COLOURS
(use-package rainbow-mode
  :hook ((html-mode css-mode xml-mode text-mode) . rainbow-mode))

;;;; TELEPHONE LINE (POWERLINE)
(use-package telephone-line ; Powerline status bar
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
	telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
	telephone-line-primary-right-separator 'telephone-line-cubed-right
	telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (telephone-line-mode 1))

;;;; HIGHLIGHT INDENT
(use-package highlight-indent-guides
  :hook
  ((prog-mode python-mode) . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;;;; PARENS
(use-package highlight-parentheses) ; Use this instead of show-paren-mode due to highlight font

;;;; RAINBOW DELIMITERS
(use-package rainbow-delimiters
  :hook ((prog-mode text-mode) . rainbow-delimiters-mode))

;;; EVIL
;;;; LEADER (needs to be before Evil)
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
    "ne" 'treemacs
    "s" 'avy-goto-char
    "b" 'helm-mini
    "r" 'org-roam-buffer-toggle
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
    "note" 'org-roam-node-find
    "time" 'org-time-stamp
    "i" 'org-roam-jump-to-index
    "cc" 'org-toggle-checkbox
    "cn" 'org-insert-todo-heading
    "obs" 'org-insert-structure-template
    "j" 'json-pretty-print-buffer
    "p" 'compile
    "log" 'org-roam-dailies-capture-today
    )
  )

;;;; GENERAL
(use-package evil
  :init
  (setq evil-want-integration t)
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

;;;; SURROUND
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;;;; EVIL-COLLECTION
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;;;; ESCAPE
(use-package evil-escape
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "nn")
  (setq evil-escape-inhibit-functions '(evil-visual-state-p))
  (setq-default evil-escape-delay 0.2)
  )

;;;; EVIL ORG
(use-package evil-org
  :hook ((org-mode-hook) . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  )

;;; OTHER PACKAGES

;;;; UNDO TREE
(use-package undo-tree
  :init
  (undo-tree-mode))
(global-undo-tree-mode)

;;;; CENTERED CURSOR
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

;;; EMACS SETTINGS
;;;; EXTRA KEYBINDINGS
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)

;;;; GENERAL SETTINGS
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

;(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;(global-display-line-numbers-mode 1)
(global-hl-line-mode t)
(set-default-coding-systems 'utf-8)
(setq compilation-ask-about-save nil)

;; Remember cursor location for files
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place-mode 1)
(require 'saveplace)

;; Remove svg files made from org-roam from the recent files list
(setq recentf-exclude '("\.svg$")) 

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; CUSTOM-SET-VARIABLES
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   (quote
    (company-lsp lsp-ui yaml-mode org-drill which-key use-package telephone-line rainbow-mode rainbow-delimiters powerline org-sticky-header org-bullets markdown-mode impatient-mode helm-org-rifle fzf evil-surround evil-org evil-leader evil-escape dashboard avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; POST STARTUP SPEED ENHANCEMENTS
(setq gc-cons-threshold (* 2 1000 1000))
