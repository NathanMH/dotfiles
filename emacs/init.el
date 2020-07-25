(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

					; Paths
(add-to-list 'custom-theme-load-path "/home/natha/.emacs.d/themes/")
(load-theme 'monokai t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

					; Navigation / Tips / General
(use-package which-key
  :diminish
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.05))

(use-package company
  :hook((org-mode . company-mode)
	(c++-mode . company-mode)
	(python-mode . company-mode)))

(use-package company-org-roam
  :config
  (push 'company-org-roam company-backends))


(use-package helm
  :config
  (helm-mode)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-ff-skip-boring-files t)
  (setq helm-mini-default-sources '(helm-source-buffers-list
				    helm-source-recentf
				    helm-source-bookmarks))
  (setq helm-boring-buffer-regexp-list '("\\*Messages" "\\*Help" "\\*Shell Command Output" "\\*Flycheck error message"
					 "\\*Compile-Log" "\\*Echo Area" "\\*helm" "\\*helm-mode" "\\*epc con" "\\*Minibuf"
					 "\\*emacsql" "\\*which-key" "\\*code-conversion" "\\*Completions" "*\\/tmp/"
					 "\\*httpd" "\\*tip" "\\*pdf" "\\tq-temp" "\\*epdfinfo" "\\*http melpa" "\\*org-src"
					 "\\*org-roam" "\\*Flymake*" "\\*elpy-rpc*" "\\Python-font-lock" "\\*WoMan-Log*"
					 "\\*Calendar*" "\\*Agenda Commands*")))

(use-package helm-org-rifle
  :config
  (setq helm-org-rifle-show-path t)
  )

(use-package avy
  :config
  (setq avy-keys '(?t ?n ?s ?e ?f ?u ?d ?h ?r ?i))
  )

(use-package ace-window) ; Used only for ace-swap-window since built in is not good

(use-package fzf
  :init
  (setenv "FZF_DEFAULT_COMMAND" "fd --type f --hidden")
  (setq fzf/window-height 30))

					; Set default directory for fzf to ~/
(defun find-file-from-home ()
  (interactive)
  (let ((default-directory "~"))
    (call-interactively 'fzf)))

					; Python
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package elpy
  :ensure t
  :init
  :config
  (setq elpy-rpc-python-command "python3"
	python-shell-interpreter "python3"
	python-shell-interpreter-args "console --simple-prompt"
	python-shell-prompt-detect-failure-warning nil
	python-shell-completion-native-enable nil)
  (elpy-enable))

(use-package blacken
  :hook (python-mode . blacken-mode))

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

					; Org
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

(use-package org-drill)
(use-package interleave
  :config (setq interleave-disable-narrowing t))

(use-package org-bullets
  :hook ((org-mode) . org-bullets-mode))

					; Aesthetics
(use-package rainbow-delimiters
  :hook ((prog-mode text-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook ((html-mode css-mode xml-mode text-mode) . rainbow-mode))

(use-package telephone-line ; Powerline status bar
  :config (telephone-line-mode 1))

(use-package highlight-indent-guides
  :hook ((prog-mode) . highlight-indent-guides-mode))

					; EVIL
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd ",O") 'show-all)
  (define-key evil-normal-state-map (kbd "H") "^")
  (define-key evil-normal-state-map (kbd "L") "$")
  (define-key evil-normal-state-map (kbd "RET") 'org-open-at-point)

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

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

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
    "s" 'avy-goto-char
    "l" 'avy-goto-line
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
    )
  )

(use-package evil-escape
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "nn")
  (setq evil-escape-inhibit-functions '(evil-visual-state-p))
  (setq-default evil-escape-delay 0.2)
  )

(use-package evil-org
  :hook ((org-mode-hook) . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  )

					; Custom modes
					; Centered Cursor
(define-minor-mode centered-point-mode
  "Always center the cursor in the middle of the screen."
  :lighter "..."
  (cond (centered-point-mode (add-hook 'post-command-hook 'line-change))
	(t (remove-hook 'post-command-hook 'line-change)))
  )
(defun line-change ()
  (recenter)
  )
(centered-point-mode 1)

					; Web Dev Preview Mode
(defun my-html-mode-hook ()
					; Starts the `simple-httpd' server if it is not already running
					; Turns on `impatient-mode' for the current buffer."
  (unless (get-process "httpd")
    (message "starting httpd server...")
    (httpd-start))
  (impatient-mode))
(add-hook #'html-mode-hook #'my-html-mode-hook)

					; General Settings
					; Extra Keybindings
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)

					; Parens
(show-paren-mode 1)
(setq show-paren-delay 0)

					; Default Emacs settings
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

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(global-hl-line-mode t)
(set-default-coding-systems 'utf-8)

					; Remember cursor location for files
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

(setq recentf-exclude '("\.svg$")) ; Remove svg files made from org-roam from the recent files list

					; No backups please
(setq create-lockfiles nil)
(setq make-backup-files nil)

(defun toggle-window-split () ; Swap vertical/horizontal split
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-drill which-key use-package telephone-line rainbow-mode rainbow-delimiters powerline org-sticky-header org-bullets markdown-mode impatient-mode helm-org-rifle fzf evil-surround evil-org evil-leader evil-escape dashboard avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
