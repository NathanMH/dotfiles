; List of packages to install
(setq package-list '(evil 
                     colemak-evil 
                     evil-leader 
                     evil-org 
                     evil-nerd-commenter 
                     evil-surround 
                     evil-terminal-cursor-changer 
                     centered-cursor-mode
                     key-chord
                     flycheck
                     rainbow-delimiters
                     company
                     company-jedi
                     neotree 
                     calfw 
                     calfw-ical
                     calfw-org
                     powerline 
                     powerline-evil
                     org-bullets
                     dashboard
                     helm
                     latex-preview-pane
                     twittering-mode
                     avy
                     ace-jump-buffer
                     page-break-lines
                     projectile
                     fireplace
                     rainbow-mode
                     tabbar
                     speed-type
                     ))

; Need to manually install hl-tags-mode, soft-tab, org-wiki

; List of repositories needed
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

; Install the packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; Load Paths

(add-to-list 'custom-theme-load-path "/home/musicnate/.emacs.d/themes/")
(add-to-list 'load-path "/home/musicnate/.emacs.d/init/")
(add-to-list 'load-path "/home/musicnate/.emacs.d/elpa/")
(add-to-list 'load-path "/home/musicnate/.emacs.d/plugins")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(let ((default-directory "~/.emacs.d/plugins/"))
  (normal-top-level-add-subdirs-to-load-path))
; Load Package Repos

(require 'package)
(package-initialize)

; Add init files

(require 'init-evil-leader)
(require 'init-evil)
(require 'init-evil-addons)
(require 'init-calendar)
(require 'init-centered)
(require 'init-company)
(require 'init-erc)
(require 'init-keychord)
(require 'init-neotree)
(require 'init-rainbow-delimiters)
(require 'init-powerline)
(require 'init-twittering)
(require 'init-org)
(require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-settings)
(require 'init-hl-tags)
(require 'init-latex-preview-pane)
(require 'init-org-wiki)
(require 'init-helm)
(require 'init-avy)
(require 'init-dashboard)
(require 'init-projectile)
(require 'init-mu4e)
(require 'init-tabbar)
(require 'init-speed-type)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Documents/org/todo.org")))
 '(package-selected-packages
   (quote
	(twittering-mode rainbow-delimiters powerline neotree key-chord flycheck evil-terminal-cursor-changer evil-surround evil-org evil-nerd-commenter evil-leader company-jedi colemak-evil centered-cursor-mode calfw ace-jump-mode)))
 '(send-mail-function nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


