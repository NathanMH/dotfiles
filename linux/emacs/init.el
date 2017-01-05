; List of packages to install
(setq package-list '(evil 
                     colemak-evil 
                     evil-leader 
                     evil-org 
                     evil-nerd-commenter 
                     evil-surround 
                     evil-terminal-cursor-changer 
                     ace-jump-mode
                     centered-cursor-mode
                     key-chord
                     flycheck
                     rainbow-delimiters
                     company
                     company-jedi
                     neotree 
                     calfw 
                     powerline 
                     twittering-mode))

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
(require 'init-ace-jump)
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
(require 'init-mu4e)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(send-mail-function nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


