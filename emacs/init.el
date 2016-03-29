; Load Paths

(add-to-list 'custom-theme-load-path "/home/musicnate/.emacs.d/themes/")
(add-to-list 'load-path "/home/musicnate/.emacs.d/init/")
;(add-to-list 'load-path "/home/musicnate/.emacs.d/evil-addons")	
(add-to-list 'load-path "/home/musicnate/.emacs.d/evil")
(add-to-list 'load-path "/home/musicnate/.emacs.d/plugins")
(add-to-list 'load-path "/home/musicnate/.emacs.d/plugins/neotree/")
(add-to-list 'load-path "/home/musicnate/.emacs.d/plugins/calfw/")
(add-to-list 'load-path "/home/musicnate/.emacs.d/plugins/powerline/")
(add-to-list 'load-path "/home/musicnate/.emacs.d/plugins/twittering/")
(add-to-list 'load-path "/home/musicnate/.emacs.d/plugins/mu4e/")
(add-to-list 'load-path "/home/musicnate/.emacs.d/plugins/yasnippet/")
(add-to-list 'load-path "/home/musicnate/.emacs.d/plugins/magit/")
(add-to-list 'load-path "/home/musicnate/.emacs.d/plugins/magit/lisp")
(add-to-list 'load-path "/home/musicnate/.emacs.d/plugins/emmet/")

; Load Package Repos

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

; Add user-init files

(require 'user-init-evil)
(require 'user-init-calendar)
(require 'user-init-centered)
(require 'user-init-easymotion)
(require 'user-init-erc)
(require 'user-init-keychord)
(require 'user-init-neotree)
(require 'user-init-parens)
(require 'user-init-surround)
(require 'user-init-minimap)
(require 'user-init-powerline)
(require 'user-init-twittering)
(require 'user-init-org)
(require 'user-init-mu4e)
(require 'user-init-ibuffer)
(require 'user-init-evil-org-mode)
(require 'user-init-softtab)
(require 'user-init-yasnippet)
(require 'user-init-emmet)
(require 'user-init-flycheck)
(require 'user-init-hl-tags)
(require 'user-init-magit)
(require 'user-init-company)
(require 'user-init-settings)

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
