(require 'annotate)

; Settings

(add-hook 'org-mode-hook #'annotate-mode)

(setq annotate-file "~/Documents/dotfiles/linux/emacs/annotations")

(provide 'init-annotate)
