;;; package --- Summary: Setup annotations
;;; Commentary:

(require 'annotate)

;;; Code:

(add-hook 'org-mode-hook #'annotate-mode)

(setq annotate-file "~/Documents/dotfiles/linux/emacs/annotations")

(provide 'init-annotate)

;;; init-annotate ends here
