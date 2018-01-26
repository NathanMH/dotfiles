;;; package --- Summary: Setup hl tags
;;; Commentary:

(require 'hl-tags-mode)

;;; Code:

(add-hook 'sgml-mode-hook (lambda () (hl-tags-mode 1)))
(add-hook 'nxml-mode-hook (lambda () (hl-tags-mode 1)))
 
(provide 'init-hl-tags)

;;; init-hl-tags ends here
