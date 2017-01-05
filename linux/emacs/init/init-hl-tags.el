;;; hl-tags-mode --- Highlight the current SGML tag context

(require 'hl-tags-mode)
(add-hook 'sgml-mode-hook (lambda () (hl-tags-mode 1)))
(add-hook 'nxml-mode-hook (lambda () (hl-tags-mode 1)))
          
(provide 'init-hl-tags)
