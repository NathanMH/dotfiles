;;; package --- Summary: Setup rainbow delimiters
;;; Commentary:

(require 'paren)
(require 'rainbow-delimiters)

;;; Code:

; Rainbow Parens
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

; Rainbow CSS mode
(add-hook 'css-mode-hook #'rainbow-mode)

; Show Matching Parens
(show-paren-mode 1)
(setq show-paren-delay 0)

(provide 'init-rainbow)

;;; init-rainbow ends here
