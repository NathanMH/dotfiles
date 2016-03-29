; Emmet

    (require 'emmet-mode)

; Settings

    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)
    (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
    (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
    (add-hook 'emmet-move-cursor-between-quotes t)

(provide 'user-init-emmet)
