; TabBar

(require 'tabbar)

; Settings

(setq tabbar-use-images t)
(setq table-time-before-update 0.1)

(global-set-key (kbd "C-l") 'tabbar-forward)
(global-set-key (kbd "C-h") 'tabbar-backward)

(defun my-tabbar-buffer-groups ()
  (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
        ((eq major-mode 'dired-mode) "emacs")
        (t "user"))))

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

; Autoload tabbar
(define-globalized-minor-mode global-tabbar-minor-mode
 tabbar-mode tabbar-mode)
(global-tabbar-minor-mode)

(provide 'init-tabbar)
