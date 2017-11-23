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

; Look and feel

(setq tabbar-background-color "black")

(custom-set-faces
 '(tabbar-default ((t (:inherit variable-pitch :background "black" :foreground "#FD971F" :weight bold))))
 '(tabbar-button ((t (:inherit tabbar-default :foreground "gold"))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default))))

 '(tabbar-highlight ((t (:underline t))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "#090909" :foreground "#F92672"))))

 '(tabbar-separator ((t (:inherit tabbar-default))))
 '(tabbar-unselected ((t (:inherit tabbar-default)))))

;; *** USE THIS TO ADJUST THE SPACES BETWEEN TABS ***

; Autoload tabbar
(define-globalized-minor-mode global-tabbar-minor-mode
 tabbar-mode tabbar-mode)
(global-tabbar-minor-mode)

(provide 'init-tabbar)
