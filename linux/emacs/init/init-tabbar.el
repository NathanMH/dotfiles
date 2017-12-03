; TabBar

(require 'tabbar)

; Settings

(setq tabbar-use-images t)
(setq table-time-before-update 0.1)

(global-set-key (kbd "C-l") 'tabbar-forward)
(global-set-key (kbd "C-h") 'tabbar-backward)

(setq tabbar-buffer-groups-function
	  (lambda ()
		(let ((dir (expand-file-name default-directory)))
		  (cond ((member (buffer-name) '("*Completions*"
										 "*scratch*"
										 "*Messages*"
										 "*Ediff Registry*"
										 "*Flycheck error messages*"
										 "*Help*"))
				 (list "misc"))
				((member (buffer-name) '("*dashboard*"
										 "*buffer-selection*"
										 "*Ibuffer*"
										 "*helm buffers*"
										 "*helm find*"))
				 (list "dashboard"))
				((string-match-p "/org/" dir)
				 (list "org"))
				(t (list dir))))))

; Look and feel

(setq tabbar-background-color "black")

(custom-set-faces
 '(tabbar-default ((t (:inherit variable-pitch :background "black" :foreground "#FD971F" :weight bold))))
 '(tabbar-button ((t (:inherit tabbar-default :foreground "gold"))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default))))

 '(tabbar-highlight ((t (:underline t))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "#090909" :foreground "#F92672"))))

 '(tabbar-unselected ((t (:inherit tabbar-default))))
)

;; *** USE THIS TO ADJUST THE SPACES BETWEEN TABS ***
(custom-set-variables
 '(tabbar-separator (quote (1.2))))

; Autoload tabbar
(define-globalized-minor-mode global-tabbar-minor-mode
 tabbar-mode tabbar-mode)
(global-tabbar-minor-mode)

(provide 'init-tabbar)
