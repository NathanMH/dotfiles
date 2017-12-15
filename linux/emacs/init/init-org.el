; Org Mode

(require 'org)
(require 'org-bullets)

; Settings

; Set location
(setq org-directory "/home/musicnate/Documents/org")

; Org agenda
(setq org-agenda-files '("/home/musicnate/Documents/org/todo.org"))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-return-follows-link t)
(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
(setq org-startup-with-inline-images t)

; Org Bullets
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(provide 'init-org)
