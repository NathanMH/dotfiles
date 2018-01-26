;;; package --- Summary: Setup evil
;;; Commentary:

(require 'evil)
(evil-mode 1)

;;; Code:

; Use evil mode in ibuffer
(setq evil-emacs-states-modes (delq 'ibuffer-mode evil-emacs-state-modes))

; Buffer Movement
;(define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
;(define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
;(define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
;(define-key evil-normal-state-map (kbd "C-l") 'windmove-right)

(defun my-evil-doc-view-hook()
  (turn-off-evil-mode)
  (define-key doc-view-mode-map (kbd "j") 'doc-view-scroll-up-or-next-page)
  (define-key doc-view-mode-map (kbd "k") 'doc-view-scroll-down-or-previous-page)
  (define-key doc-view-mode-map (kbd "h") 'image-backward-hscroll)
  (define-key doc-view-mode-map (kbd "l") 'image-forward-hscroll))

(add-hook 'doc-view-mode-hook 'my-evil-doc-view-hook)

; Move to begin/end of line
(define-key evil-normal-state-map (kbd "H") "^")
(define-key evil-normal-state-map (kbd "L") "$")

; iBuffer use evil
(evil-add-hjkl-bindings ibuffer-mode-map 'emacs)

; Close html tags easier (CAN'T BE DONE IN EVIL-LEADER)
;(define-key evil-insert-state-map ",/" 'sgml-close-tag)

; Use org-mode tab function
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

(provide 'init-evil)

;;; init-evil ends here
