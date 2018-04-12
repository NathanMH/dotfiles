;;; package --- Summary: Setup ibuffer
;;; Commentary:

(require 'ibuffer)

;;; Code:

(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))
(setq ibuffer-expert t)
(autoload 'ibuffer "ibuffer" "List Buffers." t)
    
; Create default groups for ibuffer
(setq ibuffer-saved-filter-groups
    (quote (("default"
        ("Programming"
            (or
                (mode . c-mode)
                (mode . c++-mode)
                (mode . ruby-mode)
                (mode . python-mode)
                (mode . emacs-lisp-mode)))
        ("HTML"
            (mode . html-mode))
        ("CSS"
            (mode . css-mode))
        ("Git"
            (name . "magit"))
        ("Org" ;; Org related buffers
            (or
                (mode . org-mode)
                (name . "org")))
        ("Calendar"
            (mode . cfw:calendar-mode))
        ("Mail"
            (or
                (mode . message-mode)
                (mode . mail-mode)
                (mode . mu4e-main-mode)
                (mode . mu4e-headers-mode)
                (mode . mu4e-view-mode)
                (mode . mu4e-compose-mode)))
        ("Chat"
            (mode . erc-mode)
            (mode . twitter-mode))
        ("Twitter"
            (mode . twittering-mode))
        ("Emacs"
			(name . "^\\*"))))))


; Add default groups to ibuffer
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

; Collapse groups by default
(setq mp/ibuffer-collapsed-groups (list "Default"))
(defadvice ibuffer (after collapse-helm)
	(dolist (group mp/ibuffer-collapsed-groups)
        (progn
            (goto-char 1)
            (when (search-forward (concat "[ " group " ]") (point-max) t)
                (progn
                    (move-beginning-of-line-nil)
                    (ibuffer-toggle-filter-group))))))

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
	(:name "Size" :inline t)
	(cond
		((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
		((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
		((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
		(t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
	(setq ibuffer-formats
		'((mark modified read-only " "
			(name 18 18 :left :elide)
			" "
			(size-h 9 -1 :right)
			" "
			(mode 16 16 :left :elide)
			" "
			filename-and-process)))

;; Allow movement to circle around top/bottom
(defun ibuffer-previous-line ()
	(interactive) (previous-line)
		(if (<= (line-number-at-pos) 2)
			(goto-line (- (count-lines (point-min) (point-max)) 2))))

(defun ibuffer-next-line ()
	(interactive) (next-line)
	(if (>= (line-number-at-pos) (- (count-lines (point-min) (point-max)) 1))
	(goto-line 3)))

(define-key ibuffer-mode-map (kbd "<up>") 'ibuffer-previous-line)
(define-key ibuffer-mode-map (kbd "<down>") 'ibuffer-next-line)

; Hide the ibuffer header to use tabbar
(setq ibuffer-use-header-line nil)
(defadvice ibuffer-update (around ibuffer-preserve-prev-header activate)
  "Preserve line-header used before Ibuffer if it doesn't set one"
  (let ((prev-line-header header-line-format))
	ad-do-it
	(unless header-line-format
	  (setq header-line-format prev-line-header))))

; Hide empty filter groups
(setq ibuffer-show-empty-filter-groups nil)
(ad-activate 'ibuffer)

;(ibuffer)

(provide 'init-ibuffer)

;;; init-ibuffer ends here
