; Magit

    (require 'magit)

; Settings

	(with-eval-after-load
		(info-initialize)
		(add-to-list 'Info-directory-list "~/.emacs.d/plugins/magit/Documentation"))

(provide 'user-init-magit)
