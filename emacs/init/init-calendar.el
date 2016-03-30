; Calendar
	
	(require 'calfw)
	(require 'calfw-ical)
	(require 'calfw-org)

; Settings

	(define-key evil-normal-state-map (kbd ",cal") (lambda () (interactive) (cfw:open-ical-calendar "/home/musicnate/.emacs.d/calendar/emacs-cal.ics")))

	; TODO intergrate org mode

(provide 'init-calendar)
