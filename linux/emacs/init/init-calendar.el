;;; package --- Summary: Setup calendar
;;; Commentary:
; TODO intergrate org mode

(require 'calfw)
(require 'calfw-ical)
(require 'calfw-org)

;;; Code:

(define-key evil-normal-state-map (kbd ",cal") (lambda () (interactive) (cfw:open-ical-calendar "https://calendar.google.com/calendar/ical/nathan.mador.house%40gmail.com/public/basic.ics")))
; (define-key evil-normal-state-map (kbd ",cal") (lambda () (interactive) (cfw:open-ical-calendar "/home/musicnate/.emacs.d/calendar/emacs-cal.ics")))

(provide 'init-calendar)

;;; init-calendar ends here
