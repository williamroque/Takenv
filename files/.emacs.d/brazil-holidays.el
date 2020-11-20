;;; brazil-holidays.el --- Brazillian holidays for Emacs calendar.

;; A direct fork of netherlands-holidays.el, Copyright (C) 2015 Oleh Krehel.
;; URL of original version: https://github.com/abo-abo/netherlands-holidays

;; Author: Lucas Vieira <lucasvieira@protonmail.com>
;; URL: https://github.com/luksamuk/brazil-holidays/
;; Version: 1.0.0
;; Keywords: calendar, org-mode, org-agenda, emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

(eval-when-compile
  (require 'calendar)
  (require 'holidays))

;;;###autoload
(defvar holiday-brazil-holidays
  '((holiday-fixed 1 1 "Ano Novo")
    (holiday-easter-etc -47 "Carnaval")
    (holiday-easter-etc -2 "Paixão de Cristo")
    (holiday-easter-etc 0 "Páscoa")
    (holiday-fixed 4 21 "Tiradentes")
    (holiday-fixed 5 1 "Dia do Trabalhador")
    (holiday-easter-etc +60 "Corpus Christi")
    (holiday-fixed 9 7 "Dia da Independência do Brasil")
    (holiday-fixed 10 12 "Nossa Senhora Aparecida")
    (holiday-fixed 11 2 "Finados")
    (holiday-fixed 11 15 "Proclamação da República")
    (holiday-fixed 12 25 "Natal"))
  "Brazillian holidays.")

(provide 'brazil-holidays)

;;; brazil-holidays.el ends here
