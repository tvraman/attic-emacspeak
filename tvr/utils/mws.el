;;; mws.el -*- lexical-binding: t -*-
(require 'cl-lib)

(defvar mws-aphorisms
  ["" " THE FIRST APHORISM "
   " THE SECOND APHORISM "
   " THE THIRD APHORISM "
   " THE FOURTH APHORISM "
   " THE FIFTH APHORISM "
   " THE SIXTH APHORISM "
   " THE SEVENTH APHORISM "
   " THE EIGHTH APHORISM "
   " THE NINTH APHORISM "
   " THE TENTH APHORISM "
   " THE ELEVENTH APHORISM "
   " THE TWELFTH APHORISM "
   " THE THIRTEENTH APHORISM "
   " THE FOURTEENTH APHORISM "
   " THE FIFTEENTH APHORISM "
   " THE SIXTEENTH APHORISM "
   " THE SEVENTEENTH APHORISM "
   " THE EIGHTEENTH APHORISM "
   " THE NINETEENTH APHORISM "
   " THE TWENTIETH APHORISM "
   " THE TWENTY-FIRST APHORISM "
   " THE TWENTY-SECOND APHORISM "
   " THE TWENTY-THIRD APHORISM "
   " THE TWENTY-FOURTH APHORISM "
   " THE TWENTY-FIFTH APHORISM "
   " THE TWENTY-SIXTH APHORISM "
   " THE TWENTY-SEVENTH APHORISM "
   " THE TWENTY-EIGHTH APHORISM "
   " THE TWENTY-NINTH APHORISM "
   " THE THIRTIETH APHORISM "
   " THE THIRTY-FIRST APHORISM "
   " THE THIRTY-SECOND APHORISM "
   " THE THIRTY-THIRD APHORISM "
   " THE THIRTY-FOURTH APHORISM "
   " THE THIRTY-FIFTH APHORISM "
   " THE THIRTY-SIXTH APHORISM "
   " THE THIRTY-SEVENTH APHORISM "
   " THE THIRTY-EIGHTH APHORISM "
   " THE THIRTY-NINTH APHORISM "
   " THE FORTIETH APHORISM "
   " THE FORTY-FIRST APHORISM "
   " THE FORTY-SECOND APHORISM "
   " THE FORTY-THIRD APHORISM "
   " THE FORTY-FOURTH APHORISM "
   " THE FORTY-FIFTH APHORISM "
   " THE FORTY-SIXTH APHORISM "
   " THE FORTY-SEVENTH APHORISM "
   " THE FORTY-EIGHTH APHORISM "
   " THE FORTY-NINTH APHORISM "
   " THE FIFTIETH APHORISM "
   " THE FIFTY-FIRST APHORISM "
   " THE FIFTY-SECOND APHORISM "
   " THE FIFTY-THIRD APHORISM "
   " THE FIFTY-FOURTH APHORISM "
   " THE FIFTY-FIFTH APHORISM "
   " THE FIFTY-SIXTH APHORISM "
   " THE FIFTY-SEVENTH APHORISM "
   " THE FIFTY-EIGHTH APHORISM "
   " THE FIFTY-NINTH APHORISM "
   " THE SIXTIETH APHORISM "
   " THE SIXTY-FIRST APHORISM "
   " THE SIXTY-SECOND APHORISM "
   " THE SIXTY-THIRD APHORISM "
   " THE SIXTY-FOURTH APHORISM "
   " THE SIXTY-FIFTH APHORISM "
   " THE SIXTY-SIXTH APHORISM "
   " THE SIXTY-SEVENTH APHORISM "
   " THE SIXTY-EIGHTH APHORISM "
   " THE SIXTY-NINTH APHORISM "
   " THE SEVENTIETH APHORISM "
   " THE SEVENTY-FIRST APHORISM "
   " THE SEVENTY-SECOND APHORISM "
   " THE SEVENTY-THIRD APHORISM "
   " THE SEVENTY-FOURTH APHORISM "
   " THE SEVENTY-FIFTH APHORISM "
   " THE SEVENTY-SIXTH APHORISM "
   " THE SEVENTY-SEVENTH APHORISM "
   " THE SEVENTY-EIGHTH APHORISM "
   " THE SEVENTY-NINTH APHORISM "
   " THE EIGHTIETH APHORISM "
   " THE EIGHTY-FIRST APHORISM "
   " THE EIGHTY-SECOND APHORISM "
   " THE EIGHTY-THIRD APHORISM "
   " THE EIGHTY-FOURTH APHORISM "
   " THE EIGHTY-FIFTH APHORISM "
   " THE EIGHTY-SIXTH APHORISM "
   " THE EIGHTY-SEVENTH APHORISM "
   " THE EIGHTY-EIGHTH APHORISM "
   " THE EIGHTY-NINTH APHORISM "
   " THE NINETIETH APHORISM "
   " THE NINETY-FIRST APHORISM "
   " THE NINETY-SECOND APHORISM "
   " THE NINETY-THIRD APHORISM "
   " THE NINETY-FOURTH APHORISM "
   " THE NINETY-FIFTH APHORISM  " " THE NINETY-SIXTH APHORISM "
   " THE NINETY-SEVENTH APHORISM "
   " THE NINETY-EIGHTH APHORISM "
   " THE NINETY-NINTH APHORISM "]
  "Aphorism markers")

(defsubst mws-lesson (&optional n-pat)
  "Return lesson pattern --- n-pat is a number or number pattern."
  (format "^ *LESSON +%s *$" (or n-pat "[0-9]+")))

(defsubst mws-week-start (&optional w)
  "Return day number of start of  week `w'
Default is this week."
  (1+ (* 7 (1- (or w (read (format-time-string "%W")))))))

(defsubst mws-hindi ()
  "Speak line in Hindi"
  (interactive )
  (emacspeak-google-tts-line  "hi"))

(defun mws-this-week-lessons ()
  "Read this week's lessons"
  (interactive)
  (let* ((s (mws-week-start))
         (e (+ 7 s)))
    (emacspeak-speak-extent (mws-lesson s) (mws-lesson e) t)))

(defun mws-that-week-lessons (n)
  "Read given  week's lessons specified by week number."
  (interactive "nWeek:")
  (let* ((s  (mws-week-start n))
         (e (+ 7 s)))
    (emacspeak-speak-extent (mws-lesson  s) (mws-lesson e) t)))

(defun mws-today-lesson ()
  "Today's lesson"
  (interactive)
  (let ((d (read (format-time-string "%j"))))
    (emacspeak-speak-extent  (mws-lesson d ) (mws-lesson (1+  d)) t)))

(defun mws-aphorism (&optional d)
  "Move to today's aphorism"
  (interactive
   (list
    (if current-prefix-arg
        (read-number "Aphorism:")
      (read (format-time-string "%j")))))
  (cl-declare (special mws-aphorisms))
  (setq d (1+ (% (1- d) 99)))
  (emacspeak-speak-extent (aref mws-aphorisms d) (aref mws-aphorisms (1+ d))))

(defun mws-random-lesson ()
  "Jump to random lesson"
  (interactive)
    (mws-jump-lesson (random 365)))

(defun mws-jump-lesson (n)
  "Jump to given lesson"
  (interactive "nLesson:")
  (emacspeak-speak-extent (mws-lesson n) (mws-lesson (1+ n)) t))

(defun mws-random-aphorism ()
  "Move to a random  aphorism"
  (interactive)
  ( mws-aphorism (random 100)))

(dtk-set-rate 75)
(dtk-set-punctuations 'some)
(when dtk-split-caps (dtk-toggle-split-caps ))
(emacspeak-pronounce-refresh-pronunciations)

(let ((inhibit-read-only  t)
      (map (make-sparse-keymap)))
  (define-key map  "a" 'mws-aphorism)
  (define-key map  "b" 'mws-random-aphorism)
  (define-key map "c" 'mws-today-lesson)
  (define-key map "W" 'mws-that-week-lessons)
  (define-key map "w" 'mws-this-week-lessons)
  (define-key map  "n" 'emacspeak-outline-speak-next-heading)
  (define-key map  "p" 'emacspeak-outline-speak-previous-heading)
  (define-key map  "/" 'emacspeak-wizards-espeak-line)
  (define-key map  "g" 'mws-hindi)

  (define-key map "r" 'mws-random-lesson)
  (define-key map "l" 'mws-jump-lesson)
  (put-text-property (point-min)  (point-max) 'keymap map))

;; emacspeak-speak-extent
