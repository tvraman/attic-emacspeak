(defsubst dtk-interp-note (instrument pitch duration
                                      &optional target step force)
  (declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "n %s %s %s %s %s %s\n"
                               instrument pitch duration
                               (or target 0)
                               (or step 5)
                               (if force "\nd" ""))))

(defsubst dtk-queue-note (instrument pitch duration
                                     &optional target step)
  "Queue a midi note.
Instrument  is the instrument number.
Pitch is specified as 60 for middle C.
Argument DURATION  is specified in seconds.
Optional arguments target and step let you play chords."
  (declare (special dtk-quiet
                    dtk-speak-server-initialized))
  (unless dtk-quiet
    (when dtk-speak-server-initialized
      (dtk-interp-note instrument  pitch duration
                       target step))))

;;{{{  queue a midi icon

(defun emacspeak-queue-midi-icon (midi-name)
  "Queue midi icon midi-NAME."
  (apply 'dtk-queue-note
         (emacspeak-get-midi-note midi-name)))

;;;###autoload
(defun emacspeak-play-midi-icon (midi-name)
  "Play midi icon midi-NAME."
  (apply 'dtk-force-note
         (emacspeak-get-midi-note midi-name)))

;;}}}
;;{{{  setup play function

(defcustom emacspeak-auditory-icon-function 'emacspeak-serve-auditory-icon
  "*Function that plays auditory icons."
  :group 'emacspeak-sounds
  :type '(choice
          (const emacspeak-play-auditory-icon)
          (const emacspeak-serve-auditory-icon)
          (const emacspeak-native-auditory-icon)
          (const emacspeak-queue-auditory-icon)
          (const emacspeak-play-midi-icon)))
;;;###autoload 
;;{{{  Map names to midi

(defvar emacspeak-midi-table
  (make-hash-table )
  "Association between symbols and midi notes.
When producing midi icons, other modules should use names defined here.")

(defvar emacspeak-default-midi-note '(100 100 .1)
  "Default note to play if requested icon not found.")

(defun emacspeak-define-midi (midi-name midi-note)
  "Define a midi  icon named midi-NAME.
midi-note is a list specifying
(instrument note duration) e.g.
(60 60 .1)
is a .1ms note on instrument 60."
  (declare (special emacspeak-midi-table))
  (setf (gethash  midi-name emacspeak-midi-table) midi-note ))

(defsubst emacspeak-get-midi-note (midi-name)
  "Retrieve midi note that produces midi icon midi-name."
  (declare (special emacspeak-midi-table emacspeak-default-midi-note))
  (or  (gethash midi-name emacspeak-midi-table)
       emacspeak-default-midi-note))

(defsubst emacspeak-list-midi-icons ()
  "Return the  list of midi icons that are currently defined."
  (declare (special emacspeak-midi-table))
  (loop for k being the hash-keys of emacspeak-midi-table
        collect k))

;;}}}
;;{{{  Names of midi icons

(emacspeak-define-midi 'close-object
                       '(117 20 .3))  
(emacspeak-define-midi 'open-object
                       '(52 75 .5))
(emacspeak-define-midi 'delete-object
                       '(8 85 .5 ))
(emacspeak-define-midi 'save-object
                       '(15 50 .1 ))
(emacspeak-define-midi 'modified-object
                       '(13 60 .1))
(emacspeak-define-midi 'unmodified-object
                       '(13 40 .1))
(emacspeak-define-midi 'mark-object
                       '(1 60 .1))

(emacspeak-define-midi 'center
                       '(76 60 .1))
(emacspeak-define-midi 'right
                       '(75 60 .1))
(emacspeak-define-midi 'left
                       '(65 60 .1))
(emacspeak-define-midi 'full
                       '(9 60 .1))
(emacspeak-define-midi 'fill-object
                       '(90 60 .25))
(emacspeak-define-midi 'select-object
                       '(13 30 .25 ))
(emacspeak-define-midi 'button
                       '(117 80 .1))
(emacspeak-define-midi 'news
                       '(100 60 .5))
(emacspeak-define-midi 'ellipses
                       '(9 35 .1))
(emacspeak-define-midi 'deselect-object
                       '(1 80 .1))
(emacspeak-define-midi 'quit
                       '(9 25 .1))
(emacspeak-define-midi 'task-done
                       '(126 60 .1))
(emacspeak-define-midi 'scroll
                       '(122 60 .75 70))
(emacspeak-define-midi 'help
                       '(14 60 .5))
(emacspeak-define-midi   'ask-question
                         '(14 80 .5))
(emacspeak-define-midi 'yes-answer
                       '(112 60 .1))
(emacspeak-define-midi 'no-answer
                       '(112 40 .1 ))
(emacspeak-define-midi 'ask-short-question
                       '(112 60 .1))
(emacspeak-define-midi 'n-answer
                       '(112 50 .1))
(emacspeak-define-midi 'y-answer
                       '(112 80 .1))
(emacspeak-define-midi 'large-movement
                       '(97 70 .25 90))
(emacspeak-define-midi 'yank-object
                       '(96 60 .1))
(emacspeak-define-midi 'search-hit
                       '(9 80 .1))
(emacspeak-define-midi 'search-miss
                       '(13 60 .1))
(emacspeak-define-midi 'warn-user
                       '(55 60 .1))
(emacspeak-define-midi 'progress
                       '(9 80 .1))
(emacspeak-define-midi 'alarm
                       '(102 60 1))
(emacspeak-define-midi 'alert-user
                       '(55 75 .1))
;; document objects
(emacspeak-define-midi 'paragraph
                       '(56 60 .1))
(emacspeak-define-midi 'section
                       '(56 65 .1))
(emacspeak-define-midi 'item
                       '(9 70 .1 ))
(emacspeak-define-midi  'on
                        '(9 35 .1))
(emacspeak-define-midi 'off
                       '(127 50 .5))
(emacspeak-define-midi 'new-mail
                       '(14 60 .5 70))

;;;blank lines etc 

(emacspeak-define-midi 'horizontal-rule
                       '(9 60 .25))

(emacspeak-define-midi 'decorative-rule
                       '(9 70 .25))

(emacspeak-define-midi 'unspeakable-rule
                       '(9 80 .25))

(emacspeak-define-midi 'empty-line
                       '(9 10 .1))

(emacspeak-define-midi 'blank-line
                       '(9 20 .1))
(emacspeak-define-midi 'window-resize
                       '(107 20 .3))

;;}}}
;;{{{  toggle auditory icons

;;; This is the main entry point to this module:
;;;###autoload
(defun emacspeak-toggle-auditory-icons (&optional prefix)
  "Toggle use of auditory icons.
Optional interactive PREFIX arg toggles global value."
  (interactive "P")
  (declare (special emacspeak-use-auditory-icons
                    dtk-program emacspeak-auditory-icon-function))
  (require 'emacspeak-aumix)
  (cond
   (prefix
    (setq  emacspeak-use-auditory-icons
           (not emacspeak-use-auditory-icons))
    (setq-default emacspeak-use-auditory-icons
                  emacspeak-use-auditory-icons))
   (t (setq emacspeak-use-auditory-icons
            (not emacspeak-use-auditory-icons))))
  (message "Turned %s auditory icons %s"
           (if emacspeak-use-auditory-icons  "on" "off" )
           (if prefix "" "locally"))
  (when emacspeak-use-auditory-icons
    (emacspeak-auditory-icon 'on)))

(defvar emacspeak-sounds-auditory-icon-players  
  '(
    ("emacspeak-serve-auditory-icon" . "emacspeak-serve-auditory-icon")
    ("emacspeak-play-auditory-icon" . "emacspeak-play-auditory-icon")
    ("emacspeak-native-auditory-icon" . "emacspeak-native-auditory-icon")
    ("emacspeak-play-midi-icon" . "emacspeak-play-midi-icon"))
  "Table of auditory icon players used  when selecting a player.")

(defun emacspeak-select-auditory-icon-player ()
  "Pick a player for producing auditory icons."
  (declare (special emacspeak-sounds-auditory-icon-players))
  (read 
   (completing-read "Select auditory icon player: "
                    emacspeak-sounds-auditory-icon-players
                    nil nil 
                    "emacspeak-")))
;;;###autoload
(defun  emacspeak-set-auditory-icon-player (player)
  "Select  player used for producing auditory icons.
Recommended choices:

emacspeak-serve-auditory-icon for  the wave device.
emacspeak-queue-auditory-icon when using software TTS.
emacspeak-play-midi-icon for midi device. "
  (interactive
   (list
    (emacspeak-select-auditory-icon-player )))
  (declare (special emacspeak-aumix-midi-available-p
                    emacspeak-auditory-icon-function))
  (cond
   ((and (not emacspeak-aumix-midi-available-p)
         (memq player
               '(emacspeak-play-midi-icon emacspeak-queue-midi-icon)))
    (message
     "Cannot use midi icons in your current environment."))
   (t (setq emacspeak-auditory-icon-function player)))
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

;;}}}
;;{{{ Show all icons

(defun emacspeak-play-all-icons ()
  "Plays all defined icons and speaks their names."
  (interactive)
  (mapcar
   '(lambda (f)
      (emacspeak-auditory-icon f)
      (dtk-speak (format "%s" f))
      (sleep-for 2))
   (emacspeak-sounds-icon-list)))

;;}}}
;;{{{ reset local player
(defun emacspeak-sounds-reset-local-player ()
  "Ask Emacspeak to use a local audio player.
This lets me have Emacspeak switch to using audioplay on
solaris after I've used it for a while from a remote session
where it would use the more primitive speech-server based
audio player."
  (interactive)
  (declare (special emacspeak-play-program))
  (if (file-exists-p "/usr/demo/SOUND/play")
      (setq
       emacspeak-play-program "/usr/demo/SOUND/play"
       emacspeak-play-args "-i"
       emacspeak-auditory-icon-function
       'emacspeak-play-auditory-icon))
  (if (file-exists-p "/usr/bin/audioplay")
      (setq
       emacspeak-play-program "/usr/bin/audioplay"
       emacspeak-play-args "-i"
       emacspeak-auditory-icon-function 'emacspeak-play-auditory-icon)))

;;}}}
;;{{{  flush sound driver

(defcustom emacspeak-sounds-reset-snd-module-command nil
  "Command to reset sound module."
  :type '(choice
          :tag "Command to reset sound modules: "
          (const nil :tag "None")
          (string :tag "Command "))
  :group 'emacspeak-sounds)
;;;###autoload
(defun emacspeak-sounds-reset-sound  ()
  "Reload sound drivers."
  (interactive)
  (declare (special emacspeak-sounds-reset-snd-module-command))
  (when emacspeak-sounds-reset-snd-module-command
    (shell-command emacspeak-sounds-reset-snd-module-command)))

;;}}}
(provide  'emacspeak-sounds)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
