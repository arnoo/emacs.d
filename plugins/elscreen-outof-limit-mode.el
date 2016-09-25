;;; elscreen-outof-limit-mode.el --- create any number of screens by overriding elscreen.el

;; Copyright (C) 2015 momomo5717

;; Keywords: elscreen
;; Version: 0.2.4
;; Package-Requires: ((elscreen "20140421.414"))
;; URL: https://github.com/momomo5717/elscreen-outof-limit-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Usage
;;   (require 'elscreen-outof-limit-mode)
;;   (elscreen-outof-limit-mode t)
;;
;;   You can use M-x elscreen-outof-limit-mode-off , if you want to off the mode.
;;   You can use M-x elscreen-outof-limit-mode-off-and-kill ,
;;   if you want to off the mode and kill screens more than no.9.
;;

;;; Code:

(require 'elscreen)

(defgroup elscreen-outof-limit-mode nil
  "ElScreen out of limit -- Create any of screens"
  :tag "ElScreen out of limit"
  :group 'elscreen)

(defcustom elscreen-screen-limit-number nil
  "Screen limit number.
No limit,if nil."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Screen limit number"))

  :group 'elscreen-outof-limit-mode)

;; Override elscreen functions

(defun elscreen--origin-elscreen-create-internal ()
  "Dummy to copy elscreen-create-internal" nil)

(fset 'elscreen--origin-elscreen-create-internal
      (symbol-function 'elscreen-create-internal))

(defun elscreen--create-internal-outof-limit (&optional noerror)
  "Override elscreen-create-internal."
  (cond
   ((and (integerp elscreen-screen-limit-number)
         (>= (elscreen-get-number-of-screens)
             elscreen-screen-limit-number))
    (unless noerror
      (elscreen-message "No more screens."))
    nil)
   (t
    (let ((screen-list (sort (elscreen-get-screen-list) '<))
          (screen 0))
      (elscreen-set-window-configuration
       (elscreen-get-current-screen)
       (elscreen-current-window-configuration))
      (while (eq (nth screen screen-list) screen)
        (setq screen (+ screen 1)))
      (elscreen-set-window-configuration
       screen (elscreen-default-window-configuration))
      (elscreen-append-screen-to-history screen)
      (elscreen-notify-screen-modification 'force)
      (run-hooks 'elscreen-create-hook)
      screen))))

(defun elscreen--origin-elscreen-split ()
  "Dummy to copy elscreen-split" nil)

(fset 'elscreen--origin-elscreen-split
      (symbol-function 'elscreen-split))

(defun elscreen--split-outof-limit ()
  "Override elscreen-split"
  (interactive)
  (if (and (null (one-window-p))
           (or (null elscreen-screen-limit-number)
                (< (elscreen-get-number-of-screens)
                   elscreen-screen-limit-number)))
      (let ((elscreen-split-buffer (current-buffer)))
        (delete-window)
        (elscreen-create)
        (switch-to-buffer elscreen-split-buffer)
        (elscreen-goto (elscreen-get-previous-screen)))
    (elscreen-message "cannot split screen!")))

;; Define elscreen-outof-limit-mode

(defun elscreen--outof-limit-mode-enable ()
  (fset 'elscreen-create-internal
        (symbol-function 'elscreen--create-internal-outof-limit))
  (fset 'elscreen-split
        (symbol-function 'elscreen--split-outof-limit)))

(defun elscreen--outof-limit-mode-disable ()
  (fset 'elscreen-create-internal
        (symbol-function 'elscreen--origin-elscreen-create-internal))
  (fset 'elscreen-split
        (symbol-function 'elscreen--origin-elscreen-split)))

(define-minor-mode elscreen-outof-limit-mode
  "Toggle elscreen-outof-limit-mode"
  :global t
  :group 'elscreen-outof-limit-mode
  (if  elscreen-outof-limit-mode
      (elscreen--outof-limit-mode-enable)
    (elscreen--outof-limit-mode-disable)))

;;;###autoload
(defun elscreen-outof-limit-mode-off ()
  "Turn off elscreen-outof-limit-mode."
  (interactive)
  (elscreen-outof-limit-mode -1))
;;;###autoload
(defun elscreen-outof-limit-mode-off-and-kill ()
  "Tunr off elscreen-outof-limit-mode and kill screens more than no.9."
  (interactive)  
  (let ((now-fr (selected-frame)))
    (dolist (fr (mapcar #'car elscreen-frame-confs))
      (select-frame fr)
      (let ((s-ls (sort (elscreen-get-screen-list) '<)))
        (while (and s-ls (< (car s-ls) 10)) (pop s-ls))
        (when (< (elscreen-get-current-screen) 10)
         (elscreen-set-window-configuration
          (elscreen-get-current-screen)
          (elscreen-current-window-configuration)))
        (dolist (s s-ls)
          (elscreen-kill-internal s))
        (elscreen-goto-internal (elscreen-get-current-screen))
        (elscreen-notify-screen-modification 'force)
        (run-hooks 'elscreen-kill-hook)))
    (select-frame now-fr))
  (elscreen-outof-limit-mode -1))

(provide 'elscreen-outof-limit-mode)

;;; elscreen-outof-limit-mode.el ends here
