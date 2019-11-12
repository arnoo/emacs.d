(load "/home/arno/.emacs.d/init.el")

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(define-key mu4e-headers-mode-map (kbd "<C-return>") 'arnaud/mu4e-view-msg-in-tab)

(define-key mu4e-headers-mode-map (kbd "r") 'arnaud/mu4e-archive-thread)
(define-key mu4e-view-mode-map    (kbd "r") 'arnaud/mu4e-archive-thread)

(define-key mu4e-headers-mode-map (kbd "m") 'arnaud/mu4e-mute-thread)
(define-key mu4e-view-mode-map    (kbd "m") 'arnaud/mu4e-mute-thread)

(define-key mu4e-headers-mode-map (kbd "t") 'arnaud/mu4e-msg-to-task)
(define-key mu4e-view-mode-map    (kbd "t") 'arnaud/mu4e-msg-to-task)

(define-key mu4e-view-mode-map    [escape]  'mu4e~view-quit-buffer)

(define-key mu4e-headers-mode-map (kbd "j") 'mu4e-headers-next)
(define-key mu4e-headers-mode-map "\C-j" 'mu4e-headers-next)
(define-key mu4e-view-mode-map (kbd "j") 'next-line)
(define-key mu4e-view-mode-map "\C-j" 'mu4e-view-headers-next)

(define-key mu4e-headers-mode-map (kbd "k") 'mu4e-headers-prev)
(define-key mu4e-headers-mode-map (kbd "\C-k") 'mu4e-headers-prev)
(define-key mu4e-view-mode-map (kbd "k") 'previous-line)
(define-key mu4e-view-mode-map "\C-k" 'mu4e-view-headers-prev)

(define-key mu4e-headers-mode-map (kbd "i") 'arnaud/mu4e-inbox)

(define-key mu4e-headers-mode-map (kbd "G")
                                  (lambda () (interactive)
                                    (end-of-buffer)
                                    (mu4e-headers-prev)))

(define-key mu4e-headers-mode-map (kbd "<f5>") 'mu4e-headers-rerun-search)
(define-key mu4e-headers-mode-map (kbd "\C-r") 'mu4e-headers-rerun-search)
(define-key mu4e-headers-mode-map (kbd "x") 'arnaud/mu4e-headers-execute)

(define-prefix-command 'arnaud/mu4e-g-map)
(define-key mu4e-headers-mode-map "g" 'arnaud/mu4e-g-map)
(define-key arnaud/mu4e-g-map (kbd "g") 'beginning-of-buffer)
;(define-key arnaud/mu4e-g-map (kbd "t") 'centaur-tabs-forward)
;(define-key arnaud/mu4e-g-map (kbd "T") 'centaur-tabs-backward)

;(add-hook 'mu4e-view-mode-hook 'centaur-tabs-local-mode)
;(add-hook 'ag-mode-hook 'centaur-tabs-local-mode)

(setq mail-user-agent 'mu4e-user-agent)

(defun arnaud/mu4e-inbox ()
  (interactive)
  (let* ((time (decode-time (current-time)))
         (hour (elt time 2))
         (dow  (elt time 6)))
    (message "DOW :  %S" dow)
    (mu4e-headers-search 
      (concat "maildir:/Inbox"
              (if (and (>= hour 6) (<= hour 19) (>= dow 1) (<= dow 4))
                  " OR maildir:/my-INBOX"
                  "")))))


(defun arnaud/mu4e-view-in-browser (msg)
  "View the body of the message in a web browser."
  (interactive)
  (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
        (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
    (unless html (error "No html part for this message"))
    (with-temp-file tmpfile
      (insert
        ""
        ""
        html))
    (browse-url (concat "file://" tmpfile))))

(add-to-list 'mu4e-view-actions
             '("View in browser" . arnaud/mu4e-view-in-browser) t)

(defun arnaud/mu4e-headers-execute ()
  (interactive)
  (mu4e-mark-execute-all t)
  (mu4e-update-index)
  (mu4e-headers-rerun-search))

(defun arnaud/mu4e-view-msg-in-tab ()
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (docid (or (mu4e-message-field msg :docid)
                (mu4e-warn "No message at point")))
  	  ;; decrypt (or not), based on `mu4e-decryption-policy'.
         (decrypt (and (member 'encrypted (mu4e-message-field msg :flags))
                       (if (eq mu4e-decryption-policy 'ask)
                           (yes-or-no-p (mu4e-format "Decrypt message?"))
                           mu4e-decryption-policy))))
    (elscreen-create)
    (mu4e~proc-view docid mu4e-view-show-images decrypt)))

(defun arnaud/mu4e-archive-thread ()
  (interactive)
  (when (eq major-mode 'mu4e-view-mode)
    (mu4e~view-quit-buffer))
  (mu4e-headers-mark-thread-using-markpair '(refile . (mu4e-get-refile-folder (mu4e-message-at-point)))))

;(defun arnaud/mu4e-mute-thread ()
;  (interactive)
;  (when (eq major-mode 'mu4e-view-mode)
;    (mu4e~view-quit-buffer))
;  (write-region (concat "if allof (anyof (header :contains \"References\" \"<" (plist-get (mu4e-message-at-point) :message-id) ">\",\n"
;                        "                 header :is \"Message-Id\" \"<" (plist-get (mu4e-message-at-point) :message-id) ">\"),\n"
;                        "          not anyof (address :is \"to\" \"arnaud@btmx.fr\",\n"
;                        "                     address :is \"to\" \"arnaud.betremieux@mayane.eu\",\n"
;                        "                     address :is \"to\" \"arnaud.betremieux@beta.gouv.fr\"))\n"
;                        "{\n"
;                        "  fileinto \"Octo_AllMail\";\n"
;                        "  stop;\n"
;                        "}\n"
;                        "\n")
;                nil
;                "~/.sieve/mutes"
;                'append)
;  (arnaud/mu4e-archive-thread))

(defun arnaud/mu4e-msg-to-task ()
  (interactive)
  (when (eq major-mode 'mu4e-view-mode)
    (mu4e~view-quit-buffer))
  (let ((msg (mu4e-message-at-point)))
    (when msg
      (let* ((context (mu4e-context-determine msg))
             (context-name (if context (mu4e-context-name context) "Perso"))
             (base-command (concat "task add '"
                                   (replace-regexp-in-string "'" " " (plist-get msg :subject))
                                   " m#" (plist-get msg :message-id) "'"
                                   (if (string= context-name "Mayane")
                                       " +my"
                                       "")
                                   " "))
             (actual-command (read-string "" base-command))
             (result (if actual-command (shell-command-to-string actual-command) "")))
        (arnaud/mu4e-archive-thread)
        (message result)))))

(setq mu4e-attachment-dir "~/Downloads")
(setq mu4e-change-filenames-when-moving t) ; Required for mbsync as UIDs are in the filenames
(setq mu4e-headers-auto-update t)
(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
(setq mu4e-headers-fields
     '((:human-date    .   17)
      ;(:flags         .    6)
      ;(:mailing-list  .   10)
       (:from          .   22)
       (:subject       .   nil)))
(setq mu4e-headers-include-related t)
(setq mu4e-headers-skip-duplicates t)
(setq mu4e-headers-time-format "%H:%M")
(setq mu4e-headers-visible-lines 20)
(setq mu4e-update-interval 60)
(setq mu4e-use-fancy-chars t)
(setq mu4e-user-mail-address-list '("arnaud@btmx.fr"
                                    "arnaud@rootcycle.com"
                                    "arnaud.betremieux@mayane.eu"))
(setq mu4e-view-show-addresses 't) ;; show full addresses in view message (instead of just names)
(setq mu4e-view-show-images t)

;; use imagemagick, if available
;(when (fboundp 'imagemagick-register-types)
;  (imagemagick-register-types))

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(add-hook 'message-mode-hook 'turn-on-orgtbl)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)

(setq mu4e-compose-in-new-frame nil)

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")
(setq message-sendmail-f-is-evil 't)
(setq message-sendmail-extra-arguments '("--read-envelope-from"))

(defun arnaud/no-auto-fill ()
  "Turn off auto-fill-mode"
  (auto-fill-mode -1))

(add-hook 'mu4e-compose-mode-hook 'arnaud/no-auto-fill)
;(add-hook 'mu4e-compose-mode-hook 'org-mu4e-compose-org-mode)
(require 'org-mu4e)

;(defun arnaud/htmlize-email ()
;  (goto-char (point-min))
;  (while (search-forward "##SIGNATURE_MAYANE##" nil t)
;    (replace-match (with-temp-buffer
;                     (insert-file-contents "~/.emacs.d/signature_mayane.html")
;                     (buffer-string))))
;  (let ((org-export-with-toc nil))
;    (org~mu4e-mime-convert-to-html)))
;
;(add-hook 'message-send-hook 'arnaud/htmlize-email)

(defun arnaud/email-says-attach-p ()
  "Return t if email suggests there could be an attachment."
  (save-excursion
    (goto-char (point-min))
    (or (re-search-forward "attach" nil t)
        (re-search-forward "joint" nil t))))

(defun arnaud/email-has-attachment-p ()
  "Return t if the currently open email has an attachment"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "<#part" nil t)))

(defun arnaud/email-pre-send-check-attachment ()
  (when (and (arnaud/email-says-attach-p)
             (not (arnaud/email-has-attachment-p)))
    (unless
        (y-or-n-p "Your email suggests you need an attachment, but no attachment was found. Send anyway?")
      (error "It seems an attachment is needed, but none was found. Aborting send."))))

(add-hook 'message-send-hook 'arnaud/email-pre-send-check-attachment)

(setq mu4e-contexts
    `(
       ,(make-mu4e-context
          :name "Perso"
          :enter-func (lambda () (mu4e-message "Entering context 'Perso'"))
          :leave-func (lambda () (mu4e-message "Leaving context 'Perso'"))
          :vars '( ( user-mail-address      . "arnaud@btmx.fr")
                   ( mu4e-sent-folder       . "/Sent")
                   ( mu4e-drafts-folder     . "/Drafts")
                   ( mu4e-refile-folder     . "/Archive") 
                   ( mu4e-compose-signature . "")
                   ))
       ,(make-mu4e-context
          :name "Mayane"
          :enter-func (lambda () (mu4e-message "Entering context 'Mayane'"))
          :leave-func (lambda () (mu4e-message "Leaving context 'Mayane'"))
          :match-func (lambda (msg)
                        (when msg 
                          (string-match "^/my-" (mu4e-message-field msg :maildir))))
          :vars '( ( user-mail-address      . "arnaud.betremieux@mayane.eu" )
                   ( mu4e-sent-messages-behavior . delete)
                   ( mu4e-sent-folder       . "/my-Sent")
                   ( mu4e-drafts-folder     . "/my-Drafts")
                   ( mu4e-refile-folder     . "/my-Archive") 
;                   ( mu4e-compose-signature . "##SIGNATURE_MAYANE##")
;                   ( mu4e-compose-signature .
;                        "#+OPTIONS: toc:nil num:nil"
;                        "#+BEGIN_EXPORT html"
;                          (with-temp-buffer
;                            (insert-file-contents "~/.emacs.d/signature_mayane.html")
;                            (buffer-string))
;                        "#+END_EXPORT"
;                        ))
                   ( mu4e-compose-signature .
                     (concat
                       "Arnaud BÉTRÉMIEUX\n"
                       "MAYANE\n"
                       ".....................\n"
                       "Deskopolitan Paris Voltaire\n"
                       "226 boulevard Voltaire\n"
                       "75011 Paris\n"
                       "+33 (0)6 44 26 59 89\n"
                       "du lundi au jeudi\n"
                       "http://www.mayane.eu/\n"))
                   ))
        ,(make-mu4e-context
          :name "Rootcycle"
          :enter-func (lambda () (mu4e-message "Entering context 'Rootcycle'"))
          :leave-func (lambda () (mu4e-message "Leaving context 'Rootcycle'"))
          :match-func (lambda (msg)
                        (when msg 
                          (and (not (string-match "^/my-" (mu4e-message-field msg :maildir)))
                               (or (mu4e-message-contact-field-matches msg 
                                     :to "arnaud@rootcycle.com")
                                   (mu4e-message-contact-field-matches msg 
                                     :cc "arnaud@rootcycle.com")))))
          :vars '( ( user-mail-address      . "arnaud@rootcycle.com" )
                   ( mu4e-sent-messages-behavior . delete)
                   ( mu4e-sent-folder       . "/Sent")
                   ( mu4e-drafts-folder     . "/Drafts")
                   ( mu4e-refile-folder     . "/Archive") 
                   ( mu4e-compose-signature .
                     (concat
                       "Arnaud Bétrémieux\n"
                       "Rootcycle - Sustainability Systems\n"
                       "+33 (0)6 89 85 88 41\n"
                       "http://rootcycle.com"))))
       ))

; Use first context by default when entering the main view
(setq mu4e-context-policy 'pick-first)

(mu4e~start 'arnaud/mu4e-inbox)
