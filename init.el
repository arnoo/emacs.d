(add-to-list 'load-path "~/.emacs.d/plugins/")

(setq frame-title-format '("%b - Emacs"))

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
                     kill-buffer-query-functions))

; Make background semi-transparent
;(set-frame-parameter (selected-frame) 'alpha '(85 85))

(defun set-maximized ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -badd,maximized_vert,maximized_horz"))
(add-hook 'window-setup-hook 'set-maximized t)

;(defvar min-timer)
;(defun schedule-minimize ()
;  (when (eq (frame-parameter (selected-frame) 'fullscreen)
;            'maximized)
;    (setq min-timer (run-at-time "1 sec" nil 'minimize-other-windows))))
;
;(defun minimize-other-windows ()
;  (shell-command (concat "bash -c 'xdotool search --desktop `xdotool get_desktop` --name \".*\" | grep -v " (frame-parameter (selected-frame) 'outer-window-id) " | xargs -r -n 1 xdotool windowminimize'")))
;
;; xfwm sends a focus-in event just prior to switching to another workspace, which ruins everything...
;(defun prevent-minimize ()
;  (when (and (boundp 'min-timer) min-timer)
;     (cancel-timer min-timer)
;     (setq min-timer nil)))
;
;(add-hook 'focus-in-hook 'schedule-minimize)
;(add-hook 'focus-out-hook 'prevent-minimize)

; Don't warn when loading files smaller than 200M
(setq large-file-warning-threshold 200000000)

;;; Make underscore a word character
(add-hook 'after-change-major-mode-hook
          '(lambda () (modify-syntax-entry ?_ "w")))

(require 'uniquify)

; *** MELPA ***
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;;; my-Packages
(setq my-packages
      '(package
        cl-lib
        wdired
        color
        color-theme
        dumb-jump
        dtrt-indent
        evil
        evil-numbers
        evil-tabs
        scala-mode2
        js2-mode
        fiplr
        markdown-mode
        evil-search-highlight-persist
        company
        company-tern
        ag
        neotree
        wanderlust
        web-mode))

; Install my-packages as necessary
(defun filter (condp lst)
  "Filter a list of elements with a given predicate"
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(let ((uninstalled-packages (filter (lambda (x) (not (package-installed-p x))) my-packages)))
  (when (and (not (equal uninstalled-packages '()))
             (y-or-n-p (format "Install packages %s?" uninstalled-packages)))
    (package-refresh-contents)
    (mapc 'package-install uninstalled-packages)))

(add-hook 'after-init-hook 'global-company-mode)


(define-key global-map (kbd "<C-next>") 'elscreen-next)
(define-key global-map (kbd "<C-prior>") 'elscreen-previous)
;;; Indentation ...
(define-key global-map (kbd "RET") 'newline-and-indent)
(dtrt-indent-mode 1)
(setf indent-tabs-mode nil) 
(setq make-backup-files nil)
(setq initial-scratch-message nil)
(show-paren-mode 1)
(setq show-paren-delay 0)
(tool-bar-mode -1)
(savehist-mode 1)
(scroll-bar-mode -1)
(setq require-final-newline nil)
(setq column-number-mode t)
(global-unset-key (kbd "<f11>"))
(set-face-attribute 'default nil :height 120)
(defalias 'yes-or-no-p 'y-or-n-p)

(electric-indent-local-mode -1)
(add-hook 'after-change-major-mode-hook
          '(lambda () (electric-indent-mode -1)))
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "TAB") 'tab-to-tab-stop)

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(defun set-tab-width (x)
  (setq tab-width x)
  (setq tab-stop-list (number-sequence tab-width (* tab-width 20) tab-width)))

(set-tab-width 2)


;;;; Colors
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-standard)

(undo-tree-mode 1)


(global-evil-tabs-mode t)


;;;; AG
(require 'ag)
(setq ag-highlight-search t)
(defun ag-search-at-point ()
  (interactive)
  (ag-project (thing-at-point 'symbol)))
(define-key evil-normal-state-map (kbd "C-*") 'ag-search-at-point)

(eval-after-load "evil-maps" '(define-key evil-motion-state-map "\C-]" 'dumb-jump-go))
(setq dumb-jump-fallback-regex "\\bJJJ\\j")

;;;; WEB-MODE

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;;;; Make Evil more Vim Like

;;; No limit to number of tabs
(require 'elscreen-outof-limit-mode)
(elscreen-outof-limit-mode t)

;;; Filename expansion
(define-key evil-insert-state-map (kbd "C-x C-f") 'comint-dynamic-complete-filename)

;;; :q
(defun vimlike-quit ()
  "Vimlike ':q' behavior: close current window if there are split windows;
otherwise, close current tab (elscreen)."
  (interactive)
  (let ((one-elscreen (elscreen-one-screen-p))
	      (one-window (one-window-p)))
    (cond
					; if current tab has split windows in it, close the current live window
     ((not one-window)
      (delete-window) ; delete the current window
      (balance-windows) ; balance remaining windows
      nil)
					; if there are multiple elscreens (tabs), close the current elscreen
     ((not one-elscreen)
      ;(kill-buffer)
      (elscreen-kill)
      nil)
					; if there is only one elscreen, just try to quit (calling elscreen-kill
					; will not work, because elscreen-kill fails if there is only one
					; elscreen)
     (one-elscreen
      (evil-quit)
      nil))))

(defun vimlike-write-quit ()
  "Vimlike ':wq' behavior: write then close..."
  (interactive)
  (save-buffer)
  (vimlike-quit))

(evil-ex-define-cmd "q" 'vimlike-quit)
(evil-ex-define-cmd "wq" 'vimlike-write-quit)

;;; numbers
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

(define-key evil-normal-state-map [(insert)] 'evil-insert)
(define-key evil-normal-state-map (kbd "C-p") 'fiplr-find-file-newtab)

;;; by default, repeat should include the count of the original command !

(evil-define-command my-repeat (&optional count)
    :repeat ignore
        (interactive)
      (message "count :  %S" count)
      (evil-repeat count nil))
(define-key evil-normal-state-map "." 'my-repeat)

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
    In Delete Selection mode, if the mark is active, just deactivate it;
    then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;(display-time-mode t)

;;;; YAML mode

(add-to-list 'load-path "~/.emacs.d/plugins/yaml-mode")
(require 'yaml-mode)

;;;; PHP mode
(require 'php-mode)

(add-hook 'php-mode-hook
   (lambda ()
      (evil-define-key 'normal php-mode-map (kbd "K") 'my-php-symbol-lookup)))

(defun my-php-symbol-lookup ()
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if (not symbol)
        (message "No symbol at point.")
        (browse-url (concat "http://php.net/manual-lookup.php?pattern="
                            (symbol-name symbol))))))

(require 'scala-mode2)

(add-to-list 'auto-mode-alist '("\\.ds\\'" . lisp-mode))

(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(define-key evil-normal-state-map [C-f] 'my-put-file-name-on-clipboard)

; *** SPELL CHECKING ***

(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(defun fd-switch-dictionary()
   (interactive)
   (let* ((dic ispell-current-dictionary)
     (change (if (string= dic "francais") "english" "francais")))
     (ispell-change-dictionary change)
     (message "Dictionary switched from %s to %s" dic change)))

(define-key evil-normal-state-map "]q"  'next-error)
(define-key evil-normal-state-map "[q"  'previous-error)
(define-key evil-normal-state-map "]l"  'flycheck-next-error)
(define-key evil-normal-state-map "[l"  'flycheck-previous-error)
(define-key evil-normal-state-map "]s"  'flyspell-goto-next-error)
(define-key evil-normal-state-map "z="  'ispell-word)
(define-key evil-insert-state-map (kbd "C-x s") 'ispell-word)
(global-set-key (kbd "<f8>") 'fd-switch-dictionary)

(define-key evil-insert-state-map (kbd "C-x C-L") 'evil-complete-next-line)

; *** JAVASCRIPT ***

(add-hook 'js-mode-hook
   (lambda () (push '("function" . ?ƒ) prettify-symbols-alist)
         (push '("return" . ?\u2192) prettify-symbols-alist)
         (prettify-symbols-mode)
         (add-to-list 'load-path "/usr/lib/node_modules/tern/emacs/")
         (add-to-list 'company-backends 'company-tern)
         (autoload 'tern-mode "tern.el" nil t)
         (tern-mode t)
         (setq tern-command (cons (executable-find "tern") '()))
         ;(evil-define-key 'normal tern-mode-keymap "\C-]" 'tern-find-definition)
         (eval-after-load 'tern
             '(progn
               (require 'tern-auto-complete)
               (tern-ac-setup)))))

(add-hook 'find-file-hook
  (lambda ()
    (let ((bfn buffer-file-name))
      (when (and bfn (string-match "/node_modules/" bfn))
        (unless (y-or-n-p "WARNING: are you sure you want to edit a file from node_modules (n will set read-only mode) ?")
          (read-only-mode)))
      (when (and bfn (string-match "/france-entreprises/" bfn))
        (unless (y-or-n-p "WARNING: are you sure you want to edit a file from france-entreprises (n will set read-only mode) ?")
          (read-only-mode)))
      (when (and bfn (string-match "public/images/logos-04V.svg" bfn))
          (fiplr-find-file-newtab))
      )))

; *** LATEX ***

(defun arno-latex-mode ()
  (global-set-key (kbd "<f12>")
          (lambda ()
            (interactive)
            (flyspell-mode 1)
            (shell-command (concat "pdflatex " buffer-file-name)))))

(add-hook 'latex-mode-hook 'arno-latex-mode)

; *** LISP MODE ***

(defun arno-lisp-mode ()
  (arno-all-lisps-mode)
  (require 'slime)
  (global-set-key (kbd "<f12>") 'switch-slime-buffer)
  (evil-define-key 'normal lisp-mode-map "\C-]" 'slime-edit-definition)
  (evil-define-key 'normal lisp-mode-map (kbd "K") 'slime-documentation-lookup)
  (evil-define-key 'normal lisp-mode-map (kbd "<f11>") 'slime-compile-defun)
  (evil-define-key 'normal lisp-mode-map (kbd "<C-l>") 'slime-repl-clear-buffer)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
  (slime-setup '(slime-asdf
                 slime-autodoc
                 slime-editing-commands
                 slime-fancy
                 slime-fancy-inspector
                 slime-fancy-trace
                 slime-fontifying-fu
                 ;slime-highlight-edits
                 slime-package-fu
                 slime-references
                 slime-repl
                 slime-trace-dialog
                 slime-xref-browser))
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
  (modify-syntax-entry ?\} "){" lisp-mode-syntax-table)
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq common-lisp-hyperspec-root "file:/usr/share/doc/hyperspec/"))

(defun arno-all-lisps-mode ()
  (require 'rainbow-delimiters)
  (require 'cl-lib)
  (require 'color)
  
  (rainbow-delimiters-mode)
  ;(cl-loop
  ;   for index from 1 to rainbow-delimiters-max-face-count
  ;   do
  ;   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
  ;     (cl-callf color-saturate-name (face-foreground face) 30)))
  (prettify-symbols-mode)
  ;(add-to-list 'pretty-symbol-categories 'relational)
  (mapcar (lambda (x) (modify-syntax-entry x "w"))
          (list ?- ?/ ?* ?+))
  (set-tab-width 2)
  (setq evil-shift-width tab-width)
  (setq tab-stop-list (number-sequence tab-width (* tab-width 20) tab-width)))

(add-hook 'lisp-mode-hook 'arno-lisp-mode)
(add-hook 'emacs-lisp-mode-hook 'arno-lisp-mode)
  
;; Syntax highlighting in Slime REPL
(defvar slime-repl-font-lock-keywords lisp-font-lock-keywords-2)
(defun slime-repl-font-lock-setup ()
  (setq font-lock-defaults
        '(slime-repl-font-lock-keywords
         ;; From lisp-mode.el
         nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
         (font-lock-syntactic-face-function
         . lisp-font-lock-syntactic-face-function))))

(add-hook 'slime-repl-mode-hook 'slime-repl-font-lock-setup)

(defadvice slime-repl-insert-prompt (after font-lock-face activate)
  (let ((inhibit-read-only t))
    (add-text-properties
     slime-repl-prompt-start-mark (point)
     '(font-lock-face
      slime-repl-prompt-face
      rear-nonsticky
      (slime-repl-prompt read-only font-lock-face intangible)))))


;;----- BEGIN slime switcher code 
(defun get-slime-buffer (&optional buflist)
  (if (equal 0 (length buflist))
    nil
    (if (and (> (length (buffer-name (car buflist))) 10)
             (string= "*slime-repl" (substring (buffer-name (car buflist)) 0 11 )))
      (car buflist)
      (get-slime-buffer (cdr buflist)))))

(defvar *buffer-bookmark* nil)

(defun switch-buffers ()
  (progn
    (setf *temp-bookmark* (current-buffer))
    ;TODO: (elscreen-toggle-display-tab) + disable gT/gt ?
    (switch-to-buffer (or *buffer-bookmark* (get-slime-buffer (buffer-list))))
    (setf *buffer-bookmark* *temp-bookmark*)
    (message (format "switching from %s to %s"
                     (buffer-name *temp-bookmark*) 
                     (buffer-name (current-buffer))))))

(defun my-slime () (interactive)
  (let ((wnd (current-window-configuration)))
    (call-interactively 'slime)
    (sit-for 1)
    (set-window-configuration wnd)))

(defun switch-slime-buffer () (interactive)
 (let ((cur-buf (current-buffer))
       (slime-buf (get-slime-buffer (buffer-list))))
    (if (equal cur-buf slime-buf)
        (if (equal *buffer-bookmark* nil)
          (message "Don't know which buffer to switch to!")
          (switch-buffers))
        (if (equal slime-buf nil)
          (progn
            (message "Can't find REPL buffer, starting slime !")
            (my-slime))
          (switch-buffers)))))


;;----- END slime switcher code

; *** MARKDOWN ***
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-unset-key (kbd "ESC :"))
(global-unset-key (kbd "<M-:>"))

;----- FIPLR
(require 'fiplr)
(defun fiplr-find-file-newtab ()
  (interactive)
  (fiplr-find-file-in-directory (fiplr-root) fiplr-ignored-globs #'evil-tabs-tabedit))

(setq *grizzl-read-max-results* 20)

(setq fiplr-ignored-globs
      '((directories
         (".git" "doc" ".svn" ".tmp" "dist" "node_modules" "france-entreprises" "bower_components" "eidas-node"))
        (files
         ("*.jpg" "*.png" "*.xlsx" "*.fasl" "*.fas" "*.o" "*.jks"))))

(defun my-fiplr-root (orig-fiplr-root)
  (let ((orig-root (funcall orig-fiplr-root)))
    (cond ((< (length (split-string orig-root "/")) 4)
           "/home/arno/dev/kravpass/")
          ;((or (and (>= (length orig-root) 24)
          ;          (string= (substring orig-root 0 24) "/home/arno/workspace/fc/"))
          ;     (string= orig-root "/home/arno/workspace/fc/"))
          ;  "/home/arno/workspace/fc/")
          (t orig-root))))

(advice-add #'fiplr-root :around 'my-fiplr-root)

;Redefine tabedit to be in the right dir
(evil-define-command evil-tabs-tabedit (file)
  (interactive "<f>")
  (let ((dir default-directory))
    (elscreen-create)
    (cd dir))
  (find-file file))

;----- POWERLINE
(add-to-list 'load-path "~/.emacs.d/emacs-powerline")
(require 'powerline)

(defun powerline-buffer-name ()
  (replace-regexp-in-string (fiplr-root) "" buffer-file-name))

(defpowerline buffer-id   (propertize (car (propertized-buffer-identification (powerline-buffer-name)))
                                      'face (powerline-make-face color1)))

;----- ORG-MODE STUFF
(require 'org-install)
(setq org-log-done t)
(setq org-return-follows-link t)
(setq org-startup-with-inline-images 'inlineimages)
(setq org-startup-folded 'show-everything)
(setq org-image-actual-width 300)
(setq org-startup-truncated nil) ; wrap lines

(setq org-link-abbrev-alist
      '(("mail" . "https://mail.google.com/a/arnoo.net/#mbox/%s") ; [[mail:a234xffsdf13244f]]
        ("omail" . "https://mail.google.com/a/octo.com/#mbox/%s")
        ))

;(defun org-display-inline-images (&optional include-linked refresh beg end)
;   "Display inline images.
; 
; An inline image is a link which follows either of these
; conventions:
; 
;   1. Its path is a file with an extension matching return value
;      from `image-file-name-regexp' and it has no contents.
; 
;   2. Its description consists in a single link of the previous
;      type.
; 
; When optional argument INCLUDE-LINKED is non-nil, also links with
; a text description part will be inlined.  This can be nice for
; a quick look at those images, but it does not reflect what
; exported files will look like.
; 
; When optional argument REFRESH is non-nil, refresh existing
; images between BEG and END.  This will create new image displays
; only if necessary.  BEG and END default to the buffer
; boundaries."
;   (interactive "P")
;   (when (display-graphic-p)
;     (unless refresh
;       (org-remove-inline-images)
;       (when (fboundp 'clear-image-cache) (clear-image-cache)))
;     (org-with-wide-buffer
;      (goto-char (or beg (point-min)))
;      (let ((case-fold-search t)
;            (file-extension-re (org-image-file-name-regexp)))
;        (while (re-search-forward "[][]\\[\\(?:file\\|[./~]\\)" end t)
;          (let ((link (save-match-data (org-element-context))))
;            ;; Check if we're at an inline image.
;            (when (and (equal (org-element-property :type link) "file")
;                       (or include-linked
;                           (not (org-element-property :contents-begin link)))
;                       (let ((parent (org-element-property :parent link)))
;                         (or (not (eq (org-element-type parent) 'link))
;                             (not (cdr (org-element-contents parent)))))
;                       (org-string-match-p file-extension-re
;                                           (org-element-property :path link)))
;              (let ((file (expand-file-name
;                           (org-link-unescape
;                            (org-element-property :path link)))))
;                (when (file-exists-p file)
;                  (let ((width
;                         ;; Apply `org-image-actual-width' specifications.
;                         (cond
;                          ((not (image-type-available-p 'imagemagick)) nil)
;                          ((eq org-image-actual-width t) nil)
;                          ((listp org-image-actual-width)
;                           (or
;                            ;; First try to find a width among
;                            ;; attributes associated to the paragraph
;                            ;; containing link.
;                            (let ((paragraph
;                                   (let ((e link))
;                                     (while (and (setq e (org-element-property
;                                                          :parent e))
;                                                 (not (eq (org-element-type e)
;                                                          'paragraph))))
;                                     e)))
;                              (when paragraph
;                                (save-excursion
;                                  (goto-char (org-element-property :begin paragraph))
;                                  (when
;                                      (re-search-forward
;                                       "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
;                                       (org-element-property
;                                        :post-affiliated paragraph)
;                                       t)
;                                    (string-to-number (match-string 1))))))
;                            ;; Otherwise, fall-back to provided number.
;                            (car org-image-actual-width)))
;                          ((numberp org-image-actual-width)
;                           org-image-actual-width)))
;                        (old (get-char-property-and-overlay
;                              (org-element-property :begin link)
;                              'org-image-overlay)))
;                    (if (and (car-safe old) refresh)
;                        (image-refresh (overlay-get (cdr old) 'display))
;                      (let ((image (create-image file
;                                                 (and width 'imagemagick)
;                                                 nil
;                                                 :width width)))
;                        (when image
;                          (let* ((link
;                                  ;; If inline image is the description
;                                  ;; of another link, be sure to
;                                  ;; consider the latter as the one to
;                                  ;; apply the overlay on.
;                                  (let ((parent
;                                         (org-element-property :parent link)))
;                                    (if (eq (org-element-type parent) 'link)
;                                        parent
;                                      link)))
;                                 (ov (make-overlay
;                                      (org-element-property :begin link)
;                                      (progn
;                                        (goto-char
;                                         (org-element-property :end link))
;                                        (skip-chars-backward " \t")
;                                        (point)))))
;                            (overlay-put ov 'display image)
;                            (overlay-put ov 'face 'default)
;                            (overlay-put ov 'org-image-overlay t)
;                            (overlay-put
;                             ov 'modification-hooks
;                             (list 'org-display-inline-remove-overlay))
;                            (push ov org-inline-image-overlays)))))))))))))))

(defun htmlorg-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (kill-new (shell-command-to-string "xclip -selection clipboard -o | pandoc -f html -t json | pandoc -f json -t org"))
  ; | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' 
  (yank))

(require 'ox-publish)
(setq org-publish-project-alist
  '(
     ("org-notes"
        :base-directory "~/wiki/"
        :base-extension "org"
        :publishing-directory "~/wiki/html/"
        :recursive t
        :publishing-function org-html-publish-to-html
        :headline-levels 6
        :auto-preamble t
      )
     ("org-static"
        :base-directory "~/wiki/media/"
        :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
        :publishing-directory "~/wiki/html/"
        :recursive t
        :publishing-function org-publish-attachment
      )))
(setq org-export-htmlize-output-type 'css)


; Don't blink cursor in image mode

(add-hook 'image-minor-mode-hook
  (setq blink-cursor-mode nil))

; set up Emacs for transparent encryption and decryption
(require 'epa-file)
(epa-file-enable)

;---- EVIL MODE, should remain at the end
(require 'evil)
(evil-mode 1)
(require 'evil-tabs)

;;;; Highlight Searches
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

; To only display string whose length is greater than or equal to 3
;(setq evil-search-highlight-string-min-len 3)

(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (bbdb neotree scala-mode2 markdown-mode js2-mode helm flycheck fiplr evil-tabs evil-search-highlight-persist evil-quickscope evil-numbers company-tern color-theme ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
