(add-to-list 'load-path "~/.emacs.d/plugins/")

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
                     kill-buffer-query-functions))

; Make background semi-transparent
(set-frame-parameter (selected-frame) 'alpha '(85 85))

; Don't warn when loading files smaller than 200M
(setq large-file-warning-threshold 200000000)

(setf indent-tabs-mode nil) 
(setq make-backup-files nil)
(setq initial-scratch-message nil)
(show-paren-mode 1)
(setq show-paren-delay 0)
(tool-bar-mode -1)
(savehist-mode 1)
(scroll-bar-mode -1)
(setq column-number-mode t)
(global-unset-key (kbd "<f11>"))
(set-face-attribute 'default nil :height 120)
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Indentation ...
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

;;; Make underscore a word character
(add-hook 'after-change-major-mode-hook
          '(lambda () (modify-syntax-entry ?_ "w")))

; *** MELPA ***
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;;; my-Packages
(setq my-packages
      '(package cl-lib color color-theme evil evil-numbers evil-tabs scala-mode2 js2-mode fiplr))

;;;; Install my-packages as necessary
(defun filter (condp lst)
  "Filter a list of elements with a given predicate"
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(let ((uninstalled-packages (filter (lambda (x) (not (package-installed-p x))) my-packages)))
  (when (and (not (equal uninstalled-packages '()))
             (y-or-n-p (format "Install packages %s?" uninstalled-packages)))
    (package-refresh-contents)
    (mapc 'package-install uninstalled-packages)))

(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-standard)

(undo-tree-mode 1)


(global-evil-tabs-mode t)

;(require 'hippie-expand)
(global-set-key (kbd "C-x C-f") 'my-expand-file-name-at-point)
(defun my-expand-file-name-at-point ()
  "Use hippie-expand to expand the filename"
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name)))
    (call-interactively 'hippie-expand)))

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
      (kill-buffer)
      (elscreen-kill)
      nil)
					; if there is only one elscreen, just try to quit (calling elscreen-kill
					; will not work, because elscreen-kill fails if there is only one
					; elscreen)
     (one-elscreen
      (evil-quit)
      nil)
     )))

(defun vimlike-write-quit ()
  "Vimlike ':wq' behavior: write then close..."
  (interactive)
  (save-buffer)
  (vimlike-quit))

(evil-ex-define-cmd "q" 'vimlike-quit)
(evil-ex-define-cmd "wq" 'vimlike-write-quit)

(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map [(insert)] 'evil-insert)
(define-key evil-normal-state-map (kbd "C-p") 'fiplr-find-file-newtab)

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

(require 'php-mode)
(require 'scala-mode2)

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


; *** LISP MODE ***

(defun arno-lisp-mode ()
  (arno-all-lisps-mode)
  (require 'slime)
  (global-set-key (kbd "<f12>") 'switch-slime-buffer)
  (evil-define-key 'normal lisp-mode-map (kbd "<C-]>") 'slime-edit-definition)
  (evil-define-key 'normal lisp-mode-map (kbd "K") 'slime-documentation-lookup)
  (evil-define-key 'normal lisp-mode-map (kbd "<f11>") 'slime-compile-defun)
  (evil-define-key 'normal lisp-mode-map (kbd "<C-l>") 'slime-repl-clear-buffer)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
  (slime-setup '(slime-asdf
                 slime-autodoc
                 slime-editing-commands
                 slime-fancy-inspector
                 slime-fancy-trace
                 slime-fontifying-fu
                 ;slime-highlight-edits
                 slime-package-fu
                 slime-references
                 slime-repl
                 slime-trace-dialog
                 slime-xref-browser))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq common-lisp-hyperspec-root "file:/usr/share/doc/hyperspec/"))

(defun arno-all-lisps-mode ()
  (require 'rainbow-delimiters)
  (require 'cl-lib)
  (require 'color)
  
  (rainbow-delimiters-mode)
  (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30)))
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

;;----- BEGIN Vim like buffer switching

(defun useful-buffer-p (buffer)
  (string-match (regexp-quote "*") buffer))

(defun next-useful-buffer ()
  (next-buffer)
  (when (not (useful-buffer-p (current-buffer)))
    (next-useful-buffer)))

(defun previous-useful-buffer ()
  (previous-buffer)
  (when (not (useful-buffer-p (current-buffer)))
    (previous-useful-buffer)))

(global-unset-key (kbd "ESC :"))
;(global-unset-key (kbd "ESC ESC"))
(global-unset-key (kbd "<C-tab>"))
(global-unset-key (kbd "<C-S-tab>"))
(global-set-key (kbd "<C-tab>") 'next-useful-buffer)
(global-set-key (kbd "<C-S-tab>") 'previous-useful-buffer)

(global-unset-key (kbd "<M-:>"))

;;----- END Vim like buffer switching

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
            (slime))
          (switch-buffers)))))

;;----- END slime switcher code

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(add-hook 'js-mode-hook
   (lambda () (push '("function" . ?ƒ) prettify-symbols-alist)
         (push '("return" . ?\u2192) prettify-symbols-alist)
         (prettify-symbols-mode)))

;; EVIL MODE, should remain at the end
(require 'evil)
(require 'evil-tabs)
(require 'fiplr)

(defun fiplr-find-file-newtab ()
  (interactive)
  (fiplr-find-file-in-directory (fiplr-root) fiplr-ignored-globs
      #'evil-tabs-tabedit))
(evil-mode 1)

;Redefine tabedit to be in the right dir
(evil-define-command evil-tabs-tabedit (file)
  (interactive "<f>")
  (let ((dir default-directory))
    (elscreen-create)
    (cd dir))
  (find-file file))

(add-to-list 'load-path "~/.emacs.d/emacs-powerline")
(require 'powerline)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((fiplr-ignored-globs
      (directories
       (".git" "doc" ".tmp" "dist" "node_modules"))
      (files
       ("*.jpg" "*.png" "*.xlsx")))
     (fiplr-ignored-globs
      (directories
       (".git" "doc" ".tmp" "dist" "node_modules"))
      (files
       ("*.jpg" "*.png")))
     (fiplr-ignored-globs
      (directories
       (".git" "docs" ".tmp" "dist" "node_modules"))
      (files
       ("*.jpg" "*.png")))
     (fiplr-ignored-globs
      (directories
       (".git" "docs" "webapp/dist" "node_modules" "webapp/.tmp"))
      (files
       ("*.jpg" "*.png")))
     (fiplr-ignored-globs
      (directories
       (".git" "./docs" "webapp/dist" "node_modules" "./webapp/.tmp"))
      (files
       ("*.jpg" "*.png")))
     (fiplr-ignored-globs
      (directories
       (".git" "docs" "webapp/dist" "node_modules")
       "webapp/node_modules")
      (files
       ("*.jpg" "*.png")))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
