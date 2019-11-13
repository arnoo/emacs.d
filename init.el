(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq user-full-name "Arnaud Bétrémieux")
(setq user-mail-address "arnaud@btmx.fr")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(git-gutter:ask-p nil)
 '(git-gutter:modified-sign "*")
 '(git-gutter:update-interval 2)
 '(package-selected-packages
   (quote
    (bbdb neotree markdown-mode js2-mode helm flycheck fiplr evil-search-highlight-persist evil-quickscope evil-numbers company-tern ag)))
 '(safe-local-variable-values
   (quote
    ((Base . 10)
     (Package . CL-USER)
     (Syntax . COMMON-LISP)))))

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
                     kill-buffer-query-functions))

(setq large-file-warning-threshold 200000000)

; *** MELPA ***
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq arnaud/packages
      '(package
        ag
        cl-lib
        company
        company-tern
        dtrt-indent
        evil
        flycheck-mypy
        importmagic
        js2-mode
        pydoc
        use-package
        wdired))

; Install arnaud/packages as necessary
(defun filter (condp lst)
  "Filter a list of elements with a given predicate"
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(let ((uninstalled-packages (filter (lambda (x) (not (package-installed-p x))) arnaud/packages)))
  (when (and (not (equal uninstalled-packages '()))
             (y-or-n-p (format "Install packages %s?" uninstalled-packages)))
    (package-refresh-contents)
    (mapc 'package-install uninstalled-packages)))

;(add-hook 'after-init-hook 'global-company-mode)

;;; Indentation

(define-key global-map (kbd "RET") 'newline-and-indent)
(dtrt-indent-mode 1)
(setf indent-tabs-mode nil) 
(electric-indent-local-mode -1)
(add-hook 'after-change-major-mode-hook
          (lambda () (electric-indent-mode -1)))
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "TAB") 'tab-to-tab-stop)

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(defun arnaud/set-tab-width (x)
  (setq tab-width x)
  (setq tab-stop-list (number-sequence tab-width (* tab-width 20) tab-width)))

(arnaud/set-tab-width 2)

;; UI
(load-theme 'dichromacy)
(require 'evil)
(setq column-number-mode t)
(display-time-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :height 120)
(setq frame-title-format '("%b - Emacs"))
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

; Don't blink cursor in image mode
(add-hook 'image-minor-mode-hook (setq blink-cursor-mode nil))

(setq make-backup-files nil)
(setq initial-scratch-message nil)
(show-paren-mode 1)
(setq show-paren-delay 0)
(savehist-mode 1)
(setq require-final-newline nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq default-indicate-empty-lines t)

(undo-tree-mode 1)

;;; JUMPING

(use-package dumb-jump
  :defer 1
  :ensure t
  :config (progn
            (setq dumb-jump-fallback-regex "\\bJJJ\\j")
            
            (push '(:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "lisp"
            	           :regex "\\\((defun|defmacro)\\s+JJJ\\j"
            	           ;; \\j usage see `dumb-jump-ag-word-boundary`
            	           :tests ("(defun test (blah)" "(defun test\n" "(defmacro test (blah)" "(defmacro test\n")
            	           :not ("(defun test-asdf (blah)" "(defun test-blah\n" "(defmacro test-asdf (blah)"
            	                 "(defmacro test-blah\n"  "(defun tester (blah)" "(defun test? (blah)" "(defun test- (blah)"))
                  dumb-jump-find-rules)
            	
            (push '(:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "lisp"
            	           :regex "\\\(defvar\\b\\s*JJJ\\j"
            	           :tests ("(defvar test " "(defvar test\n")
            	           :not ("(defvar tester" "(defvar test?" "(defvar test-"))
                  dumb-jump-find-rules)
            	
            (push '(:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "lisp"
            	           :regex "\\\(JJJ\\s+" :tests ("(let ((test 123)))") :not ("(let ((test-2 123)))"))
                  dumb-jump-find-rules)
            	
            (push '(:type "variable" :supports ("ag" "rg" "git-grep") :language "lisp"
            	           :regex "\\((defun|defmacro)\\s*.+\\\(?\\s*JJJ\\j\\s*\\\)?"
            	           :tests ("(defun blah (test)" "(defun blah (test blah)" "(defun (blah test)")
            	           :not ("(defun blah (test-1)" "(defun blah (test-2 blah)" "(defun (blah test-3)"))
                  dumb-jump-find-rules)
            (eval-after-load "evil-maps" '(define-key evil-motion-state-map "\M-\C-]" 'dumb-jump-go))
            (defun arnaud/dumb-jump-go-in-same-tab ()
              (interactive)
              (let ((arnaud/dumb-jump-same-tab t))
                (dumb-jump-go)))
            
            (setq arnaud/dumb-jump-same-tab nil)
            
            (defun dumb-jump-goto-file-line (thefile theline pos)
            	  "Open THEFILE and go to line THELINE"
                (evil-set-jump)
            	  (if (fboundp 'xref-push-marker-stack)
            	      (xref-push-marker-stack)
            	   (ring-insert find-tag-marker-ring (point-marker)))
            	  (let* ((visible-buffer (find-buffer-visiting thefile))
            	         (visible-window (when visible-buffer (get-buffer-window visible-buffer))))
            	    (cond
            	     ((and visible-window dumb-jump-use-visible-window)
            	      (select-window visible-window))
                   (arnaud/dumb-jump-same-tab
                    (let ((new-buffer (find-file-noselect thefile)))
                      (kill-current-buffer)
                      (switch-to-buffer new-buffer)
                      (goto-line theline)))
            	     (t
                    (find-file thefile)))
                    (goto-line theline)))
            
            (eval-after-load "evil-maps" '(define-key evil-motion-state-map "\C-]" 'arnaud/dumb-jump-go-in-same-tab))))

(use-package git-gutter
  :defer 2
  :ensure t
  :config (progn
            (global-git-gutter-mode +1)
            (set-face-foreground 'git-gutter:added "black")
            (set-face-background 'git-gutter:added "green")
            (set-face-foreground 'git-gutter:deleted "white")
            (set-face-background 'git-gutter:deleted "red")
            (set-face-foreground 'git-gutter:modified "white")
            (set-face-background 'git-gutter:modified "purple")))

(use-package web-mode
  :ensure t
  :mode ("\\.ejs\\'"
         "\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"))

;;; Filename expansion
(define-key evil-insert-state-map (kbd "C-x C-f") 'comint-dynamic-complete-filename)

;;; :q
(defun arnaud/vimlike-quit ()
  "Vimlike ':q' behavior: close current window if there are split windows;
otherwise, close current tab."
  (interactive)
  (let ((one-window (one-window-p)))
    (cond
					; if current tab has split windows in it, close the current live window
     ((not one-window)
      (delete-window) ; delete the current window
      (balance-windows) ; balance remaining windows
      nil)
     (t
      (delete-frame)
      nil))))

(defun arnaud/vimlike-write-quit ()
  "Vimlike ':wq' behavior: write then close..."
  (interactive)
  (save-buffer)
  (arnaud/vimlike-quit))

(evil-ex-define-cmd "q" 'arnaud/vimlike-quit)
(evil-ex-define-cmd "wq" 'arnaud/vimlike-write-quit)

;;; numbers
(use-package evil-numbers
  :defer 1
  :ensure t
  :config (progn
            (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
            (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)))

(define-key evil-normal-state-map [(insert)] 'evil-insert)

;;; by default, repeat should include the count of the original command !

(evil-define-command arnaud/repeat (&optional count)
    :repeat ignore
        (interactive)
      (message "count :  %S" count)
      (evil-repeat count nil))
(define-key evil-normal-state-map "." 'arnaud/repeat)

(define-key evil-normal-state-map (kbd "C-;") 'eval-expression)

(evil-set-initial-state 'ag-mode 'normal)

;;; esc quits
(defun arnaud/minibuffer-keyboard-quit ()
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
(define-key minibuffer-local-map [escape] 'arnaud/minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'arnaud/minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'arnaud/minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'arnaud/minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'arnaud/minibuffer-keyboard-quit)

;;;; Set autosave directory so that all the autosaves are in one place, and not all over the filesystem.

(setq backup-directory-alist `((".*" . "~/.emacs.d/backup")))
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms `((".*" , "~/.emacs.d/autosave/" t)))

(use-package yaml-mode
  :load-path "~/.emacs.d/plugins/yaml-mode")

(use-package php-mode
  :load-path "~/.emacs.d/plugins"
  :config (progn
            (defun arnaud/php-symbol-lookup ()
              (interactive)
              (let ((symbol (symbol-at-point)))
                (if (not symbol)
                    (message "No symbol at point.")
                    (browse-url (concat "http://php.net/manual-lookup.php?pattern="
                                        (symbol-name symbol))))))
            (add-hook 'php-mode-hook
            (lambda ()
              (evil-define-key 'normal php-mode-map (kbd "K") 'arnaud/php-symbol-lookup)))))

(add-to-list 'auto-mode-alist '("\\.ds\\'" . lisp-mode))

(defun arnaud/put-file-name-on-clipboard ()
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

(define-key evil-normal-state-map [C-f] 'arnaud/put-file-name-on-clipboard)

(use-package hy-mode
  :ensure t
  :defer 1
  :mode ("\\.hy\\'" . lisp-mode))

(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-dictionary "francais") 
  (setq ispell-really-hunspell t))

(defun arnaud/flyspell-buffer-unless-large ()
   (unless (> (buffer-size) (* 70 1024))
     (flyspell-buffer)))

(dolist (hook '(text-mode-hook org-mode-hook))
  (add-hook hook (lambda ()
                   (setq org-download-image-dir "/home/arno/wiki/media")
                   (setq org-download-heading-lvl nil)
                   (turn-on-flyspell)
                   (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))
                   (setq flyspell-issue-message-flag nil)
                   (arnaud/flyspell-buffer-unless-large))))

(defun arnaud/fd-switch-dictionary()
   (interactive)
   (let* ((dic ispell-current-dictionary)
     (change (if (string= dic "francais") "english" "francais")))
     (ispell-change-dictionary change)
     (message "Dictionary switched from %s to %s" dic change)
     (arnaud/flyspell-buffer-unless-large)))

(define-key evil-normal-state-map "]q"  'next-error)
(define-key evil-normal-state-map "[q"  'previous-error)
(define-key evil-normal-state-map "]l"  'flycheck-next-error)
(define-key evil-normal-state-map "[l"  'flycheck-previous-error)
(define-key evil-normal-state-map "]s"  'flyspell-goto-next-error)
(define-key evil-normal-state-map "z="  'ispell-word)
(define-key evil-insert-state-map (kbd "C-x s") 'ispell-word)
(global-set-key (kbd "<f8>") 'arnaud/fd-switch-dictionary)

(define-key evil-insert-state-map (kbd "C-x C-L") 'evil-complete-next-line)

(setq flyspell-issue-message-flag nil) ;printing messages for every word (when checking the entire buffer) causes an enormous slowdown
(add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))

; *** JAVASCRIPT ***

(add-hook 'js-mode-hook
   (lambda () (push '("function" . ?ƒ) prettify-symbols-alist)
         (push '("return" . ?\u2192) prettify-symbols-alist)
         (prettify-symbols-mode)
         (add-to-list 'load-path "/usr/lib/node_modules/tern/emacs/")
         (add-to-list 'company-backends 'company-tern)
         (autoload 'tern-mode "tern.el" nil t)
         (tern-mode t)
         (setq flycheck-checkers '(javascript-eslint))
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
          (read-only-mode))))))

; *** LATEX ***

(defun arnaud/latex-mode ()
  (global-set-key (kbd "<f12>")
          (lambda () (interactive)
            (flyspell-mode 1)
            (shell-command (concat "pdflatex " buffer-file-name)))))

(add-hook 'latex-mode-hook 'arnaud/latex-mode)

; *** LISP MODE ***

(defun arnaud/lisp-mode ()
  (arnaud/all-lisps-mode)
  (require 'slime)
  (global-set-key (kbd "<f12>") 'arnaud/switch-slime-buffer)
  (evil-define-key 'normal lisp-mode-map "\C-]"
                                         (lambda () (interactive)
                                           (if (equal (arnaud/get-slime-buffer (buffer-list)) nil)
                                               (call-interactively 'dumb-jump-go)
                                               (call-interactively 'slime-edit-definition))))
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

(defun arnaud/all-lisps-mode ()
  (require 'rainbow-delimiters)
  (require 'cl-lib)
  
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
  (arnaud/set-tab-width 2)
  (setq evil-shift-width tab-width)
  (setq tab-stop-list (number-sequence tab-width (* tab-width 20) tab-width)))

(add-hook 'lisp-mode-hook 'arnaud/lisp-mode)
(add-hook 'emacs-lisp-mode-hook 'arnaud/lisp-mode)
  
;; Syntax highlighting in Slime REPL
(defvar arnaud/slime-repl-font-lock-keywords lisp-font-lock-keywords-2)
(defun arnaud/slime-repl-font-lock-setup ()
  (setq font-lock-defaults
        '(arnaud/slime-repl-font-lock-keywords
         ;; From lisp-mode.el
         nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
         (font-lock-syntactic-face-function
         . lisp-font-lock-syntactic-face-function))))

(add-hook 'slime-repl-mode-hook 'arnaud/slime-repl-font-lock-setup)

(defadvice slime-repl-insert-prompt (after font-lock-face activate)
  (let ((inhibit-read-only t))
    (add-text-properties
     slime-repl-prompt-start-mark (point)
     '(font-lock-face
      slime-repl-prompt-face
      rear-nonsticky
      (slime-repl-prompt read-only font-lock-face intangible)))))


;;----- BEGIN slime switcher code 
(defun arnaud/get-slime-buffer (&optional buflist)
  (if (equal 0 (length buflist))
    nil
    (if (and (> (length (buffer-name (car buflist))) 10)
             (string= "*slime-repl" (substring (buffer-name (car buflist)) 0 11 )))
      (car buflist)
      (arnaud/get-slime-buffer (cdr buflist)))))

(defvar *buffer-bookmark* nil)

(defun arnaud/switch-buffers ()
  (progn
    (setf *temp-bookmark* (current-buffer))
    (switch-to-buffer (or *buffer-bookmark* (arnaud/get-slime-buffer (buffer-list))))
    (setf *buffer-bookmark* *temp-bookmark*)
    (message (format "switching from %s to %s"
                     (buffer-name *temp-bookmark*) 
                     (buffer-name (current-buffer))))))

(defun arnaud/slime () (interactive)
  (let ((wnd (current-window-configuration)))
    (call-interactively 'slime)
    (sit-for 1)
    (set-window-configuration wnd)))

(defun arnaud/switch-slime-buffer () (interactive)
 (let ((cur-buf (current-buffer))
       (slime-buf (arnaud/get-slime-buffer (buffer-list))))
    (if (equal cur-buf slime-buf)
        (if (equal *buffer-bookmark* nil)
          (message "Don't know which buffer to switch to!")
          (arnaud/switch-buffers))
        (if (equal slime-buf nil)
          (progn
            (message "Can't find REPL buffer, starting slime !")
            (arnaud/slime))
          (arnaud/switch-buffers)))))


;;----- END slime switcher code

(use-package markdown-mode
  :defer 1
  :ensure t
  :mode "\\.md\\'" "\\.markdown\\'")

(use-package elpy
  :defer 1
  :ensure t
  :config (progn
            (elpy-enable)
            (when (require 'flycheck nil t)
              (require 'flycheck-mypy)
              (add-to-list 'flycheck-disabled-checkers 'python-flake8)
              (add-to-list 'flycheck-disabled-checkers 'python-pylint)
              (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
              (add-hook 'elpy-mode-hook 'flycheck-mode))
            
            (add-hook 'elpy-mode-hook
               (lambda () (push '("function" . ?ƒ) prettify-symbols-alist)
                     (push '("lambda" . ?λ) prettify-symbols-alist)
                     (evil-define-key 'normal elpy-mode-map (kbd "K") 'pydoc-at-point)
                     (importmagic-mode)
                     (evil-define-key 'normal importmagic-mode-map (kbd "C-i") 'importmagic-fix-symbol-at-point)
                     (highlight-indentation-mode -1)))))

(use-package fiplr
  :defer 2
  :ensure t
  :config (progn
            (setq *grizzl-read-max-results* 20)
            
            (setq fiplr-ignored-globs
                  '((directories
                     (".git" "doc" ".svn" ".tmp" "dist" "node_modules" "france-entreprises" "bower_components" "eidas-node" "ftp_mirrors"))
                    (files
                     ("*.jpg" "*.png" "*.xlsx" "*.fasl" "*.fas" "*.o" "*.jks" "*.pyc" "*~" "#*#"))))

            (defun arnaud/fiplr-root (orig-fiplr-root)
              (let ((orig-root (funcall orig-fiplr-root)))
                (cond ((< (length (split-string orig-root "/")) 4)
                      "/home/arno/dev/kravpass/")
                      ;((or (and (>= (length orig-root) 24)
                      ;          (string= (substring orig-root 0 24) "/home/arno/workspace/fc/"))
                      ;     (string= orig-root "/home/arno/workspace/fc/"))
                      ;  "/home/arno/workspace/fc/")
                      (t orig-root))))

            (advice-add #'fiplr-root :around 'arnaud/fiplr-root)

            (define-key evil-normal-state-map (kbd "C-p") 'fiplr-find-file)))

;----- ORG-MODE
(require 'org-install)
(setq org-log-done t)
(setq org-return-follows-link t)
(setq org-startup-with-inline-images t)
(setq org-startup-folded 'show-everything)
(setq org-image-actual-width nil)
(setq org-startup-truncated nil) ; wrap lines
(setq org-hide-leading-stars t)
(setq org-src-fontify-natively t)
(setq org-babel-sh-command "bash")
(global-set-key (kbd "<f7>") 'toggle-truncate-lines)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
 '(powerline-gui-use-vcs-glyph t))

(add-hook 'org-mode-hook 'org-indent-mode)

(use-package org-pretty-table
  :load-path "~/.emacs.d/plugins/"
  :hook org-mode)

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (js         . t)
   (lisp       . t)
   (perl       . t)
   (python     . t)
   (shell      . t)
   ))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(setq org-export-htmlize-output-type 'css)

(defun arnaud/htmlorg-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (kill-new (shell-command-to-string "xclip -selection clipboard -o | pandoc -f html -t json | pandoc -f json -t org"))
  ; | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' 
  (yank))

(use-package ox-publish
  :defer 1
  :init (setq org-publish-project-alist
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
              ))))


;;;; AG
(use-package ag
  :defer 1
  :ensure t
  :config (progn
            (setq ag-highlight-search t)
            (setq ag-reuse-buffers t)
            (setq ag-reuse-window t)
            (setq ag-group-matches nil)

            (defun arnaud/ag-search-at-point ()
              (interactive)
              (ag-project (thing-at-point 'symbol))
              (other-window 1))

            (define-key evil-normal-state-map (kbd "C-*") 'arnaud/ag-search-at-point)))

;;; Make underscore a word character
(add-hook 'after-change-major-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))

(use-package wgrep-ag
  :defer 1
  :after ag
  :ensure t
  :config (progn
            (defun arnaud/wgrep-save-and-quit ()
              (interactive)
              (when (buffer-modified-p)
                (when (y-or-n-p "Save wgrep changes ?")
                  (wgrep-finish-edit)
                  (wgrep-save-all-buffers))
                (wgrep-abort-changes)
                (wgrep-exit))
              (kill-current-buffer)
              (evil-quit))

            (autoload 'wgrep-ag-setup "wgrep-ag")
            (add-hook 'ag-mode-hook 'wgrep-ag-setup)
            (add-hook 'ag-search-finished-hook 'wgrep-change-to-wgrep-mode)
            (evil-define-key 'normal wgrep-mode-map [escape] 'arnaud/wgrep-save-and-quit)
            
            ;; Press `dd' to delete lines in `wgrep-mode' in evil directly
            (defadvice evil-delete (around evil-delete-hack activate)
              ;; make buffer writable
              (if (and (boundp 'wgrep-prepared) wgrep-prepared)
                  (wgrep-toggle-readonly-area))
              ad-do-it
              ;; make buffer read-only
              (if (and (boundp 'wgrep-prepared) wgrep-prepared)
                  (wgrep-toggle-readonly-area)))))

(use-package epa-file ;transparent encryption and decryption
  :defer 1
  :init (epa-file-enable))

;---- EVIL MODE, should remain at the end
(evil-mode 1)

(global-unset-key (kbd "ESC :"))
(global-unset-key (kbd "<M-:>"))
(global-unset-key (kbd "<f11>"))

;;;; Highlight Searches
(use-package evil-search-highlight-persist
  :defer 1
  :ensure t
  :config (global-evil-search-highlight-persist t))

(add-hook 'company-mode-hook
          (lambda ()
            (evil-define-key 'insert global-map (kbd "C-n") 'company-complete-common)
            (define-key company-active-map (kbd "C-n") 'company-select-next)
            (define-key company-active-map (kbd "C-p") #'company-select-previous)))

; To only display string whose length is greater than or equal to 2
(setq evil-search-highlight-string-min-len 2)
