(setq user-full-name "Arnaud Bétrémieux")
(setq user-mail-address "arnaud@btmx.fr")

(add-to-list 'load-path "~/.emacs.d/plugins/")

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
        all-the-icons
        centaur-tabs
        cl-lib
        company
        company-tern
        dumb-jump
        dtrt-indent
        ein
        elpy
        emojify
        evil
        evil-numbers
        evil-search-highlight-persist
        ;evil-tabs
        fiplr
        flycheck-mypy
        git-gutter
        hy-mode
        importmagic
        js2-mode
        magit ; dependency of eyeliner
        markdown-mode
        org-download
        projectile ; dependency of eyeliner
        pydoc
        spaceline
        wdired
        web-mode
        wgrep-ag
        yaml-mode))

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

;;; Icons

(require 'emojify)
(add-hook 'after-init-hook #'global-emojify-mode)

(require 'all-the-icons)

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
(defun arnaud/set-maximized ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -badd,maximized_vert,maximized_horz"))
(add-hook 'window-setup-hook 'arnaud/set-maximized t)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


(setq make-backup-files nil)
(setq initial-scratch-message nil)
(show-paren-mode 1)
(setq show-paren-delay 0)
(savehist-mode 1)
(setq require-final-newline nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq default-indicate-empty-lines t)

(undo-tree-mode 1)

;;; MODE LINE

(add-to-list 'load-path "~/.emacs.d/plugins/eyeliner")
(require 'projectile)
(require 'eyeliner)
(eyeliner/install)

;;; TABS

(require 'centaur-tabs)
(setq centaur-tabs-style "wave")
(setq centaur-tabs-height 32)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-set-modified-marker t)
(centaur-tabs-mode t)
(global-set-key (kbd "C-<prior>")  'centaur-tabs-backward)
(global-set-key (kbd "C-<next>") 'centaur-tabs-forward)
(eval-after-load "evil-maps" '(define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward))
(eval-after-load "evil-maps" '(define-key evil-normal-state-map (kbd "g T") 'centaur-tabs-backward))

(defun centaur-tabs-buffer-groups ()
  (list "single-group-for-now"))

(defun centaur-tabs-hide-tab (x)
  (let ((name (format "%s" x)))
 	  (or
	    (string= "*ag search*" name)
	    (string-prefix-p "*epc con " name)
	    (string= "*Completions*" name)
	    (string= "*Compile-Log*" name)
	    (string= "*Shell Command Output*" name))))

(defun arnaud/switch-to-previous-buffer ()
  "Switch to previously open buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(eval-after-load "evil-maps" '(define-key evil-normal-state-map (kbd "<C-tab>") 'arnaud/switch-to-previous-buffer))

;;; JUMPING

(require 'dumb-jump)

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

(eval-after-load "evil-maps" '(define-key evil-motion-state-map "\C-]" 'arnaud/dumb-jump-go-in-same-tab))

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

;;;; Git-Gutter mode
(require 'git-gutter)
(global-git-gutter-mode +1)

(set-face-foreground 'git-gutter:added "black")
(set-face-background 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "white")
(set-face-background 'git-gutter:deleted "red")
(set-face-foreground 'git-gutter:modified "white")
(set-face-background 'git-gutter:modified "purple")

;;;; Web-mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

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
      (kill-current-buffer)
      nil))))

(defun arnaud/vimlike-write-quit ()
  "Vimlike ':wq' behavior: write then close..."
  (interactive)
  (save-buffer)
  (arnaud/vimlike-quit))

(evil-ex-define-cmd "q" 'arnaud/vimlike-quit)
(evil-ex-define-cmd "wq" 'arnaud/vimlike-write-quit)

;;; numbers
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

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

;;;; YAML mode

(add-to-list 'load-path "~/.emacs.d/plugins/yaml-mode")
(require 'yaml-mode)

;;;; PHP mode
(require 'php-mode)

(add-hook 'php-mode-hook
   (lambda ()
      (evil-define-key 'normal php-mode-map (kbd "K") 'arnaud/php-symbol-lookup)))

(defun arnaud/php-symbol-lookup ()
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if (not symbol)
        (message "No symbol at point.")
        (browse-url (concat "http://php.net/manual-lookup.php?pattern="
                            (symbol-name symbol))))))

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

; *** HY MODE ***
(require 'hy-mode)
;(add-to-list 'auto-mode-alist '("\\.hy\\'" . lisp-mode))

; *** SPELL CHECKING ***

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

; *** MARKDOWN ***
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


; *** PYTHON ***
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
         (highlight-indentation-mode -1)))

;----- FIPLR
(require 'fiplr)

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

(define-key evil-normal-state-map (kbd "C-p") 'fiplr-find-file)


;------ MU4E

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
(define-key arnaud/mu4e-g-map (kbd "t") 'centaur-tabs-forward)
(define-key arnaud/mu4e-g-map (kbd "T") 'centaur-tabs-backward)

(add-hook 'mu4e-view-mode-hook 'centaur-tabs-local-mode)
(add-hook 'ag-mode-hook 'centaur-tabs-local-mode)

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
                  " OR maildir:/Octo_INBOX OR maildir:/PC_INBOX"
                  "")))))

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

(defun arnaud/mu4e-mute-thread ()
  (interactive)
  (when (eq major-mode 'mu4e-view-mode)
    (mu4e~view-quit-buffer))
  (write-region (concat "if allof (anyof (header :contains \"References\" \"<" (plist-get (mu4e-message-at-point) :message-id) ">\",\n"
                        "                 header :is \"Message-Id\" \"<" (plist-get (mu4e-message-at-point) :message-id) ">\"),\n"
                        "          not anyof (address :is \"to\" \"arb@octo.com\",\n"
                        "                     address :is \"to\" \"abetremieux@octo.com\",\n"
                        "                     address :is \"to\" \"arnaud.betremieux@passculture.app\",\n"
                        "                     address :is \"to\" \"arnaud.betremieux@beta.gouv.fr\"))\n"
                        "{\n"
                        "  fileinto \"Octo_AllMail\";\n"
                        "  stop;\n"
                        "}\n"
                        "\n")
                nil
                "~/.sieve/mutes"
                'append)
  (arnaud/mu4e-archive-thread))

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
                                   (if (string= context-name "Octo")
                                       " +octo"
                                       "")
                                   (if (string= context-name "pc")
                                       " +octo +pc"
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
                                    "abetremieux@octo.com"
                                    "arnaud.betremieux@passculture.app"))
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

;; Drop an elscreen screen instead of a frame on sending.
;(defun mu4e-sent-handler (docid path)
;  "Handler function, called with DOCID and PATH for the just-sent
;message. For Forwarded ('Passed') and Replied messages, try to set
;the appropriate flag at the message forwarded or replied-to."
;  (mu4e~compose-set-parent-flag path)
;  (when (file-exists-p path) ;; maybe the draft was not saved at all
;    (mu4e~proc-remove docid))
;  ;; kill any remaining buffers for the draft file, or they will hang around...
;  ;; this seems a bit hamfisted...
;  (dolist (buf (buffer-list))
;    (when (and (buffer-file-name buf)
;	    (string= (buffer-file-name buf) path))
;      (if message-kill-buffer-on-exit
;	(kill-buffer buf))))
;  (if mu4e-compose-in-new-frame
;          (elscreen-kill)
;          (if (buffer-live-p mu4e~view-buffer)
;              (switch-to-buffer mu4e~view-buffer)
;            (if (buffer-live-p mu4e~headers-buffer)
;                (switch-to-buffer mu4e~headers-buffer)
;              ;; if all else fails, back to the main view
;              (when (fboundp 'mu4e) (mu4e)))))
;  (mu4e-message "Message sent"))

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")
(setq message-sendmail-f-is-evil 't)
(setq message-sendmail-extra-arguments '("--read-envelope-from"))

(defun arnaud/no-auto-fill ()
  "Turn off auto-fill-mode"
  (auto-fill-mode -1))

(add-hook 'mu4e-compose-mode-hook 'arnaud/no-auto-fill)

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
          :name "pc"
          :enter-func (lambda () (mu4e-message "Entering context 'pc'"))
          :leave-func (lambda () (mu4e-message "Leaving context 'pc'"))
          :match-func (lambda (msg)
                        (when msg 
                          (and (string-match "^/PC_" (mu4e-message-field msg :maildir))
                               (or (mu4e-message-contact-field-matches msg 
                                     :to "arnaud.betremieux@passculture.app")
                                   (mu4e-message-contact-field-matches msg 
                                     :cc "arnaud.betremieux@passculture.app")))))
           :vars '(( user-mail-address      . "arnaud.betremieux@passculture.app" )
                   ( mu4e-sent-folder       . "/PC_Sent")
                   ( mu4e-drafts-folder     . "/PC_Drafts")
                   ( mu4e-refile-folder     . "/PC_AllMail") 
                   ( mu4e-compose-signature .
                     (concat
                       "Arnaud Bétrémieux\n"
                       "Tech Lead Pass Culture\n"
                       "....................\n"
                       "+33 (0)6 89 85 88 41\n"
                       "du lundi au jeudi\n"
                       ))))
       ,(make-mu4e-context
          :name "Octo"
          :enter-func (lambda () (mu4e-message "Entering context 'Octo'"))
          :leave-func (lambda () (mu4e-message "Leaving context 'Octo'"))
          :match-func (lambda (msg)
                        (when msg 
                          (string-match "^/Octo_" (mu4e-message-field msg :maildir))))
          :vars '( ( user-mail-address      . "abetremieux@octo.com" )
                   ( mu4e-sent-messages-behavior . delete)
                   ( mu4e-sent-folder       . "/Octo_Sent")
                   ( mu4e-drafts-folder     . "/Octo_Drafts")
                   ( mu4e-refile-folder     . "/Octo_AllMail") 
                   ( mu4e-compose-signature .
                     (concat
                       "Arnaud Bétrémieux\n"
                       "Consultant\n"
                       "OCTO Technology\n"
                       ".....................\n"
                       "34, Avenue de l'Opéra\n"
                       "75002 Paris\n"
                       "+33 (0)6 89 85 88 41\n"
                       "du lundi au jeudi\n"
                       "http://blog.octo.com/\n"))))
        ,(make-mu4e-context
          :name "Rootcycle"
          :enter-func (lambda () (mu4e-message "Entering context 'Rootcycle'"))
          :leave-func (lambda () (mu4e-message "Leaving context 'Rootcycle'"))
          :match-func (lambda (msg)
                        (when msg 
                          (and (not (string-match "^/Octo_" (mu4e-message-field msg :maildir)))
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
(require 'org-pretty-table)
(add-hook 'org-mode-hook 'org-pretty-table-mode)

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

(defun arnaud/htmlorg-clipboard ()
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

;;;; AG
(require 'ag)
(setq ag-highlight-search t)
(setq ag-reuse-buffers t)
(setq ag-reuse-window t)
(setq ag-group-matches nil)

;;; Make underscore a word character
(add-hook 'after-change-major-mode-hook
          '(lambda () (modify-syntax-entry ?_ "w")))

(defun arnaud/ag-search-at-point ()
  (interactive)
  (ag-project (thing-at-point 'symbol))
  (other-window 1))

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

(require 'wgrep-ag)
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)
(add-hook 'ag-search-finished-hook 'wgrep-change-to-wgrep-mode)
(evil-define-key 'normal wgrep-mode-map [escape] 'arnaud/wgrep-save-and-quit)
(define-key evil-normal-state-map (kbd "C-*") 'arnaud/ag-search-at-point)

;; Press `dd' to delete lines in `wgrep-mode' in evil directly
(defadvice evil-delete (around evil-delete-hack activate)
  ;; make buffer writable
  (if (and (boundp 'wgrep-prepared) wgrep-prepared)
      (wgrep-toggle-readonly-area))
  ad-do-it
  ;; make buffer read-only
  (if (and (boundp 'wgrep-prepared) wgrep-prepared)
      (wgrep-toggle-readonly-area)))

; set up Emacs for transparent encryption and decryption
(require 'epa-file)
(epa-file-enable)

; EIN (Emacs iPython Notebook)
(require 'ein)
(require 'ein-notebook)
(require 'ein-subpackages)

;---- EVIL MODE, should remain at the end
(evil-mode 1)

(global-unset-key (kbd "ESC :"))
(global-unset-key (kbd "<M-:>"))
(global-unset-key (kbd "<f11>"))

;;;; Highlight Searches
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

(add-hook 'company-mode-hook
          (lambda ()
            (evil-define-key 'insert global-map (kbd "C-n") 'company-complete-common)
            (define-key company-active-map (kbd "C-n") 'company-select-next)
            (define-key company-active-map (kbd "C-p") #'company-select-previous)))

; To only display string whose length is greater than or equal to 3
;(setq evil-search-highlight-string-min-len 3)

(require 'server)
(set-variable 'server-name
              (if (string= (frame-parameter (selected-frame) 'name ) "emacs-mu4e")
              "mu4e"
              "emacs"))
(or (server-running-p)
    (server-start))
