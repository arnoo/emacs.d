(add-to-list 'load-path "~/.emacs.d/plugins/")

(set-frame-parameter (selected-frame) 'alpha '(85 50))

(setq make-backup-files nil)
(show-paren-mode 1)
(setq show-paren-delay 0)
(tool-bar-mode -1)
(savehist-mode 1)
(setq column-number-mode t)
(global-unset-key (kbd "<f11>"))
(set-face-attribute 'default nil :height 150)

; *** MELPA ***
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-high-contrast)

(undo-tree-mode 1)


(require 'evil-tabs)
(global-evil-tabs-mode t)

;(require 'powerline)
;(powerline-evil-vim-color-theme)
;(display-time-mode t)

; *** LISP MODE ***

(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(require 'slime)
(slime-setup '(slime-fancy))

(defun arno-lisp-mode ()
  (arno-all-lisps-mode)
  (global-set-key (kbd "<f12>") 'switch-slime-buffer)
  (evil-define-key 'normal lisp-mode-map (kbd "<C-]>") 'slime-edit-definition)
  (evil-define-key 'normal lisp-mode-map (kbd "K") 'slime-documentation-lookup)
  (evil-define-key 'normal lisp-mode-map (kbd "<f11>") 'slime-compile-defun)
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq common-lisp-hyperspec-root "file:/usr/share/doc/hyperspec/")
  )

(defun arno-all-lisps-mode ()
  (setf indent-tabs-mode nil) 
  (require 'rainbow-delimiters)
  (rainbow-delimiters-mode)
  (require 'cl-lib)
  (require 'color)
  (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30)))
  (prettify-symbols-mode)
  (prettify-symbols-mode)
  ;(add-to-list 'pretty-symbol-categories 'relational)
  )

(add-hook 'lisp-mode-hook 'arno-lisp-mode)
(add-hook 'emacs-lisp-mode-hook 'arno-lisp-mode)

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

(global-unset-key (kbd "<C-tab>"))
(global-unset-key (kbd "<C-S-tab>"))
(global-set-key (kbd "<C-tab>") 'next-useful-buffer)
(global-set-key (kbd "<C-S-tab>") 'previous-useful-buffer)

;;----- END Vim like buffer switching

;;----- BEGIN slime switcher code 
(defun get-slime-buffer (&optional buflist)
  (when (null buflist) (setf buflist (buffer-list)))
  (if (equal 0 (length buflist)) nil
    (if (and (> (length (buffer-name (car buflist))) 10)
	     (string= "*slime-repl" (substring (buffer-name (car buflist)) 0 11 )))
	(car buflist)
      (get-slime-buffer (cdr buflist)))))

(defvar *buffer-bookmark* nil)

(defun switch-buffers ()
  (progn
	(setf *temp-bookmark* (current-buffer))
    (switch-to-buffer (or *buffer-bookmark* (get-slime-buffer)))
    (setf *buffer-bookmark* *temp-bookmark*)
	(message (format "switching from %s to %s"
	(buffer-name *temp-bookmark*) 
	(buffer-name (current-buffer))))))

(defun switch-slime-buffer () (interactive)
 (let ((cur-buf (current-buffer))
       (slime-buf (get-slime-buffer)))
    (if (equal cur-buf slime-buf)
        (if (equal *buffer-bookmark* nil)
            (message "Don't know which buffer to switch to!")
            (switch-buffers))
        (if (equal slime-buf nil)
	    (progn
		(message "Can't find REPL buffer, starting slime !")
		(slime)
		(sleep-for 2)
	        (switch-buffers))
            (switch-buffers)))))

;;----- END slime switcher code

;; EVIL MODE, should remain at the end
(require 'evil)
(evil-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
