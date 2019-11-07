;;### infidicos's emacs config ###

;;set package manager rep================================>
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)
;;<======================================================

;;use use-package to enhance package manager=============>
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;;<======================================================

;;install package if not avalable with key ensure for all of use-package
(setq use-package-always-ensure t)

;;theme 
(use-package monokai-theme
             :config
             (load-theme 'monokai t))
;(use-package zenburn-theme
;             :init
;             (setq zenburn-override-colors-alist
;                   '(("zenburn-bg" . "#282828")))
;             :config
;             (load-theme 'zenburn t))
;(use-package doom-themes
;             :config
;             (load-theme 'doom-solarized-dark t))
;(use-package solarized-theme
;             :init 
;             (load-theme 'solarized-gruvbox-dark t)
;             :config
;             (setq solarized-high-contrast-mode-line t))

;;open emacs with fullscreen, maximized or fullboth
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 120))

;;set font 
(add-to-list 'default-frame-alist '(font . "Hack-20"))

;;disable scroll-bar
(scroll-bar-mode -1)

;;disable menu and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;;switch to scratch buffer on start up
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;;set user use by tool as git commits
(setq user-full-name "infidicos")

;;Garbage collection in Emacs is quite simple. Every time memory crosses a certain threshold, it garbage collects. But the default value for this threshold is quite low by modern standards, just 800KB. Most extensions of Emacs use considerably more memory than that and this low threshold makes Emacs quite slow when using them. Set it something more reasonable like 50MB.
(setq gc-cons-threshold 50000000)

;;when opening files with sizes that are common nowadays (emacs has low threshold file size) and Emacs warns you about being wary of opening such a large file. Increase that limit too.
(setq large-file-warning-threshold 100000000)

;;use title bar to show full path of current line
(setq frame-title-format
      '((:eval (if (buffer-file-name)
       (abbreviate-file-name (buffer-file-name))
       "%b"))))
;;change face color title bar to dark
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;;preserve screen position when jumping around
;;use C-SPC C-SPC to mark current point to pushes it onto a stack of marks and then go back using pop-global-mark C-x C-SPC
(setq scroll-preserve-screen-position t) 

;;current line highlighting
(global-hl-line-mode +1)


;;column number
(column-number-mode t)

;;file size
(size-indication-mode t)

;;display line number in global
(global-display-line-numbers-mode 1)

;;set folder for backup file of emacs
;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;;if edit a file outside of Emacs, the default setting is for Emacs to ask you to reload the file manually. Task Emacs to reload the file automatically
(global-auto-revert-mode t)

;;set tab/space indentation ===========================>
;;creat variable for our preferred tab width
(setq custom-tab-width 2)

;;define two function to hook, can use (interative) to use M-x
(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs ()
  ;set TAB free to use
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  ;enable use tab for indent with value custom
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

;;use hook for prog-mode (major-mode)
(add-hook 'prog-mode-hook 'enable-tabs)
;;lisp and elisp should use space for indentation since it's about alignment
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

;;language specific
(setq-default python-indent-offset custom-tab-width)
;;set indent for c/c++ code, use C-c C-o for change
(setq-default c-basic-offset custom-tab-width)

;;Make the backspace properly erase the tab instead of removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;;visual tab as a pipe character "|"
;; This will also show trailing characters as they are useful to spot.
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
  '(whitespace-tab ((t (:background unspecified :foreground "#3e3f36"))))) ;theme have background #272822, hsl(70,8,15) increase l to 20%
(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9]))) ;124 is ascii id for "|"
(global-whitespace-mode) ;enable whitespace mode everywhere
;;<=====================================================

;; Display parentheses (highlight matching brackets)
(show-paren-mode 1)
(setq show-paren-delay 0)

;;set auto save dir. auto save is #file# using when suddenly close file without saving
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs.d/auto-save/" t)))

;;DON'T know what is use for=============================>
;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; '(package-selected-packages (quote (monokai-theme use-package))))
;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; '(default ((((class color) (min-colors 257)) (:foreground "#F8F8F2" :background "#272822")) (((class color) (min-colors 89)) (:foreground "#F5F5F5" :background "#1B1E1C")))))
;(put 'erase-buffer 'disabled nil)
;;<======================================================

;;add PATH and exec-path ===============================>
(setenv "PATH" (concat "/usr/local/opt/llvm/bin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/local/opt/llvm/bin""/usr/local/bin") exec-path))
;;<======================================================

;; Manage buffers with key-chord========================>
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(use-package key-chord
  :config
  (progn
  (key-chord-mode 1)
  ;key-chord-define-global(key-chord-define-global "ss" 'switch-to-previous-buffer)
  ;(key-chord-define-global "kk" 'next-buffer)
  (key-chord-define-global "gg" 'goto-line)
  ;(key-chord-define-global "jj" 'other-window)
  (key-chord-define-global "xx" 'kill-buffer)))
;;<======================================================

;;clear eshell buffer with clc()=======================>
(defun eshell/clc()
  "to clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))
;;<=====================================================

;;let git branch fot eshell============================>
(defun git-prompt-branch-name ()
    "Get current git branch name"
    (let ((args '("symbolic-ref" "HEAD" "--short")))
      (with-temp-buffer
        (apply #'process-file "git" nil (list t nil) nil args)
        (unless (bobp)
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position))))))

(defun eshell-prompt ()
    (let ((branch-name (git-prompt-branch-name)))
      (concat
       "infidicos "
       (abbreviate-file-name (eshell/pwd)) "\n"
       (if branch-name
            (concat 
                (propertize (format "(%s)" branch-name ) 'face `(:foreground "green4"))
                "$ ")
            "$ ")
       )))

(setq eshell-prompt-function #'eshell-prompt
        eshell-prompt-regexp ".*$+ ")
;;<=====================================================

;;wrap line with indent;; need code by hand in free time;;
(use-package adaptive-wrap)
;;set visual-line-mode, minor mode of emacs
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;;set key C-. to switch buffer should use with ido-mode=>
(global-set-key (kbd "C-.")

	(lambda () (interactive "")

	  (switch-to-buffer (other-buffer (current-buffer) t))))
;;<======================================================

;;set key C-, to switch window
(global-set-key (kbd "C-,") 'other-window)

