(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))
  (package-initialize))

(when (not package-archive-contents)
  (package-refresh-contents))


(defvar dfrei/packages '(async
			 autopair
			 yaml-mode
			 sr-speedbar
			 rainbow-delimiters
                         flymake-ruby
                         inf-ruby 
			 smex
			 deft 
			 use-package
			 auto-complete
			 exec-path-from-shell
			 go-mode
			 erlang
			 puppet-mode
			 magit
			 paredit
			 go-autocomplete
			 go-eldoc
			 ac-slime
			 zenburn-theme
			 solarized-theme
			 flycheck
			 helm
			 ace-window
			 volatile-highlights
			 guide-key
			 projectile
			 helm-projectile
			 )
  "default-packages")
(require 'cl-lib)
(defun dfrei/packages-installed-p ()
  (cl-loop for pkg in dfrei/packages
	   when (not (package-installed-p pkg)) do (cl-return nil)
	   finally (cl-return t)))

(unless (dfrei/packages-installed-p)
  (message "%s" "Refreshing package database")
  (package-refresh-contents)
  (dolist (pkg dfrei/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(setq custom-safe-themes t)

(require 'use-package)

;; Highlight changed areads with certain operations
(use-package volatile-highlights
	     :commands volatile-highlights-mode
	     :config
	     (volatile-highlights-mode t))

;; no menu
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Highlight line
(global-hl-line-mode t)

;; Theme
(if (display-graphic-p)
    (load-theme 'solarized-dark)
  (load-theme 'zenburn))

;; Y or N instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;;Auto indent
(global-set-key (kbd "RET") 'newline-and-indent)

;; No start buffer
(setq initial-scratch-message nil)

;;Display empty lines
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Ido mode
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers)

;; Autopair () [] and so on
(require 'autopair)

;;rainbow
(require 'rainbow-delimiters)

;; helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)

;; ace-window
(global-set-key (kbd "M-p") 'ace-window)

;; smex
(autoload 'smex "smex")
(global-set-key (kbd "M-x") 'smex)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-c C-g"))
(guide-key-mode 1)

;; discover my major
(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-h M-m") 'discover-my-mode)

(setenv "GOPATH" "/home/danfreitas/Projects/Go")

(setq exec-path (cons "/usr/bin/" exec-path))
(add-to-list 'exec-path "/home/danfreitas/Projects/Go/bin")

(defun my-go-mode-hook ()
  ;; Using goimports instead of gofmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
	   "go build -v"))
  
  ;Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))

(add-hook 'go-mode-hook 'my-go-mode-hook)

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete")
(require 'auto-complete-config)

(ac-config-default)
(use-package auto-complete
  :init
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete/dict")
  :config
  (ac-config-default))

;; Perl
(add-to-list 'load-path "~/.emacs.d/pde/lisp")
(load "pde-load")

;; Ruby
(require 'robe)
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook 'robe-mode)

;; projectile
(projectile-global-mode)

(add-to-list 'load-path "~/.emacs.d")
(require 'go-autocomplete)
(use-package go-autocomplete
  :load-path "~/.emacs.d")


;; flymake
(add-to-list 'load-path "~/Projects/Go/src/github.com/dougm/goflymake")
(require 'go-flymake)

;;Org mode
(require 'org)
;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Headlines
'("^\\(\\**\\)\\(\\* \\)\\(.*\xa\\)" (1 (org-get-level-face 1))
  (2 (org-get-level-face 2)) (3 (org-get-level-face 3)))

;; Checkbox
(defun danfreitas/org-html-checkbox (checkbox)
  "Format CHECKBOX into HTML."
  (case checkbox (on "<span class=\"check\">&#x2611;</span>") ; checkbox (checked)
        (off "<span class=\"checkbox\">&#x2610;</span>")
        (trans "<code>[-]</code>")
        (t "")))

(defadvice org-html-checkbox (around danfreitas activate)
  (setq ad-return-value (danfreitas/org-html-checkbox (ad-get-arguments 0))))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-default-notes-file "~/git/org/refile.org")

(global-set-key (kbd "C-c c") 'org-capture)

;;Default templates
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/git/org/refile.org")
               ("* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" default)))
 '(global-hl-line-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
