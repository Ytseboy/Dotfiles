(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))


(defvar dfrei/packages '(async
			 autopair
			 yaml-mode
			 sr-speedbar
			 rainbow-delimiters
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
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete/dict")
(ac-config-default)

;; projectile
(projectile-global-mode)

(add-to-list 'load-path "~/.emacs.d")
(require 'go-autocomplete)


;; flymake
(add-to-list 'load-path "~/Projects/Go/src/github.com/dougm/goflymake")
(require 'go-flymake)


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
