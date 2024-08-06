;; init.el Configurations
;; From kosuzu

(setq inhibit-startup-message 1)	; Disable the startup message.

(add-hook 'emacs-startup-hook 'toggle-frame-maximized) ; Fullscreen on startup 

(scroll-bar-mode -1) 			; Disable scrollbar
(tooltip-mode -1)			; Disable tooltips
(menu-bar-mode -1)			; Disable menubar
(tool-bar-mode -1)			; Disable toolbar
(set-fringe-mode 15) 			; More spaceous!

(setq visible-bell t)			; Enable visual bell

(load-theme 'ef-winter  t)		; Set custom theme

(set-face-attribute 'default nil :font "CascadiaMono" :height 100) ; Set custom font

;; Enable line numbers and columns
(setq column-number-mode t)
(display-line-numbers-mode t)		; Set to absolute by default
(global-display-line-numbers-mode)	; Enabled so line numbers are persistent

;; Disable line numbers on selected modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		vterm-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

;; Org mode Config
(require 'org)		; Make Org mode work with files ending in .org

;; Package source initialisation
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Set up automatic package updater
(use-package auto-package-update
    :custom
  (auto-package-update-interval 2)	; Update every 2 days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;; Enable rainbow delimiters for improved syntax reading in prog-mode
(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

;; Set up built-in project package
(require 'project)

;; ==
;; == KEYBINDS & KEYMAPS ==

;; Set up Ivy keybinds
(use-package ivy
    :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line) 
	 ("C-k" . ivy-previous-line) 
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode t))

;; Set up Counsel keybinds
(use-package counsel
    :bind (("M-x" . counsel-M-x)	; Replaces the default "M-x"
	   ("C-x b" . counsel-ibuffer)
	   ("C-x C-f" . counsel-find-file)
	   :map minibuffer-local-map
	   ("C-r" . 'counsel-minibuffer-history))
    :config
    (setq ivy-initial-inputs-alist nil))

;; Set up Hydra keybinds
(use-package hydra)

;; Text scaling keybind
(defhydra hydra-text-scale ()
  "Scale text"
  ("+" text-scale-increase "Increase")
  ("=" text-scale-increase "Increase")
  ("-" text-scale-decrease "Decrease")
  ("f" nil "Finish" :exit t))

(global-set-key (kbd "C-c =") 'hydra-text-scale/body) ; Set the keybind

;; Line Numbers keybind
(defhydra hydra-line-number ()
  "Line numbers"
  ("a" (setq display-line-numbers 1) "Set to absolute")
  ("r" (setq display-line-numbers 'relative) "Set to relative")
  ("f" nil "Finish" :exit t))

(global-set-key (kbd "C-c l") 'hydra-line-number/body) ; Set the keybind

;; Set up which-key for keybind references
(use-package which-key
    :init (which-key-mode)
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 0.1))

;; == IDE SETUP

;; Set up lsp-mode
(use-package lsp-mode
    :hook ((c-mode . lsp)
	   (c++mode . lsp)
	   (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp
    :config
    (setq lsp-keymap-prefix "C-l")
    (define-key lsp-mode-map (kbd "C-l") lsp-command-map)
    (setq lsp-file-watch-threshold 15000)
    (setq lsp-prefer-flymake nil) ; Set so that lsp-mode uses flycheck
    (require 'ccls)
    (setq ccls-executable "/usr/bin/ccls"))

;; Set up user interface
(use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :commands (lsp-ui-mode)
    :config
    (setq lsp-ui-doc-enable -1)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; Interactive ivy interface for lsp-mode
(use-package lsp-ivy
    :commands lsp-ivy-workspace-symbol)

;; Set up treemacs
(use-package treemacs) 

;; Integrate lsp-mode and treemacs
(use-package lsp-treemacs
    :after lsp
    :commands lsp-treemacs-errors-list)
(lsp-treemacs-sync-mode t); Enables syncing between lsp workspace folders and treemac projects

;; Set up company-mode
(use-package company
    :init (add-hook 'after-init-hook 'global-company-mode)
    :config
    (setq company-show-numbers t
	  company-minimum-prefix-length 1
	  company-idle-delay 0.5
	  company-backends
	  '((company-files		; Files & directory
	     company-keywords
	     company-capf  ; Backend for completion-at-point-functions
	     company-yasnippet)
	    (company-abbrev company-dabbrev))))

(use-package flycheck
    :init (global-flycheck-mode)
    :config
    (setq flycheck-display-errors-function
	  #'flycheck-display-error-messages-unless-error-list)
    (setq flycheck-indication-mode nil))

;; Set up the ccls language server for C/C++
(use-package ccls
    :hook ((c-mode c++-mode) .
	   (lambda () (require 'ccls) (lsp))))

;; Configure electric pair mode.
(electric-pair-mode t)
(setq electric-pair-preserve-balance -1)

;; Change default compiler command
(require 'compile)
(add-hook 'c-mode-hook
	  (lambda ()
	   (setq compile-command  
		 (concat "clang -Wall -std=c11 -lm -o")))) ; -Wall for warnings. -o is to tell where to store the compiler output. Using C11 standard. 

;; ==
;; == TERMINAL MODES

(use-package vterm
    :commands vterm
    :config
    (setq term-prompt-regexp "*[*#$%>\n]*[#$%>] *") ;
    (setq vterm-max-scrollback 10000))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auto-package-update auto-package vterm ef-themes ccls flycheck company lsp-treemacs treemacs lsp-ivy lsp-ui lsp-mode which-key hydra counsel ivy rainbow-delimiters)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
