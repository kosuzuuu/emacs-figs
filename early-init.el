;; early-init.el Configurations
;;  From kosuzu

;; == Garbage Collection ==
;; Increasing the GC threshold for quicker startups
;; By default its set to 800 KB.
(setq gc-cons-threshold 100000000)	; Here its set to 100MB
(setq garbage-collection-messages t)	; Enable garabge collection message

;; == UI Configs ==
(setq inhibit-startup-message 1)	; Disable the startup message
(scroll-bar-mode -1) 			; Disable scrollbar
(tooltip-mode -1)			; Disable tooltips
(menu-bar-mode -1)			; Disable menubar
(tool-bar-mode -1)			; Disable toolbar
(set-fringe-mode 15) 			; More spaceous!
(setq visible-bell t)			; Enable visual bell

;; == Line numbers & columns ==
(setq column-number-mode t)	   
(display-line-numbers-mode t)	   ; Set to absolute by default
(global-display-line-numbers-mode) ; Enabled so line numbers are persistent

;; == Disable line numbers on selected modes ==
(dolist (mode '(org-mode-hook
		term-mode-hook
		vterm-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))


(add-hook 'emacs-startup-hook 'toggle-frame-maximized) ; Fullscreen on startup

(set-face-attribute 'default nil :font "CascadiaMono" :height 100) ; Set custom font
