;;; init.el Configurations
;;; From kosuzu

;; == Load files ==
;; == early-init.el
(setq early-init-file (expand-file-name "early-init.el" user-emacs-directory))
(load early-init-file) ; Load early-init.el
;; == custom.el
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file) ; Load custom.el

;; == Modeline ==
;; Doom modeline requires nerd-icons package to be installed
(use-package doom-modeline
  :config
  (setq doom-modeline-height 15)	; Adjust modeline height
  (setq doom-modeline-project-detection 'project) ; Detect project root
  (setq doom-modeline-icon t)			  ; Enable modeline icons (on by default)
  (setq doom-modeline-position-column-line-format '("%l|%c")) ; line/column numbers format
  (setq doom-modeline-total-line-number t)		      ; Display total line numbers
  (setq doom-modeline-gnus t)				      ; Display GNUs notifs
  (setq doom-modeline-gnus-timer 2)
  :init
  (doom-modeline-mode 1)) ; Initialise doom modeline

;; == Org mode ==
(require 'org) ; Make Org mode work with files ending in .org

;; ==  Package source initialisation & manager  ==
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
(setq use-package-always-ensure t) ; Removes the need to do ':ensure t', mostly.

;; == automatic package updater
(use-package auto-package-update
  :custom
  (auto-package-update-interval 2)	; Update every 2 days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(require 'project) ; Use built-in package.el


;; == Dired configs ==
(use-package dired
    :ensure nil ; Dired is already installed, so this is needed
    :commands (dired dired-jump)
    :bind (("C-x C-j" . dired-jump))
    :config
    (setq dired-listing-switches "-laGhs --group-directories-first") ; Show grouped directories first
    )

;; Use treemacs icons for dired
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-mode))

;; == Auto-complete & minibuffer configs ==
;; == Vertico (completion UI)
(use-package vertico
  :demand t
  :bind(:map vertico-map
	     ("C-j" . vertico-next)
	     ("C-k" . vertico-previous)
	     ("C-f" . vertico-exit)
	     :map minibuffer-local-map
	     ("C-j" . vertico-next)
	     ("C-k" . vertico-previous)
	     ("C-b" . vertico-)
	     ("M-h" . backward-killword))
  :custom
  (vertico-resize t)
  (vertico-cycle nil)		  ; Disabled cycling for next/previous
  :init
  (vertico-grid-mode)	 ; Change minibuffer appearance to grid format
  (vertico-indexed-mode) ; Allows for index selection with prefix arguments
  :config
  (vertico-mode))

;; == Consult
(use-package consult
    :demand t
    :bind (("C-s" . consult-line)
	   ("C-M-l" . consult-imenu)
	   ("C-M-j" . persp-switch-to-buffer*)
	   ("C-x b" . consult-buffer)
	   :map minibuffer-local-map
	   ("C-r" . consult-history)))


;; == Global & Hydra keybindings ==
;; == Hydra
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
  ("a" (setq display-line-numbers 1) "Set to absolute" :exit t)
  ("r" (setq display-line-numbers 'relative) "Set to relative" :exit t))
(global-set-key (kbd "C-c l") 'hydra-line-number/body) ; Set the keybind

;; == Evil Mode (vim bindings)
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(defun rune/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
   (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :hook (evil-mode . rune/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; == which-key (Keybind reference)
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

;; == Magit Configurations ==
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-window-except-diff-v1))

;; == LSP ==
(use-package lsp-mode
    :hook ((c-mode . lsp)
	   (c++-mode . lsp)
	   (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp
    :config
    (setq lsp-keymap-prefix "C-l")
    (define-key lsp-mode-map (kbd "C-l") lsp-command-map)
    (setq lsp-file-watch-threshold 15000)
    (setq lsp-enable-on-type-formatting nil) ; Re-enable if this is useful for you
    :custom
    (setq lsp-prefer-flymake nil) ; Disabled  so that lsp-mode uses flycheck
    (setq lsp-enable-global-mode nil)	; Disable lsp-mode by default unless explicitly toggled on
    (require 'ccls)
    (setq ccls-executable "/usr/bin/ccls"))

;; == LSP UI ==
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)	; Hooked to lsp-mode
  :commands (lsp-ui-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; == Flycheck ==
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-indication-mode nil))

;; == Treemacs ==
(use-package treemacs)
;; Integrate lsp-mode and treemacs
(use-package lsp-treemacs
  :after lsp
    :commands lsp-treemacs-errors-list)
(lsp-treemacs-sync-mode t); Enables syncing between lsp workspace folders and treemac projects

;; == Company Mode ==
(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-show-quick-access t
	company-minimum-prefix-length 1
	company-idle-delay 0.2
	company-backends
	'((company-files		; Files & directory
	   company-keywords
	   company-capf  ; Backend for completion-at-point-functions
	   company-yasnippet)
	  (company-abbrev company-dabbrev))))

;; == Lang ==
;; == C and C++
(use-package ccls
    :hook ((c-mode c++-mode) .
	   (lambda () (require 'ccls) (lsp))))
;; Change C default commenting style (Use // instead of /**/)
(add-hook 'c-mode-hook (lambda () (setq comment-start "//"
                                        comment-end   "")))

;; Configure electric pair mode.
(electric-pair-mode t)
(setq electric-pair-preserve-balance nil)

;; Change default compiler command
(require 'compile)
(add-hook 'c-mode-hook			; Compiler command with c-mode
	  (lambda ()
	   (setq compile-command
		 (concat "clang -Wall -std=c17 -lm -o")))) ; -Wall for warnings. -o is to tell where to store the compiler output. Using C17 standard.
(add-hook 'c++-mode-hook				   ; Compiler command with c++-mode
	  (lambda ()
	    (setq compile-command
		  (concat "clang++ -Wall -std=c++17 -lm -o")))) ; -Wall for warnings. -o is to tell where to store the compiler output. Uing C++17 standard.

;; == Terminal ==
;; == Vterm
(use-package vterm
    :commands vterm
    :config
    (setq term-prompt-regexp "*[*#$%>\n]*[#$%>] *")
    (setq vterm-max-scrollback 10000))

;; === Misc ===
;; == treemacs icons for dired
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-mode))

;; == Compiler commands
; Compiler command with c-mode
(require 'compile)
(add-hook 'c-mode-hook
	  (lambda ()
	   (setq compile-command
		 (concat "clang -Wall -std=c17 -lm -o")))) ; -Wall for warnings. -o is to tell where to store the compiler output. Using C17 standard.
; Compiler command with c++-mode
(add-hook 'c++-mode-hook
	  (lambda ()
	    (setq compile-command
		  (concat "clang++ -Wall -std=c++17 -lm -o")))) ; -Wall for warnings. -o is to tell where to store the compiler output. Uing C++17 standard.

(provide 'early-init)
(provide 'init)
;;; init.el ends here
