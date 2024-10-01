;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; == Auto-completion & minibuffer ==
;; == Vertico
(vertico-reverse-mode)	 ; Show candidates in reverse order
(vertico-indexed-mode) ; Allows for index selection with prefix arguments

;; == Consult
;; Bindings
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-M-l") 'consult-imenu)
(global-set-key (kbd "C-x b") 'consult-buffer)
(define-key minibuffer-local-map (kbd "C-r") #'consult-history)

;; == Corfu
(corfu-popupinfo-mode) ; Documentation popup

;; == Marginalia
(setq marginalia-max-relative-age 0) ; Disable showing relative age
(setq marginalia-align 'right)

;; === Dired ===
(setq dired-listing-switches "-laGhs --group-directories-first") ; Show grouped directories first

;; == Terminal ==
(setq term-prompt-regexp "*[*#$%>\n]*[#$%>] *")

;; == Dashboard ==
;; == Banner
(defun sweet-banner ()
  (let* ((banner '(
"⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠤⠤⣶⣶⣶⣶⣶⣶⣶⣤⣀⠀⠀"
"⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⣤⣤⣤⣀⣀⣀⡀⠀⠀⠀⠀⠈⠉⠙⢻⣿⣿⣿⣷⡀"
"⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣶⣶⣿⣿⣿⣿⣿⡇"
"⠀⠀⠀⠀⠀⠀⠀⠀⠹⣿⣿⣿⣿⣿⡿⠿⠛⠛⠻⠿⠿⠿⠿⢿⣿⣿⣿⣿⠟⠁"
"⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⢿⣿⣿⣿⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
"⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⠿⣿⣿⣷⣤⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
"⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠛⢿⣿⣿⣶⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
"⠀⠀⠀⣀⣤⣴⣶⣿⣿⣿⣿⣿⣿⣿⣿⣷⣾⣿⣿⣿⣿⣦⡀⠀⠀⠀⠀⠀⠀⠀"
"⠀⢠⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠿⠿⠿⠿⠿⠿⠿⠿⠷⠀⠀⠀⠀⠀⠀"
"⠀⣿⣿⣿⣿⣿⣿⣿⣿⡿⠛⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
"⠀⠘⣿⣿⣿⣿⣿⣿⣏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
"⠀⠀⠈⠻⢿⣿⣿⣿⣿⣷⣤⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
"⠀⠀⠀⠀⠀⠉⠻⢿⣿⣿⣿⣿⣿⣶⣦⣤⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
"⠀⠀⠀⠀⠀⠀⠀⠀⠈⠙⠛⠿⢿⣿⣿⣿⣿⣿⣿⣿⣷⣶⣦⣤⣀⡀⠀⠀⠀⠀"
"⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⣉⣉⣙⣛⣿⣿⣿⣿⣿⣿⣷⡄⠀⠀"
"⠀⠀⠀⠀⠀⠀⠉⠉⠛⠛⠛⠻⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠿⠛⠛⠉⠀⠀⠀"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 29)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'sweet-banner)

;; == Misc ==
(setq visible-bell t) ; Enable visual bell
(setq history-delete-duplicates t) ; Delete previous identical elements from history
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; Fullscreen on startup

;; == Themeing
(load-theme 'ef-maris-dark t) ; Set custom theme
(set-face-attribute 'default nil :font "Cascadia Mono" :height 115) ; Set custom font and size

;; == Icons
(add-hook! 'dired-mode-hook #'nerd-icons-dired-mode) ; Use nerd icons for dired

;; == Modeline
(setq doom-modeline-height 15) ; Adjust modeline height
(setq doom-modeline-project-detection 'project) ; Detect project root
(setq doom-modeline-icon t) ; Enable modeline icons (on by default)
(setq doom-modeline-position-column-line-format '("%l|%c")) ; line/column numbers format
(setq doom-modeline-total-line-number t) ; Display total line numbers
(setq doom-modeline-gnus t) ; Display GNUs notifs
(setq doom-modeline-gnus-timer 2)

;; == Compiler commands
; Compiler command with c-mode
(require 'compile)
(add-hook 'c-mode-hook
	  (lambda ()
	   (setq compile-command
		 (concat "clang -g3 -Wall -Wextra -std=c18 -lm -o")))) ; Using C17/C18 standard
; Compiler command with c++-mode
(add-hook 'c++-mode-hook
	  (lambda ()
	    (setq compile-command
		  (concat "clang++ -g3 -Wall -Wextra -std=c++17 -lm -o")))) ; Uing C++17 standard.
