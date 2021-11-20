;;packages to look for later: (evil-mode)

;; bloat removal
(setq inhibit-startup-message t)
(scroll-bar-mode -1)            ; disable scrollbar
(tool-bar-mode -1)              ; disable toolbar
(tooltip-mode -1)               ; disable tooltips
(set-fringe-mode 3)             ; ? 
(menu-bar-mode -1)              ; disable menu bar

;; Set font
; (set-face-attribute 'default nil :font "Font Name" :height x)

;; Startup time
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 50 1000 1000))

(defun personal/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'personal/display-startup-time)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
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

;; No Littering
; setting cache directory
(setq user-emacs-directory "~/.cache/emacs")
(use-package no-littering)
; Set autosave files in the same path
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Set line and column numbering
(global-display-line-numbers-mode t)
(column-number-mode)
;;Disable it for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b". counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))
;; Using ivy
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
  (ivy-mode 1))
;; Set Ivy Rich
(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))
;; Ivy Prescient - sort by frequency
(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

;; UI config
;; Command log
(use-package command-log-mode
  :commands command-log-mode)

;; Set all-icons -- Run M-x all-the-icons-install-fonts
(use-package all-the-icons)
;; Set Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
;; Load Theme
(use-package doom-themes
  :init (load-theme 'doom-oceanic-next t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; Set Transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
;; Set which-key
(use-package which-key
  :defer 0 
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.9))
;; Use helpful
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
;; Use General.el
;(use-package general)
(use-package general
  :config
  (general-create-definer personal/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (personal/leader-keys
    "a" '(org-agenda :which-key "agenda")
    "t"  '(:ignore t :which-key "toggles")
    "fc" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.el")))
    "tt" '(counsel-load-theme :which-key "choose theme")))
(general-define-key
 "C-c p" 'check-parens
 "C-c s" 'completion-at-point
 "C-c t" 'counsel-org-tag
 "C-c b" 'org-toggle-checkbox
 "C-c c" 'calendar
 "C-c q" 'org-capture
 "C-c a" 'org-agenda)
;; Evil mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) 

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (evil-set-initial-state 'dashboard-mode 'normal)) 
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
;; Setting Hydra
;(use-package hydra
;  :defer t)
;
;(defhydra hydra-text-scale
;  "scale text"
;  ("j" text-scale-increase "in")
;  ("k" text-scale-decrease "out")
;  ("=" nil "finished" :exit t))
;(personal/leader-keys
;  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Org mode
(defun personal/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))
(defun personal/org-font-setup ()
  ; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0))))

  ; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))
; Calling Org
(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . personal/org-mode-setup)
  :config (setq org-ellipsis " ▼ ")
  (personal/org-font-setup))
;; Set Org-Agenda
(setq org-agenda-files
      '("~/documents/org/agenda.org"
        "~/documents/org/anniversaries.org"
        "~/documents/org/appointments.org"
        "~/documents/org/tasks.org"
        "~/documents/org/habits.org"))
; Set agenda-view to 2 weeks
(setq org-agenda-span 14)
; Setting org-habits
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)
; Extra stuff
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))
; Archive DONE tasks
(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))
; Set level bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
; Visual Padding
(defun personal/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1))
(use-package visual-fill-column
  :hook (org-mode . personal/org-mode-visual-fill))
; Set Org Capture Templates
  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/documents/org/tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/documents/org/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/documents/org/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/documents/org/journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/documents/org/metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))
; Include diary for holidays and anniversaries
(setq org-agenda-include-diary t)
; Orthodox Feasts
(eval-after-load 'calendar
  '(load "~/code/emacs-lisp/orthodox-feasts.el" t t))

; Org Babel
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp . t)
     (haskell . t)
     (latex . t)
     (python . t)
     (C . t)
     (R . t)
     (makefile . t)
     (shell . t)
     (awk . t))))
(setq org-confirm-babel-evaluate nil)

;; Org tempo
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/work")
    (setq projectile-project-search-path '("~/work")))
  (setq projectile-switch-project-action #'projectile-dired))
; Counsel-Projectile Integration
(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))
;; Load Magit
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer #'magit-display-buffer-same-window-except-diff-v1))
; Use Forge
;(use-package forge
;  :after magit)

;; Using lsp-mode
(defun personal/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . personal/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))
;; LaTeX lsp
(use-package lsp-latex
  :after lsp)
(setq lsp-latex-texlab-executable "~/.nix-profile/bin/texlab")
(with-eval-after-load "tex-mode"
 (add-hook 'tex-mode-hook 'lsp)
 (add-hook 'latex-mode-hook 'lsp))
;; for bibtex
(with-eval-after-load "bibtex"
 (add-hook 'bibtex-mode-hook 'lsp))
;; Common Lisp lsp
(use-package sly
  :mode "\\.lisp\\'")
(use-package slime
  :mode "\\.lisp\\'")
(setq inferior-lisp-program "sbcl")
;;; Haskell lsp
;(use-package lsp-haskell)
;;; Setting hooks to trigger LSP setup and set the server PATH
;(add-hook 'haskell-mode-hook #'lsp)
;(add-hook 'haskell-literate-mode-hook #'lsp)
;(setq lsp-haskell-server-path "~/.nix-profile/bin/haskell-language-server")
;; C/C++ lsp
;(use-package ccls
;  :hook lsp c-mode c++-mode objc-mode)
;(setq ccls-executable "~/.nix-profile/bin/ccls")

;; Company
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.5))
;; Flycheck 
(use-package flycheck
  :hook lsp-mode)
(use-package smartparens
;  :hook prog-mode
  :custom smartparens-global-mode t)

; Give term more colours!
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))
 
;; File Management
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))
;; Dired Single
(use-package dired-single
  :commands (dired dired-jump))
;; Dired Icons
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
;; Dired Open
(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "sxiv")("jpg" . "sxiv")("jpeg" . "sxiv")("gif" . "sxiv")("xmp" . "sxiv")
				("ps" . "zathura")("eps" . "zathura")("pdf" . "zathura")("djvu" . "zathura")
				("avi" . "mpv")("mp4" . "mpv")("wmv" . "mpv")("mpg" . "mpv")("mpeg" . "mpv")("mov" . "mpv")("webm" . "mpv")("mkv" . "mpv"))))
;; Hide dotfiles
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(python-mode ivy-prescient no-littering dired-hide-dotfiles dired-open all-the-icons-dired dired-single eterm-256color smartparens flycheck ccls company lsp-latex sly slime lsp-haskell haskell-mode lsp-mode evil-magit magit counsel-projectile projectile visual-fill-column org-bullets evil-collection evil general helpful which-key rainbow-delimiters doom-themes doom-modeline all-the-icons ivy-rich counsel command-log-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
