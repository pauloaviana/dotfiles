;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-oceanic-next)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/documents/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq org-todo-keywords '(sequence ))
;; Set default browser to handle links
;;(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "brave")
;; Set org-agenda files
(setq org-agenda-files (list "~/documents/org/agenda.org"))
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; Archive DONE tasks
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Trigger completion on Shift-Space
(global-set-key (kbd "C-x l") #'company-complete)
(put 'set-goal-column 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#14232D" "#EC5f67" "#99C794" "#FAC863" "#6699CC" "#E27E8D" "#5FB3B3" "#D8DEE9"])
 '(custom-safe-themes
   (quote
    ("b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" default)))
 '(fci-rule-color "#C0C5CE")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2B34" "#FAC863"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2B34" "#99C794"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2B34" "#A7ADBA"))
 '(objed-cursor-color "#EC5f67")
 '(pdf-view-midnight-colors (cons "#D8DEE9" "#1B2B34"))
 '(rustic-ansi-faces
   ["#1B2B34" "#EC5f67" "#99C794" "#FAC863" "#6699CC" "#E27E8D" "#5FB3B3" "#D8DEE9"])
 '(vc-annotate-background "#1B2B34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#99C794")
    (cons 40 "#b9c783")
    (cons 60 "#d9c773")
    (cons 80 "#FAC863")
    (cons 100 "#f9b55f")
    (cons 120 "#f9a35b")
    (cons 140 "#F99157")
    (cons 160 "#f18a69")
    (cons 180 "#e9847b")
    (cons 200 "#E27E8D")
    (cons 220 "#e57380")
    (cons 240 "#e86973")
    (cons 260 "#EC5f67")
    (cons 280 "#da727b")
    (cons 300 "#c98690")
    (cons 320 "#b899a5")
    (cons 340 "#C0C5CE")
    (cons 360 "#C0C5CE")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
