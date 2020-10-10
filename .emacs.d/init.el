;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/") t)) ;; many packages won't show if using stable
  ;; '("melpa" . "https://melpa.milkbox.net/packages/") t))


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Bootstrap `use-package`
;; for org-roam
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)
;; Disable the horrid auto-save
(setq auto-save-default nil)
;; Disable the menu bar since we don't use it, especially not in the
;; terminal
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))

;; Don't ring the bell
(setq ring-bell-function 'ignore)

;; Don't show tool-bar, scroll-bar
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; full screen by default
;; (setq initial-frame-alist (quote ((fullscreen . maximized))))

;; remove startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; relative line numbering
;(global-display-line-numbers-mode 1)
;(setq display-line-numbers-type 'relative)


;;;;;;;;;;;;;;
;; Org-mode ;; 
;;;;;;;;;;;;;;
(use-package org
  :mode ("\\org\\'" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture))
  :custom
  (org-src-window-setup 'current-window)
  (org-return-follows-link t)
  (org-agenda-diary-file "~/org/diary.org")
;    (org-babel-load-languages
;   '((emacs-lisp . t)
;     (python . t)
;     (dot . t)))
  (org-confirm-babel-evaluate nil)
  (org-catch-invisible-edits 'show)
  (org-preview-latex-image-directory "/tmp/ltximg/")
  :custom-face
;  (variable-pitch ((t (:family "Libre Baskerville"))))
  (org-document-title ((t (:weight bold :height 1.5))))
  (org-done ((t (:strike-through t :weight bold))))
  (org-headline-done ((t (:strike-through t))))
  (org-level-1 ((t (:height 1.3 :weight bold))))
  (org-level-2 ((t (:height 1.2))))
;  (org-level-3 ((t (:height 1.1))))
  (org-image-actual-width (/ (display-pixel-width) 2))
  :custom
;  (org-startup-indented nil)
  (org-hide-leading-stars nil)
  (org-hide-emphasis-markers nil)
  (org-pretty-entities nil)
  (org-adapt-indentation nil))

(require 'org)


(defun james/style-org ()
  (setq line-spacing 0.2)
  (variable-pitch-mode +1)
  (mapc
   (lambda (face) ;; Other fonts with fixed-pitch.
     (set-face-attribute face nil :inherit 'fixed-pitch))
   (list 'org-code
         'org-block
         'org-table
         'org-verbatim
         'org-block-begin-line
         'org-block-end-line
         'org-meta-line
         'org-document-info-keyword)))

(add-hook 'org-mode-hook #'james/style-org)



(setq org-default-notes-file "~/Dropbox/organizer.org")
(setq initial-buffer-choice "~/Dropbox/organizer.org")
(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file "~/Dropbox/organizer.org")))
(global-set-key (kbd "C-c i")
		(lambda () (interactive) (find-file "~/org/inbox.org")))

(setq james/org-agenda-directory "~/org/")
(setq org-capture-templates
      `(("i" "inbox" entry (file ,(concat james/org-agenda-directory "inbox.org"))
         "*** TODO %?")
        ("e" "email" entry (file+headline ,(concat james/org-agenda-directory "emails.org") "Emails")
         "* TODO [#A] Reply: %a :@home:" :immediate-finish t)
        ("l" "link" entry (file ,(concat james/org-agenda-directory "inbox.org"))
         "* TODO %(org-cliplink-capture)" :immediate-finish t)
        ("c" "org-protocol-capture" entry (file ,(concat james/org-agenda-directory "inbox.org"))
         "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)))





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-adapt-indentation nil)
 '(org-agenda-diary-file "~/org/diary.org")
 '(org-agenda-files '("~/Dropbox/organizer.org"))
 '(org-catch-invisible-edits 'show)
 '(org-confirm-babel-evaluate nil)
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.2 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-hide-emphasis-markers nil)
 '(org-hide-leading-stars nil)
 '(org-pretty-entities nil)
 '(org-preview-latex-image-directory "/tmp/ltximg/")
 '(org-refile-targets '((org-agenda-files :maxlevel . 2)))
 '(org-return-follows-link t)
 '(org-roam-directory "/home/james/org/")
 '(org-src-window-setup 'current-window)
 '(org-todo-keywords '((sequence "TODO(t)" "DONE(d)")))
 '(package-selected-packages
   '(jetbrains-darcula-theme gruvbox-theme tron-legacy-theme auctex org-roam undo-tree)))



;;;;;;;;;;
;; misc ;;
;;;;;;;;;;
;;(split-window-right)              ;; C-x 3
;;(other-window 1)                              ;; C-x 0
;; toggle enable-local-variables :all           ;; Load *all* locals.
    ;; toggle org-confirm-babel-evaluate nil    ;; Eval *all* blocks.
;;      (find-file "~/.emacs.d/init.org")


;;;;;;;;;;;
;; Latex ;;
;;;;;;;;;;;
;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
;; C-x p RET for compiling and previewing pdf in LaTeX
;(defun reload-pdf ()
;  (interactive
;  (let* ((fname buffer-file-name)
;        (fname-no-ext (substring fname 0 -4))
;        (pdf-file (concat fname-no-ext ".pdf"))
;        (cmd (format "pdflatex %s" fname)))
;    (delete-other-windows)
;    (split-window-horizontally)
;    (split-window-vertically)
;    (shell-command cmd)
;    (other-window 2)
;    (find-file pdf-file)
;    (balance-windows))))
;
;(global-set-key "\C-x\p" 'reload-pdf)

;(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)


;;;;;;;;;;;;;;
;; Org-Roam ;;
;;;;;;;;;;;;;;
(use-package org-roam
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "/home/james/org/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n j" . org-roam-jump-to-index)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))


;; Tron legacy theme https://github.com/ianpan870102/tron-legacy-emacs-theme
;(unless (package-installed-p 'tron-legacy-theme)
;  (package-refresh-contents)
;  (package-install 'tron-legacy-theme))
;(use-package tron-legacy-theme
;  :config
;  (setq tron-legacy-theme-softer-bg t)
;  (setq tron-legacy-theme-vivid-cursor t)
;  (load-theme 'tron-legacy t))


;; gruvbox theme
(load-theme 'gruvbox t)
;; jetbrains darcula theme
;(load-theme 'jetbrains-darcula t)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:weight bold :height 1.5))))
 '(org-done ((t (:strike-through t :weight bold))))
 '(org-headline-done ((t (:strike-through t))))
 '(org-image-actual-width (/ (display-pixel-width) 2))
 '(org-level-1 ((t (:height 1.3 :weight bold))))
 '(org-level-2 ((t (:height 1.2)))))
