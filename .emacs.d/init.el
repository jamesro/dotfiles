;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))


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
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)


;;;;;;;;;;;;;;
;; Org-mode ;; 
;;;;;;;;;;;;;;
(setq org-default-notes-file "~/Dropbox/organizer.org")
(define-key global-map "\C-cc" 'org-capture)
(setq initial-buffer-choice "~/Dropbox/organizer.org")
(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file "~/Dropbox/organizer.org")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Dropbox/organizer.org")))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 6))))
 '(org-todo-keywords (quote ((sequence "TODO(t)" "DONE(d)"))))
 '(package-selected-packages (quote (auctex org-roam undo-tree))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq james/org-agenda-directory "~/org/")
(setq org-capture-templates
      `(("i" "inbox" entry (file ,(concat james/org-agenda-directory "inbox.org"))
         "* TODO %?")
        ("e" "email" entry (file+headline ,(concat james/org-agenda-directory "emails.org") "Emails")
         "* TODO [#A] Reply: %a :@home:" :immediate-finish t)
        ("l" "link" entry (file ,(concat james/org-agenda-directory "inbox.org"))
         "* TODO %(org-cliplink-capture)" :immediate-finish t)
        ("c" "org-protocol-capture" entry (file ,(concat james/org-agenda-directory "inbox.org"))
         "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)))






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
