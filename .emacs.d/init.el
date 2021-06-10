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
;; remove startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
;; no blinking
(blink-cursor-mode 0)



;;;;;;;;;;;;;;
;; Org-mode ;; 
;;;;;;;;;;;;;;
(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file "~/Dropbox/org/organizer.org")))
(global-set-key (kbd "C-c i")
		(lambda () (interactive) (find-file "~/Dropbox/org/inbox.org")))
;; (defun james/style-org ()
;;   (setq line-spacing 0.2)
;;   (variable-pitch-mode +1)
;;   (mapc
;;    (lambda (face) ;; Other fonts with fixed-pitch.
;;      (set-face-attribute face nil :inherit 'fixed-pitch))
;;    (list 'org-code
;;          'org-block
;;          'org-table
;;          'org-verbatim
;;          'org-block-begin-line
;;          'org-block-end-line
;;          'org-meta-line
;;          'org-document-info-keyword)))
(use-package org
  :ensure t
  :mode ("\\org\\'" . org-mode)
  :init
  (add-hook 'org-mode-hook 'org-indent-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture))
  :custom
  (org-src-window-setup 'current-window)
  (org-return-follows-link t)
  (org-agenda-diary-file "~/Dropbox/org/diary.org")
  ;;    (org-babel-load-languages
  ;;   '((emacs-lisp . t)
  ;;     (python . t)
  ;;     (dot . t)))
  (org-confirm-babel-evaluate nil)
  (org-catch-invisible-edits 'show)
  (org-preview-latex-image-directory "/tmp/ltximg/")
  (org-startup-indented nil)
  (org-startup-truncated nil)
  (org-startup-folded nil)
  (org-hide-leading-stars nil)
  (org-hide-emphasis-markers nil)
  (org-pretty-entities nil)
  (org-adapt-indentation nil)
  :custom-face
  ;;  (variable-pitch ((t (:family "Libre Baskerville"))))
  (org-document-title ((t (:weight bold :height 1.5))))
  (org-done ((t (:strike-through t :weight bold))))
  (org-headline-done ((t (:strike-through t))))
  (org-level-1 ((t (:height 1.3 :weight bold))))
  (org-level-2 ((t (:height 1.2))))
  ;;  (org-level-3 ((t (:height 1.1))))
  (org-image-actual-width (/ (display-pixel-width) 2))
  :config
  (setq org-default-notes-file "~/Dropbox/org/organizer.org"
	initial-buffer-choice "~/Dropbox/org/organizer.org"
	james/org-agenda-directory "~/Dropbox/org/"
	org-capture-templates
	`(("i" "inbox" entry (file ,(concat james/org-agenda-directory "inbox.org"))
           "*** TODO %?")
          ("e" "email" entry (file+headline ,(concat james/org-agenda-directory "emails.org") "Emails")
           "* TODO [#A] Reply: %a :@home:" :immediate-finish t)
          ("l" "link" entry (file ,(concat james/org-agenda-directory "inbox.org"))
           "* TODO %(org-cliplink-capture)" :immediate-finish t)
          ("c" "org-protocol-capture" entry (file ,(concat james/org-agenda-directory "inbox.org"))
           "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)))
  ;;  :hook (org-mode-hook . james/style-org))

  )

;;;;;;;;;;;;;;;;
;; Org-Agenda ;;
;;;;;;;;;;;;;;;;
(use-package org-agenda
  :after org
  :custom
  (org-agenda-prefix-format '((agenda . " %i %-20:c%?-12t%-6e% s")
			      (todo   . " %i %-20:c %-6e")
			      (tags   . " %i %-20:c")
			      (search . " %i %-20:c")))
  :config
  (setq org-agenda-custom-commands
	'(("d" "Today's Tasks"
	   ((tags-todo
	     "GHD+ACTIVE+PRIORITY=\"A\""
	     ((org-agenda-files '("~/Dropbox/org/goals.org"))
	      (org-agenda-overriding-header "Primary goals this month")))
	    (tags-todo
	     "GHD+ACTIVE+PRIORITY=\"C\""
	     ((org-agenda-files '("~/Dropbox/org/goals.org"))
	      (org-agenda-overriding-header "Secondary goals this month")))
	    (agenda "" ((org-agenda-span 1)
			(org-agenda-overriding-header "Today's tasks")))))

	  ("w" "This Week's Tasks"
	   ((tags-todo
	     "GHD+ACTIVE+PRIORITY=\"A\""
	     ((org-agenda-files '("~/Dropbox/org/goals.org"))
	      (org-agenda-overriding-header "Primary goals this month")))
	    (tags-todo
	     "GHD+ACTIVE+PRIORITY=\"C\""
	     ((org-agenda-files '("~/Dropbox/org/goals.org"))
	      (org-agenda-overriding-header "Secondary goals this month")))
	    (agenda))))))

;;;;;;;;;;;;;;
;; Org-Roam ;;
;;;;;;;;;;;;;;
(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n j" . org-roam-jump-to-index)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))




;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom-set-variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-adapt-indentation nil)
 '(org-agenda-diary-file "~/Dropbox/org/diary.org")
 '(org-agenda-files '("~/PhD/master_plan.org" "~/Dropbox/org/organizer.org"))
 '(org-agenda-prefix-format
   '((agenda . " %i %-20:c%?-12t%-6e% s")
     (todo . " %i %-20:c %-6e")
     (tags . " %i %-20:c")
     (search . " %i %-20:c")))
 '(org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-keep)
     (todo priority-down category-keep todo-state-down)
     (tags priority-down category-keep)
     (search category-keep)))
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
 '(org-roam-directory "~/org/")
 '(org-src-window-setup 'current-window)
 '(org-startup-folded nil)
 '(org-startup-indented nil)
 '(org-startup-truncated nil)
 '(org-todo-keywords '((sequence "TODO(t)" "DONE(d)")))
 '(package-selected-packages
   '(outline-magic counsel flyspell-correct-ivy good-scroll conda elpy doom-modeline magit vscode-dark-plus-theme neotree treemacs jetbrains-darcula-theme gruvbox-theme tron-legacy-theme auctex org-roam undo-tree))
 '(safe-local-variable-values '((TeX-master . Main))))




;;;;;;;;;;;
;; Latex ;;
;;;;;;;;;;;
;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
;; C-x p RET for compiling and previewing pdf in LaTeX
;; (defun reload-pdf ()
;;  (interactive
;;  (let* ((fname buffer-file-name)
;;        (fname-no-ext (substring fname 0 -4))
;;        (pdf-file (concat fname-no-ext ".pdf"))
;;        (cmd (format "pdflatex %s" fname)))
;;    (delete-other-windows)
;;    (split-window-horizontally)
;;    (split-window-vertically)
;;    (shell-command cmd)
;;    (other-window 2)
;;    (find-file pdf-file)
;;    (balance-windows))))
;;
;;(global-set-key "\C-x\p" 'reload-pdf)

;;(load "auctex.el" nil t t)
(setq-default TeX-master nil)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq reftex-plug-into-AUCTeX t)
(setq LaTeX-item-indent 0)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)


;;;;;;;;;;;;;;;;;;;;;;
;; Custom-set-faces ;;
;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;
;; Themes ;;
;;;;;;;;;;;;
;;; Gruvbox theme
;; (load-theme 'gruvbox-dark-hard t)
 (load-theme 'gruvbox-light-hard t)
;;; Jetbrains darcula theme
;; (load-theme 'jetbrains-darcula t)
;;; VSCode dark plus
;; (use-package vscode-dark-plus-theme
;;   :ensure t
;;   :config
;;   (load-theme
;;   'vscode-dark-plus t))

;;;;;;;;;;;;;
;; Neotree ;;
;;;;;;;;;;;;;
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;;;;;;;;;;;;;;;;;;;
;; Doom-modeline ;;
;;;;;;;;;;;;;;;;;;;
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-github t))


;;;;;;;;;;
;; elpy ;;
;;;;;;;;;;
(elpy-enable)

;;;;;;;;;;;;
;; pyvenv ;;
;;;;;;;;;;;;
(setenv "WORKON_HOME" "~/miniconda3/envs")
(pyvenv-mode 1)

;;;;;;;;;;;;;;;;;
;; good-scroll ;;
;;;;;;;;;;;;;;;;;
(good-scroll-mode 1)

;;;;;;;;;;;;;;
;; ivy mode ;;
;;;;;;;;;;;;;;
(use-package counsel
  :demand t
  :bind
  (("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-other-window)
   ("C-x k" . kill-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-x l" . counsel-locate)
   ("C-c j" . counsel-git)
   ("C-c s" . counsel-rg)
   ("M-y" . counsel-yank-pop)
   :map help-map
   ("f" . counsel-describe-function)
   ("v" . counsel-describe-variable)
   ("l" . counsel-info-lookup-symbol)
   :map ivy-minibuffer-map
   ("C-o" . ivy-occur)
   ("<return>" . ivy-alt-done)
   ("M-<return>" . ivy-immediate-done)
   :map read-expression-map
   ("C-r" . counsel-minibuffer-history))
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-display-style 'fancy)
  (ivy-use-selectable-prompt t)
  (ivy-re-builders-alist
   '((t . ivy--regex-plus)))
  :config
  (ivy-mode 1))
;; swiper
(global-set-key "\C-s" 'swiper)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fly-spell with hunspell backend ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ispell-program-name (executable-find "hunspell")
      ispell-dictionary "en_US")
(use-package flyspell-correct-ivy
  :ensure t
  :demand t
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))



(defun zathura-forward-search ()
  ;; Open the compiled pdf in Zathura with synctex. This is complicated since
  ;; 1) Zathura refuses to acknowledge Synctex directive if the pdf is not
  ;; already opened
  ;; 2) This means we have to bookkeep open Zathura processes ourselves: first
  ;; open a new pdf from the beginning, if it is not already open. Then call
  ;; Zathura again with the synctex directive.
  (interactive)
  (let* ((zathura-launch-buf (get-buffer-create "*Zathura Output*"))
         (pdfname (TeX-master-file "pdf"))
         (zatentry (assoc pdfname zathura-procs))
         (zatproc (if (and zatentry (process-live-p (cdr zatentry)))
                      (cdr zatentry)
                    (progn
                      (let ((proc (progn (message "Launching Zathura")
                                         (start-process "zathura-launch"
                                                        zathura-launch-buf "zathura"
                                                        "-x" "emacsclient +%{line} %{input}" pdfname))))
                        (when zatentry
                          (setq zathura-procs (delq zatentry zathura-procs)))
                        (add-to-list 'zathura-procs (cons pdfname proc))
                        (set-process-query-on-exit-flag proc nil)
                        proc))))
         (pid (process-id zatproc))
         (synctex (format "%s:0:%s"
                          (TeX-current-line)
                          (TeX-current-file-name-master-relative)))
         )
    (start-process "zathura-synctex" zathura-launch-buf "zathura" "--synctex-forward" synctex pdfname)
    (start-process "raise-zathura-wmctrl" zathura-launch-buf "wmctrl" "-a" pdfname)
    ))

(defun my-LaTeX-mode()
  (TeX-source-correlate-mode)        ; activate forward/reverse search
  (TeX-PDF-mode)
  (add-to-list 'TeX-view-program-list '("zathura" zathura-forward-search))
  (setq TeX-view-program-selection (quote ((output-pdf "zathura") (output-dvi "xdvi"))))
  (setq zathura-procs ())
  (outline-minor-mode)
  )
(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode)

;;;;;;;;;;;;;;;;;;;
;; outline-magic ;;
;;;;;;;;;;;;;;;;;;;
(eval-after-load 'outline
  '(progn
    (require 'outline-magic)
    (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)))
