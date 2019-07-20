 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  __  ______  ___  ____  ______ __ __ ____  ;;
;; (( \ | || | // \\ || \\ | || | || || || \\ ;;
;;  \\    ||   ||=|| ||_//   ||   || || ||_// ;;
;; \_))   ||   || || || \\   ||   \\_// ||    ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup packages and package handler ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/"))
      )

(package-initialize)
(setq inhibit-startup-screen t)
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;;(package-install 'org-plus-contrib)
;;;;;;;;;;;;;;;;;;;;
;; Setup of Theme ;;
;;;;;;;;;;;;;;;;;;;;
;; Set default font
(set-face-attribute 'default nil
                    :family "Fira Mono"
                    :height 100
                    :weight 'normal
                    :width 'normal)


;; need for doom modeline to work correctly
(use-package doom-themes)

(load-theme 'doom-dracula t)
(load-theme 'org-beautify t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(fringe-mode 1)
(setq use-dialog-box nil)
(setq exwm-floating-border-width 3)
(setq exwm-floating-border-color "orange")

(global-prettify-symbols-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; currently installed themes (only dark and doom ones) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iosvkem, challenger-deep, city-light, dracula, molokai, nord,
;; nova, one, opera, peacock, sourcerer, spacegrey, tomorrow-day,
;; tomorrow-night, vibrant

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup beginning looks ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; may make this file the set up script, but eh
;;(load "~/.emacs.d/my_ibuffer.el")

(require 'org)
(org-babel-load-file
 (expand-file-name "packages.org"
                   user-emacs-directory))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ____ __ __ __  __   ___ ______ __   ___   __  __  __  ;;
;; ||    || || ||\ ||  //   | || | ||  // \\  ||\ || (( \ ;;
;; ||==  || || ||\\|| ((      ||   || ((   )) ||\\||  \\  ;;
;; ||    \\_// || \||  \\__   ||   ||  \\_//  || \|| \_)) ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal: set up with terminal ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun terminal ()
  "Switch to terminal. Launch if nonexistent."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer "*ansi-term*")
    (ansi-term "/bin/bash"))
  (get-buffer-process "*ansi-term*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function to set up jumping to terminal with ` in ;;
;; the terminal (NEED TO WORK ON)		    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/dired+.el")
(setq diredp-hide-details-initially-flag nil)
(setq diredp-hide-details-propagate-flag nil)
(diredp-toggle-find-file-reuse-dir 1)
(add-hook `dired-mode-hook `all-the-icons-dired-mode)

(defun dired-open-term ()
  ;;   "Open an `ansi-term' that corresponds to current directory."
  (interactive)
  (let ((current-dir (dired-current-directory)))
    (term-send-string
     (terminal)
     (if (file-remote-p current-dir)
	 (progn (switch-to-buffer "*uwcern*")
		(format "cd '%s'\n" current-dir)))
     ;; (if (file-remote-p current-dir)
     ;;     (let ((v (tramp-dissect-file-name current-dir t)))
     ;;       (format "ssh %s@%s\n"
     ;;               (aref v 1) (aref v 2)))
     ;;   (format "cd '%s'\n" current-dir))
     ;; )))
     )))
(define-key dired-mode-map (kbd "`") 'dired-open-term)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command to kill terminal buffer after it's finished ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (or (string= event "finished\n") (string= event "logout\n"))
            (kill-buffer ,buff))))))
(add-hook 'term-exec-hook 'oleh-term-exec-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; change opacity to make it look more slick ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-frame-alpha (arg &optional active)
  (interactive "nEnter alpha value (1-100): \np")
  (let* ((elt (assoc 'alpha default-frame-alist))
         (old (frame-parameter nil 'alpha))
         (new (cond ((atom old)     `(,arg ,arg))
                    ((eql 1 active) `(,arg ,(cadr old)))
                    (t              `(,(car old) ,arg)))))
    (if elt (setcdr elt new) (push `(alpha ,@new) default-frame-alist))
    (set-frame-parameter nil 'alpha new)))
(global-set-key (kbd "C-c t") 'set-frame-alpha)

(set-frame-parameter nil 'alpha '(90 . 80))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup a desktop-esque environment (ie go transparent) ;;
;; use to look at desktop picture			 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun desktop ()
  (interactive)
  (setq tmper (frame-parameter nil 'alpha))
  ;; (setq tmp-alpha (car (frame-parameter nil 'alpha))
  ;; 	(print tmp-alpha)
  (eyebrowse-switch-to-window-config-0)
  (switch-to-buffer "*scratch*")
  (set-frame-parameter nil 'alpha 1)
  (setq tmpabc nil)
  (while (not (string= tmpabc "q"))
    (setq tmpabc (read-key-sequence "")))
  (set-frame-parameter nil 'alpha tmper)
  )
(global-set-key (kbd "M-]") 'desktop)





 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ___  ___ __  __    ___  ____ __    __     ___  __  __  ____   ___   __ __  __  ;;
;; ||\\//|| || (( \  //   ||    ||    ||    // \\ ||\ || ||     // \\  || || (( \ ;;
;; || \/ || ||  \\  ((    ||==  ||    ||    ||=|| ||\\|| ||==  ((   )) || ||  \\  ;;
;; ||    || || \_))  \\__ ||___ ||__| ||__| || || || \|| ||___  \\_//  \\_// \_)) ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Allow pasting from other programs
(setq x-select-enable-clipboard t)
(setq term-scroll-show-maximum-output 0)
(setq password-cache-expiry nil)


;;;;;;;;;;;;;;;;;;
;; Set up Dired ;;
;;;;;;;;;;;;;;;;;;
(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'dired-mode-hook (lambda ()))
;;; hide hidden files and emacs backup files
(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$")
(setq-default dired-omit-files-p t)
(global-set-key (kbd "C-:") `dired-jump)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
					;(load 'init-csv)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Inconsolata" :slant normal :weight normal :height 1.0 :width normal))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "Source Sans Pro" :height 180 :weight light)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up Org mode things ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key org-mode-map (kbd "C-'") nil)
(setq org-agenda-files (list "~/org-todo/"))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-src-fontify-natively t)
(setq org-file-apps
      '(("\\.docx\\'" . default)
	("\\.odt\\'" . default)
	("\\.mm\\'" . default)
	("\\.x?html?\\'" . default)
	("\\.pdf\\'" . default)
	("\\.pdf::\\([0-9]+\\)\\'" . "evince \"%s\" -p %1")
	(auto-mode . emacs)))

(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;; (custom-theme-set-faces
;;  'user
;;  '(variable-pitch ((t (:family "Source Sans Pro" :height 180 :weight light))))
;;  '(fixed-pitch ((t ( :family "Inconsolata" :slant normal :weight normal :height 1.0 :width normal)))))
;; (add-hook 'org-mode-hook 'variable-pitch-mode)

(custom-theme-set-faces
 'user
 '(org-block                 ((t (:inherit fixed-pitch))))
 '(org-document-info         ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-link                  ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value        ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim              ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent                ((t (:inherit (org-hide fixed-pitch))))))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("249f23540cc235fde8dd2035f1cca23f18e6817650c86765a1b808f3a3712a87" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "9954ed41d89d2dcf601c8e7499b6bb2778180bfcaeb7cdfc648078b8e05348c6" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "cc4e95929ebadaa3f2e9ac462b6594f02c41742efbe8505335f5a67b36cec21c" "4639288d273cbd3dc880992e6032f9c817f17c4a91f00f3872009a099f5b3f84" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default)))
 '(doom-modeline-mode t)
 '(openwith-mode t)
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (C . t)
     (css . t)
     (ruby . t)
     (shell . t)
     (awk . t)
     (R . t))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(package-selected-packages
   (quote
    (company-auctex latex-pretty-symbols projectile avy-flycheck flycheck 0blayout pretty-mode desktop-environment org-chef org-journal org-password-manager org-bullets scratch scratch-ext eyebrowse mode-icons all-the-icons-ivy all-the-icons-dired dmenu exwm-x exwm lorem-ipsum irony rainbow-delimiters doom-themes eterm-256color quelpa-use-package quelpa god-mode dired+ buffer-move dired-x ace-jump-mode doom-modeline zoom window-number use-package try tramp-term smex smartparens resize-window pdf-tools ox-gfm org-beautify-theme org-babel-eval-in-repl openwith multiple-cursors minesweeper markdown-mode+ magit ibuffer-tramp helm google-translate fit-frame evil elpy dracula-theme csv-mode counsel celestial-mode-line auctex aggressive-indent adaptive-wrap)))
 '(zoom-size (quote (0.618 . 0.618))))


;; setup final look

(setup-eyebrowseWS)

(use-package all-the-icons
  :ensure t
  ;;  :config (all-the-icons-install-fonts)
  )
(use-package all-the-icons-ivy
  :init (all-the-icons-ivy-setup))




