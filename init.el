 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;  __  ______  ___  ____  ______ __ __ ____  ;;
 ;; (( \ | || | // \\ || \\ | || | || || || \\ ;;
 ;;  \\    ||   ||=|| ||_//   ||   || || ||_// ;;
 ;; \_))   ||   || || || \\   ||   \\_// ||    ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                          
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


;;;;;;;;;;;;;;;;;;;;
;; Setup of Theme ;;
;;;;;;;;;;;;;;;;;;;;
(load-theme 'dracula t)
(load-theme 'org-beautify t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; currently installed themes (only dark ones) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; deeper-blue, dracula, manoj-dark, misterioso, org-beautify, 
   ;; shrek, tango-dark, tsdh-dark, wombat

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup beginning looks ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; may make this file the set up script, but eh
;;(load "~/.emacs.d/my_ibuffer.el")

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;  ____ __ __ __  __   ___ ______ __   ___   __  __  __  ;;
 ;; ||    || || ||\ ||  //   | || | ||  // \\  ||\ || (( \ ;;
 ;; ||==  || || ||\\|| ((      ||   || ((   )) ||\\||  \\  ;;
 ;; ||    \\_// || \||  \\__   ||   ||  \\_//  || \|| \_)) ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal: set up with terminal ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun terminal ()
  "Switch to terminal. Launch if nonexistent."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer "*ansi-term*")
    (ansi-term "/bin/zsh"))
  (get-buffer-process "*ansi-term*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function to set up jumping to terminal with ` in ;;
;; the terminal (NEED TO WORK ON)		    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(require `dired-x)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow certain functions to work in terminal ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'term)
(defun expose-global-binding-in-term (binding)
   (define-key term-raw-map binding 
     (lookup-key (current-global-map) binding)))

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


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; ____   ___    ___ __ __  ___    ___   ____  __  ;;
 ;; || \\ // \\  //   || // // \\  // \\ ||    (( \ ;;
 ;; ||_// ||=|| ((    ||<<  ||=|| (( ___ ||==   \\  ;;
 ;; ||    || ||  \\__ || \\ || ||  \\_|| ||___ \_)) ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Ace Window	                ;;
;; Allows nice switching between frames/windows ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
  :ensure t
  :config
  (progn
    (global-set-key (kbd "M-o") 'ace-window)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

;;Extra bit of code to allow Ace window to work in terminal
(expose-global-binding-in-term (kbd "M-o"))

;; Elpy


(use-package elpy
  :ensure t
  :config
  (progn
    (elpy-enable)))


(use-package buffer-move
  :ensure t
  :config
  (progn
    (global-set-key (kbd "S-<left>")  'buf-move-left)
    (global-set-key (kbd "S-<right>") 'buf-move-right)
    (global-set-key (kbd "S-<up>")    'buf-move-up)
    (global-set-key (kbd "S-<down>")  'buf-move-down)
    ))



(add-hook 'after-init-hook 'global-company-mode)

(setq password-cache-expiry nil)

(global-set-key (kbd "M-g M-g") 'avy-goto-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  Swiper   (Needs Counsel)			      ;;
;; Package for finding anything! Works with searching, buffer change, ;;
;; finding of a file, and M-x stuff. Very pretty!		      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; it looks like counsel is a requirement for swiper
(use-package counsel
  :ensure t
  )

(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        SmartParens				    ;;
;; Very finicky, but can be nice for not worrying about parentheses ;;
;; Added C-' for moving paren forward 				    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smartparens
  :ensure t
  :config
  (progn
    (smartparens-global-mode t)
    (global-set-key (kbd "C-'") 'sp-forward-slurp-sexp)
    ))



(use-package resize-window
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-c ;") 'resize-window)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Zoom				  ;;
;; Has balancing of windows. Need to fix it with dired mode,	  ;;
;; but looks nice. It's set to have a Golden Ratio, may change... ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package zoom
  :ensure t
  :config
  (progn
    (zoom-mode t)
    (custom-set-variables
     '(zoom-size '(0.618 . 0.618))
;;     '(zoom-ignored-major-modes '(dired-mode))
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Doom Modeline		    ;;
;; Changes the bottome line to look slicker ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  )

;;;; NEED TO edit this to open correct stuff!
;;https://bitbucket.org/jpkotta/openwith/src/default/README.txt
(use-package openwith
  :ensure t
  :config
  (openwith-mode t)
  )


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; ___  ___ __  __    ___  ____ __    __     ___  __  __  ____   ___   __ __  __  ;;
 ;; ||\\//|| || (( \  //   ||    ||    ||    // \\ ||\ || ||     // \\  || || (( \ ;;
 ;; || \/ || ||  \\  ((    ||==  ||    ||    ||=|| ||\\|| ||==  ((   )) || ||  \\  ;;
 ;; ||    || || \_))  \\__ ||___ ||__| ||__| || || || \|| ||___  \\_//  \\_// \_)) ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Allow pasting from other programs
(setq x-select-enable-clipboard t)
(setq term-scroll-show-maximum-output 0)






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
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up Org mode things ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-agenda-files (list "~/Desktop/TODO/"))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-file-apps
      '(("\\.docx\\'" . default)
       ("\\.odt\\'" . default)
      ("\\.mm\\'" . default)
      ("\\.x?html?\\'" . default)
      ("\\.pdf\\'" . default)
      ("\\.pdf::\\([0-9]+\\)\\'" . "evince \"%s\" -p %1")
      (auto-mode . emacs)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("cc4e95929ebadaa3f2e9ac462b6594f02c41742efbe8505335f5a67b36cec21c" "4639288d273cbd3dc880992e6032f9c817f17c4a91f00f3872009a099f5b3f84" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default)))
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (C . t)
     (css . t)
     (ruby . t)
     (sh . t)
     (awk . t)
     (R . t))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(package-selected-packages
   (quote
    (buffer-move dired-x ace-jump-mode doom-modeline zoom window-number use-package try tramp-term smex smartparens resize-window pdf-tools ox-gfm org-beautify-theme org-babel-eval-in-repl openwith multiple-cursors minesweeper markdown-mode+ magit ibuffer-tramp helm google-translate fit-frame evil elpy dracula-theme csv-mode counsel celestial-mode-line auctex aggressive-indent adaptive-wrap)))
 '(zoom-size (quote (0.618 . 0.618))))



