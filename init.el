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
;;;;;;;;;;;;;;;;;;;;
;; Setup of Theme ;;
;;;;;;;;;;;;;;;;;;;;

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
    (ansi-term "/bin/zsh"))
  (get-buffer-process "*ansi-term*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function to set up jumping to terminal with ` in ;;
;; the terminal (NEED TO WORK ON)		    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/dired+.el")
(setq diredp-hide-details-initially-flag nil)
(setq diredp-hide-details-propagate-flag nil)
(diredp-toggle-find-file-reuse-dir 1)

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


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ____   ___    ___ __ __  ___    ___   ____  __  ;;
;; || \\ // \\  //   || // // \\  // \\ ||    (( \ ;;
;; ||_// ||=|| ((    ||<<  ||=|| (( ___ ||==   \\  ;;
;; ||    || ||  \\__ || \\ || ||  \\_|| ||___ \_)) ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Ace Window	                ;;
;; Allows nice switching between frames/windows ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
  :config
  (progn
    (global-set-key (kbd "M-o") 'ace-window)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (expose-global-binding-in-term (kbd "M-o"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       Elpy	        ;;
;; Fancy python helper  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elpy
  :config
  (progn
    (elpy-enable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  Buffer move			        ;;
;; Nice way to move buffers around to different windows ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package buffer-move
  :config
  (progn
    (global-set-key (kbd "S-<left>")  'buf-move-left)
    (global-set-key (kbd "S-<right>") 'buf-move-right)
    (global-set-key (kbd "S-<up>")    'buf-move-up)
    (global-set-key (kbd "S-<down>")  'buf-move-down)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      eterm 256color	      ;;
;; Make Ansi-term have color! ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eterm-256color
  :config
  (eterm-256color-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           IBuffer		 ;;
;; Makes Buffer look 100x better ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ibuffer
  :config
  (global-set-key (kbd "C-x C-b")  'ibuffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          God Mode				   ;;
;; Allows for "modes" similar to vim and removes the need for Ctrl ;;
;; bound to Ctrl-Enter						   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package god-mode
  :config
  (global-set-key (kbd "C-<return>") 'god-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 Company Mode			      ;;
;; Autocomplete software			      ;;
;; Needs clang so might switch (because of ssh stuff) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook 'global-company-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Avy				      ;;
;; Package used for jumping to words/lines with asdf keys     ;;
;; I only like the avy-goto-line (swiper works better for me) ;;
;; and even goto-line is an iffy time saver...		      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :config
  (global-set-key (kbd "M-g M-g") 'avy-goto-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  Swiper   (Needs Counsel)			      ;;
;; Package for finding anything! Works with searching, buffer change, ;;
;; finding of a file, and M-x stuff. Very pretty!		      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; it looks like counsel is a requirement for swiper
(use-package counsel)

(use-package swiper
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "M-%") 'swiper-query-replace)
    (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
    (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
    (global-set-key (kbd "C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "M-y") 'counsel-yank-pop)
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
  :config
  (progn
    (smartparens-global-mode t)
    (global-set-key (kbd "C-'") 'sp-forward-slurp-sexp)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Zoom				  ;;
;; Has balancing of windows. Need to fix it with dired mode,	  ;;
;; but looks nice. It's set to have a Golden Ratio, may change... ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package zoom
;;   :config
;;   (progn
;;     (zoom-mode t)
;;     (custom-set-variables
;;      '(zoom-size '(0.618 . 0.75)
;; 		 ;;     '(zoom-ignored-major-modes '(dired-mode))
;; 		 ))))

(use-package aggressive-indent
  :config
  (progn
    ;; Turned on everywhere, but can add exceptions below
    (global-aggressive-indent-mode 1)
    (add-to-list 'aggressive-indent-excluded-modes 'html-mode)))

;;; Doesn't work with zoom on
(use-package resize-window
  :config
  (progn
    (global-set-key (kbd "C-c ;") 'resize-window)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Doom Modeline		    ;;
;; Changes the bottome line to look slicker ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (progn (setq find-file-visit-truename t)
	 (setq doom-modeline-height 10)
	 (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)))

;;;;;;;;;;;;;;;;;;;;;;;
;;     Magit	     ;;
;; Git but in emacs! ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  )


(use-package rainbow-delimiters
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'prog-mode-hook (show-paren-mode))))

(set-face-background 'show-paren-match-face "#FFFFFF")

(use-package dmenu
  :ensure t
  )


(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-keymap-prefix (kbd "M-e"))
  (global-unset-key (kbd "C-c C-w"))
  :config
  (eyebrowse-mode t))

(defun setup-eyebrowseWS ()
  (eyebrowse-rename-window-config 0 "desktop")
  (eyebrowse-rename-window-config 1 "music")
  (eyebrowse-rename-window-config 2 "local")
  (eyebrowse-rename-window-config 3 "ssh")
  )


(defun setup-EBWS0 ()
  (eyebrowse-switch-to-window-config-0)
  (switch-to-buffer "*scratch*")

  )
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

(set-frame-alpha 100) 
(set-frame-parameter nil 'alpha 90)

;; ;;;; NEED TO edit this to open correct stuff!
;; ;;https://bitbucket.org/jpkotta/openwith/src/default/README.txt
;; (use-package openwith
;;   :ensure t
;;   :config
;;   (openwith-mode t)
;;   )


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
    ("6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "9954ed41d89d2dcf601c8e7499b6bb2778180bfcaeb7cdfc648078b8e05348c6" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "cc4e95929ebadaa3f2e9ac462b6594f02c41742efbe8505335f5a67b36cec21c" "4639288d273cbd3dc880992e6032f9c817f17c4a91f00f3872009a099f5b3f84" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default)))
 '(doom-modeline-mode t)
 '(openwith-mode t)
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
    (scratch scratch-ext eyebrowse mode-icons all-the-icons-ivy all-the-icons-dired dmenu exwm-x exwm lorem-ipsum irony rainbow-delimiters doom-themes eterm-256color quelpa-use-package quelpa god-mode dired+ buffer-move dired-x ace-jump-mode doom-modeline zoom window-number use-package try tramp-term smex smartparens resize-window pdf-tools ox-gfm org-beautify-theme org-babel-eval-in-repl openwith multiple-cursors minesweeper markdown-mode+ magit ibuffer-tramp helm google-translate fit-frame evil elpy dracula-theme csv-mode counsel celestial-mode-line auctex aggressive-indent adaptive-wrap)))
 '(zoom-size (quote (0.618 . 0.618))))


(require 'exwm)
(require 'exwm-config)


(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "eDPI" 1 "HDMI1"))
(add-hook 'exwm-randr-screen-change-hook
	  (lambda ()
	    (start-process-shell-command
	     "xrandr" nil "xrandr --output eDP1 --primary --mode 1920x1080 --pos 0x800 --rotate normal --output HDMI1 --mode 1920x1200 --pos 1920x0 --rotate left")))
;;(setq exwm-randr-workspace-output-plist '(0 "eDP1" 1 "HDMI1"))
(exwm-randr-enable)

(exwm-config-default)
(setq exwm-workspace-number 2)

;;(require 'exwm-systemtray)
;;(exwm-systemtray-enable)
;;(add-hook 'exwm-init-hook 'exwm-x/network-manager-applet t)
;;(add-hook 'exwm-init-hook 'exwm-x/volit t)
;;(add-hook 'exwm-init-hook 'exwm-x/power-manager t)
;;(add-hook 'exwm-init-hook 'exwm-x/xscreensaver t)
;;(add-hook 'exwm-init-hook 'exwm-x/xset-bell-off t)
;;(add-hook 'exwm-init-hook 'exwm-x/xmodmap t)
;; (window-divider-mode -1)
;; (display-battery-mode 1)
;; (display-time-mode 1)

(require 'exwm-x)

()

(ido-mode -1)
;; (require 'exwmx-xfce)
;; (require 'exwmx-example)
;; (exwmx-input-set-key (kbd "C-t v") 'exwmx:file-browser)
;; (exwmx-input-set-key (kbd "C-t f") 'exwmx:web-browser)
;; (exwmx-input-set-key (kbd "C-t e") 'exwmx:emacs)
;; (exwmx-input-set-key (kbd "C-t c") 'exwmx-xfce-terminal)
;; (exwmx-input-set-key (kbd "C-t z") 'exwmx-floating-hide-all)
;; (exwmx-input-set-key (kbd "C-t C-c") 'exwmx-xfce-new-terminal)
;; (exwmx-input-set-key (kbd "C-t b") 'exwmx-switch-application)

;; (exwmx-input-set-key (kbd "C-t C-f") 'exwmx-floating-toggle-floating)


(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (string= exwm-class-name "Firefox"))
              (exwm-layout-hide-mode-line))))
(add-hook `dired-mode-hook `all-the-icons-dired-mode)

(exwm-input-set-key (kbd "s-x") #'exwm-restart)
(exwm-input-set-key (kbd "s-d") 'dmenu)
(exwm-input-set-key (kbd "s-t") #'exwm-floating-toggle-floating)
(exwm-input-set-key (kbd "s-s") #'exwm-workspace-switch-to-buffer)
(exwm-input-set-key (kbd "s-f") #'exwm-layout-toggle-fullscreen)
(exwm-input-set-key (kbd "M-o") 'ace-window)

