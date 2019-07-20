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
(setq nScreens (string-to-number (shell-command-to-string "xrandr | grep '[^dis]connected' | wc -l")))

(setq exwm-workspace-number nScreens)


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
(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)

(add-hook 'exwm-manage-finish-hook
	  (lambda ()
	    (when (and exwm-class-name
		       (string= exwm-class-name "ROOT"))
	      (exwm-floating-toggle-floating))))


(exwm-input-set-key (kbd "s-x") #'exwm-restart)
(exwm-input-set-key (kbd "s-d") 'helm-run-external-command)
(exwm-input-set-key (kbd "s-t") #'exwm-floating-toggle-floating)
(exwm-input-set-key (kbd "s-s") #'exwm-workspace-switch-to-buffer)
(exwm-input-set-key (kbd "s-f") #'exwm-layout-toggle-fullscreen)
(exwm-input-set-key (kbd "M-o") 'ace-window)
(exwm-input-set-key (kbd "C-c M-.") 'eyebrowse-switch-to-window-config)
