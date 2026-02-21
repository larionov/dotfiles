;;; pre-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; Add MELPA for packages like helm-ag
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Disable signature checking (avoids GPG errors)
(setq package-check-signature nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(helm-rg-default-extra-args '("--follow"))
 '(dired-listing-switches "-alh")
 '(dired-kill-when-opening-new-dired-buffer t)

  '(tramp-remote-path
    '(tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin" "/opt/homebrew/bin"))
  )


(when (eq system-type 'darwin)
  (setenv "LIBRARY_PATH"
	  (string-join
	   '("/opt/homebrew/opt/gcc/lib/gcc/current"
	     "/opt/homebrew/opt/libgccjit/lib/gcc/current"
	     "/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin24/14")
	   ":"))
  (add-to-list 'exec-path "/opt/homebrew/bin"))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

