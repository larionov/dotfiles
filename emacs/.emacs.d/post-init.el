;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(setq auto-revert-remote-files t)
(use-package which-key
  :ensure nil ; builtin
  :defer t
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

(use-package compile-angel
  :ensure t
  :demand t
  :config
  ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
  ;; Drawback: The minibuffer will not display compile-angel's actions.
  (setq compile-angel-verbose t)

  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))


(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit))
  :config
  (message "Magit loaded successfully"))
(use-package vterm
  :ensure t
  :defer t
  :commands vterm
  :config
  ;; Speed up vterm
  (setq vterm-timer-delay 0.01))

(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package deadgrep
  :ensure t
  :defer t
    :bind (("M-s g" . deadgrep))
    )

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ;;;; ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))



(use-package outline-indent
  :ensure t
  :defer t
  :commands outline-indent-minor-mode

  :init
  ;; The minor mode can also be automatically activated for a certain modes.
  ;; For example for Python and YAML:
  (add-hook 'python-mode-hook #'outline-indent-minor-mode)
  (add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)

  (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
  (add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode)

  :custom
  (outline-indent-ellipsis " ▼ "))


(use-package outline-indent
  :ensure t
  :defer t
  :commands outline-indent-minor-mode

  :init
  ;; The minor mode can also be automatically activated for a certain modes.
  ;; For example for Python and YAML:
  (add-hook 'python-mode-hook #'outline-indent-minor-mode)
  (add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)

  (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
  (add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode)

  :custom
  (outline-indent-ellipsis " ▼ "))



(use-package eglot
  :ensure nil
  :defer t
  :commands (eglot
             eglot-ensure
             eglot-rename
             eglot-format-buffer))


(use-package easysession
  :ensure t
  :defer t
  :commands (easysession-switch-to
             easysession-save-as
             easysession-save-mode
             easysession-load-including-geometry)

  :custom
  (easysession-mode-line-misc-info t)  ; Display the session in the modeline
  (easysession-save-interval (* 10 60))  ; Save every 10 minutes

  :init
  ;; Key mappings:
  ;; C-c l for switching sessions
  ;; and C-c s for saving the current session
  (global-set-key (kbd "C-c l") 'easysession-switch-to)
  (global-set-key (kbd "C-c s") 'easysession-save-as)

  ;; The depth 102 and 103 have been added to to `add-hook' to ensure that the
  ;; session is loaded after all other packages. (Using 103/102 is particularly
  ;; useful for those using minimal-emacs.d, where some optimizations restore
  ;; `file-name-handler-alist` at depth 101 during `emacs-startup-hook`.)
  ;; Disabled geometry loading to prevent two windows issue
  ;; (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103))



(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c TAB" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))



;; Hide warnings and display only errors
(setq warning-minimum-level :error)

;; Display of line numbers in the buffer:
(display-line-numbers-mode 1)

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
  ;; versions, except for emacs-mac.
  ;;
  ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
  ;; this version of Emacs natively supports smooth scrolling.
  ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
  (setq pixel-scroll-precision-use-momentum nil) ; Precise/smoother scrolling
  (pixel-scroll-precision-mode 1))

;; Display the time in the modeline
(display-time-mode 1)

;; Paren match highlighting
(show-paren-mode 1)

;; Track changes in the window configuration, allowing undoing actions such as
;; closing windows.
(winner-mode 1)

;; Replace selected text with typed text
(delete-selection-mode 1)

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

(use-package uniquify
  :ensure nil
  :defer t
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;; Window dividers separate windows visually. Window dividers are bars that can
;; be dragged with the mouse, thus allowing you to easily resize adjacent
;; windows.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Dividers.html
(add-hook 'after-init-hook #'window-divider-mode)

;; Automatically hide file details (permissions, size, modification date, etc.)
;; in Dired buffers for a cleaner display.
(add-hook 'dired-mode-hook #'dired-hide-details-mode)


(use-package dockerfile-mode
  :ensure t
  :defer t)
(use-package go-mode
  :ensure t
  :defer t)

;;   :ensure t
;;   :defer t
;;   :init
;;   (dirvish-override-dired-mode)
;; 
;; )

;; (use-package dirvish
;;   :ensure t
;;   :init
;;   (dirvish-override-dired-mode)
;;   :config
;;   ;;(dirvish-peek-mode)
;;   ;;(dirvish-side-follow-mode)
;;   :custom
;;   (delete-by-moving-to-trash t)
;;   (dired-listing-switches
;;    "-l --almost-all --human-readable --group-directories-first --no-group")
;;   (dirvish-attributes
;;    '(vc-state subtree-state all-the-icons collapse file-time file-size))
;;   (dirvish-quick-access-entries
;;    '(("j" "~/"              "Home")
;;      ("d" "~/Downloads/"    "Downloads")
;;      ("D" "~/Documents/"    "Documents")
;;      ("p" "~/proj/"         "Projects")
;;      ("c" "~/cloud/"         "Projects")
;;      ("/" "/"               "Root")))
;;   :bind
;;   (("C-c d" . 'dirvish)
;;    ("C-=" . 'dirvish-side)
;;    :map dirvish-mode-map
;;    ("a"   . dirvish-quick-access)
;;    ("f"   . dirvish-file-info-menu)
;;    ("z"   . dirvish-history-last)
;;    ("J"   . dirvish-history-jump)
;;    ("y"   . dirvish-yank-menu)
;;    ("TAB" . dirvish-subtree-toggle)
;;    ("M-f" . dirvish-history-go-forward)
;;    ("M-b" . dirvish-history-go-backward)
;;    ("M-l" . dirvish-ls-switches-menu)
;;    ("M-m" . dirvish-mark-menu)
;;    ("M-t" . dirvish-layout-toggle)
;;    ("M-s" . dirvish-setup-menu)
;;    ("M-e" . dirvish-emerge-menu)
;;    ("M-j" . dirvish-fd-jump)))

(global-set-key (kbd "M-s") 'dired-jump)
(global-set-key (kbd "C-x C-d") 'ibuffer)


(eval-when-compile (require 'dired))
;;;###autoload
(defun dired-find-parent-directory ()
  "Open a `dired'-buffer of the parent directory."
  (interactive)
  (find-alternate-file ".."))

;;;###autoload
(defun dired-arrow-keys-install ()
  "Install `dired-arrow-keys' by modifying `dired-mode-map'.

Map

    <right> to `dired-find-file'
    <left> to `dired-find-parent-directory'

and for `evil' users, map

    \\[evil-forward-char] to `dired-find-file'
    \\[evil-backward-char] to `dired-find-parent-directory'"
  (interactive)
  (define-key dired-mode-map (kbd "<right>") 'dired-find-file)
  (define-key dired-mode-map (kbd "<left>") 'dired-find-parent-directory)
  (eval-after-load 'evil
    '(progn
       (define-key dired-mode-map (vector 'remap 'evil-forward-char) 'dired-find-file)
       (define-key dired-mode-map (vector 'remap 'evil-backward-char) 'dired-find-parent-directory))))


(dired-arrow-keys-install)

;;(setq ls-lisp-dirs-first t)

;;(setq dired-listing-switches "-al --group-directories-first")
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  (add-to-list 'tramp-remote-path "/opt/homebrew/bin")
  (setq explicit-shell-file-name "/bin/bash")
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:upwork@macmini.local:")
                     "remote-shell" "/bin/bash --login")))

(use-package wgrep
  :ensure t
  :defer t
  :init
  (setq wgrep-auto-save-buffer t)
  )

(defun indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(use-package move-text
  :ensure t
  :defer t
  :init (move-text-default-bindings)
  
  (advice-add 'move-text-down :after 'indent-region-advice)
  (advice-add 'move-text-up :after 'indent-region-advice)
  )

(use-package go-mode :ensure t :defer t)

(add-to-list 'load-path "~/.emacs.d/packages/macrursors")
(require 'macrursors)
(define-prefix-command 'macrursors-mark-map)
(require 'macrursors-select)
  (dolist (mode '(corfu-mode goggles-mode beacon-mode))
    (add-hook 'macrursors-pre-finish-hook mode)
    (add-hook 'macrursors-post-finish-hook mode))
(global-set-key (kbd "C-c SPC") #'macrursors-select)
(global-set-key (kbd "C->") #'macrursors-mark-next-instance-of)
(global-set-key (kbd "C-<") #'macrursors-mark-previous-instance-of)
(global-set-key (kbd "C-;") 'macrursors-mark-map)
(define-key macrursors-mark-map (kbd "C-;") #'macrursors-mark-all-lines-or-instances)
(define-key macrursors-mark-map (kbd ";") #'macrursors-mark-all-lines-or-instances)
(define-key macrursors-mark-map (kbd "l") #'macrursors-mark-all-lists)
(define-key macrursors-mark-map (kbd "s") #'macrursors-mark-all-symbols)
(define-key macrursors-mark-map (kbd "e") #'macrursors-mark-all-sexps)
(define-key macrursors-mark-map (kbd "f") #'macrursors-mark-all-defuns)
(define-key macrursors-mark-map (kbd "n") #'macrursors-mark-all-numbers)
(define-key macrursors-mark-map (kbd ".") #'macrursors-mark-all-sentences)
(define-key macrursors-mark-map (kbd "r") #'macrursors-mark-all-lines)


;; Use built-in helm-do-grep-ag instead of separate helm-ag package
;; As recommended by Helm maintainer: https://github.com/melpa/melpa/pull/9520
(use-package helm
  :ensure t
  :defer t
  :bind (("C-c f" . helm-do-grep-ag)
         ("C-c F" . helm-projectile-do-grep-ag))
  :config
  ;; Configure ripgrep as backend for helm-do-grep-ag
  (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
  (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'")))
(setq  ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)

;; Remove exit confirmation
(setq confirm-kill-emacs nil)

;; Replace yes/no prompts with y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;(use-package claudemacs
;  :vc (:url "https://github.com/cpoile/claudemacs"))
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config (claude-code-mode)
  :bind-keymap ("C-c v" . claude-code-command-map)) ;; or your preferred key
