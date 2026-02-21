;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
(use-package exec-path-from-shell
  :init
  ;; Copy API keys from shell environment
  (when (daemonp)
    (exec-path-from-shell-copy-env "ANTHROPIC_API_KEY")
    (exec-path-from-shell-copy-env "OPENAI_FIM_API_KEY")
    (exec-path-from-shell-copy-env "GEMINI_API_KEY"))
  (exec-path-from-shell-initialize)
  ;; Ensure API keys are copied even in non-daemon mode
  (exec-path-from-shell-copy-env "ANTHROPIC_API_KEY")
  (exec-path-from-shell-copy-env "OPENAI_FIM_API_KEY")
  (exec-path-from-shell-copy-env "GEMINI_API_KEY"))

;; Add Go bin to exec-path
(add-to-list 'exec-path (expand-file-name "~/go/bin"))

(setq auto-revert-remote-files t)

;; Project management with built-in project.el
(use-package project
  :ensure nil  ; builtin
  :bind (("C-c p f" . project-find-file)
         ("C-c p p" . project-switch-project)
         ("C-c p b" . consult-project-buffer)
         ("C-c p g" . consult-ripgrep)
         ("C-c p d" . project-dired))
  :custom
  ;; Where to search for projects
  (project-switch-commands
   '((project-find-file "Find file")
     (consult-project-buffer "Buffer")
     (consult-ripgrep "Ripgrep")
     (project-dired "Dired")
     (magit "Magit"))))
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
  (savehist-mode)
  :config
  (setq history-length 10000
	    history-delete-duplicates nil
	    savehist-save-minibuffer-history t)
  )

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
   ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
   :map minibuffer-local-map
   ("C-c C-e" . embark-export)  ;; export to editable buffer
   ("C-c C-l" . embark-collect))

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
  ;; Include hidden files and config files in ripgrep searches
  (setq consult-ripgrep-args
        (concat consult-ripgrep-args " --hidden --glob=!.git/"))

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
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

;; yasnippet - Required for LSP completion with snippet support (e.g., JSON LSP)
(use-package yasnippet
  :ensure t
  :defer t
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :custom
  (yas-verbosity 2)  ; Reduce verbosity
  :config
  ;; Don't expand snippets with TAB, use it for indentation
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil))


(use-package eglot
  :ensure nil
  :defer t
  :commands (eglot
             eglot-ensure
             eglot-rename
             eglot-format-buffer)
  :custom
  ;; Enable event logging for debugging (set to 0 to disable)
  (eglot-events-buffer-size 2000000)
  ;; Improve completion performance
  (eglot-sync-connect nil)
  (eglot-autoshutdown t)
  :config
  ;; Use only eglot completions (no buffer words, etc.)
  (setq completion-category-defaults nil)

  ;; Configure JSON language server to enable schema validation
  ;; The server should auto-detect $schema properties in JSON files
  (setq-default eglot-workspace-configuration
                '(:json (:validate (:enable t)
                        :format (:enable t))))

  ;; Note: Eglot has built-in support for vscode-json-languageserver
  ;; It will try these in order: vscode-json-language-server, vscode-json-languageserver, json-languageserver
  ;; No custom configuration needed
  )

;; Configure Tinymist for Typst files
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(typst-ts-mode . ("tinymist"))))

;; Helper function to auto-install tree-sitter grammars
(defun my/ensure-treesit-grammar (language url)
  "Ensure tree-sitter grammar for LANGUAGE is installed from URL."
  (require 'treesit)
  (when (treesit-available-p)
    (add-to-list 'treesit-language-source-alist (list language url))
    (unless (treesit-language-available-p language)
      (message "Installing tree-sitter grammar for %s..." language)
      (treesit-install-language-grammar language))))


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
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103)

  :config
  ;; Disabled: Auto-switching sessions on project switch interferes with multiple frames
  ;; Use C-c l to manually switch sessions if needed
  ;; (defun my/easysession-auto-save-on-project-switch ()
  ;;   "Automatically save/load session when switching projects."
  ;;   (when (project-current)
  ;;     (let* ((project-root (project-root (project-current)))
  ;;            (session-name (file-name-nondirectory (directory-file-name project-root))))
  ;;       (easysession-switch-to session-name))))

  ;; (advice-add 'project-switch-project :after
  ;;             (lambda (&rest _) (my/easysession-auto-save-on-project-switch)))
  )



(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Corfu behavior
  (corfu-auto t)                        ; Enable auto completion
  (corfu-auto-delay 0.2)                ; Delay before showing completions (seconds)
  (corfu-auto-prefix 2)                 ; Minimum characters to trigger completion
  (corfu-preview-current nil)           ; Don't preview current candidate
  (corfu-quit-no-match 'separator)      ; Don't quit if no match, allow typing
  (corfu-quit-at-boundary 'separator)   ; Don't quit at completion boundary
  (corfu-cycle t)                       ; Enable cycling through candidates

  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  :bind (:map corfu-map
              ("TAB" . corfu-next)          ; Next completion
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)    ; Previous completion
              ([backtab] . corfu-previous)
              ("RET" . corfu-insert)        ; Insert completion
              ([return] . corfu-insert)
              ("M-d" . corfu-show-documentation)  ; Show documentation
              ("M-l" . corfu-show-location))      ; Show location

  ;; Enable Corfu
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c TAB" . cape-prefix-map)
  :init
  ;; Cape completions disabled by default - use only LSP/eglot completions
  ;; Uncomment below if you want buffer word completions:
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-hook 'completion-at-point-functions #'cape-file)
  )




;; Pulsar: Flash/pulse lines to draw attention (especially useful for minibuffer)
(use-package pulsar
  :ensure t
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (pulsar-global-mode 1)

  ;; Pulse when minibuffer opens to make prompts more noticeable
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line)

  ;; Recommended hooks for common navigation scenarios
  (add-hook 'next-error-hook #'pulsar-pulse-line)  ; M-g n/p (flymake, grep, etc.)
  (add-hook 'imenu-after-jump-hook #'pulsar-pulse-line)  ; M-g i (imenu)
  (add-hook 'consult-after-jump-hook #'pulsar-pulse-line)  ; consult navigation
  (add-hook 'xref-after-jump-hook #'pulsar-pulse-line)  ; M-. (go to definition)
  (add-hook 'xref-after-return-hook #'pulsar-pulse-line)  ; M-, (go back)
  (add-hook 'isearch-update-post-hook #'pulsar-pulse-line)  ; C-s (search)

  ;; Pulse after window/buffer changes
  (add-hook 'window-selection-change-functions
            (lambda (_)
              (pulsar-pulse-line))))

;; Show project directory in window title
(setq frame-title-format
      '(:eval
        (let ((project (project-current)))
          (if project
              ;; Show: filename [project-name]
              (format "%s [%s]"
                      (if buffer-file-name
                          (file-name-nondirectory buffer-file-name)
                        "%b")
                      (file-name-nondirectory
                       (directory-file-name (project-root project))))
            ;; No project: just show buffer name or file path
            (if buffer-file-name
                (abbreviate-file-name buffer-file-name)
              "%b")))))

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

;; Increase all UI font size by 1 point
(set-face-attribute 'default nil :height 150)  ; Default text (was ~120)

;; Make active window more obvious with distinct colors
(set-face-attribute 'mode-line nil
                    :background "#3a3a3a"
                    :foreground "#ffffff"
                    :box '(:line-width 2 :color "#5f87af"))
(set-face-attribute 'mode-line-inactive nil
                    :background "#1c1c1c"
                    :foreground "#808080"
                    :box '(:line-width 1 :color "#2a2a2a"))

;; Thicker window dividers for easier mouse grabbing
(window-divider-mode 1)
(setq window-divider-default-right-width 4)   ; Thicker right divider
(setq window-divider-default-bottom-width 4)  ; Thicker bottom divider
(setq window-divider-default-places 'right-only) ; Show dividers on right side
;; Darker divider color
(set-face-attribute 'window-divider nil :foreground "#444444")
(set-face-attribute 'window-divider-first-pixel nil :foreground "#444444")
(set-face-attribute 'window-divider-last-pixel nil :foreground "#444444")

;; Highlight current line globally (no underline, just background)
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil :underline nil :background "#2a2a2a")

;; Paren match highlighting
(show-paren-mode 1)

;; Track changes in the window configuration, allowing undoing actions such as
;; closing windows.
(winner-mode 1)

;; Easy window navigation with Cmd+Arrow keys
(windmove-default-keybindings 'super)

;; Replace selected text with typed text
(delete-selection-mode 1)

;; Delete word backward without adding to kill ring
(defun delete-word-backward ()
  "Delete word backward without adding to kill ring."
  (interactive)
  (delete-region (point) (progn (backward-word) (point))))

;; Keybindings
;; Rebind M-backspace and C-backspace to delete instead of kill
(global-set-key (kbd "M-<backspace>") 'delete-word-backward)
(global-set-key (kbd "M-DEL") 'delete-word-backward)
(global-set-key (kbd "C-<backspace>") 'delete-word-backward)

(global-set-key (kbd "C-c d") 'dired-jump)  ; Jump to dired (d for dired, left hand friendly)
(global-set-key (kbd "C-x C-d") 'ibuffer)
(global-set-key (kbd "C-c SPC") #'macrursors-select)
(global-set-key (kbd "C->") #'macrursors-mark-next-instance-of)
(global-set-key (kbd "C-<") #'macrursors-mark-previous-instance-of)
(global-set-key (kbd "C-;") 'macrursors-mark-map)

(global-set-key (kbd "C-a")
		(defun back-to-indentation-or-beginning () (interactive)
		       (if (= (point) (progn (back-to-indentation) (point)))
			       (beginning-of-line))))
;; / keybindings

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

;; YAML configuration
(use-package yaml-mode
  :ensure t
  :defer t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :hook (yaml-mode . (lambda ()
                       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; Python configuration with LSP
(use-package python
  :ensure nil  ; built-in
  :defer t
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :bind (:map python-mode-map
              ("M-?" . xref-find-references)     ; Find all references
              ("C-c C-d" . eldoc-doc-buffer)     ; Show documentation
              ("C-c ! n" . flymake-goto-next-error)      ; Next error
              ("C-c ! p" . flymake-goto-prev-error)      ; Previous error
              ("C-c ! l" . flymake-show-buffer-diagnostics) ; List all errors
              ("C-c ! e" . flymake-show-diagnostic))     ; Show error at point
  :config
  ;; Use Python 3 by default
  (setq python-shell-interpreter "python3")

  ;; Configure eglot to use pyright (full LSP) + ruff (linting/formatting)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(python-mode . ("pyright-langserver" "--stdio"))))

  ;; Format buffer before saving with ruff
  (add-hook 'before-save-hook
            (lambda ()
              (when (eq major-mode 'python-mode)
                (eglot-format-buffer))))

  ;; Add Python navigation to cheat-sheet
  (with-eval-after-load 'help
    (add-to-list 'help-quick-sections
                 '("Python Navigation"
                   (xref-find-definitions . "M-.: jump to def")
                   (xref-pop-marker-stack . "M-,: jump back")
                   (xref-find-references . "M-?: find refs")
                   (eldoc-doc-buffer . "C-c C-d: show docs")
                   (eglot-rename . "rename symbol")
                   (flymake-show-diagnostic . "C-c ! e: show error")
                   (flymake-goto-next-error . "C-c ! n: next error")
                   (flymake-goto-prev-error . "C-c ! p: prev error")))))

;; JSON configuration with tree-sitter and LSP
;; Auto-install JSON tree-sitter grammar
(my/ensure-treesit-grammar 'json "https://github.com/tree-sitter/tree-sitter-json")

(use-package json-mode
  :ensure t
  :defer t)

;; Prefer json-ts-mode (tree-sitter) for better syntax highlighting
(add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))

;; Enable eglot for JSON
(add-hook 'json-ts-mode-hook #'eglot-ensure)

;; Auto-format JSON on save using built-in json-pretty-print
(add-hook 'json-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (eq major-mode 'json-ts-mode)
                          (json-pretty-print-buffer)))
                      nil t)))

;; Markdown configuration with GitHub-flavored preview
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-command "pandoc -t html5"))

;; grip-mode for GitHub-flavored markdown preview in browser
(use-package grip-mode
  :ensure t
  :defer t
  :commands grip-mode
  :custom
  ;; Use grip from home venv
  (grip-binary-path (expand-file-name "~/.venv/bin/grip"))
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

(use-package go-mode
  :ensure t
  :defer t
  :hook ((go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure))
  :bind (:map go-mode-map
              ("M-?" . xref-find-references)     ; Find all references
              ("C-c C-d" . eldoc-doc-buffer)     ; Show documentation
              ("C-c ! n" . flymake-goto-next-error)      ; Next error
              ("C-c ! p" . flymake-goto-prev-error)      ; Previous error
              ("C-c ! l" . flymake-show-buffer-diagnostics) ; List all errors
              ("C-c ! e" . flymake-show-diagnostic))     ; Show error at point
  :config
  ;; Set gofmt as the format command
  (setq gofmt-command "gofmt")
  ;; Format buffer before saving
  (add-hook 'before-save-hook
            (lambda ()
              (when (eq major-mode 'go-mode)
                (eglot-format-buffer))))

  ;; Add Go navigation to cheat-sheet
  (with-eval-after-load 'help
    (add-to-list 'help-quick-sections
                 '("Go Navigation"
                   (xref-find-definitions . "jump to def")
                   (xref-pop-marker-stack . "jump back")
                   (xref-find-references . "find refs")
                   (eldoc-doc-buffer . "show docs")
                   (eglot-rename . "rename")
                   (flymake-show-diagnostic . "show error")
                   (flymake-goto-next-error . "next error")
                   (flymake-goto-prev-error . "prev error")))

    ;; Add Search to cheat-sheet
    (add-to-list 'help-quick-sections
                 '("Search & Navigation"
                   (consult-ripgrep . "M-s r: ripgrep in project")
                   (consult-line . "M-s l: search current buffer")
                   (consult-line-multi . "M-s L: search all buffers")
                   (deadgrep . "M-s g: deadgrep search")
                   (helm-do-grep-ag . "C-c f: helm grep/ag")
                   (consult-find . "M-s d: find files")
                   (dired-jump . "C-c d: jump to dired")
                   (consult-imenu . "M-g i: jump to symbol")
                   (consult-goto-line . "M-g g: goto line")
                   (consult-project-buffer . "C-c p b: project buffers")))

    ;; Add Autocomplete to cheat-sheet
    (add-to-list 'help-quick-sections
                 '("Autocomplete (Corfu)"
                   (completion-at-point . "C-M-i: manual complete")
                   (corfu-next . "TAB: next completion")
                   (corfu-previous . "S-TAB: previous completion")
                   (corfu-insert . "RET: accept completion")
                   (corfu-show-documentation . "M-d: show docs")
                   (corfu-show-location . "M-l: show location")))))

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


;; Dired arrow navigation - uses built-ins with position memory
(defvar dired--nav-history (make-hash-table :test 'equal)
  "Tracks cursor position per directory for arrow navigation.")

(defun dired-arrow-up ()
  "Go up to parent. Remembers position for `dired-arrow-down'."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (when file
      (puthash (dired-current-directory) file dired--nav-history))
    (dired-up-directory)))

(defun dired-arrow-down ()
  "Enter directory, restore previous position if available."
  (interactive)
  (let* ((target (dired-get-file-for-visit))
         (target-dir (and (file-directory-p target)
                          (file-name-as-directory target)))
         (saved (gethash target-dir dired--nav-history)))
    (dired-find-file)
    (when (and saved (eq major-mode 'dired-mode))
      (dired-goto-file saved))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<left>") 'dired-arrow-up)
  (define-key dired-mode-map (kbd "<right>") 'dired-arrow-down)
  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-mode-map
      (kbd "h") 'dired-arrow-up
      (kbd "l") 'dired-arrow-down)))

;;(setq ls-lisp-dirs-first t)

;;(setq dired-listing-switches "-al --group-directories-first")

;; Load TRAMP and Docker/Podman support
(require 'tramp)
(require 'tramp-container)
(tramp-register-file-name-handlers)

;; TRAMP configuration
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

;; Zig configuration with LSP (Zig 0.15 compatible)
(use-package zig-mode
  :ensure t
  :defer t
  :hook ((zig-mode . eglot-ensure))
  :bind (:map zig-mode-map
              ("M-?" . xref-find-references)     ; Find all references
              ("C-c C-d" . eldoc-doc-buffer)     ; Show documentation
              ("C-c ! n" . flymake-goto-next-error)      ; Next error
              ("C-c ! p" . flymake-goto-prev-error)      ; Previous error
              ("C-c ! l" . flymake-show-buffer-diagnostics) ; List all errors
              ("C-c ! e" . flymake-show-diagnostic)      ; Show error at point
              ("C-c C-c" . zig-compile)          ; Compile
              ("C-c C-r" . zig-run)              ; Run
              ("C-c C-t" . zig-test-buffer))     ; Test
  :config
  ;; Format buffer before saving with zig fmt
  (add-hook 'before-save-hook
            (lambda ()
              (when (eq major-mode 'zig-mode)
                (eglot-format-buffer))))

  ;; Configure eglot to use ZLS (Zig Language Server)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(zig-mode . ("zls"))))

  ;; Add Zig navigation to cheat-sheet
  (with-eval-after-load 'help
    (add-to-list 'help-quick-sections
                 '("Zig Navigation"
                   (xref-find-definitions . "M-.: jump to def")
                   (xref-pop-marker-stack . "M-,: jump back")
                   (xref-find-references . "M-?: find refs")
                   (eldoc-doc-buffer . "C-c C-d: show docs")
                   (eglot-rename . "rename symbol")
                   (flymake-show-diagnostic . "C-c ! e: show error")
                   (flymake-goto-next-error . "C-c ! n: next error")
                   (flymake-goto-prev-error . "C-c ! p: prev error")
                   (zig-compile . "C-c C-c: compile")
                   (zig-run . "C-c C-r: run")
                   (zig-test-buffer . "C-c C-t: test")))))

;; JavaScript/TypeScript configuration with typescript-language-server
(use-package typescript-mode
  :ensure t
  :defer t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :hook ((typescript-mode . eglot-ensure))
  :bind (:map typescript-mode-map
              ("M-?" . xref-find-references)
              ("C-c C-d" . eldoc-doc-buffer)
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! e" . flymake-show-diagnostic))
  :config
  ;; Auto-format on save
  (add-hook 'before-save-hook
            (lambda ()
              (when (eq major-mode 'typescript-mode)
                (eglot-format-buffer)))))

(use-package js-mode
  :ensure nil  ; built-in
  :defer t
  :mode (("\\.js\\'" . js-mode)
         ("\\.jsx\\'" . js-mode)
         ("\\.mjs\\'" . js-mode))
  :hook ((js-mode . eglot-ensure))
  :bind (:map js-mode-map
              ("M-?" . xref-find-references)
              ("C-c C-d" . eldoc-doc-buffer)
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! e" . flymake-show-diagnostic))
  :config
  ;; Auto-format on save
  (add-hook 'before-save-hook
            (lambda ()
              (when (eq major-mode 'js-mode)
                (eglot-format-buffer)))))

;; Configure eglot to use typescript-language-server for JS/TS
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode) . ("typescript-language-server" "--stdio"))))

;; Babashka/Clojure configuration with clj-kondo (via clojure-lsp)
(use-package clojure-mode
  :ensure t
  :defer t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.edn\\'" . clojure-mode)
         ("\\.bb\\'" . clojure-mode)      ; Babashka scripts
         ("bb\\.edn\\'" . clojure-mode))  ; Babashka config
  :hook ((clojure-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure))
  :bind (:map clojure-mode-map
              ("M-?" . xref-find-references)     ; Find all references
              ("C-c C-d" . eldoc-doc-buffer)     ; Show documentation
              ("C-c ! n" . flymake-goto-next-error)      ; Next error
              ("C-c ! p" . flymake-goto-prev-error)      ; Previous error
              ("C-c ! l" . flymake-show-buffer-diagnostics) ; List all errors
              ("C-c ! e" . flymake-show-diagnostic))     ; Show error at point
  :config
  (setq clojure-indent-style 'align-arguments
        clojure-align-forms-automatically t)

  ;; Fix completion for namespace-qualified symbols (str/, set/, etc.)
  ;; Make sure '/' is treated as part of the symbol for completion
  (add-hook 'clojure-mode-hook
            (lambda ()
              ;; Ensure corfu continues completion after typing '/'
              (setq-local completion-at-point-functions
                          (append '(eglot-completion-at-point)
                                  completion-at-point-functions))
              ;; Don't treat '/' as a word boundary
              (modify-syntax-entry ?/ "_" clojure-mode-syntax-table)
              ;; Don't let Corfu quit when typing '/'
              (setq-local corfu-quit-at-boundary nil)
              ;; Continue showing completions after typing '/'
              (setq-local corfu-auto-prefix 1)))

  ;; Configure eglot to use clojure-lsp (which includes clj-kondo)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((clojure-mode clojurescript-mode clojurec-mode) . ("clojure-lsp")))))

(use-package cider
  :ensure t
  :defer t
  :commands (cider cider-jack-in cider-jack-in-clj cider-jack-in-cljs)
  :hook ((clojure-mode . cider-mode)
         (clojurescript-mode . cider-mode)
         (clojurec-mode . cider-mode))
  :bind (:map cider-mode-map
              ("C-c C-e" . cider-eval-last-sexp)  ; Eval last expression
              ("C-c C-k" . cider-load-buffer)     ; Load buffer
              ("C-c C-z" . cider-switch-to-repl-buffer))  ; Switch to REPL
  :custom
  ;; Auto-detect build tool (deps.edn, bb.edn, or project.clj)
  ;; CIDER will automatically choose the right tool for each project

  ;; REPL configuration
  (cider-repl-display-help-banner nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-use-pretty-printing t)

  ;; Eldoc (inline documentation)
  (cider-eldoc-display-for-symbol-at-point t)

  ;; Auto-completion
  (cider-completion-annotations-include-ns t)

  :config
  ;; Configure CIDER to recognize Babashka projects
  (setq cider-jack-in-default 'clojure-cli)
  (add-to-list 'cider-jack-in-dependencies '("babashka/babashka" "latest"))

  ;; Helper function to start Babashka REPL
  (defun cider-jack-in-babashka ()
    "Start a Babashka nREPL server and connect CIDER to it."
    (interactive)
    (let* ((project-dir (or (locate-dominating-file default-directory "bb.edn")
                            default-directory))
           (default-directory project-dir)
           (port "1667"))
      ;; Start Babashka nREPL server in background
      (message "Starting Babashka nREPL server on port %s..." port)
      (start-process "babashka-nrepl" "*babashka-nrepl*"
                     "bb" "--nrepl-server" port)
      ;; Wait a moment for server to start, then connect
      (run-with-timer 2 nil
                      (lambda ()
                        (cider-connect-clj (list :host "localhost" :port port))
                        (message "Connected to Babashka REPL at localhost:%s" port)))))

  ;; Auto-connect to Babashka REPL when opening .bb files in bb.edn projects
  (defun my-babashka-auto-connect ()
    "Auto-connect to Babashka REPL if in a bb.edn project and not already connected."
    (when (and (or (derived-mode-p 'clojure-mode)
                   (string-suffix-p ".bb" (buffer-file-name)))
               (locate-dominating-file default-directory "bb.edn")
               (not (cider-connected-p)))
      (cider-jack-in-babashka)))

  ;; Enable auto-connect when opening Clojure/Babashka files
  (add-hook 'clojure-mode-hook #'my-babashka-auto-connect)

  ;; Add keybinding for Babashka REPL
  (define-key clojure-mode-map (kbd "C-c M-b") 'cider-jack-in-babashka)

  ;; Add to cheat-sheet
  (with-eval-after-load 'help
    (add-to-list 'help-quick-sections
                 '("Clojure/Babashka"
                   (xref-find-definitions . "M-.: jump to def")
                   (xref-pop-marker-stack . "M-,: jump back")
                   (xref-find-references . "M-?: find refs")
                   (eldoc-doc-buffer . "C-c C-d: show docs (LSP)")
                   (flymake-show-diagnostic . "C-c ! e: show error (clj-kondo)")
                   (flymake-goto-next-error . "C-c ! n: next error")
                   (flymake-goto-prev-error . "C-c ! p: prev error")
                   (cider-jack-in . "C-c M-j: start REPL")
                   (cider-jack-in-babashka . "C-c M-b: connect to Babashka REPL")
                   (cider-eval-last-sexp . "C-c C-e: eval expression")
                   (cider-load-buffer . "C-c C-k: load buffer")
                   (cider-switch-to-repl-buffer . "C-c C-z: switch to REPL")))))

;; Typst configuration with tree-sitter and Tinymist LSP
;; Auto-install Typst tree-sitter grammar
(my/ensure-treesit-grammar 'typst "https://github.com/uben0/tree-sitter-typst")

(use-package typst-ts-mode
  :ensure t
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode" :rev :newest)
  :defer t
  :mode "\\.typ\\'"
  :hook ((typst-ts-mode . eglot-ensure))
  :bind (:map typst-ts-mode-map
              ("M-?" . xref-find-references)     ; Find all references
              ("C-c C-d" . eldoc-doc-buffer)     ; Show documentation
              ("C-c ! n" . flymake-goto-next-error)      ; Next error
              ("C-c ! p" . flymake-goto-prev-error)      ; Previous error
              ("C-c ! l" . flymake-show-buffer-diagnostics) ; List all errors
              ("C-c ! e" . flymake-show-diagnostic))     ; Show error at point
  :config
  (setq typst-ts-mode-watch-options "--open")

  ;; Add Typst navigation to cheat-sheet
  (with-eval-after-load 'help
    (add-to-list 'help-quick-sections
                 '("Typst Navigation"
                   (xref-find-definitions . "M-.: jump to def")
                   (xref-pop-marker-stack . "M-,: jump back")
                   (xref-find-references . "M-?: find refs")
                   (eldoc-doc-buffer . "C-c C-d: show docs")
                   (eglot-rename . "rename symbol")
                   (flymake-show-diagnostic . "C-c ! e: show error")
                   (flymake-goto-next-error . "C-c ! n: next error")
                   (flymake-goto-prev-error . "C-c ! p: prev error")
                   (typst-preview-mode . "C-c C-v: toggle preview")
                   (typst-preview-send-position . "C-c C-j: jump to preview")))))

;; Typst Preview - Live preview in browser
(use-package websocket
  :ensure t
  :defer t)

(use-package typst-preview
  :ensure t
  :vc (:url "https://github.com/havarddj/typst-preview.el" :rev :newest)
  :defer t
  :commands (typst-preview-mode
             typst-preview-start
             typst-preview-stop
             typst-preview-restart
             typst-preview-send-position)
  :bind (:map typst-ts-mode-map
              ("C-c C-v" . typst-preview-mode)        ; Toggle preview
              ("C-c C-j" . typst-preview-send-position)) ; Jump to preview position
  :custom
  (typst-preview-executable "tinymist")
  (typst-preview-browser "default")  ; Use system default browser
  (typst-preview-partial-rendering t)
  (typst-preview-invert-colors "auto")
  :init
  (setq typst-preview-autostart nil)  ; Manual start with C-c C-v
  (setq typst-preview-open-browser-automatically t))

(add-to-list 'load-path "~/.emacs.d/packages/macrursors")
(require 'macrursors)
(define-prefix-command 'macrursors-mark-map)
(require 'macrursors-select)
  (dolist (mode '(corfu-mode goggles-mode beacon-mode))
    (add-hook 'macrursors-pre-finish-hook mode)
    (add-hook 'macrursors-post-finish-hook mode))

;; Make macrursors scroll to show newly marked selections
(defun macrursors-recenter-after-mark (&rest _)
  "Recenter window after marking to show the selection."
  (recenter))

(advice-add 'macrursors-mark-next-instance-of :after #'macrursors-recenter-after-mark)
(advice-add 'macrursors-mark-previous-instance-of :after #'macrursors-recenter-after-mark)


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
(use-package eat
  :ensure t
  :defer t
  :config
  ;; Enable true color support
  (setq eat-term-name "xterm-256color"))

(use-package gptel
  :vc (:url "https://github.com/karthink/gptel" :rev :newest)
  :defer t
  :config
  ;; Show reasoning/thinking content:
  ;;   t       - inline with response (default)
  ;;   nil     - hide thinking
  ;;   'ignore - show but don't send back to model
  ;;   "*thinking*" - redirect to separate buffer
  (setq gptel-include-reasoning t)

  (setq gptel-model 'qwen3-coder:30b
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "192.168.1.107:11434"
                        :stream t
                        :models '(qwen3-coder:30b
                                  gemma3:12b
                                  deepseek-r1:14b
                                  deepseek-r1:8b
                                  codellama:13b-code-q4_K_M
                                  llama3.2:latest
                                  llama3.2:1b)))) 

(use-package gptel-agent
  :vc (:url "https://github.com/karthink/gptel-agent" :rev :newest)
  :after gptel
  :config (gptel-agent-update))

;; Denote - Simple, file-name based note-taking
(use-package denote
  :ensure t
  :defer t
  :bind (("C-c n n" . denote)
         ("C-c n o" . denote-open-or-create)
         ("C-c n l" . denote-link)
         ("C-c n b" . denote-backlinks)
         ("C-c n r" . denote-rename-file))
  :custom
  (denote-directory (expand-file-name "~/Sync/notes/"))
  (denote-known-keywords '("emacs" "project" "idea" "reference"))
  (denote-file-type 'org))

(use-package consult-denote
  :ensure t
  :after denote
  :config
  (consult-denote-mode 1))
