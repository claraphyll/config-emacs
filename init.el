;; -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(setopt inhibit-splash-screen t)
(use-package doom-themes :ensure t :config (load-theme 'doom-laserwave))
(use-package diminish :ensure t)
(use-package org-contrib :ensure t
  :custom ;; Since this must be loaded before org, put custom here so it's available in config
  (org-directory "~/org/")
  (org-agenda-files '("~/org/"))
  (org-agenda-include-diary nil)
  (org-return-follows-link t)
  (org-refile-use-outline-path 'file)
  :config
  ;; Must be loaded immediately, so can't go in org-modules
  (require 'org-protocol)
  (setq org-capture-templates
        `(("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
           "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
          ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
           "* %? [[%:link][%:description]] \nCaptured On: %U")))
  ;; Cleanup capture frame
  (advice-add  #'org-capture-place-template :after 'delete-other-windows)
  ;; patch habit duration according to https://www.reddit.com/r/emacs/comments/18oweeq/comment/kekymfd/
  ;; (require 'org-habit)
  ;; (defun org-habit-duration-to-days (ts)
  ;;   (if (string-match "\\([0-9]+\\)\\([hdwmy]\\)" ts)
  ;;       ;; lead time is specified.
  ;;       (floor (* (string-to-number (match-string 1 ts))
  ;;                 (cdr (assoc (match-string 2 ts)
  ;;                             '(("h" . 0.041667)
  ;;                               ("d" . 1) ("w" . 7)
  ;;                               ("m" . 30.4) ("y" . 365.25))))))
  ;;     (error "Invalid duration string: %s" ts)))
  )

(use-package org
  :after org-contrib
  :ensure t
  :bind (("C-c a" . org-agenda))
  :config
  (add-to-list 'org-modules 'habit)
  )

(use-package origami :ensure (:type git :host github :repo "claraphyll/origami.el"))
(use-package org-superstar :ensure t :after org :hook org-mode)

(use-package websocket :ensure t)
(use-package transient :ensure t)
(use-package eglot :hook (prog-mode . eglot-ensure))
(use-package eglot-inactive-regions :ensure t :config (eglot-inactive-regions-mode 1))
(use-package projectile :ensure t :config (projectile-mode t) (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package dape :ensure t)
(use-package magit :ensure t :config (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  :custom
  (magit-diff-refine-hunk t)
  )
(use-package diff-hl :ensure t :after magit
  :config
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :custom
  (diff-hl-update-async t))
(use-package evil :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-initial-state 'eat-mode 'emacs)
  (evil-set-initial-state 'elpaca-ui-mode 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  (evil-set-initial-state 'treemacs-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'magit-status-mode 'normal)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-mode 1)
  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point))

;; evil-collection waits for forge because of https://github.com/emacs-evil/evil-collection/issues/543
(use-package evil-collection :ensure t :after (evil forge) :diminish evil-collection-unimpaired-mode :config
  (evil-collection-init '(org info forge magit dired))
  )
(use-package corfu :ensure t :config (global-corfu-mode) :custom (corfu-auto t) (corfu-auto-prefix 1))
(use-package typst-preview :ensure (:type git :host github :repo "havarddj/typst-preview.el"))
(use-package typst-ts-mode :ensure t :after eglot :config
  (add-to-list 'eglot-server-programs
               `((typst-ts-mode) .
                 ,(eglot-alternatives `(,typst-ts-lsp-download-path "tinymist")))))

(use-package haskell-ts-mode :ensure t :after eglot :config
  (add-to-list 'eglot-server-programs '(haskell-ts-mode . ("haskell-language-server" "--lsp")))
  )
(use-package transpose-frame :ensure t)
(use-package eat :ensure t :custom (eat-enable-mouse-support t) (eat-kill-buffer-on-exit t))
(use-package yasnippet-snippets :ensure t)
;; (use-package yasnippet :ensure t :config (yas-global-mode t))
(use-package meson-mode :ensure t :after eglot :config
  (add-to-list 'eglot-server-programs '(meson-mode . ("mesonlsp" "--lsp")))
  )
(use-package markdown-mode :ensure t)
(use-package yaml-mode :ensure t)

(use-package orderless :ensure t)
(use-package vertico :ensure t
  :config
  (vertico-mode 1)
  (setq completion-styles '(orderless basic))
  )

(use-package stillness-mode :ensure t :config (stillness-mode))
;; Example configuration for Consult
(use-package consult :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ([remap repeat-complex-command] . consult-complex-command)     ;; orig. repeat-complex-command
         ([remap switch-to-buffer] . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package rainbow-delimiters :ensure t :hook emacs-lisp-mode)
(use-package doom-modeline :ensure t :config (doom-modeline-mode))
(use-package treemacs :ensure t)
(use-package forge :after magit :ensure t :custom (forge-add-default-bindings nil))
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
(use-package poke-mode :ensure t)
(use-package poke :ensure t :after poke-mode)
(use-package mlscroll :ensure t :config (mlscroll-mode 1))
(use-package c-ts-mode :custom (c-ts-mode-indent-style 'K&R))


;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

(setopt use-short-answers t)
(setopt window-resize-pixelwise t)
(setopt frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

(use-package xdg)
(make-directory (concat (xdg-state-home) "/emacs/autosave/") t)
(make-directory (concat (xdg-state-home) "/emacs/backups/") t)
;; Modified from https://www.emacswiki.org/emacs/AutoSave
(setq backup-directory-alist
      `(("." . ,(concat (xdg-state-home) "/emacs/backups/"))))

;; Modified from https://emacs.stackexchange.com/a/50968
(setq auto-save-file-name-transforms
      `((".*" ,(concat (xdg-state-home) "/emacs/autosave/") t)))
;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file))

(setopt browse-url-browser-function 'browse-url-firefox)

(editorconfig-mode)
(tool-bar-mode -1)
(menu-bar-mode -1)
(when (featurep 'scroll-bar) (scroll-bar-mode -1))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(which-key-mode)

;; Stop text mode from suggesting random words
(setopt text-mode-ispell-word-completion nil)

;; Line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(column-number-mode)
;; Disable the "safe themes" mechanism
(setopt custom-safe-themes t)

;; Smooth scrolling
(setopt scroll-conservatively 101)
(pixel-scroll-precision-mode)

;; Indentation
(setopt indent-tabs-mode nil)
(setopt tab-width 4)

(setopt whitespace-style '(face tab-mark trailing))
(add-hook 'prog-mode-hook 'whitespace-mode)
(provide 'init)
;;; init.el ends here
