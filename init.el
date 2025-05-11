;; -*- lexical-binding: t; -*-
(defun my/play-sound-async (sound-file)
  (interactive "f")
  (let ((expanded-file-name (expand-file-name sound-file)))
    (pcase window-system
      ('pgtk (call-process "pw-play" nil 0 nil expanded-file-name))
      ('android (call-process "play-audio" nil 0 nil expanded-file-name)))))


(when (eq window-system 'android)
  (setenv "PATH" (concat "/data/data/com.termux/files/usr/bin:" (getenv "PATH")))
  (add-to-list 'exec-path "/data/data/com.termux/files/usr/bin")
  (setopt elpaca-menu-org-make-manual nil)
  (setopt doom-modeline-icon nil)
  (setopt tool-bar-position 'bottom))
(setq package-enable-at-startup nil)
(setopt use-package-compute-statistics t)

(defvar elpaca-installer-version 0.11)
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(use-package flymake :hook prog-mode)
(setopt inhibit-splash-screen t)
(use-package doom-themes :ensure t
  :config
  ;; https://github.com/doomemacs/doomemacs/issues/6221#issuecomment-1098560921
  ;; theme looks different if loaded in server
  (defun my/load-theme (frame)
    (select-frame frame)
    (load-theme 'doom-laserwave t))

  (if (daemonp)
      (add-hook 'after-make-frame-functions #'my/load-theme)
    (load-theme 'doom-laserwave t)))
(use-package org-contrib :ensure t :config (when (daemonp) (require 'org-protocol)))
(use-package qrencode :ensure t)
(use-package org
  :ensure t
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c r f" . org-node-find)
         :map org-mode-map
         ("C-c l" . org-id-store-link)
         ("C-c r i" . org-node-insert-link)
         )
  :custom
  (org-priority-default 67)
  (org-priority-lowest 68)
  (org-agenda-span 'day)
  (org-agenda-include-diary nil)
  (org-return-follows-link t)
  (org-refile-use-outline-path 'file)
  (org-id-link-to-org-use-id t)
  (org-M-RET-may-split-line nil)
  (org-extend-today-until 5)
  ;; From #A to #D
  ;; Use
  ;; #A for tasks that are vital to maintaining a job, livelihood etc. (Must)
  ;; #B for important but not immediately critical tasks or "tracking items" for tasks like food
  ;;    which I am unlikely to forget for a critical amount of time but want to record (Should)
  ;; #C is the default priority for tasks that should be completed but aren't critical (Could)
  ;; #D is for nice-to-have tasks, ideas etc.
  :config
  (setopt org-directory (pcase window-system
                          ('android "/content/storage/com.android.externalstorage.documents/primary:Documents%2Forg")
                          (_ "~/org")))
  (setopt org-roam-directory org-directory)
  (setopt org-agenda-files `(,org-directory))
  (setopt org-capture-templates
          `(("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
             "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
            ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
             "* %? [[%:link][%:description]] \nCaptured On: %U")))
  (defun my/org-identify-all () (org-map-entries #'org-id-get-create))
  (defun my/org-id-save-hook () (add-hook 'before-save-hook #'my/org-identify-all))
  (add-hook 'org-mode-hook #'my/org-id-save-hook)
  (defun my/clicker-click ()
    (when (string-equal "DONE" org-state) (my/play-sound-async (locate-user-emacs-file "clicker.wav"))))
  (add-hook 'org-after-todo-state-change-hook #'my/clicker-click)
  ;; (advice-add  #'org-capture-place-template :after 'delete-other-windows)
  )

(use-package org-mouse :after org)
(use-package org-modern :ensure t :after org :config (global-org-modern-mode))
(use-package org-hide-drawers :hook org-mode :ensure (:type git :host github :repo "krisbalintona/org-hide-drawers"))
;; (use-package org-tidy :ensure t :config :hook org-mode)
(use-package org-node :ensure t :after org
  :custom
  (org-node-warn-title-collisions nil)
  :config
  (org-node-cache-mode)
  )

(use-package ultra-scroll :demand t :ensure (:type git :host github :repo "jdtsmith/ultra-scroll") :config (ultra-scroll-mode 1))
(use-package origami :ensure (:type git :host github :repo "elp-revive/origami.el")
  :hook org-agenda-mode
  :bind (:map org-agenda-mode-map ("<backtab>" . origami-toggle-node)))
(use-package org-superstar :ensure t :after org :hook org-mode)
(use-package org-super-agenda :disabled t :ensure t :config (org-super-agenda-mode) :after org)
(use-package pdf-tools
  :unless (eq window-system 'android)
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode))
(use-package org-typst-preview :ensure (:type git :host github :repo "remimimimimi/org-typst-preview.el") :after org)
(use-package ox-typst :ensure t :after ox)
(use-package websocket :ensure t)
(use-package transient :ensure t)
(use-package eglot :hook (prog-mode . eglot-ensure))
(use-package eglot-inactive-regions :ensure t :after eglot :config (eglot-inactive-regions-mode 1))
(use-package projectile :ensure t :config (projectile-mode t) (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package dape :ensure t :defer t)
(use-package magit :ensure t :hook (after-save . magit-after-save-refresh-status)
  :custom
  (magit-diff-refine-hunk t)
  )
(use-package diff-hl :ensure t :demand t
  :config
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t)
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom
  (diff-hl-update-async t))

(use-package evil :ensure t :demand t :unless (eq window-system 'android)
  :init
  (defun my/set-shift-width-2 () (setq-local evil-shift-width 2))
  :config
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  ;; eat flaming megadeath primary selection
  ;; The only use this misfeature has is to convince Un*x users that the mouse is
  ;; actually bad, by embarassingly making them send random text chunks to their friends
  ;; while shadowing useful features like middle-click-scrolling
  (evil-define-key '(normal insert) 'global (kbd "<mouse-2>") nil)
  (evil-mode t)
  :custom
  (evil-want-keybinding nil)
  :hook (git-commit-mode . evil-insert-state)
  :hook (org-mode . my/set-shift-width-2))

;; evil-collection waits for forge because of https://github.com/emacs-evil/evil-collection/issues/543
(use-package evil-collection :ensure t :after (evil forge) :diminish evil-collection-unimpaired-mode :config
  (evil-collection-init))

(use-package corfu :ensure t :config (global-corfu-mode) :custom (corfu-auto t) (corfu-auto-prefix 1))
(use-package typst-preview :ensure (:type git :host github :repo "havarddj/typst-preview.el"))
(use-package sp-tutor :defer t :ensure (:type git :url "https://gitlab.cs.fau.de/oj14ozun/sp-tutor.el"))
(use-package typst-ts-mode :ensure t :after eglot :config
  (add-to-list 'eglot-server-programs
               `((typst-ts-mode) .
                 ,(eglot-alternatives `(,typst-ts-lsp-download-path "tinymist")))))

(use-package haskell-ts-mode :ensure t :after eglot :init
  (add-to-list 'eglot-server-programs '(haskell-ts-mode . ("haskell-language-server" "--lsp"))))
(use-package transpose-frame :ensure t)
(use-package eat :ensure t :custom (eat-enable-mouse-support t) (eat-kill-buffer-on-exit t))
(use-package yasnippet-snippets :ensure t :after yasnippet)
;; (use-package yasnippet :ensure t :config (yas-global-mode t))
(use-package meson-mode :ensure t :after eglot :config
  (add-to-list 'eglot-server-programs '(meson-mode . ("mesonlsp" "--lsp"))))
(use-package markdown-mode :ensure t)
(use-package yaml-mode :ensure t)

(use-package orderless :ensure t)
(use-package vertico :ensure t
  :custom
  (completion-styles '(orderless basic))
  :config
  (vertico-mode 1))

(use-package marginalia :ensure t :after vertico :config (marginalia-mode))
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
(use-package treemacs :ensure t :defer t)
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
;; (use-package mlscroll :ensure t :config (mlscroll-mode 1))
;; (use-package c-ts-mode :custom (c-ts-mode-indent-style 'K&R))


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
(unless (eq window-system 'android)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(modifier-bar-mode)
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
(setopt calendar-week-start-day 1)

(global-auto-revert-mode)
(setopt doc-view-resolution 800)
(add-hook 'prog-mode-hook 'whitespace-mode)
(provide 'init)
;;; init.el ends here
