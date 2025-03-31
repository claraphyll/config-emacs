;; random perf hacks from https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Using elpaca
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

(use-package org :ensure t
  :custom
  (org-directory "~/org")
  (org-agenda-files '("~/org/"))
  (org-return-follows-link t)
  :bind (("C-c a" . org-agenda)))
(use-package org-contrib :ensure t :after org)
(use-package org-superstar :ensure t :after org :hook org-mode)

(use-package websocket :ensure t)
(use-package transient :ensure t)
(use-package eglot :hook (prog-mode . eglot-ensure))
(use-package dape :ensure t)
(use-package magit :ensure t :config (add-hook 'after-save-hook 'magit-after-save-refresh-status t))
(use-package diff-hl :ensure t :after magit :config (global-diff-hl-mode) (diff-hl-flydiff-mode) (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
(use-package evil :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-initial-state 'eat-mode 'emacs)
  (evil-set-initial-state 'elpaca-ui-mode 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  (evil-set-initial-state 'treemacs-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-mode 1)
  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)
  )

(use-package evil-collection :ensure t :after evil :config
  (evil-collection-init '(org))
  )
(use-package corfu :ensure t :config (global-corfu-mode) :custom (corfu-auto t) (corfu-auto-prefix 1))
(use-package typst-preview :ensure (:type git :host github :repo "havarddj/typst-preview.el"))
(use-package typst-ts-mode :ensure t :after eglot :config
  (add-to-list 'eglot-server-programs
	       `((typst-ts-mode) .
		 ,(eglot-alternatives `(,typst-ts-lsp-download-path
					"tinymist"))))
  )

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

(use-package vertico :ensure t
  :config
  (vertico-mode 1)
  (setq completion-styles '(basic substring partial-completion flex))
  )
(use-package consult :ensure t)
(use-package doom-themes :ensure t :config (load-theme 'doom-one-light))
(use-package treemacs :ensure t)
(use-package forge :after magit :ensure t)
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

(setq use-short-answers t)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(use-package poke-mode :ensure t)
(use-package poke :ensure t :after poke-mode)
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

;; From https://stackoverflow.com/a/34589105
(add-hook 'prog-mode-hook (defun my/prog-mode-trailing-whitespace () (setq-local show-trailing-whitespace t)))

;; Get rid of ewww
(setq browse-url-browser-function 'browse-url-firefox)

(editorconfig-mode)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(which-key-mode)

;; Stop text mode from suggesting random words
(setopt text-mode-ispell-word-completion nil)

;; Line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
