;; random perf hacks from https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

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

(use-package websocket :ensure t)
(use-package transient :ensure t)
(use-package eglot :hook (prog-mode . eglot-ensure))
(use-package magit :ensure t :config (add-hook 'after-save-hook 'magit-after-save-refresh-status t))
(use-package evil :ensure t :config
  (evil-set-initial-state 'eat-mode 'emacs)
  (evil-set-initial-state 'elpaca-ui-mode 'emacs)
  (evil-mode 1)
)
(use-package corfu :ensure t :hook prog-mode :custom (corfu-auto t) (corfu-auto-delay 0))
(use-package typst-preview :ensure (:type git :host github :repo "havarddj/typst-preview.el"))
(use-package typst-ts-mode :ensure t :after (eglot) :config
  (add-to-list 'eglot-server-programs
	       `((typst-ts-mode) .
		 ,(eglot-alternatives `(,typst-ts-lsp-download-path
					"tinymist"))))
 )
(use-package transpose-frame :ensure t)
(use-package eat :ensure t :custom (eat-enable-mouse-support t) (eat-kill-buffer-on-exit t))

(use-package vertico :ensure t
  :config
  (vertico-mode 1)
  (setq completion-styles '(basic substring partial-completion flex))
  )
(use-package consult :ensure t)

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
(setq-default show-trailing-whitespace t)

;; Get rid of ewww
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "xdg-open")
