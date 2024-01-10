;;; Personal configuration -*- lexical-binding: t -*-
;; Generated at https://emacs.amodernist.com/ on 2024-01-09

;; Save the contents of this file under ~/.config/emacs/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.

;; Add the NonGNU ELPA package archive. Commented out because I use recent software that does this by default.
;; (require 'package)
;; (add-to-list 'package-archives  '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
;; (unless package-archive-contents  (package-refresh-contents))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Load a custom theme
(load-theme 'leuven t)

;;; Completion framework
(unless (package-installed-p 'vertico)
  (package-install 'vertico))

;; Enable completion by narrowing
(vertico-mode t)

;; Improve directory navigation
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-word)
  (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-char))

;;; Extended completion utilities
(unless (package-installed-p 'consult)
  (package-install 'consult))

;; Enable line numbering in `prog-mode'
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Automatically pair parentheses
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)

;;; LSP Support
(unless (package-installed-p 'eglot)
  (package-install 'eglot))

;;; Inline static analysis

;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)

;;; Pop-up completion
(unless (package-installed-p 'corfu)
  (package-install 'corfu))

(setq corfu-auto t)

;; Enable autocompletion by default in programming buffers
(add-hook 'prog-mode-hook #'corfu-mode)

;;; Git client
(unless (package-installed-p 'magit)
  (package-install 'magit))

;; Bind the `magit-status' command to a convenient key.
(global-set-key (kbd "C-c g") #'magit-status)

;; Show word-granularity differences within diff hunks
(setq magit-diff-refine-hunk t)

;;; Indication of local VCS changes
(unless (package-installed-p 'diff-hl)
  (package-install 'diff-hl))

;; Enable `diff-hl' support by default in programming buffers
(add-hook 'prog-mode-hook #'diff-hl-mode)

;; Update the highlighting without saving
(diff-hl-flydiff-mode t)

;;; Ada Support
(unless (package-installed-p 'ada-mode)
  (package-install 'ada-mode))

;;; Clojure Support
(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))

;;; Coq Support
(unless (package-installed-p 'proof-general)
  (package-install 'proof-general))

;;; C# Support
(unless (package-installed-p 'csharp-mode)
  (package-install 'csharp-mode))

;;; D Support
(unless (package-installed-p 'd-mode)
  (package-install 'd-mode))

;;; Elixir Support
(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))

;;; Go Support
(unless (package-installed-p 'go-mode)
  (package-install 'go-mode))

;;; Haskell Support
(unless (package-installed-p 'haskell-mode)
  (package-install 'haskell-mode))

;;; J Support
(unless (package-installed-p 'j-mode)
  (package-install 'j-mode))

;;; JSON Support
(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))

;;; Julia Support
(unless (package-installed-p 'julia-mode)
  (package-install 'julia-mode))

;;; Kotlin Support
(unless (package-installed-p 'kotlin-mode)
  (package-install 'kotlin-mode))

;;; Lua Support
(unless (package-installed-p 'lua-mode)
  (package-install 'lua-mode))

;;; NASM Support
(unless (package-installed-p 'nasm-mode)
  (package-install 'nasm-mode))

;;; PHP Support
(unless (package-installed-p 'php-mode)
  (package-install 'php-mode))

;;; Raku Support
(unless (package-installed-p 'raku-mode)
  (package-install 'raku-mode))

;;; Rust Support
(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))

;;; Scala Support
(unless (package-installed-p 'scala-mode)
  (package-install 'scala-mode))

;;; Additional Lisp support
(unless (package-installed-p 'sly)
  (package-install 'sly))

;;; Standard ML Support
(unless (package-installed-p 'sml-mode)
  (package-install 'sml-mode))

;;; Swift Support
(unless (package-installed-p 'swift-mode)
  (package-install 'swift-mode))

;;; Typescript Support
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))

;;; YAML Support
(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

;;; SWI-Prolog Support
(unless (package-installed-p 'sweeprolog)
  (package-install 'sweeprolog))

;; Use `sweeprolog-mode' instead of `prolog-mode'
(add-to-list 'auto-mode-alist '("\.plt?\'"  . sweeprolog-mode))

;;; LaTeX support
(unless (package-installed-p 'auctex)
  (package-install 'auctex))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Enable LaTeX math support
(add-hook 'LaTeX-mode-map #'LaTeX-math-mode)

;; Enable reference mangment
(add-hook 'LaTeX-mode-map #'reftex-mode)

;;; Markdown support
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

;;; Outline-based notes management and organizer
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)

;;; Additional Org-mode related functionality
(unless (package-installed-p 'org-contrib)
  (package-install 'org-contrib))

;;; Collaborative Editing
(unless (package-installed-p 'crdt)
  (package-install 'crdt))

;;; EditorConfig support
(unless (package-installed-p 'editorconfig)
  (package-install 'editorconfig))

;; Enable EditorConfig
(editorconfig-mode t)

;;; In-Emacs Terminal Emulation
(unless (package-installed-p 'eat)
  (package-install 'eat))

;; Close the terminal buffer when the shell terminates.
(setq eat-kill-buffer-on-exit t)

;; Enable mouse-support.
(setq eat-enable-mouse t)

;;; Jump to arbitrary positions
(unless (package-installed-p 'avy)
  (package-install 'avy))
(global-set-key (kbd "C-c z") #'avy-goto-word-1)

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "xdg-open")

(require 'xdg)

(dired-create-directory (concat (xdg-state-home) "/emacs/autosave/") t)
(dired-create-directory (concat (xdg-state-home) "/emacs/backups/") t)
;; Modified from https://www.emacswiki.org/emacs/AutoSave
(setq backup-directory-alist
      `(("." . ,(concat (xdg-state-home) "/emacs/backups/"))))

;; Modified from https://emacs.stackexchange.com/a/50968
(setq auto-save-file-name-transforms
      `((".*" ,(concat (xdg-state-home) "/emacs/autosave/") t)))

;; From https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-File-Lock-and-Backup.html
;; (customize-set-variable
;;  'tramp-backup-directory-alist backup-directory-alist)

;; From https://www.reddit.com/r/emacs/comments/1acg6q/comment/c8w2izv/?utm_source=share&utm_medium=web2x&context=3
(setq global-auto-revert-non-file-buffers t) (global-auto-revert-mode)

(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Vim emulation (not kicking this habit any time soon)
(evil-mode t)

;; Enable Vim emulation in programming buffers
(add-hook 'prog-mode-hook #'evil-local-mode)

;; Enable Vim emulation
(setq evil-default-state 'insert)

;; From https://stackoverflow.com/a/34589105
(setq-default show-trailing-whitespace t)
