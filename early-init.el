;; -*- lexical-binding: t; -*-

;; random perf hacks from https://emacs-lsp.github.io/lsp-mode/page/performance/  
(setq gc-cons-threshold (* 1024 1024 16)) ;; 16 mb
(setq read-process-output-max (* 1024 1024)) ;; 1 mb
(setq android-use-exec-loader nil)
;; Using elpaca
(setq package-enable-at-startup nil)

(unless (eq system-type 'android)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(let ((maybe-theme-path (locate-user-emacs-file "elpaca/builds/doom-themes/")))
  (message maybe-theme-path)
  (when (file-directory-p maybe-theme-path)
    (add-to-list 'load-path maybe-theme-path)
    (add-to-list 'custom-theme-load-path maybe-theme-path)
    (load-theme 'doom-laserwave t)))
