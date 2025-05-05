;; random perf hacks from https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold (* 1024 1024 128))
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Using elpaca
(setq package-enable-at-startup nil)

(when (eq window-system 'android)
  (setenv "PATH" (concat "/data/data/com.termux/files/usr/bin:" (getenv "PATH")))
  (add-to-list exec-path "/data/data/com.termux/files/usr/bin"))
