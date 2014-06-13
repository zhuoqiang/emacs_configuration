;; -*- coding: utf-8 -*-
;; -----------------------------------------------------------------------------
;; ZHUO Qiang's Emacs Configuration 
;; -----------------------------------------------------------------------------

;; (setq debug-on-error t)

(when (not (equal window-system 'w32))
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin"))))

;; add load path
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "default user emacs directory"))

(defun qiang-in-emacs-directory (path)
  (concat user-emacs-directory (convert-standard-filename path)))

(let ((default-directory 
        (qiang-in-emacs-directory "lisp")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(load-library "qiang")

(qiang-set-font
 '("Monaco" "Consolas" "DejaVu Sans Mono" "Monospace" "Courier New")
 '("Microsoft Yahei" "STHeiti" "hei" "文泉驿等宽微米黑" "新宋体" "宋体"))

(setq text-scale-mode-step 1.1)
;; For Linux
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)
;; For Windows
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(setq delete-by-moving-to-trash t)
(when (eq system-type 'darwin)
  (setq trash-directory "~/.Trash/emacs")
  ;; (setq mac-option-modifier 'hyper) ; Option key as Hyper
  ;; (setq mac-option-modifier 'super) ; Option key as Super
  ;; (setq mac-control-modifier 'ctrl) ; Control key as Ctrl
  (setq mac-command-modifier 'meta) ; Command key as Meta
  (setq ns-pop-up-frames nil); open file in current frame
  (global-set-key (kbd "<s-wheel-up>") 'text-scale-increase)
  (global-set-key (kbd "<s-wheel-down>") 'text-scale-decrease)
  )


;; It is default now since Emacs 24
;; (setq x-select-enable-clipboard t)
;; (setq interprogram-paste-function 'x-selection-value)
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))

(setq-default buffer-file-coding-system 'utf-8-unix)

;; (setq locale-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")

(setq frame-title-format "%f %4 %b %Z %* %10 %I")
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(column-number-mode t)
(setq display-time-day-and-date t)
(display-time-mode t)
(setq show-paren-style 'parentheses)
(show-paren-mode t)
(when window-system
  (require 'hl-line+)
  (toggle-hl-line-when-idle t))

(fset 'yes-or-no-p 'y-or-n-p)
(setq x-stretch-cursor t) ; Stretch the cursor on TABs.
(setq-default comint-process-echoes t) ;Do not echo the input in shell.
(setq-default next-line-add-newlines nil)
(setq visible-bell t)
(setq enable-recursive-minibuffers t)
(setq scroll-margin 5
      scroll-conservatively 1000)


;; Chinese sentence
(setq sentence-end
      "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*") 
(setq sentence-end-double-space nil)

(desktop-save-mode t)
(setq desktop-dirname user-emacs-directory)
(setq desktop-path `(,user-emacs-directory))
(setq desktop-restore-eager 5)

(if (functionp 'electric-pair-mode) (electric-pair-mode 1))

(setq ido-save-directory-list-file (qiang-in-emacs-directory "ido.last"))
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-file-extensions-order
      '(".org" ".txt" ".py" ".h" ".cpp" ".c" ".hpp" 
        ".emacs" ".xml" ".el" ".ini" ".cfg" ".java"))
(ido-mode t)
(setq ido-ignore-extensions t)
(setq ido-slow-ftp-host-regexps '(".*"))
(setq ido-work-directory-list-ignore-regexps '(".*"))
(add-to-list 'ido-ignore-files "\\.DS_Store")

(setq bookmark-save-flag 1)
(setq kill-whole-line t)

(global-set-key [(meta g)] 'goto-line)
(global-set-key [(f5)] 'compile)
(global-set-key (kbd "<C-f5>") 'revert-buffer)
(global-set-key [(f8)] 'next-error)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-S-o") 'qiang-open-next-line)
(global-set-key (kbd "C-o") 'qiang-open-previous-line)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol)) 
(global-set-key (kbd "M-/") 'hippie-expand)

(icomplete-mode 1)

(setq backup-directory-alist 
      `((".*" . ,(qiang-in-emacs-directory "backup"))))
(setq auto-save-file-name-transforms 
      `((".*" ,(qiang-in-emacs-directory "auto-save") t)))

(eval-after-load "compile"
  '(progn
     (setq-default compile-command "scons -u ")
     (add-to-list 'compilation-error-regexp-alist 
                  '("^ *File \"\\([^,\" \n\t]+\\)\", line \\([0-9]+\\)" 1 2))
     (add-to-list 'compilation-error-regexp-alist 
                  '("^[ \t]*\\(.+?\\)(\\([0-9]+\\),\\([0-9]+\\)): \\(error\\|warning\\): " 1 2 3))
     (add-to-list 'compilation-error-regexp-alist 
                  '("^\\(.*\\)(\\([0-9]+\\)):" 1 2))))

(global-set-key "\M-;" 'qiang-comment-dwim-line)
(global-set-key "\C-cR" 'qiang-rename-current-file-or-buffer)

;; auto re-format when pasting code
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice, command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode lisp-mode clojure-mode scheme-mode
                                     haskell-mode ruby-mode rspec-mode
                                     c-mode c++-mode objc-mode nxml-mode
                                     latex-mode js-mode plain-tex-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

(global-set-key (kbd "M-k") 'qiang-copy-line)

;; fix server unsafe error under windows
(require 'server)
(when (and (>= emacs-major-version 23)
           (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t))

(if window-system (server-mode 1))

;; Emacs-List Mode
(define-key emacs-lisp-mode-map [(f5)] 'eval-buffer)

;; CC Mode
(c-add-style
 "qiang"
 '("bsd"
   (indent-tabs-mode . nil)
   (c-basic-offset . 4)))

(add-hook
 'c-mode-common-hook '
 (lambda ()
   (c-set-style "qiang")
   (auto-fill-mode)
   ;; (c-toggle-auto-newline)
   (c-toggle-auto-hungry-state)))

(setq
 auto-mode-alist 
 (append
  '(("\\.\\(vsh\\|fsh\\|c\\)$" . c-mode)
    ("\\.\\(cpp|\\cxx\\|h\\|hpp\\|hxx\\|cc\\|dox\\|mm\\|hmm\\)$" . c++-mode)
    ("\\.\\(m\\|mm\\)$" . objc-mode))
    auto-mode-alist))

;; Java
(add-hook
 'java-mode-hook '
 (lambda ()
   (c-set-style "java")
   (auto-fill-mode)
   (c-toggle-auto-hungry-state t)))

(add-hook 'find-file-hook 'qiang-choose-header-mode)


;; VB dotNET mode
(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
                                 vbnet-mode)) auto-mode-alist))

;; hexl Mode
(setq auto-mode-alist
      (cons '("\\.\\(exe\\|dll\\|so\\|o\\)$" . hexl-mode)
            auto-mode-alist))


;; python Mode
(setq auto-mode-alist
      (cons '("\\(\\.py[w]?\\)\\|\\(sconscript\\)\\|\\(sconstruct\\)\\|\\(SConstruct\\)\\|\\(SConscript\\)|\\(config\\)$" . python-mode)
            auto-mode-alist))

(require 'cython-mode)

(require 'virtualenv)

;; Lua Mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


;; Shell Mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; nXML Mode
(setq
 auto-mode-alist 
 (cons '("\\.\\(xml\\|html\\|zcml\\)" . nxml-mode)
       auto-mode-alist))
(add-hook
 'nxml-mode-hook 
 (lambda () (define-key nxml-mode-map (kbd "C-c v") 'browse-url-of-buffer)))

(setq nxml-slash-auto-complete-flag t)
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files (qiang-in-emacs-directory "lisp/html5-el/schemas.xml")))
(require 'whattf-dt)

;; Markdown Mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))


;; SCSS mode
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

(require 'sass-mode)


;; Org Mode
(setq org-startup-truncated nil)

(require 'yasnippet) 
(yas-global-mode 1)
(when (not window-system)
  (setq-default yas-trigger-key "TAB"))
(setq yas-also-auto-indent-first-line t)

(require 'ahg)

(if (and (boundp 'custom-theme-load-path) t)
    (progn
      (add-to-list 'custom-theme-load-path (qiang-in-emacs-directory "themes"))
      (add-to-list 'custom-theme-load-path (qiang-in-emacs-directory "lisp/emacs-color-theme-solarized"))
      (load-theme 'manoj-dark t)
      ;; (load-theme 'zenburn t)
      ;; (load-theme 'solarized-light t)
      ;; (load-theme 'solarized-dark t)
      )
  (progn
    (require 'color-theme)
    (eval-after-load "color-theme"
      (if window-system
          '(progn
             (color-theme-initialize)
             ;; (load "color-theme-blackboard")
             (load "color-theme-solarized-dark")
             (color-theme-blackboard))))))

(require 'quick-jump)
(quick-jump-default-keybinding)

(require 'breadcrumb)
(global-set-key [(control f2)] 'bc-set)
(global-set-key [(f2)] 'bc-previous)
(global-set-key [(shift f2)] 'bc-next)
(global-set-key [(meta f2)] 'bc-list)

(global-set-key "\C-ci" 'qiang-ido-imenu-symbol)

(when window-system
  (if (eq system-type 'windows-nt)
      (w32-send-sys-command 61488)
    (progn
      (require 'frame-cmds)
      (maximize-frame))))

(auto-insert-mode 1)
(setq auto-insert-alist '()) ; clear build-in insert templates
(qiang-define-auto-insert "\\.\\(h\\|hpp\\|hxx\\|hmm\\)$" "h")
(qiang-define-auto-insert "\\.\\(html\\)" "html5")
(qiang-define-auto-insert "\\.\\(rst\\)" "h")
(qiang-define-auto-insert "\\.\\(py\\|pyw\\)$" "h")
(setq auto-insert-query nil)

(when (memq window-system '(mac ns))
  (progn
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))
