;; -*- coding: utf-8 -*-
;; -----------------------------------------------------------------------------
;; ZHUO Qiang's Emacs Configuration 
;; -----------------------------------------------------------------------------

;; (setq debug-on-error t)

;; Change default encoding to UTF-8 for Emacs Python Shell

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setenv "LC_CTYPE" "UTF-8")
(setenv "LANG" "zh_CN.UTF8")

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
 '("Microsoft Yahei" "STHeiti" "hei" "文泉驿等宽微米黑" "新宋体" "宋体") 16)

;; (set-face-attribute 'default nil :height 100)

(setq text-scale-mode-step 1.1)
;; For Linux
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)
;; For Windows
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(global-set-key (kbd "C-x C-o") 'ff-find-other-file)

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
(setq-default tab-width 2)
(column-number-mode t)
(setq display-time-day-and-date t)
(display-time-mode t)
(setq show-paren-style 'parentheses)
(show-paren-mode t)
;; (when window-system
;;   (require 'hl-line+)
;;   (toggle-hl-line-when-idle t))

(fset 'yes-or-no-p 'y-or-n-p)
(setq x-stretch-cursor t) ; Stretch the cursor on TABs.
(setq-default comint-process-echoes t) ;Do not echo the input in shell.
(setq-default next-line-add-newlines nil)
(setq visible-bell nil) ;; visible bell picture does not play well under Mac
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
     ;; (setq-default compile-command "scons -u ")
     (setq-default compile-command "touch CMakeLists.txt && cmake --build build && cd build; ctest -VV")
     (add-to-list 'compilation-error-regexp-alist 
                  '("^ *File \"\\([^,\" \n\t]+\\)\", line \\([0-9]+\\)" 1 2))
     (add-to-list 'compilation-error-regexp-alist 
                  '("^[ \t]*\\(.+?\\)(\\([0-9]+\\),\\([0-9]+\\)): \\(error\\|warning\\): " 1 2 3))
     (add-to-list 'compilation-error-regexp-alist 
                  '("^\\(.*\\)(\\([0-9]+\\)):" 1 2))
     ;; for `googletest unknown failure'
     ;; 1: unknown file: Failure
     (add-to-list 'compilation-error-regexp-alist 
                  '("^\\([0-9+]: \\)?unknown file: Failure"))     
     ;; for `ctest -VV` output
     ;; 1: /Users/will/Projects/vmi-quic/vmi-server/system/remoted4/rmx/test/test_protocol.cpp:30: Failure
     (add-to-list 'compilation-error-regexp-alist 
                  '("^[0-9]+: \\(.+?\\):\\([0-9]+\\): Failure" 1 2))
     ))

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

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; (c-add-style
;;  "qiang"
;;  '("bsd"
;;    (indent-tabs-mode . nil)
;;    (c-basic-offset . 4)))

;; (add-hook
;;  'c-mode-common-hook '
;;  (lambda ()
;;    (c-set-style "qiang")
;;    (auto-fill-mode)
;;    ;; (c-toggle-auto-newline)
;;    (c-toggle-auto-hungry-state)))

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
;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (require 'python-mode)
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq auto-mode-alist
      (cons '("\\(\\.py[w]?\\)\\|\\(sconscript\\)\\|\\(sconstruct\\)\\|\\(SConstruct\\)\\|\\(SConscript\\)$" . python-mode)
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
 (cons '("\\.\\(xml\\|html\\|zcml\\)$" . nxml-mode)
       auto-mode-alist))


(setq
 auto-mode-alist 
 (cons '("\\.\\(aspx\\|ascx\\|cshtml\\)$" . nxml-mode)
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
      (cons '("\\.md$" . markdown-mode) auto-mode-alist))


;; SCSS mode
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
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

(require 'jinja2-mode)

(setq js-indent-level 2)

;; coffeescript
(require 'coffee-mode)
(require 'handlebars-mode)

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
      ;; (maximize-frame)
      )))

(auto-insert-mode 1)
(setq auto-insert-alist '()) ; clear build-in insert templates
(qiang-define-auto-insert "\\.\\(h\\|hpp\\|hxx\\|hmm\\)$" "h")
(qiang-define-auto-insert "\\.\\(html\\)$" "html5")
(qiang-define-auto-insert "\\.\\(rst\\)$" "h")
(qiang-define-auto-insert "\\.\\(py\\|pyw\\)$" "h")
(setq auto-insert-query nil)

(when (memq window-system '(mac ns))
  (progn
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))

(global-set-key (kbd "<f7>") 'next-error)

;; spell check settings
;; you need to install `brew install aspell --with-lang-en`
(ispell-change-dictionary "american" t)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'html-mode-hook 'flyspell-mode)
(add-hook 'lxml-mode-hook 'flyspell-mode)
(add-hook 'c++-mode-hook 'flyspell-prog-mode)
(add-hook 'c-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)


;; C++ 11 
(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)

;; fix C++ lambda indent level
(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued C++11 lambda function opened as argument."
  (setq ad-return-value
        (if (and (equal major-mode 'c++-mode)
                 (ignore-errors
                   (save-excursion
                     (goto-char (c-langelem-pos langelem))
                     ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                     ;;   and with unclosed brace.
                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
            0                           ; no additional indent
          ad-do-it)))                   ; default behavior

;; need install clang-format via `brew install clang-format`
(require 'clang-format)
(global-set-key [C-M-tab] 'clang-format-region)

(require 'cmake-mode)

(setq enable-local-eval t)

(require 'capnp-mode)
(add-to-list 'auto-mode-alist '("\\.capnp\\'" . capnp-mode))

(require 'package) ;; You might already have this line
;;;; use mirrorfor speedup
;; (add-to-list 'package-archives
;;              '("popkit" . "https://elpa.popkit.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
;; (when (< emacs-major-version 24)
;;   ;; For important compatibility libraries like cl-lib
;;   (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; (add-to-list 'package-archives
;;              '("popkit" . "https://elpa.popkit.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/"))
;; (when (< emacs-major-version 24)
;;   ;; For important compatibility libraries like cl-lib
;;   (add-to-list 'package-archives '("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
(package-initialize) ;; You might already have this line

(if (and (boundp 'custom-theme-load-path) t)
    (progn
      (add-to-list 'custom-theme-load-path (qiang-in-emacs-directory "themes"))
      ;; (load-theme 'base16-tomorrow-dark t)
      ;; (load-theme 'base16-solarized-dark t)
      ;; (load-theme 'base16-ocean-dark t)         
      (load-theme 'tango-dark t)       
      ;; (load-theme 'seti t) 
      ;; (load-theme 'deeper-blue t)
      )
  (progn
    (require 'color-theme)
    (eval-after-load "color-theme"
      (if window-system
          '(progn
             (color-theme-initialize)
             (load "color-theme-blackboard")
             (color-theme-blackboard) (color-theme-calm-forest))))))

;; package-install base16-theme
;; (load-theme 'base16-tomorrow-dark t)

;; package-install magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; package-install exec-path-from-shell
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))


;; Enable puml-mode for PlantUML files, put it last since plantuml might not work
(setq puml-plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
(require 'puml-mode)
(add-to-list 'auto-mode-alist '("\\.puml\\'" . puml-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . puml-mode))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
