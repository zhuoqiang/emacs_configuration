(defun qiang-best-font-size ()
  (if window-system
      (/ (x-display-pixel-height) 47)
    0))

(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font))
      nil t))

(defun qiang-set-font (english-fonts
                       chinese-fonts
                       &optional font-size)
  "If font size is nil, use best font size"
  ;; 
  ;; The following 2 method cannot make the new frame work.
  ;; (set-default-font "Consolas:pixelsize=18")
  ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
  ;; We will use set-face-attribute
  
  (require 'cl)                         ; for find if
  (if window-system
      (let ((en-font (find-if #'qiang-font-existsp english-fonts))
            (zh-font (find-if #'qiang-font-existsp chinese-fonts))
            (font-size (if font-size font-size (qiang-best-font-size))))

        (message "Set Font to [%s, %s, %d]" en-font zh-font font-size)
        (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" en-font font-size))
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font) charset
                            (font-spec :family zh-font
                                        ;:size font-size ; wrong under windows
                                       ))))
    (message "Emacs in console will skip font setting.")))


(defun qiang-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun qiang-rename-current-file-or-buffer ()
  (interactive)
  (if (not (buffer-file-name))
      (call-interactively 'rename-buffer)
    (let ((file (buffer-file-name)))
      (with-temp-buffer
        (set-buffer (dired-noselect file))
        (dired-do-rename)
        (kill-buffer nil))))
  nil)

;; Smart copy, if no region active, it simply copy the current whole line
(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode js-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end))
                 (message "Copied line")
                 (list (line-beginning-position)
                       (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Copy line from point to the end, exclude the line break
(defun qiang-copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (point)
                  (line-end-position))
  ;; (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))


(defun qiang-choose-header-mode ()
  (interactive)
  (if (string-equal (substring (buffer-file-name) -2) ".h")
      (progn
        ;; OK, we got a .h file, if a .m file exists we'll assume it's an objective c file.
        ;; Otherwise, we'll look for a .cpp file.
        (let ((dot-m-file (concat (substring (buffer-file-name) 0 -1) "m"))
              (dot-cpp-file (concat (substring (buffer-file-name) 0 -1) "cpp")))
          (if (file-exists-p dot-m-file)
              (progn
                (objc-mode))
            (if (file-exists-p dot-cpp-file)
                (c++-mode)))))))

(defun qiang-system-name-is(name)
  (interactive)
  (string-equal (downcase system-name) name))

(defun qiang-ido-imenu-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (qiang-ido-imenu-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (qiang-ido-imenu-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))


;;;; Snippet hyperlink
(defun qiang-delete-current-link-maybe ()
  (ignore-errors
    (if (save-excursion
          (beginning-of-line)
          (looking-at (concat "^[ \t]*" (if comment-start (regexp-quote comment-start) "#"))))
        (beginning-of-line)
      (delete-region (point) (progn (forward-sexp -1) (point))))))

(defun qiang-expand-link (key)
  "Hyperlink function for yasnippet expansion."
  (qiang-delete-current-link-maybe)
  (insert key)
  (yas/expand))

;;;; auto-insert with yasnippet
(defun qiang-define-auto-insert (condition snippet-key &optional after)
  "Set `auto-insert-alist' to expand SNIPPET-KEY at file creation.

CONDITION may be a regexp that must match the new file's name, or it may be
a symbol that must match the major mode for this element to apply.

Associate CONDITION with SNIPPET-KEY in `auto-insert-alist'.
Optional AFTER means to insert snippet after all existing snippets for CONDITION."
  (add-to-list 'auto-insert-alist `(,condition . (lambda () (qiang-expand-link ,snippet-key))) after))

(defun qiang-open-previous-line (arg)
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(defun qiang-open-next-line (arg)
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))
