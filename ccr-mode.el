(defvar commit-comment-report-mode-hook nil)

(defface commit-comment-report-date-face
  '((t (:foreground "SteelBlue1" :bold t :height 1.35))) nil)

(defface commit-comment-report-none-percentage-face
  '((t (:foreground "Red" :bold t :height 1.35))) nil)

(defface commit-comment-report-percentage-face
  '((t (:foreground "DeepSkyBlue1" :bold t :height 1.35))) nil)

(defface commit-comment-report-github-commit-face
  '((t (:foreground "violet" :underline t)))
  nil)

(defface commit-comment-report-other-commit-face
  '((t (:foreground "violet")))
  nil)

(defvar date-face            'commit-comment-report-date-face)
(defvar percentage-face      'commit-comment-report-percentage-face)
(defvar github-commit-face   'commit-comment-report-github-commit-face)
(defvar other-commit-face    'commit-comment-report-other-commit-face)
(defvar none-percentage-face 'commit-comment-report-none-percentage-face)

(setq commit-comment-report-date-format "%Y-%m-%d:%:")
(defun commit-comment-report-insert-date ()
  (interactive)
  (insert
   (format-time-string commit-comment-report-date-format (current-time))))

(defun commit-comment-report-open-commit ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "\\([-a-z0-9A-Z/\._]+?\\)@\\([a-z0-9A-Z]+\\)" nil t nil)
      (let ((repository (match-string 1))
            (commit-hash (match-string 2)))
        (if (string-match "/" repository)
            (browse-url (format "https://github.com/%s/commit/%s" repository commit-hash)))))))

(setq commit-comment-report-mode-font-lock-keywords
      '(("^\\([0-9][0-9][0-9][0-9]-[0-9][0-9]?-[0-9][0-9]?\\):\\([0-9]+%\\):?"
         (1 date-face nil t) (2 percentage-face nil t))
        ("^\\([0-9][0-9][0-9][0-9]-[0-9][0-9]?-[0-9][0-9]?:%\\):?"
         (1 none-percentage-face nil t))
        ("^  .*?\\([-a-z0-9A-Z._]+?\/[-a-z0-9A-Z._]+?@[a-z0-9A-Z]+\\)[: ]?" .
         (1 github-commit-face nil t))
        ("^  .*?\\([-a-z0-9A-Z._]+?@[a-z0-9A-Z]+\\)[: ]?" .
         (1 other-commit-face nil t))))

(defun commit-comment-report-mode ()
  "Major mode for editing Commit Comment Report."
  (interactive)

  (setq major-mode 'commit-comment-report-mode)
  (setq mode-name "Commit Comment Report mode")

  (setq ccr-keymap (make-keymap))
  (define-key ccr-keymap (kbd "C-c d") 'commit-comment-report-insert-date)
  (define-key ccr-keymap (kbd "C-c o") 'commit-comment-report-open-commit)
  (use-local-map ccr-keymap)

  (setq tab-width 2)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        `(,commit-comment-report-mode-font-lock-keywords
          t t nil nil))
  (add-hook 'commit-comment-report-mode-hook 'turn-on-font-lock)
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'commit-comment-report-mode-hook)
  (run-hooks 'commit-comment-report-mode-hook)))

(defun ccr-mode ()
  "Major mode for editing Commit Comment Report. (same as commit-comment-report-mode)"
  (interactive)
  (commit-comment-report-mode))

(provide 'ccr-mode)
