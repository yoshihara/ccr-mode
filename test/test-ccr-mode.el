(require 'ert)

(setq this-file-path
      (file-name-directory (or load-file-name (buffer-file-name))))
(add-to-list 'load-path (format "%s../" this-file-path))
(require 'ccr-mode)

  (ert-deftest ccr:github-url-exists ()
    (let ((expected-github-url
           "https://github.com/clear-code/commit-comment-tools/commit/1e75018"))
    (with-temp-buffer
      (insert "clear-code/commit-comment-tools@1e75018: this is ....")
      (goto-char (point-min))
      (should (equal expected-github-url
                     (commit-comment-report-github-url))))))

  (ert-deftest ccr:github-url-not-exists ()
    (with-temp-buffer
      (insert "invalid-commit@1e75018: this is not commit in GitHub.")
      (goto-char (point-min))
      (should (equal nil
                     (commit-comment-report-github-url)))))

(ert-run-tests-batch-and-exit)
