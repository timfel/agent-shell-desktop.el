;;; agent-shell-desktop-tests.el --- Tests for agent-shell-desktop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tim Felgentreff

;;; Commentary:

;; Tests for `agent-shell-desktop'.

;;; Code:

(require 'cl-lib)
(require 'desktop)
(require 'ert)
(require 'map)
(require 'agent-shell-desktop)

(defun agent-shell-desktop-tests--state (&optional session-id config-id)
  "Return an Agent Shell state for SESSION-ID and CONFIG-ID."
  (let ((state (agent-shell--make-state
                :buffer (current-buffer)
                :agent-config (list (cons :identifier
                                          (or config-id 'codex))))))
    (when session-id
      (map-put! (map-elt state :session) :id session-id))
    state))

(ert-deftest agent-shell-desktop-mode-installs-handler ()
  "The global mode should install and remove Desktop integration."
  (unwind-protect
      (progn
        (agent-shell-desktop-mode 1)
        (should (member agent-shell-desktop--handler
                        desktop-buffer-mode-handlers))
        (should (memq #'agent-shell-desktop--setup-buffer
                      agent-shell-mode-hook)))
    (agent-shell-desktop-mode -1))
  (should-not (member agent-shell-desktop--handler
                      desktop-buffer-mode-handlers))
  (should-not (memq #'agent-shell-desktop--setup-buffer
                    agent-shell-mode-hook)))

(ert-deftest agent-shell-desktop-save-buffer-records-session ()
  "Desktop save data should identify the exact session to restore."
  (with-temp-buffer
    (let ((agent-shell-desktop-mode t))
      (setq major-mode 'agent-shell-mode)
      (setq default-directory temporary-file-directory)
      (setq-local agent-shell--state
                  (agent-shell-desktop-tests--state "session-123"))
      (let ((misc (agent-shell-desktop--save-buffer temporary-file-directory)))
        (should (equal (map-elt misc :version) 1))
        (should (equal (map-elt misc :session-id) "session-123"))
        (should (eq (map-elt misc :config-id) 'codex))
        (should (equal (map-elt misc :buffer-name) (buffer-name)))))))

(ert-deftest agent-shell-desktop-save-buffer-skips-missing-session ()
  "Desktop save should skip shells without an active session."
  (with-temp-buffer
    (let ((agent-shell-desktop-mode t))
      (setq major-mode 'agent-shell-mode)
      (setq-local agent-shell--state
                  (agent-shell-desktop-tests--state))
      (should-not (agent-shell-desktop--save-buffer
                   temporary-file-directory)))))

(ert-deftest agent-shell-desktop-save-buffer-respects-disabled-mode ()
  "Desktop save should skip shells when the mode is disabled."
  (with-temp-buffer
    (let ((agent-shell-desktop-mode nil))
      (setq major-mode 'agent-shell-mode)
      (setq-local agent-shell--state
                  (agent-shell-desktop-tests--state "session-123"))
      (should-not (agent-shell-desktop--save-buffer
                   temporary-file-directory)))))

(ert-deftest agent-shell-desktop-mode-hook-enables-save-buffer ()
  "Agent Shell buffers should ask Desktop to call the save hook."
  (unwind-protect
      (progn
        (agent-shell-desktop-mode 1)
        (with-temp-buffer
          (setq major-mode 'agent-shell-mode)
          (run-hooks 'agent-shell-mode-hook)
          (should (eq desktop-save-buffer
                      #'agent-shell-desktop--save-buffer))))
    (agent-shell-desktop-mode -1)))

(ert-deftest agent-shell-desktop-restore-reuses-existing-session-buffer ()
  "Desktop restore should not create a duplicate for an already-live session."
  (let ((buffer (generate-new-buffer " *agent-shell-desktop-existing*"))
        (desktop-dirname temporary-file-directory)
        (agent-shell-desktop-mode t)
        (agent-shell-agent-configs
         '(((:identifier . codex)
            (:buffer-name . "Codex")))))
    (unwind-protect
        (with-current-buffer buffer
          (setq major-mode 'agent-shell-mode)
          (setq-local agent-shell--state
                      (agent-shell-desktop-tests--state "session-123"))
          (should
           (eq (agent-shell-desktop--restore-buffer
                nil "restored"
                `((:directory . ,temporary-file-directory)
                  (:session-id . "session-123")
                  (:config-id . codex)))
               buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-desktop-restore-starts-exact-session ()
  "Desktop restore should start the saved session id and buffer name."
  (let ((desktop-dirname temporary-file-directory)
        (agent-shell-desktop-mode t)
        (agent-shell-agent-configs
         '(((:identifier . codex)
            (:buffer-name . "Codex"))))
        (captured-args nil)
        (captured-directory nil)
        (captured-name nil)
        (result-buffer (generate-new-buffer " *agent-shell-desktop-restored*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--start)
                   (lambda (&rest args)
                     (setq captured-args args)
                     (setq captured-directory default-directory)
                     result-buffer))
                  ((symbol-function 'shell-maker-set-buffer-name)
                   (lambda (buffer name)
                     (should (eq buffer result-buffer))
                     (setq captured-name name))))
          (should
           (eq (agent-shell-desktop--restore-buffer
                nil "restored"
                `((:directory . ,temporary-file-directory)
                  (:session-id . "session-123")
                  (:buffer-name . "saved buffer")
                  (:config-id . codex)))
               result-buffer))
          (should (equal captured-directory
                         (file-name-as-directory
                          (expand-file-name temporary-file-directory))))
          (should (equal (plist-get captured-args :session-id)
                         "session-123"))
          (should (plist-get captured-args :no-focus))
          (should (plist-get captured-args :new-session))
          (should (equal captured-name "saved buffer"))
          (should (equal (plist-get captured-args :config)
                         '((:identifier . codex)
                           (:buffer-name . "Codex")))))
      (when (buffer-live-p result-buffer)
        (kill-buffer result-buffer)))))

(ert-deftest agent-shell-desktop-restore-respects-disabled-mode ()
  "Desktop restore should return an explanatory buffer when disabled."
  (let ((desktop-dirname temporary-file-directory)
        (agent-shell-desktop-mode nil))
    (let ((buffer (agent-shell-desktop--restore-buffer
                   nil "restored"
                   `((:directory . ,temporary-file-directory)
                     (:session-id . "session-123")
                     (:config-id . codex)))))
      (unwind-protect
          (with-current-buffer buffer
            (should (string-match-p "restore is disabled"
                                    (buffer-string))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest agent-shell-desktop-restore-missing-data-returns-message-buffer ()
  "Desktop restore should return an explanatory buffer for invalid state."
  (let ((desktop-dirname temporary-file-directory)
        (agent-shell-desktop-mode t))
    (let ((buffer (agent-shell-desktop--restore-buffer
                   nil "restored"
                   `((:directory . ,temporary-file-directory)
                     (:config-id . codex)))))
      (unwind-protect
          (with-current-buffer buffer
            (should (string-match-p "missing an Agent Shell session ID"
                                    (buffer-string))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest agent-shell-desktop-restore-missing-config-returns-message-buffer ()
  "Desktop restore should return an explanatory buffer for unknown configs."
  (let ((desktop-dirname temporary-file-directory)
        (agent-shell-desktop-mode t)
        (agent-shell-agent-configs nil))
    (let ((buffer (agent-shell-desktop--restore-buffer
                   nil "restored"
                   `((:directory . ,temporary-file-directory)
                     (:session-id . "session-123")
                     (:config-id . codex)))))
      (unwind-protect
          (with-current-buffer buffer
            (should (string-match-p "No Agent Shell config found"
                                    (buffer-string))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(provide 'agent-shell-desktop-tests)
;;; agent-shell-desktop-tests.el ends here
