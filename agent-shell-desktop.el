;;; agent-shell-desktop.el --- Desktop restore support for Agent Shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tim Felgentreff

;; Author: Tim Felgentreff <tim.felgentreff@oracle.com>
;; URL: https://github.com/timfel/agent-shell-desktop.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.50.1") (shell-maker "0.90.1"))
;; Keywords: tools, convenience, processes

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Persist active `agent-shell-mode' buffers through Emacs Desktop.
;;
;; Enable `agent-shell-desktop-mode' before `desktop-read' runs.  Active
;; Agent Shell buffers with a session id are saved by `desktop-save' and
;; restored by `desktop-read' using the same session id, agent config,
;; working directory, and buffer name.

;;; Code:

(require 'agent-shell)
(require 'desktop)
(require 'map)
(require 'seq)
(require 'shell-maker)

(defgroup agent-shell-desktop nil
  "Desktop session restore support for Agent Shell."
  :group 'agent-shell
  :prefix "agent-shell-desktop-")

(defconst agent-shell-desktop--version "0.1.0")

(defvar agent-shell-desktop-mode)
(defvar desktop-buffer-mode-handlers)
(defvar desktop-dirname)
(defvar desktop-save-buffer)

(defconst agent-shell-desktop--handler
  '(agent-shell-mode . agent-shell-desktop--restore-buffer))

(defun agent-shell-desktop--setup-buffer ()
  "Configure the current Agent Shell buffer for Desktop saving."
  (when agent-shell-desktop-mode
    (setq-local desktop-save-buffer #'agent-shell-desktop--save-buffer)))

(defun agent-shell-desktop--clear-buffer ()
  "Remove this package's Desktop save function from the current buffer."
  (when (eq desktop-save-buffer #'agent-shell-desktop--save-buffer)
    (kill-local-variable 'desktop-save-buffer)))

(defun agent-shell-desktop--setup-existing-buffers ()
  "Configure existing Agent Shell buffers for Desktop saving."
  (dolist (buffer (agent-shell-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (agent-shell-desktop--setup-buffer)))))

(defun agent-shell-desktop--clear-existing-buffers ()
  "Remove this package's Desktop save function from existing buffers."
  (dolist (buffer (agent-shell-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (agent-shell-desktop--clear-buffer)))))

(defun agent-shell-desktop--enable ()
  "Install Desktop integration hooks."
  (add-hook 'agent-shell-mode-hook #'agent-shell-desktop--setup-buffer)
  (add-to-list 'desktop-buffer-mode-handlers agent-shell-desktop--handler)
  (agent-shell-desktop--setup-existing-buffers))

(defun agent-shell-desktop--disable ()
  "Remove Desktop integration hooks."
  (remove-hook 'agent-shell-mode-hook #'agent-shell-desktop--setup-buffer)
  (setq desktop-buffer-mode-handlers
        (seq-remove (lambda (entry)
                      (equal entry agent-shell-desktop--handler))
                    desktop-buffer-mode-handlers))
  (agent-shell-desktop--clear-existing-buffers))

(defun agent-shell-desktop--save-buffer (desktop-directory)
  "Return Desktop restore data for the current `agent-shell-mode' buffer.
DESKTOP-DIRECTORY is the directory where Desktop writes its state."
  (when (and agent-shell-desktop-mode
             (derived-mode-p 'agent-shell-mode))
    (when-let ((session-id (map-nested-elt (agent-shell--state)
                                           '(:session :id)))
               (config-id (map-nested-elt (agent-shell--state)
                                          '(:agent-config :identifier))))
      `((:version . 1)
        (:directory . ,(desktop-file-name
                        (file-name-as-directory
                         (expand-file-name default-directory))
                        desktop-directory))
        (:session-id . ,session-id)
        (:config-id . ,config-id)
        (:buffer-name . ,(buffer-name))))))

(defun agent-shell-desktop--existing-buffer (session-id config-id)
  "Return an existing shell buffer for SESSION-ID and CONFIG-ID, or nil."
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (and (derived-mode-p 'agent-shell-mode)
            (equal (map-nested-elt (agent-shell--state) '(:session :id))
                   session-id)
            (eq (map-nested-elt (agent-shell--state)
                                '(:agent-config :identifier))
                config-id))))
   (agent-shell-buffers)))

(defun agent-shell-desktop--config (config-id)
  "Return the Agent Shell config matching CONFIG-ID."
  (seq-find (lambda (candidate)
              (eq (map-elt candidate :identifier) config-id))
            agent-shell-agent-configs))

(defun agent-shell-desktop--restore-message-buffer (buffer-name message)
  "Return a scratch BUFFER-NAME containing Desktop restore MESSAGE."
  (let ((buffer (generate-new-buffer
                 (or buffer-name "*agent-shell desktop restore*"))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Could not restore agent-shell buffer.\n\n" message "\n"))
      (special-mode)
      (setq-local desktop-save-buffer nil))
    buffer))

(defun agent-shell-desktop--restore-buffer (_buffer-filename
                                            buffer-name
                                            buffer-misc)
  "Restore `agent-shell-mode' BUFFER-NAME from Desktop BUFFER-MISC."
  (let* ((session-id (map-elt buffer-misc :session-id))
         (config-id (map-elt buffer-misc :config-id))
         (saved-directory (map-elt buffer-misc :directory))
         (directory (and saved-directory
                         (file-name-as-directory
                          (expand-file-name saved-directory
                                            (or desktop-dirname
                                                default-directory)))))
         (config (and config-id
                      (agent-shell-desktop--config config-id)))
         (restore-buffer-name (or (map-elt buffer-misc :buffer-name)
                                  buffer-name)))
    (cond
     ((not agent-shell-desktop-mode)
      (agent-shell-desktop--restore-message-buffer
       restore-buffer-name
       "Agent Shell Desktop restore is disabled."))
     ((not session-id)
      (agent-shell-desktop--restore-message-buffer
       restore-buffer-name
       "Desktop entry is missing an Agent Shell session ID."))
     ((not directory)
      (agent-shell-desktop--restore-message-buffer
       restore-buffer-name
       "Desktop entry is missing an Agent Shell directory."))
     ((not (file-directory-p directory))
      (agent-shell-desktop--restore-message-buffer
       restore-buffer-name
       (format "Agent Shell Desktop directory is not available: %s"
               directory)))
     ((not config-id)
      (agent-shell-desktop--restore-message-buffer
       restore-buffer-name
       "Desktop entry is missing an Agent Shell config ID."))
     ((not config)
      (agent-shell-desktop--restore-message-buffer
       restore-buffer-name
       (format "No Agent Shell config found for %S." config-id)))
     (t
      (let ((shell-buffer (or (agent-shell-desktop--existing-buffer
                               session-id config-id)
                              (let ((default-directory directory))
                                (agent-shell--start :config config
                                                    :no-focus t
                                                    :new-session t
                                                    :session-id session-id)))))
        (when restore-buffer-name
          (shell-maker-set-buffer-name shell-buffer restore-buffer-name))
        shell-buffer)))))

;;;###autoload
(define-minor-mode agent-shell-desktop-mode
  "Persist active Agent Shell sessions with Emacs Desktop.

When enabled, active `agent-shell-mode' buffers with a session id are
saved by `desktop-save' and restored by `desktop-read'.  Enable this mode
before Desktop restores buffers, typically before `desktop-save-mode' or
`desktop-read' runs during startup."
  :global t
  :group 'agent-shell-desktop
  (if agent-shell-desktop-mode
      (agent-shell-desktop--enable)
    (agent-shell-desktop--disable)))

;;;###autoload
(defun agent-shell-desktop-version ()
  "Show `agent-shell-desktop' version."
  (interactive)
  (message "agent-shell-desktop v%s" agent-shell-desktop--version))

(provide 'agent-shell-desktop)

;;; agent-shell-desktop.el ends here
