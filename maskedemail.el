(require 's)
(require 'cl)
(require 'dash)

(defgroup maskedemail nil
  "Handle MaskedEmail provided by FastMail"
  :group 'tools)

(defcustom maskedemail-cli-path "maskedemail"
  "path of maskedemail cli binary"
  :type 'string
  :group 'maskedemail
  )

(cl-defstruct maskedemail "Masked Email Struct" email domain state)

(defvar all-maskedemails nil "All Masked Emails")


(defun maskedemail-create ()
  (interactive)
  )

(defun maskedemail-list ()
  (interactive)
  (with-temp-buffer
    (let* ((list-output (shell-command-to-string (format "%s -e list" maskedemail-cli-path)))
           (list-output (string-trim list-output))
           (list-output-lines (s-split "\n" list-output)))
      ;; (message "%s" list-output-lines)
      (setq all-maskedemails nil)
      (dolist (line list-output-lines)
        (let* ((fields (s-split " " line))
               (fields-with-key (-interleave '(:email :domain :state) fields))
               (m (apply 'make-maskedemail fields-with-key)))
          (push m all-maskedemails)))
      all-maskedemails)))

(defun maskedemail-refresh ()
  (interactive)
  (setq all-maskedemails nil)
  (maskedemail-list))

(defun maskedemail-select ()
  "Selete MaskedEmail based on domain and copy to kill ring (clipboard)"
  (interactive)
  (unless all-maskedemails (maskedemail-list))
  (let* ((domains  (-map (lambda (m)
                           `(,(maskedemail-domain m) . ,m))
                         all-maskedemails))
         (chosen (alist-get
                  (completing-read "Domain: " domains)
                  domains nil nil 'equal))
         (chosen-email       (maskedemail-email chosen))
         (chosen-domain       (maskedemail-domain chosen))
         (chosen-state       (maskedemail-state chosen)))
    (kill-new chosen-email)
    chosen-email))
