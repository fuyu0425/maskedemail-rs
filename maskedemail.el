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

(defvar all-maskedemails nil "All Masked Emails (served as a cache)")



(defun maskedemail-list ()
  "List all Masked Emails"
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
  "Refresh Masked Email list"
  (interactive)
  (setq all-maskedemails nil)
  (maskedemail-list))

(defun maskedemail-create ()
  (interactive)
  (with-temp-buffer
    (let* ((list-output (shell-command-to-string (format "%s -e create %s"
                                                         maskedemail-cli-path
                                                         (read-string "Create for domain: "))))
           (list-output (string-trim list-output)))
      (let* ((fields (s-split " " list-output))
             (fields-with-key (-interleave '(:email :domain :state) fields))
             (m (apply 'make-maskedemail fields-with-key)))
        (push m all-maskedemails)
        (message "%s" (maskedemail-email m))))))

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

(defun maskedemail-enable ()
  "Enable MaskedEmail"
  (interactive)
  (unless all-maskedemails (maskedemail-list))
  (let* ((enabled-maskedemails (-filter (lambda (m)
                                          (member (maskedemail-state m)
                                                  '("pending" "disabled")))
                                        all-maskedemails))
         (emails  (-map (lambda (m)
                          `(,(maskedemail-email m) . ,m))
                        enabled-maskedemails))
         (chosen (alist-get
                  (completing-read "Email: " emails)
                  emails nil nil 'equal))
         (chosen-email       (maskedemail-email chosen))
         (chosen-domain       (maskedemail-domain chosen))
         (chosen-state       (maskedemail-state chosen)))
    (shell-command (format "%s -e enable %s"
                           maskedemail-cli-path
                           chosen-email))
    ;; NOTE: can be optimized if use a hashmap for all-maskedemails
    (dolist (m all-maskedemails)
      (when (s-equals? (maskedemail-email m) chosen-email)
        (setf (maskedemail-state m) "enabled")))
    chosen-email))

(defun maskedemail-disable ()
  "Disable MaskedEmail"
  (interactive)
  (unless all-maskedemails (maskedemail-list))
  (let* ((disabled-maskedemails (-filter (lambda (m)
                                           (member (maskedemail-state m)
                                                   '("enabled")))
                                         all-maskedemails))
         (emails  (-map (lambda (m)
                          `(,(maskedemail-email m) . ,m))
                        disabled-maskedemails))
         (chosen (alist-get
                  (completing-read "Email: " emails)
                  emails nil nil 'equal))
         (chosen-email       (maskedemail-email chosen))
         (chosen-domain       (maskedemail-domain chosen))
         (chosen-state       (maskedemail-state chosen)))
    (shell-command (format "%s -e disable %s"
                           maskedemail-cli-path
                           chosen-email))
    ;; NOTE: can be optimized if use a hashmap for all-maskedemails
    (dolist (m all-maskedemails)
      (when (s-equals? (maskedemail-email m) chosen-email)
        (setf (maskedemail-state m) "disabled")))
    chosen-email))

(provide 'maskedemail)
