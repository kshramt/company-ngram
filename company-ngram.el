;;; company-ngram.el --- N-gram based completion
;;
;; Author: kshramt
;; Version: 0.0.1
;; Package-Requires: ((company "0.8.0"))
;; License: GPL version 3
;;
;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'json)

(defgroup company-ngram nil
  "N-gram based completion"
  :group 'company-ngram
  :prefix "company-ngram-")


;;; BACKENDS


(defconst company-ngram-dir
  (file-name-directory load-file-name))
(defconst company-ngram-ngram-py
  (concat (file-name-as-directory company-ngram-dir)
          "ngram.py"))


(defcustom company-ngram-python "python"
  "Python executable"
  :type 'string
  :group 'company-ngram
  :require 'company
)
(defcustom company-ngram-n 5
  "Maximum N of N-gram"
  :type 'integer
  :group 'company-ngram
  :require 'company
  )
(defcustom company-ngram-data-dir "/path/to/data_dir"
  "`company-ngram-data-dir/*.txt' are loaded"
  :type 'string
  :group 'company-ngram
  :require 'company
  )


(defvar company-ngram-process nil)
(defun company-ngram-init ()
  (company-ngram--init company-ngram-python
                       company-ngram-ngram-py
                       company-ngram-n
                       company-ngram-data-dir)
  )
(defun company-ngram--init (python ngram-py n dir)
  (if company-ngram-process
      (progn (kill-process company-ngram-process)
             (company-ngram-process python ngram-py n dir)
             (setq company-ngram-process nil))
    (setq company-ngram-process
          (company-ngram---init python
                                ngram-py
                                n
                                dir))))
(defun company-ngram---init (python ngram-py n dir)
  (let ((process-connection-type nil))
    (start-process "company-ngram"
                   (generate-new-buffer-name "*company-ngram*")
                   python
                   ngram-py
                   (format "%d" n)
                   (expand-file-name dir)
                   )))


(defun company-ngram-query (n-out-max words)
  (company-ngram--query company-ngram-process n-out-max words))
(defun company-ngram--query (process n-out-max words)
  (save-excursion
    (set-buffer (process-buffer process))
    (erase-buffer)
    (process-send-string process
                         (concat (format "%d\t" n-out-max)
                                 (mapconcat 'identity words "\t")
                                 "\n"))
    (accept-process-output process)
    (goto-char (point-min))
    (let ((json-array-type 'list))
      (json-read))))


(provide 'company-ngram)

;;; company-ngram.el ends here
