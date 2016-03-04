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
(defcustom company-ngram-n-out-max 1000
  "Maximum number of candidates"
  :type 'integer
  :group 'company-ngram
  :require 'company
  )
(defcustom company-ngram-data-dir "~/data/ngram"
  "`company-ngram-data-dir/*.txt' are loaded"
  :type 'string
  :group 'company-ngram
  :require 'company
  )


(defvar company-ngram-candidates nil)
(defvar company-ngram-prev-words nil)


(defun company-ngram-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ngram-backend))
    (prefix (cons
             (let* ((p2 (point))
                    (p1 (max (- p2 200) 1))
                    (s (buffer-substring p1 p2))
                    )
               (let ((l (split-string s)))
                 (if (string-suffix-p " " s)
                     (let ((ret " "))
                       (put-text-property 0 1 :words (last l (1- company-ngram-n)) ret)
                       ret)
                   (let ((ret (car (last l))))
                     (put-text-property 0 1 :words (last (butlast l) (1- company-ngram-n)) ret)
                     ret))))
             t))
    (candidates (let* ((words (get-text-property 0 :words arg)))
                  (all-completions arg
                                   (if (equal words company-ngram-prev-words)
                                       company-ngram-candidates
                                     (progn
                                       (setq company-ngram-candidates
                                             (mapcar (lambda (c) (let ((s (car c)))
                                                                   (put-text-property 0 1 :ann (format "%d %d" (cadr c) (caddr c)) s)
                                                                   s))
                                                     (company-ngram-query words)))
                                       (setq company-ngram-prev-words words)
                                       (mapcar (lambda (w) (let ((sp " "))
                                                             (put-text-property 0 1 :ann (get-text-property 0 :ann w) sp)
                                                             (concat sp w)))
                                               company-ngram-candidates))))))
    (annotation (format " %s" (get-text-property 0 :ann arg)))
    (sorted t)
    )
  )


(defvar company-ngram-process nil)
(defun company-ngram-init ()
  (company-ngram--init company-ngram-python
                       company-ngram-ngram-py
                       company-ngram-n
                       company-ngram-data-dir)
  )
(defun company-ngram--init (python ngram-py n dir)
  (condition-case nil
      (kill-process company-ngram-process)
    (error nil))
  (setq company-ngram-process
        (company-ngram---init python
                              ngram-py
                              n
                              dir)))
(defun company-ngram---init (python ngram-py n dir)
  (let ((process-connection-type nil)
        (process-adaptive-read-buffering t))
    (start-process "company-ngram"
                   (generate-new-buffer-name "*company-ngram*")
                   python
                   ngram-py
                   (format "%d" n)
                   (expand-file-name dir)
                   )))


(defun company-ngram-query (words)
  (company-ngram--query company-ngram-process company-ngram-n-out-max words))
(defun company-ngram--query (process n-out-max words)
  (save-excursion
    (set-buffer (process-buffer process))
    (erase-buffer)
    (process-send-string process
                         (concat (format "%d\t" n-out-max)
                                 (mapconcat 'identity words "\t")
                                 "\n"))
    (let ((bufsizepre 0)
          (bufsize 0))
      (accept-process-output process)
      (while (or (= bufsize 0)
                 (/= bufsize bufsizepre))
        (sleep-for 0.01) ; 0.001 s seems to be too short to update buffer content
        (setq bufsizepre bufsize)
        (setq bufsize (buffer-size))))
    (goto-char (point-min))
    (let ((json-array-type 'list))
      (json-read))))


(provide 'company-ngram)

;;; company-ngram.el ends here
