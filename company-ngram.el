;;; company-ngram.el --- N-gram based completion
;;
;; Author: kshramt
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5") (company "0.8.0"))

;; This program is distributed under the terms of
;; the GNU General Public License version 3
;; (see <http://www.gnu.org/licenses/>).
;;
;;; Commentary:
;;
;; ; https://github.com/kshramt/company-ngram
;; ; ~/.emacs.d/init.el
;;
;; (with-eval-after-load 'company-ngram
;;   ; ~/data/ngram/*.txt are used as data
;;   (setq company-ngram-data-dir "~/data/ngram")
;;   ; company-ngram does not support python2
;;   (setq company-ngram-python "/path/to/python3")
;;   (company-ngram-init)
;;   )
;; (require 'company-ngram nil t)
;;
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
  )
(defcustom company-ngram-n 5
  "Maximum N of N-gram"
  :type 'integer
  :group 'company-ngram
  )
(defcustom company-ngram-n-out-max 10000
  "Maximum number of candidates"
  :type 'integer
  :group 'company-ngram
  )
(defcustom company-ngram-data-dir "~/data/ngram"
  "`company-ngram-data-dir/*.txt' are loaded"
  :type 'string
  :group 'company-ngram
  )


(defvar company-ngram-candidates nil)
(defvar company-ngram-prev-words nil)


;;;###autoload
(defun company-ngram-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ngram-backend))
    (prefix (let* ((p2 (point))
                   (p1 (max (- p2 200) 1))
                   (s (buffer-substring p1 p2))
                   (l (split-string s))
                   (is-suffix-space (string-suffix-p " " s))
                   (words (if is-suffix-space
                              (last l (1- company-ngram-n))
                            (last (butlast l) (1- company-ngram-n))))
                   (pre (if is-suffix-space
                            " "
                          (car (last l))))
                   (candidates (all-completions
                                pre
                                (if (equal words company-ngram-prev-words)
                                    company-ngram-candidates
                                  (progn
                                    (setq company-ngram-candidates
                                          (mapcar (lambda (c) (let ((s (car c)))
                                                                (put-text-property 0 1 :ann (cadr c) s)
                                                                s))
                                                  (company-ngram-query words)))
                                    (setq company-ngram-prev-words words)
                                    (mapcar (lambda (w) (let ((sp " "))
                                                          (put-text-property 0 1 :ann (get-text-property 0 :ann w) sp)
                                                          (concat sp w)))
                                            company-ngram-candidates)
                                    ))))
                   )
              (when candidates
                (put-text-property 0 1 :candidates candidates pre)
                (cons pre t))))
    (candidates (get-text-property 0 :candidates arg))
    (annotation (concat " " (get-text-property 0 :ann arg)))
    (sorted t)
    (no-cache t)
    )
  )


(defvar company-ngram-process nil)


;;;###autoload
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
  (condition-case nil
      (with-current-buffer (process-buffer company-ngram-process)
        (erase-buffer))
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
  (with-current-buffer (process-buffer process)
    (erase-buffer)
    (with-local-quit
      (process-send-string process
                           (concat (format "%d\t" n-out-max)
                                   (mapconcat 'identity words "\t")
                                   "\n"))
      (let ((bufsizepre 0)
            (bufsize 0))
        (accept-process-output process)
        (while (or (= bufsize 0)
                   (/= bufsize bufsizepre))
          (sleep-for 0.006) ; 0.001 s seems to be too short to update buffer content
          (setq bufsizepre bufsize)
          (setq bufsize (buffer-size))))
      )
    (goto-char (point-min))
    (let ((json-array-type 'list))
      (json-read))))


(provide 'company-ngram)

;;; company-ngram.el ends here
