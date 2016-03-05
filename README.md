# company-ngram

A company backend for N-gram based completion.

```elisp
; ~/.emacs.d/init.el

(with-eval-after-load 'company-ngram
  ; ~/data/ngram/*.txt are used as data
  (setq company-ngram-data-dir "~/data/ngram")
  ; company-ngram does not support python2
  (setq company-ngram-python "/path/to/python3")
  (company-ngram-init)
  )
(require 'company-ngram nil t)
```
