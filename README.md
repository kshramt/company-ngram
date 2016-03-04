# company-ngram

A company backend for N-gram based completion.

```elisp
; ~/.emacs.d/init.el

(with-eval-after-load 'company-ngram
  ; ~/data/ngram/*.txt is used as data source
  (setq company-ngram-data-dir "~/data/ngram")
  (company-ngram-init)
  )
(require 'company-ngram nil t)
```
