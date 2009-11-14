;;
;;   Copyright (c) 2009 Takeshi Abe. All rights reserved.
;;
;;   Redistribution and use in source and binary forms, with or without
;;   modification, are permitted provided that the following conditions
;;   are met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;
;;    3. Neither the name of the authors nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;
;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(library (smathml content)
  (export smathml
          free-variables)
  (import (rnrs (6))
          (only (srfi :1) lset-difference lset-union))

  (define-syntax :
    (syntax-rules ()
      ((: prefix e)
       (string->symbol
        (string-append
         (symbol->string 'prefix)
         ":"
         (symbol->string 'e))))))

  (define-syntax smathml:apply
    (syntax-rules ()
      ((_ (prefix) f arg ...)
       `(,(: prefix apply)
         (,(: prefix f))
         ,(smathml arg prefix)
         ...))))

  (define-syntax smathml
    (syntax-rules (bvar
                   diff
                   divide
                   eq
                   exponentiale
                   geq
                   gt
                   leq
                   ln
                   lt
                   max
                   min
                   minus
                   plus
                   power
                   rem
                   times)
      ((_ (eq left right) prefix)
       (smathml:apply (prefix) eq left right))
      ((_ (lt left right) prefix)
       (smathml:apply (prefix) lt left right))
      ((_ (leq left right) prefix)
       (smathml:apply (prefix) leq left right))
      ((_ (gt left right) prefix)
       (smathml:apply (prefix) gt left right))
      ((_ (geq left right) prefix)
       (smathml:apply (prefix) geq left right))
      ((_ (divide left right) prefix)
       (smathml:apply (prefix) divide left right))
      ((_ (ln arg) prefix)
       (smathml:apply (prefix) ln arg))
      ((_ (max left right) prefix)
       (smathml:apply (prefix) max left right))
      ((_ (min left right) prefix)
       (smathml:apply (prefix) min left right))
      ((_ (minus left right) prefix)
       (smathml:apply (prefix) minus left right))
      ((_ (minus e0 e1 e2 ...) prefix)
       (smathml (minus (minus e0 e1) e2 ...) prefix))
      ((_ (plus left right) prefix)
       (smathml:apply (prefix) plus left right))
      ((_ (plus e0 e1 e2 ...) prefix)
       (smathml (plus (plus e0 e1) e2 ...) prefix))
      ((_ (power left right) prefix)
       (smathml:apply (prefix) power left right))
      ((_ (rem left right) prefix)
       (smathml:apply (prefix) rem left right))
      ((_ (times left right) prefix)
       (smathml:apply (prefix) times left right))
      ((_ (times e0 e1 e2 ...) prefix)
       (smathml (times (times e0 e1) e2 ...) prefix))
      ((_ (diff (bvar v) rest ...) prefix)
       `(,(: prefix apply)
         (,(: prefix diff))
         (,(: prefix bvar)
          (,(: prefix ci) ,(symbol->string 'v)))
         ,(smathml rest prefix)
         ...))
      ((_ (diff rest ...) prefix)
       (smathml:apply (prefix) diff rest ...))
      ;; just apply
      ((_ (f arg ...) prefix)
       `(,(: prefix apply)
         (,(: prefix ci) ,(symbol->string 'f))
         ,(smathml arg prefix)
         ...))
      ;; Constant and Symbol Elements
      ((_ exponentiale prefix)
       `(,(: prefix exponentiale)))
      ;; cn & ci
      ((_ x prefix)
       (if (number? 'x)
           `(,(: prefix cn) ,(number->string 'x))
           `(,(: prefix ci) ,(symbol->string 'x))))))

  (define-syntax free-variables
    (syntax-rules (bvar
                   diff
                   divide
                   eq
                   exponentiale
                   geq
                   gt
                   leq
                   ln
                   lt
                   max
                   min
                   minus
                   plus
                   power
                   rem
                   times)
      ((_ (eq left right))
       (lset-union eq?
                   (free-variables left)
                   (free-variables right)))
      ((_ (lt left right))
       (lset-union eq?
                   (free-variables left)
                   (free-variables right)))
      ((_ (leq left right))
       (lset-union eq?
                   (free-variables left)
                   (free-variables right)))
      ((_ (gt left right))
       (lset-union eq?
                   (free-variables left)
                   (free-variables right)))
      ((_ (geq left right))
       (lset-union eq?
                   (free-variables left)
                   (free-variables right)))
      ((_ (divide left right))
       (lset-union eq?
                   (free-variables left)
                   (free-variables right)))
      ((_ (ln arg))
       (free-variables arg))
      ((_ (max left right))
       (lset-union eq?
                   (free-variables left)
                   (free-variables right)))
      ((_ (min left right))
       (lset-union eq?
                   (free-variables left)
                   (free-variables right)))
      ((_ (minus e0 e1 ...))
       (lset-union eq?
                   (free-variables e0)
                   (free-variables e1)
                   ...))
      ((_ (plus e0 e1 ...))
       (lset-union eq?
                   (free-variables e0)
                   (free-variables e1)
                   ...))
      ((_ (power left right))
       (lset-union eq?
                   (free-variables left)
                   (free-variables right)))
      ((_ (rem left right))
       (lset-union eq?
                   (free-variables left)
                   (free-variables right)))
      ((_ (times e0 e1 ...))
       (lset-union eq?
                   (free-variables e0)
                   (free-variables e1)
                   ...))
      ((_ (diff (bvar v) rest ...))
       (lset-difference eq?
                        (lset-union eq?
                                    (free-variables rest)
                                    ...)
                        '(v)))
      ((_ (diff rest ...))
       (lset-union eq? (free-variables rest) ...))
      ;; just apply
      ((_ (f arg ...))
       (lset-union eq?
                   (free-variables f)
                   (free-variables arg)
                   ...))
      ;; Constant and Symbol Elements
      ((_ exponentiale)
       '())
      ;; 
      ((_ x)
       (if (number? 'x)
           '()
           '(x)))))

)
