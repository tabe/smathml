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

(library (smathml)
  (export define-smathml
          element-catalog
          make-smathml
          smathml
          smathml?
          smathml->string
          write-smathml)
  (import (rnrs (6)))

  (define-record-type smathml
    (fields tree prefix))

  (define-syntax define-smathml
    (syntax-rules ()
      ((_ name tree)
       (define name (make-smathml tree #f)))
      ((_ name tree prefix)
       (define name (make-smathml tree prefix)))))

  (define *element-name* (make-eq-hashtable))

  (define-syntax element-catalog
    (syntax-rules ()
      ((_)
       #t)
      ((_ (s name) rest ...)
       (begin
         (hashtable-set! *element-name* 's 'name)
         (element-catalog rest ...)))
      ((_ (s) rest ...)
       (element-catalog (s s) rest ...))))

  (define (symbol->element s prefix)
    (assert (symbol? s))
    (cond ((hashtable-ref *element-name* s #f)
           => (lambda (name)
                (if prefix
                    `(#\< ,prefix #\: ,name "/>")
                    `(#\< ,name "/>"))))
          (else (raise s))))

  (define-syntax open
    (syntax-rules ()
      ((_ name prefix)
       (if prefix
           `(#\< ,prefix #\: name #\>)
           `(#\< name #\>)))))

  (define-syntax close
    (syntax-rules ()
      ((_ name prefix)
       (if prefix
           `("</" ,prefix #\: name #\>)
           `("</" name #\>)))))

  (define (tree->mathml tree prefix)
      (cond ((list? tree)
             (if (null? tree)
                 '()
                 `(,(open apply prefix)
                   ,(symbol->element (car tree) prefix)
                   ,@(map
                      (lambda (x) (tree->mathml x prefix))
                      (cdr tree))
                   ,(close apply prefix))))
            ((symbol? tree)
             (list (open ci prefix)
                   tree
                   (close ci prefix)))
            ((number? tree)
             (list (open cn prefix)
                   tree
                   (close cn prefix)))
            ((string? tree)
             tree)
            (else (raise tree))))

  (define (write-mathml m port)
    (for-each
     (lambda (x)
       (if (list? x)
           (write-mathml x port)
           (display x port)))
     m))

  (define write-smathml
    (case-lambda
     ((s port)
      (assert (smathml? s))
      (assert (output-port? port))
      (assert (textual-port? port))
      (write-mathml (tree->mathml (smathml-tree s) (smathml-prefix s)) port))
     ((s)
      (write-smathml s (current-output-port)))))

  (define (smathml->string s)
    (call-with-string-output-port
     (lambda (port)
       (write-smathml s port))))

  (element-catalog
   (= eq)
   (/ divide)
   (max)
   (min)
   (- minus)
   (+ plus)
   (expt power)
   (reminder rem)
   (* times)
   )

)
