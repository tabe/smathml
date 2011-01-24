;;
;;   Copyright (c) 2009,2010 Takeshi Abe. All rights reserved.
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
  (import (rnrs (6)))

  (define-syntax :
    (syntax-rules ()
      ((: prefix e)
       (if (boolean? 'prefix)
           'e
           (string->symbol
            (string-append
             (symbol->string 'prefix)
             ":"
             (symbol->string 'e)))))))

  (define-syntax smathml:apply
    (syntax-rules ()
      ((_ (prefix) f x ...)
       `(,(: prefix apply)
         (,(: prefix f))
         ,(smathml x prefix)
         ...))))

  (define-syntax smathml:piece
    (syntax-rules ()
      ((_ (prefix) (x y))
       `(,(: prefix piece)
         ,(smathml x prefix)
         ,(smathml y prefix)))
      ((_ (prefix) (x))
       `(,(: prefix otherwise)
         ,(smathml x prefix)))))

  (define-syntax smathml:matrixrow
    (syntax-rules ()
      ((_ (prefix) x ...)
       `(,(: prefix matrixrow)
         ,(smathml x prefix)
         ...))))

  ;; to be completed implicitly:
  ;; - apply
  ;; - sep
  (define-syntax smathml
    (syntax-rules (:csymbol
                   :interval
                   :inverse
                   :condition
                   :declare
                   :lambda
                   :compose
                   :ident
                   :domain
                   :codomain
                   :image
                   :domainofapplication
                   :piecewise
                   :quotient
                   :factorial
                   :divide
                   :max
                   :min
                   :minus
                   :plus
                   :power
                   :rem
                   :times
                   :root
                   :gcd
                   :and
                   :or
                   :xor
                   :not
                   :implies
                   :forall
                   :exists
                   :abs
                   :conjugate
                   :arg
                   :real
                   :imaginary
                   :lcm
                   :floor
                   :ceiling
                   :eq
                   :neq
                   :gt
                   :lt
                   :geq
                   :leq
                   :equivalent
                   :approx
                   :factorof
                   :int
                   :diff
                   :partialdiff
                   :lowlimit
                   :uplimit
                   :bvar
                   :degree
                   :divergence
                   :grad
                   :curl
                   :laplacian
                   :set
                   :list
                   :union
                   :intersect
                   :in
                   :notin
                   :subset
                   :prsubset
                   :notsubset
                   :notprsubset
                   :setdiff
                   :card
                   :cartesianproduct
                   :sum
                   :product
                   :limit
                   :tendsto
                   :exp
                   :ln
                   :log
                   :sin
                   :cos
                   :tan
                   :sec
                   :csc
                   :cot
                   :sinh
                   :cosh
                   :tanh
                   :sech
                   :csch
                   :coth
                   :arcsin
                   :arccos
                   :arctan
                   :arccosh
                   :arccot
                   :arccoth
                   :arccsc
                   :arccsch
                   :arcsec
                   :arcsech
                   :arcsinh
                   :arctanh
                   :mean
                   :sdev
                   :variance
                   :median
                   :mode
                   :moment
                   :momentabout
                   :vector
                   :matrix
                   :matrixrow
                   :determinant
                   :transpose
                   :selector
                   :vectorproduct
                   :scalarproduct
                   :outerproduct
                   :annotation
                   :semantics
                   :annotation-xml
                   :integers
                   :reals
                   :rationals
                   :naturalnumbers
                   :complexes
                   :primes
                   :exponentiale
                   :imaginaryi
                   :notanumber
                   :true
                   :false
                   :emptyset
                   :pi
                   :eulergamma
                   :infinity)
      ;;; 4.4 The Content Markup Elements
      ((_ (:csymbol name) prefix)
       `(,(: prefix csymbol) ,(symbol->string 'name)))
      ;; basic content elements
      ((_ (:interval left right) prefix) ; two child elements that evaluate to real numbers
       (smathml:apply (prefix) interval left right))
      ((_ (:inverse f) prefix)
       (smathml:apply (prefix) inverse f))
      ((_ (:condition x) prefix)
       `(,(: prefix condition)
         ,(smathml x prefix)))
      ((_ (:declare e0 e1 ...) prefix)
       `(,(: prefix declare)
         ,(smathml e0 prefix)
         ,(smathml e1 prefix)
         ...))
      ((_ (:lambda (x ...) expr) prefix)
       `(,(: prefix lambda)
         (,(: prefix bvar)
          (,(: prefix ci) ,(symbol->string 'x))
          ...)
         ,(smathml expr prefix)))
      ((_ (:compose f0 ...) prefix)
       (smathml:apply (prefix) compose f0 ...))
      ((_ :ident prefix)
       `(,(: prefix ident)))
      ((_ (:domain f) prefix)
       (smathml:apply (prefix) domain f))
      ((_ (:codomain f) prefix)
       (smathml:apply (prefix) codomain f))
      ((_ (:image f) prefix)
       (smathml:apply (prefix) image f))
      ((_ (:domainofapplication x) prefix)
       `(,(: prefix domainofapplication)
         ,(smathml x prefix)))
      ((_ (:piecewise c0 c1 ...) prefix)
       `(,(: prefix piecewise)
         ,(smathml:piece (prefix) c0)
         ,(smathml:piece (prefix) c1)
         ...))
      ;; arithmetic, algebra and logic
      ((_ (:quotient left right) prefix)
       (smathml:apply (prefix) quotient left right))
      ((_ (:factorial n) prefix)
       (smathml:apply (prefix) factorial n))
      ((_ (:divide left right) prefix)
       (smathml:apply (prefix) divide left right))
      ((_ (:max x0 x1 ...) prefix)
       (smathml:apply (prefix) max x0 x1 ...))
      ((_ (:min x0 x1 ...) prefix)
       (smathml:apply (prefix) min x0 x1 ...))
      ((_ (:minus x) prefix)
       (smathml:apply (prefix) minus x))
      ((_ (:minus left right) prefix)
       (smathml:apply (prefix) minus left right))
      ((_ (:minus e0 e1 e2 ...) prefix)
       (smathml (:minus (:minus e0 e1) e2 ...) prefix))
      ((_ (:plus left right) prefix)
       (smathml:apply (prefix) plus left right))
      ((_ (:plus e0 e1 e2 ...) prefix)
       (smathml (:plus (:plus e0 e1) e2 ...) prefix))
      ((_ (:power left right) prefix)
       (smathml:apply (prefix) power left right))
      ((_ (:rem left right) prefix)
       (smathml:apply (prefix) rem left right))
      ((_ (:times left right) prefix)
       (smathml:apply (prefix) times left right))
      ((_ (:times e0 e1 e2 ...) prefix)
       (smathml (:times (:times e0 e1) e2 ...) prefix))
      ((_ (:root x) prefix)
       (smathml:apply (prefix) root x))
      ((_ (:root d x) prefix)
       `(,(: prefix apply)
         ,(: prefix root)
         (,(: prefix degree)
          ,(smathml d prefix))
         ,(smathml x prefix)))
      ((_ (:gcd e0 e1 ...) prefix)
       (smathml:apply (prefix) gcd e0 e1 ...))
      ((_ (:and x y ...) prefix)
       (smathml:apply (prefix) and x y ...))
      ((_ (:or x y ...) prefix)
       (smathml:apply (prefix) or x y ...))
      ((_ (:xor x y ...) prefix)
       (smathml:apply (prefix) xor x y ...))
      ((_ (:not x) prefix)
       (smathml:apply (prefix) not x))
      ((_ (:implies x y) prefix)
       (smathml:apply (prefix) implies x y))
      ((_ (:forall (x y ...) z ...) prefix)
       `(,(: prefix apply)
         ,(: prefix forall)
         (,(: prefix bvar)
          (,(: prefix ci) ,(symbol->string 'x)))
         (,(: prefix bvar)
          (,(: prefix ci) ,(symbol->string 'y)))
         ...
         (smathml z prefix)
         ...))
      ((_ (:exists (x y ...) z ...) prefix)
       `(,(: prefix apply)
         ,(: prefix exists)
         (,(: prefix bvar)
          (,(: prefix ci) ,(symbol->string 'x)))
         (,(: prefix bvar)
          (,(: prefix ci) ,(symbol->string 'y)))
         ...
         (smathml z prefix)
         ...))
      ((_ (:abs x) prefix)
       (smathml:apply (prefix) abs x))
      ((_ (:conjugate x) prefix)
       (smathml:apply (prefix) conjugate x))
      ((_ (:arg x) prefix)
       (smathml:apply (prefix) arg x))
      ((_ (:real x) prefix)
       (smathml:apply (prefix) real x))
      ((_ (:imaginary x) prefix)
       (smathml:apply (prefix) imaginary x))
      ((_ (:lcm x y z ...) prefix)
       (smathml:apply (prefix) lcm x y z ...))
      ((_ (:floor x) prefix)
       (smathml:apply (prefix) floor x))
      ((_ (:ceiling x) prefix)
       (smathml:apply (prefix) ceiling x))
      ;; relations
      ((_ (:eq x y ...) prefix)
       (smathml:apply (prefix) eq x y ...))
      ((_ (:neq x y) prefix)
       (smathml:apply (prefix) neq x y))
      ((_ (:gt x y ...) prefix)
       (smathml:apply (prefix) gt x y ...))
      ((_ (:lt x y ...) prefix)
       (smathml:apply (prefix) lt x y ...))
      ((_ (:geq x y ...) prefix)
       (smathml:apply (prefix) geq x y ...))
      ((_ (:leq x y ...) prefix)
       (smathml:apply (prefix) leq x y ...))
      ((_ (:equivalent x y ...) prefix)
       (smathml:apply (prefix) equivalent x y ...))
      ((_ (:approx x y) prefix)
       (smathml:apply (prefix) approx x y))
      ((_ (:factorof x y) prefix)
       (smathml:apply (prefix) factorof x y))
      ;; calculus and vector calculus 
      ((_ (:int (v low up) x) prefix)
       `(,(: prefix apply)
         (,(: prefix int))
         (,(: prefix bvar)
          (,(: prefix ci) ,(symbol->string 'v)))
         (,(: prefix lowlimit) ,(smathml low prefix))
         (,(: prefix uplimit) ,(smathml up prefix))
         ,(smathml prefix x)))
      ((_ (:int (:bvar x) y ...) prefix)
       `(,(: prefix apply)
         (,(: prefix int))
         (,(: prefix bvar)
          ((: prefix ci) ,(symbol->string 'x)))
         ,(smathml y prefix)
         ...))
      ((_ (:int (low up) x) prefix)
       `(,(: prefix apply)
         ,(: prefix int)
         (,(: prefix interval)
          ,(smathml low prefix)
          ,(smathml up prefix))
         ,(smathml x prefix)))
      ((_ (:int x) prefix)
       (smathml:apply (prefix) int x))
      ((_ (:diff (:bvar v) x) prefix)
       `(,(: prefix apply)
         (,(: prefix diff))
         (,(: prefix bvar)
          (,(: prefix ci) ,(symbol->string 'v)))
         ,(smathml x prefix)))
      ((_ (:diff x) prefix)
       (smathml:apply (prefix) diff x))
      ((_ (:partialdiff x y) prefix)
       (smathml:apply (prefix) partialdiff x y)) ; FIXME
      ((_ (:divergence x) prefix)
       (smathml:apply (prefix) divergence x))
      ((_ (:grad x) prefix)
       (smathml:apply (prefix) grad x))
      ((_ (:curl x) prefix)
       (smathml:apply (prefix) curl x))
      ((_ (:laplacian x) prefix)
       (smathml:apply (prefix) laplacian x))
      ;; theory of sets
      ((_ (:set (:bvar v) c x) prefix)
       `(,(: prefix set)
         (,(: prefix bvar) ,(symbol->string 'v))
         ,(smathml c prefix)
         ,(smathml x prefix)))
      ((_ (:set x ...) prefix)
       `(,(: prefix set)
         ,(smathml x prefix)
         ...))
      ((_ (:list (:bvar v) c x) prefix)
       `(,(: prefix list)
         (,(: prefix bvar) ,(symbol->string 'v))
         ,(smathml c prefix)
         ,(smathml x prefix)))
      ((_ (:list x ...) prefix)
       `(,(: prefix list)
         ,(smathml x prefix)
         ...))
      ((_ (:union (:bvar s) c x) prefix)
       `(,(: prefix apply)
         (,(: prefix union))
         (,(: prefix bvar) ,(symbol->string 's))
         ,(smathml c prefix)
         ,(smathml x prefix)))
      ((_ (:union x y ...) prefix)
       (smathml:apply (prefix) union x y ...))
      ((_ (:intersect (:bvar s) c x) prefix)
       `(,(: prefix apply)
         (,(: prefix intersect))
         (,(: prefix bvar) ,(symbol->string 's))
         ,(smathml c prefix)
         ,(smathml x prefix)))
      ((_ (:intersect x y ...) prefix)
       (smathml:apply (prefix) intersect x y ...))
      ((_ (:in x y) prefix)
       (smathml:apply (prefix) in x y))
      ((_ (:notin x y) prefix)
       (smathml:apply (prefix) notin x y))
      ((_ (:subset x y z ...) prefix)
       (smathml:apply (prefix) subset x y z ...))
      ((_ (:prsubset x y z ...) prefix)
       (smathml:apply (prefix) prsubset x y z ...))
      ((_ (:notsubset x y) prefix)
       (smathml:apply (prefix) notsubset x y))
      ((_ (:notprsubset x y) prefix)
       (smathml:apply (prefix) notprsubset x y))
      ((_ (:setdiff x y) prefix)
       (smathml:apply (prefix) setdiff x y))
      ((_ (:card x) prefix)
       (smathml:apply (prefix) card x))
      ((_ (:cartesianproduct x y ...) prefix)
       (smathml:apply (prefix) cartesianproduct x y ...))
      ;; sequence and series
      ((_ (:sum (v lo hi) x) prefix)
       `(,(: prefix sum)
         (,(: prefix bvar) ,(symbol->string 'v))
         (,(: prefix lowlimit)
          ,(smathml lo prefix))
         (,(: prefix uplimit)
          ,(smathml hi prefix))
         ,(smathml x prefix)))
      ((_ (:sum (v c) x) prefix)
       `(,(: prefix sum)
         (,(: prefix bvar) ,(symbol->string 'v))
         (,(: prefix condition)
          ,(smathml c prefix))
         ,(smathml x prefix)))
      ((_ (:sum d x) prefix)
       `(,(: prefix sum)
         (,(: prefix domainofapplication)
          ,(smathml d prefix))
         ,(smathml x prefix)))
      ((_ (:product (v lo hi) x) prefix)
       `(,(: prefix product)
         (,(: prefix bvar) ,(symbol->string 'v))
         (,(: prefix lowlimit)
          ,(smathml lo prefix))
         (,(: prefix uplimit)
          ,(smathml hi prefix))
         ,(smathml x prefix)))
      ((_ (:product (v c) x) prefix)
       `(,(: prefix product)
         (,(: prefix bvar) ,(symbol->string 'v))
         (,(: prefix condition)
          ,(smathml c prefix))
         ,(smathml x prefix)))
      ((_ (:product d x) prefix)
       `(,(: prefix product)
         (,(: prefix domainofapplication)
          ,(smathml d prefix))
         ,(smathml x prefix)))
      ((_ (:limit (v x) y) prefix)
       `(,(: prefix limit)
         (,(: prefix bvar) ,(symbol->string 'v))
         (,(: prefix lowlimit) ,(smathml x prefix))
         ,(smathml y prefix)))
      ((_ (:limit v x y) prefix)
       `(,(: prefix limit)
         (,(: prefix bvar) ,(symbol->string 'v))
         ,(smathml x prefix)
         ,(smathml y prefix)))
      ((_ (:tendsto x y) prefix)
       (smathml:apply (prefix) tendsto x y))
      ;; elementary classical functions
      ((_ (:exp x) prefix)
       (smathml:apply (prefix) exp x))
      ((_ (:ln x) prefix)
       (smathml:apply (prefix) ln x))
      ((_ (:log x) prefix)
       (smathml:apply (prefix) log x))
      ((_ (:log b x) prefix)
       `(,(: prefix apply)
         (,(: prefix log))
         (,(: prefix logbase) ,(smathml b prefix))
         ,(smathml x prefix)))
      ((_ (:sin x) prefix)     (smathml:apply (prefix) sin x))
      ((_ (:cos x) prefix)     (smathml:apply (prefix) cos x))
      ((_ (:tan x) prefix)     (smathml:apply (prefix) tan x))
      ((_ (:sec x) prefix)     (smathml:apply (prefix) sec x))
      ((_ (:csc x) prefix)     (smathml:apply (prefix) csc x))
      ((_ (:cot x) prefix)     (smathml:apply (prefix) cot x))
      ((_ (:sinh x) prefix)    (smathml:apply (prefix) sinh x))
      ((_ (:cosh x) prefix)    (smathml:apply (prefix) cosh x))
      ((_ (:tanh x) prefix)    (smathml:apply (prefix) tanh x))
      ((_ (:sech x) prefix)    (smathml:apply (prefix) sech x))
      ((_ (:csch x) prefix)    (smathml:apply (prefix) csch x))
      ((_ (:coth x) prefix)    (smathml:apply (prefix) coth x))
      ((_ (:arcsin x) prefix)  (smathml:apply (prefix) arcsin x))
      ((_ (:arccos x) prefix)  (smathml:apply (prefix) arccos x))
      ((_ (:arctan x) prefix)  (smathml:apply (prefix) arctan x))
      ((_ (:arccosh x) prefix) (smathml:apply (prefix) arccosh x))
      ((_ (:arccot x) prefix)  (smathml:apply (prefix) arccot x))
      ((_ (:arccoth x) prefix) (smathml:apply (prefix) arccoth x))
      ((_ (:arccsc x) prefix)  (smathml:apply (prefix) arccsc x))
      ((_ (:arccsch x) prefix) (smathml:apply (prefix) arccsch x))
      ((_ (:arcsec x) prefix)  (smathml:apply (prefix) arcsec x))
      ((_ (:arcsech x) prefix) (smathml:apply (prefix) arcsech x))
      ((_ (:arcsinh x) prefix) (smathml:apply (prefix) arcsinh x))
      ((_ (:arctanh x) prefix) (smathml:apply (prefix) arctanh x))
      ;; statistics
      ((_ (:mean x y ...) prefix)
       (smathml:apply (prefix) mean x y ...))
      ((_ (:sdev x y ...) prefix)
       (smathml:apply (prefix) sdev x y ...))
      ((_ (:variance x y ...) prefix)
       (smathml:apply (prefix) variance x y ...))
      ((_ (:median x y ...) prefix)
       (smathml:apply (prefix) median x y ...))
      ((_ (:mode x y ...) prefix)
       (smathml:apply (prefix) mode x y ...))
      ((_ (:moment d a x) prefix)
       `(,(: prefix apply)
         (,(: prefix moment))
         (,(: prefix degree) ,(smathml d prefix))
         (,(: prefix momentabout) ,(smathml a prefix))
         ,(smathml x prefix)))
      ;; linear algebra
      ((_ (:vector x ...) prefix)
       `(,(: prefix vector)
         ,(smathml x prefix)
         ...))
      ((_ (:matrix (x ...) ...) prefix)
       `(,(: prefix matrix)
         ,(smathml:matrixrow (prefix) x ...)
         ...))
      ((_ (:determinant x) prefix)
       (smathml:apply (prefix) determinant x))
      ((_ (:transpose x) prefix)
       (smathml:apply (prefix) transpose x))
      ((_ (:selector x i0 i1 ...) prefix)
       (smathml:apply (prefix) selector x i0 i1 ...))
      ((_ (:vectorproduct x y) prefix)
       (smathml:apply (prefix) vectorproduct x y))
      ((_ (:scalarproduct x y) prefix)
       (smathml:apply (prefix) scalarproduct x y))
      ((_ (:outerproduct x y) prefix)
       (smathml:apply (prefix) outerproduct x y))
      ;; semantic mapping elements
      ((_ (:annotation x) prefix)
       `(,(: prefix annotation) x))
      ((_ (:semantics x y ...) prefix)
       `(,(: prefix semantics)
         ,(smathml x prefix)
         ,(smathml y prefix)
         ...))
      ((_ (:annotation-xml x) prefix)
       `(,(: prefix annotation-xml) x))
      ;; constant and symbol elements
      ((_ :integers prefix)   `(,(: prefix integers)))
      ((_ :reals prefix)      `(,(: prefix reals)))
      ((_ :rationals prefix)  `(,(: prefix rationals)))
      ((_ :naturalnumbers prefix) `(,(: prefix naturalnumbers)))
      ((_ :complexes prefix)  `(,(: prefix complexes)))
      ((_ :primes prefix)     `(,(: prefix primes)))
      ((_ :exponentiale prefix) `(,(: prefix exponentiale)))
      ((_ :imaginaryi prefix) `(,(: prefix imaginaryi)))
      ((_ :notanumber prefix) `(,(: prefix notanumber)))
      ((_ :true prefix)       `(,(: prefix true)))
      ((_ :false prefix)      `(,(: prefix false)))
      ((_ :emptyset prefix)   `(,(: prefix emptyset)))
      ((_ :pi prefix)         `(,(: prefix pi)))
      ((_ :eulergamma prefix) `(,(: prefix eulergamma)))
      ((_ :infinity prefix)   `(,(: prefix infinity)))
      ;; just apply
      ((_ (f x ...) prefix)
       `(,(: prefix apply)
         (,(: prefix ci) ,(symbol->string 'f))
         ,(smathml x prefix)
         ...))
      ;; token elements
      ((_ x prefix)
       (if (number? 'x)
           `(,(: prefix cn) ,(number->string 'x))
           `(,(: prefix ci) ,(symbol->string 'x))))
      ;; no prefix
      ((_ x)
       (smathml x #f))))

  ;; SRFI-1 lset-union with eq?
  (define (%union . args)
    (if (null? args)
        '()
        (let loop ((a (car args))
                   (rest (cdr args)))
          (cond ((null? rest) a)
                ((null? a) (apply %union rest))
                (else
                 (let ((b (car rest)))
                   (loop
                    (fold-left (lambda (x v) (if (memq v x) x (cons v x))) a b)
                    (cdr rest))))))))

  ;; SRFI-1 lset-difference with eq?
  (define (%difference a . args)
    (let loop ((a a)
               (rest args))
      (cond ((null? a) '())
            ((null? rest) a)
            (else
             (loop (filter (lambda (v) (not (memq v (car rest)))) a)
                   (cdr rest))))))

  (define-syntax free-variables-union
    (syntax-rules ()
      ((_ x y ...)
       (%union (free-variables x) (free-variables y) ...))
      ((_)
       '())))

  (define-syntax free-variables-piece
    (syntax-rules ()
      ((_ (x y))
       (free-variables-union x y))
      ((_ (x))
       (free-variables x))))

  (define-syntax free-variables
    (syntax-rules (:interval
                   :inverse
                   :condition
                   :declare
                   :lambda
                   :compose
                   :ident
                   :domain
                   :codomain
                   :image
                   :domainofapplication
                   :piecewise
                   :quotient
                   :factorial
                   :divide
                   :max
                   :min
                   :minus
                   :plus
                   :power
                   :rem
                   :times
                   :root
                   :gcd
                   :and
                   :or
                   :xor
                   :not
                   :implies
                   :forall
                   :exists
                   :abs
                   :conjugate
                   :arg
                   :real
                   :imaginary
                   :lcm
                   :floor
                   :ceiling
                   :eq
                   :neq
                   :gt
                   :lt
                   :geq
                   :leq
                   :equivalent
                   :approx
                   :factorof
                   :int
                   :diff
                   :partialdiff
                   :lowlimit
                   :uplimit
                   :bvar
                   :degree
                   :divergence
                   :grad
                   :curl
                   :laplacian
                   :set
                   :list
                   :union
                   :intersect
                   :in
                   :notin
                   :subset
                   :prsubset
                   :notsubset
                   :notprsubset
                   :setdiff
                   :card
                   :cartesianproduct
                   :sum
                   :product
                   :limit
                   :tendsto
                   :exp
                   :ln
                   :log
                   :sin
                   :cos
                   :tan
                   :sec
                   :csch
                   :coth
                   :arcsin
                   :arccos
                   :arctan
                   :arccosh
                   :arccot
                   :arccoth
                   :arccsc
                   :arccsch
                   :arcsec
                   :arcsech
                   :arcsinh
                   :arctanh
                   :mean
                   :sdev
                   :variance
                   :median
                   :mode
                   :moment
                   :momentabout
                   :vector
                   :matrix
                   :matrixrow
                   :determinant
                   :transpose
                   :selector
                   :vectorproduct
                   :scalarproduct
                   :outerproduct
                   :annotation
                   :semantics
                   :annotation-xml
                   :integers
                   :reals
                   :rationals
                   :naturalnumbers
                   :complexes
                   :primes
                   :exponentiale
                   :imaginaryi
                   :notanumber
                   :true
                   :false
                   :emptyset
                   :pi
                   :eulergamma
                   :infinity)
      ;;; 4.4 The Content Markup Elements
      ;; basic content elements
      ((_ (:interval left right)) ; two child elements that evaluate to real numbers
       (free-variables-union left right))
      ((_ (:inverse f))
       (free-variables f))
      ((_ (:condition x))
       (free-variables x))
      ((_ (:declare e0 e1 ...))
       '())
      ((_ (:lambda (x ...) expr))
       (%difference (free-variables expr)
                    '(x ...)))
      ((_ (:compose f0 ...))
       (free-variables-union f0 ...))
      ((_ :ident)
       '())
      ((_ (:domain f))
       (free-variables f))
      ((_ (:codomain f))
       (free-variables f))
      ((_ (:image f))
       (free-variables f))
      ((_ (:domainofapplication x))
       (free-variables x))
      ((_ (:piecewise c0 c1 ...))
       (%union (free-variables-piece c0)
               (free-variables-piece c1)
               ...))
      ;; arithmetic, algebra and logic
      ((_ (:quotient left right))
       (free-variables-union left right))
      ((_ (:factorial n))
       (free-variables n))
      ((_ (:divide left right))
       (free-variables-union left right))
      ((_ (:max x0 x1 ...))
       (free-variables-union x0 x1 ...))
      ((_ (:min x0 x1 ...))
       (free-variables-union x0 x1 ...))
      ((_ (:minus e0 e1 ...))
       (free-variables-union e0 e1 ...))
      ((_ (:plus e0 e1 ...))
       (free-variables-union e0 e1 ...))
      ((_ (:power left right))
       (free-variables-union left right))
      ((_ (:rem left right))
       (free-variables-union left right))
      ((_ (:times e0 e1 ...))
       (free-variables-union e0 e1 ...))
      ((_ (:root x))
       (free-variables x))
      ((_ (:root d x))
       (free-variables-union d x))
      ((_ (:gcd e0 e1 ...))
       (free-variables-union e0 e1 ...))
      ((_ (:and x y ...))
       (free-variables-union x y ...))
      ((_ (:or x y ...))
       (free-variables-union x y ...))
      ((_ (:xor x y ...))
       (free-variables-union x y ...))
      ((_ (:not x))
       (free-variables x))
      ((_ (:implies x y))
       (free-variables-union x y))
      ((_ (:forall (x y ...) z ...))
       (%difference (free-variables-union z ...)
                    '(x y ...)))
      ((_ (:exists (x y ...) z ...))
       (%difference (free-variables-union z ...)
                    '(x y ...)))
      ((_ (:abs x))
       (free-variables x))
      ((_ (:conjugate x))
       (free-variables x))
      ((_ (:arg x))
       (free-variables x))
      ((_ (:real x))
       (free-variables x))
      ((_ (:imaginary x))
       (free-variables x))
      ((_ (:lcm x y z ...))
       (free-variables-union x y z ...))
      ((_ (:floor x))
       (free-variables x))
      ((_ (:ceiling x))
       (free-variables x))
      ;; relations
      ((_ (:eq x y ...))
       (free-variables-union x y ...))
      ((_ (:neq x y))
       (free-variables-union x y))
      ((_ (:gt x y ...))
       (free-variables-union x y ...))
      ((_ (:lt x y ...))
       (free-variables-union x y ...))
      ((_ (:geq x y ...))
       (free-variables-union x y ...))
      ((_ (:leq x y ...))
       (free-variables-union x y ...))
      ((_ (:equivalent x y ...))
       (free-variables-union x y ...))
      ((_ (:approx x y))
       (free-variables-union x y))
      ((_ (:factorof x y))
       (free-variables-union x y))
     ;; calculus and vector calculus 
      ((_ (:int (v low up) x))
       (%difference (free-variables-union low up x)
                    '(v)))
      ((_ (:int (:bvar x) y ...))
       (%difference (free-variables y ...)
                    '(x)))
      ((_ (:int (low up) x))
       (free-variables-union low up x))
      ((_ (:int x))
       (free-variables x))
      ((_ (:diff (:bvar v) x))
       (%difference (free-variables x)
                    '(v)))
      ((_ (:diff x))
       (free-variables x))
      ((_ (:partialdiff x y))
       (free-variables y)) ; FIXME
      ((_ (:divergence x))
       (free-variables x))
      ((_ (:grad x))
       (free-variables x))
      ((_ (:curl x))
       (free-variables x))
      ((_ (:laplacian x))
       (free-variables x))
      ;; theory of sets
      ((_ (:set (:bvar v) c x))
       (%difference (free-variables-union c x)
                    '(v)))
      ((_ (:set x ...))
       (free-variables-union x ...))
      ((_ (:list (:bvar v) c x))
       (%difference (free-variables-union c x)
                    '(v)))
      ((_ (:list x ...))
       (free-variables-union x ...))
      ((_ (:union (:bvar s) c x))
       (%difference (free-variables-union c x)
                    '(s)))
      ((_ (:union x y ...))
       (free-variables-union x y ...))
      ((_ (:intersect (:bvar s) c x))
       (%difference (free-variables-union c x)
                    '(s)))
      ((_ (:intersect x y ...))
       (free-variables-union x y ...))
      ((_ (:in x y))
       (free-variables-union x y))
      ((_ (:notin x y))
       (free-variables-union x y))
      ((_ (:subset x y z ...))
       (free-variables-union x y z ...))
      ((_ (:prsubset x y z ...))
       (free-variables-union x y z ...))
      ((_ (:notsubset x y))
       (free-variables-union x y))
      ((_ (:notprsubset x y))
       (free-variables-union x y))
      ((_ (:setdiff x y))
       (free-variables-union x y))
      ((_ (:card x))
       (free-variables x))
      ((_ (:cartesianproduct x y ...))
       (free-variables-union x y ...))
      ((_ (:sum (v lo hi) x))
       (%difference (free-variables-union lo hi x)
                    '(v)))
      ((_ (:sum (v c) x))
       (%difference (free-variables-union c x)
                    '(v)))
      ((_ (:sum d x))
       (free-variables x))
      ((_ (:product (v lo hi) x))
       (%difference (free-variables-union lo hi x)
                    '(v)))
      ((_ (:product (v c) x))
       (%difference (free-variables-union c x)
                    '(v)))
      ((_ (:product d x))
       (free-variables x))
      ((_ (:limit (v x) y))
       (%difference (free-variables-union x y)
                    '(v)))
      ((_ (:limit v x y))
       (%difference (free-variables-union x y)
                    '(v)))
      ((_ (:tendsto x y))
       (free-variables y))
      ((_ (:exp x))
       (free-variables x))
      ((_ (:ln x))
       (free-variables x))
      ((_ (:log x))
       (free-variables x))
      ((_ (:log b x))
       (free-variables-union b x))
      ((_ (:sin x))     (free-variables x))
      ((_ (:cos x))     (free-variables x))
      ((_ (:tan x))     (free-variables x))
      ((_ (:sec x))     (free-variables x))
      ((_ (:csch x))    (free-variables x))
      ((_ (:coth x))    (free-variables x))
      ((_ (:arcsin x))  (free-variables x))
      ((_ (:arccos x))  (free-variables x))
      ((_ (:arctan x))  (free-variables x))
      ((_ (:arccosh x)) (free-variables x))
      ((_ (:arccot x))  (free-variables x))
      ((_ (:arccoth x)) (free-variables x))
      ((_ (:arccsc x))  (free-variables x))
      ((_ (:arccsch x)) (free-variables x))
      ((_ (:arcsec x))  (free-variables x))
      ((_ (:arcsech x)) (free-variables x))
      ((_ (:arcsinh x)) (free-variables x))
      ((_ (:arctanh x)) (free-variables x))
      ;; statistics
      ((_ (:mean x y ...))
       (free-variables-union x y ...))
      ((_ (:sdev x y ...))
       (free-variables-union x y ...))
      ((_ (:variance x y ...))
       (free-variables-union x y ...))
      ((_ (:median x y ...))
       (free-variables-union x y ...))
      ((_ (:mode x y ...))
       (free-variables-union x y ...))
      ((_ (:moment d a x))
       (free-variables-union d a x))
      ;; linear algebra
      ((_ (:vector x ...))
       (free-variables-union x ...))
      ((_ (:matrix x ...))
       (free-variables-union x ...))
      ((_ (:determinant x))
       (free-variables x))
      ((_ (:transpose x))
       (free-variables x))
      ((_ (:selector x i0 i1 ...))
       (free-variables-union x i0 i1 ...))
      ((_ (:vectorproduct x y))
       (free-variables-union x y))
      ((_ (:scalarproduct x y))
       (free-variables-union x y))
      ((_ (:outerproduct x y))
       (free-variables-union x y))
      ;; semantic mapping elements
      ((_ (:annotation x))
       '())
      ((_ (:semantics x y ...))
       (free-variables-union x y ...))
      ((_ (:annotation-xml x))
       '())
      ;; constant and symbol elements
      ((_ :integers)   '())
      ((_ :reals)      '())
      ((_ :rationals)  '())
      ((_ :naturalnumbers) '())
      ((_ :complexes)  '())
      ((_ :primes)     '())
      ((_ :exponentiale) '())
      ((_ :imaginaryi) '())
      ((_ :notanumber) '())
      ((_ :true)       '())
      ((_ :false)      '())
      ((_ :emptyset)   '())
      ((_ :pi)         '())
      ((_ :eulergamma) '())
      ((_ :infinity)   '())
      ;; just apply
      ((_ (f x ...))
       (free-variables-union f x ...))
      ;; token elements
      ((_ x)
       (if (number? 'x)
           '()
           '(x)))))

)
