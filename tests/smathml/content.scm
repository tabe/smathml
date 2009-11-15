#!r6rs

(import (except (rnrs (6)) lambda)
        (only (srfi :1) lset=)
        (smathml content)
        (xunit))

(define-assert-predicate lset=)

(define-syntax assert-smathml
  (syntax-rules ()
    ((_ expected tree prefix)
     (assert-equal? expected (smathml tree prefix)))))

(define-syntax assert-free-variables
  (syntax-rules ()
    ((_ expected tree)
     (assert-lset= eq? expected (free-variables tree)))))

(assert-smathml '(m:apply (m:interval) (m:cn "0") (m:cn "1")) (interval 0 1) m)
(assert-smathml '(m:apply (m:inverse) (m:ci "f")) (inverse f) m)
(assert-smathml '(m:condition (m:apply (m:eq) (m:ci "X") (m:cn "1"))) (condition (eq X 1)) m)
(assert-smathml '(m:declare (m:ci "V") (m:vector (m:cn "1") (m:cn "2") (m:cn "3"))) (declare V (vector 1 2 3)) m)
(assert-smathml '(m:lambda (m:bvar (m:ci "x")) (m:apply (m:plus) (m:ci "x") (m:cn "1"))) (lambda (x) (plus x 1)) m)
(assert-smathml '(m:apply (m:compose) (m:ci "f") (m:ci "g")) (compose f g) m)
(assert-smathml '(m:ident) ident m)
(assert-smathml '(m:apply (m:domain) (m:ci "f")) (domain f) m)
(assert-smathml '(m:apply (m:codomain) (m:ci "f")) (codomain f) m)
(assert-smathml '(m:apply (m:image) (m:ci "f")) (image f) m)
(assert-smathml '(m:domainofapplication (m:ci "C")) (domainofapplication C) m)
(assert-smathml '(m:piecewise
                  (m:piece (m:cn "0")
                           (m:apply (m:lt) (m:ci "x") (m:cn "0")))
                  (m:otherwise (m:ci "x")))
                (piecewise (0 (lt x 0)) (x))
                m)
(assert-smathml '(m:apply (m:eq) (m:ci "X") (m:cn "1")) (eq X 1) m)
(assert-smathml '(m:apply (m:lt) (m:ci "X") (m:cn "1")) (lt X 1) m)
(assert-smathml '(m:apply (m:leq) (m:ci "X") (m:cn "1")) (leq X 1) m)
(assert-smathml '(m:apply (m:gt) (m:ci "X") (m:cn "1")) (gt X 1) m)
(assert-smathml '(m:apply (m:geq) (m:ci "X") (m:cn "1")) (geq X 1) m)
(assert-smathml '(m:apply (m:diff) (m:bvar (m:ci "x")) (m:ci "f")) (diff (bvar x) f) m)
(assert-smathml '(m:apply (m:diff) (m:apply (m:diff) (m:ci "f"))) (diff (diff f)) m)
(assert-smathml '(m:apply (m:ci "f") (m:ci "x") (m:ci "y")) (f x y) m)
(assert-smathml '(m:apply (m:minus) (m:apply (m:minus) (m:ci "A") (m:ci "B")) (m:ci "C")) (minus A B C) m)
(assert-smathml '(m:apply (m:plus) (m:apply (m:plus) (m:ci "A") (m:ci "B")) (m:ci "C")) (plus A B C) m)
(assert-smathml '(m:apply (m:times) (m:apply (m:times) (m:ci "A") (m:ci "B")) (m:ci "C")) (times A B C) m)
(assert-smathml '(m:apply (m:eq)(m:apply (m:ln) (m:exponentiale)) (m:cn "1")) (eq (ln exponentiale) 1) m)

(assert-free-variables '(X) (eq X 1))
(assert-free-variables '(X) (lt X 1))
(assert-free-variables '(X) (leq X 1))
(assert-free-variables '(X) (gt X 1))
(assert-free-variables '(X) (geq X 1))
(assert-free-variables '(f) (diff (bvar x) f))
(assert-free-variables '(f) (diff (diff f)))
(assert-free-variables '(f x y) (f x y))
(assert-free-variables '(A B C) (minus A B C))
(assert-free-variables '(A B C) (plus A B C))
(assert-free-variables '(A B C) (times A B C))
(assert-free-variables '() (eq (ln exponentiale) 1))

(report)
