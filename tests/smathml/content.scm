#!r6rs

(import (rnrs (6))
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
