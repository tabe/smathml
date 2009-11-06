#!r6rs

(import (rnrs (6))
        (smathml content)
        (xunit))

(define-syntax assert-mathml
  (syntax-rules ()
    ((_ expected tree prefix)
     (assert-string=? expected (tree->string (mathml tree prefix))))
    ((_ expected tree)
     (assert-string=? expected (tree->string (mathml tree))))))

(assert-mathml "<apply><eq/><ci>X</ci><cn>1</cn></apply>" (eq X 1))
(assert-mathml "<m:apply><m:eq/><m:ci>X</m:ci><m:cn>1</m:cn></m:apply>" (eq X 1) 'm)
(assert-mathml "<apply><diff/><bvar><ci>x</ci></bvar><ci>f</ci></apply>" (diff (bvar x) f))
(assert-mathml "<m:apply><m:diff/><m:bvar><m:ci>x</m:ci></m:bvar><m:ci>f</m:ci></m:apply>" (diff (bvar x) f) 'm)
(assert-mathml "<apply><diff/><apply><diff/><ci>f</ci></apply></apply>" (diff (diff f)))
(assert-mathml "<m:apply><m:diff/><m:apply><m:diff/><m:ci>f</m:ci></m:apply></m:apply>" (diff (diff f)) 'm)
(assert-mathml "<apply><ci>f</ci><ci>x</ci><ci>y</ci></apply>" (f x y))
(assert-mathml "<m:apply><m:ci>f</m:ci><m:ci>x</m:ci><m:ci>y</m:ci></m:apply>" (f x y) 'm)

(report)
