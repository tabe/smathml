#!r6rs

(import (rnrs)
        (smathml)
        (xunit))

(define-syntax assert-smathml->string
  (syntax-rules ()
    ((_ expected tree)
     (assert-string=? expected (smathml->string (make-smathml tree #f))))
    ((_ expected tree prefix)
     (assert-string=? expected (smathml->string (make-smathml tree prefix))))))

(assert-smathml->string "<apply><eq/><ci>X</ci><cn>3</cn></apply>" '(= X 3))
(assert-smathml->string "<m:apply><m:eq/><m:ci>X</m:ci><m:cn>3</m:cn></m:apply>" '(= X 3) 'm)

(report)
