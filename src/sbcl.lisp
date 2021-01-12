(defpackage sbcl-math-helper
  (:use :cl))
(in-package :sbcl-math-helper)

(macrolet ((def-alien (lisp-name alien-name)
             (let ((x (gensym)))
               `(progn
                  (declaim (inline ,lisp-name))
                  (defun ,lisp-name (,x)
                    (sb-alien:alien-funcall
                     (sb-alien:extern-alien
                      ,alien-name
                      (function single-float (single-float)))
                     ,x))))))
  (def-alien %cosf "cosf")
  (def-alien %sinf "sinf"))

(sb-c:defknown (%sinf %cosf)
    (single-float) single-float
    (sb-c:movable sb-c:flushable sb-c:foldable))

(macrolet ((def-transform (math-fn alien-fn)
             (let ((arg (gensym)))
               `(sb-c:deftransform ,math-fn ((,arg) (single-float) *)
                  '(,alien-fn ,arg)))))
  (def-transform sin %sinf)
  (def-transform cos %cosf))
