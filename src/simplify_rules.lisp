(defpackage #:gp.simplify-rules
  (:use #:cl #:gp.simplify)
  (:export #:*boolean-rules*
           ))

(in-package #:gp.simplify-rules)

;;; Rule base.

(defvar *boolean-rules* nil
  "The rule base for Boolean problems.")

;;; Transforms expressions of the form (not (not <xxx>)) into ;;; <xxx>.
(def-edit-rule not-not-x->-x *boolean-rules* (sexpression)
               :condition (and (consp sexpression)
                               (consp (second sexpression))
                               (eq (first sexpression) 'not)
                               (eq (first (second sexpression)) 'not))
               :action (replace-sexpression (second (second sexpression))))

;;; Transforms expressions of the form (or <xxx> t) into t.
(def-edit-rule or-t->-t *boolean-rules* (sexpression)
               :condition (and (consp sexpression)
                               (eq 'or (first sexpression))
                               (dolist (arg (rest sexpression) nil)
                                 (when (and (constant-expression-p arg)
                                            (eval arg))
                                   (return t))))
               :action (replace-sexpression t))

;;; Transforms expressions of the form (and nil <xxx>) into nil.
(def-edit-rule and-nil->-nil *boolean-rules* (sexpression)
               :condition (and (consp sexpression)
                               (eq 'and (first sexpression))
                               (dolist (arg (rest sexpression) nil)
                                 (when (and (constant-expression-p arg)
                                            (not (eval arg)))
                                   (return t))))
               :action (replace-sexpression nil))

;;; Transforms expressions of the form (and t <xxx>) into <xxx>.
(def-edit-rule and-t->-x *boolean-rules* (sexpression)
               :condition (and (consp sexpression)
                               (eq 'and (first sexpression))
                               (dolist (arg (rest sexpression) nil)
                                 (when (and (constant-expression-p arg)
                                            (eval arg))
                                   (return t))))
               :action (let ((remaining-args
                              (remove-if #'(lambda (arg)
                                             (and (constant-expression-p arg)
                                                  (eval arg)))
                                         (rest sexpression))))
                         (replace-sexpression
                          (case (length remaining-args)
                            (0 t)
                            (1 (first remaining-args))
                            (otherwise (cons 'and remaining-args))))))

;;; Transforms expressions of the form (or <xxx> nil) into
;;; <xxx>.
(def-edit-rule or-nil->-x *boolean-rules* (sexpression)
               :condition (and (consp sexpression)
                               (eq 'or (first sexpression))
                               (dolist (arg (rest sexpression) nil)
                                 (when (and (constant-expression-p arg)
                                            (not (eval arg)))
                                   (return t))))
               :action (let ((remaining-args
                              (remove-if #'(lambda (arg)
                                             (and (constant-expression-p arg)
                                                  (not (eval arg))))
                                         (rest sexpression))))
                         (replace-sexpression
                          (case (length remaining-args)
                            (0 nil)
                            (1 (first remaining-args))
                            (otherwise (cons 'or remaining-args))))))

;;; Combines calls to AND and OR into their polyadic forms, so
;;; (and (and <xxx> <yyy>) <zzz>) will be transformed into (and
;;; <xxx> <yyy> <zzz>).
(def-edit-rule polyadicize *boolean-rules* (sexpression)
               :condition (and (consp sexpression)
                               (member (first sexpression) '(and or)
                                       :test #'eq)
                               (dolist (arg (rest sexpression) nil)
                                 (when (and (consp arg)
                                            (eq (first arg)
                                                (first sexpression)))
                                   (return t))))
               :action (let ((interesting-arg
                              (dolist (arg (rest sexpression) nil)
                                (when (and (consp arg)
                                           (eq (first arg)
                                               (first sexpression)))
                                  (return arg)))))
                         (replace-sexpression
                          (cons (first sexpression)
                                (append (rest interesting-arg)
                                        (remove interesting-arg
                                                (rest sexpression)))))))

;;; Transforms expressions of the form (and <xxx> <xxx>) into
;;; <xxx>.
;;; Rule added by GM
(def-edit-rule and-x-x->-x *boolean-rules* (sexpression)
               :condition (and (consp sexpression)
                               (eq 'and (first sexpression))
                               (= (length sexpression) 3)
                               (symbolp (second sexpression))
                               (symbolp (third sexpression))
                               (eq (second sexpression) (third sexpression)))
               :action (replace-sexpression (second sexpression)))
