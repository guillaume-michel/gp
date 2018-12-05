(defpackage #:gp.examples.majority-on
  (:use #:cl
        #:gp.kernel
        #:gp.simplify-rules)
  (:export #:run
           #:run-example
   ))

(in-package #:gp.examples.majority-on)

;;; Boolean 3-Majority-on Problem

(defvar d0)
(defvar d1)
(defvar d2)

(defun define-terminal-set-for-MAJORITY-ON ()
  (values '(d2 d1 d0)))

(defun define-function-set-for-MAJORITY-ON ()
  (values '(and and or not)
          '(  2   3  2   1)))

(defstruct MAJORITY-ON-fitness-case
  d0
  d1
  d2
  target)

(defun define-fitness-cases-for-MAJORITY-ON ()
  (let (fitness-case fitness-cases index)
    (setf fitness-cases (make-array *number-of-fitness-cases*))
    (format t "~%Fitness cases")
    (setf index 0)
    (dolist (d2 '(t nil))
      (dolist (d1 '(t nil))
        (dolist (d0 '(t nil))
          (setf fitness-case (make-MAJORITY-ON-fitness-case))
          (setf (MAJORITY-ON-fitness-case-d0 fitness-case) d0)
          (setf (MAJORITY-ON-fitness-case-d1 fitness-case) d1)
          (setf (MAJORITY-ON-fitness-case-d2 fitness-case) d2)
          (setf (MAJORITY-ON-fitness-case-target fitness-case)
                (or (and d2 d1 (not d0))
                    (and d2 (not d1) d0)
                    (or (and (not d2) d1 d0)
                        (and d2 d1 d0))))
          (setf (aref fitness-cases index) fitness-case)
          (incf index)
          (format t
                  "~% ~D    ~S   ~S   ~S       ~S"
                  index d2 d1 d0
                  (MAJORITY-ON-fitness-case-target fitness-case)))))
    (values fitness-cases)))

(defun MAJORITY-ON-wrapper (result-from-program)
  (values result-from-program))

(defun evaluate-standardized-fitness-for-MAJORITY-ON (program fitness-cases)
  (let (raw-fitness hits standardized-fitness target-value
                    match-found value-from-program fitness-case)
    (setf raw-fitness 0.0)
    (setf hits 0)
    (dotimes (index *number-of-fitness-cases*)
      (setf fitness-case (aref fitness-cases index))
      (setf d0 (MAJORITY-ON-fitness-case-d0 fitness-case))
      (setf d1 (MAJORITY-ON-fitness-case-d1 fitness-case))
      (setf d2 (MAJORITY-ON-fitness-case-d2 fitness-case))
      (setf target-value (MAJORITY-ON-fitness-case-target fitness-case))
      (setf value-from-program (MAJORITY-ON-wrapper (eval program)))
      (setf match-found (eq target-value value-from-program))
      (incf raw-fitness (if match-found 1.0 0.0))
      (when match-found (incf hits)))
    (setf standardized-fitness (- *number-of-fitness-cases* raw-fitness))
    (values standardized-fitness hits)))

(defun define-parameters-for-MAJORITY-ON ()
  (setf *rules* *boolean-rules*)
  (setf *number-of-fitness-cases* 8)
  (setf *max-depth-for-new-individuals* 6)
  (setf *max-depth-for-new-subtrees-in-mutants* 4)
  (setf *max-depth-for-individuals-after-crossover* 17)
  (setf *fitness-proportionate-reproduction-fraction* 0.1)
  (setf *crossover-at-any-point-fraction* 0.2)
  (setf *crossover-at-function-point-fraction* 0.7)
  (setf *method-of-selection* :fitness-proportionate)
  (setf *method-of-generation* :ramped-half-and-half)
  (values))

(defun define-termination-criterion-for-MAJORITY-ON (current-generation
                                                     maximum-generations
                                                     best-standardized-fitness
                                                     best-hits)
  (declare (ignore best-standardized-fitness))
  (values (or (>= current-generation maximum-generations)
              (>= best-hits *number-of-fitness-cases*))))

(defun MAJORITY-ON ()
  (values 'define-function-set-for-MAJORITY-ON
          'define-terminal-set-for-MAJORITY-ON
          'define-fitness-cases-for-MAJORITY-ON
          'evaluate-standardized-fitness-for-MAJORITY-ON
          'define-parameters-for-MAJORITY-ON
          'define-termination-criterion-for-MAJORITY-ON))

(defun run (maximum-generations
            size-of-population
            seed
            &key (verbose nil))
  (let ((*verbose* (if verbose :verbose :silent)))
    (gp.kernel:run-genetic-programming-system 'MAJORITY-ON
                                            seed
                                            maximum-generations
                                            size-of-population)))

(defun run-example (&key (verbose nil))
  (multiple-value-bind (population fitness-cases)
      (run 1000 50 48 :verbose verbose)
    (declare (ignore population fitness-cases))))

(defun bench ()
  (multiple-value-bind (population fitness-cases)
      (time
       (run 1000 50 48 :verbose nil))
    (declare (ignore population fitness-cases))))
