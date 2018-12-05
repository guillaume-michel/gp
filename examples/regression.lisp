(defpackage #:gp.examples.regression
  (:use #:cl
        #:gp.kernel
        #:gp.functions
        #:gp.simplify
        #:gp.simplify-rules
        )
  (:export #:run
           #:run-example
           #:bench
           ))

(in-package #:gp.examples.regression)

;;; Regression Problem for 0.5x**2

(defvar x)

(defun define-terminal-set-for-REGRESSION ()
  (values '(x :floating-point-random-constant)))

(defun define-function-set-for-REGRESSION ()
  (values '(+ - * %)
          '(2 2 2 2)))

(defstruct REGRESSION-fitness-case
  independent-variable
  target)

(defun define-fitness-cases-for-REGRESSION ()
  (let (fitness-cases x this-fitness-case)
    (setf fitness-cases (make-array *number-of-fitness-cases*))
    (when (eq *verbose* :verbose)
      (format t "~%Fitness cases"))
    (dotimes (index *number-of-fitness-cases*)
      (setf x (/ index *number-of-fitness-cases*))
      (setf this-fitness-case (make-REGRESSION-fitness-case))
      (setf (aref fitness-cases index) this-fitness-case)
      (setf (REGRESSION-fitness-case-independent-variable this-fitness-case) x)
      (setf (REGRESSION-fitness-case-target this-fitness-case) (* 0.5 x x))
      (when (eq *verbose* :verbose)
        (format t "~% ~D      ~D      ~D" index (float x) (REGRESSION-fitness-case-target this-fitness-case))))
    (values fitness-cases)))

(defun REGRESSION-wrapper (result-from-program)
  (values result-from-program))

(defun evaluate-standardized-fitness-for-REGRESSION (program fitness-cases)
  (let (raw-fitness hits standardized-fitness x target-value difference value-from-program this-fitness-case func)
    (setf raw-fitness 0.0)
    (setf hits 0)
    (setf func (compile-program '(x) program))
    (dotimes (index *number-of-fitness-cases*)
      (setf this-fitness-case (aref fitness-cases index))
      (setf x (REGRESSION-fitness-case-independent-variable this-fitness-case))
      (setf target-value (REGRESSION-fitness-case-target this-fitness-case))
      (setf value-from-program (REGRESSION-wrapper (funcall func x)))
      (setf difference (abs (- target-value value-from-program)))
      (incf raw-fitness difference)
      (when (< difference 0.01) (incf hits)))
    (setf standardized-fitness raw-fitness)
    (values standardized-fitness hits)))

(defun define-parameters-for-REGRESSION ()
  (setf *number-of-fitness-cases* 10000)
  (setf *max-depth-for-new-individuals* 6)
  (setf *max-depth-for-individuals-after-crossover* 17)
  (setf *fitness-proportionate-reproduction-fraction* 0.1)
  (setf *crossover-at-any-point-fraction* 0.2)
  (setf *crossover-at-function-point-fraction* 0.2)
  (setf *max-depth-for-new-subtrees-in-mutants* 4)
  (setf *method-of-selection* :fitness-proportionate)
  (setf *method-of-generation* :ramped-half-and-half)
  (values))

(defun define-termination-criterion-for-REGRESSION (current-generation
                                                    maximum-generations
                                                    best-standardized-fitness
                                                    best-hits)
  (declare (ignore best-standardized-fitness))
  (values
   (or (>= current-generation maximum-generations)
       (>= best-hits *number-of-fitness-cases*))))

(defun REGRESSION ()
  (values 'define-function-set-for-REGRESSION
          'define-terminal-set-for-REGRESSION
          'define-fitness-cases-for-REGRESSION
          'evaluate-standardized-fitness-for-REGRESSION
          'define-parameters-for-REGRESSION
          'define-termination-criterion-for-REGRESSION))

(defun run (maximum-generations
            size-of-population
            seed
            &key (verbose nil))
  (let ((*verbose* (if verbose :verbose :silent)))
    (gp.kernel:run-genetic-programming-system 'REGRESSION
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
       (run 100 50 48 :verbose nil))
    (declare (ignore population fitness-cases))))
