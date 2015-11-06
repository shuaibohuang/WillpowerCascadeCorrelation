(setq *print-case* :downcase)

(defun continuous-xor (start step)
  "(start step)
Make training or test patterns for continuous-xor."
  (do ((x start (float->decimals (+ x step) 2))
       (patterns nil))
      ((> x 1.0) (reverse patterns))
    (do ((y start (float->decimals (+ y step) 2)))
        ((> y 1.0))
      (let ((output (cond ((and (< x .55)
                                (< y .55))
                           -.5)
                          ((and (> x .55)
                                (< y .55))
                           .5)
                          ((and (< x .55)
                                (> y .55))
                           .5)
                          ((and (> x .55)
                                (> y .55))
                           -.5)))
            (inputs (list x y)))
        (setf patterns (cons (list inputs (list output)) patterns))))))

(defun random-xor (start step learnability)
  "(start step percent-predictable)
Make training patterns for continuous-xor with a %predictable chance of any pattern having random output value."
  (do ((x start (float->decimals (+ x step) 2))
       (patterns nil))
      ((> x 1.0) (reverse patterns))
    (do ((y start (float->decimals (+ y step) 2)))
        ((> y 1.0))
      (let ((output (if (> learnability (random 100))
                        (cond ((and (< x .55)
                                    (< y .55))
                               -.5)
                              ((and (> x .55)
                                    (< y .55))
                               .5)
                              ((and (< x .55)
                                    (> y .55))
                               .5)
                              ((and (> x .55)
                                    (> y .55))
                               -.5))
                      (if (< (random 2) 1)
                          -.5
                        .5)))
            (inputs (list x y)))
        (if (> attention (random 100))
         (setf patterns (cons (list inputs (list output)) patterns)))
        ))))

(defun run-learnability (attention learnability patience threshold n path)
  "(attention learnability patience threshold n path)
Run n continuous-xor nets for parameter settings. Record training & test error every output epoch.
Record output-epochs, outcome, structure."
  (setf 
   *test* t
   *test-interval* 1
   *record-train-errors* t
   *record-test-errors* t
   *mark-hiddens-errors* t
   *sdcc* t
   *learning-patience* patience
   *learning-threshold* threshold)
  (do ((i 0 (1+ i))
       (epochslist nil)
       (outcome 0)
       (outcomelist nil)
       (output-epochs 0)
       (output-epochs-list nil)
       (test-errors-list nil)
       (train-errors-list nil)
       (structures nil))
      ((= i n) 
       (progn
         (lists->file train-errors-list (concatenate 'string path "train-errors"))
         (lists->file test-errors-list (concatenate 'string path "test-errors"))
         (lists->file output-epochs-list (concatenate 'string path "output-epochs"))
         (lists->file outcomelist (concatenate 'string path "outcomes"))
         (lists->file epochslist (concatenate 'string path "epochs"))
         (lists->file structures (concatenate 'string path "structures"))))
    (seed-random)
    (terpri)
    (format t "Attention = ~A, learnability = ~A, patience = ~A, threshold = ~,2F, network = ~A ~%"
      attention learnability patience threshold i)
    (set-patterns (random-xor .1 .1 attention learnability) 'train)
    (set-patterns (continuous-xor .14 .1) 'test)
    (setf 
     outcome (train 100 100 25 2000)
     outcomelist (cons outcome outcomelist)
     epochslist (cons *epoch* epochslist)
     output-epochs (length *train-errors*)
     output-epochs-list (cons output-epochs output-epochs-list)
     test-errors-list (cons (reverse *test-errors*) test-errors-list)
     train-errors-list (cons (reverse *train-errors*) train-errors-list)
     structures (cons (reverse *structure*) structures))
    (lists->file *train-errors* (concatenate 'string path "train-errors" (princ-to-string i)))
    (lists->file *test-errors* (concatenate 'string path "test-errors" (princ-to-string i)))
    (format t "structure = ~S ~%"
      (reverse *structure*))))

(defun pair-lists (x y)
  "(x y)
Pair corresponding items in lists x & y into sublists."
  (do ((xs x (cdr xs))
       (ys y (cdr ys))
       (result nil (cons (list (car xs) (car ys))
                         result)))
      ((null xs) (reverse result))))

;;;(pair-lists '(a b c) '(1 2 3))

(defun listof2->string (x)
  "(x)
Convert list of 2 items to string followed by -."
  (concatenate 'string (princ-to-string (first x)) (princ-to-string (second x)) "-"))

;;;(listof2->string '(a 1))
;;;(mapcar #'listof2->string '((a 1) (b 2)))

(defun list->strings (list)
  "(list)
Convert list of strings to 1 string."
  (apply #'concatenate 'string list))

;;;(list->strings '("lrn95-" "ptn2-" "thr0.25-"))

;;;(list->strings '(a 1 b 2))
       
(defun make-folders (parameters path &rest sets)
  "(parameters path &rest sets)
Make folders at end of path for all combinations of parameter values in sets."
  (do ((folder-labels (apply #'cartesian-product sets) (cdr folder-labels)))
      ((null folder-labels))
    (let ((label (pair-lists parameters (car folder-labels))))
      (make-directory (concatenate 'string path (list->strings (mapcar #'listof2->string label)) "\\")))))

(defun run-all (parameters n path &rest sets)
  "(parameters path &rest sets)
Make folders at end of path for all combinations of parameter values in sets."
  (do ((folder-labels (apply #'cartesian-product sets) (cdr folder-labels)))
      ((null folder-labels))
    (let* ((parameter-values (car folder-labels))
           (label (pair-lists parameters parameter-values))
           (learnability (first parameter-values))
           (patience (second parameter-values))
           (threshold (third parameter-values)))
      (run-learnability 
       learnability
       patience
       threshold
       n
       (concatenate 'string path (list->strings (mapcar #'listof2->string label)) "\\")))))

;;;(make-folders '(lrn ptn thr) 
;;;              "C:\\users\\tom_2.tom-pcstudio15\\documents\\courses\\315\\2012\\cessation\\results\\" 
;;;              '(95 100)
;;;              '(2 4)
;;;              '(.25 .30))

;;;(run-all '(lrn ptn thr)
;;;         2
;;;         "C:\\users\\tom_2.tom-pcstudio15\\documents\\courses\\315\\2012\\cessation\\results\\"
;;;         '(95 100)
;;;         '(2 4)
;;;         '(.25 .30))

;;;(make-folders '(lrn ptn thr) 
;;;              "C:\\users\\tom\\documents\\courses\\315\\2012\\cessation\\results2\\" 
;;;              '(95 100)
;;;              '(2 4)
;;;              '(.25 .30))

;;;(run-all '(lrn ptn thr)
;;;         2
;;;         "C:\\users\\tom\\documents\\courses\\315\\2012\\cessation\\results2\\"
;;;         '(95 100)
;;;         '(2 4)
;;;         '(.25 .30))

