

(defun cartesian-atom-list (x lst)
  "(x lst)
Combine item x with each sublist in lst."
  (cond ((null lst) nil)
        (t (cons (cons x (car lst))
                 (cartesian-atom-list x (cdr lst))))))

(defun cartesian-1-with-many-sets (x y)
  "(x y)
Cartesian product of 1 set x with many sets y."
  (if (and x y)
      (append (cartesian-atom-list (car x) y)
              (cartesian-1-with-many-sets (cdr x) y))))

(defun cartesian-product (&rest sets)
  "(&rest sets)
Make cartesian product of list of sets."
  (reduce #'cartesian-1-with-many-sets sets
          :from-end t
          :initial-value '(())))

;;;(cartesian-product '(95 100)
;;;                   '(2 4)
;;;                   '(.25 .30))
;;;
;;;(cartesian-product '(a b) '(1 2 3) '(x y))

(defun append-net#-score (conditions net score)
  "(conditions net score)
Append condition combinations & net# to score."
  (append conditions (list net) (list score)))

;;;(append-net#-score '(95 2 .25) 2 435)
;;;conditions will be same for all networks

(defun append-net#s-scores (conditions scores)
  "(conditions scores)
Append net#s & scores to conditions."
  (do ((scrs scores (cdr scrs))
       (i 0 (1+ i))
       (results nil (cons (append-net#-score conditions
                                             i
                                             (car scrs))
                          results)))
      ((null scrs) (reverse results))))

;;;(append-net#s-scores folder contents))))
;;;(append-net#s-scores '(95 2 0.3) '(789 944))

(defun process1folder (parameters parameter-values measure path)
  "(parameters parameter-values measure path)
Process 1 data folder from parameter names & values, dependent measure, & path."
  (let* ((label (pair-lists parameters parameter-values))
         (contents (file->list (concatenate 'string path 
                                 (list->strings (mapcar #'listof2->string label)) 
                                 "\\" 
                                 (princ-to-string measure)))))
    (append-net#s-scores parameter-values contents)))

;;;(process1folder '(lrn ptn thr)
;;;                '(95 2 0.3) 
;;;                'output-epochs
;;;                "C:\\users\\tom_2.tom-pcstudio15\\documents\\courses\\315\\2012\\cessation\\results\\")

(defun process-all-folders (parameters measure path &rest sets)
  "(parameters measure path &rest sets)
Process all data folders from parameters, dependent measure, path, & parameter-value sets."
  (do ((folders (apply #'cartesian-product sets) (cdr folders))
       (data nil (append data (process1folder parameters (car folders) measure path))))
      ((null folders) (lists->file (reverse data) (concatenate 'string path (princ-to-string measure))))))

;;;(process-all-folders 
;;; '(lrn ptn thr)
;;; 'output-epochs
;;; "C:\\users\\tom_2.tom-pcstudio15\\documents\\courses\\315\\2012\\cessation\\results\\"
;;; '(95 100) '(2 4) '(.25 .30))

(process-all-folders 
 '(lrn ptn thr)
 'output-epochs
 "C:\\users\\tom\\documents\\courses\\315\\2012\\cessation\\results2\\"
 '(95 100) '(2 4) '(.25 .30))
