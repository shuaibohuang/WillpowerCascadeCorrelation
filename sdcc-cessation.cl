;;;file: sdcc-cessation.cl
;;;purpose: add ability for SDCC to cease learning if significant progress is not made
;;;programmer: Eric Doty & Thomas R. Shultz
;;;modifications: use *hidden-first-time* throughout, rather than switching to hidden-first-time
;;;  When no progress, state that quit-counter is incremented.
;;;  When patience is exhausted, say that. 
;;;started: 15 nov 11
;;;current: 18 aug 12

;;;global variables

(defvar *hidden-first-time* t
  "If true, it is the first time a hidden unit is being recruited.")

(defvar *last-true-error* 0.0
  "Used to check the amount of learning between the recruiting of hidden units.")

(defvar *learning-threshold* 0.01
  "Error must change by at least this fraction of its old value in order to count as significant.")

(defvar *learning-patience* 2
  "Number of learning cycles willing to accept without quitting. i.e. If set to 2,
learning will quit after 3 unsuccesful hidden unit recruitments.")

(defvar *quit-counter* 0
  "Counter for how many consecutive cycles there have been without learning. If it reaches
learning-patience, the next time no progress it made, the algorithm quits.")

(defun build-net (ninputs noutputs)
  "(ninputs noutputs)
Create the network data structures, given the number of input and output
connections.  Get *MAX-UNITS* and other dimesntions from variables.
Modified to set *max-units* to 40."
  (declare (fixnum ninputs noutputs))
  ;; Fill in assorted variables and create top-level vectors.
  (setq *ninputs* ninputs
      ;;;   *max-units* increased from +20 to +35)
      *max-units* (+ ninputs 40)
      *noutputs* noutputs
      *max-cases* (length *training-inputs*)
      *ncases* *max-cases*
      *first-case* 0
      *nunits* (+ 1 *ninputs*)
      *values-cache* (make-array *max-cases* :initial-element nil)
      *extra-values* (make-array *max-units*
                                 :element-type 'short-float
                                 :initial-element 0.0)
      *values* *extra-values*
      *nconnections* (make-array *max-units*
                                 :element-type 'fixnum
                                 :initial-element 0)
      *connections* (make-array *max-units* :initial-element nil)
      *weights* (make-array *max-units* :initial-element nil)
      *outputs* (make-array *noutputs*
                            :element-type 'short-float
                            :initial-element 0.0)
      *errors-cache* (make-array *max-cases* :initial-element nil)
      *extra-errors* 	(make-array *noutputs*
                                  :element-type 'short-float
                                  :initial-element 0.0)
      *errors* *extra-errors*
      *sum-errors* (make-array *noutputs*
                               :element-type 'short-float
                               :initial-element 0.0)
      *dummy-sum-errors* (make-array *noutputs*
                                     :element-type 'short-float
                                     :initial-element 0.0)
      *output-weights* (make-array *noutputs* :initial-element nil)
      *output-weights-record* (make-array *max-units* :initial-element nil)
      *output-deltas* (make-array *noutputs* :initial-element nil)
      *output-slopes* (make-array *noutputs* :initial-element nil)
      *output-prev-slopes* (make-array *noutputs* :initial-element nil)
      *cand-sum-values* (make-array *ncandidates*
                                    :element-type 'short-float
                                    :initial-element 0.0)
      *cand-cor* (make-array *ncandidates* :initial-element nil)
      *cand-prev-cor* (make-array *ncandidates* :initial-element nil)
      *cand-weights* (make-array *ncandidates* :initial-element nil)
      *cand-deltas* (make-array *ncandidates* :initial-element nil)
      *cand-slopes* (make-array *ncandidates* :initial-element nil)
      *cand-prev-slopes* (make-array *ncandidates* :initial-element nil))
  ;; Only create the caches if *USE-CACHE* is on -- may not always have room.
  (when *use-cache*
    (dotimes1 (i *max-cases*)
              (setf (svref *values-cache* i)
                (make-array *max-units*
                            :element-type 'short-float
                            :initial-element 0.0))
              (setf (svref *errors-cache* i)
                (make-array *noutputs*
                            :element-type 'short-float
                            :initial-element 0.0))))
  ;; For each output, create the vectors holding per-weight information.
  (dotimes1 (i *noutputs*)
            (setf (svref *output-weights* i)
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *output-deltas* i)    
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *output-slopes* i)
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *output-prev-slopes* i)
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0)))
  ;; For each candidate unit, create the vectors holding the correlations,
  ;; incoming weights, and other stats.
  (dotimes1 (i *ncandidates*)
            (setf (svref *cand-cor* i)
              (make-array *noutputs*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *cand-prev-cor* i)
              (make-array *noutputs*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *cand-weights* i)
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *cand-deltas* i)
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *cand-slopes* i)
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *cand-prev-slopes* i)
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0))))

(defun train (outlimit inlimit rounds &optional (last-epoch 999) (restart nil))
  "(outlimit inlimit rounds &optional (last-epoch 999) (restart nil))
Train the output weights until stagnation or victory is reached.  Then
train the input weights to stagnation or victory.  Then install the best
candidate unit and repeat.  OUTLIMIT and INLIMIT are upper limits on the number
of cycles in each output and input phase.  ROUNDS is an upper limit on
the number of unit-installation cycles.  If RESTART is non-nil, we are
restarting training from the current point -- do not reinitialize the net.
Modified to initial *train-errors*, *test-errors*, *multi-errors*, 
*proportion-train-correct*, *proportion-test-correct*, *proportion-multi-correct*, 
*train-contributions*, *test-contributions*, *train-activations*, *test-activations*, 
*multi-test-activations*, *nets-info*, & *hintons* to nil, check for *training-inputs* 
and *training-outputs*, not call test-epoch directly unless *test-last-output-phase-epoch*, 
mark hidden unit installations in error records, record hidden activations, record all 
activations, stop after last epoch, & stop when learning stagnates."
  (declare (fixnum outlimit inlimit rounds))
  (if (zerop (length *training-inputs*))
      (error "Error -- no *training-inputs* in neighborhood. Call set-patterns before train."))
  (if (zerop (length *training-outputs*))
      (error "Error -- no *training-outputs* in neighborhood. Call set-patterns before train."))
  (unless restart (init-net))
  (list-parameters)
  (setq *train-errors* nil
      *test-errors* nil
      *multi-errors* nil
      *proportion-train-correct* nil
      *proportion-test-correct* nil
      *proportion-multi-correct* nil
      *train-contributions* nil
      *train-activations* nil
      *test-activations* nil
      *multi-test-activations* nil
      *test-contributions* nil
      *nets-info* nil
      *hintons* nil
      *hidden-first-time* t
      *last-true-error* nil
      *quit-counter* 0)
  (when *use-cache*
    (dotimes1 (i *max-cases*)
              (setq *values* (svref *values-cache* i))
              (set-up-inputs (svref *training-inputs* i))))
  (dotimes1 (r rounds  :lose)
            (case (train-outputs outlimit last-epoch)
              (:stop
               (format t "Stop after last epoch ~S . ~S units, ~S hidden, Error ~S. ~% ~%"
                 (1- *epoch*) *nunits* (- *nunits* *ninputs* 1) *true-error*)
               (return "stop"))
              (:win
               (list-parameters)
               (format t "Victory at ~S epochs, ~S units, ~S hidden, Error ~S.~%"
                 *epoch* *nunits* (- *nunits* *ninputs* 1) *true-error*)
               (return "win"))
              (:timeout
               (format t "Epoch ~D: Out Timeout  ~D bits wrong, error ~S.~2%"
                 *epoch* *error-bits* *true-error*))
              (:stagnant
               (format t "Epoch ~D: Out Stagnant ~D bits wrong, error ~S.~2%"
                 *epoch* *error-bits* *true-error*)))
            (if *mark-hiddens-errors* (mark-hiddens-errors))
            ;;;         Changes found from here...
            (cond 
             (*hidden-first-time*
              (setq *hidden-first-time* nil))
             ((> (abs (- *true-error* *last-true-error*))
                 (* *last-true-error* *learning-threshold*))
              (setq *quit-counter* 0))
             ((>= *quit-counter* *learning-patience*)
              (format t "Patience exhausted. Cease learning.~%")
              (record-output-weights)
              (format t "Stop after epoch ~S. ~S units, ~S hidden, Error ~S. ~%"
                *epoch* *nunits* (- *nunits* *ninputs* 1) *true-error*)
              (return "quit"))
             (t
              (format t "No progress. Increment quit-counter.~%")
              (setq *quit-counter* (1+ *quit-counter*))))
            (setq *last-true-error* *true-error*)
            ;;;         ...to here to stop stagnant learning
            (if *test-last-output-phase-epoch* (test-epoch *score-threshold*))
            (case (train-inputs inlimit last-epoch)
              (:stop
               (format t "~% Stop after last epoch. ~% ~%")
               (return "stop"))
              (:timeout
               (format t "Epoch ~D: In Timeout.  Cor: ~D~%"
                 *epoch* *best-candidate-score*))
              (:stagnant
               (format t "Epoch ~D: In Stagnant.  Cor: ~D~%"
                 *epoch* *best-candidate-score*)))
            (install-new-unit)))
