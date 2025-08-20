#|
Evacuation model v0

2/11/2025

- separate initialization period from actual trials

v0.1

2/18/2025:

- add model version variable and print it when loading
- rename make-decision as model-free-decision and add model-based-decision that first generates landing and damage before projecting outcome
  then add decision parameter to simulate function with model-free-decision as default
  also change landing from yes/no to 1.0/0.0 to generate probability in model-based decision and also create and adjust utility function accordingly

v 0.2

3/20/2025:

- add function protocol-output that return values from simulate function in format supported by protocol (i.e. plist with keywords)
- add function probabilize that transforms no/yes values into [0.0, .. , 1.0] probabilities

v 0.3

4/8/2025:

- add function translate-parameter-list to translate parameter list from JSON format to internal format
- add function run-model that calls simulate with parameters and uses protocol-output to return results
- update evacuate and simulate functions to take in global parameteers like thresholds as arguments
- update model-free-decision and model-based-decision to take delay argument

v 0.3.1

4/14/2025:

- correct intensity-similarity function to check for special case of 1-1 (and identity in general)

v 0.3.2

4/14/2025:

- correct intensity-similarity function to log of ratio rather than ratio of logs

v 0.3.3

4/16/2025:

- add averaging of utility values to evaluate function
- add run-batch function to collect statistics
- add averaging of accuracy values to evaluate function

v 0.3.4

8/19/2025:

- add model name to run-model function
- add *models* variable holding list of implemented models

|#

(defparameter *evacuation-model-version* "0.3.4")

(format t "Loading evacuation model version ~S~%" *evacuation-model-version*)

;;; model

(defparameter *delay* 1.0)

(defun get-attribute (attribute attributes)
  (second (assoc attribute attributes)))

(defun intensity-similarity (a b)
  "Log ratio. From 0 to minus infinity so not limited to fixed interval."
  (log (/ (min a b) (max a b))))

(defun probability-similarity (pa pb)
  "Linear differnece in [0,1] interval."
  (- (min pa pb) (max pa pb)))

(defun number-similarity (x y)
  "Assumes payoffs are integers and probabilities are real values."
  (when (and (numberp x) (numberp y))
    (if (or (integerp x) (integerp y)) ;;; if at least one of the numbers is an integer assume they are intensity
        (intensity-similarity (1+ x) (1+ y)) ;;; shift scale from [0,5] to [1,6]
        (probability-similarity x y))))

(defun init-model (&optional (parameters nil))
  (init-memory)
  (init-similarities)
  (setf *similarity-hook-function* 'number-similarity)
  (similarity 'yes 'no -1.0)
  (similarity 'no 'yes -1.0)
  (dolist (parameter parameters)
    (parameter (first parameter) (second parameter))))

#|
(defvar *goal* nil)

(defmacro goal-slot (attribute)
  `(second (assoc ,attribute *goal*)))
|#

(defun model-free-decision (stimulus &key (delay *delay*) (trace t))
  "Make decision based on stimulus by directly projecting outcome (utility) for each action and choosing the best."
  (actr-time delay)
  (let* ((evacuate-outcome (blend (append stimulus (list (list 'decision 'yes))) 'outcome))
         (stay-outcome (blend (append stimulus (list (list 'decision 'no))) 'outcome))
         (decision (if (> evacuate-outcome stay-outcome) 'yes 'no)))
    (when trace (format t "EVACUATE: ~6,3F~CSTAY: ~6,3F~CDECISION: ~S~%" evacuate-outcome #\tab stay-outcome #\tab decision))
    decision))

(defun model-based-decision (stimulus &key (delay *delay*) (trace t))
  "Make decision based on stimulus by first projecting future state (landing and damage) then projecting outcome (utility) for each action and choosing the best."
  (actr-time delay)
  (let* ((landing (blend stimulus 'landing)) ;;; generate expected landing probability
         (damage (blend stimulus 'damage)) ;;; generate expected damage
         (expanded-stimulus (append stimulus (list (list 'landing landing) (list 'damage damage)))) ;;; generate expanded stimulus with expected future state
         (evacuate-outcome (blend (append expanded-stimulus (list (list 'decision 'yes))) 'outcome))
         (stay-outcome (blend (append expanded-stimulus (list (list 'decision 'no))) 'outcome))
         (decision (if (> evacuate-outcome stay-outcome) 'yes 'no)))
    (when trace (format t "EVACUATE: ~6,3F~CSTAY: ~6,3F~CDECISION: ~S~%" evacuate-outcome #\tab stay-outcome #\tab decision))
    decision))

(defun decision-utility (decision landing damage)
  "Generate utility of decision given landing and damage outcomes."
  (if (eq decision 'yes)         ;;; if evacuating
      (if (> landing 0.5) damage (- damage 5)) ;;; then utility is proportional to damage avoided, factoring the cost of evacuation (5) is not landing
      (if (> landing 0.5) (- damage) (- 5 damage)))) ;;; otherwise utility is negative damage NOT avoided, factoring savings from evacuation (5)

(defun learn-instance (instance)
  "Learn complete instance."
  (let* ((decision (get-attribute 'decision instance))
         (landing (get-attribute 'landing instance))
         (damage (get-attribute 'damage instance))
         (outcome (decision-utility decision landing damage))) ;;; move utility computation to separate function
    (setf instance (append instance (list (list 'outcome outcome))))
    (learn instance)
    instance))

;;; simulation

(defparameter *probability-threshold* 0.25)

(defparameter *intensity-threshold* 1)

(defparameter *damage-noise* 1.0)

(defparameter *initial-trials* 10)

(defun random-decision (stimulus)
  "Decides randomly whether to evacuate or not."
  (declare (ignore stimulus))
  (nth (random 2) '(yes no)))

(defun evacuate (probability intensity &key (probability-threshold *probability-threshold*) (intensity-threshold *intensity-threshold*))
  "determine whether to evacuate based on probability and intensity of landing at location."
  (if (and (>= probability probability-threshold)
           (>= intensity intensity-threshold))
      'yes 'no))

(defun simulate (&key (runs 1) (length 100) (init *initial-trials*) (delay *delay*)
                      (probability-threshold *probability-threshold*) (intensity-threshold *intensity-threshold*) (damage-noise *damage-noise*)
                      (parameters nil) (evacuation 'evacuate) (decision 'model-free-decision) (trace nil)) ;;; add decision parameter
  "Simulate a run of evacuation of length with parameters and history chunks.
   Assume uniform distribution of probability over [0,1] and intensity over [1,5]."
  (let ((results nil))
    (dotimes (run runs)
      (let ((answers nil))
        (init-model parameters)
        (dotimes (trial (+ init length)) ;;; separate init and actual trials
          (let* ((probability (random 1.0))
                 (intensity (random 6)) ;;; 0 is a tropical storm, 1-5 is the usual hurricane intensity
                 (evacuation (funcall evacuation probability intensity :probability-threshold probability-threshold :intensity-threshold intensity-threshold)) ;;; apply evacuation rule
                 (stimulus (list (list 'probability probability)
                                 (list 'intensity intensity)
                                 (list 'evacuation evacuation)))
                 (decision (if (< trial init) (random-decision stimulus) (funcall decision stimulus :delay delay :trace trace))) ;;; call model to make decision after initial random trials
                 (landing (if (< (random 1.0) probability) 1.0 0.0)) ;;; assume probability is true estimate but represent outcome in probability terms as well to generate estimate
                 (damage (max 0 (min 5 (round (+ intensity (noise damage-noise)))))) ;;; assume normal noise in intensity
                 (instance (append stimulus 
                                   (list (list 'decision decision)
                                         (list 'landing landing)
                                         (list 'damage damage))))
                 )
            (setf instance (learn-instance instance)) ;;; call model to determine outcome and learn instance
            (when (>= trial init) (push instance answers)))) ;;; only save actual trials not inits
        (push (reverse answers) results)))
    (reverse results)))

(defun evaluate (results)
  (let* ((runs (length results))
         (trials (length (first results)))
         (evacuations (make-array trials :initial-element 0))
         (agreements (make-array trials :initial-element 0))
         (utilities (make-array trials :initial-element 0))
         (accuracies (make-array trials :initial-element 0)))
    (dolist (answers results)
      (let ((trial 0))
        (dolist (answer answers)
          (incf (svref evacuations trial)
                (if (equal (get-attribute 'decision answer)
                           'yes)
                    1 0))
          (incf (svref agreements trial)
                (if (equal (get-attribute 'evacuation answer)
                           (get-attribute 'decision answer))
                    1 0))
          (incf (svref utilities trial) (get-attribute 'outcome answer))
          (incf (svref accuracies trial)
                (if (if (equal (get-attribute 'decision answer) 'yes)
                        (> (get-attribute 'landing answer) 0.5)
                        (< (get-attribute 'landing answer) 0.5))
                    1 0))
          (incf trial))))
    (values (float (/ (reduce #'+ evacuations) runs trials))
            (float (/ (reduce #'+ agreements) runs trials))
            (float (/ (reduce #'+ utilities) runs trials))
            (float (/ (reduce #'+ accuracies) runs trials))
            evacuations agreements)))

(defun run-batch (&key (samples 100) (decision 'model-free-decision) (probability-thresholds '(0.0 0.1 0.25 0.35 0.5)) (intensity-thresholds '(0 1 2 3)))
  "Runs a batch of samples with model-free-decision sweeping probability and utility thresholds."
  (let ((probability-threshold *probability-threshold*)
        (intensity-threshold *intensity-threshold*))
    (format t "THRESHOLD ~C EVACUATION ~C COMPLIANCE ~C UTILITY ~C ACCURACY~%" #\tab #\tab #\tab #\tab)
    (dolist (threshold probability-thresholds)
      (setf *probability-threshold* threshold)
      (format t "~10,3F" threshold)
      (multiple-value-bind (evacuation compliance utility accuracy)
                           (evaluate (simulate :runs samples :decision decision))
        (format t "~C~10,3F~C~10,3F~C~10,3F~C~10,3F~%" #\tab evacuation #\tab compliance #\tab utility #\tab accuracy)))
    (setf *probability-threshold* probability-threshold)
    (dolist (threshold intensity-thresholds)
      (setf *intensity-threshold* threshold)
      (format t "~10D" threshold)
      (multiple-value-bind (evacuation compliance utility accuracy)
                           (evaluate (simulate :runs samples :decision decision))
        (format t "~C~10,3F~C~10,3F~C~10,3F~C~10,3F~%" #\tab evacuation #\tab compliance #\tab utility #\tab accuracy)))
    (setf *intensity-threshold* intensity-threshold)))

;;; API

(defun probabilize (answer &key (positive '(yes evacuate)))
  "Transform binary yes/no answer into probability."
  (cond ((numberp answer) answer)
        ((member answer positive) 1.0)
        (t 0.0)))

(defun protocol-output (results)
  "Output results of simulate function according to JSON protocol."
  (let* ((runs (length results))
         (trials (length (first results)))
         (array-plists (make-array (list runs trials) :initial-element nil)))
    (dotimes (run runs)
      (dotimes (trial trials)
          (let ((plists nil)
                (instance (nth trial (nth run results))))
            (push (list :decision
                        :value (probabilize (get-attribute 'decision instance))
                        :unit-of-measure :double
                        :output-class "observable"
                        :output-sub-class "action") plists)
            (push (list :evacuation-message
                        :value (probabilize (get-attribute 'evacuation instance))
                        :unit-of-measure :double
                        :output-class "observable"
                        :output-sub-class "context") plists)
            (push (list :probability
                        :value (probabilize (get-attribute 'probability instance))
                        :unit-of-measure :double
                        :output-class "observable"
                        :output-sub-class "context") plists)
            (push (list :intensity
                        :value (probabilize (get-attribute 'intensity instance))
                        :unit-of-measure :double
                        :output-class "observable"
                        :output-sub-class "context") plists)
            (push (list :landing
                        :value (probabilize (get-attribute 'landing instance))
                        :unit-of-measure :double
                        :output-class "observable"
                        :output-sub-class "outcome") plists)
            (push (list :damage
                        :value (probabilize (get-attribute 'damage instance))
                        :unit-of-measure :double
                        :output-class "observable"
                        :output-sub-class "outcome") plists)
            (push (list :utility
                        :value (probabilize (get-attribute 'outcome instance))
                        :unit-of-measure :double
                        :output-class "internal"
                        :output-sub-class "utility") plists)
            (setf (aref array-plists run trial) (reverse plists)))))
    array-plists))

(defun translate-parameter-list (parameter-list parameter-mappings &key (flatten nil))
  "Translate parameter list from JSON format to internal format."
  (let ((parameters nil))
    (dolist (parameter parameter-mappings parameters)
      (let ((value (assoc (first parameter) parameter-list)))
        (when value
          (let ((pair (list (second parameter) (getf (rest value) :VALUE))))
            (if flatten (setf parameters (append pair parameters))
              (push pair parameters))))))))

(defparameter *models* '(model-free-decision model-based-decision))
    
(defun run-model (parameters raw-data &key (model-parameters '((:noise :ans) (:temperature :tmp)))
                             (simulation-parameters '((:run-count :runs) (:run-length :length) (:init-length :init) (:run-delay :delay)
                                                      (:probability-threshold :probability-threshold)
                                                      (:intensity-threshold :intensity-threshold)
                                                      (:intensity-standard-deviation :damage-noise))))
  "Applies parameters then calls simulation then protocol-output to return results."
  (declare (ignore raw-data)) ;;; not currently using raw data
  (let ((translated-parameters (append (list (translate-parameter-list parameters model-parameters))
                                       (translate-parameter-list parameters simulation-parameters :flatten t))))
    (protocol-output 
     (if (eq (first (first parameters)) ':name)
         (when (member (second (first parameters)) *models* :test 'eq) ;;; check model name
           (apply 'simulate :decision (second (first parameters)) :parameters translated-parameters))
         (apply 'simulate :parameters translated-parameters)))))

#|
? (evaluate (simulate :length 100 :init 10))
EVACUATE: -0.764	STAY: -0.724	DECISION: NO
EVACUATE:  0.384	STAY:  0.567	DECISION: NO
EVACUATE:  0.079	STAY:  0.555	DECISION: NO
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  0.388	STAY: -0.010	DECISION: YES
EVACUATE: -0.566	STAY: -0.296	DECISION: NO
EVACUATE: -1.026	STAY: -0.209	DECISION: NO
EVACUATE: -0.627	STAY: -0.783	DECISION: YES
EVACUATE: -1.057	STAY: -0.340	DECISION: NO
EVACUATE: -0.457	STAY: -0.518	DECISION: YES
EVACUATE: -0.381	STAY: -0.778	DECISION: YES
EVACUATE: -0.199	STAY: -0.341	DECISION: YES
EVACUATE:  1.031	STAY:  0.343	DECISION: YES
EVACUATE:  0.880	STAY:  0.370	DECISION: YES
EVACUATE:  2.443	STAY:  1.253	DECISION: YES
EVACUATE:  0.497	STAY:  0.450	DECISION: YES
EVACUATE: -0.441	STAY:  0.010	DECISION: NO
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  0.301	STAY:  1.163	DECISION: NO
EVACUATE:  1.621	STAY:  0.791	DECISION: YES
EVACUATE:  1.371	STAY:  0.414	DECISION: YES
EVACUATE:  0.672	STAY:  0.222	DECISION: YES
EVACUATE:  1.774	STAY:  0.021	DECISION: YES
EVACUATE:  1.860	STAY:  1.279	DECISION: YES
EVACUATE:  1.354	STAY:  0.898	DECISION: YES
EVACUATE:  2.209	STAY:  1.020	DECISION: YES
EVACUATE:  2.905	STAY:  1.660	DECISION: YES
EVACUATE:  1.688	STAY:  0.681	DECISION: YES
EVACUATE:  0.680	STAY: -0.549	DECISION: YES
EVACUATE:  1.917	STAY:  1.297	DECISION: YES
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  1.419	STAY:  0.328	DECISION: YES
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  0.378	STAY:  0.454	DECISION: NO
EVACUATE:  1.045	STAY:  0.424	DECISION: YES
EVACUATE:  0.125	STAY: -0.014	DECISION: YES
EVACUATE:  0.796	STAY:  0.195	DECISION: YES
EVACUATE:  1.691	STAY:  0.813	DECISION: YES
EVACUATE:  0.278	STAY:  0.275	DECISION: YES
EVACUATE:  1.528	STAY:  0.556	DECISION: YES
EVACUATE:  0.366	STAY:  0.134	DECISION: YES
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  0.675	STAY:  0.319	DECISION: YES
EVACUATE: -0.219	STAY: -0.030	DECISION: NO
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  0.602	STAY:  0.284	DECISION: YES
EVACUATE:  0.437	STAY: -0.281	DECISION: YES
EVACUATE:  1.581	STAY:  0.629	DECISION: YES
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  1.425	STAY:  0.226	DECISION: YES
EVACUATE:  0.344	STAY: -0.034	DECISION: YES
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE: -0.297	STAY:  0.773	DECISION: NO
EVACUATE:  0.541	STAY:  0.681	DECISION: NO
EVACUATE:  0.325	STAY: -0.127	DECISION: YES
EVACUATE:  0.512	STAY: -0.274	DECISION: YES
EVACUATE:  0.078	STAY:  0.201	DECISION: NO
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  0.789	STAY:  0.138	DECISION: YES
EVACUATE: -0.606	STAY: -0.184	DECISION: NO
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  0.432	STAY:  0.731	DECISION: NO
EVACUATE:  0.656	STAY:  0.591	DECISION: YES
EVACUATE:  0.793	STAY:  0.840	DECISION: NO
EVACUATE:  0.770	STAY:  0.336	DECISION: YES
EVACUATE:  1.226	STAY:  0.805	DECISION: YES
EVACUATE:  0.240	STAY:  0.916	DECISION: NO
EVACUATE:  1.166	STAY:  0.934	DECISION: YES
EVACUATE:  1.189	STAY:  0.759	DECISION: YES
EVACUATE:  1.615	STAY:  1.449	DECISION: YES
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  0.991	STAY:  0.893	DECISION: YES
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
EVACUATE:  1.202	STAY:  0.690	DECISION: YES
EVACUATE:  0.628	STAY:  0.901	DECISION: NO
EVACUATE:  0.842	STAY:  1.014	DECISION: NO
EVACUATE:  1.140	STAY:  1.204	DECISION: NO
EVACUATE:  0.998	STAY:  0.066	DECISION: YES
EVACUATE:  0.753	STAY:  0.360	DECISION: YES
EVACUATE:  0.850	STAY:  1.015	DECISION: NO
EVACUATE:  0.905	STAY: -0.131	DECISION: YES
EVACUATE:  0.311	STAY:  0.802	DECISION: NO
EVACUATE:  -0.000	STAY:  -0.000	DECISION: NO
52
81
(0 0 1 0 0 1 1 0 1 0 0 0 0 0 1 0 0 1 0 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 1 1 1 1 1 1 1 0 1 0 0 1 1 1 0 0 0 1 1 0 0 0 0 0 1 1 0 0 0 0 1 0 0 0 1 0 1 1 0 1 1 1 0 1 0 1 0 0 0 1 1 0 1 0 0)
(1 1 1 1 1 1 1 0 1 0 0 0 0 1 0 1 0 0 0 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1)
? 
(evaluate (simulate :length 100 :init 10))
62
79
(1 1 0 0 0 1 0 1 1 0 1 0 1 1 1 1 1 1 0 1 0 1 1 0 1 1 0 1 0 1 0 1 0 0 0 0 1 0 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 0 1 1 1 0 0 1 0 0 0 0 1 0 1 1 1 1 1 0 1 1 0 0 1 1 1 1 1 0 1 1 0 0 0 0 1 1 1 1 1 1 1 1 0 1 0)
(1 1 0 0 0 1 0 1 0 0 1 1 1 1 1 1 1 1 1 0 0 1 0 1 1 1 0 1 1 1 0 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 0 1 0)
? (evaluate (simulate :length 100 :init 10))
47
80
(1 0 1 0 0 0 0 0 0 0 0 1 1 1 1 1 0 1 1 1 1 0 1 1 0 1 1 1 0 0 1 0 1 1 1 1 0 1 1 0 1 0 0 0 0 0 1 0 1 0 1 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 1 0 0 1 0 1 1 1 0 0 1 1 1 0 0 0 1 0 1 1 0 0 0 1 1 1 1 0 0 1 0 1 1)
(1 1 1 1 0 0 1 0 1 0 1 0 1 1 0 1 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 0 0 0 0 1 1 1 1 1 0 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1)
? *intensity-threshold*
1
? (setf *intensity-threshold* 3)
3
? (evaluate (simulate :length 100 :init 10))
48
67
(1 0 0 0 0 0 1 1 1 1 0 1 0 1 0 0 1 1 0 0 0 0 0 1 0 0 0 1 0 1 0 0 0 0 0 0 0 0 1 1 1 1 0 0 1 1 0 1 0 0 0 1 0 1 0 0 0 1 1 1 0 1 0 1 1 0 1 0 1 0 0 1 0 0 1 1 1 1 0 1 1 1 0 1 1 0 1 1 0 1 1 0 0 0 1 1 1 1 0 1)
(0 1 1 1 0 1 0 0 0 0 1 0 1 0 1 1 0 1 1 1 0 1 0 0 1 1 0 1 1 0 1 0 1 1 0 1 1 0 1 0 1 0 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 1 0 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 0 0 0 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1)
? (evaluate (simulate :length 100 :init 10))
51
63
(0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 1 1 0 1 1 0 1 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 0 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 1 1 0 1)
(0 0 1 1 1 1 0 1 0 1 1 0 1 1 1 1 1 1 0 1 0 0 1 0 0 1 1 1 0 1 1 0 1 1 1 1 0 1 0 1 0 1 0 0 1 1 1 1 0 1 1 1 0 0 1 1 1 0 0 1 1 1 0 1 0 1 0 0 1 1 0 1 0 0 0 1 1 0 1 1 1 1 1 0 0 1 1 1 1 1 0 1 1 1 0 0 1 1 1 0)
? (setf *intensity-threshold* 1)
1
? *probability-threshold*
0.25
? (setf *probability-threshold* 0.5)
0.5
? (evaluate (simulate :length 100 :init 10))
61
76
(1 0 1 0 0 0 0 1 0 0 0 0 1 0 0 1 0 0 0 0 0 1 1 1 1 0 0 0 1 1 1 0 0 1 0 0 0 1 1 1 1 1 0 1 1 1 0 1 1 1 0 1 0 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 0 1 0 1 0 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 0 1 1 0 1 1 0 1 1 0 1)
(0 0 0 1 1 1 1 1 1 0 0 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 0 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 0 0 1 0 1 1 0 0 0 1 1 0 1 0 1 1 0 1 0 0)
? (evaluate (simulate :length 100 :init 10))
50
84
(0 1 0 1 1 1 1 0 0 1 1 0 0 0 1 1 0 0 0 0 0 1 1 0 0 0 0 1 1 1 0 0 1 1 0 1 1 1 0 1 1 0 1 1 0 1 0 0 1 1 1 1 0 0 0 1 1 0 0 0 1 1 0 1 0 0 0 0 1 1 0 1 1 0 0 1 0 1 0 0 1 0 1 1 1 0 1 1 1 0 0 1 0 0 0 1 0 0 1 1)
(0 1 1 1 1 1 1 1 1 1 0 1 1 1 1 0 1 1 0 1 0 1 0 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0)
? (evaluate (simulate :length 100 :init 10))
48
83
(1 0 0 0 1 1 1 0 1 0 1 1 1 0 1 1 0 0 1 0 0 0 1 0 1 1 0 1 1 0 0 1 1 1 1 1 0 1 1 0 0 0 0 0 1 1 0 1 0 0 0 1 0 0 1 0 0 1 1 0 1 0 1 0 0 0 0 1 1 1 1 1 1 0 1 0 1 0 1 0 1 1 0 0 0 0 0 0 0 1 1 0 0 0 1 1 1 0 0 0)
(0 0 1 1 0 1 0 1 0 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 0 1 0 1 1 1 1 1 1 1 1 0 1 1 1 0 1 1 1 0 1 1 1 1 1 1 1 1 1 1)
? (setf *probability-threshold* 0.75)
0.75
? (evaluate (simulate :length 100 :init 10))
34
80
(1 0 1 1 1 1 1 1 1 0 1 0 0 0 1 0 1 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 1 1 1 1 0 1 0 0 0 0 1 0 1 0 1 0 0 1 0 1 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1)
(0 1 0 0 0 1 0 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 0 0 1 1 0 1 0 1 1 1 1 1 1 0 1 0 1 1 0 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1)
? (evaluate (simulate :length 100 :init 10))
56
62
(1 1 0 0 0 1 0 1 0 0 1 1 0 1 0 0 1 0 0 1 1 0 0 0 1 0 0 1 1 0 1 1 1 0 0 0 0 0 0 1 0 1 1 1 1 0 1 1 1 1 0 1 0 1 1 0 1 0 1 1 1 1 1 1 1 0 0 1 0 0 0 1 0 0 0 1 0 1 1 0 1 0 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 0 1 1)
(1 0 1 1 1 0 1 0 1 0 0 1 1 1 1 1 1 1 1 0 1 1 1 1 0 1 1 1 0 1 0 1 0 1 1 1 1 1 1 0 1 0 0 1 0 1 1 1 0 0 1 0 1 1 0 1 0 1 0 0 1 0 0 1 0 1 1 0 1 1 1 0 1 1 1 0 1 1 0 1 0 1 1 1 1 1 0 1 0 1 0 0 0 0 0 1 0 1 1 0)
? (time (evaluate (simulate :runs 10 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 10 :LENGTH 100 :INIT 10))
took 2,382,834 microseconds (2.382834 seconds) to run.
        13,872 microseconds (0.013872 seconds, 0.58%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     1,148,604 microseconds (1.148604 seconds) were spent in user mode
     1,142,815 microseconds (1.142815 seconds) were spent in system mode
 38,415,472 bytes of memory allocated.
 203,957 minor page faults, 5 major page faults, 0 swaps.
0.474
0.754
#(5 6 6 5 3 4 2 6 5 4 2 3 2 5 6 3 5 5 4 0 3 6 5 5 3 5 4 3 4 6 5 4 5 3 2 5 5 3 5 6 6 3 5 5 4 6 6 3 6 4 6 2 3 5 5 4 3 4 4 5 4 7 6 9 6 5 5 7 4 7 2 8 7 3 6 4 4 7 4 8 8 6 2 7 6 7 4 5 7 5 3 7 6 6 6 4 4 5 3 3)
#(4 4 5 4 4 8 3 2 6 5 5 6 6 9 7 4 8 8 9 7 7 7 7 7 8 9 8 10 7 10 9 7 9 4 9 7 8 8 9 8 6 6 9 9 7 8 8 6 8 8 8 8 9 9 7 5 10 6 5 9 8 6 10 9 10 8 8 7 9 8 7 10 7 7 8 8 9 8 9 10 10 10 8 9 9 8 8 7 9 9 8 9 9 7 9 8 5 7 9 7)
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 23,636,048 microseconds (23.636047 seconds) to run.
        122,376 microseconds ( 0.122376 seconds, 0.52%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     11,879,466 microseconds (11.879466 seconds) were spent in user mode
     11,506,228 microseconds (11.506228 seconds) were spent in system mode
 384,832,960 bytes of memory allocated.
 2,080,640 minor page faults, 135 major page faults, 0 swaps.
0.5232
0.7845
#(59 58 54 44 55 54 68 47 50 44 54 52 44 58 45 56 47 42 58 50 44 49 45 45 56 50 57 53 53 52 45 46 51 48 58 53 49 52 53 56 50 46 45 56 60 54 57 59 63 54 54 54 61 51 54 55 53 52 56 56 50 49 65 55 55 45 53 55 45 52 54 48 51 59 55 48 51 50 54 61 50 47 51 50 58 47 53 49 54 49 47 48 55 52 55 58 50 54 46 60)
#(59 48 48 55 54 55 48 50 52 50 77 75 71 75 70 76 69 76 78 76 71 75 75 67 82 74 83 71 79 75 71 75 80 74 82 80 77 80 80 78 79 79 85 74 85 80 86 88 79 87 81 81 84 83 81 81 79 84 80 84 85 88 81 86 82 88 92 83 82 84 88 80 84 81 90 83 85 89 81 85 82 83 87 84 80 83 92 84 85 83 90 82 85 85 88 92 84 92 86 90)
;;; Separating initialization from actual trials
?  (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 29,128,725 microseconds (29.128725 seconds) to run.
         75,302 microseconds ( 0.075302 seconds, 0.26%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     14,331,820 microseconds (14.331820 seconds) were spent in user mode
     14,260,764 microseconds (14.260764 seconds) were spent in system mode
 465,200,912 bytes of memory allocated.
 2,531,873 minor page faults, 154 major page faults, 0 swaps.
0.5287
0.8157
#(52 49 49 54 54 53 52 43 50 51 52 46 47 54 44 51 55 51 53 50 44 44 50 51 50 51 48 50 53 51 50 51 60 50 57 55 61 67 56 57 55 53 55 53 49 57 55 52 54 45 54 48 59 50 48 50 57 59 56 58 55 45 56 57 49 50 50 51 49 64 55 51 52 58 39 51 60 61 54 53 58 64 49 49 49 57 59 58 53 59 61 49 53 54 55 51 60 55 52 49)
#(72 73 71 85 76 79 73 70 80 79 74 77 77 78 79 81 71 76 73 78 78 69 77 77 77 74 80 71 81 83 77 79 80 79 78 80 87 85 84 81 80 83 79 80 78 87 83 81 83 84 82 86 82 80 80 90 85 82 83 79 84 82 88 90 85 86 87 83 83 83 83 83 89 81 87 84 93 81 89 82 85 84 85 88 85 82 86 90 83 85 90 88 84 85 84 82 84 91 86 81)
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 29,899,207 microseconds (29.899208 seconds) to run.
         64,613 microseconds ( 0.064613 seconds, 0.22%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     15,201,520 microseconds (15.201520 seconds) were spent in user mode
     14,217,830 microseconds (14.217830 seconds) were spent in system mode
 464,933,712 bytes of memory allocated.
 2,506,408 minor page faults, 136 major page faults, 0 swaps.
0.522
0.8176
#(52 56 56 51 51 52 50 52 55 47 55 47 51 53 57 51 50 51 53 57 55 55 45 50 51 53 50 56 48 48 52 58 59 51 53 57 54 53 51 49 50 54 51 52 51 51 56 61 50 42 49 58 53 50 53 55 56 49 46 50 45 50 48 48 57 51 51 41 47 55 46 52 49 47 53 57 53 54 55 52 56 54 59 49 57 54 62 49 56 53 47 52 53 62 48 48 55 56 49 58)
#(64 75 76 79 74 81 73 80 79 75 80 78 87 77 74 78 78 72 81 78 81 84 76 79 80 77 81 78 76 85 83 80 80 79 82 86 78 80 81 76 81 81 85 88 77 85 84 87 82 84 83 84 90 83 77 89 79 85 79 83 79 89 85 82 81 90 84 82 84 84 83 78 76 88 82 87 89 83 90 79 86 88 94 85 86 83 91 77 87 85 84 87 86 85 84 76 85 80 81 84)
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 30,484,286 microseconds (30.484285 seconds) to run.
         73,899 microseconds ( 0.073899 seconds, 0.24%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     15,114,453 microseconds (15.114453 seconds) were spent in user mode
     14,622,675 microseconds (14.622675 seconds) were spent in system mode
 465,603,152 bytes of memory allocated.
 2,485,415 minor page faults, 14 major page faults, 0 swaps.
0.5176
0.8151
#(44 47 44 51 46 51 52 55 53 43 49 52 42 46 49 56 47 49 49 49 49 54 43 51 49 48 48 52 53 53 50 65 53 53 63 55 52 52 59 48 52 48 50 63 58 55 50 48 50 52 46 55 50 51 47 57 47 51 51 49 44 66 53 53 57 58 53 44 54 52 50 53 55 58 55 50 44 69 49 59 43 44 47 57 51 55 53 45 54 53 57 63 60 48 55 47 53 60 45 56)
#(68 73 75 73 64 72 77 76 80 79 72 76 78 74 79 75 80 82 78 80 89 84 77 73 78 83 79 85 87 87 74 83 81 87 80 76 80 80 85 83 82 85 84 83 90 85 81 89 74 82 83 80 74 82 83 80 84 81 84 77 85 85 85 92 84 86 84 83 89 84 81 83 80 81 84 80 76 92 81 84 79 76 81 81 86 86 87 83 84 81 84 92 88 88 85 88 82 88 79 89)
? *probability-threshold*
0.25
? (setf *probability-threshold* 0.1)
0.1
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 35,010,058 microseconds (35.010056 seconds) to run.
         99,716 microseconds ( 0.099716 seconds, 0.28%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     17,859,992 microseconds (17.859991 seconds) were spent in user mode
     17,095,686 microseconds (17.095686 seconds) were spent in system mode
 455,591,216 bytes of memory allocated.
 2,467,870 minor page faults, 0 major page faults, 0 swaps.
0.5257
0.7427
#(50 43 45 40 45 47 42 42 47 52 49 53 50 52 46 58 40 45 42 53 59 60 50 54 56 46 63 49 54 57 62 50 54 61 50 59 58 63 59 54 46 53 53 51 58 52 55 52 56 53 50 54 46 58 58 56 50 54 54 56 47 52 48 52 59 57 52 53 52 53 58 52 43 46 55 60 51 59 59 56 47 56 48 48 52 60 56 57 48 53 57 53 61 56 50 56 55 48 51 57)
#(66 62 61 63 62 58 67 68 74 70 62 69 69 70 66 69 72 63 75 68 72 79 67 73 74 69 75 69 77 84 72 75 70 72 75 81 82 76 78 76 71 70 81 75 79 74 78 73 77 73 76 73 73 80 76 71 76 75 73 81 72 80 71 80 79 77 79 76 77 83 77 75 74 76 77 78 77 79 86 83 70 78 72 68 77 76 81 77 74 74 80 78 82 82 76 79 81 80 75 76)
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 35,291,193 microseconds (35.291190 seconds) to run.
         98,707 microseconds ( 0.098707 seconds, 0.28%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     17,975,456 microseconds (17.975456 seconds) were spent in user mode
     17,245,213 microseconds (17.245213 seconds) were spent in system mode
 455,510,752 bytes of memory allocated.
 2,478,080 minor page faults, 26 major page faults, 0 swaps.
0.5175
0.7364
#(44 46 40 46 44 52 48 42 44 46 45 44 46 52 46 53 52 52 51 46 49 55 52 55 46 49 51 46 40 46 42 52 50 42 47 45 49 49 42 53 56 50 43 55 51 53 61 52 49 55 54 50 49 54 64 49 54 56 62 58 46 48 53 59 57 56 54 55 55 53 57 57 54 55 52 55 59 59 58 51 64 65 54 57 62 52 54 51 58 52 55 50 52 50 62 48 61 55 54 52)
#(65 65 66 67 59 75 67 66 59 68 69 66 66 69 59 73 71 73 68 68 65 76 70 74 68 66 69 74 65 70 73 68 72 69 74 67 70 72 75 77 81 72 65 72 73 74 80 71 76 77 78 76 71 79 80 76 78 73 79 75 72 74 77 77 77 75 73 78 74 76 78 81 84 78 76 81 79 73 81 80 84 86 76 85 81 72 78 75 75 76 78 81 74 72 84 79 80 77 77 71)
? (setf *probability-threshold* 0.0)
0.0
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 35,095,350 microseconds (35.095350 seconds) to run.
        174,230 microseconds ( 0.174230 seconds, 0.50%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     17,984,096 microseconds (17.984097 seconds) were spent in user mode
     17,289,225 microseconds (17.289225 seconds) were spent in system mode
 444,738,147 bytes of memory allocated.
 2,485,996 minor page faults, 221 major page faults, 0 swaps.
0.5232
0.6887
#(47 45 45 42 50 51 49 40 53 45 54 50 52 52 46 50 63 46 57 50 44 47 53 52 51 53 48 56 50 55 47 58 53 51 51 46 54 51 49 41 50 54 53 48 52 55 51 52 52 57 51 55 58 58 54 58 51 58 49 54 56 52 58 59 62 51 50 57 61 59 47 55 52 55 51 54 59 56 55 50 56 49 52 51 49 54 63 58 58 58 51 45 57 46 58 52 59 44 50 56)
#(54 59 63 63 67 66 70 58 70 59 65 72 66 70 68 68 71 67 71 64 62 67 66 66 64 64 69 77 67 71 70 76 65 69 75 66 66 70 64 58 73 70 75 66 69 64 65 72 69 72 64 71 73 67 74 73 70 72 67 72 70 68 75 73 71 65 67 77 80 74 67 66 68 71 70 74 74 68 71 70 68 72 71 71 67 75 78 73 73 76 64 63 76 61 77 67 72 68 65 70)
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 33,833,874 microseconds (33.833874 seconds) to run.
         87,314 microseconds ( 0.087314 seconds, 0.26%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     16,717,383 microseconds (16.717382 seconds) were spent in user mode
     17,117,266 microseconds (17.117266 seconds) were spent in system mode
 443,387,808 bytes of memory allocated.
 2,476,173 minor page faults, 26 major page faults, 0 swaps.
0.5444
0.7055
#(49 48 52 56 55 56 51 49 52 54 49 60 51 53 59 58 58 56 59 50 53 56 53 53 57 61 57 56 60 64 58 59 56 50 56 47 51 54 57 53 57 49 57 60 56 56 55 55 59 69 50 57 50 50 55 51 55 57 56 56 51 57 47 57 47 59 56 51 58 57 60 56 51 56 61 57 58 58 57 52 50 54 45 54 51 48 50 49 54 51 49 52 62 48 49 56 47 58 58 57)
#(66 63 62 68 69 68 70 76 73 72 66 71 71 70 68 68 67 70 76 70 68 75 71 67 72 77 71 70 74 76 77 70 73 70 72 63 68 74 83 67 71 65 76 75 74 70 67 74 76 81 65 75 73 69 74 70 72 68 66 71 71 69 71 75 63 71 75 72 76 73 78 74 70 63 76 74 72 75 71 72 67 69 62 68 68 62 67 65 71 67 68 68 70 65 72 71 65 73 74 68)
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 33,860,739 microseconds (33.860740 seconds) to run.
         98,675 microseconds ( 0.098675 seconds, 0.29%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     17,026,354 microseconds (17.026354 seconds) were spent in user mode
     17,072,711 microseconds (17.072712 seconds) were spent in system mode
 444,238,336 bytes of memory allocated.
 2,477,856 minor page faults, 129 major page faults, 0 swaps.
0.5109
0.6763
#(43 48 49 48 37 49 51 50 50 46 40 51 51 50 46 42 41 47 45 57 54 49 44 54 53 50 51 47 59 49 52 52 56 56 46 59 53 47 57 54 54 51 49 42 43 53 54 50 46 54 54 47 56 52 57 52 45 55 59 52 55 51 50 58 53 50 58 48 54 55 56 61 47 52 52 58 59 52 52 51 44 56 44 53 51 50 60 53 46 48 58 48 55 51 57 47 54 58 45 51)
#(54 62 61 63 53 63 64 62 63 63 62 73 75 69 60 58 60 62 65 70 74 60 61 71 64 69 68 70 76 70 73 69 73 72 63 74 64 62 71 73 67 70 67 62 61 66 72 74 68 77 67 70 68 69 74 69 60 70 69 64 69 67 68 71 70 73 69 73 75 75 74 75 69 69 70 76 72 69 68 66 65 64 65 68 63 65 71 69 61 64 71 66 74 69 74 62 65 74 64 68)
? (setf *probability-threshold* 0.35)
0.35
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 34,639,612 microseconds (34.639610 seconds) to run.
         99,455 microseconds ( 0.099455 seconds, 0.29%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     17,786,573 microseconds (17.786572 seconds) were spent in user mode
     17,176,926 microseconds (17.176926 seconds) were spent in system mode
 469,999,696 bytes of memory allocated.
 2,474,659 minor page faults, 128 major page faults, 0 swaps.
0.5173
0.8402
#(51 53 43 49 48 48 47 52 48 45 45 54 45 58 48 48 51 49 47 51 52 57 45 59 61 50 39 45 47 50 61 52 46 53 54 50 49 54 55 52 54 57 54 48 59 65 49 53 53 56 51 59 54 57 56 51 51 49 51 55 56 54 51 56 48 52 52 48 44 53 48 52 64 51 47 56 55 61 56 48 57 45 59 48 58 45 51 59 51 51 49 52 53 45 52 39 57 51 52 54)
#(68 70 74 79 78 77 76 67 69 79 82 74 87 81 81 78 85 83 79 81 80 84 85 85 76 78 83 88 78 79 84 83 82 83 84 79 88 78 84 85 75 80 85 85 92 85 87 85 85 91 91 92 85 86 81 87 84 89 83 83 91 91 85 84 86 91 90 84 82 84 89 83 84 85 94 90 82 91 90 92 85 93 86 90 85 89 89 88 87 88 84 86 83 85 83 87 86 92 92 91)
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 34,667,733 microseconds (34.667732 seconds) to run.
        102,234 microseconds ( 0.102234 seconds, 0.29%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     17,491,705 microseconds (17.491705 seconds) were spent in user mode
     17,173,524 microseconds (17.173525 seconds) were spent in system mode
 469,135,888 bytes of memory allocated.
 2,475,255 minor page faults, 26 major page faults, 0 swaps.
0.4955
0.8166
#(53 46 49 41 52 48 50 50 57 52 52 48 42 47 42 46 55 44 51 43 47 43 43 50 44 53 65 46 43 53 52 47 53 53 53 46 48 53 55 45 57 41 40 41 58 51 52 45 49 46 45 56 56 50 56 49 59 56 44 49 53 52 48 47 52 53 57 48 52 42 55 48 50 48 52 52 48 47 48 53 41 48 53 48 51 59 38 47 46 47 57 52 49 47 49 55 42 49 57 55)
#(71 69 74 78 79 79 82 67 79 77 81 79 82 76 78 78 82 69 77 77 81 77 79 70 81 73 86 82 82 79 84 88 82 72 79 82 83 80 86 78 81 84 87 84 88 81 78 87 89 87 78 83 84 81 87 82 85 87 86 78 83 83 83 79 74 84 86 87 81 84 86 79 85 86 78 86 82 88 83 77 89 86 84 82 86 81 87 84 82 87 85 84 84 84 86 84 85 83 87 87)
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 34,881,643 microseconds (34.881645 seconds) to run.
        103,380 microseconds ( 0.103380 seconds, 0.30%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     17,434,379 microseconds (17.434380 seconds) were spent in user mode
     17,447,464 microseconds (17.447464 seconds) were spent in system mode
 469,284,208 bytes of memory allocated.
 2,474,169 minor page faults, 0 major page faults, 0 swaps.
0.508
0.83
#(48 47 51 46 44 50 57 49 51 46 53 47 45 64 52 47 52 53 51 58 51 45 48 51 45 51 48 57 48 40 50 50 52 50 48 49 46 58 45 56 51 48 51 52 54 57 52 50 61 55 62 54 50 48 59 66 50 54 55 51 58 53 53 47 45 55 47 55 52 55 56 53 52 47 53 52 52 51 50 53 48 48 44 57 44 47 38 48 47 47 46 48 51 54 47 49 51 48 50 50)
#(66 72 75 74 80 73 75 71 78 89 80 77 79 79 77 83 83 73 81 85 84 83 81 83 88 86 86 79 84 79 84 85 83 79 88 87 81 84 82 84 75 84 85 91 82 86 84 85 89 86 85 77 82 84 85 82 80 86 83 86 85 84 84 87 81 81 79 85 85 89 80 80 90 84 84 86 88 84 89 87 81 86 88 87 86 88 80 88 80 88 87 92 89 85 86 82 84 81 85 88)
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 34,649,112 microseconds (34.649113 seconds) to run.
        104,250 microseconds ( 0.104250 seconds, 0.30%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     17,557,810 microseconds (17.557810 seconds) were spent in user mode
     17,369,725 microseconds (17.369724 seconds) were spent in system mode
 468,160,336 bytes of memory allocated.
 2,470,727 minor page faults, 2 major page faults, 0 swaps.
0.4957
0.8462
#(47 46 52 45 53 40 48 54 65 43 54 47 47 49 45 55 52 53 49 46 51 49 46 59 45 49 54 54 55 52 49 50 50 52 49 51 52 48 47 54 58 61 45 49 44 52 43 49 60 46 49 48 51 52 57 41 50 51 50 43 52 52 46 50 44 47 47 45 51 47 54 53 46 46 49 58 49 46 50 46 46 49 43 48 50 49 53 42 50 47 44 48 53 42 53 54 48 53 55 47)
#(76 77 80 80 80 74 81 75 81 84 80 86 81 83 87 82 81 78 82 69 80 81 86 86 82 79 89 83 79 89 83 85 81 83 84 87 81 83 89 78 82 85 82 85 86 87 78 84 88 85 84 85 88 89 91 90 87 89 85 81 84 87 89 93 85 82 90 88 90 87 79 76 91 85 90 88 89 90 92 90 86 87 86 88 79 92 90 86 91 82 83 86 84 89 86 88 91 91 89 87)
? (setf *probability-threshold* 0.5)
0.5
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 30,642,236 microseconds (30.642237 seconds) to run.
         76,193 microseconds ( 0.076193 seconds, 0.25%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     15,207,966 microseconds (15.207966 seconds) were spent in user mode
     14,711,358 microseconds (14.711358 seconds) were spent in system mode
 468,095,072 bytes of memory allocated.
 2,519,136 minor page faults, 0 major page faults, 0 swaps.
0.4853
0.8198
#(45 44 41 48 50 41 48 54 42 49 49 47 38 46 47 39 54 40 44 50 45 52 56 53 49 46 48 56 49 52 42 48 61 61 55 51 43 48 47 51 45 50 45 56 45 41 42 60 48 44 49 40 52 48 55 39 58 47 46 52 51 46 52 46 59 44 50 48 48 50 42 51 34 51 51 43 52 62 47 41 48 51 54 52 51 51 49 53 48 49 46 45 55 50 49 47 50 46 49 51)
#(68 70 74 80 75 82 79 70 75 71 84 72 75 83 82 82 78 81 79 84 74 84 78 76 82 83 80 76 89 79 84 84 74 77 75 84 73 76 82 82 81 86 82 82 78 85 86 79 81 83 77 85 87 83 84 88 77 85 79 87 85 83 89 88 79 86 87 80 91 90 87 85 90 90 85 87 87 86 86 86 83 82 84 84 79 81 81 87 84 82 88 89 80 82 90 84 86 83 90 81)
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 35,950,405 microseconds (35.950405 seconds) to run.
         91,371 microseconds ( 0.091371 seconds, 0.25%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     18,500,943 microseconds (18.500944 seconds) were spent in user mode
     17,372,259 microseconds (17.372260 seconds) were spent in system mode
 468,095,952 bytes of memory allocated.
 2,530,804 minor page faults, 43 major page faults, 0 swaps.
0.4792
0.8207
#(46 50 47 51 44 47 44 47 49 44 48 48 55 49 51 47 46 49 47 47 50 56 46 52 52 51 48 46 51 45 48 53 53 54 57 44 48 51 46 50 45 46 37 48 49 47 51 51 51 44 49 46 54 53 46 47 41 37 47 36 44 49 37 47 60 46 50 47 47 42 44 40 50 45 47 56 51 43 57 51 49 52 49 47 50 49 47 55 52 44 46 56 44 45 46 47 44 51 42 42)
#(64 75 73 75 79 75 74 76 76 76 79 71 75 86 78 77 82 70 81 79 87 79 79 79 78 80 79 81 83 87 82 80 80 84 74 84 88 79 78 86 79 81 86 78 77 78 86 86 87 84 78 78 87 80 81 74 83 88 77 90 81 86 86 88 81 89 85 85 83 85 95 87 87 85 80 81 78 94 86 86 84 79 86 87 84 86 88 84 86 89 83 85 85 80 84 88 87 90 91 87)
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 34,632,579 microseconds (34.632580 seconds) to run.
        110,736 microseconds ( 0.110736 seconds, 0.32%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     17,252,579 microseconds (17.252580 seconds) were spent in user mode
     17,342,581 microseconds (17.342580 seconds) were spent in system mode
 468,146,592 bytes of memory allocated.
 2,499,391 minor page faults, 27 major page faults, 0 swaps.
0.4636
0.8372
#(45 50 53 54 51 51 44 46 53 43 43 40 44 42 45 52 50 44 45 44 45 50 49 49 37 51 51 36 47 45 44 42 42 46 46 56 56 44 49 51 41 45 50 49 45 57 47 51 44 48 44 40 52 43 42 47 56 45 53 38 51 46 44 43 46 47 39 46 47 38 44 51 41 50 42 53 49 56 56 42 52 45 32 47 45 44 44 53 44 44 37 49 41 55 49 42 40 40 40 50)
#(80 76 73 79 80 74 76 75 74 79 82 82 80 73 74 85 85 87 82 84 79 85 80 84 80 80 82 84 78 80 89 82 81 81 88 87 76 84 81 84 81 87 91 87 84 84 81 81 84 83 80 86 87 94 84 88 84 90 86 87 83 85 90 88 84 88 87 86 87 89 83 84 82 87 86 84 87 84 82 87 88 82 83 90 80 85 85 90 81 86 89 86 86 86 87 89 89 89 92 82)
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 34,589,808 microseconds (34.589810 seconds) to run.
        104,642 microseconds ( 0.104642 seconds, 0.30%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     17,600,385 microseconds (17.600386 seconds) were spent in user mode
     17,282,685 microseconds (17.282684 seconds) were spent in system mode
 468,324,032 bytes of memory allocated.
 2,475,689 minor page faults, 127 major page faults, 0 swaps.
0.4738
0.8241
#(37 40 48 43 53 45 52 52 44 50 42 44 37 51 52 44 61 44 43 44 49 51 46 44 53 46 54 46 49 43 41 46 50 40 45 45 35 48 46 39 53 48 45 52 46 47 50 52 41 54 51 44 44 37 44 48 48 51 47 51 52 50 48 50 50 47 48 44 52 52 46 46 43 39 46 54 52 49 54 53 52 46 40 55 56 45 50 50 47 53 52 44 42 51 49 51 53 45 45 42)
#(62 74 71 76 74 79 80 74 76 85 80 75 86 77 85 82 67 84 89 76 83 74 82 86 81 80 80 83 82 83 80 78 77 76 85 85 77 87 81 85 81 84 86 84 80 86 84 83 87 88 79 84 84 85 77 84 85 90 86 89 81 90 84 87 87 83 86 91 83 86 90 84 88 86 87 83 89 87 88 77 83 83 82 79 82 80 82 81 86 83 85 89 89 79 80 78 83 83 86 88)
? (setf *probability-threshold* 0.25)
0.25
? (setf *intensity-threshold* 0)
0
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 33,385,762 microseconds (33.385760 seconds) to run.
         73,356 microseconds ( 0.073356 seconds, 0.22%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     17,019,851 microseconds (17.019852 seconds) were spent in user mode
     16,391,086 microseconds (16.391087 seconds) were spent in system mode
 453,625,104 bytes of memory allocated.
 2,588,986 minor page faults, 157 major page faults, 0 swaps.
0.5183
0.6748
#(51 54 57 51 51 52 52 51 54 46 54 49 52 54 57 50 48 50 53 59 54 53 41 51 49 53 51 54 45 47 52 59 54 49 54 55 49 54 52 47 50 54 47 53 49 52 57 59 48 43 50 55 54 54 52 56 55 51 49 51 45 49 50 49 56 50 49 40 48 49 48 54 48 45 51 56 51 55 54 50 58 52 59 52 55 55 61 49 55 49 45 55 54 61 50 49 54 54 55 57)
#(58 64 62 58 54 69 60 66 62 61 65 63 75 67 66 64 62 58 64 71 69 71 54 61 67 66 67 68 59 67 67 74 64 61 63 70 60 66 67 58 62 73 68 67 66 74 69 72 69 62 66 68 68 68 65 78 69 69 67 73 59 73 79 73 67 68 63 63 67 67 75 65 65 76 67 71 76 76 74 67 72 75 80 70 74 74 78 61 69 63 65 72 67 76 75 68 77 64 67 69)
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 34,297,094 microseconds (34.297096 seconds) to run.
         85,226 microseconds ( 0.085226 seconds, 0.25%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     17,531,041 microseconds (17.531040 seconds) were spent in user mode
     16,858,297 microseconds (16.858295 seconds) were spent in system mode
 455,470,736 bytes of memory allocated.
 2,489,635 minor page faults, 166 major page faults, 0 swaps.
0.5076
0.6802
#(44 48 42 48 47 50 51 50 53 42 48 49 43 42 46 54 50 49 49 51 45 51 44 47 49 45 47 51 49 52 47 63 52 49 56 56 53 48 56 51 56 43 49 60 57 53 51 47 50 48 44 56 47 48 45 54 45 49 53 51 43 64 55 48 56 57 52 46 51 53 53 54 52 57 55 56 45 63 49 52 46 46 43 57 52 53 56 47 52 51 58 60 58 44 52 49 52 59 51 56)
#(55 64 57 57 55 61 66 61 69 65 66 64 67 54 60 58 72 69 65 76 65 70 63 61 64 62 63 75 68 75 57 73 71 69 62 71 65 65 68 72 69 63 63 73 78 71 69 69 66 66 62 69 58 69 68 63 70 60 70 67 70 72 69 67 71 73 71 74 72 70 66 72 70 69 79 76 57 75 70 71 64 69 69 76 67 75 74 65 69 72 74 80 83 70 74 74 68 76 68 78)
? (setf *intensity-threshold* 2)
2
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 34,945,020 microseconds (34.945020 seconds) to run.
         90,109 microseconds ( 0.090109 seconds, 0.26%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     17,996,332 microseconds (17.996332 seconds) were spent in user mode
     17,010,900 microseconds (17.010900 seconds) were spent in system mode
 469,364,384 bytes of memory allocated.
 2,511,390 minor page faults, 183 major page faults, 0 swaps.
0.4973
0.8351
#(47 43 43 51 49 44 39 53 47 55 47 48 39 51 48 45 50 43 49 50 50 52 55 53 47 47 54 51 49 48 47 55 58 59 51 53 49 45 47 52 40 57 46 52 47 52 50 57 53 46 50 48 43 57 46 44 61 53 50 47 51 56 52 50 53 48 55 52 48 41 45 52 44 49 52 43 54 59 48 39 55 46 47 57 53 51 56 58 54 52 43 48 59 48 49 46 48 47 50 53)
#(72 65 72 76 74 77 79 76 70 79 77 73 75 81 80 86 77 80 77 78 83 77 80 81 81 85 82 74 88 86 79 83 82 86 82 84 75 85 86 85 84 79 84 79 79 88 87 87 83 87 89 83 83 85 76 91 80 89 83 84 84 88 85 77 81 84 88 82 90 89 90 88 91 89 89 89 89 86 94 85 90 87 86 89 82 87 89 93 93 89 90 91 88 78 86 91 87 92 87 90)
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 34,739,132 microseconds (34.739132 seconds) to run.
         94,491 microseconds ( 0.094491 seconds, 0.27%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     18,141,765 microseconds (18.141766 seconds) were spent in user mode
     16,949,847 microseconds (16.949848 seconds) were spent in system mode
 469,559,184 bytes of memory allocated.
 2,492,461 minor page faults, 189 major page faults, 0 swaps.
0.5001
0.8371
#(51 57 49 50 48 49 45 56 49 50 53 44 57 53 51 46 54 50 47 49 53 52 48 49 54 50 49 44 46 43 48 52 52 54 52 48 52 52 44 54 51 47 46 47 50 41 53 46 47 49 49 50 55 65 44 46 48 38 52 45 44 51 49 47 62 44 51 47 52 55 47 51 54 43 56 57 55 47 55 51 48 57 44 51 53 52 45 51 57 54 54 65 46 46 41 52 48 51 49 46)
#(70 76 77 78 74 73 74 83 81 76 73 77 77 87 79 81 85 77 82 78 83 74 73 86 80 78 84 73 84 82 81 84 84 84 78 86 80 84 82 86 78 89 87 84 77 87 84 90 88 86 80 82 83 85 84 82 84 87 84 89 89 90 86 87 85 88 90 95 83 89 86 86 94 77 86 89 90 94 80 92 85 85 89 87 89 86 88 85 88 89 84 86 86 88 88 86 88 86 90 88)
? (setf *intensity-threshold* 3)
3
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 34,676,727 microseconds (34.676727 seconds) to run.
        100,916 microseconds ( 0.100916 seconds, 0.29%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     18,061,130 microseconds (18.061130 seconds) were spent in user mode
     17,001,052 microseconds (17.001053 seconds) were spent in system mode
 466,263,728 bytes of memory allocated.
 2,492,210 minor page faults, 1 major page faults, 0 swaps.
0.4543
0.8103
#(45 41 41 41 44 38 32 42 42 49 50 42 44 47 45 51 35 42 41 48 54 46 46 51 51 38 54 42 45 51 58 37 45 50 44 47 48 59 45 53 41 46 49 48 43 43 45 47 49 45 40 46 42 48 50 40 45 50 44 46 45 40 36 47 47 51 42 38 40 39 48 45 44 41 50 50 44 43 53 50 47 45 45 43 46 59 42 47 47 40 53 45 46 47 44 49 43 42 46 43)
#(70 70 74 77 75 73 79 77 76 78 78 76 75 78 80 80 85 78 85 76 78 73 68 78 70 75 85 75 82 79 74 84 86 76 82 81 76 74 83 82 76 78 80 82 90 80 84 86 85 78 81 82 83 76 72 82 82 78 81 86 87 84 84 82 89 85 83 88 85 89 82 86 77 85 85 83 84 83 81 86 87 86 85 80 84 78 85 84 85 90 82 84 86 86 86 85 85 89 81 84)
? (time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 35,015,830 microseconds (35.015830 seconds) to run.
        105,400 microseconds ( 0.105400 seconds, 0.30%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     18,242,579 microseconds (18.242580 seconds) were spent in user mode
     17,227,265 microseconds (17.227264 seconds) were spent in system mode
 466,302,509 bytes of memory allocated.
 2,502,367 minor page faults, 191 major page faults, 0 swaps.
0.4456
0.8148
#(38 41 37 48 36 47 38 39 42 39 35 38 45 39 38 43 36 41 46 41 37 49 42 47 43 39 42 43 39 48 38 39 42 38 38 46 38 46 39 48 44 36 45 52 42 43 50 42 40 47 52 48 47 50 48 47 49 47 52 48 40 43 46 49 53 47 53 47 42 50 55 49 43 49 45 44 53 48 45 38 50 53 52 38 49 45 47 48 54 47 45 46 50 48 57 38 42 47 39 45)
#(72 71 84 74 76 72 72 72 77 78 81 82 71 76 74 75 80 81 79 78 78 76 76 85 82 84 83 79 85 84 83 77 80 83 85 87 78 82 82 80 84 84 85 78 85 88 87 79 82 81 83 88 75 79 82 79 87 79 85 85 86 86 88 83 83 84 84 85 81 79 84 85 86 83 80 84 84 79 88 87 84 82 89 90 81 77 90 82 75 81 83 85 83 89 81 82 82 83 81 80)
;;; v0.1: added model-based decision
? Loading ACT-UP 1.3.2
Loading evacuation model version 0.1
(time (evaluate (simulate :runs 100 :length 100 :init 10)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10))
took 34,004,064 microseconds (34.004063 seconds) to run.
        139,696 microseconds ( 0.139696 seconds, 0.41%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     22,406,041 microseconds (22.406040 seconds) were spent in user mode
     16,919,241 microseconds (16.919240 seconds) were spent in system mode
 464,933,152 bytes of memory allocated.
 2,653,203 minor page faults, 255 major page faults, 0 swaps.
0.522
0.8176
#(52 56 56 51 51 52 50 52 55 47 55 47 51 53 57 51 50 51 53 57 55 55 45 50 51 53 50 56 48 48 52 58 59 51 53 57 54 53 51 49 50 54 51 52 51 51 56 61 50 42 49 58 53 50 53 55 56 49 46 50 45 50 48 48 57 51 51 41 47 55 46 52 49 47 53 57 53 54 55 52 56 54 59 49 57 54 62 49 56 53 47 52 53 62 48 48 55 56 49 58)
#(64 75 76 79 74 81 73 80 79 75 80 78 87 77 74 78 78 72 81 78 81 84 76 79 80 77 81 78 76 85 83 80 80 79 82 86 78 80 81 76 81 81 85 88 77 85 84 87 82 84 83 84 90 83 77 89 79 85 79 83 79 89 85 82 81 90 84 82 84 84 83 78 76 88 82 87 89 83 90 79 86 88 94 85 86 83 91 77 87 85 84 87 86 85 84 76 85 80 81 84)
? (time (evaluate (simulate :runs 100 :length 100 :init 10 :decision 'model-free-decision)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10 :DECISION 'MODEL-FREE-DECISION))
took 34,312,664 microseconds (34.312664 seconds) to run.
         78,886 microseconds ( 0.078886 seconds, 0.23%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     17,678,656 microseconds (17.678656 seconds) were spent in user mode
     17,112,586 microseconds (17.112585 seconds) were spent in system mode
 465,603,152 bytes of memory allocated.
 2,513,823 minor page faults, 212 major page faults, 0 swaps.
0.5176
0.8151
#(44 47 44 51 46 51 52 55 53 43 49 52 42 46 49 56 47 49 49 49 49 54 43 51 49 48 48 52 53 53 50 65 53 53 63 55 52 52 59 48 52 48 50 63 58 55 50 48 50 52 46 55 50 51 47 57 47 51 51 49 44 66 53 53 57 58 53 44 54 52 50 53 55 58 55 50 44 69 49 59 43 44 47 57 51 55 53 45 54 53 57 63 60 48 55 47 53 60 45 56)
#(68 73 75 73 64 72 77 76 80 79 72 76 78 74 79 75 80 82 78 80 89 84 77 73 78 83 79 85 87 87 74 83 81 87 80 76 80 80 85 83 82 85 84 83 90 85 81 89 74 82 83 80 74 82 83 80 84 81 84 77 85 85 85 92 84 86 84 83 89 84 81 83 80 81 84 80 76 92 81 84 79 76 81 81 86 86 87 83 84 81 84 92 88 88 85 88 82 88 79 89)
? (time (evaluate (simulate :runs 100 :length 100 :init 10 :decision 'model-based-decision)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10 :DECISION 'MODEL-BASED-DECISION))
took 67,334,706 microseconds (67.334700 seconds) to run.
        168,828 microseconds ( 0.168828 seconds, 0.25%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     32,969,264 microseconds (32.969265 seconds) were spent in user mode
     34,061,331 microseconds (34.061333 seconds) were spent in system mode
 1,009,953,440 bytes of memory allocated.
 4,925,374 minor page faults, 99 major page faults, 0 swaps.
0.5665
0.8592
#(52 50 42 53 49 50 50 51 47 54 49 52 47 56 54 53 56 62 59 50 51 45 57 57 63 56 50 58 58 55 57 60 61 58 49 62 54 60 60 51 62 56 56 65 55 58 49 54 54 66 58 50 57 61 59 56 57 61 56 55 64 60 60 58 58 58 56 72 63 57 50 58 59 58 57 62 65 56 62 61 54 62 58 61 62 59 65 60 59 56 56 61 57 56 59 61 59 57 56 50)
#(72 74 68 72 78 80 72 74 77 76 77 80 77 82 79 86 79 85 76 82 82 87 85 85 90 87 77 79 80 79 93 89 84 86 79 86 84 86 85 90 86 84 87 87 91 89 90 87 89 91 88 92 86 92 88 87 89 92 81 87 89 93 89 86 89 92 91 88 88 86 84 90 90 86 92 91 91 88 89 94 92 90 90 94 92 90 88 93 86 89 90 91 88 90 94 91 86 86 92 92)
? (time (evaluate (simulate :runs 100 :length 100 :init 10 :decision 'model-based-decision)))
(EVALUATE (SIMULATE :RUNS 100 :LENGTH 100 :INIT 10 :DECISION 'MODEL-BASED-DECISION))
took 67,226,463 microseconds (67.226460 seconds) to run.
        166,936 microseconds ( 0.166936 seconds, 0.25%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     33,058,695 microseconds (33.058697 seconds) were spent in user mode
     33,996,683 microseconds (33.996685 seconds) were spent in system mode
 1,009,659,216 bytes of memory allocated.
 4,903,132 minor page faults, 111 major page faults, 0 swaps.
0.5832
0.8683
#(55 53 61 49 59 63 62 52 54 59 57 62 62 53 51 53 60 61 55 61 65 58 57 56 58 55 53 59 63 54 58 63 62 62 59 58 57 57 50 62 64 71 62 58 57 60 63 54 64 59 57 59 56 57 65 52 66 62 58 58 54 59 55 57 58 61 63 65 57 62 54 54 52 56 56 57 59 56 62 62 52 53 59 58 58 57 59 51 59 62 60 55 60 61 60 64 56 58 61 60)
#(78 81 79 78 84 81 83 77 84 80 80 80 87 77 79 76 79 83 82 88 84 79 83 86 87 81 75 88 87 85 87 85 85 85 86 79 87 87 86 88 92 85 87 88 94 93 88 81 86 90 90 77 87 85 92 86 90 93 87 91 89 90 93 84 91 92 88 89 91 86 85 90 86 92 88 92 91 91 91 91 91 92 93 91 92 88 91 93 93 91 92 89 89 95 95 85 91 89 91 94)
?  (time (dolist (threshold '(0.0 0.1 0.25 0.35 0.5))
           (setf *probability-threshold* threshold)
           (format t "~6,3F" threshold)
           (multiple-value-bind (evacuation compliance)
                                (evaluate (simulate :runs 250 :length 100 :init 10 :decision 'model-based-decision))
             (format t "~C~6,3F~C~6,3F~%" #\tab evacuation #\tab compliance))))
 0.000	 0.579	 0.744
 0.100	 0.596	 0.807
 0.250	 0.554	 0.854
 0.350	 0.547	 0.873
 0.500	 0.509	 0.837
(DOLIST (THRESHOLD '(0.0 0.1 0.25 0.35 0.5)) (SETF *PROBABILITY-THRESHOLD* THRESHOLD) (FORMAT T "~6,3F" THRESHOLD) (MULTIPLE-VALUE-BIND (EVACUATION COMPLIANCE) (EVALUATE (SIMULATE :RUNS 250 :LENGTH 100 :INIT 10 :DECISION 'MODEL-BASED-DECISION)) (FORMAT T "~C~6,3F~C~6,3F~%" #\Tab EVACUATION #\Tab COMPLIANCE)))
took 833,496,976 microseconds (833.496950 seconds) to run.
       2,478,898 microseconds (  2.478898 seconds, 0.30%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     408,938,193 microseconds (408.938200 seconds) were spent in user mode
     417,746,685 microseconds (417.746670 seconds) were spent in system mode
 12,491,703,267 bytes of memory allocated.
 61,179,285 minor page faults, 230 major page faults, 0 swaps.
?  (time (dolist (threshold '(0 1 2 3))
           (setf *intensity-threshold* threshold)
           (format t "~D" threshold)
           (multiple-value-bind (evacuation compliance)
                                (evaluate (simulate :runs 250 :length 100 :init 10 :decision 'model-based-decision))
             (format t "~C~6,3F~C~6,3F~%" #\tab evacuation #\tab compliance))))
0	 0.501	 0.765
1	 0.508	 0.846
2	 0.488	 0.802
3	 0.468	 0.746
(DOLIST (THRESHOLD '(0 1 2 3)) (SETF *INTENSITY-THRESHOLD* THRESHOLD) (FORMAT T "~D" THRESHOLD) (MULTIPLE-VALUE-BIND (EVACUATION COMPLIANCE) (EVALUATE (SIMULATE :RUNS 250 :LENGTH 100 :INIT 10 :DECISION 'MODEL-BASED-DECISION)) (FORMAT T "~C~6,3F~C~6,3F~%" #\Tab EVACUATION #\Tab COMPLIANCE)))
took 676,686,963 microseconds (676.686950 seconds) to run.
       2,371,777 microseconds (  2.371777 seconds, 0.35%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     333,048,046 microseconds (333.048030 seconds) were spent in user mode
     339,531,457 microseconds (339.531460 seconds) were spent in system mode
 10,064,349,661 bytes of memory allocated.
 48,812,096 minor page faults, 204 major page faults, 0 swaps.
NIL
? *probability-threshold*
0.5
?  (time (dolist (threshold '(0 1 2 3))
           (setf *intensity-threshold* threshold)
           (format t "~D" threshold)
           (multiple-value-bind (evacuation compliance)
                                (evaluate (simulate :runs 250 :length 100 :init 10 :decision 'model-based-decision))
             (format t "~C~6,3F~C~6,3F~%" #\tab evacuation #\tab compliance))))
0	 0.557	 0.729
1	 0.557	 0.855
2	 0.533	 0.840
3	 0.504	 0.789
(DOLIST (THRESHOLD '(0 1 2 3)) (SETF *INTENSITY-THRESHOLD* THRESHOLD) (FORMAT T "~D" THRESHOLD) (MULTIPLE-VALUE-BIND (EVACUATION COMPLIANCE) (EVALUATE (SIMULATE :RUNS 250 :LENGTH 100 :INIT 10 :DECISION 'MODEL-BASED-DECISION)) (FORMAT T "~C~6,3F~C~6,3F~%" #\Tab EVACUATION #\Tab COMPLIANCE)))
took 682,591,583 microseconds (682.591550 seconds) to run.
       2,743,293 microseconds (  2.743293 seconds, 0.40%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     333,759,768 microseconds (333.759770 seconds) were spent in user mode
     345,361,953 microseconds (345.361940 seconds) were spent in system mode
 10,071,983,155 bytes of memory allocated.
 49,067,640 minor page faults, 176 major page faults, 0 swaps.
;;; v 0.2: formatted output

? (pprint (protocol-output (simulate :runs 2 :length 5 :init 10)))

#2A((((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context"))
     ((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context"))
     ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context"))
     ((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context"))
     ((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")))
    (((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context"))
     ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context"))
     ((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context"))
     ((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context"))
     ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context"))))
? (pprint (protocol-output (simulate :runs 2 :length 5 :init 10)))

#2A((((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:PROBABILITY :VALUE 0.32606483 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:INTENSITY :VALUE 2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:DAMAGE :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:UTILITY :VALUE -5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility"))
     ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:PROBABILITY :VALUE 0.8650311 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:INTENSITY :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:LANDING :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:DAMAGE :VALUE 3 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:UTILITY :VALUE -3 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility"))
     ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:PROBABILITY :VALUE 0.005523855 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:INTENSITY :VALUE 1 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:DAMAGE :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:UTILITY :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility"))
     ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:PROBABILITY :VALUE 0.5493901 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:INTENSITY :VALUE 4 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:DAMAGE :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:UTILITY :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility"))
     ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:PROBABILITY :VALUE 0.28204924 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:INTENSITY :VALUE 4 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:DAMAGE :VALUE 1 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:UTILITY :VALUE 4 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")))
    (((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:PROBABILITY :VALUE 0.46812302 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:INTENSITY :VALUE 1 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:DAMAGE :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:UTILITY :VALUE -5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility"))
     ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:PROBABILITY :VALUE 0.0062158704 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:INTENSITY :VALUE 3 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:DAMAGE :VALUE 2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:UTILITY :VALUE 3 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility"))
     ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:PROBABILITY :VALUE 0.9267006 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:INTENSITY :VALUE 1 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:LANDING :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:DAMAGE :VALUE 2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:UTILITY :VALUE -2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility"))
     ((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:PROBABILITY :VALUE 0.4891229 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:INTENSITY :VALUE 2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:DAMAGE :VALUE 3 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:UTILITY :VALUE -2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility"))
     ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action")
      (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:PROBABILITY :VALUE 0.7557405 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:INTENSITY :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context")
      (:LANDING :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:DAMAGE :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome")
      (:UTILITY :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility"))))
;;; run-model function
? (run-model '((:NOISE :VALUE 0.25 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model"
  :PARAMETER-SUB-CLASS "architecture")
 (:TEMPERATURE :VALUE 1.0 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
  "model" :PARAMETER-SUB-CLASS "architecture")
 (:SIMILARITY :VALUE
  (:INTEGER COMMON-LISP-USER::FUNCTIONNAME :DOUBLE
   COMMON-LISP-USER::FUNCTIONNAME)
  :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model"
  :PARAMETER-SUB-CLASS "knowledge")
 (:UTILITY :VALUE
  (:INTEGER COMMON-LISP-USER::FUNCTIONNAME :DOUBLE
   COMMON-LISP-USER::FUNCTIONNAME)
  :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model"
  :PARAMETER-SUB-CLASS "utility")
 (:DECISION :VALUE "functionName" :UNIT-OF-MEASURE NIL
  :PARAMETER-CLASS "model" :PARAMETER-SUB-CLASS "procedure")
 (:INIT-LENGTH :VALUE 10 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
  "simulation" :PARAMETER-SUB-CLASS "simulation")
 (:RUN-LENGTH :VALUE 5 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
  "simulation" :PARAMETER-SUB-CLASS "simulation")
 (:RUN-DELAY :VALUE 1.0 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
  "simulation" :PARAMETER-SUB-CLASS "simulation")
 (:RUN-COUNT :VALUE 5 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
  "simulation" :PARAMETER-SUB-CLASS "simulation")
 (:PROBABILITY-THRESHOLD :VALUE 0.25 :UNIT-OF-MEASURE NIL
  :PARAMETER-CLASS "simulation" :PARAMETER-SUB-CLASS "policy")
 (:INTENSITY-THRESHOLD :VALUE 1 :UNIT-OF-MEASURE NIL
  :PARAMETER-CLASS "simulation" :PARAMETER-SUB-CLASS "policy")
 (:INTENSITY-STANDARD-DEVIATION :VALUE 1.0 :UNIT-OF-MEASURE NIL
  :PARAMETER-CLASS "simulation" :PARAMETER-SUB-CLASS "environment")) nil)
#2A((((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.4229026 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.72146827 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 4 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 3 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 3 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.1356786 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 3 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.98625606 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 4 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 4 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE -4 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.48163113 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility"))) (((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.98197865 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.94700587 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 3 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.20605955 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 4 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE -5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.8602167 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.66877913 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 1 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility"))) (((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.08367877 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.14398715 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 4 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.41895825 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE -5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.93252516 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 1 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.44259092 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 4 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 4 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE -1 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility"))) (((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.75890183 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.3809455 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.5552214 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 1 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 1 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE -4 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.038485195 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 1 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.24943456 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility"))) (((:DECISION :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.9351577 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 4 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 1.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.38913977 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 5 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.045645162 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 1 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 3 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.14130872 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 3 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 1 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 4 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility")) ((:DECISION :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "action") (:EVACUATION-MESSAGE :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:PROBABILITY :VALUE 0.117898606 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:INTENSITY :VALUE 4 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "context") (:LANDING :VALUE 0.0 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:DAMAGE :VALUE 3 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "observable" :OUTPUT-SUB-CLASS "outcome") (:UTILITY :VALUE 2 :UNIT-OF-MEASURE :DOUBLE :OUTPUT-CLASS "internal" :OUTPUT-SUB-CLASS "utility"))))
;;; Corrected similarity function
? (time (progn
          (dolist (threshold '(0.0 0.1 0.25 0.35 0.5))
           (setf *probability-threshold* threshold)
           (format t "~6,3F" threshold)
           (multiple-value-bind (evacuation compliance)
                                (evaluate (simulate :runs 250 :length 100 :init 10 :decision 'model-based-decision))
             (format t "~C~6,3F~C~6,3F~%" #\tab evacuation #\tab compliance)))
          (setf *probability-threshold* 0.25)
          (dolist (threshold '(0 1 2 3))
           (setf *intensity-threshold* threshold)
           (format t "~D" threshold)
           (multiple-value-bind (evacuation compliance)
                                (evaluate (simulate :runs 250 :length 100 :init 10 :decision 'model-based-decision))
             (format t "~C~6,3F~C~6,3F~%" #\tab evacuation #\tab compliance)))))
 0.000	 0.618	 0.712
 0.100	 0.601	 0.754
 0.250	 0.573	 0.819
 0.350	 0.572	 0.836
 0.500	 0.539	 0.801
0	 0.616	 0.765
1	 0.572	 0.821
2	 0.545	 0.822
3	 0.520	 0.777
(PROGN (DOLIST (THRESHOLD '(0.0 0.1 0.25 0.35 0.5)) (SETF *PROBABILITY-THRESHOLD* THRESHOLD) (FORMAT T "~6,3F" THRESHOLD) (MULTIPLE-VALUE-BIND (EVACUATION COMPLIANCE) (EVALUATE (SIMULATE :RUNS 250 :LENGTH 100 :INIT 10 :DECISION 'MODEL-BASED-DECISION)) (FORMAT T "~C~6,3F~C~6,3F~%" #\Tab EVACUATION #\Tab COMPLIANCE))) (SETF *PROBABILITY-THRESHOLD* 0.25) (DOLIST (THRESHOLD '(0 1 2 3)) (SETF *INTENSITY-THRESHOLD* THRESHOLD) (FORMAT T "~D" THRESHOLD) (MULTIPLE-VALUE-BIND (EVACUATION COMPLIANCE) (EVALUATE (SIMULATE :RUNS 250 :LENGTH 100 :INIT 10 :DECISION 'MODEL-BASED-DECISION)) (FORMAT T "~C~6,3F~C~6,3F~%" #\Tab EVACUATION #\Tab COMPLIANCE))))
took 2,214,026,750 microseconds (2214.026900 seconds) to run.
         8,780,514 microseconds (   8.780514 seconds, 0.40%) of which was spent in GC.
During that period, and with 10 available CPU cores,
       875,336,457 microseconds ( 875.336400 seconds) were spent in user mode
     1,018,408,399 microseconds (1018.408400 seconds) were spent in system mode
 22,265,446,381 bytes of memory allocated.
 111,653,060 minor page faults, 1,846 major page faults, 0 swaps.
NIL
? Loading evacuation model version "0.3.2"
(time (progn
          (dolist (threshold '(0.0 0.1 0.25 0.35 0.5))
           (setf *probability-threshold* threshold)
           (format t "~6,3F" threshold)
           (multiple-value-bind (evacuation compliance)
                                (evaluate (simulate :runs 250 :length 100 :init 10 :decision 'model-free-decision))
             (format t "~C~6,3F~C~6,3F~%" #\tab evacuation #\tab compliance)))
          (setf *probability-threshold* 0.25)
          (dolist (threshold '(0 1 2 3))
           (setf *intensity-threshold* threshold)
           (format t "~D" threshold)
           (multiple-value-bind (evacuation compliance)
                                (evaluate (simulate :runs 250 :length 100 :init 10 :decision 'model-free-decision))
             (format t "~C~6,3F~C~6,3F~%" #\tab evacuation #\tab compliance)))))
 0.000	 0.543	 0.659
 0.100	 0.558	 0.723
 0.250	 0.531	 0.781
 0.350	 0.517	 0.803
 0.500	 0.492	 0.788
0	 0.549	 0.705
1	 0.532	 0.780
2	 0.504	 0.827
3	 0.474	 0.799
(PROGN (DOLIST (THRESHOLD '(0.0 0.1 0.25 0.35 0.5)) (SETF *PROBABILITY-THRESHOLD* THRESHOLD) (FORMAT T "~6,3F" THRESHOLD) (MULTIPLE-VALUE-BIND (EVACUATION COMPLIANCE) (EVALUATE (SIMULATE :RUNS 250 :LENGTH 100 :INIT 10 :DECISION 'MODEL-FREE-DECISION)) (FORMAT T "~C~6,3F~C~6,3F~%" #\Tab EVACUATION #\Tab COMPLIANCE))) (SETF *PROBABILITY-THRESHOLD* 0.25) (DOLIST (THRESHOLD '(0 1 2 3)) (SETF *INTENSITY-THRESHOLD* THRESHOLD) (FORMAT T "~D" THRESHOLD) (MULTIPLE-VALUE-BIND (EVACUATION COMPLIANCE) (EVALUATE (SIMULATE :RUNS 250 :LENGTH 100 :INIT 10 :DECISION 'MODEL-FREE-DECISION)) (FORMAT T "~C~6,3F~C~6,3F~%" #\Tab EVACUATION #\Tab COMPLIANCE))))
took 886,870,757 microseconds (886.870800 seconds) to run.
       3,227,493 microseconds (  3.227493 seconds, 0.36%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     436,128,477 microseconds (436.128480 seconds) were spent in user mode
     431,421,469 microseconds (431.421480 seconds) were spent in system mode
 11,325,613,216 bytes of memory allocated.
 58,194,787 minor page faults, 412 major page faults, 0 swaps.
NIL
;;; v 0.3.3 run-batch to collect utilities
? Loading ACT-UP 1.3.2
Loading evacuation model version "0.3.3"
(time (run-batch :samples 100 :decision 'model-free-decision))
 0.000	 0.558	 0.672	 0.814
 0.100	 0.549	 0.721	 0.900
 0.250	 0.543	 0.783	 1.055
 0.350	 0.533	 0.803	 1.077
 0.500	 0.487	 0.795	 1.077
0	 0.527	 0.691	 0.986
1	 0.525	 0.777	 1.066
2	 0.518	 0.816	 1.031
3	 0.463	 0.808	 1.119
(RUN-BATCH :SAMPLES 100 :DECISION 'MODEL-FREE-DECISION)
took 322,987,779 microseconds (322.987760 seconds) to run.
       1,053,982 microseconds (  1.053982 seconds, 0.33%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     166,448,261 microseconds (166.448260 seconds) were spent in user mode
     155,511,450 microseconds (155.511440 seconds) were spent in system mode
 4,529,869,821 bytes of memory allocated.
 22,479,126 minor page faults, 332 major page faults, 0 swaps.
1
? Loading ACT-UP 1.3.2
Loading evacuation model version "0.3.3"
(time (run-batch :samples 10 :decision 'model-based-decision))
 0.000	 0.606	 0.700	 0.909
 0.100	 0.612	 0.785	 1.064
 0.250	 0.595	 0.779	 0.870
 0.350	 0.567	 0.809	 0.930
 0.500	 0.535	 0.809	 1.221
0	 0.633	 0.758	 0.827
1	 0.549	 0.804	 1.015
2	 0.533	 0.819	 0.986
3	 0.472	 0.779	 1.046
(RUN-BATCH :SAMPLES 10 :DECISION 'MODEL-BASED-DECISION)
took 62,088,332 microseconds (62.088333 seconds) to run.
        424,870 microseconds ( 0.424870 seconds, 0.68%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     28,615,413 microseconds (28.615412 seconds) were spent in user mode
     30,408,915 microseconds (30.408915 seconds) were spent in system mode
 888,502,224 bytes of memory allocated.
 4,695,520 minor page faults, 226 major page faults, 0 swaps.
1
? (time (run-batch :samples 10 :decision 'model-based-decision))
 0.000	 0.613	 0.670	 0.798
 0.100	 0.662	 0.825	 0.986
 0.250	 0.575	 0.800	 1.193
 0.350	 0.584	 0.839	 1.049
 0.500	 0.450	 0.854	 1.158
0	 0.616	 0.763	 0.941
1	 0.576	 0.809	 1.041
2	 0.505	 0.852	 1.189
3	 0.491	 0.811	 1.166
(RUN-BATCH :SAMPLES 10 :DECISION 'MODEL-BASED-DECISION)
took 63,675,785 microseconds (63.675785 seconds) to run.
        141,519 microseconds ( 0.141519 seconds, 0.22%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     28,441,905 microseconds (28.441904 seconds) were spent in user mode
     31,477,096 microseconds (31.477097 seconds) were spent in system mode
 890,914,864 bytes of memory allocated.
 4,746,166 minor page faults, 88 major page faults, 0 swaps.
1
? (time (run-batch :samples 10 :decision 'model-based-decision))
 0.000	 0.615	 0.713	 0.862
 0.100	 0.578	 0.736	 0.965
 0.250	 0.593	 0.830	 0.954
 0.350	 0.531	 0.818	 1.253
 0.500	 0.564	 0.793	 1.205
0	 0.661	 0.800	 1.015
1	 0.648	 0.813	 0.851
2	 0.534	 0.830	 1.072
3	 0.553	 0.722	 0.859
(RUN-BATCH :SAMPLES 10 :DECISION 'MODEL-BASED-DECISION)
took 64,001,755 microseconds (64.001755 seconds) to run.
        202,734 microseconds ( 0.202734 seconds, 0.32%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     29,067,743 microseconds (29.067743 seconds) were spent in user mode
     31,854,408 microseconds (31.854408 seconds) were spent in system mode
 890,209,229 bytes of memory allocated.
 4,792,779 minor page faults, 316 major page faults, 0 swaps.
1
? Loading ACT-UP 1.3.2
Loading evacuation model version "0.3.3"
(time (run-batch :samples 100 :decision 'model-based-decision))
 0.000	 0.595	 0.684	 0.847
 0.100	 0.592	 0.762	 0.936
 0.250	 0.594	 0.817	 1.055
 0.350	 0.564	 0.842	 1.116
 0.500	 0.531	 0.814	 1.122
0	 0.615	 0.758	 1.007
1	 0.568	 0.817	 1.088
2	 0.515	 0.828	 1.127
3	 0.536	 0.770	 1.060
(RUN-BATCH :SAMPLES 100 :DECISION 'MODEL-BASED-DECISION)
took 789,383,312 microseconds (789.383300 seconds) to run.
       2,308,280 microseconds (  2.308280 seconds, 0.29%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     339,836,343 microseconds (339.836360 seconds) were spent in user mode
     390,931,001 microseconds (390.931000 seconds) were spent in system mode
 8,907,088,768 bytes of memory allocated.
 45,376,799 minor page faults, 492 major page faults, 0 swaps.
1
;;; Adding accuracy
? Loading ACT-UP 1.3.2
Loading evacuation model version "0.3.3"
(time (run-batch :samples 100 :decision 'model-free-decision))
THRESHOLD 	 EVACUATION 	 COMPLIANCE 	 UTILITY 	 ACCURACY
 0.000	 0.558	 0.672	 0.814	 0.559
 0.100	 0.549	 0.721	 0.900	 0.580
 0.250	 0.543	 0.783	 1.055	 0.605
 0.350	 0.533	 0.803	 1.077	 0.622
 0.500	 0.487	 0.795	 1.077	 0.631
0	 0.527	 0.691	 0.986	 0.609
1	 0.525	 0.777	 1.066	 0.615
2	 0.518	 0.816	 1.031	 0.593
3	 0.463	 0.808	 1.119	 0.594
(RUN-BATCH :SAMPLES 100 :DECISION 'MODEL-FREE-DECISION)
took 323,712,780 microseconds (323.712770 seconds) to run.
       1,056,950 microseconds (  1.056950 seconds, 0.33%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     166,649,070 microseconds (166.649060 seconds) were spent in user mode
     155,838,689 microseconds (155.838680 seconds) were spent in system mode
 4,529,879,104 bytes of memory allocated.
 22,532,860 minor page faults, 164 major page faults, 0 swaps.
1
? Loading ACT-UP 1.3.2
Loading evacuation model version "0.3.3"
 (time (run-batch :samples 10 :decision 'model-based-decision))
THRESHOLD 	 EVACUATION 	 COMPLIANCE 	 UTILITY 	 ACCURACY
     0.000	     0.606	     0.700	     0.909	     0.587
     0.100	     0.612	     0.785	     1.064	     0.597
     0.250	     0.595	     0.779	     0.870	     0.618
     0.350	     0.567	     0.809	     0.930	     0.616
     0.500	     0.535	     0.809	     1.221	     0.641
         0	     0.633	     0.758	     0.827	     0.610
         1	     0.549	     0.804	     1.015	     0.612
         2	     0.533	     0.819	     0.986	     0.581
         3	     0.472	     0.779	     1.046	     0.603
(RUN-BATCH :SAMPLES 10 :DECISION 'MODEL-BASED-DECISION)
took 62,960,597 microseconds (62.960594 seconds) to run.
        382,836 microseconds ( 0.382836 seconds, 0.61%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     32,689,417 microseconds (32.689415 seconds) were spent in user mode
     30,849,341 microseconds (30.849340 seconds) were spent in system mode
 888,513,968 bytes of memory allocated.
 4,566,753 minor page faults, 227 major page faults, 0 swaps.
1
?  (time (run-batch :samples 10 :decision 'model-based-decision))
THRESHOLD 	 EVACUATION 	 COMPLIANCE 	 UTILITY 	 ACCURACY
     0.000	     0.613	     0.670	     0.798	     0.571
     0.100	     0.662	     0.825	     0.986	     0.581
     0.250	     0.575	     0.800	     1.193	     0.650
     0.350	     0.584	     0.839	     1.049	     0.604
     0.500	     0.450	     0.854	     1.158	     0.639
         0	     0.616	     0.763	     0.941	     0.624
         1	     0.576	     0.809	     1.041	     0.620
         2	     0.505	     0.852	     1.189	     0.630
         3	     0.491	     0.811	     1.166	     0.605
(RUN-BATCH :SAMPLES 10 :DECISION 'MODEL-BASED-DECISION)
took 63,298,724 microseconds (63.298725 seconds) to run.
        134,717 microseconds ( 0.134717 seconds, 0.21%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     31,935,627 microseconds (31.935629 seconds) were spent in user mode
     31,222,270 microseconds (31.222270 seconds) were spent in system mode
 890,926,544 bytes of memory allocated.
 4,628,301 minor page faults, 0 major page faults, 0 swaps.
1
?  (time (run-batch :samples 100 :decision 'model-based-decision))
THRESHOLD 	 EVACUATION 	 COMPLIANCE 	 UTILITY 	 ACCURACY
     0.000	     0.612	     0.709	     0.861	     0.572
     0.100	     0.629	     0.786	     0.946	     0.589
     0.250	     0.584	     0.815	     1.034	     0.611
     0.350	     0.553	     0.824	     1.099	     0.629
     0.500	     0.510	     0.809	     1.158	     0.643
         0	     0.591	     0.764	     1.010	     0.636
         1	     0.579	     0.824	     1.084	     0.625
         2	     0.554	     0.829	     1.115	     0.606
         3	     0.527	     0.777	     1.085	     0.601
(RUN-BATCH :SAMPLES 100 :DECISION 'MODEL-BASED-DECISION)
took 916,941,187 microseconds (916.941160 seconds) to run.
       2,943,662 microseconds (  2.943662 seconds, 0.32%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     415,588,507 microseconds (415.588500 seconds) were spent in user mode
     465,200,062 microseconds (465.200070 seconds) were spent in system mode
 8,909,258,365 bytes of memory allocated.
 45,382,569 minor page faults, 1,037 major page faults, 0 swaps.
1
;;; Checking memory allocation for single runs of various parameter settings
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.0 :intensity-threshold 1 :decision 'model-free-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.0 :INTENSITY-THRESHOLD 1 :DECISION 'MODEL-FREE-DECISION) NIL)
took 435,087 microseconds (0.435087 seconds) to run.
         924 microseconds (0.000924 seconds, 0.21%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     234,051 microseconds (0.234051 seconds) were spent in user mode
     233,388 microseconds (0.233388 seconds) were spent in system mode
 4,956,112 bytes of memory allocated.
 25,283 minor page faults, 13 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.1 :intensity-threshold 1 :decision 'model-free-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.1 :INTENSITY-THRESHOLD 1 :DECISION 'MODEL-FREE-DECISION) NIL)
took 440,818 microseconds (0.440818 seconds) to run.
       1,144 microseconds (0.001144 seconds, 0.26%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     253,936 microseconds (0.253936 seconds) were spent in user mode
     256,012 microseconds (0.256012 seconds) were spent in system mode
 4,936,848 bytes of memory allocated.
 40,989 minor page faults, 0 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.25 :intensity-threshold 1 :decision 'model-free-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.25 :INTENSITY-THRESHOLD 1 :DECISION 'MODEL-FREE-DECISION) NIL)
took 391,333 microseconds (0.391333 seconds) to run.
       1,260 microseconds (0.001260 seconds, 0.32%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     244,885 microseconds (0.244885 seconds) were spent in user mode
     237,684 microseconds (0.237684 seconds) were spent in system mode
 5,060,816 bytes of memory allocated.
 42,954 minor page faults, 3 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.35 :intensity-threshold 1 :decision 'model-free-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.35 :INTENSITY-THRESHOLD 1 :DECISION 'MODEL-FREE-DECISION) NIL)
took 373,627 microseconds (0.373627 seconds) to run.
       1,790 microseconds (0.001790 seconds, 0.48%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     222,890 microseconds (0.222890 seconds) were spent in user mode
     206,970 microseconds (0.206970 seconds) were spent in system mode
 5,109,648 bytes of memory allocated.
 29,616 minor page faults, 0 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.5 :intensity-threshold 1 :decision 'model-free-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.5 :INTENSITY-THRESHOLD 1 :DECISION 'MODEL-FREE-DECISION) NIL)
took 439,258 microseconds (0.439258 seconds) to run.
       1,097 microseconds (0.001097 seconds, 0.25%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     238,717 microseconds (0.238717 seconds) were spent in user mode
     232,955 microseconds (0.232955 seconds) were spent in system mode
 5,131,120 bytes of memory allocated.
 29,754 minor page faults, 0 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.25 :intensity-threshold 0 :decision 'model-free-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.25 :INTENSITY-THRESHOLD 0 :DECISION 'MODEL-FREE-DECISION) NIL)
took 374,778 microseconds (0.374778 seconds) to run.
         975 microseconds (0.000975 seconds, 0.26%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     230,370 microseconds (0.230370 seconds) were spent in user mode
     208,080 microseconds (0.208080 seconds) were spent in system mode
 5,010,960 bytes of memory allocated.
 30,759 minor page faults, 0 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.25 :intensity-threshold 1 :decision 'model-free-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.25 :INTENSITY-THRESHOLD 1 :DECISION 'MODEL-FREE-DECISION) NIL)
took 377,242 microseconds (0.377242 seconds) to run.
       1,091 microseconds (0.001091 seconds, 0.29%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     234,315 microseconds (0.234315 seconds) were spent in user mode
     213,470 microseconds (0.213470 seconds) were spent in system mode
 4,928,688 bytes of memory allocated.
 31,508 minor page faults, 0 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.25 :intensity-threshold 2 :decision 'model-free-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.25 :INTENSITY-THRESHOLD 2 :DECISION 'MODEL-FREE-DECISION) NIL)
took 378,221 microseconds (0.378221 seconds) to run.
       1,166 microseconds (0.001166 seconds, 0.31%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     240,625 microseconds (0.240625 seconds) were spent in user mode
     210,149 microseconds (0.210149 seconds) were spent in system mode
 5,138,416 bytes of memory allocated.
 32,424 minor page faults, 8 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.25 :intensity-threshold 3 :decision 'model-free-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.25 :INTENSITY-THRESHOLD 3 :DECISION 'MODEL-FREE-DECISION) NIL)
took 377,735 microseconds (0.377735 seconds) to run.
       1,648 microseconds (0.001648 seconds, 0.44%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     237,721 microseconds (0.237721 seconds) were spent in user mode
     206,892 microseconds (0.206892 seconds) were spent in system mode
 5,058,736 bytes of memory allocated.
 26,061 minor page faults, 4 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.0 :intensity-threshold 1 :decision 'model-based-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.0 :INTENSITY-THRESHOLD 1 :DECISION 'MODEL-BASED-DECISION) NIL)
took 698,047 microseconds (0.698047 seconds) to run.
       1,978 microseconds (0.001978 seconds, 0.28%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     335,546 microseconds (0.335546 seconds) were spent in user mode
     395,360 microseconds (0.395360 seconds) were spent in system mode
 9,339,856 bytes of memory allocated.
 53,988 minor page faults, 0 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.1 :intensity-threshold 1 :decision 'model-based-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.1 :INTENSITY-THRESHOLD 1 :DECISION 'MODEL-BASED-DECISION) NIL)
took 716,967 microseconds (0.716967 seconds) to run.
       4,779 microseconds (0.004779 seconds, 0.67%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     354,650 microseconds (0.354650 seconds) were spent in user mode
     400,038 microseconds (0.400038 seconds) were spent in system mode
 9,567,248 bytes of memory allocated.
 55,515 minor page faults, 2 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.25 :intensity-threshold 1 :decision 'model-based-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.25 :INTENSITY-THRESHOLD 1 :DECISION 'MODEL-BASED-DECISION) NIL)
took 728,004 microseconds (0.728004 seconds) to run.
       3,241 microseconds (0.003241 seconds, 0.45%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     391,805 microseconds (0.391805 seconds) were spent in user mode
     388,163 microseconds (0.388163 seconds) were spent in system mode
 11,026,512 bytes of memory allocated.
 59,471 minor page faults, 0 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.35 :intensity-threshold 1 :decision 'model-based-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.35 :INTENSITY-THRESHOLD 1 :DECISION 'MODEL-BASED-DECISION) NIL)
took 716,404 microseconds (0.716404 seconds) to run.
       2,976 microseconds (0.002976 seconds, 0.42%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     377,121 microseconds (0.377121 seconds) were spent in user mode
     396,414 microseconds (0.396414 seconds) were spent in system mode
 9,984,656 bytes of memory allocated.
 58,930 minor page faults, 0 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.5 :intensity-threshold 1 :decision 'model-based-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.5 :INTENSITY-THRESHOLD 1 :DECISION 'MODEL-BASED-DECISION) NIL)
took 718,149 microseconds (0.718149 seconds) to run.
       2,460 microseconds (0.002460 seconds, 0.34%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     383,354 microseconds (0.383354 seconds) were spent in user mode
     398,702 microseconds (0.398702 seconds) were spent in system mode
 10,016,336 bytes of memory allocated.
 61,489 minor page faults, 0 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.25 :intensity-threshold 0 :decision 'model-based-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.25 :INTENSITY-THRESHOLD 0 :DECISION 'MODEL-BASED-DECISION) NIL)
took 711,356 microseconds (0.711356 seconds) to run.
       2,497 microseconds (0.002497 seconds, 0.35%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     384,406 microseconds (0.384406 seconds) were spent in user mode
     402,859 microseconds (0.402859 seconds) were spent in system mode
 9,835,472 bytes of memory allocated.
 63,277 minor page faults, 0 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.25 :intensity-threshold 1 :decision 'model-based-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.25 :INTENSITY-THRESHOLD 1 :DECISION 'MODEL-BASED-DECISION) NIL)
took 703,160 microseconds (0.703160 seconds) to run.
       2,412 microseconds (0.002412 seconds, 0.34%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     368,625 microseconds (0.368625 seconds) were spent in user mode
     374,334 microseconds (0.374334 seconds) were spent in system mode
 10,028,048 bytes of memory allocated.
 49,876 minor page faults, 0 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.25 :intensity-threshold 2 :decision 'model-based-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.25 :INTENSITY-THRESHOLD 2 :DECISION 'MODEL-BASED-DECISION) NIL)
took 691,036 microseconds (0.691036 seconds) to run.
       2,813 microseconds (0.002813 seconds, 0.41%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     352,817 microseconds (0.352817 seconds) were spent in user mode
     369,316 microseconds (0.369316 seconds) were spent in system mode
 10,025,872 bytes of memory allocated.
 51,713 minor page faults, 0 major page faults, 0 swaps.
NIL
?  (time (progn (simulate :runs 1 :length 100 :probability-threshold 0.25 :intensity-threshold 3 :decision 'model-based-decision) ()))
(PROGN (SIMULATE :RUNS 1 :LENGTH 100 :PROBABILITY-THRESHOLD 0.25 :INTENSITY-THRESHOLD 3 :DECISION 'MODEL-BASED-DECISION) NIL)
took 697,816 microseconds (0.697816 seconds) to run.
       2,697 microseconds (0.002697 seconds, 0.39%) of which was spent in GC.
During that period, and with 10 available CPU cores,
     361,819 microseconds (0.361819 seconds) were spent in user mode
     377,268 microseconds (0.377268 seconds) were spent in system mode
 9,893,520 bytes of memory allocated.
 54,478 minor page faults, 0 major page faults, 0 swaps.
NIL

|#
