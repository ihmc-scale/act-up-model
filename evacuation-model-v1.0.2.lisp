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

v 1.0.0

Refactoring model into independent modules
Show that it roughly reproduces earlier results or where it deviates

7/22/25:

- add various models, including trust-model/learn-trust, control-model/learn-control, outcome-model/learn-outcome, and behavior-model
- update simulate function to include calls to above functions ;;; removed in favor of single model calling point

v 1.0.1

8/20/25:

Integrate v 0.3.4 changes into v 1.0.0

v 1.0.2

9/4/25:

- add back in previous models
- add theory-planned-behavior function to make calls to various processes and decision models
- modify model-free-decision, model-based-decision and theory-planned-behavior functions to take instance key parameter that triggers learning instead of decision
- modify simulate to call model function for learning instead of learn-instance directly
- resolve ambiguity in variable 'decision' in simulate function by renaming decision parameter as decision-function
- also same in run-batch and run-model
- add trace parameter to learn-instance
- use blend-vote instead of blend in trust and control models since blending over discrete decisions

|#

(defparameter *evacuation-model-version* "1.0.2")

(format t "Loading evacuation model version ~S~%" *evacuation-model-version*)

;;; models

;;; general mechanics

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

;;; common utility model

(defun decision-utility (decision landing damage)
  "Generate utility of decision given landing and damage outcomes."
  (if (eq decision 'yes)         ;;; if evacuating
      (if (> landing 0.5) damage (- damage 5)) ;;; then utility is proportional to damage avoided, factoring the cost of evacuation (5) is not landing
      (if (> landing 0.5) (- damage) (- 5 damage)))) ;;; otherwise utility is negative damage NOT avoided, factoring savings from evacuation (5)

(defun learn-instance (instance &key (trace t))
  "Learn complete instance."
  (let* ((decision (get-attribute 'decision instance))
         (landing (get-attribute 'landing instance))
         (damage (get-attribute 'damage instance))
         (outcome (decision-utility decision landing damage))) ;;; move utility computation to separate function
    (setf instance (append instance (list (list 'outcome outcome))))
    (when trace (format t "INSTANCE LEARNING~C~S~%" #\tab instance))
    (learn instance)
    instance))

;;; model #1: model-free decisions

(defun model-free-decision (stimulus &key (instance nil) (delay *delay*) (trace t))
  "Make decision based on stimulus by directly projecting outcome (utility) for each action and choosing the best.
  If instance is provided, learn instance instead."
  (actr-time delay)
  (if instance
      (learn-instance instance :trace trace)
      (let* ((evacuate-outcome (blend (append stimulus (list (list 'decision 'yes))) 'outcome))
             (stay-outcome (blend (append stimulus (list (list 'decision 'no))) 'outcome))
             (decision (if (> evacuate-outcome stay-outcome) 'yes 'no)))
        (when trace (format t "EVACUATE: ~6,3F~CSTAY: ~6,3F~CDECISION: ~S~%" evacuate-outcome #\tab stay-outcome #\tab decision))
        decision)))

;;; model #2: model-based decisions

(defun model-based-decision (stimulus &key (instance nil) (delay *delay*) (trace t))
  "Make decision based on stimulus by first projecting future state (landing and damage) then projecting outcome (utility) for each action and choosing the best.
  If instance is provided, learn instance instead."
  (actr-time delay)
  (if instance
      (learn-instance instance :trace trace)
      (let* ((landing (blend stimulus 'landing)) ;;; generate expected landing probability
             (damage (blend stimulus 'damage)) ;;; generate expected damage
             (expanded-stimulus (append stimulus (list (list 'landing landing) (list 'damage damage)))) ;;; generate expanded stimulus with expected future state
             (evacuate-outcome (blend (append expanded-stimulus (list (list 'decision 'yes))) 'outcome))
             (stay-outcome (blend (append expanded-stimulus (list (list 'decision 'no))) 'outcome))
             (decision (if (> evacuate-outcome stay-outcome) 'yes 'no)))
        (when trace (format t "EVACUATE: ~6,3F~CSTAY: ~6,3F~CDECISION: ~S~%" evacuate-outcome #\tab stay-outcome #\tab decision))
        decision)))

;;; model #3: theory of planned behavior

;;; trust model

(defun trust-model (stimulus &key (delay *delay*) (trace t))
  "Generate trust-based intention based on evacuation recommendation by blending (or retrieving) past trust instances. Adds delay first."
  (actr-time delay)
  (let* ((decision (blend-vote stimulus 'decision))  ;;; could be standard retrieval
         (intention `((intention ,decision)))) 
    (when trace (format t "TRUST DECISION~C~S~C~S~%" #\tab stimulus #\tab intention))
    (learn intention) ;;; learns intention
    intention))

(defun learn-trust (instance &key (trace t))
  "Learn instance linking evacuation recommendation to eventual decision/behavior."
  (let* ((decision (get-attribute 'decision instance))
         (evacuation (get-attribute 'evacuation instance))
         (instance (list (list 'evacuation evacuation) (list 'decision decision))))
    (when trace (format t "TRUST LEARNING~C~S~%" #\tab instance))
    (learn instance) ;;; learns new instance
    instance))

;;; control model
;;; exactly the same as the trust model, just with different inputs

(defun control-model (stimulus &key (delay *delay*) (trace t))
  "Generate control-based intention based on features like probability and intensity by blending (or retrieving) past control instances. Adds delay first."
  (actr-time delay)
  (let* ((decision (blend-vote stimulus 'decision))  ;;; could be partial matching
         (intention `((intention ,decision)))) 
    (when trace (format t "CONTROL DECISION~C~S~C~S~%" #\tab stimulus #\tab intention))
    (learn intention) ;;; learns intention
    intention))

(defun learn-control (instance &key (trace t))
  "Learn instance linking features like probability and intensity to eventual decision/behavior."
  (let* ((decision (get-attribute 'decision instance))
         (probability (get-attribute 'probability instance))
         (intensity (get-attribute 'intensity instance))
         (instance (list (list 'probability probability) (list 'intensity intensity) (list 'decision decision))))
    (when trace (format t "CONTROL LEARNING~C~S~%" #\tab instance))
    (learn instance) ;;; learns new instance
    instance))

;;; outcome model
;;; a bit more complex than control but learning is the same

#|
(defun outcome-model (stimulus &key (delay *delay*) (trace t))
  "Generate outcome-based intention based on features like probability and intensity by blending (or retrieving) past outcome instances. Adds delay first."
  (actr-time delay)
  (let* ((best-utility 0.0)
         (intention nil))
    (dolist (decision '(yes no))
      (let ((utility (blend (list (list 'decision decision)) 'outcome)))
        (when (or (null intention) (> utility best-utility))
          (setf best-utility utility)
          (setf intention `((intention ,decision))))))
    (when trace (format t "OUTCOME DECISION~C~S~C~S~%" #\tab stimulus #\tab intention))
    (learn intention) ;;; learns intention
    intention))
|#

;;; more complex model-based model
(defun outcome-model (stimulus &key (delay *delay*) (trace t))
  "Generate outcome-based intention based on features like probability and intensity by blending (or retrieving) past outcome instances. Adds delay first."
  (actr-time delay)
  (let* ((landing (blend stimulus 'landing))  ;;; could be partial matching
         (damage (blend stimulus 'damage))  ;;; could be partial matching
         (best-utility 0.0)
         (intention nil))
    (dolist (decision '(yes no))
      (let ((utility (blend (append stimulus `((decision ,decision) (landing ,landing) (damage ,damage))) 'utility)))
        (when (or (null intention) (> utility best-utility))
          (setf best-utility utility)
          (setf intention `((intention ,decision))))))
    (when trace (format t "OUTCOME DECISION~C~S~C~S~%" #\tab stimulus #\tab intention))
    (learn intention) ;;; learns intention
    intention))

(defun learn-outcome (instance &key (trace t))
  "Learn instance linking features like probability and intensity to eventual decision/behavior and outcomes and utility."
  (let* ((decision (get-attribute 'decision instance))
         (landing (get-attribute 'landing instance))
         (damage (get-attribute 'damage instance))
         (outcome (decision-utility decision landing damage))) ;;; move utility computation to separate function
    (setf instance (append instance (list (list 'outcome outcome)))) ;;; adds utility
    (when trace (format t "OUTCOME LEARNING~C~S~%" #\tab instance))
    (learn instance) ;;; learns new instance
    instance))

;;; behavior model
;;; very simple and no learning

(defun behavior-model (&key (delay *delay*) (trace t))
  "Generate behavior from most active intention in memory. No input. Could be more complex Adds delay first/"
  (actr-time delay)
  (let ((behavior (retrieve `((intention any)) :mode 'partial))) ;;; no real matching - just specifying type
    (when trace (format t "BEHAVIOR DECISION~C~S~%" #\tab behavior))
    behavior))

;;; top-level TPB model

(defun theory-planned-behavior (stimulus &key (instance nil) (delay *delay*) (trace t))
  "Make decision based on stimulus by directly projecting outcome (utility) for each action and choosing the best."
  (actr-time delay)
  (cond (instance
         ;;; calls model learning functions - do filtering in learning function - return last instance
         (learn-trust instance :trace trace)
         (learn-control instance :trace trace)
         (learn-outcome instance :trace trace))
        (t
         ;;; calls 3 component processes in sequence - could be something else
         (trust-model (last stimulus) :delay delay :trace trace)
         (control-model (butlast stimulus) :delay delay :trace trace)
;         (outcome-model (butlast stimulus) :delay delay :trace trace)
         (let ((decision (behavior-model :delay delay :trace trace)))
           (when trace (format t "DECISION: ~S~%" decision))
           decision))))

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
                      (parameters nil) (evacuation 'evacuate) (decision-function 'model-free-decision) (trace nil)) ;;; add decision parameter
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
                 (decision (if (< trial init) (random-decision stimulus) (funcall decision-function stimulus :delay delay :trace trace))) ;;; call model to make decision after initial random trials
                 (landing (if (< (random 1.0) probability) 1.0 0.0)) ;;; assume probability is true estimate but represent outcome in probability terms as well to generate estimate
                 (damage (max 0 (min 5 (round (+ intensity (noise damage-noise)))))) ;;; assume normal noise in intensity
                 (instance (append stimulus 
                                   (list (list 'decision decision)
                                         (list 'landing landing)
                                         (list 'damage damage))))
                 )
            (setf instance (funcall decision-function nil :instance instance :delay delay :trace trace)) ;;; call model to determine utility and learn instance
            (when (>= trial init) (push instance answers)))) ;;; only save actual trials not inits
        (push (reverse answers) results)))
    (reverse results)))

#|
;;; abandon approach of multiple calling points to model in simulation in favor of going back to a single decision point
(defun simulate (&key (runs 1) (length 100) (init *initial-trials*) (delay *delay*)
                      (probability-threshold *probability-threshold*) (intensity-threshold *intensity-threshold*) (damage-noise *damage-noise*)
                      (parameters nil) (evacuation 'evacuate) (decision 'behavior-model) (trace nil)) ;;; add decision parameter
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
                                 (list 'evacuation evacuation))))
            ;;; unless in trial phase, call the component models in order with the proper stimuli
            (when (< trial init)
              (trust-model (last stimulus) :delay delay :trace trace)
              (control-model (butlast stimulus) :delay delay :trace trace)
              (outcome-model (butlast stimulus) :delay delay :trace trace))
            (let* ((decision (if (< trial init) (random-decision stimulus) (funcall decision :delay delay :trace trace))) ;;; call behavior model to make decision after initial random trials
                   (landing (if (< (random 1.0) probability) 1.0 0.0)) ;;; assume probability is true estimate but represent outcome in probability terms as well to generate estimate
                   (damage (max 0 (min 5 (round (+ intensity (noise damage-noise)))))) ;;; assume normal noise in intensity
                   (instance (list (list 'decision decision)
                                   (list 'landing landing)
                                   (list 'damage damage))))
              ;;; calls model learning functions
              (learn-trust (copy-tree (append (last stimulus) (list (first instance)))))
              (learn-control (copy-tree (append (butlast stimulus) (list (first instance)))))
              (setf instance (learn-outcome (copy-tree (append (butlast stimulus) (rest instance)))))
            (when (>= trial init) (push instance answers))))) ;;; only save actual trials not inits
        (push (reverse answers) results)))
    (reverse results)))
|#

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

(defun run-batch (&key (samples 100) (decision-function 'model-free-decision) (probability-thresholds '(0.0 0.1 0.25 0.35 0.5)) (intensity-thresholds '(0 1 2 3)))
  "Runs a batch of samples with model-free-decision sweeping probability and utility thresholds."
  (let ((probability-threshold *probability-threshold*)
        (intensity-threshold *intensity-threshold*))
    (format t "THRESHOLD ~C EVACUATION ~C COMPLIANCE ~C UTILITY ~C ACCURACY~%" #\tab #\tab #\tab #\tab)
    (dolist (threshold probability-thresholds)
      (setf *probability-threshold* threshold)
      (format t "~10,3F" threshold)
      (multiple-value-bind (evacuation compliance utility accuracy)
                           (evaluate (simulate :runs samples :decision-function decision-function))
        (format t "~C~10,3F~C~10,3F~C~10,3F~C~10,3F~%" #\tab evacuation #\tab compliance #\tab utility #\tab accuracy)))
    (setf *probability-threshold* probability-threshold)
    (dolist (threshold intensity-thresholds)
      (setf *intensity-threshold* threshold)
      (format t "~10D" threshold)
      (multiple-value-bind (evacuation compliance utility accuracy)
                           (evaluate (simulate :runs samples :decision-function decision-function))
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
    
(defparameter *models* '(model-free-decision model-based-decision theory-planned-behavior trust-calibration))
    
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
           (apply 'simulate :decision-function (second (first parameters)) :parameters translated-parameters))
         (apply 'simulate :parameters translated-parameters)))))


#|
;;; v 0.3.3
? (evaluate (simulate :runs 100 :length 100 :probability-threshold 0.25 :intensity-threshold 1 :decision 'model-based-decision))
0.5769
0.8278
1.0659
0.6248
#(55 47 60 59 53 53 53 57 53 60 55 57 57 64 52 52 59 53 58 59 64 59 60 59 56 66 56 56 55 55 59 62 68 60 51 62 58 45 61 51 63 64 59 60 59 53 66 65 59 63 61 54 54 54 66 53 60 57 54 57 61 58 54 60 59 55 55 65 65 58 66 65 46 60 56 59 57 63 50 52 53 60 63 54 57 57 56 64 58 56 59 59 55 55 50 55 65 50 64 55)
#(71 68 65 70 71 72 75 76 73 72 67 75 76 79 77 70 74 79 82 80 79 83 74 77 82 78 84 83 82 84 83 84 76 85 78 81 85 74 89 85 86 80 83 85 81 84 81 79 86 84 81 86 82 78 88 82 85 82 91 87 89 83 86 84 88 91 82 93 92 81 90 88 84 89 85 87 88 83 85 87 87 88 86 86 91 89 86 89 94 91 83 96 90 89 89 87 84 92 92 85)
? 
;;; v 1.0.2
? (evaluate (simulate :runs 1 :length 100 :probability-threshold 0.25 :intensity-threshold 1 :decision-function 'model-free-decision))
0.45
0.75
0.89
0.6
#(0 0 0 0 0 0 0 0 1 0 0 0 1 0 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 1 0 1 1 0 1 0 1 0 0 0 0 1 1 1 1 1 1 1 0 1 0 1 0 1 0 0 1 1 0 1 1 1 0 1 1 1 1 0 1 1 0 1 0 1 1 0 1 0 1 1 0 0 0 1 1 1 0 1 1 1 1 0 0 0 0 1 0)
#(0 0 0 1 1 0 0 0 1 1 1 0 1 1 1 0 1 1 0 0 1 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 0 0 1 0 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0 1 0 1 0 1 1)
? (evaluate (simulate :runs 10 :length 100 :probability-threshold 0.25 :intensity-threshold 1 :decision-function 'model-free-decision))
0.562
0.773
0.941
0.606
#(7 7 7 6 7 9 5 6 7 3 2 6 6 3 5 1 4 6 6 7 6 7 7 6 6 6 5 7 8 5 6 4 8 6 5 4 5 8 7 4 5 7 7 3 5 3 8 8 5 5 7 7 4 7 7 4 4 2 7 5 4 7 5 6 6 8 4 3 9 7 5 6 4 4 4 6 5 5 5 8 6 4 5 6 5 5 5 6 3 6 4 6 6 7 8 6 6 8 3 6)
#(7 5 8 7 7 4 8 9 7 6 6 7 8 3 6 4 6 9 7 7 8 7 5 6 6 7 9 9 10 8 7 7 7 10 7 7 8 8 8 6 5 8 10 9 8 7 8 9 7 8 10 6 8 10 8 10 9 9 7 10 8 9 8 8 8 8 9 7 9 8 8 9 8 7 8 9 7 9 10 8 10 9 8 8 9 7 8 9 8 7 7 6 8 8 7 6 10 9 8 9)
? (evaluate (simulate :runs 100 :length 100 :probability-threshold 0.25 :intensity-threshold 1 :decision-function 'model-free-decision))
0.5441
0.7802
0.9714
0.6025
#(56 60 46 59 56 55 58 61 55 51 51 53 53 52 53 59 51 53 62 55 59 45 61 61 61 55 53 59 56 50 49 60 54 64 52 58 52 60 55 50 60 57 53 56 48 52 58 49 54 61 53 57 55 54 57 46 62 52 59 57 44 52 55 44 57 54 64 52 56 56 44 53 53 51 56 52 42 46 51 56 45 57 60 53 60 61 53 51 52 57 51 53 60 61 55 54 43 62 58 44)
#(69 65 67 73 58 64 68 68 73 70 75 74 72 76 72 72 76 81 81 78 83 73 69 77 83 85 76 80 80 75 78 76 78 79 78 83 73 79 78 77 81 80 84 81 77 81 77 72 76 80 80 81 82 75 78 76 73 83 80 79 79 82 79 80 87 79 80 75 73 84 80 78 81 82 82 73 81 82 84 86 73 86 90 81 84 79 82 79 82 82 76 82 86 81 80 78 81 84 82 79)
? (evaluate (simulate :runs 100 :length 100 :probability-threshold 0.25 :intensity-threshold 1 :decision-function 'model-based-decision))
0.5792
0.8071
1.0327
0.6172
#(60 58 54 58 56 59 61 60 60 59 63 53 56 54 60 58 57 56 55 50 67 64 59 63 58 60 66 65 64 54 58 66 47 50 51 62 51 54 56 64 62 57 60 56 50 59 55 58 64 57 61 63 67 50 51 67 61 60 59 67 46 52 50 58 60 58 59 53 50 58 58 59 62 55 63 56 55 62 62 56 56 61 64 54 60 54 59 52 57 55 56 57 60 61 66 57 58 48 58 57)
#(67 66 60 65 72 78 73 70 75 72 78 73 66 72 71 68 68 77 78 74 81 75 74 76 85 79 88 72 76 88 84 78 75 75 83 86 74 82 80 83 74 85 89 80 83 77 82 81 90 83 86 87 84 90 83 81 83 86 81 87 79 77 84 76 82 85 87 81 83 80 89 82 86 87 86 83 86 88 79 90 87 87 90 89 82 85 83 85 89 81 89 80 87 88 84 86 81 82 89 88)
? (evaluate (simulate :runs 1 :length 100 :probability-threshold 0.25 :intensity-threshold 1 :decision-function 'theory-planned-behavior))
0.0
0.0
0.02
0.52
#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)


|#
