#|
ACT-UP Procedural Module

v0.1 

Basic idea is a working memory module that operates similarly (or even same) as declarative memory
No buffers, no special syntax, leverage declarative memory as much as possible
Leave open whether WM is collection of chunks or just slots

Issues:
- prevent variable binding to nil? Might be tricky without negative test. Maybe omit slot entirely
- merge vs blend for chunk changes? Might be taken care of if using blending for all retrievals
- separate working and declarative memory module? Need top-level object storing memories and parameters
- production compilation? proceduralize retrieval, combine sequential production firings, or both?

v0.2

3/3/25

- add structure to hold declarative and working memories (and similarities) and their parameters
* define model structure
* switch productions from list to hash table and change init-production, production and cycle functions accordingly
* fix execute function to avoid destructive changes


- map production calls to matching working or declarative memory while preserving flexibility


|#

(defparameter *production-module-version* "0.2")

(format t "Loading Procedural Module version ~A~%" *production-module-version*)

(defvar *productions* nil
  "Hash table of productions.")

(defstruct production
  "A production is defined by name, conditions, actions and utility."
  name
  conditions
  actions
  utility)

(defstruct model
  "A model is composed of procedural, declarative, and working memories (and similarities) and their parameters."
  procedural
  declarative
  working
  similarities
  parameters)

(defun init-productions ()
  (setf *productions* (make-hash-table :test #'eql)))

(defun production (name conditions actions &key (utility 0.0) (productions *productions*) (trace *verbose*))
  "Define a production rule in the lightweight syntax."
  (let ((production (make-production :name name
                                     :conditions conditions
                                     :actions actions
                                     :utility utility)))
    (when trace (format t "Creating Production ~A Content ~S.~%" name production))
    (setf (gethash name productions) production))) 

(defmacro p (name conditions actions &key (utility 0.0))
  `(production ',name ',conditions ',actions :utility ,utility))

(defun variablep (value)
  "Determines if value is a variable starting with =."
  (and (symbolp value)
       (eq #\= (schar (symbol-name value) 0))))

(defun split-tests-and-binds (conditions bindings)
  "Split conditions into tests and binds depend on bindings and update bindings."
  (let ((tests nil)
        (binds nil))
    (dolist (condition conditions)
      (if (variablep (second condition))
          (let ((binding (assoc (second condition) bindings)))
            (if binding
                (push (list (first condition) (second binding)) tests)
                (push condition binds)))
          (push condition tests)))
    (values tests binds)))

(defun instantiate (production &key (mode 'exact))
  "Returns instantiation of production ready for execution, or nil if not possible."
  (let ((bindings (list (list 'retrievals)))) ;;; dummy to enable appending of new bindings
    (dolist (condition (production-conditions production) bindings)
      (multiple-value-bind (tests binds) (split-tests-and-binds condition bindings)
        (let ((chunk (retrieve tests :mode mode)))
          (unless chunk (return nil))
          (nconc (first bindings) (list chunk)) ;;; store retrieved chunk in 'retrievals binding
          (dolist (bind binds)
            (nconc bindings (list (list (second bind) (attribute chunk (first bind)))))))))))

(defun execute (production instantiation)
  "Execute instantiation of production. Potentially multiple actions both changing (first chunk) and creating chunk(s) in WM."
  (let ((goal-slots (chunk-content (second (first instantiation))))) ;;; slots of first chunk bound in conditions
    (dolist (action (production-actions production))
      (let ((modify t)
            (pattern nil))
        (dolist (condition action)
          (let ((value (or (second (assoc (second condition) instantiation)) ;;; if variable
                           (second condition)))) ;;; or constant
            (setf pattern (append pattern (list (list (first condition) value))))
            (unless (assoc (first condition) goal-slots)
              (setf modify nil)
              (return))))
        (format t "Modify ~S Pattern ~S~%" modify pattern)
        (if modify
            (let ((new-goal-slots (copy-tree goal-slots)))   ;;; modify rather than destroy chunk representations
              (dolist (slot-value pattern)
                (setf (second (assoc (first slot-value) new-goal-slots)) (second slot-value)))
              (setf (chunk-content (second (first instantiation))) new-goal-slots))
            (learn pattern))))))

(defun cycle (&key (productions *productions*) (trace *verbose*))
  (let ((conflict-set nil))
    (maphash #'(lambda (name production)
                 (let ((instantiation (instantiate production)))
                   (when instantiation
                     (when trace (format t "~%Instantiating production ~A as ~S.~%" name instantiation))
                     (push (list production instantiation (production-utility production)) conflict-set))))
             productions)
    (setf conflict-set (sort conflict-set #'> :key #'third))
    (when trace (format t "~%Conflict set:") (pprint conflict-set))
    (when conflict-set
      (let ((winner (first conflict-set)))
        (when trace (format t "~%Executing:") (pprint (first conflict-set)))
        (execute (first winner) (second winner))
        winner))))

;;; Tests

(defun basic-arithmetic-test (&key (cycles 1) (trace t))
  "Basic test of arithmetic production firing."
  (init-memory)
  (init-similarities)
  (init-productions)
  (learn '((arg1 4) (op +) (arg2 3) (result 7)))
  (learn '((arg1 4) (op +) (arg2 3) (result nil)))
  (p retrieve
     (((arg1 =arg1) (op =op) (arg2 =arg2) (result nil))
      ((arg1 =arg1) (op =op) (arg2 =arg2) (result =result)))
     (((result =result))))
  (dotimes (cycle cycles)
    (format t "~%Cycle: ~D~%" cycle)
    (format t "Memory:")
    (pprint (dump-memory))
    (actr-time 1.0)
    (cycle :trace trace))
  (dump-memory))

(defun model-free-scale-test (&key (cycles 1) (trace t))
  "Basic test of SCALE model-free productions."
  (init-memory)
  (init-similarities)
  (init-productions)
  (learn '((probability 1) (intensity 3) (evacuation yes) (decision yes) (utility 2)))
  (learn '((probability 1) (intensity 3) (evacuation yes) (decision no) (utility -3)))
  (learn '((probability 1) (intensity 3) (evacuation yes) (decision nil) (evac-utility nil) (stay-utility nil) (utility nil)))
  (p evacuate-utility
     (((probability =probability) (intensity =intensity) (evacuation =evacuation) (evac-utility nil))
      ((probability =probability) (intensity =intensity) (evacuation =evacuation) (decision yes) (utility =evac-utility)))
     (((evac-utility =evac-utility))))
   (p stay-utility
     (((probability =probability) (intensity =intensity) (evacuation =evacuation) (stay-utility nil))
      ((probability =probability) (intensity =intensity) (evacuation =evacuation) (decision no) (utility =stay-utility)))
     (((stay-utility =stay-utility))))
   (p evacuate
      (((stay-utility -3) (evac-utility 2) (decision nil)))
      (((decision yes))))
   (p stay
      (((stay-utility 2) (evac-utility -3) (decision nil)))
      (((decision no))))
  (dotimes (cycle cycles)
    (format t "~%Cycle: ~D~%" cycle)
    (format t "Memory:")
    (pprint (dump-memory))
    (actr-time 1.0)
    (cycle :trace trace))
  (dump-memory))

      

#|
? (basic-arithmetic-test)

Conflict set:
((#S(PRODUCTION :NAME RETRIEVE
                :CONDITIONS (((ARG1 =ARG1) (OP =OP) (ARG2 =ARG2) (RESULT NIL))
                             ((ARG1 =ARG1) (OP =OP) (ARG2 =ARG2) (RESULT =RESULT)))
                :ACTIONS (((RESULT =RESULT))) :UTILITY 0.0)
  ((RETRIEVALS #S(CHUNK :NAME MEMORY357513 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT NIL)) :CREATION 0.0 :REFERENCES 1)
    #S(CHUNK :NAME MEMORY357512 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1))
   (=ARG2 3) (=OP +) (=ARG1 4) (=RESULT 7))
  0.0))
Memory:
((MEMORY357512 #S(CHUNK :NAME MEMORY357512 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1))
 (MEMORY357513 #S(CHUNK :NAME MEMORY357513 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1)))
;;; illustrates the impact of stochasticty of retrieval on production firing and the need for a seaprate working memory
? (basic-arithmetic-test :cycles 5)
Cycle: 0
Memory:
((MEMORY357532 #S(CHUNK :NAME MEMORY357532 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1))
 (MEMORY357533 #S(CHUNK :NAME MEMORY357533 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT NIL)) :CREATION 0.0 :REFERENCES 1)))
Conflict set:
((#S(PRODUCTION :NAME RETRIEVE
                :CONDITIONS (((ARG1 =ARG1) (OP =OP) (ARG2 =ARG2) (RESULT NIL))
                             ((ARG1 =ARG1) (OP =OP) (ARG2 =ARG2) (RESULT =RESULT)))
                :ACTIONS (((RESULT =RESULT))) :UTILITY 0.0)
  ((RETRIEVALS #S(CHUNK :NAME MEMORY357533 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT NIL)) :CREATION 0.0 :REFERENCES 1)
    #S(CHUNK :NAME MEMORY357533 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT NIL)) :CREATION 0.0 :REFERENCES 1))
   (=ARG2 3) (=OP +) (=ARG1 4) (=RESULT NIL))
  0.0))Cycle: 1
Memory:
((MEMORY357532 #S(CHUNK :NAME MEMORY357532 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1))
 (MEMORY357533 #S(CHUNK :NAME MEMORY357533 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT NIL)) :CREATION 0.0 :REFERENCES 1)))
Conflict set:
((#S(PRODUCTION :NAME RETRIEVE
                :CONDITIONS (((ARG1 =ARG1) (OP =OP) (ARG2 =ARG2) (RESULT NIL))
                             ((ARG1 =ARG1) (OP =OP) (ARG2 =ARG2) (RESULT =RESULT)))
                :ACTIONS (((RESULT =RESULT))) :UTILITY 0.0)
  ((RETRIEVALS #S(CHUNK :NAME MEMORY357533 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT NIL)) :CREATION 0.0 :REFERENCES 1)
    #S(CHUNK :NAME MEMORY357533 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT NIL)) :CREATION 0.0 :REFERENCES 1))
   (=ARG2 3) (=OP +) (=ARG1 4) (=RESULT NIL))
  0.0))Cycle: 2
Memory:
((MEMORY357532 #S(CHUNK :NAME MEMORY357532 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1))
 (MEMORY357533 #S(CHUNK :NAME MEMORY357533 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT NIL)) :CREATION 0.0 :REFERENCES 1)))
Conflict set:
((#S(PRODUCTION :NAME RETRIEVE
                :CONDITIONS (((ARG1 =ARG1) (OP =OP) (ARG2 =ARG2) (RESULT NIL))
                             ((ARG1 =ARG1) (OP =OP) (ARG2 =ARG2) (RESULT =RESULT)))
                :ACTIONS (((RESULT =RESULT))) :UTILITY 0.0)
  ((RETRIEVALS #S(CHUNK :NAME MEMORY357533 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT NIL)) :CREATION 0.0 :REFERENCES 1)
    #S(CHUNK :NAME MEMORY357532 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1))
   (=ARG2 3) (=OP +) (=ARG1 4) (=RESULT 7))
  0.0))Cycle: 3
Memory:
((MEMORY357532 #S(CHUNK :NAME MEMORY357532 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1))
 (MEMORY357533 #S(CHUNK :NAME MEMORY357533 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1)))
Conflict set:
NILCycle: 4
Memory:
((MEMORY357532 #S(CHUNK :NAME MEMORY357532 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1))
 (MEMORY357533 #S(CHUNK :NAME MEMORY357533 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1)))
Conflict set:
NIL
((MEMORY357532 #S(CHUNK :NAME MEMORY357532 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1)) (MEMORY357533 #S(CHUNK :NAME MEMORY357533 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1)))
;;; updated production implementation
Cycle: 0
Memory:
((MEMORY9 #S(CHUNK :NAME MEMORY9 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1))
 (MEMORY10 #S(CHUNK :NAME MEMORY10 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT NIL)) :CREATION 0.0 :REFERENCES 1)))
Instantiating production RETRIEVE as ((RETRIEVALS #S(CHUNK :NAME MEMORY10 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT NIL)) :CREATION 0.0 :REFERENCES 1) #S(CHUNK :NAME MEMORY9 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1)) (=ARG2 3) (=OP +) (=ARG1 4) (=RESULT 7)).

Conflict set:
((#S(PRODUCTION :NAME RETRIEVE
                :CONDITIONS (((ARG1 =ARG1) (OP =OP) (ARG2 =ARG2) (RESULT NIL)) ((ARG1 =ARG1) (OP =OP) (ARG2 =ARG2) (RESULT =RESULT)))
                :ACTIONS (((RESULT =RESULT))) :UTILITY 0.0)
  ((RETRIEVALS #S(CHUNK :NAME MEMORY10 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT NIL)) :CREATION 0.0 :REFERENCES 1)
    #S(CHUNK :NAME MEMORY9 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1))
   (=ARG2 3) (=OP +) (=ARG1 4) (=RESULT 7))
  0.0))
Cycle: 1
Memory:
((MEMORY9 #S(CHUNK :NAME MEMORY9 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1))
 (MEMORY10 #S(CHUNK :NAME MEMORY10 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1)))
Conflict set:
NIL
Cycle: 2
Memory:
((MEMORY9 #S(CHUNK :NAME MEMORY9 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1))
 (MEMORY10 #S(CHUNK :NAME MEMORY10 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1)))
Conflict set:
NIL
Cycle: 3
Memory:
((MEMORY9 #S(CHUNK :NAME MEMORY9 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1))
 (MEMORY10 #S(CHUNK :NAME MEMORY10 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1)))
Conflict set:
NIL
Cycle: 4
Memory:
((MEMORY9 #S(CHUNK :NAME MEMORY9 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1))
 (MEMORY10 #S(CHUNK :NAME MEMORY10 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1)))
Conflict set:
NIL
((MEMORY9 #S(CHUNK :NAME MEMORY9 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1)) (MEMORY10 #S(CHUNK :NAME MEMORY10 :CONTENT ((ARG1 4) (OP +) (ARG2 3) (RESULT 7)) :CREATION 0.0 :REFERENCES 1)))
;;; model-free SCALE test
? Loading Procedural Module version 0.2
(model-free-scale-test :cycles 5 :trace t)

Cycle: 0
Memory:
((MEMORY169
  #S(CHUNK :NAME MEMORY169 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NO) (UTILITY -3)) :CREATION 0.0
           :REFERENCES 1))
 (MEMORY170
  #S(CHUNK :NAME MEMORY170
           :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NIL) (EVAC-UTILITY NIL) (STAY-UTILITY NIL) (UTILITY NIL))
           :CREATION 0.0 :REFERENCES 1))
 (MEMORY168
  #S(CHUNK :NAME MEMORY168 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION YES) (UTILITY 2)) :CREATION 0.0
           :REFERENCES 1)))
Instantiating production STAY-UTILITY as ((RETRIEVALS #S(CHUNK :NAME MEMORY170 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NIL) (EVAC-UTILITY NIL) (STAY-UTILITY NIL) (UTILITY NIL)) :CREATION 0.0 :REFERENCES 1) #S(CHUNK :NAME MEMORY169 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NO) (UTILITY -3)) :CREATION 0.0 :REFERENCES 1)) (=EVACUATION YES) (=INTENSITY 3) (=PROBABILITY 1) (=STAY-UTILITY -3)).

Instantiating production EVACUATE-UTILITY as ((RETRIEVALS #S(CHUNK :NAME MEMORY170 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NIL) (EVAC-UTILITY NIL) (STAY-UTILITY NIL) (UTILITY NIL)) :CREATION 0.0 :REFERENCES 1) #S(CHUNK :NAME MEMORY168 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION YES) (UTILITY 2)) :CREATION 0.0 :REFERENCES 1)) (=EVACUATION YES) (=INTENSITY 3) (=PROBABILITY 1) (=EVAC-UTILITY 2)).

Conflict set:
((#S(PRODUCTION :NAME EVACUATE-UTILITY
                :CONDITIONS (((PROBABILITY =PROBABILITY) (INTENSITY =INTENSITY) (EVACUATION =EVACUATION) (EVAC-UTILITY NIL))
                             ((PROBABILITY =PROBABILITY) (INTENSITY =INTENSITY) (EVACUATION =EVACUATION) (DECISION YES)
                              (UTILITY =EVAC-UTILITY)))
                :ACTIONS (((EVAC-UTILITY =EVAC-UTILITY))) :UTILITY 0.0)
  ((RETRIEVALS
    #S(CHUNK :NAME MEMORY170
             :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NIL) (EVAC-UTILITY NIL) (STAY-UTILITY NIL)
                       (UTILITY NIL))
             :CREATION 0.0 :REFERENCES 1)
    #S(CHUNK :NAME MEMORY168 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION YES) (UTILITY 2)) :CREATION 0.0
             :REFERENCES 1))
   (=EVACUATION YES) (=INTENSITY 3) (=PROBABILITY 1) (=EVAC-UTILITY 2))
  0.0)
 (#S(PRODUCTION :NAME STAY-UTILITY
                :CONDITIONS (((PROBABILITY =PROBABILITY) (INTENSITY =INTENSITY) (EVACUATION =EVACUATION) (STAY-UTILITY NIL))
                             ((PROBABILITY =PROBABILITY) (INTENSITY =INTENSITY) (EVACUATION =EVACUATION) (DECISION NO)
                              (UTILITY =STAY-UTILITY)))
                :ACTIONS (((STAY-UTILITY =STAY-UTILITY))) :UTILITY 0.0)
  ((RETRIEVALS
    #S(CHUNK :NAME MEMORY170
             :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NIL) (EVAC-UTILITY NIL) (STAY-UTILITY NIL)
                       (UTILITY NIL))
             :CREATION 0.0 :REFERENCES 1)
    #S(CHUNK :NAME MEMORY169 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NO) (UTILITY -3)) :CREATION 0.0
             :REFERENCES 1))
   (=EVACUATION YES) (=INTENSITY 3) (=PROBABILITY 1) (=STAY-UTILITY -3))
  0.0))
Executing:
(#S(PRODUCTION :NAME EVACUATE-UTILITY
               :CONDITIONS (((PROBABILITY =PROBABILITY) (INTENSITY =INTENSITY) (EVACUATION =EVACUATION) (EVAC-UTILITY NIL))
                            ((PROBABILITY =PROBABILITY) (INTENSITY =INTENSITY) (EVACUATION =EVACUATION) (DECISION YES)
                             (UTILITY =EVAC-UTILITY)))
               :ACTIONS (((EVAC-UTILITY =EVAC-UTILITY))) :UTILITY 0.0)
 ((RETRIEVALS
   #S(CHUNK :NAME MEMORY170
            :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NIL) (EVAC-UTILITY NIL) (STAY-UTILITY NIL)
                      (UTILITY NIL))
            :CREATION 0.0 :REFERENCES 1)
   #S(CHUNK :NAME MEMORY168 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION YES) (UTILITY 2)) :CREATION 0.0
            :REFERENCES 1))
  (=EVACUATION YES) (=INTENSITY 3) (=PROBABILITY 1) (=EVAC-UTILITY 2))
 0.0)Modify T Pattern ((EVAC-UTILITY 2))

Cycle: 1
Memory:
((MEMORY169
  #S(CHUNK :NAME MEMORY169 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NO) (UTILITY -3)) :CREATION 0.0
           :REFERENCES 1))
 (MEMORY170
  #S(CHUNK :NAME MEMORY170
           :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NIL) (EVAC-UTILITY 2) (STAY-UTILITY NIL) (UTILITY NIL))
           :CREATION 0.0 :REFERENCES 1))
 (MEMORY168
  #S(CHUNK :NAME MEMORY168 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION YES) (UTILITY 2)) :CREATION 0.0
           :REFERENCES 1)))
Instantiating production STAY-UTILITY as ((RETRIEVALS #S(CHUNK :NAME MEMORY170 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NIL) (EVAC-UTILITY 2) (STAY-UTILITY NIL) (UTILITY NIL)) :CREATION 0.0 :REFERENCES 1) #S(CHUNK :NAME MEMORY169 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NO) (UTILITY -3)) :CREATION 0.0 :REFERENCES 1)) (=EVACUATION YES) (=INTENSITY 3) (=PROBABILITY 1) (=STAY-UTILITY -3)).

Conflict set:
((#S(PRODUCTION :NAME STAY-UTILITY
                :CONDITIONS (((PROBABILITY =PROBABILITY) (INTENSITY =INTENSITY) (EVACUATION =EVACUATION) (STAY-UTILITY NIL))
                             ((PROBABILITY =PROBABILITY) (INTENSITY =INTENSITY) (EVACUATION =EVACUATION) (DECISION NO)
                              (UTILITY =STAY-UTILITY)))
                :ACTIONS (((STAY-UTILITY =STAY-UTILITY))) :UTILITY 0.0)
  ((RETRIEVALS
    #S(CHUNK :NAME MEMORY170
             :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NIL) (EVAC-UTILITY 2) (STAY-UTILITY NIL) (UTILITY NIL))
             :CREATION 0.0 :REFERENCES 1)
    #S(CHUNK :NAME MEMORY169 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NO) (UTILITY -3)) :CREATION 0.0
             :REFERENCES 1))
   (=EVACUATION YES) (=INTENSITY 3) (=PROBABILITY 1) (=STAY-UTILITY -3))
  0.0))
Executing:
(#S(PRODUCTION :NAME STAY-UTILITY
               :CONDITIONS (((PROBABILITY =PROBABILITY) (INTENSITY =INTENSITY) (EVACUATION =EVACUATION) (STAY-UTILITY NIL))
                            ((PROBABILITY =PROBABILITY) (INTENSITY =INTENSITY) (EVACUATION =EVACUATION) (DECISION NO)
                             (UTILITY =STAY-UTILITY)))
               :ACTIONS (((STAY-UTILITY =STAY-UTILITY))) :UTILITY 0.0)
 ((RETRIEVALS
   #S(CHUNK :NAME MEMORY170
            :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NIL) (EVAC-UTILITY 2) (STAY-UTILITY NIL) (UTILITY NIL))
            :CREATION 0.0 :REFERENCES 1)
   #S(CHUNK :NAME MEMORY169 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NO) (UTILITY -3)) :CREATION 0.0
            :REFERENCES 1))
  (=EVACUATION YES) (=INTENSITY 3) (=PROBABILITY 1) (=STAY-UTILITY -3))
 0.0)Modify T Pattern ((STAY-UTILITY -3))

Cycle: 2
Memory:
((MEMORY169
  #S(CHUNK :NAME MEMORY169 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NO) (UTILITY -3)) :CREATION 0.0
           :REFERENCES 1))
 (MEMORY170
  #S(CHUNK :NAME MEMORY170
           :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NIL) (EVAC-UTILITY 2) (STAY-UTILITY -3) (UTILITY NIL))
           :CREATION 0.0 :REFERENCES 1))
 (MEMORY168
  #S(CHUNK :NAME MEMORY168 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION YES) (UTILITY 2)) :CREATION 0.0
           :REFERENCES 1)))
Instantiating production EVACUATE as ((RETRIEVALS #S(CHUNK :NAME MEMORY170 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NIL) (EVAC-UTILITY 2) (STAY-UTILITY -3) (UTILITY NIL)) :CREATION 0.0 :REFERENCES 1))).

Conflict set:
((#S(PRODUCTION :NAME EVACUATE :CONDITIONS (((STAY-UTILITY -3) (EVAC-UTILITY 2) (DECISION NIL))) :ACTIONS (((DECISION YES)))
                :UTILITY 0.0)
  ((RETRIEVALS
    #S(CHUNK :NAME MEMORY170
             :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NIL) (EVAC-UTILITY 2) (STAY-UTILITY -3) (UTILITY NIL))
             :CREATION 0.0 :REFERENCES 1)))
  0.0))
Executing:
(#S(PRODUCTION :NAME EVACUATE :CONDITIONS (((STAY-UTILITY -3) (EVAC-UTILITY 2) (DECISION NIL))) :ACTIONS (((DECISION YES)))
               :UTILITY 0.0)
 ((RETRIEVALS
   #S(CHUNK :NAME MEMORY170
            :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NIL) (EVAC-UTILITY 2) (STAY-UTILITY -3) (UTILITY NIL))
            :CREATION 0.0 :REFERENCES 1)))
 0.0)Modify T Pattern ((DECISION YES))

Cycle: 3
Memory:
((MEMORY169
  #S(CHUNK :NAME MEMORY169 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NO) (UTILITY -3)) :CREATION 0.0
           :REFERENCES 1))
 (MEMORY170
  #S(CHUNK :NAME MEMORY170
           :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION YES) (EVAC-UTILITY 2) (STAY-UTILITY -3) (UTILITY NIL))
           :CREATION 0.0 :REFERENCES 1))
 (MEMORY168
  #S(CHUNK :NAME MEMORY168 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION YES) (UTILITY 2)) :CREATION 0.0
           :REFERENCES 1)))
Conflict set:
NIL
Cycle: 4
Memory:
((MEMORY169
  #S(CHUNK :NAME MEMORY169 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NO) (UTILITY -3)) :CREATION 0.0
           :REFERENCES 1))
 (MEMORY170
  #S(CHUNK :NAME MEMORY170
           :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION YES) (EVAC-UTILITY 2) (STAY-UTILITY -3) (UTILITY NIL))
           :CREATION 0.0 :REFERENCES 1))
 (MEMORY168
  #S(CHUNK :NAME MEMORY168 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION YES) (UTILITY 2)) :CREATION 0.0
           :REFERENCES 1)))
Conflict set:
NIL
((MEMORY169 #S(CHUNK :NAME MEMORY169 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION NO) (UTILITY -3)) :CREATION 0.0 :REFERENCES 1)) (MEMORY170 #S(CHUNK :NAME MEMORY170 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION YES) (EVAC-UTILITY 2) (STAY-UTILITY -3) (UTILITY NIL)) :CREATION 0.0 :REFERENCES 1)) (MEMORY168 #S(CHUNK :NAME MEMORY168 :CONTENT ((PROBABILITY 1) (INTENSITY 3) (EVACUATION YES) (DECISION YES) (UTILITY 2)) :CREATION 0.0 :REFERENCES 1)))

|#

#|
Arithmetic example

;;; maybe make this an architectural default, e.g., predictive coding
(production retrieve
   ((arg1 =arg1) (op =op) (arg2 =arg2) (result nil)) ;;; WM: no need for chunk name
   ((arg1 =arg1) (op =op) (arg2 =arg2) (result =result)) ;;; DM: pattern completion
   ((result =result))) ;;; WM: modify chunk because slow already mentioned

;;; subogaling - less efficient i.e. lower utility than retrieving (but depends on accuracy)
;;; Needs to know that retrieval has failed if selected first, e.g., single cycle
(production compute-times
   ((arg1 =arg1) (op times) (arg2 =arg2) (result nil)) ;;; WM: no need for chunk name
   ((acc 0) (op plus) (inc =arg1) (counter =arg2))) ;;; WM: create chunk because new slots

(production compute-plus
   ((arg1 =arg1) (op plus) (arg2 =arg2) (result nil)) ;;; WM: no need for chunk name
   ((acc =arg1) (op count) (counter =arg2))) ;;; WM: create chunk because new slots

;;; iterate counting and adding
;;; base condition same for both subgoals
;;; could just stop and rely on storing result and retrieving
(production stop-iteration
   ((acc =acc) (counter 0)) ;;; WM: same condition for both cases
   ((arg1 =arg1) (op =op) (arg2 =arg2) (result nil)) ;;; DM: retrieving previous goal - not recursive
   ((arg1 =arg1) (op =op) (arg2 =arg2) (result =acc))) ;;; WM: recreating and filling past goal

(production iterate-plus
   ((acc =acc) (op plus) (inc =arg1) (counter =arg2))

|#