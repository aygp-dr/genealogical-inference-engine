;;; engine.scm -- Core inference engine for genealogical relationships
;;; Copyright (C) 2025 Aidan Pace

(define-module (inference engine)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (evidence types)
  #:use-module (evidence collector)
  #:use-module (models person)
  #:use-module (models family-tree)
  #:use-module (inference bayesian)
  #:export (make-inference-engine
            inference-engine?
            add-evidence!
            infer-relationships
            reconstruct-family-tree
            calculate-relationship-probability))

;;; Inference engine record type
(define-record-type <inference-engine>
  (make-inference-engine evidence-collector family-tree inference-cache)
  inference-engine?
  (evidence-collector engine-evidence-collector)
  (family-tree engine-family-tree)
  (inference-cache engine-inference-cache set-engine-inference-cache!))

;;; Constructor
(define (create-inference-engine)
  "Create a new inference engine."
  (make-inference-engine (create-evidence-collector)
                        (create-family-tree)
                        '()))

;;; Evidence management
(define (add-evidence! engine evidence-or-list)
  "Add evidence to the ENGINE."
  (let ((collector (engine-evidence-collector engine)))
    (if (list? evidence-or-list)
        (for-each (lambda (ev) (add-evidence! collector ev)) evidence-or-list)
        (add-evidence! collector evidence-or-list))
    ;; Clear cache when new evidence is added
    (set-engine-inference-cache! engine '())))

;;; Relationship inference
(define (infer-relationships engine)
  "Infer all possible relationships based on evidence."
  (let ((evidence (get-evidence (engine-evidence-collector engine))))
    (append-map
     (lambda (ev1)
       (append-map
        (lambda (ev2)
          (infer-pairwise-relationships ev1 ev2))
        evidence))
     evidence)))

;;; Pairwise relationship inference
(define (infer-pairwise-relationships ev1 ev2)
  "Infer relationships between two pieces of evidence."
  (let ((hints1 (evidence-relationship-hints ev1))
        (hints2 (evidence-relationship-hints ev2))
        (temporal1 (evidence-temporal-constraints ev1))
        (temporal2 (evidence-temporal-constraints ev2)))
    (cond
     ;; Temporal-based inference
     ((and temporal1 temporal2)
      (infer-from-temporal-constraints temporal1 temporal2 hints1 hints2))
     ;; Hint-based inference
     ((and hints1 hints2)
      (infer-from-hints hints1 hints2))
     (else '()))))

;;; Temporal constraint inference
(define (infer-from-temporal-constraints range1 range2 hints1 hints2)
  "Infer relationships from temporal constraints."
  (let ((age-diff (- (date-range-start range2) (date-range-start range1))))
    (cond
     ((and (> age-diff 15) (< age-diff 50))
      (list (cons 'parent-child 0.7)))
     ((and (> age-diff 40) (< age-diff 80))
      (list (cons 'grandparent-grandchild 0.6)))
     ((and (> age-diff -10) (< age-diff 10))
      (list (cons 'sibling 0.5)))
     (else '()))))

;;; Hint-based inference
(define (infer-from-hints hints1 hints2)
  "Infer relationships from evidence hints."
  (define (match-hints hint-pair)
    (match hint-pair
      ((list "elderly_couple" _) '((spouse . 0.8)))
      ((list "parent_with_child" _) '((parent-child . 0.9)))
      ((list "family_gathering" _) '((family-member . 0.6)))
      (_ '())))
  (append-map match-hints (cartesian-product hints1 hints2)))

;;; Cartesian product helper
(define (cartesian-product lst1 lst2)
  "Compute cartesian product of two lists."
  (append-map (lambda (x)
                (map (lambda (y) (list x y)) lst2))
              lst1))

;;; Relationship probability calculation
(define (calculate-relationship-probability engine person-a person-b relationship-type)
  "Calculate probability of RELATIONSHIP-TYPE between PERSON-A and PERSON-B."
  (let* ((evidence (get-evidence (engine-evidence-collector engine)))
         (relevant-evidence (filter (lambda (ev)
                                     (or (member person-a (evidence-relationship-hints ev))
                                         (member person-b (evidence-relationship-hints ev))))
                                   evidence))
         (base-prob (calculate-base-probability relationship-type))
         (evidence-weight (weight-evidence relevant-evidence)))
    (bayesian-update base-prob evidence-weight)))

;;; Base probability estimation
(define (calculate-base-probability relationship-type)
  "Calculate base probability for a relationship type."
  (match relationship-type
    ('parent 0.2)
    ('grandparent 0.1)
    ('sibling 0.15)
    ('spouse 0.1)
    ('cousin 0.05)
    (_ 0.01)))

;;; Family tree reconstruction
(define (reconstruct-family-tree engine root-person-id evidence-threshold)
  "Reconstruct family tree starting from ROOT-PERSON-ID."
  (let ((tree (engine-family-tree engine))
        (relationships (filter (lambda (rel)
                                (>= (relationship-hypothesis-probability rel) 
                                    evidence-threshold))
                              (infer-relationships engine))))
    ;; Add relationships to tree
    (for-each (lambda (rel) (add-relationship! tree rel)) relationships)
    tree))