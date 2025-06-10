;;; collector.scm -- Evidence collection and management
;;; Copyright (C) 2025 Aidan Pace

(define-module (evidence collector)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (evidence types)
  #:export (make-evidence-collector
            evidence-collector?
            add-evidence!
            get-evidence
            filter-evidence-by-type
            filter-evidence-by-confidence
            merge-evidence))

;;; Evidence collector record type
(define-record-type <evidence-collector>
  (make-evidence-collector evidence-list)
  evidence-collector?
  (evidence-list evidence-collector-list set-evidence-collector-list!))

;;; Constructor
(define (create-evidence-collector)
  "Create a new empty evidence collector."
  (make-evidence-collector '()))

;;; Add evidence
(define (add-evidence! collector evidence)
  "Add EVIDENCE to the COLLECTOR."
  (set-evidence-collector-list! collector
    (cons evidence (evidence-collector-list collector))))

;;; Retrieve evidence
(define (get-evidence collector)
  "Get all evidence from COLLECTOR."
  (evidence-collector-list collector))

;;; Filter evidence by type
(define (filter-evidence-by-type collector type)
  "Filter evidence in COLLECTOR by TYPE."
  (filter (lambda (ev) (eq? (evidence-type ev) type))
          (evidence-collector-list collector)))

;;; Filter evidence by confidence threshold
(define (filter-evidence-by-confidence collector threshold)
  "Filter evidence in COLLECTOR with confidence >= THRESHOLD."
  (filter (lambda (ev) (>= (evidence-confidence ev) threshold))
          (evidence-collector-list collector)))

;;; Merge evidence from multiple collectors
(define (merge-evidence . collectors)
  "Merge evidence from multiple COLLECTORS into a new collector."
  (let ((merged (create-evidence-collector)))
    (for-each
     (lambda (collector)
       (for-each (lambda (ev) (add-evidence! merged ev))
                 (get-evidence collector)))
     collectors)
    merged))

;;; Evidence correlation
(define (correlate-temporal-evidence collector)
  "Find temporal correlations in evidence."
  (let ((temporal-evidence (filter (lambda (ev)
                                    (evidence-temporal-constraints ev))
                                  (get-evidence collector))))
    (map (lambda (ev1)
           (cons ev1
                 (filter (lambda (ev2)
                          (and (not (eq? ev1 ev2))
                               (date-range-overlap? 
                                (evidence-temporal-constraints ev1)
                                (evidence-temporal-constraints ev2))))
                        temporal-evidence)))
         temporal-evidence)))

;;; Evidence weighting
(define (weight-evidence evidence-list)
  "Calculate weighted confidence for a list of evidence."
  (if (null? evidence-list)
      0.0
      (/ (fold + 0 (map evidence-confidence evidence-list))
         (length evidence-list))))