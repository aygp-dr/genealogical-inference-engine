;;; types.scm -- Evidence type definitions for genealogical inference
;;; Copyright (C) 2025 Aidan Pace

(define-module (evidence types)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:export (make-evidence
            evidence?
            evidence-type
            evidence-relationship-hints
            evidence-temporal-constraints
            evidence-confidence
            evidence-source
            make-date-range
            date-range?
            date-range-start
            date-range-end
            date-range-contains?
            date-range-overlap?))

;;; Date range record type
(define-record-type <date-range>
  (make-date-range start end)
  date-range?
  (start date-range-start)
  (end date-range-end))

(define (date-range-contains? range date)
  "Check if DATE falls within RANGE."
  (and (>= date (date-range-start range))
       (<= date (date-range-end range))))

(define (date-range-overlap? range1 range2)
  "Check if two date ranges overlap."
  (and (<= (date-range-start range1) (date-range-end range2))
       (<= (date-range-start range2) (date-range-end range1))))

;;; Evidence record type
(define-record-type <evidence>
  (make-evidence type relationship-hints temporal-constraints confidence source)
  evidence?
  (type evidence-type)
  (relationship-hints evidence-relationship-hints)
  (temporal-constraints evidence-temporal-constraints)
  (confidence evidence-confidence)
  (source evidence-source))

;;; Evidence type validation
(define (valid-evidence-type? type)
  "Check if TYPE is a valid evidence type."
  (member type '(photographic documentary biological cultural technological temporal)))

;;; Evidence constructors with validation
(define* (create-evidence #:key type relationship-hints temporal-constraints confidence source)
  "Create a new evidence record with validation."
  (unless (valid-evidence-type? type)
    (error "Invalid evidence type" type))
  (unless (and (number? confidence) (>= confidence 0) (<= confidence 1))
    (error "Confidence must be between 0 and 1" confidence))
  (make-evidence type relationship-hints temporal-constraints confidence source))