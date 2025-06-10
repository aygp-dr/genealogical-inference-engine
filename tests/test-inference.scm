#!/usr/bin/env guile
!#
;;; test-inference.scm -- Tests for inference engine
;;; Copyright (C) 2025 Aidan Pace

(add-to-load-path "..")
(use-modules (evidence types)
             (models person)
             (inference engine)
             (inference bayesian)
             (srfi srfi-64))

(test-begin "inference-tests")

;;; Test Bayesian utilities
(test-group "bayesian-utilities"
  (test-approximate "bayesian update" 0.8 
                   (bayesian-update 0.5 0.8) 0.01)
  
  (test-equal "conditional probability" 0.3 
              (conditional-probability 0.6 0.5))
  
  (test-approximate "joint probability" 0.12 
                   (joint-probability 0.3 0.4) 0.01)
  
  ;; Test relationship likelihood
  (test-assert "photographic parent likelihood" 
               (> (relationship-likelihood 'photographic 'parent) 0.7))
  (test-assert "documentary parent likelihood" 
               (> (relationship-likelihood 'documentary 'parent) 0.9)))

;;; Test inference engine creation
(test-group "engine-creation"
  (define engine (create-inference-engine))
  
  (test-assert "engine creation" (inference-engine? engine))
  
  ;; Add some evidence
  (define ev1 (create-evidence 
                #:type 'photographic
                #:relationship-hints '("elderly_couple")
                #:temporal-constraints (make-date-range 1980 1990)
                #:confidence 0.8
                #:source "test"))
  
  (add-evidence! engine ev1)
  (test-equal "evidence added" 1 
              (length (get-evidence (engine-evidence-collector engine)))))

;;; Test relationship inference
(test-group "relationship-inference"
  (define engine (create-inference-engine))
  
  ;; Create test evidence
  (define ev1 (create-evidence 
                #:type 'temporal
                #:relationship-hints '("person_a")
                #:temporal-constraints (make-date-range 1950 1960)
                #:confidence 0.9
                #:source "birth_record"))
  
  (define ev2 (create-evidence 
                #:type 'temporal
                #:relationship-hints '("person_b")
                #:temporal-constraints (make-date-range 1975 1985)
                #:confidence 0.9
                #:source "birth_record"))
  
  (add-evidence! engine (list ev1 ev2))
  
  ;; Infer relationships
  (define relationships (infer-relationships engine))
  (test-assert "relationships inferred" (list? relationships)))

;;; Test probability calculation
(test-group "probability-calculation"
  (define engine (create-inference-engine))
  (define p1 (make-person "P001" "Person A" 1950 #f '()))
  (define p2 (make-person "P002" "Person B" 1975 #f '()))
  
  (define prob (calculate-relationship-probability engine p1 p2 'parent))
  (test-assert "probability is number" (number? prob))
  (test-assert "probability in range" (and (>= prob 0) (<= prob 1))))

;;; Test base probability
(test-group "base-probability"
  (test-approximate "parent base prob" 0.2 (calculate-base-probability 'parent) 0.01)
  (test-approximate "grandparent base prob" 0.1 (calculate-base-probability 'grandparent) 0.01)
  (test-approximate "sibling base prob" 0.15 (calculate-base-probability 'sibling) 0.01)
  (test-approximate "cousin base prob" 0.05 (calculate-base-probability 'cousin) 0.01))

(test-end "inference-tests")