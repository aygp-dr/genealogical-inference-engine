#!/usr/bin/env guile
!#
;;; test-evidence.scm -- Tests for evidence types and collector
;;; Copyright (C) 2025 Aidan Pace

(add-to-load-path "..")
(use-modules (evidence types)
             (evidence collector)
             (srfi srfi-64))

(test-begin "evidence-tests")

;;; Test date range
(test-group "date-range"
  (define range1 (make-date-range 1980 1990))
  (define range2 (make-date-range 1985 1995))
  (define range3 (make-date-range 2000 2010))
  
  (test-assert "date-range creation" (date-range? range1))
  (test-equal "date-range start" 1980 (date-range-start range1))
  (test-equal "date-range end" 1990 (date-range-end range1))
  
  (test-assert "date-range contains" (date-range-contains? range1 1985))
  (test-assert "date-range not contains" (not (date-range-contains? range1 1995)))
  
  (test-assert "date-range overlap" (date-range-overlap? range1 range2))
  (test-assert "date-range no overlap" (not (date-range-overlap? range1 range3))))

;;; Test evidence creation
(test-group "evidence-creation"
  (define ev1 (create-evidence 
                #:type 'photographic
                #:relationship-hints '("family_photo")
                #:temporal-constraints (make-date-range 1980 1990)
                #:confidence 0.8
                #:source "photo-album-1"))
  
  (test-assert "evidence creation" (evidence? ev1))
  (test-equal "evidence type" 'photographic (evidence-type ev1))
  (test-equal "evidence confidence" 0.8 (evidence-confidence ev1))
  (test-equal "evidence source" "photo-album-1" (evidence-source ev1))
  
  ;; Test invalid evidence type
  (test-error "invalid evidence type" 
              (create-evidence #:type 'invalid 
                              #:confidence 0.5 
                              #:source "test"))
  
  ;; Test invalid confidence
  (test-error "invalid confidence" 
              (create-evidence #:type 'photographic 
                              #:confidence 1.5 
                              #:source "test")))

;;; Test evidence collector
(test-group "evidence-collector"
  (define collector (create-evidence-collector))
  (define ev1 (create-evidence #:type 'photographic #:confidence 0.8 #:source "test1"))
  (define ev2 (create-evidence #:type 'documentary #:confidence 0.9 #:source "test2"))
  (define ev3 (create-evidence #:type 'photographic #:confidence 0.6 #:source "test3"))
  
  (test-assert "collector creation" (evidence-collector? collector))
  (test-equal "empty collector" '() (get-evidence collector))
  
  (add-evidence! collector ev1)
  (add-evidence! collector ev2)
  (add-evidence! collector ev3)
  
  (test-equal "collector size" 3 (length (get-evidence collector)))
  
  (test-equal "filter by type" 2 
              (length (filter-evidence-by-type collector 'photographic)))
  
  (test-equal "filter by confidence" 2 
              (length (filter-evidence-by-confidence collector 0.7))))

;;; Test evidence merging
(test-group "evidence-merging"
  (define collector1 (create-evidence-collector))
  (define collector2 (create-evidence-collector))
  (define ev1 (create-evidence #:type 'photographic #:confidence 0.8 #:source "c1"))
  (define ev2 (create-evidence #:type 'documentary #:confidence 0.9 #:source "c2"))
  
  (add-evidence! collector1 ev1)
  (add-evidence! collector2 ev2)
  
  (define merged (merge-evidence collector1 collector2))
  (test-equal "merged collector size" 2 (length (get-evidence merged))))

(test-end "evidence-tests")