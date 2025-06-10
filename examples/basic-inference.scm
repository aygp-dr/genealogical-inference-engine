#!/usr/bin/env guile
!#
;;; basic-inference.scm -- Basic example of genealogical inference
;;; Copyright (C) 2025 Aidan Pace

(add-to-load-path "..")
(use-modules (evidence types)
             (evidence collector)
             (models person)
             (models family-tree)
             (inference engine)
             (ice-9 format))

(define (main)
  (format #t "~%=== Genealogical Inference Engine - Basic Example ===~%~%")
  
  ;; Create inference engine
  (define engine (create-inference-engine))
  
  ;; Create some test persons
  (define subject (make-person "P001" "Subject (born 2000)" 2000 #f '()))
  (define elderly-man (make-person "P002" "Elderly Man" 1920 #f '((photo . "mystery-roll-1"))))
  (define elderly-woman (make-person "P003" "Elderly Woman" 1925 #f '((photo . "mystery-roll-1"))))
  
  ;; Add photographic evidence
  (format #t "Adding photographic evidence...~%")
  (define photo-evidence 
    (create-evidence 
     #:type 'photographic
     #:relationship-hints '("elderly_couple" "comfortable_posing")
     #:temporal-constraints (make-date-range 1985 1995)
     #:confidence 0.8
     #:source "mystery-family-roll-1"))
  
  ;; Add temporal evidence
  (format #t "Adding temporal evidence...~%")
  (define birth-evidence
    (create-evidence
     #:type 'temporal
     #:relationship-hints '("subject_born_2000")
     #:temporal-constraints (make-date-range 2000 2000)
     #:confidence 1.0
     #:source "known_birth_date"))
  
  ;; Add evidence to engine
  (add-evidence! engine (list photo-evidence birth-evidence))
  
  ;; Infer relationships
  (format #t "~%Inferring relationships...~%")
  (define relationships (infer-relationships engine))
  
  ;; Display results
  (format #t "~%Inferred relationships:~%")
  (for-each
   (lambda (rel)
     (format #t "  ~a <-> ~a: ~a (probability: ~,2f)~%"
             (car rel) (cadr rel) (caddr rel) (cadddr rel)))
   relationships)
  
  ;; Calculate specific relationship probability
  (format #t "~%Calculating specific relationship probabilities:~%")
  (define prob-grandparent 
    (calculate-relationship-probability engine elderly-man subject 'grandparent))
  (format #t "  Probability that Elderly Man is grandparent of Subject: ~,2f~%"
          prob-grandparent)
  
  ;; Reconstruct family tree
  (format #t "~%Reconstructing family tree...~%")
  (define tree (reconstruct-family-tree engine "P001" 0.5))
  
  ;; Add persons to tree
  (add-person! tree subject)
  (add-person! tree elderly-man)
  (add-person! tree elderly-woman)
  
  ;; Export to GEDCOM
  (format #t "Exporting to GEDCOM format...~%")
  (export-gedcom tree "examples/output/inferred-family.ged")
  
  (format #t "~%Example complete! Check examples/output/ for results.~%"))

;; Run the example
(main)