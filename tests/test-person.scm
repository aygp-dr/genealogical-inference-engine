#!/usr/bin/env guile
!#
;;; test-person.scm -- Tests for person and relationship models
;;; Copyright (C) 2025 Aidan Pace

(add-to-load-path "..")
(use-modules (models person)
             (srfi srfi-64))

(test-begin "person-tests")

;;; Test person creation
(test-group "person-creation"
  (define p1 (make-person "P001" "John Doe" 1950 2020 '((occupation . "teacher"))))
  (define p2 (make-person "P002" "Jane Doe" 1955 #f '((occupation . "doctor"))))
  
  (test-assert "person creation" (person? p1))
  (test-equal "person id" "P001" (person-id p1))
  (test-equal "person name" "John Doe" (person-name p1))
  (test-equal "person birth" 1950 (person-birth-date p1))
  (test-equal "person death" 2020 (person-death-date p1))
  (test-equal "person attributes" '((occupation . "teacher")) (person-attributes p1))
  
  (test-assert "person without death date" (not (person-death-date p2))))

;;; Test relationship hypothesis
(test-group "relationship-hypothesis"
  (define p1 (make-person "P001" "Parent" 1950 #f '()))
  (define p2 (make-person "P002" "Child" 1975 #f '()))
  (define evidence '())
  
  (define rel (make-relationship-hypothesis p1 p2 'parent 0.85 evidence))
  
  (test-assert "relationship creation" (relationship-hypothesis? rel))
  (test-equal "relationship subject-a" p1 (relationship-hypothesis-subject-a rel))
  (test-equal "relationship subject-b" p2 (relationship-hypothesis-subject-b rel))
  (test-equal "relationship type" 'parent (relationship-hypothesis-type rel))
  (test-equal "relationship probability" 0.85 (relationship-hypothesis-probability rel)))

;;; Test relationship type validation
(test-group "relationship-types"
  (test-assert "valid parent" (valid-relationship-type? 'parent))
  (test-assert "valid grandparent" (valid-relationship-type? 'grandparent))
  (test-assert "valid sibling" (valid-relationship-type? 'sibling))
  (test-assert "valid spouse" (valid-relationship-type? 'spouse))
  (test-assert "invalid type" (not (valid-relationship-type? 'friend))))

;;; Test reciprocal relationships
(test-group "reciprocal-relationships"
  (test-equal "parent reciprocal" 'child (infer-reciprocal-relationship 'parent))
  (test-equal "child reciprocal" 'parent (infer-reciprocal-relationship 'child))
  (test-equal "grandparent reciprocal" 'grandchild (infer-reciprocal-relationship 'grandparent))
  (test-equal "sibling reciprocal" 'sibling (infer-reciprocal-relationship 'sibling))
  (test-equal "spouse reciprocal" 'spouse (infer-reciprocal-relationship 'spouse))
  (test-equal "aunt reciprocal" 'niece (infer-reciprocal-relationship 'aunt))
  (test-equal "uncle reciprocal" 'nephew (infer-reciprocal-relationship 'uncle)))

;;; Test age validation
(test-group "age-validation"
  (define parent (make-person "P001" "Parent" 1950 #f '()))
  (define child (make-person "P002" "Child" 1975 #f '()))
  (define grandchild (make-person "P003" "Grandchild" 2000 #f '()))
  (define sibling (make-person "P004" "Sibling" 1952 #f '()))
  
  (test-assert "valid parent-child age" 
               (validate-age-difference parent child 'parent))
  (test-assert "valid grandparent-grandchild age" 
               (validate-age-difference parent grandchild 'grandparent))
  (test-assert "valid sibling age" 
               (validate-age-difference parent sibling 'sibling))
  
  ;; Test invalid age differences
  (define too-young (make-person "P005" "Too Young" 1960 #f '()))
  (test-assert "invalid parent age (too young)" 
               (not (validate-age-difference too-young child 'parent))))

(test-end "person-tests")