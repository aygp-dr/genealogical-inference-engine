;;; person.scm -- Person and relationship models
;;; Copyright (C) 2025 Aidan Pace

(define-module (models person)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (make-person
            person?
            person-id
            person-name
            person-birth-date
            person-death-date
            person-attributes
            make-relationship-hypothesis
            relationship-hypothesis?
            relationship-hypothesis-subject-a
            relationship-hypothesis-subject-b
            relationship-hypothesis-type
            relationship-hypothesis-probability
            relationship-hypothesis-supporting-evidence
            valid-relationship-type?))

;;; Person record type
(define-record-type <person>
  (make-person id name birth-date death-date attributes)
  person?
  (id person-id)
  (name person-name)
  (birth-date person-birth-date)
  (death-date person-death-date)
  (attributes person-attributes))

;;; Relationship hypothesis record type
(define-record-type <relationship-hypothesis>
  (make-relationship-hypothesis subject-a subject-b type probability supporting-evidence)
  relationship-hypothesis?
  (subject-a relationship-hypothesis-subject-a)
  (subject-b relationship-hypothesis-subject-b)
  (type relationship-hypothesis-type)
  (probability relationship-hypothesis-probability)
  (supporting-evidence relationship-hypothesis-supporting-evidence))

;;; Valid relationship types
(define (valid-relationship-type? type)
  "Check if TYPE is a valid relationship type."
  (member type '(parent child grandparent grandchild 
                 great-grandparent great-grandchild
                 sibling half-sibling step-sibling
                 spouse cousin aunt uncle niece nephew)))

;;; Relationship inference helpers
(define (infer-reciprocal-relationship type)
  "Given a relationship TYPE, return the reciprocal relationship."
  (match type
    ('parent 'child)
    ('child 'parent)
    ('grandparent 'grandchild)
    ('grandchild 'grandparent)
    ('great-grandparent 'great-grandchild)
    ('great-grandchild 'great-grandparent)
    ('sibling 'sibling)
    ('half-sibling 'half-sibling)
    ('step-sibling 'step-sibling)
    ('spouse 'spouse)
    ('cousin 'cousin)
    ('aunt 'niece)
    ('uncle 'nephew)
    ('niece 'aunt)
    ('nephew 'uncle)
    (_ #f)))

;;; Age-based relationship validation
(define (validate-age-difference person-a person-b relationship-type)
  "Validate if the age difference between persons is consistent with the relationship type."
  (let ((birth-a (person-birth-date person-a))
        (birth-b (person-birth-date person-b)))
    (when (and birth-a birth-b)
      (let ((age-diff (- birth-b birth-a)))
        (match relationship-type
          ('parent (and (> age-diff 12) (< age-diff 70)))
          ('grandparent (and (> age-diff 30) (< age-diff 100)))
          ('great-grandparent (and (> age-diff 50) (< age-diff 130)))
          ('sibling (and (> age-diff -20) (< age-diff 20)))
          ('spouse (and (> age-diff -40) (< age-diff 40)))
          (_ #t))))))