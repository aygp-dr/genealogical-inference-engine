;;; family-tree.scm -- Family tree data structure and operations
;;; Copyright (C) 2025 Aidan Pace

(define-module (models family-tree)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (models person)
  #:export (make-family-tree
            family-tree?
            family-tree-persons
            family-tree-relationships
            add-person!
            add-relationship!
            find-person
            find-relationships
            get-ancestors
            get-descendants
            export-gedcom))

;;; Family tree record type
(define-record-type <family-tree>
  (make-family-tree persons relationships)
  family-tree?
  (persons family-tree-persons set-family-tree-persons!)
  (relationships family-tree-relationships set-family-tree-relationships!))

;;; Constructor
(define (create-family-tree)
  "Create a new empty family tree."
  (make-family-tree '() '()))

;;; Person management
(define (add-person! tree person)
  "Add a PERSON to the family TREE."
  (set-family-tree-persons! tree
    (cons person (family-tree-persons tree))))

(define (find-person tree id)
  "Find a person in TREE by ID."
  (find (lambda (p) (equal? (person-id p) id))
        (family-tree-persons tree)))

;;; Relationship management
(define (add-relationship! tree relationship)
  "Add a RELATIONSHIP to the family TREE."
  (set-family-tree-relationships! tree
    (cons relationship (family-tree-relationships tree))))

(define (find-relationships tree person-id)
  "Find all relationships involving PERSON-ID."
  (filter (lambda (rel)
            (or (equal? (person-id (relationship-hypothesis-subject-a rel)) person-id)
                (equal? (person-id (relationship-hypothesis-subject-b rel)) person-id)))
          (family-tree-relationships tree)))

;;; Ancestry functions
(define (get-ancestors tree person-id)
  "Get all ancestors of PERSON-ID in TREE."
  (define (collect-ancestors id visited)
    (if (member id visited)
        '()
        (let ((parents (filter-map
                        (lambda (rel)
                          (and (equal? (person-id (relationship-hypothesis-subject-b rel)) id)
                               (member (relationship-hypothesis-type rel) 
                                      '(parent grandparent great-grandparent))
                               (> (relationship-hypothesis-probability rel) 0.5)
                               (person-id (relationship-hypothesis-subject-a rel))))
                        (family-tree-relationships tree))))
          (append parents
                  (append-map (lambda (parent-id)
                                (collect-ancestors parent-id (cons id visited)))
                              parents)))))
  (collect-ancestors person-id '()))

(define (get-descendants tree person-id)
  "Get all descendants of PERSON-ID in TREE."
  (define (collect-descendants id visited)
    (if (member id visited)
        '()
        (let ((children (filter-map
                         (lambda (rel)
                           (and (equal? (person-id (relationship-hypothesis-subject-a rel)) id)
                                (member (relationship-hypothesis-type rel) 
                                       '(parent grandparent great-grandparent))
                                (> (relationship-hypothesis-probability rel) 0.5)
                                (person-id (relationship-hypothesis-subject-b rel))))
                         (family-tree-relationships tree))))
          (append children
                  (append-map (lambda (child-id)
                                (collect-descendants child-id (cons id visited)))
                              children)))))
  (collect-descendants person-id '()))

;;; GEDCOM export
(define (export-gedcom tree filename)
  "Export TREE to FILENAME in GEDCOM format."
  (call-with-output-file filename
    (lambda (port)
      (format port "0 HEAD~%")
      (format port "1 GEDC~%")
      (format port "2 VERS 5.5.1~%")
      (format port "1 CHAR UTF-8~%")
      (format port "1 SOUR Genealogical Inference Engine~%")
      
      ;; Export individuals
      (for-each
       (lambda (person)
         (format port "0 @I~a@ INDI~%" (person-id person))
         (format port "1 NAME ~a~%" (person-name person))
         (when (person-birth-date person)
           (format port "1 BIRT~%")
           (format port "2 DATE ~a~%" (person-birth-date person)))
         (when (person-death-date person)
           (format port "1 DEAT~%")
           (format port "2 DATE ~a~%" (person-death-date person))))
       (family-tree-persons tree))
      
      ;; Export families (simplified)
      (format port "0 TRLR~%"))))