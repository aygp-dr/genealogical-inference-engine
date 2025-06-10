;;; bayesian.scm -- Bayesian inference utilities
;;; Copyright (C) 2025 Aidan Pace

(define-module (inference bayesian)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (bayesian-update
            bayesian-network
            conditional-probability
            joint-probability
            marginalize))

;;; Basic Bayesian update
(define (bayesian-update prior likelihood)
  "Update PRIOR probability with LIKELIHOOD using Bayes' rule."
  (let ((evidence-prob 0.5)) ; Simplified for now
    (/ (* prior likelihood)
       (+ (* prior likelihood)
          (* (- 1 prior) (- 1 likelihood))))))

;;; Conditional probability
(define (conditional-probability p-a-given-b p-b)
  "Calculate P(A) given P(A|B) and P(B)."
  (* p-a-given-b p-b))

;;; Joint probability
(define (joint-probability . probs)
  "Calculate joint probability of independent events."
  (fold * 1.0 probs))

;;; Marginalization
(define (marginalize joint-probs marginal-var)
  "Marginalize JOINT-PROBS over MARGINAL-VAR."
  (fold + 0.0 (map cdr joint-probs)))

;;; Bayesian network node
(define-record-type <bayes-node>
  (make-bayes-node name parents cpt)
  bayes-node?
  (name node-name)
  (parents node-parents)
  (cpt node-cpt)) ; Conditional Probability Table

;;; Simple Bayesian network
(define (bayesian-network nodes)
  "Create a Bayesian network from NODES."
  (define (find-node name)
    (find (lambda (n) (eq? (node-name n) name)) nodes))
  
  (define (calculate-probability node-name evidence)
    "Calculate probability of NODE-NAME given EVIDENCE."
    (let ((node (find-node node-name)))
      (if node
          (let ((parent-values (map (lambda (p) (assoc-ref evidence p))
                                   (node-parents node))))
            (apply (node-cpt node) parent-values))
          0.0)))
  
  (lambda (query evidence)
    (calculate-probability query evidence)))

;;; Relationship-specific Bayesian calculations
(define (relationship-likelihood evidence-type relationship-type)
  "Calculate likelihood of EVIDENCE-TYPE given RELATIONSHIP-TYPE."
  (match (list evidence-type relationship-type)
    ;; Photographic evidence
    (('photographic 'parent) 0.8)
    (('photographic 'grandparent) 0.7)
    (('photographic 'sibling) 0.75)
    (('photographic 'spouse) 0.85)
    ;; Documentary evidence
    (('documentary 'parent) 0.95)
    (('documentary 'grandparent) 0.9)
    (('documentary 'sibling) 0.9)
    (('documentary 'spouse) 0.95)
    ;; Biological evidence
    (('biological 'parent) 0.99)
    (('biological 'sibling) 0.95)
    (('biological 'grandparent) 0.85)
    ;; Default
    (_ 0.5)))

;;; Evidence combination
(define (combine-evidence-probabilities evidence-list relationship-type)
  "Combine multiple evidence probabilities for RELATIONSHIP-TYPE."
  (let ((likelihoods (map (lambda (ev)
                           (relationship-likelihood 
                            (evidence-type ev) 
                            relationship-type))
                         evidence-list)))
    (if (null? likelihoods)
        0.5
        (/ (fold + 0 likelihoods) (length likelihoods)))))