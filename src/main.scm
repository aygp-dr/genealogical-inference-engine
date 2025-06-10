#!/usr/bin/env guile
!#
;;; main.scm -- Main entry point for genealogical inference engine
;;; Copyright (C) 2025 Aidan Pace

(use-modules (evidence types)
             (evidence collector)
             (models person)
             (models family-tree)
             (inference engine)
             (ice-9 format)
             (ice-9 getopt-long))

(define (display-help)
  (format #t "Genealogical Inference Engine~%")
  (format #t "Usage: genealogical-inference [options]~%")
  (format #t "~%Options:~%")
  (format #t "  -h, --help           Display this help~%")
  (format #t "  -i, --input FILE     Input evidence file~%")
  (format #t "  -o, --output FILE    Output GEDCOM file~%")
  (format #t "  -t, --threshold NUM  Evidence threshold (0.0-1.0, default: 0.5)~%")
  (format #t "  -v, --verbose        Verbose output~%"))

(define (main args)
  (let* ((option-spec '((help (single-char #\h) (value #f))
                       (input (single-char #\i) (value #t))
                       (output (single-char #\o) (value #t))
                       (threshold (single-char #\t) (value #t))
                       (verbose (single-char #\v) (value #f))))
         (options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (input-file (option-ref options 'input #f))
         (output-file (option-ref options 'output "output.ged"))
         (threshold (string->number (option-ref options 'threshold "0.5")))
         (verbose (option-ref options 'verbose #f)))
    
    (when help-wanted
      (display-help)
      (exit 0))
    
    (when verbose
      (format #t "Genealogical Inference Engine starting...~%"))
    
    ;; Create inference engine
    (define engine (create-inference-engine))
    
    ;; Load evidence from file if provided
    (when input-file
      (when verbose
        (format #t "Loading evidence from ~a...~%" input-file))
      ;; TODO: Implement evidence file loading
      )
    
    ;; Run inference
    (when verbose
      (format #t "Running inference with threshold ~a...~%" threshold))
    
    (define relationships (infer-relationships engine))
    
    (when verbose
      (format #t "Found ~a potential relationships~%" (length relationships)))
    
    ;; Build family tree
    (define tree (reconstruct-family-tree engine "root" threshold))
    
    ;; Export to GEDCOM
    (when verbose
      (format #t "Exporting to ~a...~%" output-file))
    
    (export-gedcom tree output-file)
    
    (when verbose
      (format #t "Complete!~%"))))

;; Run main if called as script
(when (equal? (car (command-line)) (car (program-arguments)))
  (main (command-line)))