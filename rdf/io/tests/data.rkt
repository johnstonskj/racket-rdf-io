#lang racket/base

(require racket/file
         racket/port
         ;; --------------------------------------
         net/url-string
         ;; --------------------------------------
         rackunit
         ;; --------------------------------------
         rdf/core/dataset
         rdf/core/graph
         rdf/core/io
         rdf/core/literal
         rdf/core/nsname
         rdf/core/resource
         rdf/core/triple
         rdf/core/v/rdf
         ;; --------------------------------------
         "../base.rkt"
         "../registry.rkt")

(provide (all-defined-out))

(define *example-language-literal* (make-lang-string-literal "Me" "en"))

(define *example-typed-literal*
  (make-typed-literal
   "42"
   (string->resource "http://www.w3.org/2001/XMLSchema#integer")))

(define *empty-dataset* (unnamed-dataset (make-hash)))

(define *empty-graph* (unnamed-graph '()))

(define *empty-named-graph* (named-graph (string->resource "http://example.org/example/named") '()))

(define *example-graph-1*
  (named-graph
   (string->resource "http://example.com/peeps")
   (make-statements
    "http://example.com/p/me"
    (list (list "http://example.com/v/people#hasFirstName"
                (list *example-language-literal*))
          (list "http://example.com/v/people#hasLastName"
                (list "!"))
          (list "http://example.com/v/people#hasScores"
                (list 2 4 *example-typed-literal*))))))

(define *example-unnamed-graph-1*
  (unnamed-graph
   (make-statements
    "http://example.com/p/me"
    (list (list "http://example.com/v/people#hasFirstName"
                (list *example-language-literal*))
          (list "http://example.com/v/people#hasLastName"
                (list "!"))
          (list "http://example.com/v/people#hasScores"
                (list 2 4 *example-typed-literal*))))))

(define *example-type-statement*
  (triple
   (string->resource "http://example.com/peeps")
   (nsname->resource rdf:type)
   (string->resource "http://xmlns.com/foaf/0.1/Person")))

(define (check-reader test-data-file repr-id (expected #f))
  (let* ((representation (get-representation repr-id))
         (reader (representation-reader representation))
         (actual (with-input-from-string
                   (file->string
                    (format "~a.~a"
                            test-data-file
                            repr-id)
                    #:mode 'text)
                   (λ () (reader)))))
    (when expected
      (check-equal? actual expected))))

(define (check-writer test-data repr-id expected-results-file accessor)
  (let* ((representation (get-representation repr-id))
         (writer (accessor representation))
         (actual (with-output-to-string
                   (λ () (writer test-data))))
         (expected (file->string
                    (format "~a.~a"
                            expected-results-file
                            repr-id)
                    #:mode 'text)))
    (check-equal? actual expected)))

(define (check-write-dataset test-data repr-id expected-results-file)
  (check-writer test-data repr-id expected-results-file representation-dataset-writer))

(define (check-write-graph test-data repr-id expected-results-file)
  (check-writer test-data repr-id expected-results-file representation-graph-writer))
