#lang racket/base

(require rackunit
         rackunit/text-ui
         ;; --------------------------------------
         net/url-string
         ;; --------------------------------------
         rdf/core/literal
         rdf/core/resource
         rdf/core/statement
         rdf/core/triple
         ;; --------------------------------------
         "./data.rkt")

(provide ntriples-read-test-suite
         ntriples-write-test-suite)

;; -----------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -----------------------------------------------------------------------------------------------

(define ntriples-read-test-suite
  (test-suite
   "Test N-Triples reader"

   (test-case
       "read empty"
     (check-reader "" 'ntriples '()))

   (test-case
       "read URI-URI-URI"
     (check-reader
      "<http://example.com/p/me> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .\n"
      'ntriples
      (list
       (triple
        (string->resource "http://example.com/p/me")
        (string->resource "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
        (string->resource "http://xmlns.com/foaf/0.1/Person")))))

    (test-case
       "read Blank-URI-URI"
     (check-reader
      "_:B1234 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .\n"
      'ntriples
      (list
       (triple
        (make-blank-node "B1234")
        (string->resource "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
        (string->resource "http://xmlns.com/foaf/0.1/Person")))))

   (test-case
       "read URI-URI-Blank"
     (check-reader
      "<http://example.com/p/me> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> _:B1234 .\n"
      'ntriples
      (list
       (triple
        (string->resource "http://example.com/p/me")
        (string->resource "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
        (make-blank-node "B1234")))))

   (test-case
       "read URI-URI-Literal"
     (check-reader
      "<http://example.com/p/me> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> \"Person\" .\n"
      'ntriples
      (list
       (triple
        (string->resource "http://example.com/p/me")
        (string->resource "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
        (make-untyped-literal "Person")))))

   (test-case
       "read URI-URI-Language-Literal"
     (check-reader
      "<http://example.com/p/me> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> \"Person\"@en .\n"
      'ntriples
      (list
       (triple
        (string->resource "http://example.com/p/me")
        (string->resource "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
        (make-lang-string-literal "Person" "en")))))

   (test-case
       "read URI-URI-Typed-Literal"
     (check-reader
      "<http://example.com/p/me> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> \"Person\"^^<http://www.w3.org/2001/XMLSchema#string> .\n"
      'ntriples
      (list
       (triple
        (string->resource "http://example.com/p/me")
        (string->resource "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
        (make-typed-literal "Person" (string->resource "http://www.w3.org/2001/XMLSchema#string"))))))))

(define ntriples-write-test-suite
  (test-suite
   "Test N-Triples writer"

   (test-case
       "empty-graph"
     (check-write-graph *empty-graph* 'ntriples "empty-graph"))

   (test-case
       "empty-named-graph"
     (check-write-graph *empty-named-graph* 'ntriples "empty-named-graph"))

   (test-case
       "example-graph-1"
     (check-write-graph *example-graph-1* 'ntriples "example-graph-1"))))

;; -----------------------------------------------------------------------------------------------
;; Test Runner
;; -----------------------------------------------------------------------------------------------

(run-tests ntriples-read-test-suite)
(run-tests ntriples-write-test-suite)
