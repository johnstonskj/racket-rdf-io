#lang racket/base

(require racket/bool
         racket/function
         racket/list
         racket/set
         racket/string
         ;; --------------------------------------
         net/url-string
         net/url-structs
         ;; --------------------------------------
         json
         ;; --------------------------------------
         rdf/core/graph
         rdf/core/literal
         rdf/core/nsmap
         rdf/core/statement
         rdf/core/triple
         rdf/core/tree
         ;; --------------------------------------
         media-type
         ;; --------------------------------------
         "./base.rkt")

(provide json-representation)

;; In general, a triple (subject S, predicate P, object O) is serialized in the following structure:
;;
;; { "S" : { "P" : [ O ] } }
;;
;; The object of the triple O is represented as a further JSON object with the following keys:
;;
;; - type :: one of 'uri', 'literal' or 'bnode' (required)
;; - value :: the URI of the object, its lexical value or a blank node label depending on whether the object is a
;;   uri, literal or bnode
;; - lang :: the language of a literal value (optional but if supplied it must not be empty)
;; - datatype :: the datatype URI of the literal value (optional)
;;
;; Blank node subjects are named using a string conforming to the nodeID production in Turtle. For example: _:A1
;;
;; The 'lang' and 'datatype' keys should only be used if the value of the 'type' key is "literal".

(define (object->json object)
  (make-hash (cond
               ((url? object)
               (list (cons 'uri (url->string object))))
               ((blank-node? object)
                (list (cons 'bnode (format "_:~a" (blank-node->string object)))))
               ((literal? object)
                (let ((literal (list (cons 'value (literal-lexical-form object)))))
                  (cond
                    ((has-language-tag? object)
                     (cons (cons 'lang (literal-language-tag object)) literal))
                    ((has-datatype-iri? object)
                     (cons (cons 'datatype (url->string (literal-datatype-iri object))) literal))
                    (else literal)))))))

(define (predicate->json predicate objects)
  (cons (string->symbol (url->string predicate))
        (map object->json objects)))

(define (subject->json subject predicates)
  (cons (cond
          ((url? subject)
           (string->symbol (url->string subject)))
          ((blank-node? subject)
           (format "_:~a" (blank-node->string subject))))
        (make-hash (hash-map predicates predicate->json))))

(define (tree->json tree)
  (make-hash (hash-map tree subject->json)))

(define (json->object v)
  (let ((type (hash-ref v 'type #f))
        (value (hash-ref v 'value))
        (lang (hash-ref v 'lang #f))
        (datatype (hash-ref v 'datatype #f)))
    (cond
      ((false? type)
       (make-untyped-literal value))
      ((symbol=? type 'uri) (string->url value))
      ((symbol=? type 'bnode) (make-blank-node (substring value 2)))
      ((symbol=? type 'literal)
       (cond
         (lang (make-lang-string-literal value lang))
         (datatype (make-typed-literal value datatype))
         (else (make-untyped-literal value))))
      (else (raise-argument-error 'object-type "type?" type)))
    ))

(define (json-predicate-map->list subject predicate objects)
  (let ((predicate (string->url (symbol->string predicate))))
    (map (λ (object)
           (triple subject predicate (json->object object)))
         objects)))

(define (json->subject v)
  (let ((v (symbol->string v)))
    (if (string-prefix? v "_:") (make-blank-node (substring v 2)) (string->url v))))

(define (json-statement-map->statement-list subject predicate-objects)
  (let ((subject (json->subject subject)))
    (hash-map predicate-objects
              (curry json-predicate-map->list subject))))

;; -------------------------------------------------------------------------------------------------

(define (json-read (inp (current-input-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (let ((data (read-json inp)))
    (if (hash? data)
        (unnamed-graph
         (flatten
          (hash-map data json-statement-map->statement-list)))
        (raise-argument-error 'rdf-json "hash?" data))
    ))

;; -------------------------------------------------------------------------------------------------

(define (json-write-graph graph (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (write-json (tree->json (graph->tree graph)) out)
  (newline out))

(define json-representation
  (representation
   'json
   "RDF/JSON"
   (string->media-type "application/rdf+json")
   '("rj")
   json-read
   #f
   json-write-graph))
