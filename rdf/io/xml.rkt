#lang racket/base

(require racket/bool
         racket/function
         racket/list
         racket/set
         ;; --------------------------------------
         net/url-string
         ;; --------------------------------------
         xml
         xml/ns
         ;; --------------------------------------
         rdf/core/dataset
         rdf/core/graph
         rdf/core/literal
         rdf/core/nsname
         rdf/core/nsmap
         rdf/core/resource
         rdf/core/statement
         rdf/core/triple
         rdf/core/v/rdf
         ;; --------------------------------------
         media-type
         ;; --------------------------------------
         "./base.rkt"
         ;; --------------------------------------
         "./private/xmlu.rkt")

(provide xml-representation)

(define representation-name "RDF/XML")

;; -------------------------------------------------------------------------------------------------
;; Internal Reader
;; -------------------------------------------------------------------------------------------------

(define rdf-special-attributes '(about datatype ID nodeID parseType resource))

(define xml-namespace-string "http://www.w3.org/XML/1998/namespace")
(define xml-special-attributes '(base lang space))

(define (special-attribute? att)
  (or (and (string=? (attribute-namespace att) rdf-namespace-string)
           (member (attribute-name att) rdf-special-attributes))
      (and (string=? (attribute-namespace att) xml-namespace-string)
           (member (attribute-name att) xml-special-attributes))))

(define (rdf-attribute? att (name #f))
  (and (string=? (attribute-namespace att) rdf-namespace-string)
       (if name
           (eq? (attribute-name att) name)
           #t)))

(define (rdf-element? elt (name #f))
  (and (string=? (element-namespace elt) rdf-namespace-string)
       (if name
           (eq? (element-name elt) name)
           #t)))

(define (parse-type? att)
  (if (rdf-attribute? att 'parseType)
      (let ((value (attribute-value att)))
        (cond
          ((string=? value "Literal") 'literal)
          ((string=? value "Resource") 'resource)
          ((string=? value "Collection") 'collection)
          (else
           (raise-representation-read-error 'xml "Collection|Literal|Resource" value))))
      #f))

(define (context-from elt (defaults (make-hash '())))
  (let ((context (hash-copy defaults)))
    (for-each
     (λ (att)
       (cond
         ;; TODO: check namespaces
         ((eq? (attribute-name att) 'lang)
          (hash-set! context 'lang (attribute-value att)))
         ((eq? (attribute-name att) 'base)
          (let* ((base-resource-str (attribute-value att))
                 (base-resource (string->resource base-resource-str)))
            (if (resource-absolute? base-resource)
                (hash-set! context 'base base-resource)
                (raise-representation-read-error 'xml "resource-absolute?" base-resource-str))))))
     (element-attributes elt))
    context))

(define (string->absolute-resource s ctx)
  (let ((resource (string->resource s))
        (base (hash-ref ctx 'base #f)))
    (if (resource-absolute? resource)
        resource
        (if base
            (combine-url/relative base s)
            (raise-representation-read-error 'xml "resource-absolute?" s)))))

(define (xmlns-name->resource attr-or-elt)
  (let ((ns+name (cond
    ((attribute? attr-or-elt)
     (list (attribute-namespace attr-or-elt) (attribute-name attr-or-elt)))
    ((element? attr-or-elt)
     (list (element-namespace attr-or-elt) (element-name attr-or-elt)))
    (else #f))))
    (string->resource
     (string-append (car ns+name)
                    (symbol->string (cadr ns+name))))))

(define (attribute->object att ctx)
  (let ((value (attribute-value att))
        (lang (hash-ref ctx 'lang #f)))
    (if lang
        (make-lang-string-literal value lang)
        (make-untyped-literal value))))

(define (property-element->object elt ctx)
  (let ((objects
         (make-hash
          (filter-map
           (λ (att)
             (cond
               ((rdf-attribute? att 'resource)
                (cons 'resource (string->absolute-resource (attribute-value att) ctx)))
               ((rdf-attribute? att 'about)
                (cons 'ID (string->absolute-resource (string-append "#" (attribute-value att)) ctx)))
               ((rdf-attribute? att 'nodeID)
                (cons 'nodeID (make-blank-node (attribute-value att))))
               (else #false)))
           (element-attributes elt))))
        (parse-type (filter-map parse-type? (element-attributes elt))))
    (cond
      ((hash-empty? objects)
       (let ((datatype (filter (λ (att) (rdf-attribute? att 'datatype)) (element-attributes elt)))
             (lang (hash-ref ctx 'lang #f))
             (pcd-string (apply string-append
                                (map
                                 (λ (pcd) (pcdata-string pcd))
                                 (filter pcdata? (element-content elt))))))
         (cond
           ((not (empty? datatype))
            (make-typed-literal pcd-string (string->resource (attribute-value (car datatype)))))
           (lang
            (make-lang-string-literal pcd-string lang))
           (else (make-untyped-literal pcd-string)))))
      ((= (hash-count objects) 1)
       (first (hash-values objects)))
      (else
       (raise-representation-read-error 'xml "resource|ID|nodeID" (format "~s" hash-keys objects))))))

(define (node-element-subject elt ctx)
  (let ((subjects
         (make-hash
          (filter-map
           (λ (att)
             (cond
               ((rdf-attribute? att 'about)
                (cons 'about (string->absolute-resource (attribute-value att) ctx)))
               ((rdf-attribute? att 'ID)
                (cons 'ID (string->absolute-resource (string-append "#" (attribute-value att)) ctx)))
               ((rdf-attribute? att 'nodeID)
                (cons 'nodeID (make-blank-node (attribute-value att))))
               (else #false)))
           (element-attributes elt)))))
    (cond
      ((hash-empty? subjects)
       (make-blank-node))
      ((= (hash-count subjects) 1)
       (first (hash-values subjects)))
      (else
       (raise-representation-read-error 'xml "about|ID|nodeID" (format "~s" hash-keys subjects))))))

(define (node-element->statements elt ctx)
  (let ((description? (rdf-element? elt 'Description))
        (subject (node-element-subject elt ctx)))
    (append
     (if (not description?)
         (list (triple subject
                       (nsname->resource rdf:type)
                       (xmlns-name->resource elt)))
         '())
     (map
      (λ (att) (triple subject (xmlns-name->resource att) (attribute->object att ctx)))
      (filter
       (λ (att) (not (special-attribute? att)))
       (element-attributes elt)))
     (map
      (λ (elt) (triple subject (xmlns-name->resource elt) (property-element->object elt (context-from elt ctx))))
      (filter element? (element-content elt))))))

(define (document->statements doc)
  (let* ((root (document-element doc))
         (root-context (context-from root)))
    (cond
      ((rdf-element? root 'RDF)
       (append* '()
        (map (λ (elt) (node-element->statements elt (context-from elt root-context)))
            (filter element? (element-content root)))))
      (else
       (node-element->statements root root-context)))))

;; -------------------------------------------------------------------------------------------------
;; Internal Writer
;; -------------------------------------------------------------------------------------------------

(define (namespace->string ns)
  (nsname->resource
   (resource->string ns)))

(define (prefix+namespace->attribute prefix+ns)
  (let ((prefix (car prefix+ns))
        (namespace (cdr prefix+ns)))
    (make-xmlns-attribute
     ((namespace->string namespace)
     #:prefix (if prefix (prefix->name-string prefix) prefix)))))

(define (nsmap->xmlns nsmap)
  (let ((initial-list (map prefix+namespace->attribute
                           (nsmap->list nsmap)))
        (rdf-prefix (nsmap-prefix-ref nsmap rdf:)))
    (if (false? rdf-prefix)
        (cons (make-xmlns-attribute
               rdf-namespace-string
               #:prefix (substring rdf-prefix-string
                                   0
                                   (- (string-length rdf-prefix-string) 2)))
              initial-list)
        initial-list)))

(define (type-attributes object)
  (cond
    ((resource? object)
     (list (make-attribute #f #f
                           'rdf:resource
                           (resource->string object))))
    ((blank-node? object)
     (list (make-attribute #f #f
                           'rdf:nodeID
                           (blank-node->string object))))
    ((and (literal? object) (has-language-tag? object))
     (list (make-lang-attribute (literal-language-tag object))))
    ((and (literal? object) (has-datatype-iri? object))
     (list (make-attribute #f #f
                           'rdf:datatype
                           (resource->string (literal-datatype-iri object)))))
    (else '())))

(define (xml-property predicate object nsmap)
  (let* ((ns+name (resource->namespace+name predicate))
         (ns-resource (car ns+name))
         (ns-prefix (nsmap-prefix-ref nsmap (resource-namespace ns-resource)))
         (ns-attr (if (false? ns-prefix)
                      (list (make-xmlns-attribute (resource->string ns-resource)))
                      (list)))
         (name (if (false? ns-prefix)
                   (cdr ns+name)
                   (format "~a~a" (prefix->string ns-prefix) (cdr ns+name)))))
    (make-element
     #f #f
     (string->symbol name)
     (append ns-attr (type-attributes object))
     (if (literal? object)
         (list (make-pcdata #f #f (literal-lexical-form object)))
         '()))))

(define (xml-describe subject statement-list nsmap)
  (let ((subject (if (blank-node? subject)
                     (make-attribute #f #f
                                     'rdf:nodeID (blank-node->string subject))
                     (make-attribute #f #f
                                     'rdf:about (resource->string subject)))))
    (make-element
     #f #f
     'rdf:Description
     (list subject)
     (map (λ (stmt) (xml-property (get-predicate stmt) (get-object stmt) nsmap))
          statement-list))))

(define (graph->document graph nsmap)
  (make-document
   (make-prolog (list (make-xml-declaration "1.1" #:encoding 'UTF-8))
                #f
                '())
   (make-element
    #f #f
    'rdf:RDF
    (nsmap->xmlns (or nsmap (make-common-nsmap)))
    (map (λ (subject)
           (xml-describe subject
                         (graph-filter-by-subject graph subject)
                         nsmap))
         (set->list (graph-distinct-subjects graph))))
   '()))

;; -------------------------------------------------------------------------------------------------
;; Provided Reader, Writer, and Representation
;; -------------------------------------------------------------------------------------------------

(define (xml-read (inp (current-input-port)))
  (let* ((initial (read-xml inp))
         (document (xml-expand-names initial)))
    (document->statements document)))

(define (xml-write-graph graph (out (current-output-port)) #:map (nsmap (make-rdf-only-nsmap)))
  (display-xml (graph->document graph nsmap) out
               #:indentation 'classic)
  (newline out))

(define xml-representation
  (representation
   'xml
   representation-name
   (string->media-type "application/rdf+xml")
   '("xml" "rdf")
   xml-read
   #f
   xml-write-graph))
