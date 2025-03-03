#lang racket/base

(require racket/port
         ;; --------------------------------------
         rackunit
         rackunit/text-ui
         ;; --------------------------------------
         net/url-string
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
         rdf/core/io
         ;; --------------------------------------
         "../registry.rkt"
         ;; --------------------------------------
         "./data.rkt")

(provide xml-write-file-test-suite
         xml-read-spec-test-suite)

;; -----------------------------------------------------------------------------------------------
;; Test Helpers
;; -----------------------------------------------------------------------------------------------

(define (wrap-in-rdf content)
  (format #<<EOXML
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:dc="http://purl.org/dc/elements/1.1/"
            xmlns:ex="http://example.org/stuff/1.0/">
  ~a
</rdf:RDF>
EOXML
          content))

(define (check-parser str expected-count (expected-triples #f))
  (with-input-from-string str
    (λ () (let ((stmts (representation-read 'xml)))
            (displayln ">>>>>>>>>>>>>>>>>>>>")
            (for-each write-ntriple-statement stmts)
            (check-equal? (length stmts) expected-count)
            (when expected-triples
              (displayln "--------------------")
              (for-each write-ntriple-statement expected-triples)
              (check-equal? stmts expected-triples))
            (displayln "<<<<<<<<<<<<<<<<<<<<")))))

;; -----------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -----------------------------------------------------------------------------------------------

(define xml-read-spec-test-suite
  (test-suite
   "Test xml-read"

;;   Number	Subject		Predicate								Object
;;   1		genid:A2207	http://example.org/stuff/1.0/editor		genid:A2208
;;   2		genid:A2208	http://example.org/stuff/1.0/homePage	genid:A2209

   (test-case
       "Spec Example 1"
     (check-parser
      (wrap-in-rdf #<<EOXML
<rdf:Description>
  <ex:editor>
    <rdf:Description>
      <ex:homePage>
        <rdf:Description>
        </rdf:Description>
      </ex:homePage>
    </rdf:Description>
  </ex:editor>
</rdf:Description>
EOXML
                   )
      2))

;;   Number	Subject									Predicate								Object
;;   1		http://www.w3.org/TR/rdf-syntax-grammar	http://example.org/stuff/1.0/editor		genid:A2222
;;   2		genid:A2222								http://example.org/stuff/1.0/homePage	http://purl.org/net/dajobe/

   (test-case
       "Spec Example 2"
     (check-parser
      (wrap-in-rdf #<<EOXML
<rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
  <ex:editor>
    <rdf:Description>
      <ex:homePage>
        <rdf:Description rdf:about="http://purl.org/net/dajobe/">
        </rdf:Description>
      </ex:homePage>
    </rdf:Description>
  </ex:editor>
</rdf:Description>
EOXML
                   )
      2))

;;   Number	Subject									Predicate	O							bject
;;   1		http://www.w3.org/TR/rdf-syntax-grammar	http://example.org/stuff/1.0/editor		genid:A2231
;;   2		genid:A2231								http://example.org/stuff/1.0/homePage	http://purl.org/net/dajobe/
;;   3		http://www.w3.org/TR/rdf-syntax-grammar	http://example.org/stuff/1.0/editor		genid:A2232
;;   4		genid:A2232								http://example.org/stuff/1.0/fullName	"Dave Beckett"
;;   5		http://www.w3.org/TR/rdf-syntax-grammar	http://purl.org/dc/elements/1.1/title	"RDF 1.1 XML Syntax"

   (test-case
       "Spec Example 3"
     (check-parser
      (wrap-in-rdf #<<EOXML
<rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
  <ex:editor>
    <rdf:Description>
      <ex:homePage>
        <rdf:Description rdf:about="http://purl.org/net/dajobe/">
        </rdf:Description>
      </ex:homePage>
    </rdf:Description>
  </ex:editor>
</rdf:Description>

<rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
  <ex:editor>
    <rdf:Description>
      <ex:fullName>Dave Beckett</ex:fullName>
    </rdf:Description>
  </ex:editor>
</rdf:Description>

<rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
  <dc:title>RDF 1.1 XML Syntax</dc:title>
</rdf:Description>
EOXML
                   )
      4))

;;   Number	Subject									Predicate								Object
;;   1		http://www.w3.org/TR/rdf-syntax-grammar	http://example.org/stuff/1.0/editor		genid:A2233
;;   2		genid:A2233								http://example.org/stuff/1.0/homePage	http://purl.org/net/dajobe/
;;   3		genid:A2233								http://example.org/stuff/1.0/fullName	"Dave Beckett"
;;   4		http://www.w3.org/TR/rdf-syntax-grammar	http://purl.org/dc/elements/1.1/title	"RDF 1.1 XML Syntax"

   (test-case
       "Spec Example 4"
     (check-parser
      (wrap-in-rdf #<<EOXML
<rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
  <ex:editor>
    <rdf:Description>
      <ex:homePage>
        <rdf:Description rdf:about="http://purl.org/net/dajobe/">
        </rdf:Description>
      </ex:homePage>
      <ex:fullName>Dave Beckett</ex:fullName>
    </rdf:Description>
  </ex:editor>
  <dc:title>RDF 1.1 XML Syntax</dc:title>
</rdf:Description>
EOXML
                   )
      4))

;;   Number	Subject									Predicate								Object
;;   1		http://www.w3.org/TR/rdf-syntax-grammar	http://example.org/stuff/1.0/editor		genid:A2234
;;   2		genid:A2234								http://example.org/stuff/1.0/homePage	http://purl.org/net/dajobe/
;;   3		genid:A2234								http://example.org/stuff/1.0/fullName	"Dave Beckett"
;;   4		http://www.w3.org/TR/rdf-syntax-grammar	http://purl.org/dc/elements/1.1/title	"RDF 1.1 XML Syntax"

   (test-case
       "Spec Example 5"
     (check-parser
      (wrap-in-rdf #<<EOXML
<rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
  <ex:editor>
    <rdf:Description>
      <ex:homePage rdf:resource="http://purl.org/net/dajobe/"/>
      <ex:fullName>Dave Beckett</ex:fullName>
    </rdf:Description>
  </ex:editor>
  <dc:title>RDF 1.1 XML Syntax</dc:title>
</rdf:Description>
EOXML
                   )
      4))

;;   Number	Subject									Predicate								Object
;;   1		http://www.w3.org/TR/rdf-syntax-grammar	http://purl.org/dc/elements/1.1/title	"RDF 1.1 XML Syntax"
;;   2		genid:A2235								http://example.org/stuff/1.0/fullName	"Dave Beckett"
;;   3		http://www.w3.org/TR/rdf-syntax-grammar	http://example.org/stuff/1.0/editor		genid:A2235
;;   4		genid:A2235								http://example.org/stuff/1.0/homePage	http://purl.org/net/dajobe/

   (test-case
       "Spec Example 6"
     (check-parser
      (wrap-in-rdf #<<EOXML
<rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar"
           dc:title="RDF 1.1 XML Syntax">
  <ex:editor>
    <rdf:Description ex:fullName="Dave Beckett">
      <ex:homePage rdf:resource="http://purl.org/net/dajobe/"/>
    </rdf:Description>
  </ex:editor>
</rdf:Description>
EOXML
                   )
      4))

   (test-case
       "Spec Example 7"
     (check-parser
      #<<EOXML
<?xml version="1.0" encoding="utf-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:dc="http://purl.org/dc/elements/1.1/">

  <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
    <dc:title>RDF 1.1 XML Syntax</dc:title>
    <dc:title xml:lang="en">RDF 1.1 XML Syntax</dc:title>
    <dc:title xml:lang="en-US">RDF 1.1 XML Syntax</dc:title>
  </rdf:Description>

  <rdf:Description rdf:about="http://example.org/buecher/baum" xml:lang="de">
    <dc:title>Der Baum</dc:title>
    <dc:description>Das Buch ist außergewöhnlich</dc:description>
    <dc:title xml:lang="en">The Tree</dc:title>
  </rdf:Description>

</rdf:RDF>
EOXML
      6))

   (test-case
       "Spec Example 8"
     (check-parser
      #<<EOXML
< <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
    <dc:title>RDF 1.1 XML Syntax</dc:title>
    <dc:title xml:lang="en">RDF 1.1 XML Syntax</dc:title>
    <dc:title xml:lang="en-US">RDF 1.1 XML Syntax</dc:title>
  </rdf:Description>

  <rdf:Description rdf:about="http://example.org/buecher/baum" xml:lang="de">
    <dc:title>Der Baum</dc:title>
    <dc:description>Das Buch ist außergewöhnlich</dc:description>
    <dc:title xml:lang="en">The Tree</dc:title>
  </rdf:Description>

</rdf:RDF>
EOXML
      6))

   (test-case
       "Spec Example 9"
     (check-parser
      #<<EOXML
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:ex="http://example.org/stuff/1.0/">

  <rdf:Description rdf:about="http://example.org/item01">
    <ex:prop rdf:parseType="Literal" xmlns:a="http://example.org/a#">
      <a:Box required="true">
        <a:widget size="10" />
        <a:grommit id="23" />
      </a:Box>
    </ex:prop>
  </rdf:Description>

</rdf:RDF>
EOXML
      4))

   (test-case
       "Spec Example 10"
     (check-parser
      #<<EOXML
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:ex="http://example.org/stuff/1.0/">

  <rdf:Description rdf:about="http://example.org/item01">
    <ex:size rdf:datatype="http://www.w3.org/2001/XMLSchema#int">123</ex:size>
  </rdf:Description>

</rdf:RDF>
EOXML
      1
      (list
       (triple (string->url "http://example.org/item01")
               (string->url "http://example.org/stuff/1.0/size")
               (make-typed-literal
                "123"
                (string->url "http://www.w3.org/2001/XMLSchema#int"))))))

   (test-case
       "Spec Example 11"
     (check-parser
      #<<EOXML
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:dc="http://purl.org/dc/elements/1.1/"
            xmlns:ex="http://example.org/stuff/1.0/">

  <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar"
             dc:title="RDF 1.1 XML Syntax">
    <ex:editor rdf:nodeID="abc"/>
  </rdf:Description>

  <rdf:Description rdf:nodeID="abc" ex:fullName="Dave Beckett">
    <ex:homePage rdf:resource="http://purl.org/net/dajobe/"/>
  </rdf:Description>

</rdf:RDF>
EOXML
      4))

   (test-case
       "Spec Example 12"
     (check-parser
      #<<EOXML
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:dc="http://purl.org/dc/elements/1.1/"
            xmlns:ex="http://example.org/stuff/1.0/">
  <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar"
                   dc:title="RDF 1.1 XML Syntax">
    <ex:editor rdf:parseType="Resource">
      <ex:fullName>Dave Beckett</ex:fullName>
      <ex:homePage rdf:resource="http://purl.org/net/dajobe/"/>
    </ex:editor>
  </rdf:Description>
</rdf:RDF>
EOXML
      4))

   (test-case
       "Spec Example 13"
     (check-parser
      #<<EOXML
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:dc="http://purl.org/dc/elements/1.1/"
            xmlns:ex="http://example.org/stuff/1.0/">

  <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar"
            dc:title="RDF 1.1 XML Syntax">
    <ex:editor ex:fullName="Dave Beckett" />
            <!-- Note the ex:homePage property has been ignored for this example -->
  </rdf:Description>

</rdf:RDF>
EOXML
      4))

   (test-case
       "Spec Example 14"
     (check-parser
      #<<EOXML
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:dc="http://purl.org/dc/elements/1.1/"
            xmlns:ex="http://example.org/stuff/1.0/">

  <rdf:Description rdf:about="http://example.org/thing">
    <rdf:type rdf:resource="http://example.org/stuff/1.0/Document"/>
    <dc:title>A marvelous thing</dc:title>
  </rdf:Description>
</rdf:RDF>
EOXML
      2
      (list
       (triple (string->resource "http://example.org/thing")
               (nsname->resource rdf:type)
               (string->resource "http://example.org/stuff/1.0/Document"))
       (triple (string->resource "http://example.org/thing")
               (string->resource "http://purl.org/dc/elements/1.1/title")
               (make-untyped-literal "A marvelous thing")))))

   (test-case
       "Spec Example 15"
     (check-parser
      #<<EOXML
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:dc="http://purl.org/dc/elements/1.1/"
            xmlns:ex="http://example.org/stuff/1.0/">

  <ex:Document rdf:about="http://example.org/thing">
    <dc:title>A marvelous thing</dc:title>
  </ex:Document>

</rdf:RDF>
EOXML
      2
      (list
       (triple (string->resource "http://example.org/thing")
               (nsname->resource rdf:type)
               (string->resource "http://example.org/stuff/1.0/Document"))
       (triple (string->resource "http://example.org/thing")
               (string->resource "http://purl.org/dc/elements/1.1/title")
               (make-untyped-literal "A marvelous thing")))))

   (test-case
       "Spec Example 16"
     (check-parser
      #<<EOXML
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:ex="http://example.org/stuff/1.0/"
            xml:base="http://example.org/here/">

  <rdf:Description rdf:ID="snack">
    <ex:prop rdf:resource="fruit/apple"/>
  </rdf:Description>

</rdf:RDF>
EOXML
      1
      (list
       (triple
        (string->resource "http://example.org/here/#snack")
        (string->resource "http://example.org/stuff/1.0/prop")
        (string->resource "http://example.org/here/fruit/apple")))))

   (test-case
       "Spec Example 17"
     (check-parser
      #<<EOXML
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">

  <rdf:Seq rdf:about="http://example.org/favourite-fruit">
    <rdf:_1 rdf:resource="http://example.org/banana"/>
    <rdf:_2 rdf:resource="http://example.org/apple"/>
    <rdf:_3 rdf:resource="http://example.org/pear"/>
  </rdf:Seq>

</rdf:RDF>
EOXML
      4
      (let ((subject (string->resource "http://example.org/favourite-fruit")))
        (list (triple subject (nsname->resource rdf:type) (nsname->resource rdf:Seq))
              (triple subject (nsname->resource (rdf:_ 1)) (string->resource "http://example.org/banana"))
              (triple subject (nsname->resource (rdf:_ 2)) (string->resource "http://example.org/apple"))
              (triple subject (nsname->resource (rdf:_ 3)) (string->resource "http://example.org/pear"))))))

   (test-case
       "Spec Example 18"
     (check-parser
      #<<EOXML
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">

  <rdf:Seq rdf:about="http://example.org/favourite-fruit">
    <rdf:li rdf:resource="http://example.org/banana"/>
    <rdf:li rdf:resource="http://example.org/apple"/>
    <rdf:li rdf:resource="http://example.org/pear"/>
  </rdf:Seq>

</rdf:RDF>
EOXML
      4
      (let ((subject (string->resource "http://example.org/favourite-fruit")))
        (list (triple subject (nsname->resource rdf:type) (nsname->resource rdf:Seq))
              (triple subject (nsname->resource rdf:li) (string->resource "http://example.org/banana"))
              (triple subject (nsname->resource rdf:li) (string->resource "http://example.org/apple"))
              (triple subject (nsname->resource rdf:li) (string->resource "http://example.org/pear"))))))

   (test-case
       "Spec Example 19"
     (check-parser
      #<<EOXML
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:ex="http://example.org/stuff/1.0/">

  <rdf:Description rdf:about="http://example.org/basket">
    <ex:hasFruit rdf:parseType="Collection">
      <rdf:Description rdf:about="http://example.org/banana"/>
      <rdf:Description rdf:about="http://example.org/apple"/>
      <rdf:Description rdf:about="http://example.org/pear"/>
    </ex:hasFruit>
  </rdf:Description>

</rdf:RDF>
EOXML
      4))

   (test-case
       "Spec Example 20"
     (check-parser
      #<<EOXML
<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:ex="http://example.org/stuff/1.0/"
            xml:base="http://example.org/triples/">
  <rdf:Description rdf:about="http://example.org/">
    <ex:prop rdf:ID="triple1">blah</ex:prop>
  </rdf:Description>

</rdf:RDF>
EOXML
      4))))

;; -----------------------------------------------------------------------------------------------

(define xml-write-file-test-suite
  (test-suite
   "Test xml"

   (test-case
       "empty-graph"
     (check-write-graph *empty-graph* 'xml "empty-graph"))

   (test-case
       "empty-named-graph"
     (check-write-graph *empty-named-graph* 'xml "empty-named-graph"))

   (test-case
       "example-graph-1"
     (check-write-graph *example-graph-1* 'xml "example-graph-1"))))

;; -----------------------------------------------------------------------------------------------
;; Test Runner
;; -----------------------------------------------------------------------------------------------

(run-tests xml-write-file-test-suite)
(run-tests xml-read-spec-test-suite)
