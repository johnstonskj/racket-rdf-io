#lang scribble/manual

@(require racket/sandbox
          scribble/core
          scribble/eval
          rdf/core/graph
          media-type
          (for-label racket/base
                     racket/contract
                     racket/port
                     rdf/core/dataset
                     rdf/core/graph
		     rdf/core/nsname
                     rdf/core/statement
                     media-type
                     rdf/io/base
                     rdf/io/registry
                     rdf/io/json
                     rdf/io/json-ld
                     rdf/io/ntriples
                     rdf/io/turtle
                     rdf/io/trig
                     rdf/io/n3
                     rdf/io/nquads
                     rdf/io/trix
                     rdf/io/xml))

@;{============================================================================}

@(define example-eval
   (make-base-eval '(require net/url-string
                             rdf/core/graph
                             rdf/core/literal
			     rdf/core/resource
                             rdf/core/triple
                             rdf/io/base
                             rdf/io/registry)
                   '(define
                     graph-about-me
                     (named-graph
                      (string->resource "http://example.com/my-graph")
                      (make-statements
                        "http://example.com/p/me"
                        (list (list "http://example.com/v/people#hasFirstName"
                               (list (make-lang-string-literal "Me" "en")))
                               (list "http://example.com/v/people#hasLastName" (list "Myself"))
                               (list "http://example.com/v/people#hasScores" (list 2 4))))))))

@;{============================================================================}
@title[#:version  "0.1.1"]{RDF I/O Representations}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]

This package provides a framework for reading and writing representations of RDF structures, specifically graphs and
datasets. This framework centers on a structure, @racket[representation], that not only has the reader and writer
capabilities but also metadata concerning the representation. A registry module allows for the lookup of representations
and the addition of new representations external to this package.

Representations may be categorized as @italic{dataset-oriented}, @italic{graph-oriented}, or @italic{statement-oriented}
depending on whether they understand semantics at each of these levels.

@tabular[
#:style 'boxed
#:sep @hspace[2]
#:row-properties '(bottom-border ())
#:column-properties '(top top)
(list
 (list @bold{Orientation} @bold{json}  @bold{json-ld} @bold{nquads} @bold{ntriples} @bold{trig} @bold{trix} @bold{turtle}  @bold{n3} @bold{xml})
 (list @bold{Dataset}     @para{No}    @para{}        @para{No}     @para{No}       @para{Yes}  @para{}     @para{}        @para{}   @para{No})
 (list @bold{Graph}       @para{No}    @para{}        @para{Yes*}   @para{No}       @para{Yes}  @para{Yes}  @para{}        @para{}   @para{No})
 (list @bold{Statement}   @para{Yes}   @para{}        @para{Yes}    @para{Yes}      @para{-}    @para{-}    @para{Yes}     @para{}   @para{Yes})
)]

@table-of-contents[]

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module base}
@defmodule[rdf/io/base]

The following struct is used by the @racket[registry] module to hold details about a representation implementation. Each
representation creates an instance of the struct and can provide it to the registry module for lookup.

@defstruct*[representation (
            (id symbol?)
            (name string?)
            (mime-type (or/c media-type? #f))
            (file-extensions (listof string?))
            (reader (or/c reader/c #f))
            (dataset-writer (or/c dataset-writer/c #f))
            (graph-writer (or/c graph-writer/c #f)))]{
TBD

@itemlist[
  @item{@racket[id] -- the symbol used to look up a representation in the registry.}
  @item{@racket[name] -- a display name commonly used for this representation.}
  @item{@racket[mime-type] -- an IANA registered media-type, if one exists, for this representation.}
  @item{@racket[file-extensions] -- any registered, or well-known, file extensions used for this representation.}
  @item{@racket[reader] -- a function to read an RDF representation from an input port.}
  @item{@racket[dataset-writer] -- a function to write an RDF dataset in this representation.}
  @item{@racket[graph-writer] -- a function to write an RDF dataset in this representation.}
]
}

@;{============================================================================}
@subsection[]{Reading}


@defthing[reader/c flat-contract?]{
TBD

@itemlist[
  @item{@racket[statement-list?] --}
  @item{@racket[graph?] --}
  @item{@racket[dataset?] --}
]
}

@;{============================================================================}
@subsection[]{Writing}

@defthing[dataset-writer/c flat-contract?]{
TBD
}

@defthing[graph-writer/c flat-contract?]{
TBD

@examples[
  #:eval example-eval
(define (graph->xml-string graph)
  (let* ((representation (get-representation 'xml))
         (writer (representation-graph-writer representation)))   
    (with-output-to-string (λ () (writer graph)))))
]
}

@;{============================================================================}
@subsection[]{Exceptions}


@defstruct*[(exn:fail:representation exn:fail) ()
            #:extra-constructor-name make-exn:fail:representation]{
TBD
}

@defthing[representation-operation/c flat-contract?]{
TBD

@itemlist[
  @item{@racket['read] -- (see @racket[raise-representation-read-error])}
  @item{@racket['write] -- (see @racket[raise-representation-write-error])}
]
}

@defproc[(raise-representation-error
          [repr-id symbol?]
          [op representation-operation/c]
          [message string?])
         any]{
TBD
}

@defproc[(raise-representation-read-error
          [repr-id symbol?]
          [expecting string?]
          [found string?]
          [context (or/c string? (listof (cons/c string? string?))) '()])
         any]{
TBD
}

@defproc[(raise-representation-write-error
          [repr-id symbol?]
          [component string?]
          [context (or/c string? (listof (cons/c string? string?))) '()])
         any]{
TBD
}


@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module registry}
@defmodule[rdf/io/registry]

TBD


@defproc[(register-representation
          [repr representation?])
         void?]{
TBD
}

@defproc[(unregister-representation
          [repr-id symbol?])
         void?]{
TBD
}

@defproc[(has-representation?
          [repr-id symbol?])
          boolean?]{
TBD

@examples[
  #:eval example-eval
  (has-representation? 'n-triples)
  (has-representation? 'ntriples)
]
}

@defproc[(get-representation
          [repr-id symbol?])
          (or/c representation? #f)]{
TBD

@examples[
  #:eval example-eval
  (get-representation 'n-triples)
  (get-representation 'ntriples)
]
}

@defproc[(get-registered-representations)
         (listof symbol?)]{
TBD

@examples[
  #:eval example-eval
  (get-registered-representations)
]
}

@subsection[]{Helper Functions}

@defstruct*[(exn:fail:representation:unknown exn:fail:representation) ()
            #:extra-constructor-name make-exn:fail:representation:unknown]{
TBD
}

@defproc[(representation-read
          [repr-id symbol?]
          [inport input-port? (current-input-port)])
         (or/c statement-list? graph? dataset?)]{
TBD
}

@defproc[(representation-write-dataset
          [repr-id symbol?]
          [dataset dataset?]
          [out output-port? (current-output-port)]
          [#:map nsmap nsmap? (make-rdf-only-nsmap)])
         void?]{
TBD
}

@defproc[(representation-write-graph
          [repr-id symbol?]
          [graph graph?]
          [out output-port? (current-output-port)]
          [#:map nsmap nsmap? (make-rdf-only-nsmap)])
         void?]{
TBD
}

@defproc[(representation-write-statement
          [repr-id symbol?]
          [statement statement?]
          [out output-port? (current-output-port)]
          [#:map nsmap nsmap? (make-rdf-only-nsmap)])
         void?]{
TBD
}

@defproc[(representation-write-literal
          [repr-id symbol?]
          [literal literal?]
          [out output-port? (current-output-port)]
          [#:map nsmap nsmap? (make-rdf-only-nsmap)])
         void?]{
TBD
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module nquads}
@defmodule[rdf/io/nquads]

TBD

@examples[
  #:eval example-eval
  (let* ((representation (get-representation 'nquads))
         (writer (representation-graph-writer representation)))   
    (writer graph-about-me))
]

See the specification at @cite["RDF11NQ"].

@defthing[nquad-representation representation?]{
TBD

@examples[
  #:eval example-eval
  (require rdf/io/nquads)
  nquad-representation
]
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module ntriples}
@defmodule[rdf/io/ntriples]

TBD

@examples[
  #:eval example-eval
  (let* ((representation (get-representation 'ntriples))
         (writer (representation-graph-writer representation)))   
    (writer graph-about-me))
]

See the specification at @cite["RDF11NT"].

@defthing[ntriple-representation representation?]{
TBD

@examples[
  #:eval example-eval
  (require rdf/io/ntriples)
  ntriple-representation
]
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module trig}
@defmodule[rdf/io/trig]

TBD

@examples[
  #:eval example-eval
  (let* ((representation (get-representation 'trig))
         (writer (representation-graph-writer representation)))   
    (writer graph-about-me))
]

See the specification at @cite["RDF11TRIG"].

@defthing[trig-representation representation?]{
TBD

@examples[
  #:eval example-eval
  (require rdf/io/trig)
  trig-representation
]
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module trix}
@defmodule[rdf/io/trix]

TBD

@examples[
  #:eval example-eval
  (let* ((representation (get-representation 'trix))
         (writer (representation-graph-writer representation)))   
    (writer graph-about-me))
]

See the specification at @cite["RDF11TRIX"].

@defthing[trix-representation representation?]{
TBD

@examples[
  #:eval example-eval
  (require rdf/io/trix)
  trix-representation
]
}

@defthing[trix-representation/w3c representation?]{
TBD

@examples[
  #:eval example-eval
  (require rdf/io/trix)
  trix-representation/w3c
]
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module turtle}
@defmodule[rdf/io/turtle]

TBD

See the specification at ["RDF11TTL"].

@defthing[turtle-representation representation?]{
TBD

@; @examples[
@;   #:eval example-eval
@;   (require rdf/io/turtle)
@;   turtle-representation
@; ]
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module xml}
@defmodule[rdf/io/xml]

TBD

@examples[
  #:eval example-eval
  (let* ((representation (get-representation 'xml))
         (writer (representation-graph-writer representation)))   
    (writer graph-about-me))
]

See the specification at @cite["RDF11XML"].

@defthing[xml-representation representation?]{
TBD

@examples[
  #:eval example-eval
  (require rdf/io/xml)
  xml-representation
]
}

@;{============================================================================}
@;{============================================================================}
@section[#:style '(toc)]{Module tabular}
@defmodule[rdf/io/tabular]

TBD

@examples[
  #:eval example-eval
  (let* ((representation (get-representation 'tabular))
         (writer (representation-graph-writer representation)))   
    (writer graph-about-me))
]

@defthing[tabular-representation representation?]{
TBD

@examples[
  #:eval example-eval
  (require rdf/io/tabular)

  tabular-representation
]
}

@defproc[(tabular-display-graph
          [graph graph?]
          [out output-port? (current-output-port)])
         void?]{

@examples[
  #:eval example-eval
  (require rdf/core/graph
           rdf/io/tabular)

  (tabular-display-graph graph-about-me)
]
}

@;{============================================================================}
@;{============================================================================}

@(bibliography

  (bib-entry #:key "RDF11JSON"
             #:title "RDF 1.1 JSON Alternate Serialization (RDF/JSON)"
             #:author "I. Davis, T. Steiner, and A. J Le Hors"
             #:location "W3C"
             #:url "https://www.w3.org/TR/rdf-json/"
             #:date "7 November 2013")

  (bib-entry #:key "JSONLD"
             #:title "JSON-LD 1.1 -- A JSON-based Serialization for Linked Data"
             #:author "G. Kellogg, P.-A. Champin, and D. Longley"
             #:location "W3C"
             #:url "https://www.w3.org/TR/json-ld/"
             #:date "16 July 2020")

  (bib-entry #:key "RDF11NQ"
             #:title "RDF 1.1 N-Quads -- A line-based syntax for RDF datasets"
             #:author "G. Carothers"
             #:location "W3C"
             #:url "https://www.w3.org/TR/n-quads/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11NT"
             #:title "RDF 1.1 N-Triples -- A line-based syntax for an RDF graph"
             #:author "P. J. Hayes, and P. F. Patel-Schneider"
             #:location "W3C"
             #:url "https://www.w3.org/TR/n-triples/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11TRIG"
             #:title "RDF 1.1 TriG: RDF Dataset Language"
             #:author "G. Carothers, A. Seaborne, C. Bizer, and R. Cyganiak"
             #:location "W3C"
             #:url "https://www.w3.org/TR/trig/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11TRIX"
             #:title "TriX: An XML Serialization for RDF Triples"
             #:author "J. J. Carroll, and P. Stickler"
             #:location "Nokia"
             #:url "https://web.archive.org/web/20110724134923/http://sw.nokia.com/trix/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11TTL"
             #:title "RDF 1.1 Turtle -- Terse RDF Triple Language"
             #:author "D. Beckett, T. Berners-Lee, E. Prud'hommeaux, and . Carothers"
             #:location "W3C"
             #:url "https://www.w3.org/TR/turtle/"
             #:date "25 February 2014")

  (bib-entry #:key "RDF11XML"
             #:title "RDF 1.1 XML Syntax"
             #:author "F. Gandon, and G. Schreiber"
             #:location "W3C"
             #:url "https://www.w3.org/TR/rdf-syntax-grammar/"
             #:date "25 February 2014")

  (bib-entry #:key "RFC8259"
             #:title "The JavaScript Object Notation (JSON) Data Interchange Format"
             #:author "T. Bray"
             #:location "RFC"
             #:url "http://www.ietf.org/rfc/rfc8259.txt"
             #:date "December 2017")

)

@;{============================================================================}
@;{============================================================================}
@index-section[]
