#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)
(import :std/make)

;;(defbuild-script '("monday.ss"))

(def build-spec
  '("monday"
    (exe: "monday" "-ld-options" "-lyaml -lssl -lz -L/usr/local/lib/" "-cc-options" "-I/usr/local/include")
    ))

(def build-spec-static
  '("monday"
    (static-exe: "monday"
                 "-ld-options" "-lyaml -lssl -lz -L/usr/local/lib"
                 "-prelude" "(declare (not safe))")))

(def srcdir
  (path-normalize (path-directory (this-source-file))))

(def (main . args)
  (match args
    (["deps"]
     (let (build-deps (make-depgraph/spec build-spec))
       (call-with-output-file "build-deps" (cut write build-deps <>))))
    (["static"]
     (let (depgraph (call-with-input-file "build-deps" read))
       (make srcdir: srcdir
             bindir: srcdir
             optimize: #t
             static: #t
             depgraph: depgraph
             prefix: "monday"
             build-spec-static)))
    ([]
     (let (depgraph (call-with-input-file "build-deps" read))
       (make srcdir: srcdir
             bindir: srcdir
             optimize: #t
             debug: 'env
             static: #t
             depgraph: depgraph
             prefix: "monday"
             build-spec)))))
