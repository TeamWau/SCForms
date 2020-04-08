;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MPL-2.0

(library (scforms misc)
  (export add1 sub1 void* range c-if compose
           -> do-to thunk thunk* comment do-times)
  (import (rnrs))

;;;; Helper procedures
  (define (add1 n) (+ n 1))
  (define (sub1 n) (- n 1))

  ;; Renamed to avoid clash with C void
  (define (void* . args)
    (if #f 'fnord))

  (define range
    (case-lambda
     (() '())
     ((end) (range 0 end))
     ((start end) (range start end 1))
     ((start end step)
      (if (>= start end)
          '()
          (cons start
                (range (+ start step) end step))))))

  (define (c-if value)
    (if value 1 0))

  (define (compose proc . procs)
    (if (null? procs)
        proc
        (lambda args
          (call-with-values
              (thunk (apply (apply compose procs) args))
            proc))))

;;;; Helper macros
  (define-syntax ->
    (syntax-rules ()
      ((_ val)
       val)
      ((_ val (proc args ...) rest ...)
       (-> (proc val args ...) rest ...))
      ((_ val proc rest ...)
       (-> (proc val) rest ...))))

  (define-syntax ->>
    (syntax-rules ()
      ((_ val)
       val)
      ((_ val (proc args ...) rest ...)
       (->> (proc args ... val) rest ...))
      ((_ val proc rest ...)
       (->> (proc val) rest ...))))

  (define-syntax do-to
    (syntax-rules ()
      ((_ val)
       val)
      ((_ val (proc! args ...) rest ...)
       (begin
         (proc! val args ...)
         (do-to val rest ...)))
      ((_ val proc! rest ...)
       (do-to val (proc!) rest ...))))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((_ (id . args) body body* ...)
       (define-syntax id
         (syntax-rules ()
           ((id . args)
            (begin body body* ...)))))))

  (define-syntax-rule (thunk body body* ...)
    (lambda () body body* ...))

  (define-syntax-rule (thunk* body body* ...)
    (lambda ignored body body* ...))

  (define-syntax-rule (comment ignored ...)
    (values))

  (define-syntax-rule (do-times n body body* ...)
    (do ((i n (sub1 i)))
        ((zero? i))
      body body* ...)))
