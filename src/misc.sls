;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MPL-2.0

(library (scforms misc)
  (import (rnrs base))

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

;;;; Helper macros

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
      body body* ...))
  
  (define-syntax defines
    (syntax-rules ()
      ((_) (void*))
      ((_ (id val) (id* val*) ...)
       (begin
         (define id val)
         (defines (id* val*) ...)))))

;;; Not to be confused with `define-enumeration',
;;; which is overkill for our needs.
  (define-syntax-rule (define-enum n (vals ...))
    (define-values (vals ...)
      (apply values (range n (length (quote (vals ...)))))))

  (define-syntax-rule (define-foreign-procedure id object return symbol (args ...))
    (define id (foreign-procedure object return symbol (args ...)))))
