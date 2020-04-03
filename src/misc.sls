;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MPL-2.0

(library (scforms misc)
  (export add1 sub1 void* range c-if
          thunk thunk* comment do-times -> do-to
          defines define-enum define-foreign-procedure)
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

;;;; Helper macros
  (define-syntax ->
    (syntax-rules ()
      ((_)
       (void*))
      ((_ val)
       val)
      ((_ val (proc args ...) rest ...)
       (-> (proc val args ...) rest ...))
      ((_ val proc rest ...)
       (-> (proc val) rest ...))))

  (define-syntax do-to
    (syntax-rules ()
      ((_)
       (void*))
      ((_ val)
       val)
      ((_ val (proc! args ...) rest ...)
       (begin
         (proc! val args ...)
         (do-to val rest ...)))))

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

  (define-syntax define-values
    (syntax-rules ()
      ((_ () body)
       (call-with-values (thunk body) void*))
      ((_ (var . vars) body)
       (begin
         (define var (call-with-values (thunk body) list))
         (define-values vars (apply values (cdr var)))
         (set! var (car var))))
      ((_ var body)
       (define var (call-with-values (thunk body) list)))))

;;; Not to be confused with `define-enumeration',
;;; which is overkill for our needs.
  (define-syntax-rule (define-enum n (vals ...))
    (define-values (vals ...)
      (apply values (range n (+ n (length (quote (vals ...))))))))

;;; This definition mirrors that of `define-foreign-variable' in PFFI.
  (define-syntax define-foreign-procedure
    (lambda (stx)
      (define (->scheme-name name)
        (string->symbol
         (apply string
                (map (lambda (c)
                       (if (char=? c #\_) #\- c))
                     (-> name
                         syntax->datum
                         symbol->string
                         string-downcase
                         string->list)))))
      (syntax-case stx ()
        ((_ object return symbol (args ...))
         (with-syntax ((scheme-name (datum->syntax stx (->scheme-name #'symbol))))
           (syntax (define scheme-name (foreign-procedure object return symbol (args ...))))))))))
