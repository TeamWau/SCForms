;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MPL-2.0

(library (scforms define)
  (export defines define-c-enum define-foreign-procedure)
  (import (rnrs)
          (for (pffi) run expand)
          (for (scforms misc) run expand))

  (define-syntax defines
    (syntax-rules ()
      ((_ (id val) ...)
       (begin
         (define id val) ...))))

  (define-syntax define-c-enum-aux
    (lambda (stx)
      (syntax-case stx ()
        ((_ enum val) #'(define enum int))
        ((_ enum val (id new-val) rest ...)
         #'(define-c-enum-aux enum new-val id rest ...))
        ((_ enum val id rest ...)
         (with-syntax ((name (datum->syntax #'id (string->symbol
                                                  (string-append
                                                   (symbol->string (syntax->datum #'enum)) ":"
                                                   (symbol->string (syntax->datum #'id)))))))
                      #'(begin
                          (define name val)
                          (define-c-enum-aux enum (add1 val) rest ...)))))))

  (define-syntax define-c-enum
    (syntax-rules ()
      ((_ enum elems ...)
       (define-c-enum-aux enum 0 elems ...))))

;;; This definition mirrors that of `define-foreign-variable' in PFFI.
  (define-syntax define-foreign-procedure
    (lambda (stx)
      (define (c-name->scheme-name name)
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
         (with-syntax ((scheme-name
                        (datum->syntax #'symbol (c-name->scheme-name #'symbol))))
                      (syntax
                       (define scheme-name
                         (foreign-procedure object return symbol (args ...))))))))))
