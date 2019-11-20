;;  Filename : srfi-0.scm
;;  About    : SRFI-0 Feature-based conditional expansion construct
;;
;;  Copyright (c) 2007-2008 SigScheme Project <uim-en AT googlegroups.com>
;;
;;  All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions
;;  are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;  3. Neither the name of authors nor the names of its contributors
;;     may be used to endorse or promote products derived from this software
;;     without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;;  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(require-extension (srfi 23))

(define-macro %cond-expand-dummy
  (lambda () #t))

(define %cond-expand-feature?
  (lambda (feature-exp)
    (cond
     ((symbol? feature-exp)
      (or (eq? feature-exp 'else)
          (provided? (symbol->string feature-exp))))
     ((pair? feature-exp)
      (let ((directive (car feature-exp))
            (args (cdr feature-exp)))
      (case directive
        ((and)
         ;;(every %cond-expand-feature? args))
         (not (memq #f (map %cond-expand-feature? args))))
        ((or)
         ;;(any %cond-expand-feature? args))
         (not (not (memq #t (map %cond-expand-feature? args)))))
        ((not)
         (if (not (null? (cdr args)))
             (error "invalid feature expression"))
         (not (%cond-expand-feature? (car args))))
        (else
         (error "invalid feature expression"))))))))

(define-macro cond-expand
  (lambda clauses
    (if (null? clauses)
        (error "unfulfilled cond-expand")
;;        (let ((clause (find (lambda (clause)
;;                              (%cond-expand-feature? (car clause)))
;;                            clauses)))
        (let ((clause (let rec ((rest clauses))
                          (cond
                           ((null? rest)
                            #f)
                           ((%cond-expand-feature? (caar rest))
                            (car rest))
                           (else
                            (rec (cdr rest)))))))
          (if clause
              `(begin
                 ;; raise error if cond-expand is placed in non-toplevel
                 (define-macro %cond-expand-dummy (lambda () #t))
                 . ,(cdr clause))
              (error "unfulfilled cond-expand"))))))
