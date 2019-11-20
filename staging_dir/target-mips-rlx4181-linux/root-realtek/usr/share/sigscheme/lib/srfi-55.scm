;;  Filename : srfi-55.scm
;;  About    : SRFI-55 require-extension
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


(define %require-extension-handler-srfi
  (lambda numbers
    (for-each (lambda (n)
                (let ((srfi-n (string-append "srfi-" (number->string n))))
                  (or (%%require-module srfi-n)
                      (%require-sysfile srfi-n))))
              numbers)))

;; Be quasiquote free to allow --disable-quasiquote
(define %require-extension-alist
  (list
   (cons 'srfi %require-extension-handler-srfi)))

(define %require-sysfile
  (lambda (ext-id)
    (or (provided? ext-id)
        (let* ((file (string-append ext-id ".scm"))
               (path (string-append (%%system-load-path) "/" file)))
          (load path)
          (provide ext-id)))))

(define %require-extension
  (lambda clauses
    (for-each (lambda (clause)
                (let* ((id (car clause))
                       (args (cdr clause))
                       (id-str (symbol->string id))
                       (default-handler (lambda ()
                                          (or (%%require-module id-str)
                                              (%require-sysfile id-str))))
                       (handler (cond
                                 ((assq id %require-extension-alist) => cdr)
                                 (else
                                  default-handler))))
                  (apply handler args)))
              clauses)))
