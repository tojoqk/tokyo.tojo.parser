(defpackage #:tokyo.tojo.parser/port
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:cell #:coalton-library/cell))
  (:export #:Port
           #:peek-char
           #:read-char!

           #:IntoPort
           #:into-port!

           #:IteratorPort))

(in-package #:tokyo.tojo.parser/port)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class (Port :p)
    (peek-char (:p -> Optional Char))
    (read-char! (:p -> Optional Char)))

  (define-class (Port :p => IntoPort :t :p (:t -> :p))
    (into-port! (:t -> :p)))

  (define-type IteratorPort (%IterPort (cell:Cell (Optional Char)) (iter:Iterator Char)))

  (define-instance (Port IteratorPort)
    (define (peek-char (%IterPort cell _))
      (match (cell:read cell)
        ((Some c) (Some c))
        ((None) None)))
    (define (read-char! (%IterPort cell iter))
      (match (cell:read cell)
        ((None) None)
        ((Some ch)
         (match (iter:next! iter)
           ((Some next-ch)
            (cell:write! cell (Some next-ch))
            (Some ch))
           ((None)
            (cell:write! cell None)
            (Some ch)))))))

  (define-instance (IntoPort (iter:Iterator Char) IteratorPort)
    (define (into-port! iter)
      (%IterPort (cell:new (iter:next! iter)) iter)))

  (define-instance (IntoPort String IteratorPort)
    (define (into-port! str)
      (into-port! (iter:into-iter str)))))
