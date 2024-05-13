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
           #:into-port!))

(in-package #:tokyo.tojo.parser/port)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define-class (Port :p)
    (peek-char (:p -> Optional Char))
    (read-char! (:p -> Optional Char)))

  (define-class (Port :p => IntoPort :t :p (:t -> :p))
    (into-port! (:t -> :p)))

  (define-type IterPort (%IterPort (cell:Cell (Optional Char)) (iter:Iterator Char)))

  (define-instance (Port IterPort)
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

  (define-instance (IntoPort (iter:Iterator Char) IterPort)
    (define (into-port! iter)
      (%IterPort (cell:new (iter:next! iter)) iter)))

  (define-instance (IntoPort String IterPort)
    (define (into-port! str)
      (into-port! (iter:into-iter str)))))
