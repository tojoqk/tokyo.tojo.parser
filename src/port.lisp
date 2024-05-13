(defpackage #:tokyo.tojo.parser/port
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:local-nicknames
   (#:iter #:coalton-library/iterator))
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
    (read-char! (:p -> (Optional (Tuple Char :p)))))

  (define-class (Port :p => IntoPort :t :p (:t -> :p))
    (into-port! (:t -> :p)))

  (define-type IterPort (%IterPort (Optional Char) (iter:Iterator Char)))

  (define-instance (Port IterPort)
    (define (peek-char (%IterPort opt _))
      (match opt
        ((Some c) (Some c))
        ((None) None)))
    (define (read-char! p)
      (match p
        ((%IterPort (None) _) None)
        ((%IterPort (Some ch) iter)
         (match (iter:next! iter)
           ((Some next-ch)
            (Some (Tuple ch (%IterPort (Some next-ch) iter))))
           ((None) (Some (Tuple ch (%IterPort None iter)))))))))

  (define-instance (IntoPort (iter:Iterator Char) IterPort)
    (define (into-port! iter)
      (%IterPort (iter:next! iter) iter)))

  (define-instance (IntoPort String IterPort)
    (define (into-port! str)
      (into-port! (iter:into-iter str)))))
