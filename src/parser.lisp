(defpackage #:tokyo.tojo.parser/parser
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:error)
  (:local-nicknames
   (#:cell #:coalton-library/cell)
   (#:iter #:coalton-library/iterator)
   (#:result #:coalton-library/result)
   (#:output #:tokyo.tojo.parser/private/output-stream)
   (#:port #:tokyo.tojo.parser/port)
   (#:iterable #:tokyo.tojo.iterable))
  (:export #:Parser

           #:peek-char
           #:peek-char-or-eof
           #:read-char
           #:read-char-or-eof

           #:do-while
           #:do-times

           #:buffer-push
           #:buffer-pop
           #:buffer-write-char
           #:buffer-write-string

           #:run!))

(in-package #:tokyo.tojo.parser/parser)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :transparent)
  (define-type (Parser :p :a)
    (Parser ((Tuple :p (List output:Stream)) -> Result String (Tuple :a (List output:Stream)))))

  (define-instance (Functor (Parser :p))
    (define (map f (Parser parse!))
      (Parser
       (fn ((Tuple port streams))
         (do ((Tuple x streams) <- (parse! (Tuple port streams)))
             (pure (Tuple (f x) streams)))))))

  (define-instance (Applicative (Parser :p))
    (define (pure x)
      (Parser (fn ((Tuple port streams)) (Ok (Tuple x streams)))))

    (define (liftA2 op (Parser parse1!) (Parser parse2!))
      (Parser
       (fn ((Tuple port streams))
         (do ((Tuple x streams) <- (parse1! (Tuple port streams)))
             ((Tuple y streams) <- (parse2! (Tuple port streams)))
             (pure (Tuple (op x y) streams)))))))

  (define-instance (Monad (Parser :p))
    (define (>>= (Parser parse!) f)
      (Parser
       (fn ((Tuple port streams))
         (do ((Tuple x streams) <- (parse! (Tuple port streams)))
             (let (Parser parse!) = (f x))
             ((Tuple x streams) <- (parse! (Tuple port streams)))
             (pure (Tuple x streams)))))))

  (define-instance (MonadFail (Parser :p))
    (define (fail msg)
      (Parser (const (Err msg)))))

  (declare peek-char-or-eof (port:Port :p => Parser :p (Optional Char)))
  (define peek-char-or-eof
    (Parser (fn ((Tuple port streams))
              (do (let opt-ch = (port:peek-char port))
                  (pure (Tuple opt-ch streams))))))

  (declare peek-char (port:Port :p => Parser :p Char))
  (define peek-char
    (do (opt <- peek-char-or-eof)
        (match opt
          ((Some c) (pure c))
          ((None) (Parser (const (Err "Unexpected eof")))))))

  (declare read-char-or-eof (port:Port :p => Parser :p (Optional Char)))
  (define read-char-or-eof
    (Parser
     (fn ((Tuple port streams))
       (do (let opt-ch = (port:read-char! port))
           (match opt-ch
             ((Some ch) (Ok (Tuple (Some ch) streams)))
             ((None) (Ok (Tuple None streams))))))))

  (declare read-char (port:Port :p => Parser :p Char))
  (define read-char
    (Parser
     (fn ((Tuple port streams))
       (do (let opt-ch = (port:read-char! port))
           (match opt-ch
             ((Some ch) (Ok (Tuple ch streams)))
             ((None) (Err "Unexpected eof")))))))

  (declare run! (port:Port :p => Parser :p :a -> :p -> Result String :a))
  (define (run! (Parser parse!) port)
    (map (fn ((Tuple x _)) x)
         (parse! (Tuple port (singleton (output:make-string-output-stream))))))

  (define-instance (iterable:Iterable (Parser :p))
    (define (iterable:iterate f state)
      (Parser
       (fn ((Tuple port streams))
         (let state* = (cell:new state))
         (let streams* = (cell:new streams))
         (loop
           (let (Parser parse!) = (f (cell:read state*)))
           (match (parse! (Tuple port (cell:read streams*)))
             ((Ok (Tuple (iterable:Continue state) streams))
              (cell:write! streams* streams)
              (cell:write! state* state)
              Unit)
             ((Ok (Tuple (iterable:Return state) streams))
              (cell:write! streams* streams)
              (cell:write! state* state)
              (break))
             ((Err e) (return (Err e)))))
         (Ok (Tuple (cell:read state*)
                    (cell:read streams*)))))))

  (declare do-while (Parser :p Boolean -> Parser :p Unit))
  (define (do-while p)
    (iterable:iterate (fn ((Unit))
                        (do (b <- p)
                            (if b
                                (pure (iterable:Continue Unit))
                                (pure (iterable:Return Unit)))))
                      Unit))

  (declare do-times (UFix -> Parser :p Unit -> Parser :p Unit))
  (define (do-times n p)
    (if (== n 0)
        (pure Unit)
        (do (iterable:iterate (fn (k)
                                (do p
                                    (if (== k 0)
                                        (pure (iterable:Return 0))
                                        (pure (iterable:Continue (1- k))))))
                              n)
            (pure Unit))))

  ;;
  ;; String Buffer feature
  ;;

  (declare buffer-push (Parser :p Unit))
  (define buffer-push
    (Parser
     (fn ((Tuple _port streams))
       (Ok (Tuple Unit
                  (Cons (output:make-string-output-stream)
                        streams))))))

  (declare buffer-pop (Parser :p String))
  (define buffer-pop
    (Parser
     (fn ((Tuple _port streams))
       (match streams
         ((Cons s ss)
          (Ok (Tuple (output:get-output-stream-string s) ss)))
         ((Nil)
          (Err "pop-buffer: stack underflow"))))))

  (declare buffer-write-char (Char -> Parser :p Unit))
  (define (buffer-write-char ch)
    (Parser
     (fn ((Tuple _port streams))
       (match streams
         ((Cons stream _)
          (output:write-char ch stream)
          (Ok (Tuple Unit streams)))
         ((Nil)
          (Err "write-char: No string buffer"))))))

  (declare buffer-write-string (String -> Parser :p Unit))
  (define (buffer-write-string str)
    (Parser
     (fn ((Tuple _port streams))
       (match streams
         ((Cons stream _)
          (output:write-string str stream)
          (Ok (Tuple Unit streams)))
         ((Nil)
          (Err "write-string: No string buffer")))))))
