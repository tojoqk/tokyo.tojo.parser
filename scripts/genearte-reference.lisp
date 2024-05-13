(ql:quickload :coalton/doc)
(ql:quickload :tokyo.tojo.parser)
(coalton-doc:write-documentation-to-file
 "docs/reference.md"
 :packages '(tokyo.tojo.parser tokyo.tojo.parser/port)
 :asdf-system :tokyo.tojo.parser
 :file-link-prefix "https://github.com/tojoqk/tokyo.tojo.parser/tree/main/src/")
