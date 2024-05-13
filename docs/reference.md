# Package `tokyo.tojo.parser`<a name="tokyo.tojo.parser-package"></a>

## [parser.lisp](https://github.com/tojoqk/tokyo.tojo.parser/tree/main/src/parser.lisp) <a name="tokyo.tojo.parser-parser-lisp-file"></a>

### Types

#### <code>PARSER :A :B</code> <sup><sub>[TYPE]</sub></sup><a name="parser-type"></a>

<details>
<summary>Instances</summary>

- <code><a href="#runtimerepr-class">RUNTIMEREPR</a> (<a href="#parser-type">PARSER</a> :A :B)</code>
- <code><a href="#monad-class">MONAD</a> (<a href="#parser-type">PARSER</a> :A)</code>
- <code><a href="#functor-class">FUNCTOR</a> (<a href="#parser-type">PARSER</a> :A)</code>
- <code><a href="#monadfail-class">MONADFAIL</a> (<a href="#parser-type">PARSER</a> :A)</code>
- <code><a href="#applicative-class">APPLICATIVE</a> (<a href="#parser-type">PARSER</a> :A)</code>

</details>


***

### Values

#### <code>(RUN! (PARSER PARSE!) SRC)</code> <sup><sub>FUNCTION</sub></sup><a name="run!-value"></a>
<code>&forall; :A :B :C. <a href="#intoport-class">INTOPORT</a> :C :A &rArr; ((<a href="#parser-type">PARSER</a> :A :B) &rarr; :C &rarr; (<a href="#result-type">RESULT</a> <a href="#string-type">STRING</a> :B))</code>

***

#### <code>(DO-WHILE P)</code> <sup><sub>FUNCTION</sub></sup><a name="do-while-value"></a>
<code>&forall; :A. ((<a href="#parser-type">PARSER</a> :A <a href="#boolean-type">BOOLEAN</a>) &rarr; (<a href="#parser-type">PARSER</a> :A <a href="#unit-type">UNIT</a>))</code>

***

#### <code>PEEK-CHAR</code> <sup><sub>[VALUE]</sub></sup><a name="peek-char-value"></a>
<code>&forall; :A. <a href="#port-class">PORT</a> :A &rArr; (<a href="#parser-type">PARSER</a> :A <a href="#char-type">CHAR</a>)</code>

***

#### <code>READ-CHAR</code> <sup><sub>[VALUE]</sub></sup><a name="read-char-value"></a>
<code>&forall; :A. <a href="#port-class">PORT</a> :A &rArr; (<a href="#parser-type">PARSER</a> :A <a href="#char-type">CHAR</a>)</code>

***

#### <code>BUFFER-POP</code> <sup><sub>[VALUE]</sub></sup><a name="buffer-pop-value"></a>
<code>&forall; :A. (<a href="#parser-type">PARSER</a> :A <a href="#string-type">STRING</a>)</code>

***

#### <code>(FOLD-WHILE F ACC STATE)</code> <sup><sub>FUNCTION</sub></sup><a name="fold-while-value"></a>
<code>&forall; :A :B :C. ((:A &rarr; :B &rarr; (<a href="#parser-type">PARSER</a> :C (<a href="#tuple-type">TUPLE</a> :A (<a href="#optional-type">OPTIONAL</a> :B)))) &rarr; :A &rarr; :B &rarr; (<a href="#parser-type">PARSER</a> :C :A))</code>

***

#### <code>BUFFER-PUSH</code> <sup><sub>[VALUE]</sub></sup><a name="buffer-push-value"></a>
<code>&forall; :A. (<a href="#parser-type">PARSER</a> :A <a href="#unit-type">UNIT</a>)</code>

***

#### <code>PEEK-CHAR-OR-EOF</code> <sup><sub>[VALUE]</sub></sup><a name="peek-char-or-eof-value"></a>
<code>&forall; :A. <a href="#port-class">PORT</a> :A &rArr; (<a href="#parser-type">PARSER</a> :A (<a href="#optional-type">OPTIONAL</a> <a href="#char-type">CHAR</a>))</code>

***

#### <code>READ-CHAR-OR-EOF</code> <sup><sub>[VALUE]</sub></sup><a name="read-char-or-eof-value"></a>
<code>&forall; :A. <a href="#port-class">PORT</a> :A &rArr; (<a href="#parser-type">PARSER</a> :A (<a href="#optional-type">OPTIONAL</a> <a href="#char-type">CHAR</a>))</code>

***

#### <code>(BUFFER-WRITE-CHAR CH)</code> <sup><sub>FUNCTION</sub></sup><a name="buffer-write-char-value"></a>
<code>&forall; :A. (<a href="#char-type">CHAR</a> &rarr; (<a href="#parser-type">PARSER</a> :A <a href="#unit-type">UNIT</a>))</code>

***

#### <code>(BUFFER-WRITE-STRING STR)</code> <sup><sub>FUNCTION</sub></sup><a name="buffer-write-string-value"></a>
<code>&forall; :A. (<a href="#string-type">STRING</a> &rarr; (<a href="#parser-type">PARSER</a> :A <a href="#unit-type">UNIT</a>))</code>

***

# Package `tokyo.tojo.parser/port`<a name="tokyo.tojo.parser/port-package"></a>

## [port.lisp](https://github.com/tojoqk/tokyo.tojo.parser/tree/main/src/port.lisp) <a name="tokyo.tojo.parser/port-port-lisp-file"></a>

### Classes

#### <code>PORT</code> <sup><sub>[CLASS]</sub></sup><a name="port-class"></a>
<code><a href="#port-class">PORT</a> :A</code>

Methods:
- <code>PEEK-CHAR :: (:A &rarr; (<a href="#optional-type">OPTIONAL</a> <a href="#char-type">CHAR</a>))</code>
- <code>READ-CHAR! :: (:A &rarr; (<a href="#optional-type">OPTIONAL</a> <a href="#char-type">CHAR</a>))</code>

<details>
<summary>Instances</summary>

- <code><a href="#port-class">PORT</a> <a href="#iterport-type">ITERPORT</a></code>

</details>


***

#### <code>INTOPORT</code> <sup><sub>[CLASS]</sub></sup><a name="intoport-class"></a>
<code><a href="#port-class">PORT</a> :A &rArr; <a href="#intoport-class">INTOPORT</a> :B :A</code>

Methods:
- <code>INTO-PORT! :: (:B &rarr; :A)</code>

<details>
<summary>Instances</summary>

- <code><a href="#intoport-class">INTOPORT</a> <a href="#string-type">STRING</a> <a href="#iterport-type">ITERPORT</a></code>
- <code><a href="#intoport-class">INTOPORT</a> (<a href="#iterator-type">ITERATOR</a> <a href="#char-type">CHAR</a>) <a href="#iterport-type">ITERPORT</a></code>

</details>


***

