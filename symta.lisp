#|
Copyright (C) 2013  Nikita Sadkov

Permission is hereby granted, free of charge, to any person obtaining a copy of this 
software and associated documentation files (the .Software.), to deal in the Software 
without restriction, including without limitation the rights to use, copy, modify, merge, 
publish, distribute, sublicense, and/or sell copies of the Software, and to permit 
persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or 
substantial portions of the Software.

THE SOFTWARE IS PROVIDED .AS IS., WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING 
BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, 
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
|#

(defpackage :|st|)
(in-package :symta)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(defmacro m (x xs body) `(loop as ,x in ,xs collect ,body)) ; map
(defmacro e (x xs body) `(loop as ,x in ,xs do ,body)) ; each

(to headed head o ! match o (('head . _) t))

(defparameter *error* nil) ;;default error handler

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; READER
(defparameter /origin nil)
(defparameter /input nil)
(defparameter /output nil)
(defparameter /table nil)
(defparameter /specs nil)

(defparameter *symbol-source* (make-hash-table :test 'eq))


(to-expand $ obj msg &rest args ! `(funcall ,obj ',msg ,@args))

(to token-is w x ! match x ((:token 'w . _) t))
(to token-col x ! second (getf x :src))

(to new-input text &key (row 0) (col 0) (off 0) (last nil)
  ! len = length text
  ! (fn msg &rest args ! case msg
      (peek (when (< off len) (aref text off)))
      (next (! when (< off len)
               (! last := aref text off
                ! incf col
                ! incf off)
             ! when (eq last #\newline)
               (! col := 0
                ! incf row)
             ! last))
       (src (list row col /origin))
       (last last)
       (error (funcall *error* "{row},{col}: {car args}"))))

(to /add-lexeme dst pattern type
  ! unless pattern (! gethash :type dst := type ! ret nil)
  ! cs = car pattern
  ! next = cdr pattern
  ! when (headed '+ next) (! next := `(,cs * ,@(cdr next)))
  ! when (stringp cs) (! cs := coerce cs 'list)
  ! e c (if (listp cs) cs (list cs))
    (! unless (gethash c dst)
       (! gethash c dst := if (headed '* next) dst (make-hash-table))
     ! next = (if (headed '* next) (cdr next) next)
     ! /add-lexeme (gethash c dst) next type))

(to keywordize x ! intern (string-upcase x) "KEYWORD")

(to /init-tokenizer
  ! digit = "0123456789"
  ! head-char = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_?<>"
  ! tail-char = "{head-char}{digit}"
  ! ls = `(".." "+" "-" "*" "/" "%" "^" "." "->" "~" "|" ";" "," ":" "=" "=>"
           "\\" "$" "@" "&" "!" (() :end)
           ")" ("(" ,(fn r o ! `(:|()| ,(/list r o :|)|))))
           "]" ("[" ,(fn r o ! `(:|[]| ,(/list r o :|]|))))
           "}" (,(string #\{) ,(fn r o ! `(:} ,(/list r o :|}|))))
           ("'" ,(fn r cs ! `(:text ,(cons "\\" (/string r nil #\')))))
           ("\"" ,(fn r cs ! `(:text ,(cons "\"" (/string r #\[ #\")))))
           ("`" ,(fn r cs ! `(:symbol ,(first (/string r nil #\`)))))
           ("//" ,#'/comment)
           ("/*" ,#'/multi-comment)
           (((#\space #\newline) +) ,(fn r cs ! /token r t))
           (("#" "0123456789ABCDEFabcdef" +) :hex)
           ((,digit +) :integer)
           ((,head-char ,tail-char *) :symbol))
  ! ss = '(("if" :if) ("then" :then) ("else" :else) ("and" :and) ("or" :or)
           ("Yes" :kw) ("No" :kw) ("Void" :kw))
  ! /table := (make-hash-table)
  ! /specs := make-hash-table :test 'equal
  ! e (a b) ss (! gethash a /specs := b)
  ! e l ls
    (! (pattern type) = if (consp l) l (list l (keywordize l))
     ! when (stringp pattern) (! pattern := coerce pattern 'list)
     ! /add-lexeme /table pattern type))

(to /token r &optional left-spaced
  ! src = $ r src
  ! head = $ r peek
  ! next = /table
  ! cur = nil
  ! c = nil
  ! cs = nil
  ! while t
    (! cur := next
     ! c := ($ r peek)
     ! next := (gethash c next)
     ! unless next
      (! value = coerce (nreverse cs) 'string
       ! type = or (gethash value /specs) (gethash :type cur)
       ! when (and (equal "-" value) left-spaced (not (find c '(#\newline #\space))))
         (setf type :negate)
       ! when (and (eq type :end) c) (setf type nil)
       ! unless type ($ r error "unexpected `{value}{or c '||}`")
       ! when (functionp type)
        (! value := funcall type r value
         ! when (headed :token value) (ret value)
         ! type := first value
         ! value := second value)
       ! ret (list :token type :value value :src src))
     ! push c cs
     ! $ r next))

(to /add-bars xs
  ! ys = nil
  ! first = t
  ! while xs
    (! x = pop xs
     ! (row col orig) = getf x :src
     ! when (and (or (zerop col) first)
                 (not (find (second x) '(:|\|| :then :else))))
       (push `(:token :|\|| :value "|" :src (,row ,(- col 1) ,orig)) ys)
       (setf first nil)
     ! push x ys)
  ! nreverse ys)

(to /tokenize r
  ! ts = nil
  ! while t
    (! tok = /token r
     ! when (token-is :end tok) (ret (/add-bars (nreverse ts)))
     ! push tok ts))

(to /list r open close
  ! (row col orig) = $ r src
  ! xs = nil
  ! while t
    (! x = /token r
     ! when (token-is close x) (ret (nreverse xs))
     ! when (token-is :end x) (funcall *error* "{orig}:{row},{col}: unclosed `{open}`")
     ! push x xs))

(to str-merge left middle right
  ! left = if (str-empty? left) (list middle) (list left middle)
  ! if (and (not (cdr right)) (zerop (length (car right))))
       left
       `(,@left ,@right))

(to /string r incut end
  ! l = nil
  ! while t
    (! c = $ r peek
     ! unless (eq c incut) ($ r next)
     ! match c
       (#\\ (match ($ r next)
              (#\n (push #\newline l))
              (#\t (push #\tab l))
              (#\\ (push #\\ l))
              ((= c (or #\n 'incut 'end)) (push c l))
              (nil ($ r error "EOF in string"))
              (else ($ r error "Invalid escape code: {else}"))))
       ('end (ret (list (coerce (reverse l) 'string))))
       ('incut (! l = coerce (reverse l) 'string
                ! m = cdr (/token r)
                ! e = /string r incut end
                ! ret (str-merge l m e)))
       (nil ($ r error "EOF in string"))
       (else (push c l))))

(to /comment-char? c ! and c (not (eq c #\newline)))
(to /comment r cs ! while (/comment-char? ($ r next)) ! /token r)
(to /multi-comment r cs
  ! o = 1
  ! while (plusp o)
     (match (list ($ r next) ($ r peek))
       ((_ nil) ($ r error "`/*`: missing `*/`"))
       ((#\* #\/) ($ r next) (decf o))
       ((#\/ #\*) ($ r next) (incf o)))
  ! /token r)

(to parser-error cause tok
  ! (row col orig) = or (getf tok :src) (list -1 -1 "<unknown>")
  ! funcall *error* "{orig}:{row},{col}: {cause} `{or (getf tok :value) 'eof}`")
(to /expect what &optional (head nil)
  ! tok = car /input
  ! unless (token-is what tok) (parser-error "expected {symbol-name what}; got" (or head tok))
  ! pop /input)

(to /if sym
  ! head = (/xs)
  ! /expect :then sym
  ! then = (/xs)
  ! /expect :else sym
  ! else = (/xs)
  ! list sym head then else)

(to /bar h
  ! c = token-col h
  ! zs = nil
  ! while /input
    (! ys = nil
     ! while (and /input (> (token-col (car /input)) c))
       (push (pop /input) ys)
     ! push (/parse (nreverse ys)) zs
     ! x = car /input
     ! unless (and (token-is :|\|| x) (= (token-col x) c))
       (ret `(,h ,@(nreverse zs)))
     ! pop /input))

(to-expand try expr fail
  ! g = gensym "V"
  ! `(! ,g = ,expr ! if (eq ,g :fail) (ret ,fail) ,g))

(to /negate h
  ! a = try (/mul) :fail
  ! unless (or (token-is :integer a) (token-is :float a)) (ret `(,h ,a))
  ! v = "{getf h :value}{getf a :value}"
  ! r = list :token (second a) :value v :src (getf h :src) :parsed (read-from-string v)
  ! ret r)

(to /term
  ! unless /input (ret :fail)
  ! tok = pop /input
  ! when (getf tok :parsed) (parser-error "already parsed token" tok)
  ! v = getf tok :value
  ! p = case (second tok)
     ((:escape :symbol :text) (ret tok))
     (:integer (read-from-string v))
     (:kw (keywordize v))
     (:|()| (/parse v))
     (:|[]| `((:token :symbol :value "[]" :src (getf tok :src)) ,@(/parse v)))
     (:|\|| (ret (/bar tok)))
     (:if (ret (/if tok)))
     (:- (ret (/negate tok)))
     (otherwise (push tok /input) (ret :fail))
  ! `(,@tok :parsed ,p))

(to delim? x ! match x ((:token (or :|:| :|=| :|=>| :|,| :if :then :else) . _) t))

(to /op ops
  ! v = second (car /input)
  ! unless (find v ops) (ret :fail)
  ! pop /input)

(to /binary-loop ops down e
  ! o = try (/op ops) e
  ! when (token-is :} o)
    (! as = /parse (getf o :value)
     ! as = if (find-if #'delim? as) (list as) as ;allow Xs.map{X=>...}
     ! ret (/binary-loop ops down `((,@o :parsed '"{}") ,e ,@as)))
  ! b = try (funcall down) (parser-error "no right operand for" o)
  ! unless (and (token-is :|.| o) (token-is :integer e) (token-is :integer b))
    (ret (/binary-loop ops down (list o e b)))
  ! v = "{getf e :value}.{getf b :value}"
  ! f = list :token :float :value v :src (getf e :src) :parsed (read-from-string v)
  ! ret (/binary-loop ops down f))

(to /binary down ops ! a = try (funcall down) :fail ! /binary-loop ops down a)
(to /suffix-loop e ! o = try (/op '(:!)) e ! /suffix-loop (list o e))
(to /suffix ! a = try (/binary #'/term '(:. :-> :~ :})) :fail ! /suffix-loop a)
(to /prefix ! o = try (/op '(:negate :\\ :$ :@ :&)) (/suffix)
            ! when (token-is :negate o) (ret (/negate o))
            ! a = try (/prefix) (parser-error "no operand for" o)
            ! list o a)
(to /exp ! /binary #'/prefix '(:^))
(to /mul ! /binary #'/exp '(:* :/ :%))
(to /add ! /binary #'/mul '(:+ :-))
(to /dots ! /binary #'/add '(:..))

(to /logic
  ! o = try (/op '(:and :or)) (/dots)
  ! /output := nreverse /output
  ! p = position-if #'delim? /input ;hack LL(1) to speed-up parsing
  ! tok = and p (elt /input p)
  ! when (or (not p) (find (second tok) '(:if :then :else)))
    (! /output := list (/xs) /output o ! ret :fail)
  ! r = subseq /input 0 p
  ! /input := subseq /input p
  ! /output := if (token-is :|:| tok)
                  (list (list o (cdr /output) (/parse r)) (car /output))
                  (list (list o /output (/parse r)))
  ! nil)

(to /delim
  ! o = try (/op '(:|:| :|=| :|=>| :|,|)) (/logic)
  ! pref = or (nreverse /output) '(:void)
  ! unless (token-is :|,| o) (! /output := `(,(/xs) ,pref ,o) ! ret nil)
  ! pref = m x pref `(:token :escape :value ,(/strip x) :src ,(getf o :src))
  ! r = split-if (fn x ! token-is :|,| x) /input
  ! r = m x (nreverse r) `(,@x (:token :|:| :value ":" :src ,(getf o :src)))
  ! /input := apply #'append `(,@r ,pref)
  ! /output := nreverse (/xs)
  ! nil)

(to /semicolon
  ! p = position-if (fn x ! if (token-is :|\|| x) (ret nil) (token-is :|;| x)) /input
  ! unless p (ret)
  ! l = /parse (subseq /input 0 p)
  ! m = elt /input p
  ! r = /parse (subseq /input (+ p 1))
  ! /input := nil
  ! /output := if (token-is :|;| (first r)) `(,@(nreverse (cdr r)) ,l ,m) `(,r ,l ,m))

(to /xs
  ! /output = nil
  ! (/semicolon)
  ! while t
    (! x = try (/delim) (ret (nreverse /output))
     ! when x (push x /output)))

(to /parse input
  ! /input = input
  ! xs = (/xs)
  ! when /input (parser-error "unexpected" (car /input))
  ! xs)

(to /strip x
  ! unless (consp x) (ret x)
  ! when (headed :token x)
    (! p = position :parsed x
     ! r = if p (/strip (elt x (+ p 1))) (getf x :value)
     ! when (stringp r) (setf (gethash r *symbol-source*) (getf x :src))
     ! ret r)
  ! e v x (when (and (token-is :! (car v)) (not (token-is :! (car x))))
            (ret `("!!" ,@(mapcar #'/strip x))))
  ! mapcar #'/strip x)

(to tokenize input ! /tokenize (new-input input))
(to parse input ! /strip (/parse input))

(to /read-toplevel input
  ! r = first (/strip (/parse (/tokenize (new-input input))))
  ! match r ((_ s) s) (r r))
(to /read xs ! /read-toplevel xs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; EVALUATOR
(defparameter *pkg* "st") ; where to store compiled symbols
(defparameter *env* nil)   ; code walker environment
(defparameter *error* nil)

(to env-sym? sym &optional (env *env*)
  ! unless (stringp sym) (ret)
  ! find-if (fn closure ! find sym closure :test 'equal) env)
(to env-push args &optional (env *env*)
  ! args = m a args (if (incut? a) (second a) a)
  ! cons args env)

(to incut? o ! headed "@" o)
(to string-headed f o ! and (stringp o) (> (length o) 0) (funcall f (aref o 0)))
(to var-sym? x ! string-headed #'upper-case-p x)
(to fn-sym? x ! and (stringp x) (not (var-sym? x)))
(to builtin? x ! string-headed (fn h ! eql h #\_) x)

(to /compile-do xs
  ! unless xs (ret :void)
  ! unless (cdr xs) (ret (if (fn-sym? (car xs)) :void (/compile (car xs))))
  ! x = car (last xs)
  ! void = or (eql x :void) (fn-sym? x)
  ! xs = m x xs (if (fn-sym? x) (intern x) (/compile x))
  ! when void (ret `(tagbody ,@xs))
  ! x = car (last xs)
  ! xs = butlast xs
  ! r = gensym "R"
  ! `(let ((,r :void))
       (tagbody ,@xs (setf ,r ,x))
       ,r))

(to /compile-fn as xs
  ! e = and as (car (last as))
  ! as = if (incut? e) `(,@(butlast as) ,(second e)) as
  ! tag = if (headed "_tag" (car xs)) (second (pop xs)) :no
  ! *env* = env-push as *env*
  ! body = /compile-do xs
  ! as = m a as (intern a *pkg*)
  ! as = if (incut? e) `(,@(butlast as) &rest ,@(last as)) as
  ! if (eql tag :no) `(lambda ,as ,body) `(typed-closure ,tag ,as ,body))

(to bad-sexp x msg
  ! (row col orig) = or (gethash (if (consp x) (car x) x) *symbol-source*) `(-1 -1 "<unknown>")
  ! funcall *error* "{orig}:{row},{col}: {msg}")

(to /compile-builtin xs ! match xs
  (("_fn" as . xs) (/compile-fn as xs))
  (("_if" a b c) `(if (eq ,(/compile a) :no) ,(/compile c) ,(/compile b)))
  (("_quote" x) (if (or (numberp x) (stringp x)) x `(quote ,x)))
  (("_set" p v) (unless (env-sym? p) (bad-sexp xs "unknown variable `{p}`"))
                `(setq ,(intern p *pkg*) ,(/compile v)))
  (("_goto" label) `(go ,(intern label)))
  (("_show" x) (print (/compile x)) nil)
  (xs (bad-sexp xs "invalid builtin `{car xs}`")))

(to invoke-error obj ! funcall *error* "invoke error: invalid object ({obj})")
(to gen-invoker recur env type as gs
  ! (v n . as) = as
  ! (h m . gs) = gs
  ! s = when (stringp n) (intern "{type}_{n}" "st")
  ! when (and s (lexically-bound-p s env)) (ret `(funcall ,s ,h ,@gs))
  ! s = when (stringp n) (intern "generic_{n}" "st")
  ! when (and s (lexically-bound-p s env)) (ret `(funcall ,s ,h ,@gs))
  ! r = `(funcall ,(intern "{type}_" "st") ,m ,h ,@gs)
  ! if recur
       `(typecase ,m
          (integer ,(gen-invoker nil env type `(,v "get" ,n ,@as) `(,h "get" ,m ,@gs)))
          (function ,(gen-invoker nil env type `(,v "map" ,n ,@as) `(,h "map" ,m ,@gs)))
          (otherwise ,r))
       r)
(to-expand invoke &rest as &environment e
  ! gs = m a as (gensym)
  ! h = car gs
  ! `(let ,(mapcar #'list gs as)
       (if (functionp ,h)
           (funcall ,@gs)
           ,(if (not (cdr as))
                h
                `(typecase ,h
                   (integer ,(gen-invoker t e "integer" as gs))
                   (list ,(gen-invoker t e "list" as gs))
                   (string ,(gen-invoker t e "text" as gs))
                   (keyword ,(gen-invoker t e "fn" as gs))
                   (otherwise (invoke-error ,h)))))))

(to /compile-var h f as
  ! match as (((_ (= a ("list" . _)))) (ret `(invoke ,h ,@(cddr (/compile a)))))
  ! r = m a as (if (incut? a) (/compile (second a)) `(list ,(/compile a)))
  ! r = if (> (length as) 1) `(append ,@r) (car r)
  ! if (equal f "list") r `(apply ,h ,r)) ;FIXME: what if `list` got redeclared?

(to /compile-fix head f as
  ! as = m a as (/compile a)
  ! (cond ((headed "_fn" f) `(,head ,@as))
          ((fn-sym? f) `(funcall ,head ,@as))
          (t `(invoke ,head ,@as))))

(to /compile-form f as
  ! unless (or (consp as) (fn-sym? f)) (ret (/compile f))
  ! when (builtin? f) (ret (/compile-builtin (cons f as)))
  ! head = if (fn-sym? f) (/compile-atom f) (/compile f)
  ! if (find-if #'incut? as) (/compile-var head f as) (/compile-fix head f as))

(to /compile-atom e &key (ignore-fn nil) (default :error) ! cond
  ((and (functionp e) (equal (get-closure-type e) "extern")) (funcall e))
  ((not (stringp e)) e)
  ((and (fn-sym? e) ignore-fn) e)
  ((env-sym? e) (intern e *pkg*))
  ((eql default :error) (bad-sexp e "unknown variable `{e}`"))
  ((t default)))

(to /compile expr ! match expr
  (("&" x) (/compile-atom x))
  ((f . as) (/compile-form f as))
  (() :void)
  (_ (/compile-atom expr :ignore-fn t)))

(to /eval expr env
  ! expr = `("_fn" ,(mapcar #'first env) ,expr)
  ! c = eval `(muffle-compiler-note (muffle-compiler-warning ,(/compile expr)))
  ! apply c (mapcar #'second env))

(defvar *unisym-counter* 0)
(to unisym &optional (name "G") ! "{name}{incf *unisym-counter*}&")
(defparameter *chars* (make-hash-table :test 'eq)) ;;char-to-string cache
(to char-string c
  ! s = gethash c *chars*
  ! when s (ret s)
  ! s = string c
  ! gethash c *chars* := s
  ! s)
(to tag-of x ! typecase x
  (string    "text") ;we use strings to represent symbols
  (list      "list")
  (integer   "integer")
  (keyword   "bool") ;in future keywords will be implemented as functions
  (function  (or (get-closure-type x) :no))
  (otherwise (funcall *error* "tag-of: cant handle {type-of x}")))
(to-expand yes? &rest x ! `(if ,x :yes :no))
(to-expand no? &rest x ! `(if ,x :no :yes))
(eval-when (:compile-toplevel :load-toplevel :execute)
 (to make-builtin xs
   ! ((n . as) . xs) = split '! xs
   ! (b n) = if (symbolp n) `(,n ,(string-downcase (symbol-name n))) `(nil ,n)
   ! `(list ,n (named-fn ,(intern n "KEYWORD") ,as (block ,b ,(!body xs))))))
(to-expand builtins &body xs ! `(list ,@(mapcar #'make-builtin xs)))

(to run-kernel root &rest args
 ;; apps should give access to complete applications, like /usr/bin
 ;; lib holds overridable static data
 ;; data holds dynamic data, unique to each instance of the running program, like `/tmp`
 ;; home holds dynamic data, shared between all instance of the program, like preferences
 ;; how about `here`/`workdir`?
 ! cd root
 ! setf *error* (fn x ! error "~a" x)
 ! (/init-tokenizer)
 ! bs = builtins
     (address_of o ! object-address o)
     (tag_of o ! tag-of o)
     (halt ! (abort))
     (dbg s ! print s ! s)
     (set_error_handler h ! setf *error* h)
     (load_file path ! load-file path)
     (utf8_to_text bytes ! utf8-to-string bytes)
     (text_to_utf8 text ! string-to-utf8 text)
     ;;(time ! / (get-internal-real-time) internal-time-units-per-second)
     ;;(apply_macro e ! apply-macro e)
     ;;(eval e ! process-toplevel e)
     ;;(host expr ! )
     (fn_ m o &rest as ! funcall *error* "fn has no method {m}")
     (fn_is a b ! yes? eq a b)
     (integer_ m o &rest as ! funcall *error* "integer has no method {m} {as}")
     (integer_is a b ! yes? eql a b)
     (integer_isnt a b ! no? eql a b)
     (integer_< a b ! yes? < a b)
     (integer_> a b ! yes? > a b)
     (integer_<< a b ! yes? <= a b)
     (integer_>> a b ! yes? >= a b)
     (integer_neg a ! - a)
     (integer_+ a b ! + a b)
     (integer_- a b ! - a b)
     (integer_* a b ! * a b)
     (integer_/ a b ! truncate a b)
     (integer_% a b ! q r = truncate a b ! r)
     (text_ m o &rest as ! funcall *error* "text has no method {m}")
     (text_is a b ! yes? equal a b)
     (text_< a b ! yes? string< a b)
     (text_> a b ! yes? string> a b)
     (text_<< a b ! yes? string<= a b)
     (text_>> a b ! yes? string>= a b)
     (text_size a ! length a)
     (text_get a i ! char-string (aref a i))
     (text_upcase a ! string-upcase a)
     (text_downcase a ! string-downcase a)
     (text_capitalize a ! string-capitalize a)
     (text_upcase? a ! yes? every #'upper-case-p a)
     (text_downcase? a ! yes? every #'lower-case-p a)
     (text_alpha? a ! yes? and (every #'alpha-char-p a) (plusp (length a)))
     (text_digit? a ! yes? and (every #'digit-char-p a) (plusp (length a)))
     (text_parse x &optional (o :no) ! /origin = o ! /read-toplevel x)
     (text_source x ! or (gethash x *symbol-source*) :no)
     (text_out s ! write-string s ! (force-output) ! :void)
     ;; generic_ should act as a place, where users can register their ad-hoc methods
     (generic_ m &rest as ! funcall *error* "generic_ got called with {m}")
     (list_ m o &rest as ! funcall *error* "list has no method {m}")
     (list_end a ! if a :no :yes)
     (list_head a ! if a (car a) (funcall *error* "head: list is empty"))
     (list_tail a ! if a (cdr a) (funcall *error* "tail: list is empty"))
     (list_headed a b ! cons b a)
     (list_join_text a ! apply #'concatenate 'string a)
     (list_eval x env ! /eval x env)
 ! stage-text = utf8-to-string (load-file "{root}/boot/stage0.hit")
 ! e = `(("Root" ,root) ,@bs)
 ! /origin = "stage0.hit"
 ! /eval (/read-toplevel stage-text) `(("Env" ,e) ,@e)
 ! nil)

;;(run-kernel "/Users/nikita/Documents/prj/symta/libs/symta/root")


(defparameter *ssa-env* nil)
(defparameter *ssa-out* nil) ; where resulting assembly code is stored
(defparameter *ssa-ns*  nil) ; unique name of current function
(defparameter *ssa-closure* nil) ; other lambdas', this lambda references
(defparameter *ssa-builtins* nil)


(defun ssa-resolved (name) (cons name *ssa-ns*))
(defun ssa-name (name) (symbol-name (gensym name)))

(to ssa name &rest args ! push `(,name ,@args) *ssa-out*)

(to ssa-get-parent-index parent
  ! p = position-if (fn e ! equal parent e) (car *ssa-closure*)
  ! when p (ret p) ; already exist
  ! setf (car *ssa-closure*) `(,@(car *ssa-closure*) ,parent)
  ! - (length (car *ssa-closure*)) 1)

(to ssa-path-to-sym x es
  ! unless es (ret nil)
  ! p = position-if (fn v ! equal x (car v)) (car es)
  ! unless p (ret (ssa-path-to-sym x (cdr es)))
  ! when (eq es *ssa-env*) (ret (list p nil)) ; it is an argument of the current function
  ! list p (ssa-get-parent-index (cdr (nth p (car es)))))

(to ssa-symbol x ptr
  ! match (ssa-path-to-sym x *ssa-env*)
     ((pos parent) (if parent
                       (! ssa 'load 'r 'p parent
                        ! ssa (if ptr 'add 'load) 'r 'r pos)
                       (ssa (if ptr 'add 'load) 'r 'e pos)))
     (else (error "undefined variable: ~a" x)))

(to ssa-atom x
  ! cond
    ((eql x 'run) (ssa 'move 'r "run"))
    ((integerp x) (ssa 'integer 'r x))
    ((stringp x) (ssa-symbol x nil))
    (t (error "unexpected ~a" x)))

(to ssa-quote x
  ! cond
     ((stringp x) (ssa 'string 'r x))
     ((integerp x) (ssa-atom x))
     ((listp x) (error "FIXME"))
     (t (error "unsupported quoted value: ~a" x)))

(to ssa-fn args body
  ! body-end = ssa-name "e"
  ! ssa 'goto body-end
  ! f = ssa-name "f"
  ! cs = nil
  ! (! *ssa-ns* = f
     ! *ssa-env* = cons (mapcar #'ssa-resolved args) *ssa-env*
     ! *ssa-closure* = cons nil *ssa-closure*
     ! ssa 'label *ssa-ns*
     ! produce-ssa body
     ! ssa 'label body-end
     ! setf cs (car *ssa-closure*))
  ! ssa 'alloc 'r (length cs)
  ! i = -1
  ! e c cs (! if (equal c *ssa-ns*) ; self?
                 (ssa 'store 'r (incf i) 'e)
                 (ssa 'copy 'r (incf i) 'p (ssa-get-parent-index c)))
  ;; check if we really need new closure here, because in some cases we can reuse parent's closure
  ;; a single argument to a function could be passed in register, while a closure would be created if required
  ! ssa 'closure 'r f 'r)

(to ssa-apply f as
  ;; FIXME: if it is a lambda call, we don't have to change env or create a closure, just push env
  ! produce-ssa f
  ! unless (eql (first (car *ssa-out*)) 'closure)
     (ssa 'tagcheck 'r 't_closure) ;no need to check local closures
  ! ssa 'move 'c 'r
  ! ssa 'alloc 'a (length as)
  ! i = -1
  ! e a as (! produce-ssa a
            ! ssa 'store 'a (incf i) 'r)
  ! ssa 'move 'e 'a ; replace current frame with new environment
  ! ssa 'call 'c)

(to ssa-set k place value
  ! ssa 'store (ssa-symbol place t) value
  ! produce-ssa `(,k ,value))

(to ssa-form xs
  ! match xs
    (("_fn" as body) (ssa-fn as body))
    (("_quote" x) (ssa-quote x))
    (("_set" k place value) (ssa-set k place value))
    (("_goto" x) (ssa-goto x))
    (("_show" x) (ssa-show x))
    ((f . as) (ssa-apply f as))
    (else (error "invalid CPS form: ~a" xs)))

(to produce-ssa x ! if (listp x) (ssa-form x) (ssa-atom x))

(to cps-to-ssa x
  ! *ssa-out* = nil
  ! produce-ssa x
  ! nreverse *ssa-out*)

(to cps-fn k args body
  ! kk = ssa-name "k"
  ! `(,k ("_fn" (,kk ,@args) ,(produce-cps kk body))))

(to cps-apply k f as
  ! fas = `(,f ,@as)
  ! (g . gs) = m a fas (if (listp a) (ssa-name "a") a)
  ! r = `(,g ,k ,@gs)
  ! rgs = reverse `(,g ,@gs)
  ! ras = reverse fas
  ! while rgs
      ;; treat quoted and _fn values as constants
      (when (listp (car ras))
        (setf r (produce-cps `("_fn" (,(car rgs)) ,r) (car ras))))
      (pop rgs)
      (pop ras)
  ! r)

(to cps-set xs k place value
  ! unless (stringp place) (bad-sexp xs "_set cant handle `{place}`")
  ! v = ssa-name "value"
  ! (produce-cps `("_fn" (,v) ("_set" ,k ,place ,v)) value))

(to cps-form k xs
  ! match xs
    (("_fn" as body) (cps-fn k as body))
    (("_if" cnd then else) (cps-form k `("_fn_if" ,cnd ("_fn" () ,then) ("_fn" () ,else))))
    (("_quote" x) `(,k ,xs))
    (("_set" place value) (cps-set xs k place value))
    ;;(("_goto" x) (cps-goto k x))
    ;;(("_show" x) (cps-show k x))
    ((f . as) (cps-apply k f as))
    (else `(,k :void)))

(to cps-atom k x ! `(,k ,x))

(to produce-cps k x ! if (listp x) (cps-form k x) (cps-atom k x))

(defun print-ssa (xs)
  (e x xs (format t "~{~s ~}~%" x)))

(defparameter *compiled* nil)

(to to-c-emit &rest args ! (push (apply #'format nil args) *compiled*))

(defun ssa-to-c (xs)
  ;; before ssa-to-c we can reoder code chunks to eliminate unneeded gotoes
  (let ((*compiled* nil)
        (decls nil))
    (push "#include \"common.h\"" decls)
    (to-c-emit "static void entry() {")
    (e x xs
       (match x
         ((''label label-name)
          (push (format nil "static void ~a();" label-name) decls)
          (to-c-emit "}~%")
          (to-c-emit "static void ~a() {" label-name)
          (to-c-emit "  printf(\"entering %s\\n\", \"~a\");" label-name)
          )
         ((''call name) (to-c-emit "  CALL(~a);" name))
         ((''goto name) (to-c-emit "  ~a();" name))
         ((''alloc place size) (to-c-emit "  ALLOC(~a, ~a);" place size))
         ((''load dst src off) (to-c-emit "  LOAD(~a, ~a, ~a);" dst src off))
         ((''store dst off src) (to-c-emit "  STORE(~a, ~a, ~a);" dst off src))
         ((''copy dst p src q) (to-c-emit "  COPY(~a, ~a, ~a, ~a);" dst p src q))
         ((''move dst src) (to-c-emit "  MOVE(~a, ~a);" dst src))
         ((''add dst a b) (to-c-emit "  ADD(~a, ~a, ~a);" dst a b))
         ((''integer dst str) (to-c-emit "  INTEGER(~a, ~s);" dst str))
         ((''string dst str) (to-c-emit "  STRING(~a, ~s);" dst str))
         ((''closure dst code env) (to-c-emit "  CLOSURE(~a, ~a, ~a);" dst code env))
         ((''tagcheck src tag) (to-c-emit "  TAGCHECK(~a, ~a);" src tag))
         (else (error "invalid ssa: ~a" x))))
    (to-c-emit "}")
    (format nil "~{~a~%~}" (reverse (append *compiled* decls)))))

(to ssa-compile k fn-expr
  ! cps = produce-cps k fn-expr
  ! ssa = cps-to-ssa cps
  ! ssa-to-c ssa)

;;we shold traverse CPS tree and replace all _fn's with refernces to them

;;instead of parent environment, closure should capture only pointers to used variables

(to ssa-compile-entry expr
  ! builtins = '("*" "+") ;;'("_fn_if" "+" "-" "*" "/" "text_out")
  ! fn-expr = `("_fn" ("host") (("_fn" ,builtins ,expr) ,@(m b builtins `("host" ("_quote" ,b)))))
  ! ssa-compile 'run fn-expr)

;;(to ssa-compile-file file src ! save-text-file file (ssa-compile-entry src))
(to ssa-compile-file file src ! ssa-compile-entry src)


;;(cps-to-ssa '("_fn" ("+" "x") ("+" "x" 1)))
;;(lisp-to-ssa '("_fn" ("a" "b") ("_fn" ("x") ("+" ("*" "a" "x") "b"))))
;;(print-ssa (cps-to-ssa '("_fn" ("+" "x") ("+" "x" 1))))
;;(print-ssa (cps-to-ssa (produce-cps '("_fn" ("x") 123) '("_fn" ("+" "*" "x") ("*" ("+" "x" 1) 2)))))
;;(ssa-compile-fn '("_fn" ("x") "x") '("_fn" ("+" "*" "x") ("*" ("+" "x" 123) 456)))
;;(ssa-to-c (cps-to-ssa (produce-cps '("_fn" ("x") "x") '("_fn" ("+" "*" "x") ("*" ("+" "x" 123) 456)))))
;;(ssa-compile-file "/Users/nikita/Documents/prj/symta/libs/symta/c/test.c" '("*" ("+" 123 789) 456))
