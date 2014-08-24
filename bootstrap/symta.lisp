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

(defparameter g_error nil) ;;default error handler

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; READER
(defparameter g_origin nil)
(defparameter g_input nil)
(defparameter g_output nil)
(defparameter g_table nil)
(defparameter g_specs nil)

(defparameter g_symbol_source (make-hash-table :test 'eq))


(to-expand $ obj msg &rest args ! `(funcall ,obj ',msg ,@args))

(to token-is w x ! match x ((:token 'w . _) t))
(to token-col x ! second (getf x :src))

(to new-input text &key (row 0) (col 0) (off 0) (last nil)
  ! len = length text
  ! (fn msg &rest args ! case msg
      (peek (when (< off len) (aref text off)))
      (next (when (< off len)
              (! last := aref text off
               ! incf col
               ! incf off
               ! when (eq last #\newline)
                 (! col := 0
                  ! incf row)
               ! last)))
      (src (list row col g_origin))
      (last last)
      (error (error "{row},{col}: {car args}"))))

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
  ! when g_table (return-from /init-tokenizer)
  ! digit = "0123456789"
  ! head-char = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_?"
  ! tail-char = "{head-char}{digit}"
  ! ls = `("+" "-" "*" "/" "%" "^" "." "->" "~" "|" ";" "," ":" "=" "=>" "<="
           "++" "--" "**" ".."
           "><" "<>" "<" ">" "<<" ">>"
           "\\" "$" "@" "&" "!" (() :end)
           ")" ("(" ,(fn r o ! `(:|()| ,(/list r o :|)|))))
           "]" ("[" ,(fn r o ! `(:|[]| ,(/list r o :|]|))))
           "}" (,(string #\{) ,(fn r o ! `(:|{}| ,(/list r o :|}|))))
           ("'" ,(fn r cs ! `(:text ,(cons "\\" (/string r nil #\')))))
           ("\"" ,(fn r cs ! `(:splice ,(/string r #\[ #\"))))
           ("`" ,(fn r cs ! `(:symbol ,(first (/string r nil #\`)))))
           ("//" ,#'/comment)
           ("/*" ,#'/multi-comment)
           (((#\space #\newline) +) ,(fn r cs ! /token r t))
           (("#" "0123456789ABCDEFabcdef" +) :hex)
           ((,digit +) :integer)
           ((,head-char ,tail-char *) :symbol))
  ! ss = '(("if" :if) ("then" :then) ("else" :else) ("and" :and) ("or" :or) ("Void" :kw))
  ! g_table := (make-hash-table)
  ! g_specs := make-hash-table :test 'equal
  ! e (a b) ss (! gethash a g_specs := b)
  ! e l ls
    (! (pattern type) = if (consp l) l (list l (keywordize l))
     ! when (stringp pattern) (! pattern := coerce pattern 'list)
     ! /add-lexeme g_table pattern type))

(to /token r &optional left-spaced
  ! src = $ r src
  ! head = $ r peek
  ! next = g_table
  ! cur = nil
  ! c = nil
  ! cs = nil
  ! while t
    (! cur := next
     ! c := ($ r peek)
     ! next := (gethash c next)
     ! unless next
      (! value = coerce (nreverse cs) 'string
       ! type = or (gethash value g_specs) (gethash :type cur)
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
     ! when (token-is :end x) (error "{orig}:{row},{col}: unclosed `{open}`")
     ! push x xs))

(to spliced-string-normalize xs
  ! ys = remove-if (fn x ! equal x "") xs
  ! m y ys (cond ((stringp y) `(:token :symbol :value ,y :src (0 0 nil)))
                 ((headed :token y) y)
                 (t `(:token :|()| :value ,y :src (0 0 nil)))))

(to /string r incut end
  ! incut = or incut :none
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
       ('end (! ys = (list (coerce (reverse l) 'string))
              ! when (eq end #\") (setf ys (spliced-string-normalize ys))
              ! ret ys))
       ('incut (! l = coerce (reverse l) 'string
                ! m = (getf (/token r) :value)
                ! e = /string r incut end
                ! ret (spliced-string-normalize `(,l ,m ,@e))))
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
  ! error "{orig}:{row},{col}: {cause} `{or (getf tok :value) 'eof}`")
(to /expect what &optional (head nil)
  ! tok = car g_input
  ! unless (token-is what tok) (parser-error "expected {symbol-name what}; got" (or head tok))
  ! pop g_input)

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
  ! while g_input
    (! ys = nil
     ! while (and g_input (> (token-col (car g_input)) c))
       (push (pop g_input) ys)
     ! push (/parse (nreverse ys)) zs
     ! x = car g_input
     ! unless (and (token-is :|\|| x) (= (token-col x) c))
       (ret `(,h ,@(nreverse zs)))
     ! pop g_input))

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
  ! unless g_input (ret :fail)
  ! tok = pop g_input
  ! when (getf tok :parsed) (parser-error "already parsed token" tok)
  ! v = getf tok :value
  ! p = case (second tok)
     ((:escape :symbol :text) (ret tok))
     (:splice `((:token :symbol :value "\"" :src (getf tok :src)) ,@(/parse v)))
     (:integer (read-from-string v))
     (:kw (keywordize v))
     (:|()| (/parse v))
     (:|[]| `((:token :symbol :value "[]" :src (getf tok :src)) ,@(/parse v)))
     (:|\|| (ret (/bar tok)))
     (:if (ret (/if tok)))
     (:- (ret (/negate tok)))
     (otherwise (push tok g_input) (ret :fail))
  ! `(,@tok :parsed ,p))

(to delim? x ! match x ((:token (or :|:| :|=| :|=>| :|<=| :|,| :if :then :else) . _) t))

(to /op ops
  ! v = second (car g_input)
  ! unless (find v ops) (ret :fail)
  ! pop g_input)

(to /binary-loop ops down e
  ! o = try (/op ops) e
  ! when (token-is :|{}| o)
    (! as = /parse (getf o :value)
     ! as = if (find-if #'delim? as) (list as) as ;allow Xs.map{X=>...}
     ! ret (/binary-loop ops down `((,@o :parsed "{}") ,e ,@as)))
  ! b = try (funcall down) (parser-error "no right operand for" o)
  ! unless (and (token-is :. o) (token-is :integer e) (token-is :integer b))
    (ret (/binary-loop ops down (list o e b)))
  ! v = "{getf e :value}.{getf b :value}"
  ! f = list :token :float :value v :src (getf e :src) :parsed (read-from-string v)
  ! ret (/binary-loop ops down f))

(to /binary down ops ! a = try (funcall down) :fail ! /binary-loop ops down a)
(to /suffix-loop e ! o = try (/op '(:!)) e ! /suffix-loop (list o e))
(to /suffix ! a = try (/binary #'/term '(:. :^ :-> :~ :|{}|)) :fail ! /suffix-loop a)
(to /prefix ! o = try (/op '(:negate :\\ :$ :@ :&)) (/suffix)
            ! when (token-is :negate o) (ret (/negate o))
            ! a = try (/prefix) (parser-error "no operand for" o)
            ! list o a)
(to /mul ! /binary #'/prefix '(:* :/ :%))
(to /add ! /binary #'/mul '(:+ :-))
(to /dots ! /binary #'/add '(:..))
(to /bool ! /binary #'/dots '(:>< :<> :< :> :<< :>>))

(to /logic
  ! o = try (/op '(:and :or)) (/bool)
  ! g_output := nreverse g_output
  ! p = position-if #'delim? g_input ;hack LL(1) to speed-up parsing
  ! tok = and p (elt g_input p)
  ! when (or (not p) (find (second tok) '(:if :then :else)))
    (! g_output := list (/xs) g_output o ! ret :fail)
  ! r = subseq g_input 0 p
  ! g_input := subseq g_input p
  ! g_output := if (token-is :|:| tok)
                  (list (list o (cdr g_output) (/parse r)) (car g_output))
                  (list (list o g_output (/parse r)))
  ! nil)

(to /delim
  ! o = try (/op '(:|:| :|=| :|=>| :|<=| :|,|)) (/logic)
  ! pref = or (nreverse g_output) '(:void)
  ! unless (token-is :|,| o) (! g_output := `(,(/xs) ,pref ,o) ! ret nil)
  ! pref = m x pref `(:token :escape :value ,(/strip x) :src ,(getf o :src))
  ! r = split-if (fn x ! token-is :|,| x) g_input
  ! r = m x (nreverse r) `(,@x (:token :|:| :value ":" :src ,(getf o :src)))
  ! g_input := apply #'append `(,@r ,pref)
  ! g_output := nreverse (/xs)
  ! nil)

(to /semicolon
  ! p = position-if (fn x ! if (token-is :|\|| x) (ret nil) (token-is :|;| x)) g_input
  ! unless p (ret)
  ! l = /parse (subseq g_input 0 p)
  ! m = elt g_input p
  ! r = /parse (subseq g_input (+ p 1))
  ! g_input := nil
  ! g_output := if (token-is :|;| (first r)) `(,@(nreverse (cdr r)) ,l ,m) `(,r ,l ,m))

(to /xs
  ! g_output = nil
  ! (/semicolon)
  ! while t
    (! x = try (/delim) (ret (nreverse g_output))
     ! when x (push x g_output)))

(to /parse input
  ! g_input = input
  ! xs = (/xs)
  ! when g_input (parser-error "unexpected" (car g_input))
  ! xs)

(to /strip x
  ! unless (consp x) (ret x)
  ! when (headed :token x)
    (! p = position :parsed x
     ! r = if p (/strip (elt x (+ p 1))) (getf x :value)
     ! when (stringp r) (setf (gethash r g_symbol_source) (getf x :src))
     ! ret r)
  ! e v x (when (and (token-is :! (car v)) (not (token-is :! (car x))))
            (ret `("!!" ,@(mapcar #'/strip x))))
  ! mapcar #'/strip x)

(to tokenize input ! /tokenize (new-input input))
(to parse input ! /strip (/parse input))

(to /read-toplevel input
  ! r = first (/strip (/parse (/tokenize (new-input input))))
  ! match r ((_ s) s) (r r))

(to /normalize expr ! match expr (("|" . as) expr)
                                 (x `("|" ,x)))

(to /read xs ! /read-toplevel xs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; COMPILER
(defparameter *ssa-env* nil)
(defparameter *ssa-out* nil) ; where resulting assembly code is stored
(defparameter *ssa-ns*  nil) ; unique name of current function
(defparameter *ssa-raw-inits* nil)
(defparameter *ssa-fns* nil)
(defparameter *ssa-closure* nil) ; other lambdas, this lambda references
(defparameter *ssa-bases* nil)
(defparameter *uniquify-stack* nil)
(defparameter *hoisted-texts* nil)
(defparameter *resolved-methods* nil)

(to ssa-name name ! symbol-name (gensym name))

(to ssa name &rest args ! push `(,name ,@args) *ssa-out*)

(to ssa-get-parent-index parent
  ! p = position-if (fn e ! equal parent e) (car *ssa-closure*)
  ! when p (ret p) ; already added to the current closure
  ! setf (car *ssa-closure*) `(,@(car *ssa-closure*) ,parent)
  ! - (length (car *ssa-closure*)) 1)

(to ssa-path-to-sym x es
  ! unless es (ret nil)
  ! head = car es
  ! tail = cdr es
  ! when (eql (first head) :all) ; reference to the whole arglist?
     (setf head (second head))
     (unless (equal x (car head)) (ret (ssa-path-to-sym x tail)))
     (when (eq es *ssa-env*) (ret (list :all nil))) ; it is an argument of the current function
     (ret (list :all (ssa-get-parent-index (cdr head))))
  ! p = position-if (fn v ! equal x (car v)) head
  ! unless p (ret (ssa-path-to-sym x tail))
  ! when (eq es *ssa-env*) (ret (list p nil)) ; it is an argument of the current function
  ! list p (ssa-get-parent-index (cdr (nth p head))))

(to ssa-symbol k x value
  ! match (ssa-path-to-sym x *ssa-env*)
     ((pos parent)
      (! base = if parent (ssa-name "b") 'e
       ! when parent ; symbol resides in parent environment?
          (ssa 'var base)
          (ssa 'load base 'p parent)
       ! when (eql pos :all)
          (when value (error "can't set ~a" x))
          (ssa 'tagged k base 'T_LIST)
          (ret nil)
       ! unless value (return-from ssa-symbol (ssa 'arg_load k base pos))
       ! if (and (eql base 'e) (= 1 (length *ssa-bases*)))
            (ssa 'arg_store base pos value)
            (ssa 'lift base pos value)
       ))
     (else (error "undefined variable: ~a" x)))

(to ssa-quote-list-rec xs
   ! `("_list" ,@(m x xs (if (listp x) (ssa-quote-list-rec x) `("_quote" ,x)))))

(to ssa-quote-list k xs ! ssa-expr k (ssa-quote-list-rec xs))

(to cstring s ! `(,@(m c (coerce s 'list) (char-code c)) 0))

(to ssa-cstring src
  ! name = ssa-name "b"
  ! ssa 'bytes name (cstring src)
  ! name)

(to ssa-var &optional (n "t")
  ! v = ssa-name n
  ! ssa 'var v
  ! v)

(to ssa-global name
  ! v = ssa-name name
  ! ssa 'global v
  ! v)

(to ev x
  ! r = ssa-var "r"
  ! ssa-expr r x
  ! r)

(to ssa-quote k x
  ! cond
     ((stringp x) (ssa-expr k (gethash x *hoisted-texts*)))
     ((listp x) (ssa-quote-list k x))
     (t (ssa-expr k x)))

(to ssa-resolved name ! cons name *ssa-ns*)

(to ssa-fn-body k f args body o prologue epilogue
  ! *ssa-bases* = list ()
  ! *ssa-out* = nil
  ! *ssa-ns* = f
  ! *ssa-env* = if (stringp args)
                   (cons `(:all ,(ssa-resolved args)) *ssa-env*)
                   (cons (mapcar #'ssa-resolved args) *ssa-env*)
  ! *ssa-closure* = cons nil *ssa-closure*
  ! when prologue (ssa 'label *ssa-ns*)
  ! size-var = format nil "~a_size" f
  ! when prologue
      (if (stringp args)
          (ssa 'check_varargs size-var "Empty" #|(get-meta o)|#)
          (ssa 'check_nargs (length args) size-var "Empty" #|(get-meta o)|#))
  ! unless k (setf k (ssa-var "result"))
  ! ssa-expr k body
  ! when epilogue (ssa 'return k)
  ! list *ssa-out* (car *ssa-closure*))

;; check if we really need new closure here, because in some cases we can reuse parent's closure
;; a single argument to a function could be passed in register, while a closure would be created if required
;; a single reference closure could be itself held in a register
;; for now we just capture required parent's closure
(to ssa-fn name k args body o
  ! f = ssa-name "f"
  ! (body cs) = ssa-fn-body nil f args body o t t
  ! push body *ssa-fns*
  ! nparents = length cs
  ! ssa 'alloc_closure k f nparents
  ! i = -1
  ! e c cs (! if (equal c *ssa-ns*) ; self?
                 (ssa 'store k (incf i) 'e)
                 (ssa 'copy k (incf i) 'p (ssa-get-parent-index c))))

(to ssa-if k cnd then else
  ! then-label = ssa-name "then"
  ! end-label = ssa-name "endif"
  ! ssa 'branch (ev cnd) then-label
  ! ssa-expr k else
  ! ssa 'jmp end-label
  ! ssa 'local_label then-label
  ! ssa-expr k then
  ! ssa 'local_label end-label)

(to ssa-hoist-decls expr hoist ; C/C++ style declaration hoisting
  ! when (atom expr) (return-from ssa-hoist-decls expr)
  ! match expr
     (("_fn" . xs) expr)
     ((("_fn" as . xs) . vs)
      (setf vs (m v vs (ssa-hoist-decls v hoist)))
      (if (stringp as)
          (progn (funcall hoist `(,as))
                 `("_progn" ("_set" ,as ("_list" ,@vs))
                            ,@(m x xs (ssa-hoist-decls x hoist))))
          (progn (funcall hoist as)
                 `("_progn" ,@(m a as `("_set" ,a ,(pop vs)))
                            ,@(m x xs (ssa-hoist-decls x hoist))))))
     (xs (m x xs (ssa-hoist-decls x hoist))))

(to ssa-let k args vals xs
  ! body = ssa-hoist-decls `("_progn" ,@xs)
             (fn hs ! setf args (append args hs)
                    ! setf vals (append vals (m h hs 0)))
  ! unless args
      (ssa-expr k body)
      (return-from ssa-let)
  ! f = ssa-name "f"
  ! (ssa-body cs) = ssa-fn-body k f args body () nil nil
  ! nparents = length cs
  ! p = ssa-var "p" ;; parent environment
  ! ssa 'local_closure p nparents
  ! i = -1
  ! e c cs (! if (equal c *ssa-ns*) ; self?
                 (ssa 'store p (incf i) 'e)
                 (ssa 'copy p (incf i) 'p (ssa-get-parent-index c)))
  ! e = ssa-var "env"
  ! ssa 'arglist e (length args)
  ! i = -1
  ! e v vals (ssa 'arg_store e (incf i) (ev v))
  ! save-p = ssa-var "save_p"
  ! save-e = ssa-var "save_e"
  ! ssa 'move save-p 'p
  ! ssa 'move save-e 'e
  ! ssa 'move 'e e
  ! ssa 'move 'p p
  ! setf *ssa-out* `(,@ssa-body ,@*ssa-out*)
  ! ssa 'move 'p save-p
  ! ssa 'move 'e save-e)

(to ssa-apply k f as
  ! match f (("_fn" bs . body)
             (return-from ssa-apply (ssa-let k bs as body)))
  ! ssa 'push_base
  ! *ssa-bases* = cons nil *ssa-bases*
  ! h = ev f
  ! vs = m a as (ev a)
  ! e = ssa-var "env"
  ! ssa 'arglist e (length as)
  ! i = -1
  ! e v vs (ssa 'arg_store e (incf i) v)
  ! if (fn-sym? f) (ssa 'call k h) (ssa 'call_tagged k h))

(to resolve-method name
  ! m = gethash name *resolved-methods*
  ! when m (return-from resolve-method m)
  ! setf m (ssa-global "m")
  ! setf (gethash name *resolved-methods*) m
  ! method-name-bytes = ssa-cstring name
  ! push `(resolve_method ,m ,method-name-bytes) *ssa-raw-inits*
  ! m)

(to ssa-apply-method k name o as
  ! ssa 'push_base
  ! *ssa-bases* = cons nil *ssa-bases*
  ! setf as `(,o ,@as)
  ! vs = m a as (ev a)
  ! e = ssa-var "env"
  ! ssa 'arglist e (length as)
  ! i = -1
  ! e v vs (ssa 'arg_store e (incf i) v)
  ! ssa 'call_method k (car vs) (resolve-method (second name)))

(to ssa-set k place value
  ! r = ev value
  ! ssa-symbol nil place r
  ! ssa 'move k r)

;; FIXME: _label should be allowed only inside of _progn
(to ssa-progn k xs
  ! unless xs (setf xs '(()))
  ! d = ssa-var "dummy"
  ! e x xs
      (match x
        (("_label" name)
          (! (b . bs) = *ssa-bases*
           ! setf *ssa-bases* `((,name ,@b) ,@bs))))
  ! while xs
     (! x = pop xs
      ! unless xs (setf d k)
      ! ssa-expr d x
      ! when (and (headed "_label" x) (not xs))
          (ssa 'move d "Void")))

(to expr-symbols-sub expr syms
  ! cond ((stringp expr) (setf (gethash expr syms) t))
         ((listp expr) (e x expr (expr-symbols-sub x syms))))

(to expr-symbols expr
  ! syms = make-hash-table :test 'equal
  ! expr-symbols-sub expr syms
  ! syms)

(to uniquify-let xs
  ! match xs
    ((("_fn" as . body) . vs)
     (when (stringp as) (return-from uniquify-let xs))
     (when (/= (length as) (length vs)) (error "invalid number of arguments in ~a" xs))
     (unless (find-if (fn v ! match v (("_import" x y) 1)) vs)
       (return-from uniquify-let xs))
     (let ((used (expr-symbols body))
           (new-as nil)
           (new-vs nil))
       (while as
         (let* ((a (pop as))
                (v (pop vs)))
           (match v
             (("_import" x ("_quote" y))
              (when (gethash y used)
                (push a new-as)
                (push v new-vs)))
             (else
              (push a new-as)
              (push v new-vs)))))
       (setf as (reverse new-as))
       (setf vs (reverse new-vs))
       (setf xs `(("_fn" ,as ,@body) ,@vs))
       ))
  ! xs)

(to uniquify-form expr
  ! match expr
     (("_fn" as . body)
      (! bs = if (stringp as) `(,as) as
       ! us = remove-duplicates bs :test 'equal
       ! when (/= (length us) (length bs)) (error "duplicate args in {bs}")
       ! rs = m b bs (list b (ssa-name b))
       ! *uniquify-stack* = cons rs *uniquify-stack*
       ! bs = m r rs (second r)
       ! bs = if (stringp as) (first bs) bs
       ! `("_fn" ,bs ,@(m x body (uniquify-expr x)))))
     (("_quote" x)
      (when (stringp x)
        (setf (gethash x *hoisted-texts*) (ssa-name "T")))
      expr)
     (("_label" x) expr)
     (("_goto" x) expr)
     (("_call" . xs) (uniquify-form xs))
     (xs
      (setf xs (uniquify-let xs))
      (m x xs (uniquify-expr x))))

(to uniquify-name s
  ! e closure *uniquify-stack*
     (e x closure
        (when (equal (first x) s)
          (return-from uniquify-name (second x)))))

(to special-sym? x ! and (stringp x) (> (length x) 0) (eql (aref x 0) #\_))

(to uniquify-atom expr
  ! unless (stringp expr) (return-from uniquify-atom expr)
  ! when (special-sym? expr) (return-from uniquify-atom expr)
  ! renamed = uniquify-name expr
  ! unless renamed (error "undefined variable: ~a" expr)
  ! renamed)

(to uniquify-expr expr
  ! if (listp expr)
       (uniquify-form expr)
       (uniquify-atom expr))

;; give symbols unique names: allows non-local references
(to uniquify expr
  ! *uniquify-stack* = nil
  ! r = uniquify-expr expr
  ! ks = nil
  ! vs = nil
  ! maphash (fn k v ! push k ks ! push v vs) *hoisted-texts*
  ! `(("_fn" ,vs ,r) ,@(m k ks `("_text" ,k))))

(to ssa-list k xs
  ! when (= (length xs) 0) (return-from ssa-list (ssa 'move k "Empty"))
  ! l = ssa-var "l"
  ! ssa 'arglist l (length xs)
  ! i = -1
  ! e x xs (ssa 'arg_store l (incf i) (ev x))
  ! ssa 'tagged k l 'T_LIST)

(to ssa-data k type xs
  ! size = length xs
  ! bytes-name = ssa-cstring (second type)
  ! type-var = ssa-global "t"
  ! (! *ssa-out* = nil
     ! ssa 'type type-var bytes-name bytes-name size
     ! setf *ssa-raw-inits* (append *ssa-out* *ssa-raw-inits*))
  ! ssa 'alloc_data k type-var size
  ! i = -1
  ! e x xs (ssa 'dinit k (incf i) (ev x)))

(to ssa-dget k src off
  ! unless (integerp off) (error "dget: offset must be integer")
  ! ssa 'dget k (ev src) off)

(to ssa-dset k dst off value
  ! unless (integerp off) (error "dset: offset must be integer")
  ! d = ssa-var "d"
  ! ssa-expr d dst
  ! ssa-expr k value
  ! ssa 'dset d off k)

(to ssa-dmet k method-name type-name handler
  ! method-var = resolve-method (second method-name)
  ! type-name-bytes = ssa-cstring (second type-name)
  ! type-var = ssa-global "t"
  ! push `(resolve_type ,type-var ,type-name-bytes) *ssa-raw-inits*
  ! ssa 'dmet method-var type-var (ev handler)
  ! ssa 'move k 0)

(to ssa-import k lib symbol
  ! lib = second lib
  ! symbol = second symbol
  ! g = ssa-global "i"
  ! push `(import ,g ,lib ,symbol ,(ssa-cstring lib) ,(ssa-cstring symbol)) *ssa-raw-inits*
  ! ssa 'move k g)

(to ssa-label name ! ssa 'local_label name)

(to ssa-goto name
  ! n = position-if (fn b ! find name b :test 'equal) *ssa-bases*
  ! unless n (error "cant find label {name}")
  ! while (> n 0)
     (ssa 'gc (ssa-var "d") 0) ; have to GC, simple pop_base wont LIFT
     (ssa 'pop_base)
     (decf n)
  ! ssa 'jmp name)

(to ssa-mark name
  ! v = ssa-var "m"
  ! ssa-text v (second name)
  ! ssa 'mark v)

(to ssa-fixed1 k op x ! ssa op k (ev x))
(to ssa-fixed2 k op a b ! ssa op k (ev a) (ev b))

(to ssa-alloc k n
  ! x = ssa-var "x"
  ! ssa-fixed1 x 'fixnum_unfixnum n
  ! ssa 'arglist k x)

(to ssa-store base off value ! ssa 'untagged_store (ev base) (ev off) (ev value))
(to ssa-tagged k tag x ! ssa 'tagged k (ev x) (second tag))

(to ssa-text k s ! ssa 'text k (ssa-cstring s))

(to ssa-form k xs
  ! match xs
    (("_fn" as body) (ssa-fn (ssa-name "n") k as body xs))
    (("_if" cnd then else) (ssa-if k cnd then else))
    (("_quote" x . xs) (ssa-quote k x))
    (("_set" place value) (ssa-set k place value))
    (("_progn" . xs) (ssa-progn k xs))
    (("_label" name) (ssa-label name))
    (("_goto" name) (ssa-goto name))
    (("_mark" name) (ssa-mark name))
    (("_data" type . xs) (ssa-data k type xs))
    (("_dget" src index) (ssa-dget k src index))
    (("_dset" dst index value) (ssa-dset k dst index value))
    (("_dmet" method type handler) (ssa-dmet k method type handler))
    (("_mcall" o m . as) (ssa-apply-method k m o as))
    (("_list" . xs) (ssa-list k xs))
    (("_text" x) (ssa-text k x))
    (("_alloc" n) (ssa-alloc k n))
    (("_store" base off value) (ssa-store base off value))
    (("_tagged" tag x) (ssa-tagged k tag x))
    (("_import" lib symbol) (ssa-import k lib symbol))
    (("_add" a b) (ssa-fixed2 k 'fixnum_add a b))
    (("_eq" a b) (ssa-fixed2 k 'fixnum_eq a b))
    (("_lt" a b) (ssa-fixed2 k 'fixnum_lt a b))
    (("_gte" a b) (ssa-fixed2 k 'fixnum_gte a b))
    (("_tag" x) (ssa-fixed1 k 'fixnum_tag x))
    (("_fatal" msg) (ssa 'fatal (ev msg)))
    (("_this_method") (ssa 'this_method k))
    (("_type_id" o) (ssa 'type_id k (ev o)))
    ((f . as) (ssa-apply k f as))
    (() (ssa-atom k :void))
    (else (error "invalid CPS form: ~a" xs)))

(to ssa-atom k x
  ! cond
    ((integerp x) (ssa 'load_fixnum k x))
    ((stringp x) (ssa-symbol k x nil))
    ((eql x :void) (ssa 'move k "Void"))
    ((eql x :empty) (ssa 'move k "Empty"))
    (t (error "unexpected ~a" x)))

(to ssa-expr k x ! if (listp x) (ssa-form k x) (ssa-atom k x))

(to produce-ssa entry expr
  ! *ssa-env* = nil
  ! *ssa-out* = nil
  ! *ssa-fns* = nil
  ! *ssa-raw-inits* = nil
  ! *ssa-closure* = nil
  ! *ssa-bases* = '(())
  ! *hoisted-texts* = make-hash-table :test 'equal
  ! *resolved-methods* = make-hash-table :test 'equal
  ! ssa 'entry entry
  ! r = ssa-var "result"
  ! expr = uniquify expr
  ! ssa = ssa-expr r expr
  ! ssa 'return r
  ! ssa 'entry "setup"
  ! setf *ssa-out* (append *ssa-raw-inits* *ssa-out*)
  ! ssa 'return_no_gc 0
  ! rs = apply #'concatenate 'list `(,@(reverse *ssa-fns*) ,*ssa-out*)
  ;;! rs = peephole-optimize rs
  ! nreverse rs)

(defparameter *compiled* nil)

(to to-c-emit x ! push x *compiled*)

(to cnorm xs
  ! (x . xs) = xs
  ! to-c-emit (format nil '"  ~a(~{~a~^,~});" x xs))

(to ssa-to-c xs
  ! *compiled* = nil
  ! statics = nil
  ! decls = nil
  ! imports = make-hash-table :test 'equal
  ! to-c-emit "BEGIN_CODE"
  ! e x xs
    (match x
      ((''entry name) (to-c-emit "ENTRY({name})"))
      ((''label name)
       (push "DECL_LABEL({name})" decls)
       (to-c-emit "LABEL({name})"))
      ((''global name) (push "static void *{name};" decls))
      ((''alloc_closure place name size)
       (push "#define {name}_size {size}" decls)
       (cnorm x))
      ((''type place name tagname size)
       (! tname = ssa-name "n"
        ! to-c-emit "  RESOLVE_TYPE({place}, {name});"
        ! to-c-emit "  VAR({tname});"
        ! to-c-emit "  TEXT({tname}, {tagname});"
        ! to-c-emit "  SET_TYPE_SIZE_AND_NAME((intptr_t){place}, {size}, {tname});"))
      ((''import dst lib symbol lib-cstr symbol-cstr)
       (! key = "{lib}::{symbol}"
        ! import = gethash key imports
        ! lib-exports = gethash lib imports
        ! if import
             (to-c-emit "  MOVE({dst}, {import});")
             (! unless lib-exports
                  (setf lib-exports (ssa-name "n"))
                  (to-c-emit "  void *{lib-exports} = api->load_lib(api,(char*)({lib-cstr}));")
                  (setf (gethash lib imports) lib-exports)
              ! symbol-text = ssa-name "s"
              ! to-c-emit "  VAR({symbol-text});"
              ! to-c-emit "  TEXT({symbol-text}, {symbol-cstr});"
              ! to-c-emit "  {dst} = api->find_export(api, {symbol-text}, {lib-exports});"
              ! setf (gethash key imports) dst)))
      ((''bytes name values)
       (push (format nil '"static uint8_t ~a[] = {~{~a~^,~}};" name values) decls))
      (else (cnorm x)))
  ! to-c-emit "END_CODE"
  ! format nil '"~{~a~%~}" (reverse (append *compiled* decls (list "#include \"runtime.h\""))))

(to ssa-produce-file file src
  ! ssa = produce-ssa "entry" src
  ! text = ssa-to-c ssa
  ! save-text-file file text)





(defun var-sym? (s) (and (stringp s) (string/= s "") (upper-case-p (aref s 0))))
(defun fn-sym? (s) (and (stringp s) (not (var-sym? s))))

(defun expand-list-hole (key hole hit miss)
  (match hole
    (() (return-from expand-list-hole
          `("if" ("_mcall" ,key "end")
                 ,hit
                 ,miss)))
        ;;(return-from expand-list-hole (expand-hole key :empty hit miss)))
    ((("@" zs)) (return-from expand-list-hole (expand-hole key zs hit miss)))
    ((("@" zs) . more) (error "@ in the middle isn't supported")))
  (let* ((h (ssa-name "X"))
         (hit (expand-list-hole key (cdr hole) hit miss)))
    `("if" ("_mcall" ,key "end")
           ,miss
           ("let_" ((,h ("_mcall" ,key "head"))
                    (,key ("_mcall" ,key "tail")))
               ,(expand-hole h (car hole) hit miss)))))

(defun expand-hole (key hole hit miss)
  (unless (consp hole)
    (when (fn-sym? hole) (setf hole `("_quote" ,hole)))
    (return-from expand-hole
      (if (equal hole "_")
          hit
          (if (var-sym? hole)
              `("let_" ((,hole ,key))
                        ,hit)
              `("if" ("><" ,hole ,key)
                     ,hit
                     ,miss)))))
  (when (equal (car hole) ">")
    (return-from expand-hole
       (expand-hole key (second hole) (expand-hole key (third hole) hit miss) miss)))
  (when (equal (car hole) "in")
    (return-from expand-hole
      `("if" ,(expand-match key (m x (cdr hole) `(,x 1)) 0)
             ,hit
             ,miss)))
  (when (equal (car hole) "not")
    (return-from expand-hole
      `("if" ,(expand-match key (m x (cdr hole) `(,x 1)) 0)
             ,miss
             ,hit)))
  (when (equal (car hole) "bind")
    (return-from expand-hole
      (let ((g (ssa-name "G")))
        `("let_" ((,g (,(second hole) ,key)))
           ,(expand-hole g (third hole) hit miss)))))
  (when (equal (car hole) "=>")
    (return-from expand-hole
      `("let_" ((,(first (second hole)) ,key))
         ("if" ("|" ,@(third hole))
             ,hit
             ,miss))))
  (when (equal (car hole) "&")
    (return-from expand-hole
      `("if" ("><" ,(second hole) ,key)
             ,hit
             ,miss)))
  (when (equal (car hole) "[]")
    (return-from expand-hole
      `("if" ("_mcall" ,key ("_quote" "is_list"))
             ,(expand-list-hole key (cdr hole) hit miss)
             ,miss)))
  (error "bad hole: ~a" hole))

(defun expand-match (keyform cases default &key (keyvar nil))
  (let* ((key (or keyvar (ssa-name "Key")))
         (e (ssa-name "end"))
         (d (ssa-name "default"))
         (r (ssa-name "R"))
         (ys (reduce (lambda (next case)
                       (let* ((name (ssa-name "c"))
                              (next-label (when next (second (first next))))
                              (miss (if next-label `("_goto" ,next-label) `("_goto" ,d)))
                              (hit `("_progn" ("_set" ,r ("_progn" ,@(cdr case)))
                                              ("_goto" ,e))))
                         `(("_label" ,name) ,(expand-hole key (car case) hit miss) ,@next)))
                     (cons nil (reverse cases)))))
    `("let_" ((,key ,keyform)
             (,r 0))
       ,@(cdr ys)
       ("_label" ,d)
       ("_set" ,r ,default)
       ("_label" ,e)
       ,r)))

(to mangle-name name
  ! cs = coerce name 'list
  ! with-output-to-string (out)
    (format out "_")
    (e c cs
       (let ((n (char-code c)))
         (if (or (and (<= (char-code #\a) n) (<= n (char-code #\z)))
                 (and (<= (char-code #\A) n) (<= n (char-code #\Z)))
                 (and (<= (char-code #\0) n) (<= n (char-code #\9))))
             (format out "~a" (string c))
             (format out "_~2,'0x" n)))))

(to result-and-label name
  ! mangled = mangle-name name
  ! list (concatenate 'string "ReturnOf" mangled "_")
         (concatenate 'string "end_of" mangled "_"))

(to expand-named name body
  ! (r end) = result-and-label name
  ! `("let_" ((,r 0))
       ("_set" ,r ,body)
       ("_label" ,end)
       ,r))

(to expand-leave name value
  ! (r end) = result-and-label name
  ! `("_progn" ("_set" ,r ,value)
               ("_goto" ,end)))

(to add-pattern-matcher args body
  ! default = match args
               ((("&" default) . tail)
                (setf args tail)
                default)
               (else `("_list"))
  ! match args
    ((("@" all)) (setf args all))
    (else
      (! gs = m a args (ssa-name "A")
       ;; FIXME: value gets duplicated - potentially exponential code growth
       ! e g gs (setf body (expand-match g `((,(pop args) ,body)) default))
       ! setf args gs))
  ! list args body)

(to pattern-arg x ! or (not (stringp x)) (fn-sym? x))

(to expand-block-item-fn name args body
  ! kname = concatenate 'string "_k_" name
  ! (args body) = if (find-if #'pattern-arg args) (add-pattern-matcher args body) (list args body)
  ! setf body `("default_leave_" ,name ,(expand-named name body))
  ! list name `("_fn" ,args ("_progn" ("_mark" ,name) ,body)))

(to expand-destructuring value bs
  ! xs-var = nil
  ! match (car (last bs))
     (("@" x)
      (setf xs-var x)
      (setf bs (butlast bs)))
  ! i = -1
  ! o = ssa-name "O"
  ! ys = m b bs (list b `("_mcall" ,o ,'"." ,(incf i)))
  ! when xs-var (setf ys `((,xs-var ("_mcall" ,o "drop" ,(length bs))) ,@ys))
  ! `((,o ,value) ,@ys))

(to expand-assign place value
 ! match place
    (("." object field)
     (if (fn-sym? field)
         `("_mcall" ,object ,"set_{field}" ,value)
         `("_mcall" ,object "!" ,field ,value)))
    (else `("_set" ,place ,value)))

(to expand-assign-result as
  ! ys = copy-list as
  ! v = nil
  ! p = position-if (fn x ! match x (("!" x) (setf v x) t)) as
  ! unless p (error "!!: no ! in ~a" as)
  ! setf (nth p ys) v
  ! expand-assign v ys)

(to expand-block-item-data name fields
  ! gs = m f fields (ssa-name "A")
  ! o = ssa-name "O"
  ! v = ssa-name "V"
  ! j = -1
  ! k = -1
  ! `(("=" (,"new_{name}" ,@gs) ("_data" ,name ,@gs))
      ("=" (("." ,name ,"is_{name}")) 1)
      ("=" (("." "_" ,"is_{name}")) 0)
      ,@(m f fields `("=" (("." ,name ,f)) ("_dget" "Me" ,(incf j))))
      ,@(m f fields `("=" (("." ,name ,"set_{f}") ,v) ("_dset" "Me" ,(incf k) ,v)))))

(to expand-block-item-method type name args body
  ! setf body `("default_leave_" ,name ,(expand-named name body))
  ! list nil `("_dmet" ,name ,type ("_fn" ("Me" ,@args)
                                      ("_progn" ("_mark" ,"{type}.{name}") ,body))))

(to expand-block-item x
  ! y = match x
     (("data" name . fields)
      (return-from expand-block-item
        (apply #'concatenate 'list
               (m x (expand-block-item-data name fields)
                  (expand-block-item x)))))
     (("=" ("!!" ("!" place)) value) (list nil (expand-assign place value)))
     (("=" (("." type method) . args) body) (expand-block-item-method type method args body))
     (("=" (("[]" . bs)) value) (return-from expand-block-item (expand-destructuring value bs)))
     (("=" (name . args) value)
      (if (var-sym? name)
          (list name value) ;; FIXME: check that args are empty
          (expand-block-item-fn name args value)))
     (else (list nil x))
  ! list y)

(to make-multimethod xs
  ! when (match xs ((("=>" as expr)) (or (not as) (var-sym? (first as)))))
     (return-from make-multimethod (first xs))
  ! dummy = ssa-name "D"
  ! all = ssa-name "A"
  ! key = ssa-name "K"
  ! cases = m x xs
      (match x
        (("=>" as expr)
         (unless as (error "prototype doesnt support no args multimethods"))
         (list (first as)
               `("_fn" ,(if (var-sym? (first as)) as `(,dummy ,@(cdr as)))
                       ,expr))))
  ! sel = expand-match `("_mcall" ,all "." 1) cases `("no_method_" ,key) :keyvar key
  ! `("_fn" ,all ("_mcall" ,all "apply" ,sel)))

(to expand-block xs
  ! when (and (= (length xs) 1)
              (or (atom (first xs))
                  (not (match (first xs) (("=" . _) t)))))
     (return-from expand-block (first xs))
  ! ms = nil
  ! ys = nil
  ! e x xs (match x
             (("=>" a b) (push x ms))
             (else (push x ys)))
  ! when ms (push (make-multimethod (reverse ms)) ys)
  ! xs = reverse ys
  ! xs = m x xs (expand-block-item x)
  ! xs = apply #'concatenate 'list xs
  ! r = nil
  ! e x (reverse xs)
      (setf r
        (match x
          ((a b)
           (if a
               (if (fn-sym? a)
                   `(("_set" ,a ,b) ,@r)
                   `(("let_" ((,a ,b)) ,(if r `("_progn" ,@r) :void))))
               `(,b ,@r)))))
  ! setf r `("_progn" ,@r)
  ! bs = remove-if-not (fn x ! fn-sym? (car X)) xs
  ! when bs (setf r `("let_" ,(m b bs `(,(first b) :void)) ,r))
  ! r)

(to expand-export xs
  ! xs = m x xs
         (match x
           (("\\" x) `("_list" ("_quote" ,x) ("new_macro" ("_quote" ,x) ("&" ,x))))
           (else `("_list" ("_quote" ,x) ("&" ,x))))
  ! `("_list" ,@xs))

(to expand-while head body
  ! l = ssa-name "l"
  ! `("_progn" ("_label" ,l)
               ("_if" ,head
                      ("_progn" ,body ("_goto" ,l))
                      ())))

;;FIXME: code can segfault if users sets i
(to expand-times var count body
  ! i = if var var (ssa-name "I")
  ! n = ssa-name "N"
  ! `("|" ("=" (,n) ,count)
          ("=" (,i) (0))
          ("unless" ("and" ("_eq" ("_tag" ,n) 0)
                           ("_gte" ,n 0))
            ("_fatal" "times: bad loop count"))
          ("while" ("_lt" ,i ,n)
             ("|" ,body
                  ("_set" ,i ("_add" ,i 1))))))

(to expand-dup var count body
  ! i = if var var (ssa-name "I")
  ! n = ssa-name "N"
  ! ys = ssa-name "Ys"
  ! `("|" ("=" (,n) ,count)
          ("=" (,i) (0))
          ("unless" ("and" ("_eq" ("_tag" ,n) 0)
                           ("_gte" ,n 0))
            ("_fatal" "dup: bad loop count"))
          ("=" (,ys) ("_alloc" ,n))
          ("while" ("_lt" ,i ,n)
             ("|" ("_store" ,ys ,i ,body)
                  ("_set" ,i ("_add" ,i 1))))
          ("_tagged" ("_quote" "T_LIST") ,ys)))

(to expand-map item items body
  ! xs = ssa-name "Xs"
  ! i = ssa-name "I"
  ! n = ssa-name "N"
  ! `("|" ("=" (,xs) ("_mcall" ,items "harden"))
          ("dup" ,i ("_mcall" ,xs "size")
            ("|" ("=" (,item) ("_mcall" ,xs "." ,i))
                 ,body))))

(to expand-for item items body
  ! xs = ssa-name "Xs"
  ! i = ssa-name "I"
  ! n = ssa-name "N"
  ! `("|" ("=" (,xs) ("_mcall" ,items "harden"))
          ("times" ,i ("_mcall" ,xs "size")
            ("|" ("=" (,item) ("_mcall" ,xs "." ,i))
                 ,body))))


(to incut? x ! match x (("@" x) t))

(to expand-list as
  ! incut-count = count-if #'incut? as
  ! when (= 0 incut-count)  (return-from expand-list `("_list" ,@as))
  ! when (= 1 incut-count) 
      (match (car (last as))
        (("@" xs)
         (setf as (cdr (reverse as)))
         (while as
           (setf xs `("_mcall" ,xs "pre" ,(car as)))
           (setf as (cdr as)))
         (return-from expand-list xs)))
  ! as = m a as (if (incut? a) (second a) `("_list" ,a))
  ! `("_mcall" ("_list" ,@as) "join"))

(to expand-string-splice xs
  ! match xs ((x) (when (stringp x) (return-from expand-string-splice `("_quote" ,x))))
  ! as = m x xs (if (stringp x) `("_quote" ,x) `("_mcall" ,x "textify_"))
  ! `("_mcall" ("_list" ,@as) "unchars"))

(to expand-and a b ! `("if" ,a ,b 0))
(to expand-or a b
  ! v = ssa-name "V"
  ! `("let_" ((,v ,a)) ("if" ,v ,v ,b)))

(to expand-lambda as body
  ! body = `("|" ,body)
  ! (as body) = if (find-if #'pattern-arg as) (add-pattern-matcher as body) (list as body)
  ! `("_fn" ,as ,body))

(to expand-quasiquote o
  ! unless (listp o) (return-from expand-quasiquote `("_quote" ,o))
  ! match o
     (("$" o) o)
     (else `("[]" ,@(m x o (expand-quasiquote x)))))

(to group-by n xs
  ! ys = nil
  ! while xs
    (! setf n (min n (length xs))
     ! push (subseq xs 0 n) ys
     ! setf xs (subseq xs n))
  ! reverse ys)

(to expand-pop o
  ! r = ssa-name "R"
  ! `("|" ("=" (,r) ("_mcall" ,o "head"))
          ("_set" ,o ("_mcall" ,o "tail"))
          ,r))

(to expand-push o item ! `("_set" ,o ("_mcall" ,o "pre" ,item)))

(defparameter *default-leave* nil)

(to expand-shade bs body
  ! gs = m b bs `(,(ssa-name "G") ,@b)
  ! r = ssa-name "R"
  !`("let_" ((,r 0) ,@(m g gs `(,(first g) ,(second g))))
       ,@(m g gs `("_set" ,(second g) ,(third g)))
       ("_set" ,r ,body)
       ,@(m g gs `("_set" ,(second g) ,(first g)))
       ,r))

(to normalize-matryoshka o
  ! match o ((x) (if (fn-sym? x)
                     o
                     (normalize-matryoshka x)))
            (x x))

(defun builtin-expander (xs &optional (head nil))
  ;; FIXME: don't notmalize macros, because the may expand for fn syms
  (let ((xs (normalize-matryoshka xs))
        (ys nil))
    (when (atom xs) (return-from builtin-expander xs))
    (setf ys
      (match xs
        (("_fn" as body)
         (return-from builtin-expander
           `("_fn" ,as ,(builtin-expander body))))
        (("_set" place value)
         (return-from builtin-expander
           `("_set" ,place ,(if (fn-sym? value)
                                `("_quote" ,value)
                                (builtin-expander value)))))
        (("_label" name) (return-from builtin-expander xs))
        (("_goto" name) (return-from builtin-expander xs))
        (("_quote" x) (return-from builtin-expander xs))
        (("_nomex" x) (return-from builtin-expander x))
        (("&" o) (return-from builtin-expander
                   (if (fn-sym? o) o `(,(builtin-expander o)))))
        (("default_leave_" name body)
         (return-from builtin-expander
           (let ((*default-leave* name))
             (builtin-expander body))))
        (("let_" bs . xs)
         `("_call"
           ("_fn" ,(m b bs (first b)) ,`("_progn" ,@xs))
           ,@(m b bs (second b))))
        (("if" a b c) `("_if" ,a ,b ,c))
        (("=>" as body) (expand-lambda as body))
        (("not" . xs) `("_if" ,xs 0 1))
        (("when" . xs) `("_if" ,(butlast xs) ,@(last xs) :void))
        (("unless" . xs) `("_if" ,(butlast xs) :void ,@(last xs)))
        (("while" . xs) (expand-while (butlast xs) (car (last xs))))
        (("till" . xs) (expand-while `("not" ,(butlast xs)) (car (last xs))))
        (("for" x xs body) (expand-for x xs body))
        (("map" x xs body) (expand-map x xs body))
        (("dup" x xs body) (expand-dup x xs body))
        (("dup" xs body) (expand-dup nil xs body))
        (("dup" xs) (expand-dup nil xs 0))
        (("times" x xs body) (expand-times x xs body))
        (("pop" o) (expand-pop o))
        (("push" item o) (expand-push o item))
        (("and" a b) (expand-and a b))
        (("or" a b) (expand-or a b))
        (("|" . xs) (expand-block xs))
        ((";" . xs) (expand-block xs))
        (("[]" . as) (expand-list as))
        (("^" a b) `(,b ,a))
        ((":" a b) `(,@a ,b))
        (("\"" . xs) (expand-string-splice xs))
        (("." a b) (cond
                     ((fn-sym? b) `("{}" ,xs))
                     (t `("_mcall" ,a "." ,b))))
        (("{}" ("." a b) . as) `("_mcall" ,a ,b ,@as))
        (("{}" ("^" a b) . as) `(,b ,@as ,a))
        (("{}" h . as) `("_mcall" ,h "{}" ,as))
        (("{}" . else) (error "bad {}: ~%" xs))
        (("\\" o) (expand-quasiquote o))
        (("+" a b) `("_mcall" ,a "+" ,b))
        (("-" a) `("_mcall" ,a "neg"))
        (("-" a b) `("_mcall" ,a "-" ,b))
        (("*" a b) `("_mcall" ,a "*" ,b))
        (("/" a b) `("_mcall" ,a "/" ,b))
        (("%" a b) `("_mcall" ,a  "%" ,b))
        (("<" a b) `("_mcall" ,a "<" ,b))
        ((">" a b) `("_mcall" ,a  ">" ,b))
        (("<<" a b) `("_mcall" ,a "<<" ,b))
        ((">>" a b) `("_mcall" ,a  ">>" ,b))
        (("><" a b) `("_mcall" ,a "><" ,b))
        (("<>" a b) `("_mcall" ,a  "<>" ,b))
        (("and" a b) `("if" ,a ,b 0))
        (("or" a b) (let ((n (ssa-name "T")))
                      `("let_" ((,n ,a)) ("if" ,n ,n ,b))))
        (("named" name . body) (expand-named name `("_progn" ,@body)))
        (("leave" name value) (expand-leave name value))
        (("leave" value) (expand-leave *default-leave* value))
        (("<=" (place) value) (expand-assign place value))
        (("!!" . as) (expand-assign-result as))
        (("case" keyform . cases) (expand-match keyform (group-by 2 cases) 0))
        (("let" . xs) (expand-shade (group-by 2 (butlast xs)) (car (last xs))))
        (("export" . xs) (expand-export xs))
        (else (return-from builtin-expander
                (cons (builtin-expander (car xs))
                      (m x (cdr xs)
                         (if (fn-sym? x)
                             `("_quote" ,x)
                             (builtin-expander x))))))))
    (builtin-expander ys)))





(defparameter *root-folder* nil)
(defparameter *src-folders* nil)
(defparameter *dst-folder* nil)
(defparameter *header-timestamp* nil)

;; FIXME: do caching
(to get-lib-exports lib-name
  ! e folder *src-folders*
    (! lib-file = "{folder}{lib-name}.s"
     ! when (file-exists-p lib-file)
       (return-from get-lib-exports
         (! text = load-text-file lib-file
          ! expr = /normalize (/read text)
          ! match (first (last expr))
             (("export" . xs)
              (remove-if (fn x ! match x (("\\" x) t)) xs))
             (_ nil))))
  ! error "no {lib-name}.s")


(to shell command &rest args
  ! s = (make-string-output-stream)
  ! sb-ext:run-program command args :output s :search t :wait t
  ! get-output-stream-string s)

(to c-runtime-compiler dst src
  ! rt-folder = "{*root-folder*}runtime"
  ! shell "gcc" "-O1" "-Wno-return-type" "-Wno-pointer-sign" "-I" rt-folder #|"-DNDEBUG"|# "-g" "-o" dst src)

(to c-compiler dst src
  ! rt-folder = "{*root-folder*}runtime"
  ! shell "gcc" "-O1" "-Wno-return-type" "-Wno-pointer-sign" "-I" rt-folder #|"-DNDEBUG"|# "-g" "-fpic" "-shared" "-o" dst src)


(to file-older src-file dst-file
  ! dst-date = if (file-exists-p dst-file)
                  (file-write-date dst-file)
                  0
  ! and (<= (file-write-date src-file) dst-date)
        (<= *header-timestamp* dst-date))

(to compile-runtime src-file dst-file
  ! when (file-older src-file dst-file) (return-from compile-runtime)
  ! format t "compiling runtime...~%"
  ! (finish-output)
  ! result = c-runtime-compiler dst-file src-file
  ! when (string/= result "")
      (e l (split #\Newline result) (format t "~a~%" l)))

(to add-imports expr deps
  ! unless deps (return-from add-imports expr)
  ! `(("_fn" ,(m d deps (second d)) ,expr)
      ,@(m d deps `("_import" ("_quote" ,(first d))
                              ("_quote" ,(second d))))))

(to symta-compile-expr name dst expr
  ! uses = list "core" "prelude"
  ! expr = match expr
             (("|" ("use" . us) . xs)
              (setf uses `(,@uses ,@us))
              `("|" ,@xs))
             (else expr)
  ! setf uses (remove-if (fn x ! equal name x) uses)
  ! setf uses (remove-duplicates uses :test 'equal)
  ! deps = cdr uses
  ! e d deps (unless (compile-module d) (error "cant compile {d}.s"))
  ! format t "compiling {name}...~%"
  ! (finish-output)
  ! imports = apply #'concatenate 'list (m u uses
                                           (m e (get-lib-exports u)
                                              (list u e)))
  ! expr-with-deps = add-imports expr imports
  ! expanded-expr = builtin-expander expr-with-deps
  ! c-file = "{dst}.c"
  ! ssa-produce-file c-file expanded-expr
  ! result = c-compiler dst c-file
  ! when (string/= result "")
     (e l (split #\Newline result) (format t "~a~%" l))
  ! deps)

(to symta-read-file filename
  ! text = load-text-file filename
  ! (/init-tokenizer)
  ! /normalize (/read text)
  )

(to compile-module name
  ! e folder *src-folders*
    (! src-file = "{folder}{name}.s"
     ! when (file-exists-p src-file)
       (! dst-file = "{*dst-folder*}{name}"
        ! dep-file = "{dst-file}.dep"
        ! when (and (file-exists-p dep-file)
                    (file-older src-file dep-file))
          (! deps = second (symta-read-file dep-file)
           ! compiled-deps = m d deps (compile-module d)
           ! when (and (file-older src-file dst-file)
                       (every (fn x ! and x (file-older x dst-file))
                              compiled-deps))
              (return-from compile-module dst-file))
        ! expr = symta-read-file src-file
        ! deps = symta-compile-expr name dst-file expr
        ! deps-text = format nil '"~{~a~^ ~}" deps
        ! save-text-file dep-file deps-text
        ! return-from compile-module dst-file))
  ! nil)

(to build build-folder
  ! *root-folder* = "/Users/nikita/Documents/git/symta/"
  ! *src-folders* = list "{build-folder}src/" "{*root-folder*}src/"
  ! *dst-folder* = "{build-folder}lib/"
  ! runtime-src = "{*root-folder*}/runtime/runtime.c"
  ! *header-timestamp* = file-write-date "{*root-folder*}/runtime/runtime.h"
  ! runtime-path = "{build-folder}run"
  ! compile-runtime runtime-src runtime-path
  ! dst-file = compile-module "main"
  ! unless dst-file (error "cant compile main.s")
  ! result = shell runtime-path *dst-folder*
  ! e l (butlast (split #\Newline result)) (format t "~a~%" l)
  )
