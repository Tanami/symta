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
      (error (funcall g_error "{row},{col}: {car args}"))))

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
  ! ls = `(".." "+" "-" "*" "/" "%" "^" "." "->" "~" "|" ";" "," ":" "=" "=>"
           "><" "<>" "<" ">" "<<" ">>"
           "\\" "$" "@" "&" "!" (() :end)
           ")" ("(" ,(fn r o ! `(:|()| ,(/list r o :|)|))))
           "]" ("[" ,(fn r o ! `(:|[]| ,(/list r o :|]|))))
           "}" (,(string #\{) ,(fn r o ! `(:|{}| ,(/list r o :|}|))))
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
     ! when (token-is :end x) (funcall g_error "{orig}:{row},{col}: unclosed `{open}`")
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
  ! funcall g_error "{orig}:{row},{col}: {cause} `{or (getf tok :value) 'eof}`")
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
     (:integer (read-from-string v))
     (:kw (keywordize v))
     (:|()| (/parse v))
     (:|[]| `((:token :symbol :value "[]" :src (getf tok :src)) ,@(/parse v)))
     (:|\|| (ret (/bar tok)))
     (:if (ret (/if tok)))
     (:- (ret (/negate tok)))
     (otherwise (push tok g_input) (ret :fail))
  ! `(,@tok :parsed ,p))

(to delim? x ! match x ((:token (or :|:| :|=| :|=>| :|,| :if :then :else) . _) t))

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
  ! o = try (/op '(:|:| :|=| :|=>| :|,|)) (/logic)
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
(defparameter *ssa-inits* nil)
(defparameter *ssa-raw-inits* nil)
(defparameter *ssa-fns* nil)
(defparameter *ssa-closure* nil) ; other lambdas', this lambda references
(defparameter *compiler-meta-info* (make-hash-table :test 'eq))

(to set-meta meta object
  ! (setf (gethash object *compiler-meta-info*) meta)
  ! object)

(to get-meta object ! gethash object *compiler-meta-info*)

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
          (ssa 'add_tag k base 'T_LIST)
          (ret nil)
       ! unless value (return-from ssa-symbol (ssa 'arg_load k base pos))
       ! ssa 'arg_store base pos value
       ! if (eql base 'e)
            (ssa 'arg_store base pos value)
            (ssa 'lift base pos value)
       ))
     (else (error "undefined variable: ~a" x)))

(to ssa-quote-list-rec xs
   ! `("_list" ,@(m x xs (if (listp x) (ssa-quote-list-rec x) `("_quote" ,x)))))

(to ssa-quote-list k xs
  ! name = ssa-name "list"
  ! ssa 'move k name
  ! push `(,name ,(ssa-quote-list-rec xs)) *ssa-inits*)

(to cstring s ! `(,@(m c (coerce s 'list) (char-code c)) 0))

(to ssa-cstring src
  ! name = ssa-name "b"
  ! ssa 'bytes name (cstring src)
  ! name)

(to ssa-global name
  ! var-name = ssa-name name
  ! ssa 'global var-name
  ! var-name)

(to ssa-text k s
  ! bytes-name = ssa-cstring s
  ! name = ssa-global "s"
  ! push `(text ,name ,bytes-name) *ssa-raw-inits*
  ! ssa 'move k name)

(to ssa-quote k x
  ! cond
     ((stringp x) (ssa-text k x))
     ((listp x) (ssa-quote-list k x))
     ((integerp x) (ssa-atom k x))
     (t (error "unsupported quoted value: ~a" x)))

(to ssa-resolved name ! cons name *ssa-ns*)

(to ssa-fn-body k f args body o prologue epilogue
  ! cs = nil
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
          (ssa 'check_varargs size-var (get-meta o))
          (ssa 'check_nargs (length args) size-var (get-meta o)))
  ! unless k
     (setf k (ssa-name "result"))
     (ssa 'var k)
  ! ssa-expr k body
  ! when epilogue (ssa 'return k)
  ! list *ssa-out* (car *ssa-closure*)
  )

(to ssa-fn name k args body o
  ! f = ssa-name "f"
  ! (body cs) = ssa-fn-body nil f args body o t t
  ! push body *ssa-fns*
  ! nparents = length cs
  ;; check if we really need new closure here, because in some cases we can reuse parent's closure
  ;; a single argument to a function could be passed in register, while a closure would be created if required
  ;; a single reference closure could be itself held in a register
  ;; for now we just capture required parent's closure
  ! ssa 'closure k f nparents
  ! i = -1
  ! e c cs (! if (equal c *ssa-ns*) ; self?
                 (ssa 'store k (incf i) 'e)
                 (ssa 'copy k (incf i) 'p (ssa-get-parent-index c))))

(to ssa-if k cnd then else
  ! then-label = ssa-name "then"
  ! end-label = ssa-name "endif"
  ! c = ssa-name "cnd"
  ! ssa 'var c
  ! ssa-expr c cnd
  ! ssa 'branch c then-label
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
  ! p = ssa-name "p" ;; parent environment
  ! ssa 'var p
  ! ssa 'local_closure p nparents
  ! i = -1
  ! e c cs (! if (equal c *ssa-ns*) ; self?
                 (ssa 'store p (incf i) 'e)
                 (ssa 'copy p (incf i) 'p (ssa-get-parent-index c)))
  ! e = ssa-name "env"
  ! ssa 'var e
  ! ssa 'arglist e (length args)
  ! i = -1
  ! e v vals (! tmp = ssa-name "tmp"
              ! ssa 'var tmp
              ! ssa-expr tmp v
              ! ssa 'arg_store e (incf i) tmp)
  ! save-p = ssa-name "save_p"
  ! save-e = ssa-name "save_e"
  ! ssa 'var save-p
  ! ssa 'var save-e
  ! ssa 'move save-p 'p
  ! ssa 'move save-e 'e
  ! ssa 'move 'e e
  ! ssa 'move 'p p
  ! setf *ssa-out* `(,@ssa-body ,@*ssa-out*)
  ! ssa 'move 'p save-p
  ! ssa 'move 'e save-e
  )


(to ssa-var &optional (n "t")
  ! v = ssa-name n
  ! ssa 'var v
  ! v)

(to ssa-apply k f as
  ! match f (("_fn" bs . body) (return-from ssa-apply (ssa-let k bs as body)))
  ! ssa 'push_base
  ! h = ssa-var "head"
  ! ssa-expr h f
  ! vs = m a as
        (! v = ssa-var "a"
         ! ssa-expr v a
         ! v)
  ! e = ssa-var "env"
  ! nargs = length as
  ! ssa 'arglist e nargs
  ! i = -1
  ! e v vs (ssa 'arg_store e (incf i) v)
  ! when (fn-sym? f) (return-from ssa-apply (ssa 'call k h))
  ! method-name = and (> nargs 0)
                      (match as ((("_quote" x . renamed) . xs) (and (stringp x) x)))
  ! unless method-name (return-from ssa-apply (ssa 'call_tagged_dynamic k h))
  ! method-name-bytes = ssa-cstring method-name
  ! m = ssa-global "m"
  ! push `(resolve_method ,m ,method-name-bytes) *ssa-raw-inits*
  ! ssa 'call_tagged k h m)

(to ssa-set k place value
  ! r = ssa-name "r"
  ! ssa 'var r
  ! ssa-expr r value
  ! ssa-symbol nil place r
  ! ssa 'move k r)

(to ssa-progn k xs
  ! unless xs (setf xs '(()))
  ! d = ssa-name "dummy"
  ! ssa 'var d
  ! while xs
     (! x = car xs
      ! unless (cdr xs) (setf d k)
      ! ssa-expr d x
      ! when (and (headed "_label" x) (not (cdr xs)))
          (ssa 'move d "Void")
      ! setf xs (cdr xs)))

(defparameter *uniquify-stack* nil)

(to uniquify-name s
  ! e closure *uniquify-stack*
     (e x closure
        (when (equal (first x) s)
          (return-from uniquify-name (second x)))))

(to uniquify-form expr
  ! match expr
     (("_fn" as . body)
      (! bs = if (stringp as) `(,as) as
       ! rs = m b bs (list b (ssa-name b))
       ! *uniquify-stack* = cons rs *uniquify-stack*
       ! bs = m r rs (second r)
       ! bs = if (stringp as) (first bs) bs
       ! `("_fn" ,bs ,@(m x body (uniquify-expr x)))))
     (("_quote" x) expr)
     (("_label" x) expr)
     (("_goto" x) expr)
     (("_call" . xs) (uniquify-form xs))
     (xs
      (match xs
        ((("_fn" as . body) . vs)
         (when (and (not (stringp as)) (/= (length as) (length vs)))
           (error "invalid number of arguments in ~a" expr))))
      (m x xs (uniquify-expr x))))

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
  ! uniquify-expr expr)

(to ssa-fixed k op a b
  ! av = ssa-name "a"
  ! bv = ssa-name "b"
  ! ssa 'var av
  ! ssa 'var bv
  ! ssa-expr av a
  ! ssa-expr bv b
  ! ssa op k av bv)

(to ssa-list k xs
  ! l = ssa-name "l"
  ! ssa 'var l
  ! ssa 'arglist l (length xs)
  ! i = -1
  ! e x xs (! r = ssa-name "r"
            ! ssa 'var r
            ! ssa-expr r x
            ! ssa 'arg_store l (incf i) r)
  ! ssa 'add_tag k l 'T_LIST)

(to ssa-data k type xs
  ! size = length xs
  ! bytes-name = ssa-cstring (second type)
  ! type-var = ssa-global "t"
  ! (! *ssa-out* = nil
     ! ssa 'type type-var bytes-name bytes-name size
     ! setf *ssa-raw-inits* (append *ssa-out* *ssa-raw-inits*))
  ! ssa 'data k type-var size
  ! tmp = ssa-var "v"
  ! i = -1
  ! e x xs (! ssa-expr tmp x
            ! ssa 'dinit k (incf i) tmp))

(to ssa-dget k src off
  ! unless (integerp off) (error "dget: offset must be integer")
  ! s = ssa-var "s"
  ! ssa-expr s src
  ! ssa 'dget k s off)

(to ssa-dset k dst off value
  ! unless (integerp off) (error "dset: offset must be integer")
  ! d = ssa-var "d"
  ! ssa-expr d dst
  ! ssa-expr k value
  ! ssa 'dset d off k)

(to ssa-dmet k method-name type-name handler
  ! method-name-bytes = ssa-cstring (second method-name)
  ! method-var = ssa-global "m"
  ! push `(resolve_method ,method-var ,method-name-bytes) *ssa-raw-inits*
  ! type-name-bytes = ssa-cstring (second type-name)
  ! type-var = ssa-global "t"
  ! push `(resolve_type ,type-var ,type-name-bytes) *ssa-raw-inits*
  ! h = ssa-var "h"
  ! ssa-expr h handler
  ! ssa 'dmet method-var type-var h
  ! ssa 'move k 0)

(to ssa-import k lib symbol
  ! lib = second lib
  ! symbol = second symbol
  ! g = ssa-global "i"
  ! push `(import ,g ,lib ,symbol ,(ssa-cstring lib) ,(ssa-cstring symbol)) *ssa-raw-inits*
  ! ssa 'move k g)

(to ssa-form k xs
  ! match xs
    (("_fn" as body) (ssa-fn (ssa-name "n") k as body xs))
    (("_if" cnd then else) (ssa-if k cnd then else))
    (("_quote" x . xs) (ssa-quote k x))
    (("_set" place value) (ssa-set k place value))
    (("_progn" . xs) (ssa-progn k xs))
    (("_label" name) (ssa 'local_label name))
    (("_goto" name) (ssa 'jmp name))
    (("_data" type . xs) (ssa-data k type xs))
    (("_dget" src index) (ssa-dget k src index))
    (("_dset" dst index value) (ssa-dset k dst index value))
    (("_dmet" method type handler) (ssa-dmet k method type handler))
    (("_list" . xs) (ssa-list k xs))
    (("_import" lib symbol) (ssa-import k lib symbol))
    #|(("_add" a b) (ssa-fixed k 'fixed_add a b))
    (("_sub" a b) (ssa-fixed k 'fixed_sub a b))
    (("_lt" a b) (ssa-fixed k 'fixed_lt a b))
    (("_gt" a b) (ssa-fixed k 'fixed_gt a b))|#
    ((f . as) (ssa-apply k f as))
    (() (ssa-atom k :void))
    (else (error "invalid CPS form: ~a" xs)))

(to ssa-atom k x
  ! cond
    ((integerp x) (ssa 'fixnum k x))
    ((stringp x) (ssa-symbol k x nil))
    ((eql x :void) (ssa 'move k "Void"))
    ((eql x :empty) (ssa 'move k "Empty"))
    (t (error "unexpected ~a" x)))

(to ssa-expr k x ! if (listp x) (ssa-form k x) (ssa-atom k x))


(to shell command &rest args
  ! s = (make-string-output-stream)
  ! sb-ext:run-program command args :output s :search t :wait t
  ! get-output-stream-string s)

(defparameter *root-folder* "/Users/nikita/Documents/prj/symta/libs/symta/")

(to c-runtime-compiler dst src ! shell "gcc" "-O1" "-I" *root-folder* "-g" #|"-DNDEBUG"|# "-o" dst src)
(to c-compiler dst src ! shell "gcc" "-O1" "-I" *root-folder* "-g" #|"-DNDEBUG"|# "-fpic" "-shared" "-o" dst src)

(defparameter *exports-cache* (make-hash-table :test 'equal))

;; FIXME: do caching
(to get-lib-exports lib-name
  ! lib-folder = "{*root-folder*}lib/"
  ! lib-file = "{lib-folder}{lib-name}.s"
  ! text = load-text-file lib-file
  ! expr = /normalize (/read text)
  ! match (first (last expr))
     (("export" . xs) xs)
     (_ nil))

(to produce-ssa entry expr
  ! *ssa-out* = nil
  ! *ssa-fns* = nil
  ! *ssa-inits* = nil
  ! *ssa-raw-inits* = nil
  ! *ssa-closure* = nil
  ! ssa 'entry entry
  ! r = ssa-name "result"
  ! ssa 'var r
  ! expr = uniquify expr
  ! ssa = ssa-expr r expr
  ! ssa 'return r
  ! init-labels = nil
  ! setf *ssa-inits*
     (m x *ssa-inits*
        (! *ssa-inits* = nil
         ! *ssa-closure* = nil
         ! name = first x
         ! expr = second x
         ! l = "init_{name}"
         ! push l init-labels
         ! ssa 'label l
         ! expr = uniquify expr
         ! ssa-expr name expr
         ! ssa 'return_no_gc name
         ! ssa 'global name))
  ! ssa 'entry "setup"
  ! setf *ssa-out* (append *ssa-raw-inits* *ssa-out*)
  ! when init-labels (e l init-labels (ssa 'gosub l))
  ! ssa 'return_no_gc 0
  ! rs = apply #'concatenate 'list `(,@(reverse *ssa-fns*) ,*ssa-out*)
  ;;! rs = peephole-optimize rs
  ! nreverse rs)


(defparameter *compiled* nil)

(to to-c-emit &rest args ! (push (apply #'format nil args) *compiled*))

(defparameter *pool-size* 64)

(to ssa-to-c xs
  ! *compiled* = nil
  ! statics = nil
  ! decls = nil
  ! imports = make-hash-table :test 'equal
  ! to-c-emit "BEGIN_CODE"
  ! e x xs
       (match x
         ((''entry label-name) (to-c-emit "ENTRY(~a)" label-name))
         ((''var name) (to-c-emit "  VAR(~a);" name))
         ((''return value) (to-c-emit "  RETURN(~a);" value))
         ((''return_no_gc value) (to-c-emit "  RETURN_NO_GC(~a);" value))
         ((''label label-name)
          (push (format nil "DECL_LABEL(~a)" label-name) decls)
          (to-c-emit "LABEL(~a)" label-name)
          ;(to-c-emit "  D;")
          )
         ((''global name) (push (format nil "static void *~a;" name) decls))
         ((''local_label label-name) (to-c-emit "  LOCAL_LABEL(~a);" label-name))
         ((''branch cnd label-name) (to-c-emit "  BRANCH(~a, ~a);" cnd label-name))
         ((''zbranch cnd label-name) (to-c-emit "  ZBRANCH(~a, ~a);" cnd label-name))
         ((''jmp label-name) (to-c-emit "  JMP(~a);" label-name))
         ((''gosub label-name) (to-c-emit "  GOSUB(~a);" label-name))
         ((''branch cond label) (to-c-emit "  BRANCH(~a, ~a);" cond label))
         ((''push_base) (to-c-emit "  PUSH_BASE();"))
         ((''call k name) (to-c-emit "  CALL(~a, ~a);" k name))
         ((''call_tagged k obj method) (to-c-emit "  CALL_TAGGED(~a, ~a, ~a);" k obj method))
         ((''call_tagged_dynamic k obj) (to-c-emit "  CALL_TAGGED_DYNAMIC(~a, ~a);" k obj))
         ((''arglist place size) (to-c-emit "  ARGLIST(~a, ~a);" place size))
         ((''lift base pos value) (to-c-emit "  LIFT(~a,~a,~a);" base pos value))
         ((''get_tag dst src) (to-c-emit "  ~a = (void*)GET_TAG(~a);" dst src))
         ((''add_tag dst src tag) (to-c-emit "  ~a = ADD_TAG(~a,~a);" dst src tag))
         ((''local_closure place size) (to-c-emit "  LOCAL_CLOSURE(~a, ~a);" place size))
         ((''closure place name size)
          (push (format nil "#define ~a_size ~a" name size) decls)
          (to-c-emit "  ALLOC_CLOSURE(~a, ~a, ~a);" place name size))
         ((''data place type size) (to-c-emit "  ALLOC_DATA(~a, ~a, ~a);" place type size))
         ((''type place name tagname size)
          (let ((tname (ssa-name "n")))
            (to-c-emit "  RESOLVE_TYPE(~a, ~a);" place name)
            (to-c-emit "  VAR(~a);" tname)
            (to-c-emit "  TEXT(~a, ~a);" tname tagname)
            (to-c-emit "  SET_TYPE_SIZE_AND_NAME((intptr_t)~a, ~a, ~a);" place size tname)))
         ((''dget dst src off) (to-c-emit "  ~a = DATA_REF(~a, ~a);" dst src off))
         ((''dset dst off src) (to-c-emit "  DATA_SET(~a, ~a, ~a);" dst off src))
         ((''dinit dst off src) (to-c-emit "  DATA_REF(~a, ~a) = ~a;" dst off src))
         ((''resolve_type place name) (to-c-emit "  RESOLVE_TYPE(~a, ~a);" place name))
         ((''resolve_method place name) (to-c-emit "  RESOLVE_METHOD(~a, ~a);" place name))
         ((''dmet method type handler) (to-c-emit "  DATA_METHOD(~a, ~a, ~a);" method type handler))
         ((''import dst lib symbol lib-cstr symbol-cstr)
          (let* ((key (concatenate 'string lib "::" symbol))
                 (import (gethash key imports))
                 (lib-exports (gethash lib imports)))
            (if import
                (to-c-emit "  MOVE(~a, ~a);" dst import)
                (progn
                  (unless lib-exports
                    (setf lib-exports (ssa-name "n"))
                    (to-c-emit "  void *~a = api->load_lib(api,(char*)(~a));" lib-exports lib-cstr)
                    (setf (gethash lib imports) lib-exports))
                  (let ((symbol-text (ssa-name "s")))
                    (to-c-emit "  VAR(~a);" symbol-text)
                    (to-c-emit "  TEXT(~a, ~a);" symbol-text symbol-cstr)
                    (to-c-emit "  ~a = api->find_export(api, ~a, ~a);" dst symbol-text lib-exports))
                  (setf (gethash key imports) dst)
                  ))))
         ((''arg_load dst src off) (to-c-emit "  ARG_LOAD(~a, ~a, ~a);" dst src off))
         ((''arg_store dst off src) (to-c-emit "  ARG_STORE(~a, ~a, ~a);" dst off src))
         ((''load dst src off) (to-c-emit "  LOAD(~a, ~a, ~a);" dst src off))
         ((''store dst off src) (to-c-emit "  STORE(~a, ~a, ~a);" dst off src))
         ((''copy dst p src q) (to-c-emit "  COPY(~a, ~a, ~a, ~a);" dst p src q))
         ((''move dst src) (to-c-emit "  MOVE(~a, ~a);" dst src))
         ((''list_flip dst src) (to-c-emit "  ~a = LIST_FLIP(~a);" dst src))
         ((''fixnum dst str) (to-c-emit "  LOAD_FIXNUM(~a, ~s);" dst str))
         ((''bytes name values)
          (push (format nil '"static uint8_t ~a[] = {~{~a~^,~}};" name values) decls))
         ((''text name bytes-name) (to-c-emit "  TEXT(~a, ~a);" name bytes-name))
         ((''fatal msg) (to-c-emit "  api->fatal(api, ~a);" msg))
         ((''check_nargs expected size meta)
          (to-c-emit "  CHECK_NARGS(~a, ~a, ~a);" expected size (or meta "Empty")))
         ((''check_varargs size meta) (to-c-emit "  CHECK_VARARGS(~a, ~a);" size (or meta "Empty")))
         ((''fixed_add dst a b) (to-c-emit "  ~a = FIXNUM_ADD(~a, ~a);" dst a b))
         ((''fixed_sub dst a b) (to-c-emit "  ~a = FIXNUM_SUB(~a, ~a);" dst a b))
         ((''fixed_lt dst a b) (to-c-emit "  ~a = FIXNUM_LT(~a, ~a);" dst a b))
         ((''fixed_gt dst a b) (to-c-emit "  ~a = FIXNUM_GT(~a, ~a);" dst a b))
         (else (error "invalid ssa: ~a" x))
         )
  ! to-c-emit "END_CODE"
  ! format nil '"~{~a~%~}" (reverse (append *compiled* decls)))

(to ssa-produce-file file src
  ! ssa = produce-ssa "entry" src
  ! text = ssa-to-c ssa
  ! header = "#include \"runtime.h\""
  ! save-text-file file (format nil "~a~%~%~a" header text))



(defun var-sym? (s) (and (stringp s) (string/= s "") (upper-case-p (aref s 0))))
(defun fn-sym? (s) (and (stringp s) (not (var-sym? s))))

(defun expand-list-hole (key hole hit miss)
  (match hole
    (() (return-from expand-list-hole
          `("if" (,key "end")
                 ,hit
                 ,miss)))
        ;;(return-from expand-list-hole (expand-hole key :empty hit miss)))
    ((("@" zs)) (return-from expand-list-hole (expand-hole key zs hit miss)))
    ((("@" zs) . more) (error "@ in the middle isn't supported")))
  (let* ((h (ssa-name "X"))
         (hit (expand-list-hole key (cdr hole) hit miss)))
    `("if" (,key "end")
           ,miss
           ("_let" ((,h (,key "head"))
                    (,key (,key "tail")))
               ,(expand-hole h (car hole) hit miss)))))

(defun expand-hole (key hole hit miss)
  (unless (consp hole)
    (when (fn-sym? hole) (setf hole `("_quote" ,hole)))
    (return-from expand-hole
      (if (equal hole "_")
          hit
          (if (var-sym? hole)
              `("_let" ((,hole ,key))
                        ,hit)
              `("if" (,hole "><" ,key)
                     ,hit
                     ,miss)))))
  (when (equal (car hole) "><")
    (return-from expand-hole
       (expand-hole key (second hole) (expand-hole key (third hole) hit miss) miss)))
  (when (equal (car hole) "or")
    (return-from expand-hole
      `("if" (:void "><" ("match" ,key ,@(mapcar (lambda (x) `(,x 1)) (cdr hole))))
             ,miss
             ,hit)))
  (when (equal (car hole) "not")
    (return-from expand-hole
      `("if" (:void "><" ("match" ,key ,@(mapcar (lambda (x) `(,x 1)) (cdr hole))))
             ,hit
             ,miss)))
  (when (equal (car hole) "bind")
    (return-from expand-hole
      (let ((g (ssa-name "G")))
        `("_let" ((,g (,(second hole) ,key)))
           ,(expand-hole g (third hole) hit miss)))))
  (when (equal (car hole) "fn")
    (return-from expand-hole
      `("_let" ((,(second hole) ,key))
         ("if" ("|" ,@(cddr hole))
             ,hit
             ,miss))))
  (when (equal (car hole) "_quote")
    (return-from expand-hole
      `("if" (,(second hole) "><" ,key)
             ,hit
             ,miss)))
  (when (equal (car hole) "[]")
    (return-from expand-hole
      `("if" (("_quote" "list") "><" ("tag_of" ,key))
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
    `("_let" ((,key ,keyform)
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
  ! `("_let" ((,r 0))
       ("_set" ,r ,body)
       ("_label" ,end)
       ,r))

(to expand-leave name value
  ! (r end) = result-and-label name
  ! `("_progn" ("_set" ,r ,value)
               ("_goto" ,end)))

(to pattern-arg x ! not (stringp x))

(to expand-block-item-data name fields
  ! gs = m f fields (ssa-name "A")
  ! o = ssa-name "O"
  ! v = ssa-name "V"
  ! i = -1
  ! j = -1
  ! k = -1
  ! `(("=" (,"new_{name}" ,@gs) ("_data" ,name ,@gs))
      ,@(m f fields `("=" (("." ,name ,o) ,f) ("_dget" ,o ,(incf j))))
      ,@(m f fields `("=" (("." ,name ,o) ,"set_{f}" ,v) ("_dset" ,o ,(incf k) ,v)))))

(to expand-block-item-fn name args value
  ! kname = concatenate 'string "_k_" name
  ! default = match args
               ((("&" default) . tail)
                (setf args tail)
                default)
               (else :empty)
  ! when (find-if #'pattern-arg args)
      (match args
        ((("@" all)) (setf args all))
        (else
         (! gs = m a args (ssa-name "A")
          ;; FIXME: value gets duplicated - potentially exponential code growth
          ! e g gs (setf value (expand-match g `((,(pop args) ,value)) default))
          ! setf args gs)))
  ! setf value (expand-named name value)
  ! list name `("_fn" ,args ,value))

(to expand-block-item x
  ! y = match x
     (("data" name . fields)
      (return-from expand-block-item
        (apply #'concatenate 'list
               (m x (expand-block-item-data name fields)
                  (expand-block-item x)))))
     (("=" ("!!" ("!" place)) value)
      (match place
        (("." object field) `(nil (,object "set_{field}" ,value)))
        (else `(nil ("_set" ,place ,value)))))
     (("=" (("." type var) method . args) value)
      (list nil `("_dmet" ,method ,type ("_fn" (,var ,@args) ,value))))
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
  ! key = ssa-name "K"
  ! sel = expand-match `(,all "{}" 1) cases `("_no_method" ,key) :keyvar key
  ! `("_fn" ,all ("_apply" ,sel ,all)))

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
  ! `("let" ,(m x (remove-if-not #'car xs) `(,(first x) :void))
        ,@(m x xs (if (first x)
                      `("_set" ,(first x) ,(second x))
                      (second x)))))

(to normalize-matryoshka o
  ! match o ((x) (if (fn-sym? x)
                     o
                     (normalize-matryoshka x)))
            (x x))

(to expand-export xs ! `("_list" ,@(m x xs `("_list" ("_quote" ,x) ("&" ,x)))))


(to expand-while head body
  ! l = ssa-name "l"
  ! `("_progn" ("_label" ,l)
               ("_if" ,head
                      ("_progn" ,body ("_goto" ,l))
                      ())))

(to incut? x ! match x (("@" x) t))

(to expand-list as
  ! incut-count = count-if #'incut? as
  ! when (= 0 incut-count)  (return-from expand-list `("_list" ,@as))
  ! as = m a as (if (incut? a) (second a) `("_list" ,a))
  ! `(("_list" ,@as) "join"))

(to expand-string-splice x
  ! as = nil
  ! s = position #\{ x
  ! unless s (return-from expand-string-splice `("_quote" x))
  ! while s
      (! e = position #\} x
       ! unless e (error '"unterminated {")
       ! push `("_quote" ,(subseq x 0 s)) as
       ! push `("text" ,(/read (subseq x (+ s 1) e))) as
       ! setf x (subseq x (+ e 1) (length x))
       ! setf s (position #\{ x))
  ! when (/= 0 (length x)) (push `("_quote" ,x) as)
  ! `(("_list" ,@(reverse as)) "join_text"))

(to expand-and a b ! `("if" ,a ,b 0))
(to expand-or a b
  ! v = ssa-name "V"
  ! `("_let" ((,v ,a)) ("if" ,v ,v ,b)))

(defun builtin-expander (xs &optional (head nil))
  ;; FIXME: don't notmalize macros, because the may expand for fn syms
  (let ((xs (normalize-matryoshka xs))
        (ys nil))
    (when (atom xs)
      (return-from builtin-expander
        (if (and (fn-sym? xs) (not head))
            `("_quote" ,xs)
            xs)))
    (setf ys
      (match xs
        (("if" a b c) `("_if" ,a ,b ,c))
        (("_fn" as body)
         (return-from builtin-expander
           `("_fn" ,as ,(builtin-expander body))))
        (("_let" bs . xs)
         `("_call"
           ("_fn" ,(m b bs (first b)) ,`("_progn" ,@xs))
           ,@(m b bs (second b))))
        (("_set" place value)
         (return-from builtin-expander
           `("_set" ,place ,(if (fn-sym? value)
                                `("_quote" ,value)
                                (builtin-expander value)))))
        (("_label" name) (return-from builtin-expander xs))
        (("_goto" name) (return-from builtin-expander xs))
        (("_quote" x) (return-from builtin-expander xs))
        (("fn" as . body) `("_fn" ,as ("|" ,@body)))
        (("=>" as body) `("_fn" ,as ,body))
        (("when" . xs) `("_if" ,(butlast xs) ,@(last xs) :void))
        (("unless" . xs) `("_if" ,(butlast xs) :void ,@(last xs)))
        (("while" . xs) (expand-while (butlast xs) (car (last xs))))
        (("and" a b) (expand-and a b))
        (("or" a b) (expand-or a b))
        (("let" bs . body) `("_let" ,bs ,@body))
        (("|" . xs) (expand-block xs))
        (("[]" . as) (expand-list as))
        (("." a b) `(,a ,b))
        (("^" a b) `(,b ,a))
        ((":" a b) `(,@a ,b))
        (("\"" x) (expand-string-splice x))
        (("{}" ("." a b) . as) `(,a ,b ,@as))
        (("{}" ("^" a b) . as) `(,b ,@as ,a))
        (("{}" h . as) (if (fn-sym? h)
                           `(,h ,@as)
                           (if (> (length as) 1)
                               `(,h "{!}" ,@as)
                               `(,h "{}" ,@as))))
        (("{}" . else) (error "bad {}: ~%" xs))
        (("\\" o) (return-from builtin-expander `("_quote" ,o)))
        (("+" a b) `(,a "+" ,b))
        (("-" a) `(,a "neg"))
        (("-" a b) `(,a "-" ,b))
        (("*" a b) `(,a "*" ,b))
        (("/" a b) `(,a "/" ,b))
        (("%" a b) `(,a  "%" ,b))
        (("<" a b) `(,a "<" ,b))
        ((">" a b) `(,a  ">" ,b))
        (("<<" a b) `(,a "<<" ,b))
        ((">>" a b) `(,a  ">>" ,b))
        (("><" a b) `(,a "><" ,b))
        (("<>" a b) `(,a  "<>" ,b))
        (("&" o) (return-from builtin-expander
                   (if (fn-sym? o) o `(,(builtin-expander o)))))
        (("and" a b) `("if" ,a ,b 0))
        (("or" a b) (let ((n (ssa-name "T")))
                      `("let" ((,n ,a)) ("if" ,n ,n ,b))))
        (("named" name . body) (expand-named name `("_progn" ,@body)))
        (("leave" name value) (expand-leave name value))
        (("!!" . as)
         (let* ((ys (copy-list as))
                (v nil)
                (p (position-if (fn x ! match x (("!" x) (setf v x) t)) as)))
           (if p
               (setf (nth p ys) v)
               (error "!!: no ! in ~a" as))
           `("_set" ,v ,ys)))
        (("match" keyform . cases) (expand-match keyform cases :empty))
        (("export" . xs) (expand-export xs))
        (else (return-from builtin-expander
                (cons (builtin-expander (car xs) t)
                      (m x (cdr xs) (builtin-expander x)))))))
    (builtin-expander ys)))

(to compile-runtime src-file dst-file
  ! result = c-runtime-compiler dst-file src-file
  ! when (string/= result "")
      (e l (split #\Newline result) (format t "~a~%" l)))

(to add-imports expr deps
  ! unless deps (return-from add-imports expr)
  ! `(("_fn" ,(m d deps (second d)) ,expr)
      ,@(m d deps `("_import" ("_quote" ,(first d))
                              ("_quote" ,(second d))))))

(to symta-compile-expr dst expr
  ! uses = list "core"
  ! expr = match expr
             (("|" ("use" . us) . xs)
              (setf uses `(,@uses ,@us))
              `("|" ,@xs))
             (else expr)
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
  )

(to symta-read-file filename
  ! text = load-text-file filename
  ! (/init-tokenizer)
  ! /normalize (/read text)
  )

(to compile-lib name
  ! filename = "{*root-folder*}lib/{name}.s"
  ! dst = "{*root-folder*}native/lib/{name}"
  ! expr = symta-read-file filename
  ! symta-compile-expr dst expr
  )

(to symta filename
  ! compile-lib "prelude"
  ! native-folder = "{*root-folder*}native/"
  ! runtime-src = "{*root-folder*}runtime.c"
  ! runtime-path = "{native-folder}runtime"
  ! compile-runtime runtime-src runtime-path
  ! lib-path = "{native-folder}lib/"
  ! bin-file = "{native-folder}test"
  ! expr = symta-read-file filename
  ! symta-compile-expr bin-file expr
  ! result = shell runtime-path lib-path bin-file
  ! e l (butlast (split #\Newline result)) (format t "~a~%" l)
  )
