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
  ! head-char = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_?<>"
  ! tail-char = "{head-char}{digit}"
  ! ls = `(".." "+" "-" "*" "/" "%" "^" "." "->" "~" "|" ";" "," ":" "=" "=>"
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

(to /logic
  ! o = try (/op '(:and :or)) (/dots)
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
(to /read xs ! /read-toplevel xs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; COMPILER
(defparameter *ssa-env* nil)
(defparameter *ssa-out* nil) ; where resulting assembly code is stored
(defparameter *ssa-ns*  nil) ; unique name of current function
(defparameter *ssa-fns* nil)

(defparameter *ssa-closure* nil) ; other lambdas', this lambda references
(defparameter *ssa-inits* nil)


(defparameter *compiler-meta-info* (make-hash-table :test 'eq))

(to set-meta meta object
  ! (setf (gethash object *compiler-meta-info*) meta)
  ! object)

(to get-meta object ! gethash object *compiler-meta-info*)


(defun ssa-name (name) (symbol-name (gensym name)))

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

(to ssa-symbol x value
  ! match (ssa-path-to-sym x *ssa-env*)
     ((pos parent)
      (! base = if parent 'r 'e
           #|(if parent
               (ssa 'store 'p parent value)
               (ssa 'copy 'e value))
           (ret nil)|#
       ! when parent (ssa 'load 'r 'p parent) ; symbol resides in parent environment
       ! when (eql pos :all)
          (when value (error "can't set ~a" x))
          (unless (eql base 'r) (ssa 'move 'r base))
          (ssa 'list_flip 'r 'r)
          (ret nil)
       ! if value
            (ssa 'store base pos value)
            (ssa 'load 'r base pos)))
     (else (error "undefined variable: ~a" x)))

(to ssa-atom x
  ! cond
    ((integerp x) (ssa 'fixnum 'r x))
    ((stringp x) (ssa-symbol x nil))
    ((eql x 'run) (ssa 'move 'r "run"))
    ((eql x :void) (ssa 'move 'r "Void"))
    ((eql x :empty) (ssa 'move 'r "Empty"))
    (t (error "unexpected ~a" x)))

(to ssa-quote-list-rec xs
   ! `("list" ,@(m x xs (if (listp x) (ssa-quote-list-rec x) `("_quote" ,x)))))

(to ssa-quote-list xs
  ! name = ssa-name "list"
  ! ssa 'move 'r name
  ! push `(,name ,(ssa-quote-list-rec xs)) *ssa-inits*)

(to ssa-quoted-symbol s
  ! name = ssa-name "s"
  ! ssa 'text name s
  ! ssa 'move 'r name)

(to ssa-quote x
  ! cond
     ((stringp x) (ssa-quoted-symbol x))
     ((integerp x) (ssa-atom x))
     ((listp x) (ssa-quote-list x))
     (t (error "unsupported quoted value: ~a" x)))

(to ssa-resolved name ! cons name *ssa-ns*)

(to ssa-fn args body o
  ! f = ssa-name "f"
  ! cs = nil
  ! (! *ssa-out* = nil
     ! *ssa-ns* = f
     ! *ssa-env* = if (stringp args)
                      (cons `(:all ,(ssa-resolved args)) *ssa-env*)
                      (cons (mapcar #'ssa-resolved args) *ssa-env*)
     ! *ssa-closure* = cons nil *ssa-closure*
     ! ssa 'label *ssa-ns*
     ! size-var = format nil "~a_size" f
     ! if (stringp args)
          (ssa 'check_varargs size-var (get-meta o))
          (ssa 'check_nargs (length args) size-var (get-meta o))
     ! produce-ssa body
     ! push *ssa-out* *ssa-fns*
     ! setf cs (car *ssa-closure*)
     )
  ! nparents = length cs
  ;; check if we really need new closure here, because in some cases we can reuse parent's closure
  ;; a single argument to a function could be passed in register, while a closure would be created if required
  ;; a single reference closure could be itself held in a register
  ;; for now we just capture required parent's closure
  ! ssa 'closure 'r f nparents
  ! i = -1
  ! e c cs (! if (equal c *ssa-ns*) ; self?
                 (ssa 'store 'r (incf i) 'e)
                 (ssa 'copy 'r (incf i) 'p (ssa-get-parent-index c)))
  ! ssa 'known_closure)

(to ssa-if as
  ! ssa 'array 'a 1
  ! label = ssa-name "branch"
  ! produce-ssa (first as)
  ! ssa 'store 'a 0 'r
  ! produce-ssa (second as)
  ! ssa 'branch 'r label
  ! produce-ssa (fourth as)
  ! ssa 'move 'c 'r
  ! ssa 'move 'e 'a
  ! ssa 'call 'c
  ! ssa 'label label
  ! produce-ssa (third as)
  ! ssa 'move 'c 'r
  ! ssa 'move 'e 'a
  ! ssa 'call 'c)

(to ssa-apply f as
  ;; FIXME: if it is a lambda call, we don't have to change env or create a closure, just push env
  ! when (equal f "_call")
     (setf f (second as))
     (setf as (cddr as))
  ! when (eql f :if)
     (ssa-if as)
     (return-from ssa-apply)
  ! produce-ssa f
  ! known-closure = eql (first (car *ssa-out*)) 'known_closure
  ! ssa 'move 'c 'r
  ! ssa 'array 'a (length as)
  ! i = -1
  ! e a as (! produce-ssa a
            ! ssa 'store 'a (incf i) 'r)
  ! ssa 'move 'e 'a ; replace current frame with new environment
  ! if known-closure (ssa 'call 'c) (ssa 'call_tagged 'c))

(to ssa-set k place value
  ! produce-ssa value
  ! ssa 'move 'a 'r
  ! ssa-symbol place 'a
  ! produce-ssa `(,k ,value))

(to ssa-form xs
  ! match xs
    (("_fn" as body) (ssa-fn as body xs))
    (("_quote" x) (ssa-quote x))
    (("_set" k place value) (ssa-set k place value))
    (("_move" dst src) (ssa 'move dst src))
    ((f . as) (ssa-apply f as))
    (else (error "invalid CPS form: ~a" xs)))

(to produce-ssa x ! if (listp x) (ssa-form x) (ssa-atom x))

(to peephole-optimize xs
  ! match xs
    (((''move a ''r) (''move ''r c) . zs) `((move ,a ,c) ,@(peephole-optimize zs)))
    (((''move a ''r) (''load ''r c d) . zs) `((load ,a ,c ,d) ,@(peephole-optimize zs)))
    (((''store a b c) (''move c d) . zs) `((store ,a ,b ,d) ,@(peephole-optimize zs)))
    (((''store a b ''r) (''load ''r d e) . zs) `((copy ,a ,b ,d ,e) ,@(peephole-optimize zs)))
    (((''move a b) (''store b c d) (''known_closure) (''closure d x y) (''array b e) . zs)
     `((store ,a ,c ,d) (closure ,d ,x ,y) (array ,a ,e) ,@(peephole-optimize zs)))
    (((''move a b) (''known_closure) (''store b c d) (''closure b e f) . zs)
     `((store ,a ,c ,d) (closure ,a ,e ,f) ,@(peephole-optimize zs)))
    ((z . zs) (cons z (peephole-optimize zs)))
    (nil nil))

(to host-deps expr deps ! `("_fn" ("host")
                                  ("host" ("_fn" ,deps ,expr)
                                          ,@(m d deps `("_quote" ,d)))))

(to cps-to-ssa x
  ! *ssa-out* = nil
  ! *ssa-fns* = nil
  ! produce-ssa x
  ! setf *ssa-inits*
     (m x *ssa-inits*
        (! name = first x
         ! expr = host-deps (second x) '("list")
         ! list name (ssa-compile-entry "run" "init_{name}" expr)))
  ! rs = apply #'concatenate 'list  `(,@(reverse *ssa-fns*) ,*ssa-out*)
  ! rs = peephole-optimize rs
  ! nreverse rs)

(to find-duplicates xs ys zs
  ! unless xs (return-from find-duplicates ys)
  ! x = car xs
  ! xs = cdr xs
  ! if (find x zs :test 'equal)
       (find-duplicates xs (cons x ys) zs)
       (find-duplicates xs ys (cons x zs)))

(to cps-fn-nargs kk args body
  ! args = `(,kk ,@args)
  ! ds = find-duplicates args nil nil
  ! when ds (error "duplicate symbols in arglist: ~a" args)
  ! `("_fn" ,args ,(produce-cps kk body)))

(to cps-fn-varargs kk args body
  ! m = ssa-name "m"
  ! `("_fn" ,args
       (,args ("_fn" (,m) (("_fn" (,kk) ,(produce-cps kk body)) ,m))
              ("_quote" "{}")
              0)))

(to cps-fn kk k args body o
  ! `(,k ,(set-meta (get-meta o)
                    (if (stringp args)
                        (cps-fn-varargs kk args body)
                        (cps-fn-nargs kk args body)))))

(to cps-const? x ! or (not (listp x)) (equal (first x) "_quote"))

(to cps-apply k f as o
  ! fas = `(,f ,@as)
  ! (g . gs) = m a fas (if (cps-const? a) a (ssa-name "a"))
  ! r = `(,g ,k ,@gs)
  ! rgs = reverse `(,g ,@gs)
  ! ras = reverse fas
  ! while rgs
      ;; treat quoted and _fn values as constants
      (unless (cps-const? (car ras))
        (setf r (produce-cps (set-meta (get-meta o) `("_fn" (,(car rgs)) ,r)) (car ras))))
      (pop rgs)
      (pop ras)
  ! r)

(to cps-set k place value o
  ! unless (stringp place) (error "_set cant handle `{place}`")
  ! unless (listp value) (ret `("_set" ,k ,place ,value))
  ! v = ssa-name "value"
  ! r = produce-cps (set-meta (get-meta o) `("_fn" (,v) ("_set" ,k ,place ,v))) value
  ! r)

(to cps-form k xs
  ! match xs
    (("_fn" as body) (cps-fn (ssa-name "k") k as body xs))
    (("_kfn" kk as body) (cps-fn kk k as body xs))
    (("_if" cnd then else) (cps-form k `(:if ,cnd ("_fn" () ,then) ("_fn" () ,else))))
    (("_quote" x) `(,k ,xs))
    (("_set" place value) (cps-set k place value xs))
    ((f . as) (cps-apply k f as xs))
    (else `(,k :void)))

(to cps-atom k x ! `(,k ,x))

(to produce-cps k x ! if (listp x) (cps-form k x) (cps-atom k x))

(defparameter *compiled* nil)

(to to-c-emit &rest args ! (push (apply #'format nil args) *compiled*))

(defparameter *pool-size* 64)

(defun ssa-to-c (entry xs)
  (let ((*compiled* nil)
        (statics nil)
        (decls nil)
        (inits nil)
        (data-name (ssa-name "data"))
        (inits-name (ssa-name "inits"))
        (data nil)
        )
    (e x *ssa-inits* (to-c-emit "static void *~a;" (first x)))
    (e x *ssa-inits* (to-c-emit "~a" (second x)))
    (to-c-emit "DECL_LABEL(~a)" inits-name)
    (if (equal entry "entry")
        (to-c-emit "ENTRY(~a)" entry)
        (to-c-emit "static ENTRY(~a)" entry))
    (to-c-emit "  BRANCH(R, ~a);" inits-name)
    (e x xs
       (match x
         ((''label label-name)
          (push (format nil "DECL_LABEL(~a)" label-name) decls)
          (to-c-emit "LABEL(~a)" label-name)
          ;(to-c-emit "  D;")
          )
         ((''branch cond label) (to-c-emit "  BRANCH(~a, ~a);" cond label))
         ((''call name) (to-c-emit "  CALL(~a);" name))
         ((''call_tagged name) (to-c-emit "  CALL_TAGGED(~a);" name))
         ((''array place size) (to-c-emit "  LIST(~a, ~a);" place size))
         ((''closure place name size)
          (progn
            (push (format nil "#define ~a_size ~a" name size) decls)
            ;;(push (format nil "static int ~a_pool;" name) decls)
            ;;(push (format nil "NEW_POOL(~a_pool);" name) inits)
            (to-c-emit "  ALLOC(~a, ~a, ~a);" place name size)))
         ((''load dst src off) (to-c-emit "  LOAD(~a, ~a, ~a);" dst src off))
         ((''store dst off src) (to-c-emit "  STORE(~a, ~a, ~a);" dst off src))
         ((''copy dst p src q) (to-c-emit "  COPY(~a, ~a, ~a, ~a);" dst p src q))
         ((''move dst src) (to-c-emit "  MOVE(~a, ~a);" dst src))
         ((''list_flip dst src) (to-c-emit "  ~a = LIST_FLIP(~a);" dst src))
         ((''known_closure) (to-c-emit "  /* known closure */"))
         ((''fixnum dst str) (to-c-emit "  LOAD_FIXNUM(~a, ~s);" dst str))
         ((''text name str)
          (let ((bytes `(,@(m c (coerce str 'list) (char-code c)) 0)))
            (push (format nil "static uint8_t ~a_bytes[] = {~{~a~^,~}};" name bytes) decls)
            (push (format nil "static void *~a;" name) decls)
            (push (format nil "TEXT(~a, ~a_bytes);" name name) inits)))
         ((''list dst xs)
          (let ((name (ssa-name "s")))
            (to-c-emit "  MOVE(~a, ~a);" dst name))
          (abort))
         ((''check_nargs expected size meta)
          (to-c-emit "  CHECK_NARGS(~a, ~a, ~a);" expected size (or meta "Empty")))
         ((''check_varargs size meta) (to-c-emit "  CHECK_VARARGS(~a_size, ~a);" size (or meta "Empty")))
         (else (error "invalid ssa: ~a" x))))
    (to-c-emit "LABEL(~a)" inits-name)
    (when (or *ssa-inits* inits)
      (e i inits (to-c-emit "  ~a" i))
      (e x *ssa-inits*
         (progn
           (to-c-emit "  JMP(~a);" (first x))
           (to-c-emit "  LOAD(~a, E, 0);" (first x)))))
    (to-c-emit "END_OF_CODE~%")
    (format nil "~{~a~%~}" (reverse (append *compiled* decls)))))

(to ssa-compile k entry fn-expr
  ! *ssa-inits* = nil
  ! cps = produce-cps k fn-expr
  ! ssa = cps-to-ssa cps
  ! ssa-to-c entry ssa)

(to ssa-compile-entry k entry expr ! ssa-compile `("_move" r ,k) entry expr)

(to ssa-produce-file file src
  ! text = ssa-compile-entry "run" "entry" src
  ! header = "#include \"../runtime.h\""
  ! save-text-file file (format nil "~a~%~%~a" header text))

(defparameter *native-files-folder* "/Users/nikita/Documents/prj/symta/libs/symta/native/")

(to shell command &rest args
  ! s = (make-string-output-stream)
  ! sb-ext:run-program command args :output s :search t :wait t
  ! get-output-stream-string s)

(to c-runtime-compiler dst src ! shell "gcc" "-O1" "-g" #|"-DNDEBUG"|# "-o" dst src)
(to c-compiler dst src ! shell "gcc" "-O1" "-g" #|"-DNDEBUG"|# "-fpic" "-shared" "-o" dst src)

(to compile-runtime main-file
  ! src-file = "{*native-files-folder*}../runtime.c"
  ! result = c-runtime-compiler main-file src-file
  ! when (string/= result "")
      (e l (split #\Newline result) (format t "~a~%" l)))

(to test-ssa src
  ! main-file = "{*native-files-folder*}/runtime"
  ! compile-runtime main-file
  ! c-file = "{*native-files-folder*}test.c"
  ! exe-file = "{c-file}.bin"
  ! ssa-produce-file c-file src
  ! result = c-compiler exe-file c-file
  ! when (string/= result "")
     (e l (split #\Newline result) (format t "~a~%" l))
  ! result = shell main-file exe-file
  ! e l (butlast (split #\Newline result)) (format t "~a~%" l)
  )

(defun var-sym? (s) (and (stringp s) (string/= s "") (upper-case-p (aref s 0))))
(defun fn-sym? (s) (and (stringp s) (not (var-sym? s))))

(defun expand-list-hole (key hole hit miss)
  (match hole
    (() (return-from expand-list-hole (expand-hole key :empty hit miss)))
    ((("@" zs)) (return-from expand-list-hole (expand-hole key zs hit miss)))
    ((("@" zs) . more) (error "@ in the middle isn't supported")))
     (let* ((h (ssa-name "X"))
            (hit (expand-list-hole key (cdr hole) hit miss)))
    `("if" (,key "end")
           ,miss
           ("let" ((,h (,key "head"))
                   (,key (,key "tail")))
             ,(expand-hole h (car hole) hit miss)))))

(defun expand-hole (key hole hit miss)
  (unless (consp hole)
    (when (fn-sym? hole) (setf hole `("_quote" ,hole)))
    (return-from expand-hole
      (if (equal hole "_")
          hit
          (if (var-sym? hole)
              `("let" ((,hole ,key))
                      ,hit)
              `("if" (,hole "is" ,key)
                     ,hit
                     ,miss)))))
  (when (equal (car hole) "is")
    (return-from expand-hole
       (expand-hole key (second hole) (expand-hole key (third hole) hit miss) miss)))
  (when (equal (car hole) "or")
    (return-from expand-hole
      `("if" (:void "is" ("match" ,key ,@(mapcar (lambda (x) `(,x 1)) (cdr hole))))
             ,miss
             ,hit)))
  (when (equal (car hole) "not")
    (return-from expand-hole
      `("if" (:void "is" ("match" ,key ,@(mapcar (lambda (x) `(,x 1)) (cdr hole))))
             ,hit
             ,miss)))
  (when (equal (car hole) "bind")
    (return-from expand-hole
      (let ((g (ssa-name "G")))
        `("let" ((,g (,(second hole) ,key)))
           ,(expand-hole g (third hole) hit miss)))))
  (when (equal (car hole) "fn")
    (return-from expand-hole
      `("let" ((,(second hole) ,key))
         ("if" ("|" ,@(cddr hole))
             ,hit
             ,miss))))
  (when (equal (car hole) "_quote")
    (return-from expand-hole
      `("if" (,(second hole) "is" ,key)
             ,hit
             ,miss)))
  (when (equal (car hole) "[]")
    (return-from expand-hole
      `("if" (("_quote" "list") "is" ("tag_of" ,key))
             ,miss
             ,(expand-list-hole key (cdr hole) hit miss))))
  (error "bad hole: ~a" hole))

(defun expand-match (keyform cases default &key (keyvar nil))
  (let* ((key (or keyvar (ssa-name "Key")))
         (b (ssa-name "B"))
         (ys (reduce (lambda (next case)
                       (let* ((name (ssa-name "c"))
                              (miss (if next (second (first next)) `("_call" ,b ,default)))
                              (hit `("_call" ,b ("|" ,@(cdr case)))))
                         `(("=" (,name) ,(expand-hole key (car case) hit miss) )
                           ,@next)))
                     (cons nil (reverse cases)))))
    `(("_kfn" ,b (,key) ("|" ,@ys ,(second (first ys))))
      ,keyform)))

(to lambda-sequence xs
  ! unless (cdr xs) (return-from lambda-sequence (car xs))
  ! dummy = ssa-name "A"
  ! `(("_fn" (,dummy) ,(lambda-sequence (cdr xs))) ,(car xs)))

(to pattern-arg x ! not (stringp x))

(to expand-block-item x
  ! match x
     (("=" ("!!" ("!" name)) value) `(nil ("_set" ,name ,value)))
     (("=" (name . args) value)
      (if (var-sym? name)
          (list name value) ;; FIXME: check that args are empty
          (let ((kname (concatenate 'string "_k_" name))
                (default (match args
                           ((("&" default) . tail)
                            (setf args tail)
                            default)
                           (else :empty))))
            (when (find-if #'pattern-arg args)
              (let ((gs (m a args (ssa-name "A"))))
                ;; FIXME: value gets duplicated - potentially exponential code growth
                (e g gs (setf value (expand-match g `((,(pop args) ,value)) default)))
                (setf args gs)))
            (list name `("_kfn" ,kname ,args ,value)))))
     (else (list nil x)))

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
  ! `("_fn" ,all ("_apply" ,sel ,all))
  )

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
  ! `("let" ,(m x (remove-if-not #'car xs) `(,(first x) :void))
        ,@(m x xs (if (first x)
                      `("_set" ,(first x) ,(second x))
                      (second x)))))

(to normalize-ampersand x ! match x (("&" y) y) (else x))

(to normalize-symbol s
  ! unless (stringp s) (return-from normalize-symbol s)
  ! if (var-sym? s) s `("_quote" ,s))

(to normalize-matryoshka o
  ! match o ((x) (if (fn-sym? x)
                     o
                     (normalize-matryoshka x)))
            (x x))

(defun builtin-expander (xs)
  ;; FIXME: don't notmalize macros, because the may expand for fn syms
  (let ((xs (normalize-matryoshka xs))
        (ys nil))
    (when (atom xs) (return-from builtin-expander xs))
    (setf ys
      (match xs
        (("if" a b c) `("_if" ,a ,b ,c))
        (("_fn" as body)
         (return-from builtin-expander
           `("_fn" ,as ,(builtin-expander body))))
        (("_kfn" all as body)
         (return-from builtin-expander
           `("_kfn" ,all ,as ,(builtin-expander body))))
        (("fn" as . body) `("_fn" ,as ("|" ,@body)))
        (("=>" as body) `("_fn" ,as ,body))
        (("set" dst src) `("_set" ,dst ,src))
        (("when" a body) `("_if" ,a ,body :void))
        (("unless" a body) `("_if" ,a :void ,body))
        (("let" bs . body)
         (let ((body (if (= (length body) 1)
                         (car body)
                         (lambda-sequence body))))
           (if bs
               `(("_fn" ,(m b bs (first b)) ,body) ,@(m b bs (second b)))
               body)))
        (("|" . xs) (expand-block xs))
        (("[]" . as) `("list" ,@as))
        (("." a b) `(,a ,b))
        (("^" a b) `(,b ,a))
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
        (("is" a b) `(,a "is" ,b))
        (("isnt" a b) `(,a  "isnt" ,b))
        (("&" o) (return-from builtin-expander
                   (if (fn-sym? o)
                       `("&" ,o)
                       `(,(builtin-expander o)))))
        (("and" a b) `("if" ,a ,b 0))
        (("or" a b) (let ((n (ssa-name "T")))
                      `("let" ((,n ,a)) ("if" ,n ,n ,b))))
        (("leave" from value)
         (let ((kname (concatenate 'string "_k_" from)))
           `("_call" ,kname ,value)))
        (("!!" . as)
         (let* ((ys (copy-list as))
                (v nil)
                (p (position-if (fn x ! match x (("!" x) (setf v x) t)) as)))
           (if p
               (setf (nth p ys) v)
               (error "!!: no ! in ~a" as))
           `("_set" ,v ,ys)))
        (("match" keyform . cases) (expand-match keyform cases :empty))
    (else (return-from builtin-expander
            (let ((ys (m x xs (builtin-expander x))))
              (if (and (consp ys)
                       (or (equal (car ys) "_quote")
                           (equal (car ys) "_kfn")
                           (equal (car ys) "_fn")
                           (equal (car ys) "_set")))
                  ys
                  (m x (cons (car ys) (m y (cdr ys) (normalize-symbol y)))
                     (normalize-ampersand x))))))))
    (builtin-expander ys)))
      
(to symta-eval text
  ! (/init-tokenizer)
  ! expr = /read text
  ! deps = list "tag_of" "halt" "log" "list" "_apply" "_no_method" "read_file_as_text"
  ! normalized-expr = match expr (("|" . as) expr)
                                  (x `("|" ,x))
  ! expr-with-deps = host-deps normalized-expr deps
  ! expanded-expr = builtin-expander expr-with-deps
  ! test-ssa expanded-expr)

(to symta filename
  ! text = load-text-file filename
  ! symta-eval text)



;;(test-ssa '("list" 1 2 3 4 5))
