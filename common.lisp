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

(in-package :symta)


(defun list-directory (dirname)
  "Return a list of the contents of the directory named by dirname.
Names of subdirectories will be returned in `directory normal
form'. Unlike CL:DIRECTORY, LIST-DIRECTORY does not accept
wildcard pathnames; `dirname' should simply be a pathname that
names a directory. It can be in either file or directory form."
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))

  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    ;; SBCL, CMUCL, and Lispworks return subdirectories in directory
    ;; form just the way we want.
    (directory wildcard)
    
    #+openmcl
    ;; OpenMCl by default doesn't return subdirectories at all. But
    ;; when prodded to do so with the special argument :directories,
    ;; it returns them in directory form.
    (directory wildcard :directories t)
            
    #+allegro
    ;; Allegro normally return directories in file form but we can
    ;; change that with the :directories-are-files argument.
    (directory wildcard :directories-are-files nil)
            
    #+clisp
    ;; CLISP has a particularly idiosyncratic view of things. But we
    ;; can bludgeon even it into doing what we want.
    (nconc 
     ;; CLISP won't list files without an extension when :type is
     ;; wild so we make a special wildcard for it.
     (directory wildcard)
     ;; And CLISP doesn't consider subdirectories to match unless
     ;; there is a :wild in the directory component.
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))




(defun file-exists-p (pathname)
  "Similar to CL:PROBE-FILE except it always returns directory names
in `directory normal form'. Returns truename which will be in
`directory form' if file named is, in fact, a directory."

  #+(or sbcl lispworks openmcl)
  ;; These implementations do "The Right Thing" as far as we are
  ;; concerned. They return a truename of the file or directory if it
  ;; exists and the truename of a directory is in directory normal
  ;; form.
  (probe-file pathname)

  #+(or allegro cmu)
  ;; These implementations accept the name of a directory in either
  ;; form and return the name in the form given. However the name of a
  ;; file must be given in file form. So we try first with a directory
  ;; name which will return NIL if either the file doesn't exist at
  ;; all or exists and is not a directory. Then we try with a file
  ;; form name.
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  ;; Once again CLISP takes a particularly unforgiving approach,
  ;; signalling ERRORs at the slightest provocation.

  ;; pathname in file form and actually a file      -- (probe-file file)      ==> truename
  ;; pathname in file form and doesn't exist        -- (probe-file file)      ==> NIL
  ;; pathname in dir form and actually a directory  -- (probe-directory file) ==> truename
  ;; pathname in dir form and doesn't exist         -- (probe-directory file) ==> NIL

  ;; pathname in file form and actually a directory -- (probe-file file)      ==> ERROR
  ;; pathname in dir form and actually a file       -- (probe-directory file) ==> ERROR
  (or (ignore-errors
        ;; PROBE-FILE will return the truename if file exists and is a
        ;; file or NIL if it doesn't exist at all. If it exists but is
        ;; a directory PROBE-FILE will signal an error which we
        ;; ignore.
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        ;; PROBE-DIRECTORY returns T if the file exists and is a
        ;; directory or NIL if it doesn't exist at all. If it exists
        ;; but is a file, PROBE-DIRECTORY will signal an error.
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))


    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented"))

(defun directory-wildcard (dirname)
  (make-pathname 
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))


(defun directory-pathname-p (p)
  "Is the given pathname the name of a directory? This function can
usefully be used to test whether a name returned by LIST-DIRECTORIES
or passed to the function in WALK-DIRECTORY is the name of a directory
in the file system since they always return names in `directory normal
form'."
  (flet ((component-present-p (value)
           (and value (not (eql value :unspecific)))))
    (and 
     (not (component-present-p (pathname-name p)))
     (not (component-present-p (pathname-type p)))
     p)))

(defun file-pathname-p (p)
  (unless (directory-pathname-p p) p))

(defun pathname-as-directory (name)
  "Return a pathname reperesenting the given pathname in
`directory normal form', i.e. with all the name elements in the
directory component and NIL in the name and type components. Can
not be used on wild pathnames because there's not portable way to
convert wildcards in the name and type into a single directory
component. Returns its argument if name and type are both nil or
:unspecific."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname 
       :directory (append (or (pathname-directory pathname) (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname)
      pathname)))

(defun pathname-as-file (name)
  "Return a pathname reperesenting the given pathname in `file form',
i.e. with the name elements in the name and type component. Can't
convert wild pathnames because of problems mapping wild directory
component into name and type components. Returns its argument if
it is already in file form."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
      (let* ((directory (pathname-directory pathname))
             (name-and-type (pathname (first (last directory)))))
        (make-pathname 
         :directory (butlast directory)
         :name (pathname-name name-and-type)
         :type (pathname-type name-and-type)
         :defaults pathname))
      pathname)))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  "Walk a directory invoking `fn' on each pathname found. If `test' is
supplied fn is invoked only on pathnames for which `test' returns
true. If `directories' is t invokes `test' and `fn' on directory
pathnames as well."
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))


(defun folder-p (name)
  "Is `name' the name of an existing directory."
  (let ((truename (file-exists-p name)))
    (and truename (directory-pathname-p name))))

#|
(defun file-p (name)
  "Is `name' the name of an existing file, i.e. not a directory."
  (let ((truename (file-exists-p name)))
    (and truename (file-pathname-p name))))
|#

(defun pwd () (namestring (truename ".")))
(defun cd (path)
  (unless (case (aref path 0) (#\/ t) (#\\ t))
    (setf path (concatenate 'string (pwd) path)))
  (let ((dst (pathname-as-directory (pathname path))))
    (when (folder-p (namestring dst))
      (setf *default-pathname-defaults* dst) ;;change where SBCL loads files
      t)))
(defun ls (&optional path)
  (unless path (setf path "."))
  (unless (case (aref path 0) (#\/ t) (#\\ t))
    (setf path (concatenate 'string (pwd) path)))
  (mapcar #'namestring (list-directory path)))

(defun load-file (path)
  (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun save-file (path bytes)
  (with-open-file (stream path
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence bytes stream))
  nil)

(defun load-text-file (file)
  (with-output-to-string (out)
    (with-open-file (in file :direction :input)
      (loop with l while (setf l (read-line in nil nil))
	   do (progn (fresh-line out)
		     (format out "~a" l))))))


#|
(defun open-temporary-file ()
  (or
  ;;#+allegro (make-temp-file-name)
  ;;#+clisp (mkstemp)
   (loop thereis (open (format nil "TEMP-~D" (random 100000))
                       :direction :probe :if-exists nil
                       :if-does-not-exist :create))))
|#




(defmacro muffle-compiler-note (&body body)
  `(locally
       #+SBCL (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       ,@body))

(defmacro muffle-compiler-style-warning (&body body)
  `(locally
       #+SBCL (declare (sb-ext:muffle-conditions sb-ext::style-warning))
       ,@body))

(defmacro muffle-compiler-warning (&body body)
  `(locally
       #+SBCL (declare (sb-ext:muffle-conditions sb-ext::warning))
       ,@body))

(defun exit ()
  #+sbcl (sb-ext:quit)
  #+clisp (ext:exit)
  #+ccl (ccl:quit)
  #+allegro (excl:exit))

(defmacro stack-trace ()
  (or #+SBCL `(caaddr (sb-debug::backtrace-as-list))
      ;#+ACL (excl::external-fn_symdef)
      ''unnamed))


#|
;; portable way
(defmacro typed-closure (type args &body body)
  (unless (stringp type) (error "invalid type-string ~a" type))
  (unless body (setf body '(nil)))
  `(lambda ,args ,type ,@body))
(defun get-closure-type (o)
  (documentation o 'function))

;;another portable way
(defparameter *closure-types* (make-hash-table))
(defmacro typed-closure (type args &body body)
  (unless (stringp type) (error "invalid type-string ~a" type))
  (let ((g (gensym)))
    `(let ((,g (lambda ,args ,@body)))
       (setf (gethash ,g *closure-types*) ,type)
       ,g)))
(defun get-closure-type (o)
  (gethash o *closure-types*))

;; half-portable way
(defmacro typed-closure (type args &body body)
  (unless (stringp type) (error "invalid type-string ~a" type))
  (let ((type (intern type "KEYWORD")))
    `(labels ((,type ,args ,@body)) #',type)))
(defun get-closure-type (o)
  (multiple-value-bind (x y type)
      (function-lambda-expression o)
    (declare (ignorable x) (ignorable y))
    (and (consp type)
         (keywordp (second type))
         (symbol-name (second type)))))
|#

;; SBCL-way
(defmacro typed-closure (type args &body body)
  (unless (stringp type) (error "invalid type-string ~a" type))
  `(sb-int:named-lambda ,type ,args ,@body))
(defun get-closure-type (o)
  (multiple-value-bind (x y type)
      (function-lambda-expression o)
    (declare (ignorable x) (ignorable y))
    (when (stringp type) type)))


#+SBCL (defmacro named-fn (name args &body body)
         `(SB-INT:NAMED-LAMBDA ,name ,args ,@body))
#-SBCL (defmacro named-fn (name args &body body)
         `(lambda ,args ,@body))

;;(defmacro named-fn (name args &body body)
;;  `(labels ((,name ,args ,@body)) #',name))



;;(defmacro lexically-bound-p (symbol)
;;  `(handler-case (or ,symbol t)
;;     (unbound-variable (c)
;;       (declare (ignorable c))
;;       nil)))
;;(defun get-env-vars (env) (mapcar #'car (sb-c::lexenv-vars env)))
;;(defun env-boundp (sym env) (find sym (get-env-vars env)))
;;(eql (type-of (sb-kernel:make-null-lexenv)) env)
;;(defmacro lexixally-bound-p (sym &environment env)
;;  `(identity ',(find sym (mapcar #'car (sb-c::lexenv-vars env)))))
;;(defmacro env-vars (&environment extern-env)
;;  (get-env-vars extern-env))

;;(defmacro lexically-bound-p (variable &environment env)
;;  (eq :lexical (sb-cltl2:variable-information variable env)))
;;(defmacro symbol-macro-bound-p (variable &environment env)
;;  (eq :symbol-macro (sb-cltl2:variable-information variable env)))

(defun lexically-bound-p (symbol env)
  "Return T if the symbol is bound in lexical environment env"
  ;;#+sbcl (sb-walker:var-lexical-p symbol env)
  #+sbcl (eq :lexical (sb-cltl2:variable-information symbol env))
  #-(or sbcl) (error 'not-implemented :proc (list 'lexically-bound-p symbol env)))


;; On 32-bit machines lower three bits of address word usually contain type tag.
;; Example:
;; 000    even-fixnum
;; 001    defstruct-instance-pointer
;; 010    character
;; 011    cons-cell (and NIL)
;; 100    odd-fixnum
;; 101    function-pointer
;; 110    other-immediate-1
;; 111    other-pointer
(defmacro object-address (object)
  (let ((x (gensym)))
    `(let ((,x ,object))
       (muffle-compiler-note
         (locally (declare (optimize (safety 0) (speed 3)))
           (sb-kernel:get-lisp-obj-address ,x))))))

(defmacro with-gensyms (names &body body)
  `(let ,(loop for name in names collect `(,name (gensym ,(string name))))
     ,@body))


(defmacro while (expr &body body)
  (with-gensyms (var)
    `(do ((,var ,expr ,expr))
         ((not ,var))
       ,@body)))

(defmacro times (v n &body body)
  (with-gensyms (c)
    `(do ((,v 0 (1+ ,v))
          (,c ,n))
         ((>= ,v ,c) NIL)
       (declare (type fixnum ,v ,c)
                (ignorable ,v))
       ,@body)))





(defun string-to-utf8 (string) (sb-ext:string-to-octets string))
(defun utf8-to-string (bytes)
  (sb-ext:octets-to-string
   (coerce bytes '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)))))




(defun say (&rest xs)
  "Prints arguments separates by space. Returns nil."
  (format t "~a~%"
    (with-output-to-string (o)
      (while xs
        (format o "~a" (pop xs))
        (when xs (format o " "))))))


;; (split #\. "abc.def") -> ("abc" "def")
(defun split (sep xs)
  (loop for i = 0 then (1+ j)
        as j = (position sep xs :start i)
        collect (subseq xs i j)
        while j))

(defun split-if (predicate xs)
  (loop for i = 0 then (1+ j)
        as j = (position-if predicate xs :start i)
        collect (subseq xs i j)
        while j))

(defun unsplit (sep xs)
  (setf sep (string sep))
  (reduce (lambda (a b) (concatenate 'string a sep b)) (cdr xs)
          :initial-value (or (car xs) "")))

(defun includes (v xs)
  (when (consp xs)
    (or (eql (car xs) v)
        (includes v (cdr xs)))))

(defun match-hole (key hole hit miss)
  (unless (consp hole)
    (return-from match-hole
      (if (and hole (symbolp hole) (not (keywordp hole)) (not (eql hole t)))
          (if (string= (symbol-name hole) "_")
              hit
              `(let ((,hole ,key))
                 ,hit))
          `(if (equal ',hole ,key)
               ,hit
               ,miss))))
  (when (eql (car hole) '=)
    (return-from match-hole
       (match-hole key (second hole) (match-hole key (third hole) hit miss) miss)))
  (when (eql (car hole) 'or)
    (return-from match-hole
      `(if (match ,key ,@(mapcar (lambda (x) `(,x t)) (cdr hole)))
           ,hit
           ,miss)))
  (when (eql (car hole) 'not)
    (return-from match-hole
      `(if (match ,key ,@(mapcar (lambda (x) `(,x t)) (cdr hole)))
           ,miss
           ,hit)))
  (when (eql (car hole) '/)
    (return-from match-hole
      (let ((g (gensym)))
        `(let ((,g (,(second hole) ,key)))
           ,(match-hole g (third hole) hit miss)))))
  (when (includes '! hole)
    (let ((xs (split '! hole)))
      (return-from match-hole
        `(let ((,@(car xs) ,key))
           (if ,(!body (cdr xs))
               ,hit
               ,miss)))))
  (when (and (eql (car hole) 'quote)
             (= (length hole) 2))
    (return-from match-hole
      `(if (equal ,(second hole) ,key)
           ,hit
           ,miss)))
  (let ((x (gensym))
        (hit (match-hole key (cdr hole) hit miss)))
    `(if (consp ,key)
         (let ((,x (car ,key))
               (,key (cdr ,key)))
           ,(match-hole x (car hole) hit miss))
         ,miss)))

(defmacro match (keyform &body cases)
  (let ((key (gensym))
        (b (gensym)))
    `(let ((,key ,keyform))
       (block ,b
         (tagbody
           ,@(reduce (lambda (next case)
                       (let ((miss (gensym))
                             (hit `(return-from ,b (progn ,@(cdr case)))))
                         `(,(match-hole key (car case) hit `(go ,miss)) ,miss ,@next)))
                     (cons nil (nreverse cases))))))))

#|
;; example usage:
(defun flatten (x)
  (match x ((x . xs) (append (flatten x) (flatten xs)))
           ((x ! and x (atom x)) (list x))))
|#

(defun !string (x)
  "Generates code for string splicing"
  (let ((as nil)
        (fmt nil)
        (s (position #\{ x)))
    (unless s (return-from !string x))
    (while s
      (let ((e (position #\} x)))
        (unless e (error "unterminated {"))
        (push (subseq x 0 s) fmt)
        (push "~a" fmt)
        (push `(! ,@(read-from-string (format nil "(~a)" (subseq x (+ s 1) e))))
              as)
        (setf x (subseq x (+ e 1) (length x)))
        (setf s (position #\{ x))))
    `(format nil ,(apply #'concatenate 'string (nreverse (cons x fmt)))
             ,@(nreverse as))))

(defun !strings (x)
  (cond ((consp x) (if (or (eql (car x) 'quote) (eql (car x) '!))
                       x
                       (cons (!strings (car x)) (!strings (cdr x)))))
        ((stringp x) (!string x))
        (t x)))

(defun !decls (xs)
  (unless xs (return-from !decls xs))
  (let* ((x  (pop xs))
         (xs (!decls xs))
         (p  (and (consp x)
                  (position-if (lambda (s) (and (symbolp s)
                                                (equal (symbol-name s) "=")))
                            x))))
    (unless p
      (when (= (length x) 1) (setf x (first x)))
      (return-from !decls `(,x ,@xs)))
    (let* ((l (subseq x 0 p))
           (r (subseq x (+ p 1))))
      (when (= (length r) 1) (setf r (first r)))
      (when (keywordp (elt x p))
        (when (= (length l) 1) (setf l (first l)))
        (return-from !decls `((setf ,l ,r) ,@xs)))
      (if (= (length l) 1)
          (let ((a (first l)))
            (if (consp a)
                `((match ,r
                    (,a ,@xs)
                    (else (error "cant match ~a with ~a" else ',a))))
                `((let ((,a ,r)) ,@xs))))
          `((multiple-value-bind ,l ,r ,@xs))))))

(defun !body (xs)
  (let* ((xs (!strings xs))
         (fs nil)
         (xs (mapcar (lambda (x)
                       (if (and (consp x)
                                (consp (car x))
                                (eql (car (car x)) 'to))
                           (let* ((x (split '! (cdr (first x))))
                                  (name (car (car x)))
                                  (args (cdr (car x)))
                                  (body (cdr x)))
                             (push name fs)
                             `(setf ,name (lambda ,args
                                            (block ,name ,(!body body)))))
                           x))
                     xs))
         (xs (!decls xs))
         (xs (if (cdr xs)
                 `(progn ,@xs)
                 (car xs))))
    (when fs
      (setf xs
            `(let ,fs
               (macrolet ,(mapcar (lambda (f)
                                    `(,f (&rest xs)
                                         (cons 'funcall (cons ',f xs))))
                                  fs)
                 ,xs))))
    xs))

(defmacro ! (&body xs)
  "Imperative notation, where each statement is prefixed by `!`"
  (!body (split '! xs)))

(defmacro to (name &body xs)
  (let ((xs (split '! xs)))
    `(defun ,name ,(car xs)
       (macrolet ((ret (&rest xs) (list* 'return-from ',name xs)))
         ,(!body (cdr xs))))))

(defmacro to-expand (name &body xs)
  (let ((xs (split '! xs)))
    `(defmacro ,name ,(car xs)
       (macrolet ((ret (&rest xs) (list* 'return-from ',name xs)))
         ,(!body (cdr xs))))))

(defmacro fn (&body xs)
  (let* ((xs (split '! xs)))
    `(lambda ,(car xs) ,(!body (cdr xs)))))
