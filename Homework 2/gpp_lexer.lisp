(defvar dfa 0)
(defvar secondPart 0)
(defvar kw_list '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(defvar kw_tokens '("KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(defvar op_list '("+" "-" "/" "*" "(" ")" "**" "\"" ","))
(defvar op_tokens '("OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP" "OP_DBLMULT" "OP_OC" "OP_COMMA" "OP_CC"))

(defvar quote_counter 0)

(defun is-alpha (c)
  (if (alpha-char-p c)
      (setq dfa 1)
      (setq dfa -1)))

(defun is-alphanum (c)
  (if (alphanumericp c)
      (setq dfa 1)
      (setq dfa -1)))

(defun is-num-except-zero (c)
  (if (and (not (equal c #\0)) (digit-char-p c))
      (setq dfa 1)
      (setq dfa -1)))

(defun is-zero (c)
  (if (and (equal c #\0)(digit-char-p c))
      (setq dfa 1)
      (setq dfa -1)))

(defun is-numeric (c)
  (if (digit-char-p c)
      (setq dfa 1)
      (setq dfa -1)))

(defun is-numeric-or-dot (c)
  (cond ((digit-char-p c) (setq dfa 1))
	((equal c #\.) (setq dfa 1) (setq secondPart 1))
	(t (setq dfa -1))))

(defun dfa-ident (str)
  (setq dfa 0)
  (loop for i across str
	do (cond ((equal dfa 0) (is-alpha i))
		 ((equal dfa 1) (is-alphanum i))
		 (t 0)))
  (if (equal dfa 1)
      1
      0))
	
(defun dfa-value (str)
  (setq dfa 0)
  (if (equal (length str) 1)
      (if (is-numeric (char str 0))
	  1
	  0))
  (loop for i across str
	do (cond ((equal dfa 0) (is-num-except-zero i))
		 ((equal dfa 1) (is-numeric i))
		 (t 0)))
  (if (equal dfa 1)
      1
      0))

(defun dfa-real-value (str)
  (setq dfa 0)
  (setq secondPart 0)
  (if (< (length str) 3)
	 0
(progn	 
  (loop for i across str
	do (cond ((and (equal (char str 1) #\.)(equal dfa 0))(is-numeric i))
		 ((equal dfa 0) (is-num-except-zero i))
		 ((and (equal dfa 1) (equal secondPart 0)) (is-numeric-or-dot i))
		 ((and (equal dfa 1) (equal secondPart 1)) (is-numeric i))
		 (t 0)))))
  (if (equal dfa 1)
      1
      0))

(defun dfa-error-value (str)
  (setq dfa 0)
  (if (equal (length str) 1)
      0
      (progn
	(loop for i across str
	do (cond ((equal dfa 0) (is-zero i))
		 ((equal dfa 1) (is-numeric i))
		 (t 0)))
	(if (equal dfa 1)
	    1
	    0))))

(defun dfa-not-defined (str)
  (if (equal (length str) 1)
      (if (not (or (equal str " ") 
		   (equal str "\n") 
		   (equal str "\t")
		   (equal str "\r")
		   (equal (char str 0) #\tab)))
	  1
	  0)
      0))

(defun dfa-comment (str)
  (if (>= (length str) 2)
      (if (equal (subseq str 0 2) ";;")
	  1
	  0)
      0))

(defun dfa-keyword (str)
  (setf dfa -1)
  (let ((i 0))
    (loop for x in kw_list
	  do (if (equalp str x)
		 (setf dfa i))
	     (setf i (+ i 1))))
  dfa)

(defun dfa-op (str)
  (setf dfa -1)
  (let ((i 0))
    (loop for x in op_list
	  do (if (equalp str x)
		 (setf dfa i))
	     (setf i (+ i 1))))
  (if (equal dfa 7)
      (progn
	(setf quote_counter (+ quote_counter 1))
	(if (equal (mod quote_counter 2) 0)
	    (setf dfa 9))))
  dfa) 

(defvar lineList nil)
(setq lineList nil)

(defun stringToList (line)
  (let ((lower 0) (upper 1) (max (length line)))
    (loop while (and (< lower max) (<= upper max))
	  do (cond  ((or 
		      (equal (subseq line lower upper) "(")
		      (equal (subseq line lower upper) ")")
		      (equal (subseq line lower upper) ";;") 
		      (equal (subseq line lower upper) "\""))
		    (setf lineList (append lineList (list (subseq line lower upper))))
		    (setf lower upper)
		    (setf upper (+ 1 upper)))
		   ((or 
		     (equal (char line lower) #\ )
		     (equal (char line lower) #\tab))
		    (setf lower upper)
		    (setf upper (+ 1 upper)))
		   ((or 
		     (equal (char line lower) #\Newline)
		     (equal (char line lower) #\Linefeed)
		     (equal (char line lower) #\return))
		    (return))
		   ((equal upper max)
		    (setf lineList (append lineList (list (subseq line lower upper))))
		    (setf upper (+ upper 1)))
		   ((equal (char line upper) #\ )
		    (setf lineList (append lineList (list (subseq line lower upper))))
		    (setf lower upper)
		    (setf upper (+ upper 1)))
		   ((or (equal (char line upper) #\()
			(equal (char line upper) #\))
			(equal (char line upper) #\"))
		    (setf lineList (append lineList (list (subseq line lower upper))))
		    (setf lower upper)
		    (setf upper (+ upper 1)))
		   (t (setf upper (+ upper 1)))))))
		    
(defun lex (line)
  (setq lineList nil)
  (setq outlst nil)
  (stringToList line)
  (let ((err 0))
    (loop for str in lineList
	 do (cond ((or (equal str "\n")
		       (equal str "\r")
		       (equal str "\t")
		       (equal str (string #\tab)))
		   ())
		  ((>= (dfa-op str) 0)
		   (setf outlst (append outlst (list (list (nth dfa op_tokens))))))
		  ((>= (dfa-keyword str) 0)
		   (setf outlst (append outlst (list (list (nth dfa kw_tokens))))))
		  ((equal (dfa-comment str) 1)
		   (setf outlst (append outlst (list (list "COMMENT"))))
		   (return))
		  ((equal (dfa-ident str) 1)
		   (setf outlst (append outlst (list (list "IDENTIFIER" str)))))
		  ((or (equal (dfa-value str) 1)
		       (equal (dfa-real-value str) 1))
		   (setf outlst (append outlst (list (list "VALUE" str)))))
		  (t
		   (setf outlst (append outlst (list (list (concatenate 'string 
								   "SYNTAX_ERROR " (concatenate 'string str " can not be tokenized"))))))
		   (setf err 1)
		   (return))))
    (if(= err 1)
     (cons outlst -1)
	 (cons outlst 1))))
   
(defun tokenize-file (filename)
  (let ((in (open filename :if-does-not-exist nil)) (end) (tokens))
	  (when in
	    (loop for line = (read-line in nil)
		  while line do (setq end (lex line))
		  (setq tokens (append tokens (car end)))
		  (when (equal (cdr end) -1) 
		    (close in)
		    (return)))
	  (close in))
    ;(write tokens)
	;(write (cdr end))
    (cons tokens (cdr end))))		
