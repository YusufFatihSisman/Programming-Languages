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

(defvar outlst nil)
(setq outlst nil)

(defun lex (line lower upper)
  (setq err 0)
  (loop while (< lower upper)
	do (cond ((equal (subseq line lower upper) " ")
		  (setf lower upper)
		  (setf upper (length line)))
		 ((equal (subseq line lower upper) "\r")
		  (setf lower upper)
		  (setf upper (length line)))
		 ((equal (subseq line lower upper) "\t")
		  (setf lower upper)
		  (setf upper (length line)))
		 ((equal (subseq line lower upper) "\n")
		  (setf lower upper)
		  (setf upper (length line)))
		 ((equal (subseq line lower upper) (string #\tab))
		  (setf lower upper)
		  (setf upper (length line)))
		 ((>= (dfa-op (subseq line lower upper)) 0)
		  (progn
		    (setf outlst (append outlst (list (nth dfa op_tokens))))
		    (setf lower upper)
		    (setf upper (length line))))
		 ((>= (dfa-keyword (subseq line lower upper)) 0)
		  (progn
		    (setf outlst (append outlst (list (nth dfa kw_tokens))))
		    (setf lower upper)
		    (setf upper (length line))))
		 ((equal (dfa-comment (subseq line lower upper)) 1)
		  (progn 
		    (setf outlst (append outlst (list "COMMENT")))
		    (setf lower upper)))
		 ((equal (dfa-ident (subseq line lower upper)) 1)
		  (progn
		    (setf outlst (append outlst (list "IDENTIFIER")))
		    (setf lower upper)
		    (setf upper (length line))))
		 ((or (equal (dfa-value (subseq line lower upper)) 1)
		      (equal (dfa-real-value (subseq line lower upper)) 1))
		  (progn
		    (setf outlst (append outlst (list "VALUE")))
		    (setf lower upper)
		    (setf upper (length line))))
		 ((equal (dfa-error-value (subseq line lower upper)) 1)
		  (progn
		    (setf outlst (append outlst (list (concatenate 'string 
								   "SYNTAX_ERROR " 
								   (concatenate 'string
										(subseq line lower upper)
										" value can not tokenized")))))
		    (setf err 1)
		    (setf lower 1)
		    (setf upper 0)))
		 ((equal (dfa-not-defined (subseq line lower upper)) 1)
		  (progn 
		    (setf outlst (append outlst (list (concatenate 'string 
								   "SYNTAX_ERROR "
								   (concatenate 'string 
										(subseq line lower upper)
										" can not tokenized")))))
		    (setf err 1)
		    (setf lower 1)
		    (setf upper 0)))
		 (t 
		  (setf upper (- upper 1)))))
  (if(= err 1)
     -1
     0))
  

  
(defun gppinterpreter (&optional (filename 0))
  (defvar line nil)
  (defvar input nil)
  (setq end 0)
  (with-open-file (fout "parsed_lisp.txt"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (if (equal filename 0)
      (loop 
	(setf line (read-line))
	(setq end (lex line 0 (length line)))
	(loop for x in outlst
	      do (format fout "~a~%" x)
		 (format t "~a~%" x))
	(if (not (equal outlst nil))
	    (setf input 1)
	    (setf input 0))
	(setf outlst nil)
	(when (equal end -1) (return))
	(when (equal input 0) (return)))
      (progn 
	(let ((in (open filename :if-does-not-exist nil)))
	  (when in
	    (loop for line = (read-line in nil)
		  while line do (progn
				  (setq end (lex line 0 (length line)))
				  (loop for x in outlst
					do (format fout "~a~%" x)
					   (format t "~a~%" x))
				  (setf outlst nil))
		  (when (equal end -1) 
		    (close in)
		    (return)))
	  (close in)))
	(if (not (equal outlst nil))
	    (setf input 1)
	    (setf input 0))
	(setf outlst nil)
	(loop 
	  (setf line (read-line))
	  (setq end (lex line 0 (length line)))
	  (loop for x in outlst
	      do (format fout "~a~%" x)
			 (format t "~a~%" x))
	  (if (not (equal outlst nil))
	    (setf input 1)
	    (setf input 0))
	(setf outlst nil)
	(when (equal end -1) (return))
	(when (equal input 0) (return)))))))
      
(defun main ()
  (if (equal (car *args*) nil)
      (gppinterpreter)
      (gppinterpreter (car *args*))))

(main)
  
  
			
