(load "gpp_lexer.lisp")
(defvar table nil)
(defvar undefined nil)
(defvar end nil)

(defun set-table (id val)
  (if (equal (get-table-index id) -1)
      (setq table (append table (list id val)))
      (setf (nth (get-table-index id) table) val)))

(defun get-table-index (id)
  (loop for x from 0 to (length table)
	do (if (equal (nth x table) id)
	       (return-from get-table-index (+ x 1))))
  -1) 

(defun get-table-value (id)
  (loop for x from 0 to (length table)
	do (when (equal (nth x table) id)
	     (return-from get-table-value (nth (+ x 1) table))))
  (setq undefined id)
  (return-from get-table-value nil))

(defun START (tokens index)
  (let ((res))
    (when (>= index (length tokens)) (return-from START))
    (setq res (INPUT tokens index))
    (if (equal (cdr res) nil)
	(progn
	  (when (equal (car res) "EXIT")
	    (format t "SYNTAX OK.~%Program Terminated.~%")
	    (setq end T)
	    (return-from START)) 
	  (if (not (equal undefined nil))
	      (progn
		(format t "ERROR : Identifier ~a has not been defined~%" undefined)
		(setq end T)
		(setq undefined nil))
	      (format t "SYNTAX_ERROR Expression not recognized~%"))
	  (return-from START))
	(progn
	  (when (not (equal (car (nth (- (cdr res) 1) tokens)) "COMMENT"))
	    (format t "SYNTAX OK.~%Result: ~d~%" (car res)))
	  (START tokens (cdr res))))))
    
(defun INPUT (tokens index)
  (let ((res))
    (cond
      ((setq res (COMMENT tokens index)) res)
      ((setq res (EXPI tokens index)) res)
      ((setq res (EXPB tokens index)) res)
      ((setq res (EXPLISTI tokens index)) res)
      ((setq res (TERMINATE tokens index)) res)
      (t nil))))

(defun TERMINATE (tokens index)
  (when (equal (car (nth index tokens)) "OP_OP")
    (when (equal (car (nth (+ index 1) tokens)) "KW_EXIT")
      (when (equal (car (nth (+ index 2) tokens)) "OP_CP")
	(return-from TERMINATE (cons "EXIT"
				NIL))))))

(defun COMMENT (tokens index)
  (when (equal (car (nth index tokens)) "COMMENT")
    (return-from COMMENT (cons ()
			       (+ index 1)))))

(defun EXPI (tokens index)
  (let ((res nil))
    (cond 
      ((setq res (EXPI-1 tokens index)) res)
      ((car (setq res (EXPI-2 tokens index))) res)
      ((setq res (EXPI-3 tokens index)) res)
      ((setq res (EXPI-4 tokens index)) res)
      ((setq res (EXPI-5 tokens index)) res)
      (t nil))))
      
;; + | - | * | /
(defun EXPI-1 (tokens index)
  (let ((op) (first) (second))
    (when (equal (car (nth index tokens)) "OP_OP")
      (when (is-arithmetic (setq op (car (nth (+ index 1) tokens))))
	(when (setq first (EXPI tokens (+ index 2)))
	  (when (setq second (EXPI tokens (cdr first)))
	    (when (equal (car (nth (cdr second) tokens)) "OP_CP")
	      (return-from EXPI-1 (cons (operate op (car first) (car second))
					(+ (cdr second) 1))))))))))

;; IDENTIFIER
(defun EXPI-2 (tokens index)
  (when (equal (car (nth index tokens)) "IDENTIFIER")
    (return-from EXPI-2 (cons (get-table-value (car (cdr (nth index tokens))))
			      (+ index 1)))))
  
;; VALUE    
(defun EXPI-3 (tokens index)
  (when (equal (car (nth index tokens)) "VALUE")
    (return-from EXPI-3 (cons (parse-integer (car (cdr (nth index tokens))))
			     (+ index 1)))))

;; set
(defun EXPI-4 (tokens index)
  (let ((first))
    (when (equal (car (nth index tokens)) "OP_OP")
      (when (equal (car (nth (+ index 1) tokens)) "KW_SET")
	(when (equal (car (nth (+ index 2) tokens)) "IDENTIFIER")
	  (when (setq first (EXPI tokens (+ index 3)))
	    (when (equal (car (nth (cdr first) tokens)) "OP_CP")
	      (set-table (car (cdr (nth (+ index 2) tokens))) (car first))
	      (return-from EXPI-4 (cons 
				   (car first)
				   (+ (cdr first) 1))))))))))

;; if
(defun EXPI-5 (tokens index)
  (let ((condition) (first) (second))
    (when (equal (car (nth index tokens)) "OP_OP")
      (when (equal (car (nth (+ index 1) tokens)) "KW_IF")
	(when (setq condition (EXPB tokens (+ index 2)))
	  (when (or 
		 (setq first (EXPLISTI tokens (cdr condition)))
		 (setq first (EXPI tokens (cdr condition))))
	    (when (equal (car (nth (cdr first) tokens)) "OP_CP")
	      (return-from EXPI-5 (cons
				   (execute-if (car condition) (car first))
				   (+ (cdr first) 1))))
	    (when (or 
		   (setq second (EXPLISTI tokens (cdr first)))
		   (setq second (EXPI tokens (cdr first))))
	      (when (equal (car (nth (cdr second) tokens)) "OP_CP")
		(return-from EXPI-5 (cons
				     (execute-if (car condition) (car first) (car second))
				     (+ (cdr second) 1)))))))))))

	
(defun EXPB (tokens index)
  (let ((res nil))
    (cond 
      ((setq res (EXPB-1 tokens index)) res)
      ((setq res (EXPB-2 tokens index)) res)
      ((setq res (EXPB-3 tokens index)) res)
      ((setq res (EXPB-4 tokens index)) res)
      (t nil))))

;; (and|or|equal) EXPB EXPB 
(defun EXPB-1 (tokens index)
  (let ((op) (first) (second))
    (when (equal (car (nth index tokens)) "OP_OP")
      (when (is-logical (setq op (car (nth (+ index 1) tokens))))
	(when (setq first (EXPB tokens (+ index 2)))
	  (when (setq second (EXPB tokens (cdr first)))
	    (when (equal (car (nth (cdr second) tokens)) "OP_CP")
	      (return-from EXPB-1 (cons 
				   (logic-operate op (car first) (car second))
				   (+ (cdr second) 1))))))))))

;; equal EXPI EXPI | less EXPI EXPI
(defun EXPB-2 (tokens index)
  (let ((first) (second))
    (when (equal (car (nth index tokens)) "OP_OP")
      (when (or
	     (equal (car (nth (+ index 1) tokens)) "KW_EQUAL")
	     (equal (car (nth (+ index 1) tokens)) "KW_LESS"))
	(when (setq first (EXPI tokens (+ index 2)))
	  (when (setq second (EXPI tokens (cdr first)))
	    (when (equal (car (nth (cdr second) tokens)) "OP_CP")
	      (return-from EXPB-2 (cons 
				   (logic-operate (car (nth (+ index 1) tokens))
						  (car first) 
						  (car second))
				   (+ (cdr second) 1))))))))))
  
;; not EXPB
(defun EXPB-3 (tokens index)
  (let ((first))
    (when (equal (car (nth index tokens)) "OP_OP")
      (when (equal (car (nth (+ index 1) tokens)) "KW_NOT")
	(when (setq first (EXPB tokens (+ index 2)))
	  (when (equal (car (nth (cdr first) tokens)) "OP_CP")
	    (return-from EXPB-3 (cons (not (car first))
				      (+ (cdr first) 1)))))))))

;; binary
(defun EXPB-4 (tokens index)
  (cond 
    ((equal (car (nth index tokens)) "KW_TRUE") (cons T (+ index 1)))
    ((equal (car (nth index tokens)) "KW_FALSE") (cons nil (+ index 1)))))

      
(defun EXPLISTI (tokens index)
  (let ((res nil))
    (cond 
      ((setq res (EXPLISTI-1 tokens index)) res)
      ((setq res (EXPLISTI-2 tokens index)) res)
      ((setq res (EXPLISTI-3 tokens index)) res)
      (t nil)))) 

; concat
(defun EXPLISTI-1 (tokens index)
  (let ((first) (second))
    (when (equal (car (nth index tokens)) "OP_OP")
      (when (equal (car (nth (+ index 1) tokens)) "KW_CONCAT")
	(when (setq first (EXPLISTI tokens (+ index 2)))
	  (when (setq second (EXPLISTI tokens (cdr first)))
	    (when (equal (car (nth (cdr second) tokens)) "OP_CP")
	      (return-from EXPLISTI-1 (cons
				       (concatenate 'list (car first) (car second))
				       (+ (cdr second) 1))))))))))
	
; append
(defun EXPLISTI-2 (tokens index)
  (let ((first) (second))
    (when (equal (car (nth index tokens)) "OP_OP")
      (when (equal (car (nth (+ index 1) tokens)) "KW_APPEND")
	(when (setq first (EXPI tokens (+ index 2)))
	  (when (setq second (EXPLISTI tokens (cdr first)))
	    (when (equal (car (nth (cdr second) tokens)) "OP_CP")
	      (return-from EXPLISTI-2 (cons
				       (append (list (car first)) (car second))
				       (+ (cdr second) 1))))))))))

; listvalue
(defun EXPLISTI-3 (tokens index)
  (let ((first))
    (when (equal (car (nth index tokens)) "KW_NIL")
      (return-from EXPLISTI-3 (cons
			       nil
			       (+ index 1))))
    (when (equal (car (nth index tokens)) "OP_OP")
      (when (equal (car (nth (+ index 1) tokens)) "KW_LIST")
	(when (setq first (is-values tokens (+ index 2)))
	  (when (equal (car (nth (cdr first) tokens)) "OP_CP")
	    (return-from EXPLISTI-3 (cons
				     (car first)
				     (+ (cdr first) 1)))))))))

; values integervalue | integervalue
(defun is-values (tokens index)
  (let ((vals) (val))
    (loop
      (if (setq val (EXPI-3 tokens index))
	  (progn
	    (setq vals (append vals (list (car val))))
	    (setq index (cdr val))
	    (setq val nil))
	  (return)))
    (cons vals index)))
    
(defun execute-if (condition first &optional (second nil))
  (if condition
      first
      second))

(defun is-logical (str)
  (when (or
	 (equal str "KW_AND")
	 (equal str "KW_OR")
	 (equal str "KW_EQUAL")
	 (equal str "KW_LESS"))
    t))

(defun logic-operate (op val1 val2)
  (cond
    ((equal op "KW_AND") (and val1 val2))
    ((equal op "KW_OR") (or val1 val2))
    ((equal op "KW_EQUAL") (equal val1 val2))
    ((equal op "KW_LESS") (< val1 val2))))

(defun is-arithmetic (str)
  (when (or
	 (equal str "OP_PLUS")
	 (equal str "OP_MINUS")
	 (equal str "OP_DIV")
	 (equal str "OP_MULT"))
    t))
      
(defun operate (op val1 val2)
  (cond 
    ((equal op "OP_PLUS") (+ val1 val2))
    ((equal op "OP_MINUS") (- val1 val2))
    ((equal op "OP_MULT") (* val1 val2))
    ((equal op "OP_DIV") (/ val1 val2))))

(defun gppinterpreter (&optional (filename nil))
  (let ((tokens) (line))
    (when (not (equal filename nil))
      (setq tokens (tokenize-file filename))
      (if (equal (cdr tokens) -1)
	  (progn
	    (format t "~a~%" (car (nth (- (length (car tokens)) 1) (car tokens))))
	    (setq end T))
	  (START (car tokens) 0)))
    (loop 
      (when end (return))
      (setq line (read-line))
      (setq tokens (lex line))
      (when (equal (car tokens) nil)(return))
      (when (equal (cdr tokens) -1) (return))
      (START (car tokens) 0))))
      
(defun main ()
  (if (equal (car *args*) nil)
      (gppinterpreter)
      (gppinterpreter (car *args*))))

(main)
  
