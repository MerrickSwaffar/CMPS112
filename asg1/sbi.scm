#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm 
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stderr* (current-error-port))

(define *run-file*
   (let-values
      (((dirpath basepath root?)
         (split-path (find-system-path 'run-file))))
      (path->string basepath)))

(define (die list)
   (for-each (lambda (item) (display item *stderr*)) list)
   (newline *stderr*)
   (exit 1))

(define (usage-exit)
   (die `("Usage: " ,*run-file* " filename")))

(define (readlist-from-inputfile filename)
   (let ((inputfile (open-input-file filename)))
       (if (not (input-port? inputfile))
          (die `(,*run-file* ": " ,filename ": open failed"))
          (let ((program (read inputfile)))
              (close-input-port inputfile)
                   program))))

(define *symbol-table* (make-hash)) 
(define (symbol-put! key value)
        (hash-set! *symbol-table* key value))
(for-each
    (lambda (pair)
            (symbol-put! (car pair) (cadr pair)))
    `(
        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (-       ,-)
        (*       ,*)
        (/       ,(lambda (x y) (/ (+ x 0.0) (+ y 0.0))))
        (%       ,(lambda (x y) (- x (* (truncate (/ x y)) y))))
        (=       ,=)
        (<       ,<)
        (>       ,>)
        (<=      ,<=)
        (>=      ,>=)
        (<>      ,(lambda (x y) (not (= x y))))
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (sqrt    ,sqrt)
        (abs     ,abs)
        (acos    ,acos)
        (asin    ,asin)
        (atan    ,atan)
        (cos     ,cos)
        (sin     ,sin)
        (tan     ,tan)
        (log     ,(lambda (x) (log (+ x 0.0))))
        (log2    ,(lambda (x) (/ (log x) (log 2.0))))
        (round   ,round)
        (trunc   ,truncate)
     ))
 
(define *function-table* (make-hash)) 
(define *label-table* (make-hash)) 
(define (h_eval expr) 
  (cond
    ((string? expr) expr)
    ((number? expr) expr)
    ((hash-has-key? *symbol-table* expr) 
      (hash-ref *symbol-table* expr))
    ((list? expr)
      (if (hash-has-key? *symbol-table* (car expr))
        (let((head (hash-ref *symbol-table*  (car expr))))
          (cond 
            ((procedure? head)
             (apply head (map (lambda (x) (h_eval x)) (cdr expr))))
            ((vector? head)
             (vector-ref head (cadr expr)))
            ((number? head) head)
            (else
              (die "Fatal: Broken symbol table."))))
        (die (list "Fatal error: " 
                   (car expr) " not in symbol table!\n"))))))

(define (sbir-print expr)
   (map (lambda (x) (display (h_eval x))) expr)
   (newline))

(define (sbir-dim expr)
  (set! expr (car expr))
  (let((arr (make-vector (h_eval (cadr expr)) (car expr))))
    (symbol-put! (car expr) (+ (h_eval (cadr expr)) 1))))

(define (sbir-let expr)
  (symbol-put! (car expr) (h_eval (cadr expr))))

(define (sbir-input2 expr count)
  (if (null? expr)
    count
     (let ((input (read)))
        (if (eof-object? input)
          -1
          (begin
            (symbol-put! (car expr) input)
            (set! count (+ 1 count))
            (sbir-input2 (cdr expr) count))))))

(define (sbir-input expr) 
  (symbol-put! 'inputcount 0)
  (if (null? (car expr))
    (symbol-put! 'inputcount -1)
    (begin
    (symbol-put! 'inputcount (sbir-input2 expr 0)))))


(define (exec-line instr program line-nr) 
  (when (not (hash-has-key? *function-table* (car instr))) 
        (die "~s is not a valid instruction." (car instr)))
  (cond
        ((eq? (car instr) 'goto)
            (execute-program program (hash-ref 
                *label-table* (cadr instr))))
        ((eq? (car instr) 'if)
         (if (h_eval (car (cdr instr)))
           (execute-program program 
               (hash-ref *label-table* (cadr (cdr instr))))
           (execute-program program (+ line-nr 1))))
        ((eq? (car instr) 'print)
         (if (null? (cdr instr))
           (newline)
           (sbir-print (cdr instr))) 
           (execute-program program (+ line-nr 1)))
        (else
          ((hash-ref *function-table* (car instr)) (cdr instr))
          (execute-program program (+ line-nr 1)))))


(define (execute-program program line-nr) 
   (when (> (length program) line-nr)
 
    (let((line (list-ref program line-nr)))
    (cond
      ((= (length line) 3)
       (set! line (cddr line))
       (exec-line (car line) program line-nr))
      ((and (= (length line) 2) (list? (cadr line)))
       (set! line (cdr line))
       (exec-line (car line) program line-nr))
      (else 
        (execute-program program (+ line-nr 1)))
    ))))

(define length
   (lambda (ls)
     (if (null? ls)
         0
         (+ (length (cdr ls)) 1))))

(define (get-labels program)

   (map (lambda (line) 
          (when (not (null? line))
            (when (or (= 3 (length line))
                      (and (= 2 (length line)) 
                           (not (list? (cadr line)))))

                (hash-set! *label-table* (cadr line) (- (car line) 1 ))
                ))) program)
)

(define (main arglist)
   (if (or (null? arglist) (not (null? (cdr arglist))))
      (usage-exit) 
      (let* ((sbprogfile (car arglist))
            (program (readlist-from-inputfile sbprogfile))) 
 
        (get-labels program)
        (execute-program program 0)
        )))
(for-each
  (lambda (pair)
          (hash-set! *function-table* (car pair) (cadr pair)))
  `(      
      (print ,sbir-print)
      (dim   ,sbir-dim)
      (let   ,sbir-let)
      (input ,sbir-input)
      (if    (void))
      (goto  (void))))
(main (vector->list (current-command-line-arguments)))
