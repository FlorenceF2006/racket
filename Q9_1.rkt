#lang racket


(define (op-trans op)
  (match op
    ['+ 'add]
    ['* 'mul]
    ['- 'sub]
    ['div 'div]
    ['mod 'mod]
    ['= 'eq]
    ['> 'gt]
    ['< 'lt]
    ['>= 'ge]
    ['<= 'le]
    ['not 'lnot]
    ['and 'land]
    ['or 'lor]
    ))
;;
;;assume we have a global counter : stk-ptr
(define stk-ptr 0)
;;assume we have a global accumulative data storage mem
(define mem (box empty))
;;get-sym returns the pysmbol with stk-ptr
(define (get-sym ptr)
  (+ '_' (number->symbol ptr)))

;; can you use + to coonect two symbols?

(define (add-inst inst s)
    (set-box! s (append (unbox s) (list inst))))

(define acc (box empty))

;;what should it return?
;;should it return a struct?
;;or a list? I think it should return a list.
;;we need a list of temporary data we can constantly cons.
(define (eval-aexp exp)
  ;;eval-h returns a pysymbol
  (define (eval-h exp)
    (match exp
      [`(,op ,exp1 ,exp2)
       (define op (op-trans op))
       (define opd1 (eval-h exp1))
       (define opd2 (eval-h exp2))
       (define pys (get-sym stk-ptr))
       (add-inst (list op pys opd1 opd2) acc)
       (add-inst (list 'data pys 0) mem)
       (set! stk-ptr (+ stk-ptr 1))
       pys]
      [x x]))
  (eval-h exp)
  )

(define (eval-bexp exp)
  ;;eval-h: return a pysymbol
  (define (delete-srd lst)
    (cons (first lst) (rest (rest lst))))
  (define (eval-h exp)
    (match exp
      [`(and ,bexp1 ,bexp2)
       (define op (op-trans 'and))
       (define val1 (eval-bexp bexp1))
       (define val2 (eval-bexp bexp2))
       (define pys (get-sym stk-ptr))
       (add-inst (list op pys val1 val2) acc) 
       (add-inst (list 'data pys 0) mem)
       (set! stk-ptr (+ stk-ptr 1))
       pys]
      [`(or ,bexp1 ,bexp2)
       (define op (op-trans 'and))
       (define val1 (eval-bexp bexp1))
       (define val2 (eval-bexp bexp2))
       (define pys (get-sym stk-ptr))
       (add-inst (list op pys val1 val2) acc) 
       (add-inst (list 'data pys 0) mem)
       (set! stk-ptr (+ stk-ptr 1))
       pys]
      [`(and ,bexp1 ...)
       (define val1 (eval-bexp bexp1))
       (define val2 (eval-bexp (delete-srd exp)))
       (define op (op-trans 'and))
       (define pys (get-sym stk-ptr))
       (add-inst (list op pys val1 val2) acc) 
       (add-inst (list 'data pys 0) mem)
       (set! stk-ptr (+ stk-ptr 1))
       pys]
      [`(or ,bexp1 ...)
       (define val1 (eval-bexp bexp1))
       (define val2 (eval-bexp (delete-srd exp)))
       (define op (op-trans 'and))
       (define pys (get-sym stk-ptr))
       (add-inst (list op pys val1 val2) acc) 
       (add-inst (list 'data pys 0) mem)
       (set! stk-ptr (+ stk-ptr 1))
       pys]
      [`(,op ,aexp1 ,aexp2)
       (define op (op-trans op))
       (define val1 (eval-aexp aexp1))
       (define val2 (eval-aexp aexp2))
       (define pys (get-sym stk-ptr))
       (add-inst (list op pys val1 val2) acc) 
       (add-inst (list 'data pys 0) mem)
       (set! stk-ptr (+ stk-ptr 1))
       pys]
      [`(not ,bexp)
       (define op (op-trans 'not))
       (define val (eval-bexp bexp))
       (define pys (get-sym stk-ptr))
       (add-inst (list op pys val) acc)
       (add-inst (list 'data pys 0) mem)
       (set! stk-ptr (+ stk-ptr 1))
       pys]
      [true 1]
      [false 0]))
  (eval-h exp))


(define (compile prog)
  (set-box! mem (second prog))
  (set! prog (rest (rest prog)))
  (define (compile-h prog)
    (cond[ (empty? prog) void]
    [else (match (first prog)
      [`(print ,exp)
       (cond[(string? exp)
             (add-inst (list 'print-string exp) acc)]
            [else
             (define pys (eval-aexp exp))
             (add-inst (list 'print-val pys) acc)])]
      [`(set ,id ,aexp)
       (define pys (eval-aexp exp))
       (add-inst (list 'move id pys) acc)]
      [`(iif ,bexp ,stmt1 ,stmt2)
       (define bs (eval-bexp bexp))
       (define tl (get-sym stk-ptr))
       (set! stk-ptr (+ stk-ptr 1))
       (define fl (get-sym stk-ptr))
       (set! stk-ptr (+ stk-ptr 1))
       (add-inst (list 'branch bs tl) acc)
       (add-inst (list 'jump fl) acc)
       (add-inst (list 'label tl) acc)
       (compile-h stmt1)
       (define nl (get-sym stk-ptr))
       (set! stk-ptr (+ stk-ptr 1))
       (add-inst (list 'jump nl) acc)
       (add-inst (list 'label fl) acc)
       (compile-h stmt2)
       (add-inst (list 'label nl) acc)]
      [`(skip) void]
      [_
       (cond[(equal? (first (first prog)) 'seq)
             (compile-h (rest (first prog)))]
            [else
             (define loop (first prog))
             (define bdy (rest prog))
             (set! bdy (cons 'seq bdy))
             (define bexp (second loop))
             (define bs (eval-bexp bexp))
             (define top (get-sym stk-ptr))
             (set! stk-ptr (+ stk-ptr))
             (define tl (get-sym stk-ptr))
             (set! stk-ptr (+ 1 stk-ptr))
             (define fl (get-sym stk-ptr))
             (set! stk-ptr (+ 1 stk-ptr))
             (add-inst (list 'label top) acc)
             (add-inst (list 'branch bs tl) acc)
             (add-inst (list 'jump fl) acc)
             (add-inst (list 'label tl) acc)
             (compile-h bdy)
             (add-inst (list 'jump top) acc)
             (add-inst (list 'label fl))])])
    (compile-h (rest prog))]
    ))
  (compile-h prog)
  (append (unbox acc) '((halt)) (unbox mem))
  )

       
       
       
  


























             
       
       
