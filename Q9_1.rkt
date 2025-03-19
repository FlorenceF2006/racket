#lang racket

(define (op-trans op)
  (match op
    ['+ 'add]
    ['* 'mul]
    ['- 'sub]
    ['div 'div]
    ['mod 'mod]
    
    ))
;;
;;assume we have a global counter : stk-ptr
(define stk-ptr 0)
;;assume we have a global accumulative data storage mem
(define mem empty)
;;get-sym returns the pysmbol with stk-ptr
(define (get-sym ptr)
  (+ '_' (number->symbol ptr)))

(define (add-inst inst s)
    (set! s (append s (list inst))))

(define acc empty)

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
       (define pys (get-sym ptr))
       (add-inst (list op pys opd1 opd2) acc)
       (add-inst (list 'data pys 0) mem)
       (set! stk-ptr (+ stk-ptr 1))
       pys]
      [x x]))
  (eval-h exp)
  )

(define (compile prog)
  (set! mem (second (prog)))
  (set! prog (rest (rest prog)))
  (define (compile-h prog)
    (match (first prog)
      [`(print ,exp)
       (cond[(string? exp)
             (add-inst (list 'print-string exp))]
            [else
             (define pys (eval-aexp exp))
             (add-inst (list 'print-val pys))])]
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
             (compile-h (rest (first porg)))]
            [else
             (define (first prog) loop)
             (define (rest prog) bdy)
             (set! bdy (cons 'seq bdy))
             (define (second loop) bexp)
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
             (add-inst (list 'label fl))])])))
       
       
       
  


























             
       
       
