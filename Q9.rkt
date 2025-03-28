;;Fee Chen, Florence Fan
#lang racket

(define (compile-simpl program)
  
(define (op-trans op)
  (match op
    ['+ 'add]
    ['* 'mul]
    ['- 'sub]
    ['div 'div]
    ['mod 'mod]
    ['= 'equal]
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
  (string->symbol (string-append "_" (number->string ptr))))

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
      [`(,ops ,exp1 ,exp2)
       (define op (op-trans ops))
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
  (define (eval-h lst op sym)
    (cond[(empty? lst) void]
         [else
          (define cur (eval-bexp (first lst)))
          (add-inst (list (op-trans op) sym cur sym) acc)
          (eval-h (rest lst) op sym)]))
  (match exp
    [`(,op ,bexp1 ,bexp2)
     (define val1 (eval-bexp bexp1))
     (define val2 (eval-bexp bexp2))
     (define psy (get-sym stk-ptr))
     (set! stk-ptr (+ stk-ptr 1))
     (add-inst (list (op-trans op) psy val1 val2) acc)
     (add-inst (list  'data psy #f) mem)
     psy]
    [`(,op,bexp)
     (define val (eval-bexp bexp))
     (define psy (get-sym stk-ptr))
     (set! stk-ptr (+ stk-ptr 1))
     (add-inst (list (op-trans op) psy val) acc)
     (add-inst (list 'data psy #f) mem) psy]
    ['true #t]
    ['false #f]
    [x (cond[(not (list? x)) x]
            [else (define psy (get-sym stk-ptr))
       (set! stk-ptr (+ stk-ptr 1))
       (if (equal? (first exp) 'and) (add-inst (list 'data psy #t) mem)
           (add-inst (list 'data psy #f) mem))
       (eval-h (rest exp) (first exp) psy) psy])])
  )

(define (to-imm x)
  (match x
    [`true #t]
    [`false #f]
    [_ x]))
(define (convert-data lst)
  (cond[(empty? lst) empty]
       [else (cons (list 'data (first (first lst)) (to-imm (second (first lst))))
                   (convert-data (rest lst)))
                    ]))

(define (compile prog)
  (set-box! mem (convert-data (second prog)))
  (set! prog (rest (rest prog)))
  (define (compile-h prog)
    (cond[ (empty? prog) void]
    [else
     (match (first prog)
      [`(print ,exp)
       (cond[(string? exp)
             (add-inst (list 'print-string exp) acc)]
            [else
             (define pys (eval-aexp exp))
             (add-inst (list 'print-val pys) acc)])]
      [`(set ,id ,aexp)
       (define pys (eval-aexp aexp))
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
       (compile-h (list stmt1))
       (define nl (get-sym stk-ptr))
       (set! stk-ptr (+ stk-ptr 1))
       (add-inst (list 'jump nl) acc)
       (add-inst (list 'label fl) acc)
       (compile-h (list stmt2))
       (add-inst (list 'label nl) acc)]
      [`(skip) void]
      [`(while ...)
       (define loop (first prog))
       (define bdy (rest (rest loop)))
       (define bexp (second loop))
       (define top (get-sym stk-ptr))
       (set! stk-ptr (+ 1 stk-ptr))
       (define tl (get-sym stk-ptr))
       (set! stk-ptr (+ 1 stk-ptr))
       (define fl (get-sym stk-ptr))
       (set! stk-ptr (+ 1 stk-ptr))
       (add-inst (list 'label top) acc)
       (define bs (eval-bexp bexp))
       (add-inst (list 'branch bs tl) acc)
       (add-inst (list 'jump fl) acc)
       (add-inst (list 'label tl) acc)
       (compile-h bdy)
       (add-inst (list 'jump top) acc)
       (add-inst (list 'label fl) acc)]
      [`(seq ...)
       (compile-h (rest (first prog)))])])
    (compile-h (rest prog))]
    ))
  (compile-h prog)
  (append (unbox acc) '((halt)) (unbox mem))
  )
(compile program)
)
       
       
       
  


























             
       
       
