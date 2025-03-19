#lang racket

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
(define acc empty)
;;assume we have a global accumulative data storage mem
(define mem empty)
;;get-sym returns the pysmbol with stk-ptr
(define (get-sym ptr)
  (+ '_' (number->symbol ptr)))

(define (add-inst inst s)
    (set! s (append s (list inst))))

;;(eval-aexp) will return a pysymbol representing the results of evaluation.
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



























             
       
       


