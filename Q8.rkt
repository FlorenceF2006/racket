#lang racket
;;Florence Fan, Fee Chen

(define (primplify input)
  (struct sub (name def) #:transparent)
  (struct notfound () #:transparent)
  (define (not-found? strct)
    (match strct
      [(notfound) #t]
      [_ #f]))
  (define (imm? val)
    (or (number? val) (boolean? val)))
  (define (dup? tb nm)
    (cond[(empty? tb) #f]
         [(equal? nm (sub-name (first tb))) #t]
         [else (dup? (rest tb) nm)]))
  (define (duplicate? tb1 tb2 tb3 nm)
    (or (dup? tb1 nm) (dup? tb2 nm ) (dup? tb3 nm)))
  (define table-const empty)
  (define table-data empty)
  (define table-label empty)
  (define stk-ctr 0)
  (define (subst lst)
    (cond[(empty? lst) empty]
         [else
          (define inst (first lst))
          (match inst
            ;label and const will not increase the stack pointer
            [`(label ,name)
             (if (duplicate? table-label table-const table-data name)
                 (error "duplicate") void)
             (set! table-label (cons (sub name stk-ctr) table-label))
             (subst (rest lst))]
            [`(const ,name ,def)
             (if (duplicate? table-const table-label table-data name)
                 (error "duplicate") void)
             (set! table-const (cons (sub name def) table-const))
             (subst (rest lst))]
            ;(data name (n val)) will increase the stack pointer by n.
            [`(data ,name ( ,n ,val))
             (if (duplicate? table-data table-const table-label name)
                 (error "duplicate") void)
             (set! table-data (cons (sub name stk-ctr) table-data))
             (set! stk-ctr (+ stk-ctr n))
             (define res (subst (rest lst)))
             ;pad n vals in front of resulting lst
             (define (loop n val)
               (cond[(equal? n 0) res]
                    [else
                     (set! res (cons val res))
                     (loop (- n 1) val)
                     ]))
             (loop n val)
             ]
            ;data name def with one imm or psymbol)
            [`(data ,name ,def)
             (if (duplicate? table-label table-const table-data name)
                 (error "duplicate") void)
             (set! table-data (cons (sub name stk-ctr) table-data))
             (set! stk-ctr (+ 1 stk-ctr))
             (cons def (subst (rest lst)))]
            ;halt
            [`(halt)
             (set! stk-ctr (+ 1 stk-ctr))
             (cons 0 (subst (rest lst)))]
            [_
             ;if it is data with array def
             (cond[(and (list? inst)
                        (not (empty? inst))
                        (not (empty? (rest inst)))
                        (equal? 'data (first inst)))
                   ;have a loop that can pad all the values at the front of res
                   ;increase stk-ctr by the length of the array
                   ;update table-data
                   (define name (second inst))
                   (set! table-data (cons (sub name stk-ctr) table-data))
                   (set! stk-ctr (+ stk-ctr (length (rest (rest inst)))))
                   (define res (subst (rest lst)))
                   (append (rest (rest inst)) res)
                   ]
                  [else (set! stk-ctr (+ stk-ctr 1))
                        (cons inst (subst (rest lst)))])])]))
  (define memory (list->vector (subst input)))
  ;;after this step, memory contains all the things we need.
  ;;next, we need to take a look at the psymbol used in definition of const,data and label.
  ;;observing the outcome, circular definition of const should be immediately yeild as error.
  ;;so we should take a look at const first. i.e. the table-const
  ;;substitute the constant number with data
  ;;we should abstract the search function to make code more readable.
  (define (lk-no-error tb def)
    (cond[(empty? tb) (notfound)]
         [(equal? (sub-name (first tb)) def) (sub-def (first tb))]
         [else (lk-no-error (rest tb) def)]))
  ; table, def, and error
  ; need to test for the cases where the const is defined by data
  (define (lookup tb def)
    (define ctr 0)
    (define ntb tb)
    (define n (length tb))
    (define (lookup-h tb def)
      (cond[(empty? tb) (error "undefined")]
           [(equal? ctr n) (error "circular definition")]
           [(equal? (sub-name (first tb)) def)
            (set! ctr (+ ctr 1))
            (if (imm? (sub-def (first tb)))
                (sub-def (first tb))
                (lookup-h ntb (sub-def (first tb))))]
           [else (lookup-h (rest tb) def)]))
    (lookup-h tb def))
  ;;substitute all the entries of const table with imm values
  (define (sub-const tb)
    (cond[(empty? tb) empty]
         [(match (first tb)
            [(sub name def)
             (define val def)
             (cond[(imm? def) void]
                  [else
                   (define res (lk-no-error table-data def))
                   (define lb (lk-no-error table-label def))
                   (if (and (not-found? res) (not-found? lb))
                       (set! val (lookup table-const def))
                       (set! val (if (not-found? res) lb res)))])
             (cons (sub name val) (sub-const (rest tb)))])]))

  (set! table-const (sub-const table-const))
  ;;search in both tables
    
  (define (new-search nm keyword)
    (define dt (lk-no-error table-data nm))
    (define cst (lk-no-error table-const nm))
    (define lb (lk-no-error table-label nm))
    (cond[(not (not-found? dt))
          (list dt)]
         [(not (not-found? cst))
          (if (equal? keyword "dest")
          (error "incorrect")
          (if (equal? keyword "label")
              (list cst) cst
          ))]
         [(not (not-found? lb)) (if (equal? keyword "dest")
                                    (error "incorrect") lb)]
         [else (if (equal? keyword "label")
                   (error "incorrect") (error "undefined"))
               ]))
  (define (to-imm exp)
    (if (imm? exp) exp (first exp)))
  
  (define (find exp keyword)
    (match exp
      [`(,x ,y)
       (if (or (imm? x) (list? x)) void (set! x (to-imm (new-search x keyword))))
       (if (or (imm? y) (list? y)) void (set! y (new-search y keyword)))
       (list x y)]
      [x
       (if (or (imm? x) (list? x)) void (set! x (new-search x keyword)))
       x]))

    
  
  ;;now we may want to translate all the variables
  ;;need to modify the t-dest function
  ;;a new translate function is needed.
  (define (new-translate vec)
    (define (loop i)
      (cond[(>= i (vector-length vec)) void]
           [else
            (define inst (vector-ref vec i))
            (match inst
              [`(,bin ,dest ,opd1 ,opd2)
               (set! dest (find dest "dest"))
               (set! opd1 (find opd1 ""))
               (set! opd2 (find opd2 ""))
               (vector-set! vec i (list bin dest opd1 opd2))]
              [`(branch ,opd1 ,opd2)
               (set! opd1 (find opd1 ""))
               (set! opd2 (find opd2 "label"))
               (vector-set! vec i (list 'branch opd1 opd2))]
              [`(,op ,dest ,opd)
               (set! dest (find dest "dest"))
               (set! opd (find opd ""))
               (vector-set! vec i (list op dest opd))]
              [`(jump ,opd)
               (set! opd (find opd "label"))
               (vector-set! vec i (list 'jump opd))]
              [`(print-string ,str) void]
              [`(, op ,opd)
               (set! opd (find opd ""))
               (vector-set! vec i (list op opd))]
              [x (vector-set! vec i  (to-imm (find x "")))
                 ])
            (loop (+ i 1))
            ]))
    (loop 0)
    vec
    )
  (vector->list (new-translate memory)))


              





       
  
   
    
                   






  
  



    







    
