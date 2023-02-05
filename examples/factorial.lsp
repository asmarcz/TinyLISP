(define (fact n)
  (if (= n 0)
    1
    (* n (fact (- n 1)))))

(define (for-rec i n fun)
  (if (= i n)
    (fun i)
    (cons (fun i) (for-rec (+ i 1) n fun))))

(define (for n fun)
  (for-rec 0 n fun))

(for 5 fact)
