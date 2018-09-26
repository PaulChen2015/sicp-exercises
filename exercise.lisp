;;(/ (+ 6 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2 ) (- 2 7)))

(defun my-max(x y)
    (if (> x y) x y))

(defun my-min(x y)
    (if (> x y) y x))

(defun sum-of-squares-of-two-larger(x y z)
    (sum-of-squares (my-max x y) (my-max (my-min x y) z)))

(defun new-if(predicate then-clause else-clause)
    (cond (predicate then-clause)
        (t else-clause)))

;;infinit loop, do not call
;(defun sqrt-iteri(guess x)
    ;(new-if (good-enough? guess x)
            ;guess
            ;(sqrt-iteri (improve guess x) x)))

(defun cubert-improve(guess x)
    (/ (+ (/ x (square guess)) (+ 2 guess))
        3))

(defun good-cubert?(guess x)
    (< (abs (- (cube guess) x)) 0.001))


(defun cubert(guess x)
    (format t "~f~10t" guess)
    (if (good-cubert? guess x)
        guess
        (cubert (cubert-improve guess x) x)))

(defun Ackermann(x y)
    (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (t (Ackermann (- x 1) (Ackermann x (- y 1))))))

(defun fib-plus(n)
    (if (< n 3)
        n
        (+ (fib-plus (- n 1)) (* 2 (fib-plus (- n 2))) (* 3 (fib-plus (- n 3))))))

(defun tail-fib-plus(n)
    (defun iter(a b c counter)
       (if (= counter 2)
            c
            (iter b c (+ (* 3 a) (* 2 b) c) (- counter 1)))) 
    (if (< n 3)
        n
        (iter 0 1 2 n)))

(defun pascal-triangle(n index)
    (if (or (= n 1) (= 1 index) (= n index))
        1
        (+ (pascal-triangle (- n 1) (- index 1)) (pascal-triangle (- n 1) index))))


(defun next-divisor(d)
    (if (= d 2)
        (+ d 1)
        (+ d 2)))

(defun tail-sum(term a next b)
    (defun iter(a result)
        (if (> a b)
            result
            (iter (funcall next a) (+ result (funcall term a)))))
    (iter a 0))

(defun integral(f a b dx)
    (defun add-dx(x)
        (+ x dx))
    (* (tail-sum f (+ a (/ dx 2.0)) #'add-dx b)))

(defun pi-sum(a b)
    (defun pi-term(x)
        (/ 1.0 (* x (+ x 2))))
    (defun pi-next(x)
        (+ x 4))
    (tail-sum #'pi-term a #'pi-next b))

(defun product(term a next b)
    (if (> a b)
        1
        (* (funcall term a)
            (product term (funcall next a) next b))))

(defun tail-product(term a next b)
    (defun iter(a result)
        (if (> a b)
            result
            (iter (funcall next a) (* result (funcall term a)))))
    (iter a 1))

(defun factorial-ex(a b)
    (product #'identity a #'inc b))

(defun pi-product(a b)
    (defun pi-term(x)
        (/ (* (- x 1) (+ x 1))
            (* x x)))
    (defun pi-next(x)
        (+ x 2.0))
    (tail-product #'pi-term a #'pi-next b))

(defun accumulate-e(combiner null-value term a next b)
    (if (> a b)
        null-value
        (funcall combiner (funcall term a)
                        (accumulate-e combiner null-value term (funcall next a) next b))))

(defun tail-accumulate(combiner null-value term a next b)
    (defun iter(a result)
        (if (> a b)
            result
            (iter (funcall next a) (funcall combiner result (funcall term a)))))
    (iter a null-value))

(defun filtered-accumulate(combiner null-value filter term a next b)
    (defun iter(a result)
        (cond ((> a b) result)
            ((funcall filter a) (iter (funcall next a) (funcall combiner result (funcall term a))))
            (t (iter (funcall next a) result))))
    (iter a null-value))

(defun squares-of-prime(a b)
    (filtered-accumulate #'+ 0 #'prime? #'square a #'inc b))

(defun product-relative-prime(i n)
    (defun predict(x)
        (and (> x 0) (= 1 (gcd x n))))
    (filtered-accumulate #'* 1 #'predict #'identity i #'inc n))

(defun f-ex(g)
    (funcall g 2))


(defun exp-x-test(guess)
    (defun f(g)
        (/ (log 1000)
            (log g)))
    (fixed-point #'f guess))

(defun cont-frac (n d k)
    (defun iter(r i)
        (let ((nv (funcall n i))
                (dv (funcall d i)))
            (if (= i k)
                r
                (iter 
                    (/ nv 
                       (+ dv r))
                    (+ i 1)))))
    (iter 0.0 1))

(defun d-n-one(k)
    (cont-frac #'(lambda (x) (/ x x))
                #'(lambda (x) (/ x x))
                k))


(defun euler-exp(n k)
    (defun d(i)
        (let ((r (rem i 3)))
            (if (= r 2)
                (/ (+ i 1) 1.5)
                1)))
    (cont-frac #'(lambda (x) (* n (/ x x))) #'d k))

(defun tan-cf(x k)
    (cont-frac #'(lambda (i) (if (= i 1) x (- (* x x))))
                    #'(lambda (i) (- (* i 2) 1))
                k))


(defun compose(f g)
    (lambda (x) (funcall f (funcall g x))))

(defun repeated(f n)
    (defun iter(r index)
        (if (= index 1)
            r
            (iter (compose f r) (- index 1))))
    (iter f n))

(defun smooth(f)
    (let ((dx 0.001))
        (lambda (x) (/ (+ (funcall f (- x dx))
                            (funcall f x)
                            (funcall f (+ x dx)))
                        3))))

(defun n-smooth(f n)
    (repeated (smooth f) n))

(defun make-ral-enhanced(n d)
    (let ((pn (abs n))
            (pd (abs d))
            (g (gcd n d)))
        (if (or (and (> n 0) (> d 0)) (and (< n 0) (< d 0)))
            (cons (/ pn g) (/ pd g))
            (cons (/ (- pn) g) (/ pd g)))))

(defun make-point(x y) (cons x y))
(defun x-point(p) (car p))
(defun y-point(p) (cdr p))

(defun make-segment(s e) (cons s e))
(defun start-segment(s) (car s))
(defun end-segment(s) (cdr s))

(defun midpoint-segment(s)
    (make-point (/ (+ (x-point (start-segment s)) (x-point (end-segment s)))
                    2.0)
                (/ (+ (y-point (start-segment s)) (y-point (end-segment s)))
                    2.0)))

(defun make-interval(a b) (cons a b))
(defun upper-bound(i) (car i))

(defun lower-bound(i) (cdr i))

(defun add-interval(x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                    (+ (upper-bound x) (upper-bound y))))

(defun mul-interval(x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
       (make-interval (min p1 p2 p3 p4)
                        (max p1 p2 p3 p4)))) 

(defun div-interval(x y)
    (mul-interval
        x
        (make-interval (/ 1.0 (upper-bound y))
                        (/ 1.0 (lower-bound y)))))

(defun last-pair(ls)
    (if (null (cdr ls))
        (car ls)
        (last-pair (cdr ls))))

(defun my-reverse(ls)
    (defun iter(result ls)
        (if (null ls)
            result
            (iter (cons (car ls) result) (cdr ls))))
    (iter nil ls))

(defun same-parity(x &rest y)
    (defun same-parity?(a)
        (= (rem x 2) (rem a 2)))

    (defun iter(result ls)
        (cond ((null ls) result)
                ((same-parity? (car ls)) (iter (cons result (car ls)) (cdr ls)))
                (t (cons result (iter result (cdr ls))))))
    (iter nil y)) 

(defun same-oddeven?(a b)
    (= (rem a 2) (rem b 2)))

(defun same-parity-rec(x &rest y)
    (defun iter (a ls)
        (cond ((null ls) a)
            ((same-oddeven? a (car ls)) (iter (cons a (car ls)) (cdr ls)))
            (t (iter a (cdr ls))))
        (iter x y)))

(defun square-list1 (items)
    (if (null items)
        nil
        (cons (square (car items)) (square-list1 (cdr items)))))

(defun square-list2 (items)
    (my-map #'square items))

(defun square-list-tail (items)
    (defun iter (result ls)
        (if (null ls)
            result
            (iter (append result (list (square (car ls)))) (cdr ls))))
    (iter nil items))


(defun for-each (proc ls)
    (cond ((null ls) t)
        (t (funcall proc (car ls))
            (for-each proc (cdr ls)))))

(defun display (ls)
    (for-each (lambda (x) (format t "~d~%" x)) ls))


(defun deep-reverse(ls)
    (if (listp ls)
        (reverse (my-map #'deep-reverse ls))
        ls))

(defun fringe(ls)
    (cond ((null ls) nil)
            ((listp (car ls)) (append (fringe (car ls)) (fringe (cdr ls))))
            (t (cons (car ls) (fringe (cdr ls))))))


;;2.29
(defun make-mobile(left right) (list left right))
(defun make-branch(len struct) (list len struct))
(defun left-branch(mobile) (car mobile))
(defun right-branch(mobile) (cadr mobile))
(defun branch-length(branch) (car branch))
(defun branch-structure(branch) (cadr branch))

(defun total-weight(mobile)
    (cond ((null mobile) 0)
            ((not (listp mobile)) mobile)
            (t (+ (total-weight (branch-structure (left-branch mobile)))
                    (total-weight (branch-structure (right-branch mobile)))))))
            

(defun topque (branch)
    (* (branch-length branch) (total-weight (branch-structure branch))))

(defun balanced-mobile? (mobile)
    (if (not (listp mobile))
        t
        (and (= (topque (left-branch mobile)) (topque (right-branch mobile)))
            (balanced-mobile? (branch-structure (left-branch mobile)))
            (balanced-mobile? (branch-structure (right-branch mobile))))))

;; 2.30
(defun square-tree (tree)
    (cond ((null tree) nil)
        ((not (listp tree)) (square tree))
        (t (cons (square-tree (car tree))
                (square-tree (cdr tree))))))

(defun square-tree-r (tree)
    (my-map (lambda (sub-tree)
                (if (listp sub-tree)
                    (square-tree-r sub-tree)
                    (square sub-tree)))
            tree))

;; 2.31
(defun tree-map (proc tree)
    (my-map (lambda (sub-tree)
                (if (listp sub-tree)
                    (tree-map proc sub-tree)
                    (funcall proc sub-tree)))
            tree))

(defun square-tree-tm (tree)
    (tree-map #'square tree))

;; 2.32
(defun subsets (s)
    (if (null s)
        (list nil)
        (let ((rests (subsets (cdr s))))
            (append rests (my-map (lambda (x) (cons (car s) x)) rests)))))

;; 2.33
(defun map-e (p seq)
    (accumulate (lambda (x y) (cons (funcall p x) y)) nil seq))

(defun append-e (seq1 seq2)
    (accumulate #'cons seq2 seq1))

;;(defun length-e (seq)
    ;;(accumulate (lambda (x y) (+ 1 y)) 0 seq))

;; 2.34
(defun horner-eval (x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-term) (+ this-coeff (* x higher-term))) 0 coefficient-sequence))

;; 2.35
;;(defun count-leaves-e (tree)
    ;;(accumulate (lambda (x y) (+ 1 y)) 0 (enumerate-tree tree)))

;; 2.36
(defun accumulate-n (op init seqs)
    (if (null (car seqs))
        nil
        (cons (accumulate op init (my-map #'car seqs))
                (accumulate-n op init (my-map #'cdr seqs)))))

;; 2.37
(defun map-e (proc ls1 ls2)
    (if (or (null ls1) (null ls2))
        nil
        (cons (funcall proc (car ls1) (car ls2))
                (map-e proc (cdr ls1) (cdr ls2)))))

(defun dot-product (v w)
    (accumulate #'+ 0 (map-e #'* v w)))

(defun matrix-*-vector (m v)
    (my-map (lambda (x) (dot-product x v)) m))

(defun transpose (mat)
    (accumulate-n #'cons nil mat))

(defun matrix-*-matrix (m n)
    (let ((cols (transpose n)))
        (my-map (lambda (row) (matrix-*-vector cols row)) m)))

;; 2.38
(defun fold-left (op init seq)
    (defun iter (result rests)
        (if (null rests)
            result
            (iter (funcall op result (car rests))
                    (cdr rests))))
    (iter init seq))

(defun fold-right (op init seq)
    (accumulate op init seq))

;; 2.39
(defun reverse-fl (seq)
    (fold-left (lambda (x y) (cons y x)) nil seq))

(defun reverse-fr (seq)
    (fold-right (lambda (x y) (append y (list x))) nil seq))

;; 2.41
(defun make-triples (n)
    (flatmap (lambda (i)
               (flatmap (lambda (j)
                            (my-map (lambda (k)
                                        (list k j i))
                                    (enumerate-interval 1 (- j 1))))
                        (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))

(defun unique-triples-sum (n s)
    (filter (lambda (ls) (= (accumulate #'+ 0 ls) s)) (make-triples n)))

;; 2.40
(defun unique-pairs (n)
    (flatmap (lambda (i)
                (my-map (lambda (j)
                            (list i j))
                        (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))

(defun prime-sum-pairs-e (n)
    (my-map #'make-pair-sum
            (filter #'prime-sum? (unique-pairs n))))

;; 2.42
;;(defun safe? (k positions)
    ;;t)

(defun adjoin-position (row col rests)
    (cons (list row col) rests))

;;(defun queens (board-size)
 ;;   (defun queen-cols (k)
  ;;      (if (= k 0)
   ;;         (list nil)
    ;;        (filter
     ;;           (lambda (positions) (safe? k positions))
      ;;          (flatmap
       ;;             (lambda (rest-of-queens)
        ;;                (my-map (lambda (new-row)
         ;;                           (adjoin-position
          ;;                              new-row k rest-of-queens))
           ;;                     (enumerate-interval 1 board-size)))
            ;;        (queen-cols (- k 1))))))
    ;;(queen-cols board-size))
