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
;;            (filter
;;                (lambda (positions) (safe? k positions))
;;               (flatmap
;;                  (lambda (rest-of-queens)
;;                        (my-map (lambda (new-row)
;;                                   (adjoin-position
;;                                        new-row k rest-of-queens))
;;                               (enumerate-interval 1 board-size)))
;;                    (queen-cols (- k 1))))))
;;    (queen-cols board-size))


;; 2.54
(defun equals? (ls1 ls2)
    (if (and (null ls1) (null ls2))
        t
        (let ((v1 (car ls1))
                (v2 (car ls2)))
            (if (and (listp v1) (listp v2))
                (and (equals? v1 v2) (equals? (cdr ls1) (cdr ls2)))
                (and (eq v1 v2) (equals? (cdr ls1) (cdr ls2)))))))


;; 2.56 2.57
(defun base (s) (cadr s))
(defun exponent (s) (caddr s))
(defun exponentiation? (x) (and (listp x) (eq (car x) '**)))
(defun make-exponentiation (base exponent)
    (cond ((=number? exponent 0) 1)
            ((=number? exponent 1) base)
            (t (list '** base exponent))))

(defun augend-e (s)
    (if (> (length (cddr s)) 1)
        (cons '+ (cddr s))
        (caddr s)))

(defun multiplicand-e (p)
    (if (> (length (cddr p)) 1)
        (cons '* (cddr p))
        (caddr p)))

(defun deriv-e (exp var)
    (cond ((numberp exp) 0)
            ((variable? exp) (if (same-variable? exp var) 1 0))
            ((sum? exp) (make-sum (deriv-e (addend exp) var)
                                    (deriv-e (augend-e exp) var)))
            ((product? exp)
                (make-sum
                    (make-product (multiplier exp)
                                    (deriv-e (multiplicand-e exp) var))
                    (make-product (deriv-e (multiplier exp) var)
                                    (multiplicand exp))))
            ((exponentiation? exp)
                (make-product 
                    (exponent exp)
                    (make-product
                        (make-exponentiation (base exp)
                                                (- (exponent exp) 1))
                        (deriv-e (base exp) var))))
            (t (error "unknown expression type: DERIV ~a" exp))))

;; 2.58
(defun sum-infix? (x) (and (listp x) (eq (cadr x) '+)))
(defun addend-infix (s) (car s))
(defun augend-infix (s) (caddr s))
(defun product-infix? (x) (and (listp x) (eq (cadr x) '*)))
(defun multiplier-infix (p) (car p))
(defun multiplicand-infix (p) (caddr p))

(defun make-product-infix (m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (numberp m1) (numberp m2)) (* m1 m2))
            (t (list m1 '* m2))))

(defun make-sum-infix (a1 a2)
    (cond ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (numberp a1) (numberp a2))
                (+ a1 a2))
            (t (list a1 '+ a2))))

(defun deriv-infix (exp var)
    (cond ((numberp exp) 0)
            ((variable? exp) (if (same-variable? exp var) 1 0))
            ((sum-infix? exp) (make-sum-infix (deriv-infix (addend-infix exp) var)
                                    (deriv-infix (augend-infix exp) var)))
            ((product-infix? exp)
                (make-sum-infix
                    (make-product-infix (multiplier-infix exp)
                                    (deriv-infix (multiplicand-infix exp) var))
                    (make-product-infix (deriv-infix (multiplier-infix exp) var)
                                    (multiplicand-infix exp))))
            (t (error "unknown expression type: DERIV ~a" exp))))

;; 2.59
(defun union-set (s1 s2)
    (cond ((null s1) s2)
            ((null s2) s1)
            ((element-of-set? (car s1) s2) (union-set (cdr s1) s2))
            (t (cons (car s1) (union-set (cdr s1) s2)))))

;; 2.61
(defun adjoin-set-sorted (x set)
    (cond ((null set) (list x))
            ((= x (car set)) set)
            ((< x (car set)) (cons x set))
            (t (cons (car set) (adjoin-set-sorted x (cdr set))))))

;; 2.62
(defun union-set-sorted (s1 s2)
    (cond ((null s1) s2)
            ((null s2) s1)
            (t
                (let ((x1 (car s1)) (x2 (car s2)))
                    (cond ((= x1 x2) (cons x1 (union-set-sorted (cdr s1) (cdr s2))))
                            ((< x1 x2) (cons x1 (union-set-sorted (cdr s1) s2)))
                            (t (cons x2 (union-set-sorted s1 (cdr s2)))))))))
                    

;; 2.63
(defun tree->list-1 (tree)
    (if (null tree)
        nil
        (append (tree->list-1 (left-tree tree))
                (cons (entry tree) (tree->list-1 (right-tree tree))))))

(defun tree->list-2 (tree)
    (defun copy-to-list (tree result)
        (if (null tree)
            result
            (copy-to-list (left-tree tree)
                            (cons (entry tree)
                                    (copy-to-list (right-tree tree) result)))))
    (copy-to-list tree nil))

;; 2.64
(defun quotient (a b) (floor (/ a b)))

(defun partial-tree (elts n)
    (if (= n 0)
        (cons nil elts)
        (let ((left-size (quotient (- n 1) 2)))
            (let ((left-result (partial-tree elts left-size)))
                (let ((left-tree (car left-result))
                        (non-left-elts (cdr left-result))
                        (right-size (- n (+ left-size 1))))
                    (let ((this-entry (car non-left-elts))
                            (right-result (partial-tree (cdr non-left-elts) right-size)))
                        (let ((right-tree (car right-result))
                                (remaining-elts (cdr right-result)))
                            (cons (make-tree this-entry left-tree right-tree)
                                    remaining-elts))))))))

(defun list->tree (ls)
    (car (partial-tree ls (length ls))))


;; 2.65
(defun union-treeset (s1 s2)
    (list->tree
        (union-set-sorted
            (tree->list-2 s1)
            (tree->list-2 s2))))


(defun intersection-treeset (s1 s2)
    (list->tree
        (intersection-set-sorted
            (tree->list-2 s1)
            (tree->list-2 s2))))

;; 2.66
(defun lookup-btree (given-key set-of-records)
    (cond ((null set-of-records) nil)
            ((= given-key (caar (entry set-of-records))) t)
            ((< given-key (caar (entry set-of-records)))
                (lookup-btree given-key (left-tree set-of-records)))
            (t (lookup-btree given-key (right-tree set-of-records)))))
            
;; 2.68
(defun contain-symbol (symbol tree)
    (element-of-set? symbol (symbols tree)))

(defun encode-symbol (symbol tree)
    (if (leaf? tree)
        nil
        (let ((lt (left-ht tree))
                (rt (right-ht tree)))
            (cond ((contain-symbol symbol lt) (cons 0 (encode-symbol symbol lt)))
                ((contain-symbol symbol rt) (cons 1 (encode-symbol symbol rt)))
            (t (error "symbol dose not exists: ENCODE-SYMBOL ~a" symbol))))))


(defun encode (message tree)
    (if (null message)
        nil
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

;; 2.69
;; incorrect. such as ((A 3) (B 5) (C 6) (D 6))
;;(defun successive-merge (leaf-set)
;;    (defun iter (result set)
;;        (if (null set)
;;            result
;;            (iter
;;                (make-code-tree
;;                    result
;;                    (car set))
;;                (cdr set))))
;;    (iter (car leaf-set) (cdr leaf-set)))

(defun successive-merge (leaf-set)
    (if (< (length leaf-set) 2)
        (car leaf-set)
        (successive-merge
            (adjoin-ht-set
                (make-code-tree
                    (car leaf-set)
                    (cadr leaf-set))
                (cddr leaf-set)))))

(defun generate-huffman-tree (pairs)
    (successive-merge (make-leaf-set pairs)))

;; 2.70
;;(defvar song-tree 
;;    (generate-huffman-tree
;;        '((a 2) (get 2) (sha 3) (wah 1) (boom 1) (job 2) (na 16) (yip 9))))
;;
;;(defvar bit-song
;;    (encode '(Get a job Sha na  na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom) song-tree))
