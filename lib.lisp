(defun square(x) 
    (* x x))

(defun sum-of-squares(x y)
    (+ (square x) (square y)))

(defun myabs(x)
    (cond ((< x 0) (- x))
          (t x)))

(defun average(x y)
    (/ (+ x y) 2))

(defun my-sqrt(x)
    (defun improve(guess)
        (average guess (/ x guess)))
    (defun good-enough?(guess)
        (< (abs (- (square guess) x)) 0.001))
    (defun sqrt-iter(guess)
        (format t "~f~10t~%" guess)
        (if (good-enough? guess)
            guess
            (sqrt-iter (improve guess))))
    (sqrt-iter (/ x 2.0)))

(defun cube(x)
    (* x x x))

(defun factorial(n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))

(defun tail-factorial(n)
    (defun iter(product counter)
        (if (> counter n)
            product
            (iter (* counter product)
                    (+ counter 1))))
    (iter 1 1))

(defun fib(n)
    (cond ((= n 1) 1)
        ((= n 2) 1)
        (t (+ (fib (- n 1)) (fib (- n 2))))))

(defun tail-fib(n)
    (defun iter(fst snd counter)
        (if (< counter 1)
            snd
            (iter snd (+ fst snd) (- counter 1))))
    (cond ((= n 1) 1)
        ((= n 2) 1)
        (t (iter 1 1 n))))


(defun remainder(n d)
    (if (< (/ (abs n) d) 1)
        n
        (remainder (* (/ (abs n) n)
                        (- (abs n) d))
                    d)))

(defun even?(n)
    (= (remainder n 2) 0))

(defun my-gcd(a b)
    (if (= b 0)
        a
        (my-gcd b (rem a b))))


(defun divides?(a b)
    (= (rem b a) 0))

(defun find-divisor(n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (t (find-divisor n (+ test-divisor 1)))))

(defun smallest-divisor(n)
    (find-divisor n 2))


(defun sum(term a next b)
    (if (> a b)
        0
        (+ (funcall term a)
            (sum term (funcall next a) next b))))

(defun inc(x)
    (+ x 1))

(defun prime?(n)
    (= n (smallest-divisor n)))

(defun fixed-point(f first-guess)
    (defun close-enough?(v1 v2)
        (< (abs (- v1 v2))
            0.00001))
    (defun try(guess)
        (format t "guess=~f~%" guess)
        (let ((next (funcall f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(defun average-damp(f)
    (lambda (x) (average x (funcall f x))))

(defun cube-root(x)
    (fixed-point (average-damp #'(lambda (y) (/ x (square y))))
            1.0))

(defun deriv(g)
    (let ((dx 0.00001))
        (lambda (x) (/ (- (funcall g (+ x dx)) (funcall g x))
                        dx))))

(defun newtons-method(g guess)
    (defun newton-transform(f)
        (lambda (x) (- x (/ (funcall f x) (funcall (deriv f) x)))))
    (let ((trans (newton-transform g)))
        (fixed-point trans guess)))

(defun newton-sqrt(x)
    (newtons-method
        (lambda (y) (- (square y) x)) 1.0))

(defun make-rat-v1(n d)
    (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))

(defun numer(x) (car x))
(defun denom(x) (cdr x))

(defun my-append(l1 l2)
    (if (null l1)
        l2
        (cons (car l1) (my-append (cdr l1) l2))))

(defun my-map (proc items)
    (if (null items)
        nil
        (cons (funcall proc (car items)) (my-map proc (cdr items)))))

(defun my-length (ls)
    (if (null ls)
        0
        (+ 1 (my-length (cdr ls)))))

(defun count-leaves (ls)
    (cond ((null ls) 0)
        ((not (listp ls)) 1)
        (t (+ (count-leaves (car ls))
                (count-leaves (cdr ls))))))

(defun scale-tree (tree factor)
    (my-map (lambda (sub-tree)
                (if (listp sub-tree)
                    (scale-tree sub-tree factor)
                    (* sub-tree factor)))
            tree))

(defun odd? (n)
    (not (even? n)))

(defun accumulate (op initial seq)
    (if (null seq)
        initial
        (funcall op (car seq)
            (accumulate op initial (cdr seq)))))

(defun filter (predicate seq)
    (cond ((null seq) nil)
            ((funcall predicate (car seq))
                (cons (car seq)
                        (filter predicate (cdr seq))))
            (t (filter predicate (cdr seq)))))

(defun enumerate-tree (tree)
    (cond ((null tree) nil)
            ((not (listp tree)) (list tree))
            (t (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree))))))

(defun enumerate-interval (low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

(defun flatmap (proc seq)
    (accumulate #'append nil (my-map proc seq)))

(defun prime-sum? (pair)
    (prime? (+ (car pair) (cadr pair))))

(defun make-pair-sum (pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(defun prime-sum-pairs (n)
    (my-map #'make-pair-sum
            (filter #'prime-sum? (flatmap
                                (lambda (i)
                                    (my-map (lambda (j) (list i j))
                                        (enumerate-interval 1 (- i 1))))
                                (enumerate-interval 1 n)))))

(defun permutations (seq)
    (if (null seq)
        (list nil)
        (flatmap (lambda (x)
                    (my-map (lambda (p) (cons x p))
                        (permutations (remove x seq))))
                seq)))

(defun memq (item x)
    (cond ((null x) nil)
            ((eq item (car x)) x)
            (t (memq item (cdr x)))))

(defun =number? (exp num)
    (and (numberp exp) (= exp num)))
(defun variable? (x) (symbolp x))
(defun same-variable? (v1 v2)
    (and (variable? v1) (variable? v2) (eq v1 v2)))
(defun sum? (x) (and (listp x) (eq (car x) '+)))
(defun addend (s) (cadr s))
(defun augend (s) (caddr s))
(defun product? (x) (and (listp x) (eq (car x) '*)))
(defun multiplier (p) (cadr p))
(defun multiplicand (p) (caddr p))

(defun make-sum (a1 a2)
    (cond ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (numberp a1) (numberp a2))
                (+ a1 a2))
            (t (list '+ a1 a2))))

(defun make-product (m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (numberp m1) (numberp m2)) (* m1 m2))
            (t (list '* m1 m2))))

(defun deriv (exp var)
    (cond ((numberp exp) 0)
            ((variable? exp) (if (same-variable? exp var) 1 0))
            ((sum? exp) (make-sum (deriv (addend exp) var)
                                    (deriv (augend exp) var)))
            ((product? exp)
                (make-sum
                    (make-product (multiplier exp)
                                    (deriv (multiplicand exp) var))
                    (make-product (deriv (multiplier exp) var)
                                    (multiplicand exp))))
            (t (error "unknown expression type: DERIV ~a" exp))))

(defun element-of-set? (x set)
    (cond ((null set) nil)
            ((equal x (car set)) t)
            (t (element-of-set? x (cdr set)))))

(defun adjoin-set (x set)
    (if (element-of-set? x set)
        set
        (cons x set)))

(defun intersection-set (s1 s2)
    (cond ((or (null s1) (null s2)) nil)
            ((element-of-set? (car s1) s2)
                (cons (car s1) (intersection-set (cdr s1) s2)))
            (t (intersection-set (cdr s1) s2))))

(defun intersection-set-sorted (s1 s2)
    (if (or (null s1) (null s2))
        nil
        (let ((x1 (car s1)) (x2 (car s2)))
            (cond ((= x1 x2)
                    (cons x1 (intersection-set-sorted (cdr s1) (cdr s2))))
                ((< x1 x2)
                    (intersection-set-sorted (cdr s1) s2))
                (t (intersection-set-sorted s1 (cdr s2)))))))

(defun entry (tree) (car tree))
(defun left-tree (tree) (cadr tree))
(defun right-tree (tree) (caddr tree))
(defun make-tree (entry left right) (list entry left right))

(defun element-of-treeset (x set)
    (cond ((null set) nil)
            ((= x (entry set)) t)
            ((< x (entry set))
                (element-of-treeset x (left-tree set)))
            (t (element-of-treeset x (right-tree set)))))

(defun adjoin-treeset (x set)
    (cond ((null set) (make-tree x nil nil))
            ((= x (entry set)) set)
            ((< x (entry set))
                (make-tree (entry set)
                            (adjoin-treeset x (left-tree set))
                            (right-tree set)))
            (t (make-tree (entry set)
                            (left-tree set)
                            (adjoin-treeset x (right-tree set))))))

(defun make-leaf (symbol weight) (list 'leaf symbol weight))
(defun leaf? (object) (eq (car object) 'leaf))
(defun symbol-leaf (x) (cadr x))
(defun weight-leaf (x) (caddr x))

(defun symbols (tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

(defun weight (tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

(defun left-ht (tree) (car tree))
(defun right-ht (tree) (cadr tree))

(defun make-code-tree (left right)
    (list left
            right
            (append (symbols left) (symbols right))
            (+ (weight left) (weight right))))

(defun choose-branch (bit branch)
    (cond ((= bit 0) (left-ht branch))
            ((= bit 1) (right-ht branch))
            (t (error "bad bit: CHOOSE-BRANCH ~a" bit))))

(defun decode (bits tree)
    (defun decode-1 (bits current-branch)
        (if (null bits)
            nil
            (let ((next-branch (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

(defun adjoin-ht-set (x set)
    (cond ((null set) (list x))
            ((< (weight x) (weight (car set))) (cons x set))
            (t (cons (car set) (adjoin-ht-set x (cdr set))))))

(defun make-leaf-set (pairs)
    (if (null pairs)
        nil
        (let ((pair (car pairs)))
            (adjoin-ht-set (make-leaf (car pair) (cadr pair))
                        (make-leaf-set (cdr pairs))))))


(defun pow (base exponent)
    (defun iter (result count)
        (if (= count 0)
            result
            (iter (* base result) (- count 1))))
    (iter 1 exponent))
