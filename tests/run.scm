#;(include "../matchable.scm")

(import scheme (chicken base) matchable test)

(test-begin "match")

(test-group "simple"
  (test-error "no matching" (match 0 (1 'ok)))

  (test "any"  'ok (match 'any (_ 'ok)))
  (test "symbol"  'ok (match 'ok (x x)))
  (test "number"  'ok (match 28 (28 'ok)))
  (test "string"  'ok (match "good" ("bad" 'fail) ("good" 'ok)))
  (test "literal symbol"  'ok (match 'good ('bad 'fail) ('good 'ok)))
  (test "null"  'ok (match '() (() 'ok)))
  (test "pair"  'ok (match '(ok) ((x) x)))
  (test "vector"  'ok (match '#(ok) (#(x) x)))
  (test "any doubled"  'ok (match '(1 2) ((_ _) 'ok)))
  (test "and empty"  'ok (match '(o k) ((and) 'ok)))
  (test "and single"  'ok (match 'ok ((and x) x)))
  (test "and double"  'ok (match 'ok ((and (? symbol?) y) 'ok)))
  (test "or empty"  'ok (match '(o k) ((or) 'fail) (else 'ok)))
  (test "or single"  'ok (match 'ok ((or x) 'ok)))
  (test "or double"  'ok (match 'ok ((or (? symbol? y) y) y)))
  (test "not"  'ok (match 28 ((not (a . b)) 'ok)))
  (test "pred"  'ok (match 28 ((? number?) 'ok)))
  (test "named pred"  29 (match 28 ((? number? x) (+ x 1)))))

(test-group "duplicate symbols"
  (test "duplicate symbols pass"  'ok (match '(ok . ok) ((x . x) x)))
  (test "duplicate symbols fail"  'ok (match '(ok . bad) ((x . x) 'bad) (else 'ok)))
  (test "duplicate symbols samth"  'ok (match '(ok . ok) ((x . 'bad) x) (('ok . x) x))))

(test-group "ellipses"
  (test "ellipses"
        '((a b c) (1 2 3))
        (match '((a . 1) (b . 2) (c . 3))
               (((x . y) ___) (list x y))))

  (test "real ellipses"
        '((a b c) (1 2 3))
        (match '((a . 1) (b . 2) (c . 3))
               (((x . y) ...) (list x y))))

  (test "vector ellipses"
        '(1 2 3 (a b c) (1 2 3))
        (match '#(1 2 3 (a . 1) (b . 2) (c . 3))
               (#(a b c (hd . tl) ...) (list a b c hd tl))))

  (test "pred ellipses"
        '(1 2 3)
        (match '(1 2 3)
               (((? odd? n) ___) n)
               (((? number? n) ___) n))))

(test "failure continuation"
      'ok
      (match '(1 2)
             ((a . b) (=> next) (if (even? a) 'fail (next)))
             ((a . b) 'ok)))

(test-group "let"
  (test "let"
        '(o k)
        (match-let ((x 'ok) (y '(o k)))
                   y))

  (test "let*"
        '(f o o f)
        (match-let* ((x 'f) (y 'o) ((z w) (list y x)))
                    (list x y z w))))

(test-group "getter/setter"
  (test "getter car"
        '(1 2)
        (match '(1 . 2) (((get! a) . b) (list (a) b))))

  (test "getter cdr"
        '(1 2)
        (match '(1 . 2) ((a . (get! b)) (list a (b)))))

  (test "getter vector"
        '(1 2 3)
        (match '#(1 2 3) (#((get! a) b c) (list (a) b c))))

  (test "setter car"
        '(3 . 2)
        (let ((x '(1 . 2)))
          (match x (((set! a) . b) (a 3)))
          x))

  (test "setter cdr"
        '(1 . 3)
        (let ((x '(1 . 2)))
          (match x ((a . (set! b)) (b 3)))
          x))

  (test "setter vector"
        '#(1 0 3)
        (let ((x '#(1 2 3)))
          (match x (#(a (set! b) c) (b 0)))
          x)))

(test-group "records"
  (module boxes (box make-box)
    (import scheme (chicken base))
    (define-record box value))

  (import boxes)

  (define-record point x y)
  (define-record-type my-box (make-my-box x) box? (x get-my-box-x))

  (test "toplevel record using raw name"
        '(123 456)
        (match (make-point 123 456) (($ 'point x y) (list x y))))
  
  (test "toplevel record using identifier"
        '(123 456)
        (match (make-point 123 456) (($ point x y) (list x y))))

  (test-error "module-namespaced record using invalid raw name fails"
              (match (make-box 123) (($ 'foo#box x) (list x))))

  (test "module-namespaced record using raw name"
        '(123)
        (match (make-box 123) (($ 'boxes#box x) (list x))))

  (test "module-namespaced record using identifier"
        '(456)
        (match (make-box 456) (($ box x) (list x))))
  
  (test "record with different predicate name"
        'ok
        (match (make-my-box 'ok) (($ my-box x) x)))

  (test "record with literals"
        456
        (match (make-point 123 456)
          (($ point 123 x) x)
          (else #f)))

  (test-error "record with @ pattern should fail"
              (match (make-point 123 456) ((@ point (x a) (y b)) 'ok)))


  (test "record nested"
        '(123 456 789)
        (match (make-point 123 '(456 789)) (($ point x (y z)) (list x y z))))

  (test "record getter"
        '(123 456)
        (let ((p (make-point 123 456)))
          (match p (($ point x (get! y)) (list x (y))))))

  (test "record setter"
        '(123 789)
        (let ((p (make-point 123 456)))
          (match p (($ point x (set! y)) (y 789)))
          (list (point-x p) (point-y p)))))

(test-group "tails"
  (test "single tail"
        '((a b) (1 2) (c . 3))
        (match '((a . 1) (b . 2) (c . 3))
               (((x . y) ... last) (list x y last))))

  (test "single tail 2"
        '((a b) (1 2) 3)
        (match '((a . 1) (b . 2) 3)
               (((x . y) ... last) (list x y last))))

  (test "multiple tail"
        '((a b) (1 2) (c . 3) (d . 4) (e . 5))
        (match '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5))
               (((x . y) ... u v w) (list x y u v w)))))

(test "Riastradh quasiquote"
      '(2 3)
      (match '(1 2 3) (`(1 ,b ,c) (list b c))))

(test-end "match")

(test-exit)
