import evaluator.evaluator.eval
import parser.parser.{parseProgram, parseTokensToNodes}
import tokenize.Tokenizer._

object Main extends App {
  println(tokenize(str = "11+22"))
  println(tokenize(str = "(ab1 + 22.012 + a * 'ccd - ab2 / 3)"))

  val code1 = """
(define (length lst)
  (if (null? lst)
    0
    (+ 1 (length (cdr lst))))
)

(define (cadr lst) (car (cdr lst)))

(define (cddr lst) (cdr (cdr lst)))

(define (caddr lst) (car (cddr lst)))

(define (cdddr lst) (cdr (cddr lst)))

(define (cadddr lst) (car (cdddr lst)))

(define (gteq a b) (or (> a b) (= a b)))

(define nil '())

(define (append lst1 lst2)
  (if (null? lst1)
    lst2
    (cons (car lst1) (append (cdr lst1) lst2))))

|(define (add-last x lst)
 |  (if (null? lst)
 |      (list x)
 |    (cons (car lst) (add-last x (cdr lst)))
 |    )
 |  )
 |
 |(define (addend-and-augend exp)
 |  (define (iter addend exp)
 |  (cond
 |    ((null? exp) #f)
 |    ((eq? '+ (car exp)) (cons addend (cdr exp)))
 |    (else
 |     (iter (add-last (car exp) addend) (cdr exp))
 |     )
 |    )
 |  )
 |  (iter '() exp))
 |
 |(define (deriv exp var)
 |  (cond ((number? exp) 0)
 |        ((variable? exp)
 |         (if (same-variable? exp var) 1 0))
 |         ((one? exp) (deriv (car exp) var))
 |        ((simple-sum? exp)
 |          (make-sum (deriv (addend exp) var)
 |                    (deriv (augend exp) var)))
 |        ((sum? exp)
 |         (make-sum (deriv (car (sum? exp)) var)
 |                   (deriv (cdr (sum? exp)) var)))
 |        ((product? exp)
 |          (make-sum
 |           (make-product (multiplier exp)
 |                         (deriv (multiplicand exp) var))
 |           (make-product (deriv (multiplier exp) var)
 |                         (multiplicand exp))))
 |         (else
 |          (error "unknown expression type -- DERIV" exp))))
 |
 |
 |(define (variable? x) (symbol? x))
 |
 |(define (one? x) (and (pair? x) (= (length x) 1)))
 |
 |(define (same-variable? v1 v2)
 |  (and (variable? v1) (variable? v2) (eq? v1 v2)))
 |
 |(define (simple-sum? x)
 |  (and (pair? x) (eq? (cadr x) '+)
 |       (null? (cdddr x))))
 |
 |(define (sum? x)
 |   (addend-and-augend x))
 |
 |(define (addend s) (car s))
 |
 |(define (augend s) (caddr s))
 |
 |(define (product? x)
 |  (and (pair? x) (eq? (cadr x) '*)))
 |
 |(define (multiplier p) (car p))
 |
 |(define (multiplicand p)
 |(if (null? (cdddr p))
 |  (caddr p)
 |  (cddr p)))
 |
 |
 |
 |(define (make-sum a1 a2)
 |  (cond ((number-exp? a1 0) a2)
 |        ((number-exp? a2 0) a1)
 |        ((and (number? a1) (number? a2)) (+ a1 a2))
 |        (else (list a1 '+ a2))))
 |
 |(define (number-exp? exp num)
 |  (and (number? exp) (= exp num)))
 |
 |(define (make-product m1 m2)
 |  (cond ((or (number-exp? m1 0) (number-exp? m2 0)) 0)
 |        ((number-exp? m1 1) m2)
 |        ((number-exp? m2 1) m1)
 |        ((and (number? m1) (number? m2)) (* m1 m2))
 |        (else (list m1 '* m2))))
 |
|(print (deriv '(x + 3) 'x))
 |(print (deriv '(x * y) 'x))
 |(print (deriv '((x * y) * (x + 3)) 'x))
 |(print (deriv '(x + 3 * (x + y + 2)) 'x))
 |(print (deriv '(x + 3 * (x + x) * 2) 'x))
 |(print (deriv '(x + 3 * x + x + x * y) 'x))
 |(print (deriv '(x * 3 * x * x + x * y * y + y * x) 'x))
 |(print (deriv '(x + x + x * x * 3 * y * 5 + x * x) 'x))
 |(print (deriv '(x + 3 * x * x + x * (y + 2)) 'x))
 |(print (deriv '(x + 3 * x * x + (x + (x * x + x))) 'x))
 |(print (deriv '(x + 3 * x * x + (x + (x * (x + x) * y * x))) 'x))
 |(print (deriv '(x + 3 * x * x + (x + (x * (x + x) * y * x))) 'y))
"""

  val result1 = eval(parseProgram(parseTokensToNodes(tokenize(code1))))
  println(result1)

  val code2 =
    """
      |(define (length lst)
      |  (if (null? lst)
      |    0
      |    (+ 1 (length (cdr lst))))
      |)
      |
      |(define (cadr lst) (car (cdr lst)))
      |
      |(define (cddr lst) (cdr (cdr lst)))
      |
      |(define (caddr lst) (car (cddr lst)))
      |
      |(define (cdddr lst) (cdr (cddr lst)))
      |
      |(define (cadddr lst) (car (cdddr lst)))
      |
      |(define (gteq a b) (or (> a b) (= a b)))
      |
      |(define nil '())
      |
      |(define (append lst1 lst2)
      |  (if (null? lst1)
      |    lst2
      |    (cons (car lst1) (append (cdr lst1) lst2))))
      |
      |(define (entry tree) (car tree))
      |
      |(define (make-leaf symbol weight)
      |  (list 'leaf symbol weight))
      |
      |(define (leaf? object)
      |  (eq? (car object) 'leaf))
      |
      |(define (symbols tree)
      |  (if (leaf? tree)
      |    (list (symbol-leaf tree))
      |    (caddr tree)))
      |
      |(define (weight tree)
      |  (if (leaf? tree)
      |    (weight-leaf tree)
      |    (cadddr tree)))
      |
      |
      |(define (symbol-leaf x) (cadr x))
      |
      |(define (weight-leaf x) (caddr x))
      |
      |(define (make-code-tree left right)
      |  (list left
      |    right
      |    (append (symbols left) (symbols right))
      |    (+ (weight left) (weight right))))
      |
      |(define (left-branch tree) (car tree))
      |
      |(define (right-branch tree) (cadr tree))
      |

      |(define (decode bits tree)
      |  (define (decode-1 bits current-branch)
      |    (if (null? bits)
      |        '()
      |      (let ((next-branch
      |              (choose-branch (car bits) current-branch)))
      |        (if (leaf? next-branch)
      |          (cons (symbol-leaf next-branch)
      |            (decode-1 (cdr bits) tree))
      |          (decode-1 (cdr bits) next-branch)))))
      |  (decode-1 bits tree))
      |
      |(define (choose-branch bit branch)
      |  (cond ((= bit 0) (left-branch branch))
      |    ((= bit 1) (right-branch branch))
      |    (else (error "bad bit -- CHOOSE-BRANCH" bit))))
      |
      |(define (encode message tree)
      |  (if (null? message)
      |    nil
      |    (append (encode-symbol (car message) tree)
      |      (encode (cdr message) tree))))
      |
      |(define (encode-symbol message tree)
      |  (cond
      |    ((leaf? tree) nil)
      |    (else
      |      (let ((right-symbols (symbols (right-branch tree)))
      |             (left-symbols (symbols (left-branch tree))))
      |        (cond
      |          ((element_of_symbols? message right-symbols)
      |            (cons 1 (encode-symbol message (right-branch tree))))
      |          ((element_of_symbols? message left-symbols)
      |            (cons 0 (encode-symbol message (left-branch tree))))
      |          (else
      |            (error "message not found in tree --ENCODE SYMBOL" message))
      |        ))
      |    )
      |  ))
      |(define (element_of_symbols? message symbols)
      |  (cond
      |    ((null? symbols) #f)
      |    ((equal? message (car symbols)) #t)
      |    (else
      |      (element_of_symbols? message (cdr symbols))
      |    )
      |  )
      |)
      |
      |(define (adjoin-set x set)
      |  (cond ((null? set) (list x))
      |    ((< (weight x) (weight (car set))) (cons x set))
      |    (else (cons (car set)
      |            (adjoin-set x (cdr set))))))
      |
      |(define (make-leaf-set pairs)
      |  (if (null? pairs)
      |      '()
      |    (let ((pair (car pairs)))
      |      (adjoin-set (make-leaf (car pair)
      |                    (cadr pair))
      |        (make-leaf-set (cdr pairs))))))
      |
      |
      |(define (successive-merge set)
      |  (if (null? (cdr set))
      |    (car set)
      |    (successive-merge
      |      (adjoin-set (make-code-tree
      |                    (car set)
      |                    (cadr set))
      |        (cddr set)))))
      |
      |(define (generate-huffman-tree pairs)
      |  (successive-merge (make-leaf-set pairs)))
      |
      |(define (insert x pairs)
      |  (cond
      |    ((null? pairs) (cons x nil))
      |    ((< (weight x) (weight (car pairs))) (cons x pairs))
      |    (else
      |      (cons (car pairs) (insert x (cdr pairs)))
      |    )
      |  )
      |)
      |(define (insert-sort pairs)
      |  (cond
      |    ((null? pairs) nil)
      |    (else
      |      (insert (car pairs) (insert-sort (cdr pairs))))
      |  )
      |)
      |
      |(define (my-successive-merge leaves)
      |  (cond
      |    ((null? leaves) nil)
      |    ((= (length leaves) 2)
      |      (make-code-tree (car leaves) (cadr leaves)))
      |    (else
      |      (my-successive-merge
      |        (insert-sort
      |          (cons (make-code-tree (car leaves) (cadr leaves)) (cddr leaves))))
      |    )
      |  )
      |)
      |(define (my-generate-huffman-tree pairs)
      |  (my-successive-merge (make-leaf-set pairs)))
      |
      |(define huffman-tree (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16))))
      |(print (encode '(A) huffman-tree))
      |
      |(print (encode '(B) huffman-tree))
      |
      |(print (encode '(C) huffman-tree))
      |
      |(print (encode '(D) huffman-tree))
      |
      |(print (encode '(E) huffman-tree))
      |
      |
      |(define huffman-tree2 (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512))))
      |(print (encode '(A) huffman-tree2))
      |
      |(print (encode '(B) huffman-tree2))
      |
      |(print (encode '(C) huffman-tree2))
      |
      |(print (encode '(D) huffman-tree2))
      |
      |(print (encode '(E) huffman-tree2))
      |
      |(print (encode '(F) huffman-tree2))
      |
      |(print (encode '(G) huffman-tree2))
      |
      |(print (encode '(H) huffman-tree2))
      |
      |(print (encode '(I) huffman-tree2))
      |
      |(print (encode '(J) huffman-tree2))
      |
      |
      |(define huffman-tree (generate-huffman-tree '((B 2) (A 1) (E 16) (D 8) (C 4))))
      |(print huffman-tree)
      |(print huffman-tree2)
      |
      |(define (encode message tree)
      |  (if (null? message)
      |    nil
      |    (append (encode-symbol (car message) tree)
      |      (encode (cdr message) tree))))
      |
      |(define (encode-symbol message tree)
      |  (cond
      |    ((leaf? tree) nil)
      |    (else
      |      (let ((right-symbols (symbols (right-branch tree)))
      |             (left-symbols (symbols (left-branch tree))))
      |        (cond
      |          ((element_of_symbols? message right-symbols)
      |            (cons 1 (encode-symbol message (right-branch tree))))
      |          ((element_of_symbols? message left-symbols)
      |            (cons 0 (encode-symbol message (left-branch tree))))
      |          (else
      |            (error "message not found in tree --ENCODE SYMBOL" message))
      |        ))
      |    )
      |  ))
      |(define (element_of_symbols? message symbols)
      |  (cond
      |    ((null? symbols) #f)
      |    ((equal? message (car symbols)) #t)
      |    (else
      |      (element_of_symbols? message (cdr symbols))
      |    )
      |  )
      |)
    """.stripMargin
  val result2 = eval(parseProgram(parseTokensToNodes(tokenize(code2))))
}
