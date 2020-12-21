(module project1 mzscheme
  
 ;;;;;;;;;;;; Comp 301 Project 1 ;;;;;;;;;;;;;;;;
  
  ;;; Add group members below
  ;;; Mert Gulsun, 68979
  ;;; Adar Bayan, 63970
  ;;; Mehmet Enes Erciyes, 68906


  
  ;;;;;;;;;;;;;;;;;;; PROBLEMS ;;;;;;;;;;;;;;;;;;;;
  
  ;; PROJECT 1 Part A | Write your answer below here as a comment

  ;hex-prefix ::= 0x
  ;hex-exp :: =(hex-prefix {hex-char}*)
  ;hex-char ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | A | B | C | D | E
  ;bin-prefix ::= bin
  ;bin-exp ::= (bin-prefix {bin-char}*)
  ;bin-char ::= 0 | 1


  
  ;; PROJECT 1 Part B
  ;; First Representation | We added a suffix so that both first and second representations can be tested at once.

  (define create-a
      (lambda(N)
      (if(= N 0) '(0x)
         (append (create-a (/ (- N (remainder N 16)) 16)) (list (dec-2-hex (remainder N 16))))
       )
      ))
  
  (define dec-2-hex
    (lambda (x)
      (cond ((< x 10) x)
            ((eq? x 10) 'A)
            ((eq? x 11) 'B)
            ((eq? x 12) 'C)
            ((eq? x 13) 'D)
            ((eq? x 14) 'E)
            ((eq? x 15) 'F))))
  (define hex-2-dec
    (lambda (x)
      (cond ((eq? x 'A) 10)
            ((eq? x 'B) 11)
            ((eq? x 'C) 12)
            ((eq? x 'D) 13)
            ((eq? x 'E) 14)
            ((eq? x 'F) 15)
            ((< x 10) x))))

  (define nextChar
    (lambda (hex)
      (dec-2-hex (remainder (+ 1 (hex-2-dec hex)) 16))))
  
  (define is-zero-a?
    (lambda (hex-num)
      (cond ((eq? hex-num '()) #t)
        ((null? (cdr hex-num )) #t)
        (else #f))   
      ))

  (define successor-helper
    (lambda (hex-num n)
      (cond ((= n 0) (hex-2-dec(car hex-num)))
            ((not (= n 0)) (+
                            (successor-helper (cdr hex-num) (- n 1))
                            (* (expt 16 n) (hex-2-dec(car hex-num))))))))

          
  (define successor-a
    (lambda (hex-num)
      (cond ((is-zero-a? hex-num) '())
            (else (create-a (+ (successor-helper (cdr hex-num) (- (length hex-num) 2)) 1)))
            )
      ))
  
    
  ;; Second Representation | We added a -b suffix so that both Unary and BigNum can be tested at once.
     (define create-b
       (lambda(N)
         (if(= N 0) '(bin)
           (append (create-b (/ (- N (remainder N 2)) 2)) (list (remainder N 2))))
      ))
 
    (define is-zero-b?
       (lambda (binary)
          (cond ((eq? binary '()) #t)
         ((null? (cdr binary )) #t)
        (else #f))   
      ))
    (define successor-b
       (lambda (binary)
          (cond ((is-zero-b? binary) '())
          (else (create-b (+ (successor-helper-b (cdr binary) (- (length binary) 2)) 1)))
            )
      ))
     (define successor-helper-b
        (lambda (binary n)
            (cond ((= n 0) (car binary))
            ((not (= n 0)) (+
                            (successor-helper-b (cdr binary) (- n 1))
                            (* (expt 2 n) (car binary)))))
      ))

  
  ;; PROJECT 1 Part C | Write your answer below here as a comment
  ; 
  ; Constructor-> Constructors are functions which take certain parameters in as inputs, and create a desired data type according to the outlined grammar definition.
  ; Observer-> Observers extract information from values of the data type.
  ; Extractors-> Extractors are a kind of observer, which take in a certain data type, and take out a parameter value which was used to build that data type.
  ; Predicates-> Predicates are a kind of a observer function,which take in a certain data type, and return a boolean value in accordance with an outlined logic.
  ; create-a->Constructor
  ; is-zero-a?->Predicate,Observer
  ; successor-a->Constructor
  ; create-b->Constructor
  ; is-zero-b?->Predicate,Observer
  ; successor-b->Constructor
  ;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Don't worry about the below function, we included it to test your implemented functions and display the result in the console
  ;; As you implement your functions you can Run (the button on the top right corner) to test your implementations
  (define-syntax equal??
    (syntax-rules ()
      ((_ test-exp correct-ans)
       (let ((observed-ans test-exp))
         (if (not (equal? observed-ans correct-ans))
           (printf "Oops! ~s returned ~s, should have returned ~s~%" 'test-exp observed-ans correct-ans)
           (printf "Correct! ~s => ~s~%" 'test-exp correct-ans))))))
  
  
  ;; PROJECT 1 Part D | Remove the comments and write test cases.
  (display "First Representation Tests\n")
  (equal?? (is-zero-a? '()) #t) 
  (equal?? (successor-a '()) '()) 
  (equal?? (create-a 165) '(0x A 5))
  (equal?? (is-zero-a? '(0x 2 3 4 B)) #f)
  (equal?? (is-zero-a? '()) #t)
  (equal?? (successor-a (create-a 165)) '(0x A 6))
  (newline)

  
  (display "Second Representation Tests\n")
  (equal?? (create-b 4) '(bin 1 0 0))
  (equal?? (is-zero-b? '(bin 2 1 1)) #f)
  (equal?? (is-zero-b? '()) #t)
  (equal?? (successor-b (create-b 3)) '(bin 1 0 0))
  (newline)
)