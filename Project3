; Make a list of n length
(define (makelist n)
  (if (<= n 1)
    (list 1)
    (cons n (makelist (- n 1)))))


; Randomize the list
(define (random-sort lst)
  (sort lst
    (lambda (x y)
      (equal? 0 (random 2)))))


; Perform ascending sort of list
(define (asc-sort lst)
  (sort lst
    (lambda (x y)
      (< x y))))


; Break into k-sized groups
(define (split-by lst k)
  (if (not (null? lst))
    (cons (asc-sort (take lst k) ) (split-by (drop lst k) k) )
    '() ))


; Number of occurences in a list
; x is the grouping you're looking for
; lst is the list to scan
(define (occurence x lst)
  (if (null? lst) 
    0
    (if (equal? x (car lst)) 
      (+ 1 (occurence x (cdr lst)))
      (occurence x (cdr lst)))))


; Make candidate grouping
(define (make-group n k)
  (split-by (random-sort (makelist n)) k))


; Appends the assignement list groupings into a single list
(define (append . lsts)
  (cond
    ((null? lsts) '())
    ((null? (car lsts)) (apply append (cdr lsts)))
    (else (cons (caar lsts) (apply append (cdar lsts) (cdr lsts))))))


; gets random distribution of all students over all assignements
(define (get-all-groupings x)
  (if (= x 0)
    '()
    (append (random-sort (makelist n)) (get-all-groupings (- x 1)))))


; Determines the total number of times a grouping is repeated
; within the 8 assignment groupings
(define (total-occurences lst)
  (if (null? lst) 
    0
    (+ 
      (total-occurences (cdr lst)) 
      (occurence (car lst) (cdr lst)))))


; Returns the width in which to display the student number
(define (num-width n)
  (if (< n 10)
    1
    (+ 1 (num-width (/ n 10)))))


; Places the student number to display into an appropriately
;  sized field based on the number of students (i.e. 3 spaces
;  if n >= 100, 2 spaces if n >= 10, or 1)
(define (format-value x)
  (string-append
    (make-string 
      (- (num-width n) (string-length x)) 
      #\ ) x ))


; Displays the students within a group to the screen in a formatted
;  way to allow all groups to line up
(define (format-cell lst)
  (if (not (null? lst))
    (begin
      (display (format-value (number->string (car (take lst 1)) )))
      (if (> (length lst) 1)
        (display " "))
      (format-cell (drop lst 1)))))


; Displays an individual group to the screen
(define (format-group lst)
  (display "(")
  (format-cell lst)
  (display ")"))

  
; Responsible for displaying an 'assignment' in a formatted
;  manner
(define (display-groups lst)
  (if (not (null? lst))
    (begin
      (format-group (car (take lst 1)) )
      (display " ")
      (display-groups (drop lst 1))))) 


; The messages to precede the data when displayed
(define msgs 
  (vector 
    "\n\nAssignment1: " 
    "Assignment2: " 
    "Assignment3: "
    "Midterm:     "
    "Assignment4: "
    "Assignment5: "
    "Assignment6: "
    "Final Exam:  "))


; Displays the list to the screen
; idx is the index in to the messages that preced the data
; g is the number of groups (n/k)
(define (display-the-list lst g idx)
  (if (not (null? lst))
    (begin 
      (display (vector-ref msgs idx))
      (display-groups (take lst g))
      (display "\n")
      (display-the-list (drop lst g) g (+ idx 1)))))


; If list is OK, prints to the screen and returns a 1 (means done)
; If list is not OK (has a repeated group), a 0 is returned
; n is the number of students
; k is the group size
(define (display-if-ok lst n k )
  (if (= (total-occurences lst) 0)
    (begin
      (display-the-list lst (/ n k) 0) 
      1 )
    0))


; Creates the k-sized grouping for all 8 assignments
(define (get-data n k)
  (split-by (get-all-groupings 8) k ))


; Verifies the test can be run (number of students is divisible by
;  group size), then starts recursively looking for a solution
; n is the number of students
; k is the group size
; iters is the total number of tries to find a solution
; remaining is the number of tries left
(define (run-test n k iters remaining)
  (if (not (= (modulo n k) 0))
    (begin
       (prnt "Can not form full groups of students with n=" n " and k=" k "\n\n"))
    (if (<= remaining 0)
      (begin
	 (prnt "Could not find a solution for n=" n " and k=" k " trying " iters " times.\n\n"))
      (if (= 0 (display-if-ok (get-data n k) n k))
        (run-test n k iters (- remaining 1))
        (display "Successfully found a solution\n\n")))))
     


; Basic method to retrieve user input
(define prompt-read (lambda (Prompt)
   (display Prompt)
   (read)))

; Get input from the user
(define (welcome-page)
  (display "\n\nStudent Grouping Program\n\n")
  (display "This program will attempt to group students into set\n")
  (display "sized groups over eight events without repeating group\n")
  (display "composition. It does so by randomly grouping students\n")
  (display "until a satisfactory solution is found, or until the max\n")
  (display "number of attempts is performed, at which time a message\n")
  (display "indicating no solution could be found is displayed.\n\n")
  (display "The user is prompted for the class size (n), group size (k),\n")
  (display "and the number of iterations to try before failing (1000 is\n")
  (display "recommended). Note the number of students must be divisible\n")
  (display "by the group size (n%k=0), or an error will be generated.\n\n") 
)



(define (prnt . args)
  (for-each display args))


; Prompts for user input
(welcome-page)
(define n (prompt-read "\n\nEnter number of students: "))
(define k (prompt-read "Enter people per group: "))
(define iters (prompt-read "Attempts: "))


; todos
; 1. do error checking
;   number of students > 0 and less than or equal to (say) 1000
;   k must be > 0
;   k must be < n
;   iters must be > 0
; 2. Add header block to file (name of program, description, name(s), class, professor, etc...
; 3. Add common header to each function
;    - name
;    - description
;    - inputs
;    - return value

; Performs actual test 
(run-test n k iters iters)
