;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname grade-calculator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
(require spd/tags)

#|



|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;   DATA DEFINITIONS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   INPUT   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@htdd Grade)
;; Grade is type-of Number
;; CONSTRAINT: grade is a number between [0, 100]

(define TEST1 55)
(define TEST2 97)



(@htdd Section)
(define-struct section (name weight number dropped grades))
;; Section is (make-section String Grade Grade (listof Grade))
;; interp. name    -> name of the section
;;         weight  -> total % weight of the section between 0% and 100%
;;         number  -> number of assignments/tests/tasks in the section
;;         dropped -> number of lowest assignments/tests/tasks that get dropped
;;         grades  -> student grades in the section

(define CLICKER (make-section "Clicker" 15 12 5 (list 0 0 50 75
                                                      100 20 50 12
                                                      78 90 30 0 0)))
                              
(define MIDTERMS (make-section "Midterms" 22 2 0 (list 67 95)))

(define (fn-for-section s)
  (... (section-name s)
       (section-weight s)
       (section-number s)
       (section-dropped s)
       (fn-for-grades (section-grades s))))

;; (listof Section)
(define (fn-for-los los)
  (cond [(empty? los) ...]
        [else (... (fn-for-section (first los))
                   (fn-for-los (rest los)))]))



(@htdd Course)
(define-struct course (name sections))
;; Subject is (make-subject String (listof Section))
;; interp. a course name and its section

(define CPSC110 (make-course "CPSC 110" (list CLICKER MIDTERMS)))

(define (fn-for-course c)
  (... (course-name c)
       (fn-for-sections (course-sections c))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   OUTPUT   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(@htdd Section-Summary)
(define-struct ss (name lowest highest average))
;; Section-Summary is (make-ss String Grade Grade Grade)
;; interp. name    -> name of the section
;;         lowest  -> lowest grade in that section
;;         highest -> highest grade in that section
;;         average -> average grade of all the grades in the section

(define SS1 (make-ss "Clickers" 20 80 50))

(define (fn-for-ss ss)
  (... (ss-name ss)
       (ss-lowest ss)
       (ss-highest ss)
       (ss-average ss)))


;; (listof Section-Summary)

(define (fn-for-loss loss)
  (cond [(empty? loss) ...]
        [else (... (first loss)
                   (fn-for-loss (rest loss)))]))



(@htdd Course-Summary)
(define-struct cs (name average sections))
;; Course-Summary is (make-cs String Grade (listof Section-Summary))
;; interp. a course name with it's average result and results of its sections

(define CPSC110SUM (make-cs "CPSC 110" 80 (list ))) ;; !!! fill up the list

(define (fn-for-cs cs)
  (... (cs-name cs)
       (cs-average cs)
       (fn-for-ss (cs-sections cs))))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;   MAIN FUNCTION   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

CPSC 110    |    Grade    |      90%     |
==========================================
            | Average | Lowest | Highest |
------------------------------------------
Clicker     |   50%   |   40%  |   60%   |
------------------------------------------
Midterms    |   90%   |   80%  |   95%   |
------------------------------------------
Assignments |   70%   |   40%  |   80%   |
------------------------------------------

|#







 
