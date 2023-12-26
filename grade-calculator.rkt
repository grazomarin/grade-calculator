;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname grade-calculator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

#|

                   USER MANUAL IS AT THE BOTTOM OF THE PAGE

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;   DATA DEFINITIONS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   INPUT   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@htdd Percentage)
;; Percentage is type-of Number
;; CONSTRAINT: Percentage is a number between [0, 100]

(define TEST1 55)
(define TEST2 97)
(define AVERAGE 77.7)

;; (listof Percentage)
(define (fn-for-log log)
  (cond [(empty? log) ...]
        [else (... (first log)
                   (fn-for-log (rest log)))]))

(@htdd Section)
(define-struct section (name weight dropped percentages))
;; Section is (make-section String Percentage Natural (listof Percentage))
;; interp. name    -> name of the section
;;         weight  -> total % weight of the section between 0% and 100%
;;         dropped -> number of lowest assignments/tests/tasks that get dropped
;;         percentages  -> student percentages in the section

(define CLICKERS (make-section "Clickers" 10 5
                               (list 0 0 50 75
                                     100 20 50 12
                                     78 90 30 0 0
                                     22 88 90 100
                                     85 99 87 99
                                     100 100 100)))

(define LABS (make-section "Labs" 10 1
                           (list 100 100 100
                                 88 100 100
                                 67 100 100
                                 99 77)))

(define PSETS (make-section "Problem Sets" 15 1
                            (list 100 100 100
                                  87 90 90
                                  90 100 100
                                  88 100)))
                                  
(define MT1 (make-section "Midterm 1" 15 0
                          (list 88)))

(define MT2 (make-section "Midterm 2" 20 0
                          (list 97)))

(define FINAL (make-section "Final" 30 0
                            (list 75)))
                              
                              
(define (fn-for-section s)
  (... (section-name s)
       (section-weight s)
       (section-dropped s)
       (fn-for-percentages (section-percentages s))))

;; (listof Section)
(define (fn-for-los los)
  (cond [(empty? los) ...]
        [else (... (fn-for-section (first los))
                   (fn-for-los (rest los)))]))



(@htdd Course)
(define-struct course (name sections))
;; Subject is (make-subject String (listof Section))
;; interp. a course name and its section

(define CPSC110 (make-course "CPSC 110"
                             (list CLICKERS LABS PSETS MT1 MT2 FINAL)))

(define (fn-for-course c)
  (... (course-name c)
       (fn-for-los (course-sections c))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   OUTPUT   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(@htdd Section-Summary)
(define-struct ss (name average highest lowest))
;; Section-Summary is (make-ss String Percentage Percentage Percentage)
;; interp. name    -> name of the section
;;         average -> average percentage of all the percentages in the section
;;         highest -> highest percentage in that section
;;         lowest  -> lowest percentage in that section

(define CLICKERSS (make-ss "Clickers" 77 100 20))
(define LABSS (make-ss "Labs" 96.4 100 77))
(define PSETSS (make-ss "Problem Sets" 95.8 100 88))
(define MT1SS (make-ss "Midterm 1" 88 88 88))
(define MT2SS (make-ss "Midterm 2" 97 97 97))
(define FINALSS (make-ss "Final" 75 75 75))

(define (fn-for-ss ss)
  (... (ss-name ss)
       (ss-average ss)
       (ss-highest ss)
       (ss-lowest ss)))


;; (listof Section-Summary)

(define (fn-for-loss loss)
  (cond [(empty? loss) ...]
        [else (... (fn-for-ss (first loss))
                   (fn-for-loss (rest loss)))]))



(@htdd Course-Summary)
(define-struct cs (name average sections))
;; Course-Summary is (make-cs String Percentage (listof Section-Summary))
;; interp. a course name with it's average result and results of its sections

(define CPSC110S (make-cs "CPSC 110" 86.81
                          (list CLICKERSS LABSS PSETSS MT1SS MT2SS FINALSS)))

(define (fn-for-cs cs)
  (... (cs-name cs)
       (cs-average cs)
       (fn-for-loss (cs-sections cs))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   FUNCTIONS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#| Initial draft:

CPSC 110    |    Percentage    |      90%     |
==========================================
            | Average | Highest | Lowest |
------------------------------------------
Clicker     |   50%   |   60%   |   40%  |
------------------------------------------
Midterms    |   90%   |   95%   |   80%  |
------------------------------------------
Assignments |   70%   |   80%   |   40%  |
------------------------------------------

|#


(@htdf produce-grade-summary)
(@signature Course -> Image)
;; produce grade summary of a course in a table format
;; course must contain at least 1 section

;; THESE ARE NOT MY RESULTS, I SWEAR
(check-expect
 (produce-grade-summary CPSC110)
 (above (header "CPSC 110" 86.81)
        HB
        (row "Clickers" 77 100 20)
        HB
        (row "Labs" 96.4 100 77)
        HB
        (row "Problem Sets" 95.8 100 88)
        HB
        (row "Midterm 1" 88 88 88)
        HB
        (row "Midterm 2" 97 97 97)
        HB
        (row "Final" 75 75 75)))
                
(define (produce-grade-summary c)
  (render-grades (calculate-grades c)))



(@htdf calculate-grades)
(@signature Course -> Course-Summary)
;; calculate grade summary of a course

(check-expect (calculate-grades CPSC110) CPSC110S)

(define (calculate-grades c)
  (local 
    [(define (fn-for-course c)
       (local [(define section-summaries (fn-for-los (course-sections c)))
               (define section-weights (map section-weight (course-sections c)))
               (define (average ss sw)
                 (cond [(empty? ss) 0]
                       [else (+ (* (ss-average (first ss)) (/ (first sw) 100))
                                (average (rest ss) (rest sw)))]))]
         (make-cs (course-name c) 
                  (average section-summaries section-weights)
                  section-summaries)))
  
     (define (fn-for-los los)
       (cond [(empty? los) empty]
             [else (cons (fn-for-section (first los))
                         (fn-for-los (rest los)))])) 
                  
     (define (fn-for-section s)
       (local [(define dropped-sorted
                 (drop-all-lowest (sort (section-percentages s) <)
                                  (section-dropped s)))]
         (make-ss (section-name s)
                  (/ (foldr + 0 dropped-sorted) (length dropped-sorted))
                  (first (reverse dropped-sorted))
                  (first dropped-sorted))))
 
     ;; drop is Natural
     ;; number of lowest percentages that should be dropped
     ;; log has to be sorted in increasing order
     (define (drop-all-lowest log drop)
       (cond [(empty? log) (list 0)]
             [(zero? drop) log]
             [else (drop-all-lowest (rest log) (sub1 drop))]))]

    (fn-for-course c)))



(@htdf render-grades)
(@signature Course-Summary -> Image)
;; produce a table out of grade summary

(check-expect (render-grades CPSC110S)
              (above (header "CPSC 110" 86.81)
                     HB
                     (row "Clickers" 77 100 20)
                     HB
                     (row "Labs" 96.4 100 77)
                     HB
                     (row "Problem Sets" 95.8 100 88)
                     HB
                     (row "Midterm 1" 88 88 88)
                     HB
                     (row "Midterm 2" 97 97 97)
                     HB
                     (row "Final" 75 75 75)))
              

;(define (render-grades cs) empty-image)

(define (render-grades cs)
  (local
    [(define (fn-for-cs cs)
       (above
        (header (cs-name cs) (cs-average cs))
        (fn-for-loss (cs-sections cs))))

     (define (fn-for-loss loss)
       (cond [(empty? loss) empty-image]
             [else (above HB
                          (fn-for-ss (first loss))
                          (fn-for-loss (rest loss)))]))

     (define (fn-for-ss ss)
       (row (ss-name ss)
            (ss-average ss)
            (ss-highest ss)
            (ss-lowest ss)))]
    
    (fn-for-cs cs)))


;;;;;;;;;;;;;;;;;;;;;;;;;   HELPERS AND CONSTANTS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define HB (rectangle 600 2 "solid" "black")) 
(define VB (rectangle 3 40 "solid" "black"))

(@htdf header)
(@signature String Percentage -> Image)
;; produce header part of the grade summary table
;; c is course name 
;; a is course average

(define (header c a)
  (above (beside (overlay (txt c)
                          (cell 200 "white"))
                 VB
                 (overlay (txt "Grade")
                          (cell 200 "white"))
                 VB
                 (overlay (txt (string-append (number->string-digits a 4) "%"))
                          (cell 200 (grade-color a))))
         HB HB
         (beside (cell 200 "white")
                 VB
                 (overlay (txt "Average")
                          (cell 133 "white"))
                 VB
                 (overlay (txt "Highest")
                          (cell 133 "white"))
                 VB
                 (overlay (txt "Lowest")
                          (cell 133 "white")))))

(@htdf row)
(@signature String Percentage Percentage Percentage -> Image)
;; produce row part of the grade summary table

;; s is section name
;; a is course average
;; h is highest grade
;; l is lowest grade
(define (row s a h l)
  (beside (overlay
           (txt s)
           (cell 200 "white"))
          VB
          (overlay (txt (string-append (number->string-digits a 4) "%"))
                   (cell 133 (grade-color a)))
          VB
          (overlay (txt (string-append (number->string-digits h 4) "%"))
                   (cell 133 (grade-color h)))
          VB
          (overlay (txt (string-append (number->string-digits l 4) "%"))
                   (cell 133 (grade-color l)))))

(@htdf cell)
(@signature Number String -> Image)
;; produce cell with background color c

(define (cell w c) (rectangle w 40 "solid" c))

(@htdf txt)
(@signature String -> Image)
;; produce text image from te 
(define (txt t) (text t 18 "black"))

(@htdf grade-color)
(@signature Percentage -> String)
;; produce color of the grade
;; [0,  50]  produces red
;; (50, 75)  produces yellow
;; [75, 100] produces green

(check-expect (grade-color 50) "light red")
(check-expect (grade-color 51) "light yellow")
(check-expect (grade-color 75) "light green")
(check-expect (grade-color 100) "light green")

(define (grade-color g)
  (cond [(>= g 75) "light green"]
        [(> g 50) "light yellow"]
        [else "light red"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|

to produce table with your grades:
(produce-grade-summary your-course)

replace "your-course" with your compound data that is following the structure of
"Course" data definition

when you get the table rendering the coloring indicates this:
red    -> below and including 50
yellow -> above 50 till and not including 75
green  -> above 75

Only restrictions are the ones established by the data definitions.

Enjoy!!!!


For way cooler and more interactive projects feel free to visit my github:
https://github.com/grazomarin


|#

;; Image example was removed as it causes issues with the code when 
;; viewing outside Dr Racket

(produce-grade-summary
 (make-course
  "CPSC109"
  (list (make-section "Clickers" 10 3 (list 22 12 40 30 88 90 100 89))
        (make-section "Labs" 10 2 (list 12 33 44 100 100 100 100 100))
        (make-section "Problem Sets" 15 1 (list 12 33 44 100 100 100 60 100))
        (make-section "Midterm 1" 15 0 (list 90))
        (make-section "Midterm 2" 20 0 (list 88))
        (make-section "Final" 30 0 (list 74)))))



                                  
        