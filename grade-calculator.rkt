;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname grade-calculator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

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
(define AVERAGE 77.7)

;; (listof Grade)
(define (fn-for-log log)
  (cond [(empty? log) ...]
        [else (... (first log)
                   (fn-for-log (rest log)))]))

(@htdd Section)
(define-struct section (name weight number dropped grades))
;; Section is (make-section String Grade Grade (listof Grade))
;; interp. name    -> name of the section
;;         weight  -> total % weight of the section between 0% and 100%
;;         number  -> number of assignments/tests/tasks in the section
;;         dropped -> number of lowest assignments/tests/tasks that get dropped
;;         grades  -> student grades in the section

(define CLICKERS (make-section "Clickers" 10 23 5
                               (list 0 0 50 75
                                     100 20 50 12
                                     78 90 30 0 0
                                     22 88 90 100
                                     85 99 87 99
                                     100 100 100)))

(define LABS (make-section "Labs" 10 11 1
                           (list 100 100 100
                                 88 100 100
                                 67 100 100
                                 99 77)))

(define PSETS (make-section "Problem Sets" 15 11 1
                            (list 100 100 100
                                  87 90 90
                                  90 100 100
                                  88 100)))
                                  
(define MT1 (make-section "Midterm 1" 15 1 0
                          (list 88)))

(define MT2 (make-section "Midterm 2" 20 1 0
                          (list 97)))

(define FINAL (make-section "Final" 30 1 0
                            (list 75)))
                              
                              
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

(define CPSC110 (make-course "CPSC 110" (list CLICKERS LABS PSETS MT1 MT2 FINAL)))

(define (fn-for-course c)
  (... (course-name c)
       (fn-for-los (course-sections c))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   OUTPUT   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(@htdd Section-Summary)
(define-struct ss (name average lowest highest))
;; Section-Summary is (make-ss String Grade Grade Grade)
;; interp. name    -> name of the section
;;         average -> average grade of all the grades in the section
;;         highest -> highest grade in that section
;;         lowest  -> lowest grade in that section

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
        [else (... (first loss)
                   (fn-for-loss (rest loss)))]))



(@htdd Course-Summary)
(define-struct cs (name average sections))
;; Course-Summary is (make-cs String Grade (listof Section-Summary))
;; interp. a course name with it's average result and results of its sections

(define CPSC110S (make-cs "CPSC 110" 86.81
                          (list CLICKERSS LABSS PSETSS MT1SS MT2SS FINALSS)))

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
            | Average | Highest | Lowest |
------------------------------------------
Clicker     |   50%   |   60%   |   40%  |
------------------------------------------
Midterms    |   90%   |   95%   |   80%  |
------------------------------------------
Assignments |   70%   |   80%   |   40%  |
------------------------------------------

|#

(define HB (rectangle 600 2 "solid" "black")) 
(define VB (rectangle 3 40 "solid" "black"))
(define (cell w) (rectangle w 40 "solid" "white")) 
(define (txt t) (text t 18 "black"))
;; header fn
;; row fn


(@htdf produce-grade-summary)
(@signature Course -> Image)
;; produce grade summary of a course in a table format

;; THESE ARE NOT MY RESULTS, I SWEAR
#;(check-expect
   (produce-grade-summary CPSC110)
   (above (beside (overlay (txt "CPSC110")
                           (cell 200))
                  VB
                  (overlay (txt "Grade")
                           (cell 200))
                  VB
                  (overlay (txt "...%")
                           (cell 200)))
          HB HB
          (beside (cell 200)
                  VB
                  (overlay (txt "Average")
                           (cell 133))
                  VB
                  (overlay (txt "Lowest")
                           (cell 133))
                  VB
                  (overlay (txt "Highest")
                           (cell 133)))
          HB
          (beside (overlay
                   (txt "Clickers")
                   (cell 200))
                  VB
                  (overlay (txt "76.3%")
                           (cell 133))
                  VB
                  (overlay (txt "100%")
                           (cell 133))
                  VB
                  (overlay (txt "20%")
                           (cell 133)))
          HB
          (beside (overlay
                   (txt "Labs")
                   (cell 200))
                  VB
                  (overlay (txt "96.4%")
                           (cell 133))
                  VB
                  (overlay (txt "100%")
                           (cell 133))
                  VB
                  (overlay (txt "77%")
                           (cell 133)))
          HB
          (beside (overlay
                   (txt "Problem Sets")
                   (cell 200))
                  VB
                  (overlay (txt "95.8%")
                           (cell 133))
                  VB
                  (overlay (txt "100%")
                           (cell 133))
                  VB
                  (overlay (txt "88%")
                           (cell 133)))
          HB
          (beside (overlay
                   (txt "...")
                   (cell 200))
                  VB
                  (overlay (txt "...")
                           (cell 133))
                  VB
                  (overlay (txt "...")
                           (cell 133))
                  VB
                  (overlay (txt "...")
                           (cell 133)))
          HB
          (beside (overlay
                   (txt "...")
                   (cell 200))
                  VB
                  (overlay (txt "...")
                           (cell 133))
                  VB
                  (overlay (txt "...")
                           (cell 133))
                  VB
                  (overlay (txt "...")
                           (cell 133)))
          HB
          (beside (overlay
                   (txt "...")
                   (cell 200))
                  VB
                  (overlay (txt "...")
                           (cell 133))
                  VB
                  (overlay (txt "...")
                           (cell 133))
                  VB
                  (overlay (txt "...")
                           (cell 133)))))

                
(define (produce-grade-summary c) empty-image)



(@htdf calculate-grades)
(@signature Course -> Course-Summary)
;; calculate grade summary of a course

(check-expect (calculate-grades CPSC110) CPSC110S)

;(define (render-grades c) (make-cs "CPSC 110" 100 (list )))

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
       (local [(define dropped
                 (drop-all-lowest (sort (section-grades s) <)
                                  (section-dropped s)))]
         (make-ss (section-name s)
                  (/ (foldr + 0 dropped) (length dropped))
                  (first (reverse dropped))
                  (first dropped))))
 
     ;; drop is Natural
     ;; number of lowest grades that should be dropped
     ;; log has to be sorted in increasing order
     (define (drop-all-lowest log drop)
       (cond [(empty? log) (list 0)]
             [(zero? drop) log]
             [else (drop-all-lowest (rest log) (sub1 drop))]))]

    (fn-for-course c)))




(@htdf render-grades)
(@signature Course-Summary -> Image)
;; produce a table out of grade summary
;; !!!


(define (render-grades cs) empty-image)













