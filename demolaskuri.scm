(use srfi-69)

(define *db* (make-hash-table))

(define-record course
               name times excs done)

(define (define-course name times excs)
  (make-course name times excs 0))

(define (course-percentage course)
  (let ((total (* (course-times course) (course-excs course))))
    (string-append 
      (number->string ( * 100  (/ (course-done course) total)))
      "%")))

(define (save-course course)
  (hash-table-set! *db* (course-name course) course))

(define (mark-demonstrations name n)
  (hash-table-update! *db*
                      name 
                      (lambda (course)
                        (course-done-set! course (+ (course-done course) n))
                        course)))
