(use posix args fmt fmt-unicode fmt-color sqlite3)

; Data

(define-record course
               name
               done
               total-exc
               required)

(define (course-percentage course)
  (/ (course-done course) (course-total-exc course)))

(define (course-exc-required course)
  (* (course-total-exc course) (course-required course)))


(define (create-course name done total-exc required)
  (define (with-default x def)
    (if (sql-null#sql-null? x) def x))
  (make-course
    (with-default name "")
    (with-default done 0)
    (with-default total-exc 0)
    (with-default required 0)))

; Database

(define home
  (string-join (list
                     (get-environment-variable "HOME")
                     ".local"
                     "share"
                     "demolaskuri") "/"))
(create-directory home #t)

(define *db*
  (open-database (string-join (list home "demonstrations.db") "/")))

(define (init-db)
  (execute *db* "create table if not exists courses (name primary key, times, excs, required)")
  (execute *db* "create table if not exists demonstrations (name, done, timestamp default current_timestamp, foreign key (name) references demonstrations(name))"))

(init-db)

(define (add-course! name demonstrations excercises required)
  (execute *db*
           "insert or ignore into courses (name, times, excs, required) values (?, ?, ?, ?)"
           name demonstrations excercises required))

(define (mark-as-done! name n)
  (execute *db*
           "insert into demonstrations (name, done) values (?, ?)"
           name n))

(define (get-course! name)
  (row-to-course ( first-row *db* "select name, (select sum(done) from demonstrations where name=courses.name) as done, (times * excs * 1.0) as total_exc, required from courses where name=?" name)))

(define (get-courses!)
  (map-row create-course *db* "select name, (select sum(done) from demonstrations where name=courses.name) as done, (times * excs * 1.0) as total_exc, required from courses"))

(define (get-max-length!)
  (let ((max-length (car (first-row *db* "select max(length(name)) from courses"))))
    (if (sql-null#sql-null? max-length)
      0
      max-length)))

(define (row-to-course row)
  (apply create-course row))

; Pretty printing

(define (pretty-percentage percent)
  (cat (num (* 100 percent) 10 2) (dsp "%")))

(define (course-percentage-pretty course)
  (let* ((required (course-required course))
        (percent (course-percentage course))
        (pretty (pretty-percentage percent)))
    (if (>= percent required)
      (fmt-green pretty)
      (fmt-red pretty))))

(define (pretty-print-tabular courses)
  (let* ((name-column-width (+ 2 (get-max-length!)))
         (separator (dsp "|"))
         (row (lambda (name total done percent required)
                (apply cat
                       (cons separator
                       (intersperse (list
                                      (fmt-unicode (pad/both name-column-width (dsp name)))
                                      (pad/both 10 total) 
                                      (pad/both 10 done) 
                                      (fmt-unicode (pad/both 12 percent))
                                      (pad/both 10 required)
                                      nl)
                                    separator)))))
        (header (row "Course name" "Total" "Done" "Percent" "Required"))) 
    (apply cat (cons header (map (lambda (course)
           (apply row (map (lambda (fn) (fn course)) (list course-name
                                                           course-total-exc
                                                           course-done
                                                           course-percentage-pretty
                                                           course-required))))
         courses)))))


(define (list-courses courses)
  (fmt #t (pretty-print-tabular courses)))
(define (list-all)
  (list-courses (get-courses!)))
(define (show-course name)
  (list-courses (list (get-course! name))))
(define opts
  (list
    (args:make-option (d done) #:required "How many excercises have you done")
    (args:make-option (c course) #:required "Course name")
    (args:make-option (n new) #:none "Create a new course")
    (args:make-option (t times) #:required "How many demonstrations are held")
    (args:make-option (e excercises) #:required "How many excercises are in a demonstration")
    (args:make-option (r required) #:required "How many percentage of the assignments are required")
    (args:make-option (l list) #:none "List courses" (list-all))
    (args:make-option (h help) #:none "Help text" (usage))))

(define (usage)
  (print "Usage: " (car (argv)) " [options..]")
  (newline)
  (print (args:usage opts))
  (exit 0))

(receive (options operands)
         (args:parse (command-line-arguments) opts)
         (let ((course (alist-ref 'course options))
               (done (alist-ref 'done options))
               (new (assq 'new options))
               (times (alist-ref 'times options))
               (excercises (alist-ref 'excercises options))
               (required (alist-ref 'required options)))
           (cond ((and course done) (mark-as-done! course done))
                 ((and course new times excercises required)
                  (let ((required (string->number required))) 
                    (add-course! course times excercises 
                                  (if (> required 1) (/ required 100) required))))
                 (course (show-course course))
                 (done (usage))
                 (else (list-all)))
           ))
