(use format sqlite3 matchable)

(define-record course
               name
               done
               total-exc
               required)

(define (create-course name done total-exc required)
  (define (with-default x def)
    (if (sql-null#sql-null? x) def x))
  (make-course
    (with-default name "")
    (with-default done 0)
    (with-default total-exc 0)
    (with-default required 0)))

(define *db* (open-database "demonstrations.db"))

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

(define (row-to-course row)
  (apply create-course row))

(define (enough-done? course)
  (let ((done (course-done course)) 
        (total (course-total-exc course))
        (required (course-required course)))
    (> (/ (if (sql-null#sql-null? done) 0 done) total) required)))
