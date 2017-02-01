#lang racket
(provide (all-defined-out)) ;; for us to test
;;(require racket/trace)		;; in case you want to use tracing
(require "DB.rkt")			;; to access the database definitions in DB.rkt

;;;;;;;;;;;;;;
;;	3 (a)	;;
;;;;;;;;;;;;;;

;; Apply input function f to each record in the given student-table, and return
;; the results of the operation as a new list.
;; \param student-table:	List of student records.
;; \param f:				Function to be applied to each student record.
(define applyonstds
	(lambda (student-table)
		(lambda (f)
			(if (null? student-table)
				student-table								;; Table is empty nothing to do.
				(cons 										;; Construct the returned list.
					(f (car student-table))					;; Apply f to the first element.
					((applyonstds (cdr student-table)) f)	;; Recurse on the tail of the list.
)))))

;;;;;;;;;;;;;;
;;	3 (b)	;;
;;;;;;;;;;;;;;

;; Replaces the course plan list in a student record 
;; with a single integer value equal to the length of the that list.
;; \param student-record:	A single student record.
(define numberofcourses
	(lambda (student-record)
		(cons 
			(car student-record)							;; Keep the first element the same.
			(cons											;; Note the second argument must be a list.
				(cadr student-record)						;; Keep the second element the same.
				(list (immlength (caddr student-record)))	;; Replace the last element with its length
))))

;;;;;;;;;;;;;;
;;	3 (c)	;;
;;;;;;;;;;;;;;

;; \param grade-table:		List of grades for students in a database.
;; \param student-record:	The record for a single student.
(define studentgpa
	(lambda (grade-table)
		(lambda (student-record)
			(caddr student-record)	;; This is the class list portion of the student record.
)))

;;;;;;;;;;;;;;;;;;
;;	Utilities	;;
;;;;;;;;;;;;;;;;;;

;; Counts the total number of classes that have been graded for the given student.
;; \param grade-table:	Table of grades in our database.
;; \param student:		Unique identifier for a student in the database.
;; \return:				The numbe of courses graded for the given student.
(define coursesgraded
	(lambda (grade-table)
		(lambda (student)
			0
)))

;; Returns the numerical gpa value for the course for the given student in the gpa table.
;; \param grade-table:	Table of grades in our database.
;; \param course:		Course code for a course in the students course-plan.
;; \param student:		Unique identifier for a student in the database.
;; \return:				Numerical value for the grade the student received,
;;						0.0 if the student has not yet taken the class.
(define gradefromtable
	(lambda (grade-table)
		(lambda (course)
			(lambda (student)
			  	(if (null? grade-table)
					0.0 ;; Course not found.
					;; Base case when the front of the grade-table list is the course we are looking for.
					(if (and (equal? course (car (car grade-table))) (equal? student (cadr (car grade-table))))
						;; Return the course grade as a gpa value.
						(gradetogpa (caddr (car grade-table)))
						;; else recurse to the tail of the list
						(((gradefromtable (cdr grade-table)) course) student)
))))))

;; Converts a single letter grade to a numerical gpa value.
;; \param letter:	The letter grade receieved for a course.
(define gradetogpa
	(lambda (letter)
		(if (equal? letter 'A)
			4.0
			(if (equal? letter 'B)
				3.0
				(if (equal? letter 'C)
					2.0
					(if (equal? letter 'D)
						1.0
						0.0
))))))

;; Provides a custom implementation of the built in length function.
;; Returns the length of the input list.
;; \param lst:	Any list.
(define immlength
	(lambda (lst)
		(if (null? lst)
			0							;; Empty list has size 0...
			(+ 1 (immlength (cdr lst)))	;; else length = 1 + length(tail).
)))
