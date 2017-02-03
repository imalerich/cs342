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
				'()											;; Table is empty nothing to do.
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
			(cons
				(car student-record)
				(cons
					(cadr student-record)
					(list ( /
						;; Divide the total quality points...
						((qualitypoints grade-table) (car  student-record))
						;; ... by the total number of classes graded.
						((coursesgraded grade-table) (car  student-record))
)))))))

;;;;;;;;;;;;;;
;;	3 (d)	;;
;;;;;;;;;;;;;;

;; Searches the grade-table for all students who have taken the given course.
;; Outputs the list of students, where each student is denoted by the results of f
;; followed by the grade the student earned in the requested course.
;; \param course:		Course code for any available course.
;; \param grade-table:	Grade table containing course records.
;; \param f:			A function taking a single student-id as parameter.
(define gradebook
	(lambda (course)
		(lambda (grade-table)
			(lambda (f)
				(if (null? grade-table)
					'()		;; The grade-table is empty, nothing to do.
					;; If the front of the grade-table is what we are looking for...
					(if (equal? course (car (car grade-table)))
						;; Construct a new list..
						(cons	
							;; Create a new list including the results of f and the students course grade.
							(append (f (cadr (car grade-table))) (list (caddr (car grade-table))))
							;; Add the recursive part of our list.
							(((gradebook course) (cdr grade-table)) f))

						;; Else we only need the recursive part.
						(((gradebook course) (cdr grade-table)) f)
))))))

;;;;;;;;;;;;;;;;;;
;;	Utilities	;;
;;;;;;;;;;;;;;;;;;

;; Implements the findstudentwithgpa function described in the assignment specification.
;; This is used for testing the gradebook function.
;; \param student-table:	List of all enrolled students in our database.
;; \param grade-table:		Table of grades in our database.
;; \param student:			Unique identifier for a student in the database.
(define findstudentwithgpa
	(lambda (student-table)
		(lambda (grade-table)
			(lambda (student)
				((studentgpa grade-table) ((getrecord student-table) student))
))))

;; Searches the student-table for the given student id.
;; Returns the student record for the given student from the corresponding
;; row in the student-table.
;; \param student-table:	List of all enrolled students in our database.
;; \param student:			Unique identifier for a student in the database.
(define getrecord
	(lambda (student-table)
		(lambda (student)
			(if (null? student-table)
				;; Exhausted the student table list, return an empty list.
				'()
				;; Otherwise check the front of the list to see if we have found them.
				(if (equal? (car (car student-table)) student)
					;; We have found our student, return the record.
					(car student-table)
					;; Else, not yet found, recurse to the rest of the list.
					((getrecord (cdr student-table)) student)
)))))

;; Looks for the given student id in the student-table, returns the name of that student.
;; \param student-table:	List of all enrolled students in our database.
;; \param student:			Unique identifier for a student in the database.
(define getstudentname
	(lambda (student-table)
		(lambda (student)
			(cadr ((getrecord student-table) student))
)))

;; Counts the total number of classes that have been graded for the given student.
;; \param grade-table:	Table of grades in our database.
;; \param student:		Unique identifier for a student in the database.
;; \return:				The numbe of courses graded for the given student.
(define coursesgraded
	(lambda (grade-table)
		(lambda (student)
			(if (null? grade-table)
				0 ;; Empty table has no courses for this student.
				(if (equal? (cadr (car grade-table)) student)
					;; This is one of the current students courses, increment courses graded.
					(+ 1 ((coursesgraded (cdr grade-table)) student))
					;; else don't count this class, only consider the remainder of the list.
					((coursesgraded (cdr grade-table)) student)
)))))

;; Computes the total number of quality points a student has earned in the given grade-table.
;; This searches the grade-table for each class the given student has taken, pulls their letter
;; grade from the table, converts it to a GPA, and returns the sum of all such classes.
;; \param grade-table:	Table of grades in our database.
;; \param student:		Unique identifier for a student in the database.
;; \return:				The total number of quality points earned by the student.
(define qualitypoints
	(lambda (grade-table)
		(lambda (student)
			(if (null? grade-table)
				0 ;; No more grades to check.
				(if (equal? (cadr (car grade-table)) student) ;; This grade is associated with the given student.
					;; Convert the letter grade to a scale, and sum it with the results from the rest of the list.
					(+ (gradetogpa (caddr (car grade-table))) ((qualitypoints (cdr grade-table)) student))
					;; Else just use the remainder of the list.
					((qualitypoints (cdr grade-table)) student)
)))))

;; Converts a single letter grade to a numerical gpa value.
;; \param scale:	List of grades and their corresponding numerical value.
;; \param letter:	The letter grade receieved for a course.
(define gradefromscale
	(lambda (scale)
		(lambda (letter)
			(if (null? scale)
				0.0 ;; Letter grade not found in the given scale.
				(if (equal? letter (car (car scale)))
					(cadr (car scale))
					((gradefromscale (cdr scale)) letter)
)))))

;; Compute the numerical gpa for a given letter grade
;; using the default grading scale.
(define gradetogpa 
	(gradefromscale 
	  '((A 4.0) (B 3.0) (C 2.0) (D 1.0) (F 0.0))
))

;; Provides a custom implementation of the built in length function.
;; Returns the length of the input list.
;; \param lst:	Any list.
(define immlength
	(lambda (lst)
		(if (null? lst)
			0							;; Empty list has size 0...
			(+ 1 (immlength (cdr lst)))	;; else length = 1 + length(tail).
)))

;;;;;;;;;;;;;;;;;;
;;	Exmaples	;;
;;;;;;;;;;;;;;;;;;

;; s-table:	student-table
;; course:	course id
;; review the usage
(define planincludes
	(lambda (s-table)
		(lambda (course)
			(if (null? s-table)
				'()
				(if (presentin (cadr (cdr (car s-table))) course)
					(cons (list (car (car s-table)) (cadr (car s-table)))
						((planincludes (cdr s-table)) course))

					((planincludes (cdr s-table)) course)
)))))

;; Not using lambda paramaters as I don't need to.
;; lst:		list of courses
;; course:	course id
(define (presentin lst course)
	(if (null? lst)
		#f
		(if (equal? (car lst) course)
			#t
			(presentin (cdr lst) course)
)))
