;;;
;;; Copyright (c) 2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.
;;;

(in-package :run-logger)

(defstruct route
  (name)
  (distance))

(defvar *db-file* nil
  "The location of the database file.")

(defvar *route-hash* nil
  "Routes hashtable")

(defvar *route-hooks* nil
  "Hooks for updating the list of routes.")

(defun route-populate-hash (lst)
  "Populates the routes from LST."
  (dolist (r lst)
    (setf (gethash (route-name r) *route-hash*) r)))

(defun route-update ()
  "Call all hooks on route list update."
  (dolist (f *route-hooks*)
    (funcall f)))

(defun route-add (route)
  "Add the route to the hashtable."
  (setf (gethash (route-name route) *route-hash*) route)
  (route-update)
  (write-database))

(defun route-delete (name)
  "Deletes the route with name NAME."
  (remhash name *route-hash*)
  (route-update)
  (write-database))

(defstruct activity
  (date)
  (start)
  (duration)
  (distance)
  (heartrate)
  (route))

(defvar *activity-hash* nil
  "The hashtable of all activities.")

(defvar *activity-hooks* nil
  "Hooks for updating activity list.")

(defun activity-id (activity)
  (format nil "~{~2,'0d~^/~}-~{~2,'0d~^:~}"
          (reverse (activity-date activity))
          (activity-start activity)))

(defun activity-populate-hash (lst)
  "Populates activity hashtable."
  (dolist (tr lst)
    (setf (gethash (activity-id tr) *activity-hash*) tr)))

(defun activity-update ()
  "Call all hooks on track list update."
  (dolist (f *activity-hooks*)
    (funcall f)))

(defun activity-add (activity)
  "Adds a activity to the activity hashtable."
  (setf (gethash (activity-id activity) *activity-hash*) activity)
  (activity-update)
  (write-database))

(defun activity-delete (id)
  "Delets the activity with ID."
  (remhash id *activity-hash*)
  (activity-update)
  (write-database))

(defun read-database ()
  "Reads the database file."
  (setf *activity-hash* (make-hash-table :test #'equal))
  (setf *route-hash* (make-hash-table :test #'equal))
  (when (probe-file *db-file*)
    (with-open-file (s *db-file* :external-format :utf-8)
      (with-standard-io-syntax
        (route-populate-hash (read s))
        (activity-populate-hash (read s)))))
  (activity-update)
  (route-update))

(defun write-database ()
  "Writes the database to the file."
  
  (let ((backup (merge-pathnames (make-pathname :type "bak") *db-file*)))
    (when (probe-file *db-file*)
      (cl-fad:copy-file *db-file* backup :overwrite t)))

  (with-open-file (s *db-file*
                     :direction :output
                     :if-exists :supersede
                     :external-format :utf-8)
    (let ((activities (loop for k being the hash-values in *activity-hash*
                     collect k))
          (routes (loop for k being the hash-values in *route-hash*
                     collect k)))
      (with-standard-io-syntax
        (print routes s)
        (print activities s)))))

(defun activities-in-year (year)
  "Returns the list of activities in the YEAR."
  (loop for value being the hash-values in *activity-hash*
     when (= (car (activity-date value)) year)
     collect value))

(defun activities-in-month (month year)
  "Returns the list of activities in the MONTH of YEAR."
  (loop for value being the hash-values in *activity-hash*
     when (and (= (car (activity-date value)) year)
               (= (cadr (activity-date value)) month))
     collect value))

(defun activities-on-route (route &key (by-duration nil))
  "Returns the list of activities on the ROUTE."
  (let ((activities (loop for value being the hash-values in *activity-hash*
                       when (string= route (activity-route value))
                       collect value)))
    (if by-duration
        (activities-sort-by-duration activities)
        (activities-sort-by-date activities))))

(defun activities-sort-by-duration (activities)
  "Sorts activities according to duration."
  (sort activities
        (lambda (a b)
          (< (compare-lists a b) 0))
        :key #'activity-duration))

(defun activities-sort-by-date (activities)
  "Sorts activities accordint to date."
  (sort activities
        (lambda (a b)
          (< compare-lists a b) 0)
        :key #'activity-date))

(defun activities-sort-by-date (activities)
  "Sorts acticities by date."
  (sort activities
        (lambda (a b)
          (> (compare-lists a b) 0))
        :key #'activity-date))

(defun all-activities ()
  "Returns the all activities."
  (activities-sort-by-date
   (loop for v being the hash-value in *activity-hash*
      collect v)))

(defun date-minus-days (date days)
  "For DATE = (YEAR MONTH DAY) returns a triple of the date before DAYS days."
  (let* ((u-date (encode-universal-time 0 0 0 (caddr date) (cadr date) (car date))))
    (multiple-value-bind (s m h day month year)
        (decode-universal-time (- u-date (* days 24 60 60)))
      (declare (ignore s m h))
      (list year month day))))

(defun compare-lists (date1 date2)
  "Returns -1 if DATE1 < DATE2, 0 if DATE1=DATE2 and 1 if DATE1>DATE2"
  (cond ((null date1) 0)
        ((< (car date1) (car date2)) -1)
        ((> (car date1) (car date2)) 1)
        (t (compare-lists (cdr date1) (cdr date2)))))

(defun activities-between (date1 date2 &key)
  "Returns the activities d between date1 < d <= date2."
  (loop for val being the hash-values in *activity-hash*
     when (and (< (compare-lists date1 (activity-date val)) 0)
               (<= (compare-lists (activity-date val) date2) 0))
     collect val))

(defun today ()
  "Returns current date as (YEAR, MONTH, DAY)."
  (multiple-value-bind (s m h day month year)
      (get-decoded-time)
    (declare (ignore s m h))
    (list year month day)))

(defun today-day-of-week ()
  "Returns the day of the week for today."
  (multiple-value-bind (s m h day month year dow)
      (get-decoded-time)
    (declare (ignore s m h day month year))
    dow))


(defun triple-to-mins (triple)
  "Turns (hour min sec) into minutes."
  (+ (* 60 (first triple))
     (second triple)
     (/ (third triple) 60.0)))

(defun mins-to-triple (mins)
  "Truns minutes into (hour min sec)."
  (multiple-value-bind (m s)
      (truncate mins)
    (multiple-value-bind (hours minutes)
        (truncate m 60)
      (list hours minutes (truncate (/ (* s 6000) 100))))))

(defun activities-statistics-sums (activities)
  "Calculates total distance and total duration of activities."
  (loop for a in activities
     sum (triple-to-mins (activity-duration a)) into dur
     sum (activity-distance a) into dist
     finally (return (list dur dist))))

(defun pace-disp (dec)
  "A nice display for pace."
  (multiple-value-bind (min sec)
      (truncate dec)
    (format nil "~d:~2,'0d min/km" min (truncate (/ (* 6000 sec) 100)))))

(defun speed-disp (speed)
  "A nice display for speed."
  (multiple-value-bind (km m)
      (truncate speed)
    (format nil "~d:~2,'0d km/h" km (truncate (* 100 m)))))

(defun activity-speed (act)
  "Display activity speed."
  (let ((duration (triple-to-mins (activity-duration act))))
    (if (> duration 0)
        (speed-disp (/ (activity-distance act) (/ duration 60.0)))
        "-")))

(defun time-disp (time)
  "A nice diplay for time"
  (format nil "~{~2,'0d~^:~}" time))

(defun date-disp (date)
  "A nice display for dates."
  (format nil "~{~2,'0d~^/~}" (reverse date)))

(defun activities-statistics (activities)
  "Returns the number of activities, total distance, total duration
and average pace of acitvities."
  (destructuring-bind (dur dist)
      (activities-statistics-sums activities)
    (let ((n (length activities)))
      (list n
            (format nil "~,2f km" dist)
            (time-disp (mins-to-triple dur))
            (if (> dist 0)
                (pace-disp (/ dur dist))
                "-")))))

(defun activities-by-weeks ()
  "Returns a list of activities separated by weeks."
  (let* ((dates (loop repeat 12
                   for diff = (1+ (today-day-of-week)) then 7
                   for date = (date-minus-days (today) diff) then (date-minus-days date diff)
                   collect date))
         (acts (loop for (date1 date2) on (reverse (cons (today) dates))
                  collect (activities-between date1 date2))))
    (loop
       repeat 12
       for act in acts
       for date in (mapcar (lambda (d) (date-minus-days d -1)) (reverse dates))
       collect (if act
                   (append (list date (length act)) (activities-statistics-sums act))
                   (list date 0 0.0 0.0)))))

(defun activities-histogram (data &key (key #'second) (height 100))
  "Computes the data for histograms."
  (let* ((dates (mapcar #'(lambda (p) (date-disp (first p))) data))
         (values (mapcar key data))
         (max-value (ceiling (apply #'max values))))
    (when (= max-value 0)
      (setf max-value 1))
    (list dates
          (mapcar #'(lambda (p) (truncate (* (/ p max-value) height))) values)
          max-value)))
