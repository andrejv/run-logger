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

(defun get-track-points (file)
  "Reads the file in the GPX format."
  (with-open-file (s file :direction :input)
    (let* ((gpx (xmls:parse s))
           (trk (xmls:xmlrep-find-child-tag "trk" gpx))
           (trkseg (xmls:xmlrep-find-child-tag "trkseg" trk)))
      (mapcar #'(lambda (node)
                  (append
                   (mapcar #'read-from-string
                           (list (xmls:xmlrep-attrib-value "lat" node)
                                 (xmls:xmlrep-attrib-value "lon" node)
                                 (xmls:xmlrep-string-child (xmls:xmlrep-find-child-tag "ele" node))))
                   (list (xmls:xmlrep-string-child (xmls:xmlrep-find-child-tag "time" node)))))
              (xmls:xmlrep-find-child-tags "trkpt" trkseg)))))

(defun to-radians (deg)
  (/ (* deg pi) 180.0))

(defun distance (point-1 point-2)
  "Computes the distance between POINT-1 and POINT-2 given as (latitude longitude) pairs."
  (let* ((r 6371)
         (lat-distance (to-radians (- (first point-2) (first point-1))))
         (lon-distance (to-radians (- (second point-2) (second point-1))))
         (a (+ (* (sin (/ lat-distance 2))
                  (sin (/ lat-distance 2)))
               (* (cos (to-radians (first point-1)))
                  (cos (to-radians (first point-2)))
                  (sin (/ lon-distance 2))
                  (sin (/ lon-distance 2)))))
         (d (* 2 (atan (sqrt a) (sqrt (- 1 a))) r))
         (e (/ (- (third point-1) (third point-2)) 1000)))
    (sqrt (+ (* d d) (* e e)))
    d))

(defun track-length (points)
  "Returns the distance of the track given by POINTS gives as a list
of (latitude longitude) pairs."
  (loop for (p1 p2) on points
     when p2
     sum (distance p1 p2)))

(defun parse-time (str)
  "Gets the time from STR.

STR is a string `2015-01-10T10:13:44Z`"
  (let ((h (read-from-string (subseq str 11 13)))
        (m (read-from-string (subseq str 14 16)))
        (s (read-from-string (subseq str 17 19))))
    (list h m s)))
         
(defun parse-date (str)
  "Gets the date from STR.

STR is a string `2015-01-10T10:13:44Z`"
  (let ((y (read-from-string (subseq str 0 4)))
        (m (read-from-string (subseq str 5 7)))
        (d (read-from-string (subseq str 8 10))))
    (list y m d)))

(defun time-diff (t1 t2)
  "Computes the difference T1-T2 where T1 and T2 are lists (hour minute second)."
  (let ((d (mapcar #'- t1 t2)))
    (when (< (third d) 0)
      (incf (third d) 60)
      (decf (second d)))
    (when (< (second d) 0)
      (incf (second d) 60)
      (decf (first d)))
    d))

(defun import-file (file)
  "Imports the FILE in the GPX format and returns the length,
duration, date and start time of the track."
  (let* ((track (get-track-points file))
         (len (track-length track))
         (date (parse-date (fourth (first track))))
         (start (parse-time (fourth (first track))))
         (end (parse-time (fourth (first (last track)))))
         (duration (time-diff end start)))
    (values (format nil "~,3f" len) duration date start)))
