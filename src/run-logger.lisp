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

(defun update-all-stats ()
  "Updates the ALL stats column."
  (let* ((all (loop for value being the hash-values in *activity-hash*
                 collect value))
         (stats (activities-statistics all)))
    (loop for s in stats
       for i = 0 then (1+ i)
       do (setf (var-value (format nil "grid-~a-0" i)) s))))

(defun update-year-stats ()
  "Updates the YEAR stats column."
  (let* ((year (car (today)))
         (activities (activities-in-year year))
         (stats (activities-statistics activities)))
    (loop for s in stats
       for i = 0 then (1+ i)
       do (setf (var-value (format nil "grid-~a-1" i)) s))))

(defun update-month-stats ()
  "Updates the CURR MONTH stats column."
  (let* ((today (today))
         (year (car today))
         (month (cadr today))
         (activities (activities-in-month month year))
         (stats (activities-statistics activities)))
    (loop for s in stats
       for i = 0 then (1+ i)
       do (setf (var-value (format nil "grid-~a-2" i)) s))))

(defun update-10-day-stats ()
  "Updates the CURR MONTH stats column."
  (let* ((today (today))
         (today-10 (date-minus-days today 10))
         (activities (activities-between today-10 today))
         (stats (activities-statistics activities)))
    (loop for s in stats
       for i = 0 then (1+ i)
       do (setf (var-value (format nil "grid-~a-3" i)) s))))

(defun update-prev-10-day-stats ()
  "Updates the CURR MONTH stats column."
  (let* ((today (date-minus-days (today) 10))
         (today-10 (date-minus-days today 10))
         (activities (activities-between today-10 today))
         (stats (activities-statistics activities)))
    (loop for s in stats
       for i = 0 then (1+ i)
       do (setf (var-value (format nil "grid-~a-4" i)) s))))

(defun update-histogram (c key color)
  "Updates the histogram in canvas c."
  (let* ((activities (activities-by-days))
         (c-width (window-reqwidth c))
         (c-height (window-reqheight c))
         (left-margin 30)
         (right-margin 10)
         (top-margin 10)
         (bottom-margin 20)
         (right-border (- c-width right-margin))
         (bottom-border (- c-height bottom-margin))
         (h-width (- c-width left-margin right-margin))
         (h-height (- c-height top-margin bottom-margin))
         (h-middle (+ top-margin (truncate h-height 2)))
         (h-q1 (+ top-margin (truncate h-height 4)))
         (h-q3 (+ h-middle (truncate h-height 4)))
         (hist (activities-histogram activities :key key :height h-height))
         (d (truncate h-width (length activities)))
         (d/2 (truncate d 2)))

    (canvas-delete c "all")

    (canvas-create-line c (list left-margin top-margin right-border top-margin) :fill "grey")
    (canvas-create-line c (list left-margin h-middle right-border h-middle) :fill "grey" :dash "-")
    (canvas-create-line c (list left-margin h-q1 right-border h-q1) :fill "grey" :dash ".")
    (canvas-create-line c (list left-margin h-q3 right-border h-q3) :fill "grey" :dash ".")
    (canvas-create-line c (list left-margin bottom-border right-border bottom-border) :fill "grey")
    (canvas-create-line c (list left-margin top-margin left-margin bottom-border) :fill "grey")
    (canvas-create-line c (list right-border top-margin right-border bottom-border) :fill "grey")
    (canvas-create-text c (list left-margin top-margin)
                        :text (format nil "~a" (third hist)) :anchor "e")
    (canvas-create-text c (list left-margin bottom-border) :text "0" :anchor "e")

    (loop
       for h in (second hist)
       for i = left-margin then (+ i d)
       do (canvas-create-rectangle c (list (+ 10 i) bottom-border (+ i d -10) (- bottom-border h))
                                   :fill color :outline color))
    (loop
       for date in (first hist)
       for i = (+ left-margin d/2) then (+ i d)
       do (canvas-create-text c (list i bottom-border) :text (subseq date 5) :anchor "n"))))

(defun make-graphs-tab (p)
  "This is the graphs notebook tab."
  (let* ((f (frame :parent p :padding '(10 10 10 10)))
         (fa (labelframe :parent f :text "Activities"))
         (fds (labelframe :parent f :text "Distance (km)"))
         (fdu (labelframe :parent f :text "Duration (min)"))
         (ca-a (canvas :parent fa :width 700 :height 120 :background "white"))
         (ca-ds (canvas :parent fds :width 700 :height 120 :background "white"))
         (ca-du (canvas :parent fdu :width 700 :height 120 :background "white")))
    (pack (list fa fds fdu) :padx 5 :pady 5)
    (pack (list ca-a ca-ds ca-du) :padx 5 :pady 5)

    (push (lambda ()
            (update-histogram ca-a #'second "blue"))
          *activity-hooks*)
    (push (lambda ()
            (update-histogram ca-ds #'fourth "navy"))
          *activity-hooks*)
    (push (lambda ()
            (update-histogram ca-du #'third "purple"))
          *activity-hooks*)
    
    f))

(defun make-stats-frame (p)
  "This is the bottom stats tab for the diary tab."
  (let ((f (frame :parent p :padding '(5 5 5 5))))
    
    (grid (label :parent f :text "All"
                 :anchor "center" :relief "raised" :width 12)
          :row 0 :column 1 :sticky "we")
    (grid (label :parent f :text "This year"
                 :anchor "center" :relief "raised" :width 12)
          :row 0 :column 2 :sticky "we")
    (grid (label :parent f :text "This month"
                 :anchor "center" :relief "raised" :width 12)
          :row 0 :column 3 :sticky "we")
    (grid (label :parent f :text "Last 10 days"
                 :anchor "center" :relief "raised" :width 12)
          :row 0 :column 4 :sticky "we")
    (grid (label :parent f :text "Prev. 10 days"
                 :anchor "center" :relief "raised" :width 12)
          :row 0 :column 5 :sticky "we")

    (grid (label :parent f :text "Activities:" :anchor "e")
          :row 1 :column 0 :sticky "we")
    (grid (label :parent f :text "Total distance:" :anchor "e")
          :row 2 :column 0 :sticky "we")
    (grid (label :parent f :text "Total time:" :anchor "e")
          :row 3 :column 0 :sticky "we")
    (grid (label :parent f :text "Avg. pace:" :anchor "e")
          :row 4 :column 0 :sticky "we")

    (dotimes (i 4)
      (dotimes (j 5)
        (grid (label :parent f :textvariable (format nil "grid-~a-~a" i j) :anchor "center")
              :row (1+ i) :column (1+ j))
        (setf (var-value (format nil "grid-~a-~a" i j)) "-")))

    (push #'update-year-stats *activity-hooks*)
    (push #'update-month-stats *activity-hooks*)
    (push #'update-10-day-stats *activity-hooks*)
    (push #'update-prev-10-day-stats *activity-hooks*)
    (push #'update-all-stats *activity-hooks*)

    (dotimes (i 6)
      (grid-columnconfigure f i :weight 1))

    (dolist (sl (grid-slaves f))
      (grid-configure sl :pady 2 :padx 2))
    f))

(defun make-diary-tab (nb)
  "This is the diary tab."
  (let* ((f (frame :parent nb :padding '(10 10 10 10)))
         (browser (frame :parent f))
         (list (treeview :parent browser :columns '("act")))
         (scroll (scrollbar :parent browser :orient "vertical"))
         (date (string-var))
         (start (string-var))
         (dur (string-var))
         (dist (string-var))
         (hr (string-var))
         (pace (string-var))
         (rt (string-var)))
    
    (setf (treeview-heading-text list "act") "Activity")
    (setf (treeview-column-anchor list "act") "center")
    (window-configure list :show '("headings"))
    
    (grid browser :row 0 :column 0 :rowspan 8 :sticky "nse")
    (grid list :row 0 :column 0 :sticky "nwes")
    (grid scroll :row 0 :column 1 :sticky "ns")
    (grid-columnconfigure browser 0 :weight 1)
    (grid-rowconfigure browser 0 :weight 1)
    (scrollbar-connect list scroll)

    (grid (button :text "Remove" :parent browser
                  :command (lambda ()
                             (let* ((idx (first (treeview-selection list)))
                                    (id (treeview-get list idx "act")))
                               (activity-delete id))))
          :row 2 :column 0 :columnspan 2 :sticky "e"
          :pady 3)

    (grid (label :text "Date:" :parent f)
          :row 0 :column 1 :sticky "e")
    (grid (label :textvariable date :parent f :width 10)
          :row 0 :column 2 :sticky "w")
    (grid (label :text "Start:" :parent f)
          :row 1 :column 1 :sticky "e")
    (grid (label :textvariable start :parent f)
          :row 1 :column 2 :sticky "w")
    (grid (label :text "Duration:" :parent f)
          :row 2 :column 1 :sticky "e")
    (grid (label :textvariable dur :parent f)
          :row 2 :column 2 :sticky "w")
    (grid (label :text "Distance:" :parent f)
          :row 3 :column 1 :sticky "e")
    (grid (label :textvariable dist :parent f)
          :row 3 :column 2 :sticky "w")
    (grid (label :text "Heart rate:" :parent f)
          :row 5 :column 1 :sticky "e")
    (grid (label :textvariable hr :parent f)
          :row 5 :column 2 :sticky "w")
    (grid (label :text "Pace:" :parent f)
          :row 4 :column 1 :sticky "e")
    (grid (label :textvariable pace :parent f)
          :row 4 :column 2 :sticky "w")
    (grid (label :text "Route:" :parent f)
          :row 6 :column 1 :sticky "e")
    (grid (label :textvariable rt :parent f :width 20)
          :row 6 :column 2 :sticky "w")

    (push (lambda ()
            (let ((routes (loop for k being the hash-key in *activity-hash*
                             collect k)))
              (treeview-delete list (treeview-children list ""))
              (dolist (item (sort routes #'string>))
                (treeview-insert list "" "end"
                                 :values (list item)))
              (treeview-selection-set list (first (treeview-children list "")))))
          *activity-hooks*)

    (bind-event list "<<TreeviewSelect>>"
                (lambda (e)
                  (declare (ignore e))
                  (let* ((idx (car (treeview-selection list)))
                         (tr-id (and idx (car (treeview-item-values list idx))))
                         (tr (gethash tr-id *activity-hash*)))
                    (when tr
                      (setf (var-value date) (date-disp (activity-date tr))
                            (var-value start) (time-disp (activity-start tr))
                            (var-value dur) (time-disp (activity-duration tr))
                            (var-value dist) (format nil "~a km" (activity-distance tr))
                            (var-value pace) (if (> (activity-distance tr) 0)
                                                 (pace-disp (/ (triple-to-mins (activity-duration tr))
                                                               (activity-distance tr)))
                                                 "-")
                            (var-value hr) (activity-heartrate tr)
                            (var-value rt) (activity-route tr))))))

    (grid (make-stats-frame f)
          :column 0 :row 8 :columnspan 3 :sticky "wen")
    
    (grid-columnconfigure f 0 :weight 1)
    (grid-columnconfigure f 2 :weight 1)
    (grid-rowconfigure f 7 :weight 1)

    (dolist (sl (grid-slaves f))
      (grid-configure sl :padx 5 :pady 5))

    f))

(defun make-routes-tab (nb)
  "This is the routes tab."
  (let* ((f (frame :parent nb :padding '(10 10 10 10)))
         (browser (frame :parent f :padding '(5 5 5 5)))
         (list (treeview :parent browser
                         :columns '("route")))
         (scroll (scrollbar :parent browser :orient "vertical"))
         (aframe (frame :parent f :padding '(5 5 5 5)))
         (acts (treeview :parent aframe
                         :columns '("act")))
         (acts-scroll (scrollbar :parent aframe :orient "vertical"))
         (r-name (string-var))
         (r-dist (float-var))
         (save (button :parent f :text "Save"))
         (delete (button :parent f :text "Remove")))

    (setf (treeview-heading-text list "route") "Route")
    (window-configure list :show '("headings"))

    (setf (treeview-heading-text acts "act") "Activity")
    (setf (treeview-column-anchor acts "act") "center")
    (window-configure acts :show '("headings"))
    
    (grid list :row 0 :column 0 :sticky "news")
    (grid scroll :row 0 :column 1 :sticky "sn")
    (scrollbar-connect list scroll)
    (grid-columnconfigure browser 0 :weight 1)
    (grid-rowconfigure browser 0 :weight 1)
    
    (grid browser :row 0 :column 0 :rowspan 5 :sticky "nse")

    (grid (label :text "Name:" :parent f)
          :row 0 :column 1 :sticky "e")
    (grid (entry :parent f :width 20 :textvariable r-name)
          :row 0 :column 2 :sticky "w")
    (grid (label :text "Distance:" :parent f)
          :row 1 :column 1 :sticky "e")
    (grid (spinbox :parent f :width 20
                   :from 0 :to 100 :increment 0.05 :textvariable r-dist)
          :row 1 :column 2 :sticky "w")
    (grid save :row 2 :column 2 :sticky "w")
    (grid delete :row 3 :column 2 :sticky "nw")
    

    (grid aframe :row 5 :column 0 :rowspan 4 :sticky "nse")
    (grid acts :row 0 :column 0 :sticky "nse")
    (grid-columnconfigure aframe 0 :weight 1)
    (grid-rowconfigure aframe 0 :weight 1)
    (grid acts-scroll :row 0 :column 1 :sticky "ns")
    (scrollbar-connect acts acts-scroll)

    (grid (label :text "Date:" :parent f)
          :row 5 :column 1 :sticky "e")
    (grid (label :textvariable "disp-date" :parent f :width 20)
          :row 5 :column 2 :sticky "w")
    (grid (label :text "Duration:" :parent f)
          :row 6 :column 1 :sticky "e")
    (grid (label :textvariable "disp-dur" :parent f :width 20)
          :row 6 :column 2 :sticky "w")
    (grid (label :text "Pace:" :parent f)
          :row 7 :column 1 :sticky "e")
    (grid (label :textvariable "disp-pace" :parent f :width 20)
          :row 7 :column 2 :sticky "w")

    (setf (var-value "disp-date") "-"
          (var-value "disp-dur") "-"
          (var-value "disp-pace") "-")
    
    (push (lambda ()
            (let ((routes (loop for k being the hash-key in *route-hash*
                             collect k)))
              (dolist (item (treeview-children list ""))
                (treeview-delete list item))
              (dolist (item routes)
                (treeview-insert list "" "end" :values (list item)))))
          *route-hooks*)

    (bind-event list "<<TreeviewSelect>>"
                (lambda (e)
                  (declare (ignore e))
                  (let* ((idx (first (treeview-selection list)))
                         (curr (and idx (treeview-get list idx "route")))
                         (route (and curr (gethash curr *route-hash*))))
                    (when route
                      (setf (var-value r-name) (route-name route))
                      (setf (var-value r-dist) (route-distance route))
                      (setf (var-value "disp-date") "-"
                            (var-value "disp-dur") "-"
                            (var-value "disp-pace") "-")
                      (treeview-delete acts (treeview-children acts ""))
                      (dolist (act (activities-on-route (route-name route)))
                        (treeview-insert acts "" "end" :values (list (activity-id act))))))))

    (bind-event acts "<<TreeviewSelect>>"
                (lambda (e)
                  (declare (ignore e))
                  (let* ((idx (first (treeview-selection acts)))
                         (curr (and idx (treeview-get acts idx "act")))
                         (act (and curr (gethash curr *activity-hash*))))
                    (when act
                      (setf (var-value "disp-date") (date-disp (activity-date act))
                            (var-value "disp-dur") (time-disp (activity-duration act))
                            (var-value "disp-pace")
                            (if (> (activity-distance act) 0)
                                (pace-disp (/ (triple-to-mins (activity-duration act))
                                              (activity-distance act)))
                                "-"))))))

    (bind-command save (lambda ()
                         (route-add (make-route :name (var-value r-name)
                                                :distance (var-value r-dist)))))
    (bind-command delete (lambda ()
                           (route-delete (var-value r-name))))

    (push (lambda ()
            (event-generate list "<<TreeviewSelect>>"))
          *activity-hooks*)
    (push (lambda ()
            (event-generate list "<<TreeviewSelect>>"))
          *route-hooks*)

    (grid-columnconfigure f 0 :weight 1)
    (grid-columnconfigure f 3 :weight 1)
    (grid-rowconfigure f 4 :weight 1)
    (grid-rowconfigure f 8 :weight 5)
    
    (dolist (sl (grid-slaves f))
      (grid-configure sl :padx 5 :pady 5))
    (dolist (sl (grid-slaves acts))
      (grid-configure sl :padx 5 :pady 5))

    f))

(defun make-new-tab (nb)
  "This is the log activity tab."
  (let* ((f (frame :parent nb :padding '(5 5 5 5)))
         (day (integer-var))
         (month (string-var))
         (year (integer-var))
         (months '("January" "February" "March" "April"
                   "May" "June" "July" "August" "September"
                   "October" "November" "December"))
         (h (integer-var))
         (m (integer-var))
         (s (integer-var))
         (s-h (integer-var))
         (s-m (integer-var))
         (s-s (integer-var))
         (dist (float-var))
         (hr (integer-var))
         (route (string-var))
         (routes (combobox :parent f :width 6 :textvariable route)))

    (grid (label :text "Date:" :parent f)
          :row 0 :column 0 :sticky "es")
    (grid (combobox :parent f :textvariable day :width 3
                    :values '( 1  2  3  4  5  6  7  8  9 10
                              11 12 13 14 15 16 17 18 19 20
                              21 22 23 24 25 26 27 28 29 30
                              31))
          :row 0 :column 1 :sticky "esw")
    (grid (combobox :parent f :textvariable month
                    :values months :width 10)
          :row 0 :column 2 :columnspan 2 :sticky "esw")
    (grid (spinbox :parent f :from 2000 :to 2100 :textvariable year :width 6)
          :row 0 :column 4 :sticky "sw")
    (grid (label :text "Start:" :parent f)
          :row 1 :column 0 :sticky "e")
    (grid (spinbox :parent f :from 0 :to 24 :textvariable s-h :width 3)
          :row 1 :column 1 :sticky "ew")
    (grid (spinbox :parent f :from 0 :to 60 :textvariable s-m :width 3)
          :row 1 :column 2 :sticky "ew")
    (grid (spinbox :parent f :from 0 :to 60 :textvariable s-s :width 3)
          :row 1 :column 3 :sticky "ew")
    (grid (label :text "h/m/s" :parent f)
          :row 1 :column 4 :sticky "w")

    (grid (label :text "Duration:" :parent f)
          :row 2 :column 0 :sticky "e")
    (grid (spinbox :parent f :from 0 :to 24 :textvariable h :width 3)
          :row 2 :column 1 :sticky "ew")
    (grid (spinbox :parent f :from 0 :to 60 :textvariable m :width 3)
          :row 2 :column 2 :sticky "ew")
    (grid (spinbox :parent f :from 0 :to 60 :textvariable s :width 3)
          :row 2 :column 3 :sticky "ew")
    (grid (label :text "h/m/s" :parent f)
          :row 2 :column 4 :sticky "w")
    (setf (var-value h) 0
          (var-value m) 0
          (var-value s) 0)

    (grid (label :text "Distance:" :parent f)
          :row 3 :column 0 :sticky "e")
    (grid (spinbox :parent f :textvariable dist :width 6 :from 0 :to 100 :increment 0.05)
          :row 3 :column 1 :columnspan 2 :sticky "we")
    (grid (label :text "km" :parent f)
          :row 3 :column 3 :sticky "w")
    (setf (var-value dist) 5.0)

    (grid (label :text "Heart rate:" :parent f)
          :row 4 :column 0 :sticky "e")
    (grid (spinbox :textvariable hr :width 6 :from 0 :to 200 :parent f)
          :row 4 :column 1 :columnspan 2 :sticky "we")
    (grid (label :text "bpm" :parent f)
          :row 4 :column 3 :sticky "w")
    (setf (var-value hr) 140)

    (grid (label :text "Route:" :parent f)
          :row 5 :column 0 :sticky "e")
    (grid routes
          :row 5 :column 1 :columnspan 3 :sticky "we")
    
    (grid (button :text "Save" :parent f
                  :command (lambda ()
                             (activity-add
                              (make-activity
                               :date (list (var-value year)
                                           (1+ (position (var-value month)
                                                         months
                                                         :test #'string=))
                                           (var-value day))
                               :start (list (var-value s-h)
                                            (var-value s-m)
                                            (var-value s-s))
                               :duration (list (var-value h)
                                               (var-value m)
                                               (var-value s))
                               :distance (var-value dist)
                               :heartrate (var-value hr)
                               :route (var-value route)))
                             (message-box "Saved" :title "OK" :detail "Activity has been saved.")))
          :row 6 :column 1 :columnspan 2 :sticky "wn")

    (push (lambda ()
            (let ((rts (loop for k being the hash-key in *route-hash*
                          collect k)))
              (window-configure routes :values rts)))
          *route-hooks*)
    (bind-event routes "<<ComboboxSelected>>"
                (lambda (ev)
                  (declare (ignore ev))
                  (let* ((r (gethash (var-value route) *route-hash*)))
                    (when r
                      (setf (var-value dist) (route-distance r))))))

    (grid-columnconfigure f 0 :weight 2)
    (grid-columnconfigure f 4 :weight 2)
    (grid-columnconfigure f 1 :weight 1)
    (grid-columnconfigure f 2 :weight 1)
    (grid-columnconfigure f 3 :weight 1)
    
    (grid-rowconfigure f 0 :weight 1)
    (grid-rowconfigure f 6 :weight 3)

    (dolist (sl (grid-slaves f))
      (grid-configure sl :padx 5 :pady 5))

    (multiple-value-bind (sec min hour d m y dy dp z)
        (get-decoded-time)
      (declare (ignore dy dp z))
      (setf (var-value year) y
            (var-value month) (nth (1- m) months)
            (var-value day) d
            (var-value s-h) hour
            (var-value s-m) min
            (var-value s-s) sec))
    
    f))

(defun build-frame (root)
  (setf (window-title root)"Run Logger")
  (setf (window-resizable root) '(nil nil))
  (let* ((f (frame :parent root))
         (nb (notebook :parent f)))
    (pack f :expand t :fill "both")
    (pack nb :expand t :fill "both" :padx 6 :pady 6)
    
    (notebook-add nb (make-new-tab nb) :text "Log activity" :underline 0)
    (notebook-add nb (make-diary-tab nb) :text "Diary" :underline 0)
    (notebook-add nb (make-graphs-tab nb) :text "Graphs" :underline 0)
    (notebook-add nb (make-routes-tab nb) :text "Routes" :underline 0)

;    (notebook-enable-traversal nb)

    (setf (notebook-select nb) (nth 1 (notebook-tabs nb)))))

(defun main ()
  (with-tk-root (root)
    
    (setf *route-hooks* nil
          *activity-hooks* nil
          *route-hash* (make-hash-table :test #'equal)
          *activity-hash* (make-hash-table :test #'equal))

    #+darwin
    (let* ((home (pathname-directory (user-homedir-pathname)))
           (libdir (make-pathname :directory (append home '("Library" "Application Support" "Run Logger"))))
           (settings (merge-pathnames "db.lisp" libdir)))
      (ensure-directories-exist settings)
      (setf *db-file* settings))
    #+(or win32 windows)
    (let* ((home (pathname-directory (user-homedir-pathname)))
	   (last-2 (last home 2))
	   (libdir (make-pathname :directory
				  (if (and (string= "AppData" (car last-2))
					   (string= "Roaming" (cadr last-2)))
				      (append home '("RunLogger"))
				      (append home '("AppData" "Roaming" "RunLogger")))))
	   (settings (merge-pathnames "db.lisp" libdir)))
      (ensure-directories-exist settings)
      (setf *db-file* settings))
    
    #-(or darwin win32 windows)
    (setf *db-file* (merge-pathnames ".run-logger.lisp" (user-homedir-pathname)))

    #+darwin
    (let* ((mbar (menu :parent root))
           (apple (menu :parent mbar :tk-name "apple")))
      (menu-add-command apple "About Run Logger..." #'tk-mac-about-panel)
      (menu-add-cascade mbar "" apple)
      (window-configure root :menu mbar))
    
    (build-frame root)
    
    (let ((sw (window-screenwidth root))
          (sh (window-screenheight root)))
      (setf (window-geometry root) (list ()
                                         (truncate (- sw 800) 2)
                                         (truncate (- sh 600) 2))))
    
    (read-database)))
