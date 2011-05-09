
#|
To run a human subject enter: (run-set-human a-string-identifying-subject ) ;;(run-set "1111")
  This displays the board and creates a log file named MG-xxxx  where xxxx = subject id you entered.
  The file is created in the users home directory (for a PC it will be under documents and setttings.

  Be sure to close the Set Game window so that the log file will be closed.

  Calling the function (stop-set) will also close the window and file

To run with a model:
 1. Put this file in the ACT-R 6 other-files folder. Then load ACT-R and ACT-R will load this file.
 2. Enter  (run-set-model a-string-identifying-model-subject  )  ;;(run-set "M123" ) into the listener
    A log file for the model run will be created, just as if a human had played the game.

The board will be displayed and the current visicon will be printed out and your model will execute.

Be sure to close the Set Game window so that the log file will be closed.

After the model executes the easiest way to rerun the model is to reload this file. 

The parameter +actr-run-time+ contains the act-r run time

The parameter +set-board+ if nil then randomly chooses a game else should be a value 0 <= +set-board+ < (length +games+)


|#
(defmacro defclassic (class supers &rest slots) 
  `(defclass 
	,class 
	,supers    
	,(mapcar 
	  #'(lambda (s)                   ; s  = slot specification 
	     (flet ((build (sn)           ; sn = slot name  
		      (list sn ':accessor sn ':initarg
			    (read-from-string                       
			     (concatenate 'string ":" (symbol-name sn))))))
       
	       (cond ((atom s) (append (build s) '(:initform nil)))
                     ((null (cddr s)) (append (build (first s)) (list ':initform (second s))))
                     ((eql t (second s))
                      (append (build (first s)) 
                              (nthcdr 2 s)
                              (if (not (member :initform (nthcdr 2 s))) '(:initform nil))))
                     (t s))))
	  slots)))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
     (progn ,@body)))

(defmacro while (test &rest body)
  `(do ()
      ((not ,test))
      ,@body))

(defun convert-to-list (x)
  (if x (read-from-string (concatenate 'string "(" (string-trim '(#\Space #\. #\?) x) ")"))))


(defparameter +actr-run-time+ 10)
(defparameter +game-timeout+ 360)

(defparameter +games+ '(
                        ( 8 14 15 18 19 20 27 31 46 47 62 75) ;;game 0 = +set-board+
                        ( 4  9 14 15 21 33 36 37 59 76 77 78) ;;game 1
                        ( 4 11 23 29 37 40 41 63 65 72 74 76) ;;game 2
                        ( 2  8 14 17 26 29 35 47 50 55 57 59) ;;game 3
                        (11 20 33 37 38 39 40 44 46 56 64 68) ;;game 4
                        ( 2  6 25 37 49 51 58 66 73 74 77 80) ;;game 5
                        ( 5 12 18 19 28 31 34 59 61 76 77 80) ;;game 6
                        ( 9 10 18 22 27 34 38 42 51 76 77 80) ;;game 7
                        ( 2 29 30 37 40 43 50 53 61 68 70 75) ;;game 8 easy
                        (10 19 36 39 41 43 44 53 60 67 76 78) ;;game 9 
                        ( 2 15 17 20 28 33 45 48 49 50 66 80) ;;game 10 5 of 6
                        (13 15 18 19 25 40 41 42 56 61 65 69) ;;game 11 6 of 6 4 min 2 sec
                        ( 2  7 10 21 29 34 37 54 56 59 63 64) ;;game 12  3 easy 3 hard
                        ( 6 14 21 25 32 36 40 46 47 58 71 77) ;;game 13
                        ( 8 32 44 53 56 67 73 74 78 79 80 81) ;;game 14
                        ( 9 25 29 33 49 57 60 65 66 68 69 81) ;;game 15 3 easy, 5 of 6
                        ))
(defparameter +trial-list-to-game-map+ '(0 4 8 10 2 12)) ;;indexs +games+
(defparameter +latin-square+ '((0 1 2 3 4 5) (1 3 0 5 2 4) (2 0 4 1 5 3) (3 5 1 4 0 2) (4 2 5 0 3 1) (5 4 3 2 1 0))) ;;indexs +trial-list-to-game-map+
(defparameter +cnds+ '(A B C D E F)) ;;position
(defparameter +trial-list+ '(0 1 2 3 4 5))  ;;set by cw condition from +latin-square+
(defparameter +set-board+ (nth (first +trial-list+) +trial-list-to-game-map+ )) ;current game

(when (and +set-board+ (or (minusp +set-board+) (>= +set-board+ (length +games+))))
    (capi:display-message (format nil "Invalid +set-board+ parameter ~S" +set-board+))
    )


(defconstant +game-size+ 12)


(defclass card-check-box (capi:check-button)
 ((value :accessor value :initarg :value :initform nil)
 (feat :accessor feat :initform nil)
 (myimage :initform nil :accessor myimage :initarg :myimage)
 ))

(defclass card-display (capi:drawn-pinboard-object )
  ((myimage :initform nil :accessor myimage :initarg :myimage)
   (value :accessor value :initarg :value :initform nil)
   (feat :accessor feat :initform nil))
  )

(defun make-cards (images num-cols)
  (let* ((cards nil) (l1 nil) (l2 nil)
         (ran-lst (permute-a-list (nth (aif +set-board+ it (random (length +games+))) +games+))))  ;;
    (log-event nil 'make-cards ran-lst)
    (dotimes (i (floor +game-size+ num-cols) )
      (dotimes (j num-cols)
        (let* ((idx (+ j (* i num-cols)))
               (val (nth idx ran-lst)) 
               (c (make-instance 'card-check-box  :data (+ j (* i num-cols)) :text ""
                           :value val ;;(- val 1)
                           :selection-callback 'process-card :retract-callback 'process-retract))
               (disp (make-instance 'card-display  :visible-min-width 150 :visible-min-height 200
                                    :value val ;;(- val 1)
                               :display-callback 'draw-card
                               )))
          (setf (myimage disp) (second (find val images :key (lambda(x) (parse-integer (first x) :junk-allowed t)) )))           
          (setf (myimage c) (second (find val images :key (lambda(x) (parse-integer (first x) :junk-allowed t) ))))  
          (push disp l1)
          (push c l2 ))))
    (let ((idx1 0) (idx2 0))
      (dotimes (i (floor +game-size+ num-cols))
        (setq cards (append cards (subseq l1 idx1 (incf idx1 num-cols )) (subseq l2 idx2 (incf idx2 num-cols ))))))
    cards))

(defmethod draw-card (pane (self card-display) &rest args)
  (capi:with-geometry self
    (gp:draw-rectangle pane capi:%x%  capi:%y% capi:%width% capi:%height%)
    (if (draw-enable (set-game))
     (aif (myimage self) (gp:draw-image pane (gp:convert-external-image pane it) (+ 5 capi:%x%) (+ 5 capi:%y%) )))))

(defclass set-item-display (capi:drawn-pinboard-object)
 ((image :initform nil :accessor image)
  (value :accessor value :initarg :value :initform nil)
  (feat :accessor feat :initform nil)
  (ind :accessor ind :initform nil :initarg :ind)))

(defun make-sets-found ()
  (let ((res nil))
    (dotimes (i 18 (reverse res))
        (push (make-instance 'set-item-display :ind i
                             :display-callback 'update-set-display
                             :visible-min-width 150 :visible-min-height 120) res))))

(capi:define-interface set-board ()
  ((trial-num :initform 0 :accessor trial-num)
   (draw-enable :initform nil :accessor draw-enable)
   (game-size :initarg :game-size :accessor game-size :initform +game-size+)
   (num-sets-found :accessor num-sets-found :initform 0)
   (current-cards :initarg :current-cards :accessor current-cards :initform nil)
   (current-sets :initform nil :accessor current-sets)
   (subject-id :initarg :subject-id :accessor subject-id  :initform nil)
   (f-stream :initarg :f-stream :accessor f-stream :initform nil)
   (modelp :initarg nil :accessor modelp :initarg :modelp )
   (second-timer :initform -1 :accessor second-timer :initarg :second-timer)
   (timer-obj :accessor timer-obj :initform nil)
   )
  (:panes
   (game-btn capi:push-button :title "" :text "Start" :data 'start :callback 'game-btn-callback :accessor game-btn)
   (timer capi:title-pane :title "Game Timer" :text "00:00" :x 10 :y 10 :accessor timer)
   (gameno capi:title-pane :title "Game Number" :text " " :x 10 :y 30 :accessor gameno))
  (:layouts
   (card-list capi:grid-layout (make-cards (get-images) 4) :columns 4 :accessor card-list :x 200 :y 50 )
   (sets-found capi:grid-layout (make-sets-found) :accessor sets-found :columns 3 :x 850 :y 50)
 ;  (game-row capi:row-layout '(card-list sets-found) :x 0 :y 0 :accessor game-row)
   (pinboard capi:pinboard-layout '(timer gameno card-list sets-found) :accessor pinboard
              :draw-pinboard-objects :local-buffer) 
   (game-btn-layout capi:row-layout '(nil game-btn nil))
   (main capi:column-layout '( pinboard game-btn-layout))
   )
  (:default-initargs :title "Set Game Deck"
   :window-styles '(:BORDERLESS)
   :initial-focus nil
    :layout 'main

 ;   :best-x 0 ;(floor (- (capi:screen-width (capi:convert-to-screen)) 1200) 2)
 ;   :best-y 0 ;(floor (- (capi:screen-height (capi:convert-to-screen)) 900) 2)
    :best-width (capi:screen-width (capi:convert-to-screen));1200
    :best-height (capi:screen-height (capi:convert-to-screen)) ;900

    ))

(defmethod (setf second-timer) :after (val (win set-board))
  (when (>= val +game-timeout+ ) 
    (capi:display-message "Time for game has expired")
    (log-event (f-stream win) 'game-timeout :trial (trial-num win) :found (length (current-sets win)))
    (game-btn-callback 'next win)))
      
(defmethod timer-fn ((win set-board))
  (capi:apply-in-pane-process (timer win)
    (lambda (tm)
      (multiple-value-bind (mn sec) (floor (incf (second-timer win)) 60)
        (setf (capi:title-pane-text tm) (format nil "~d:~2,'0d" mn sec))))
    (timer win)))

(defun game-btn-callback (data interface)
  (let ((btn (game-btn interface)))
    (case data
      (start
       (setf (capi:item-text btn) "Next")
       (setf (capi:item-data btn) 'next)
       (setf (draw-enable interface) t)
       (setf (capi:title-pane-text (gameno interface)) "1") 
       (gp:invalidate-rectangle (pinboard interface))
       ;;initialize
       (setf (second-timer interface) 0)
       (mp:schedule-timer-relative (setf (timer-obj interface) (mp:make-timer 'capi:execute-with-interface interface 'timer-fn interface)) 1 1)
       (log-event (f-stream interface) 'set-board (mapcar 'value (capi:layout-description (card-list interface))))
       (log-event (f-stream interface) 'start-trial (trial-num interface) 'game +set-board+ +trial-list+ ))
      (next
       (cond ((< (1+ (trial-num interface)) (length +trial-list+))
              (setf +set-board+ (nth (nth (incf (trial-num interface)) +trial-list+) +trial-list-to-game-map+))
              (setf (capi:title-pane-text (gameno interface)) (write-to-string (1+ (trial-num interface))))
              (setf (capi:layout-description (card-list interface)) (make-cards (get-images) 4))
              (setf (capi:layout-description (sets-found interface)) (make-sets-found))
              (capi:apply-in-pane-process interface #'capi:raise-interface interface)
              (gp:invalidate-rectangle (pinboard interface))
              ;;;continue play
              (aif (timer-obj interface) (mp:unschedule-timer it))
              (setf (second-timer interface) 0)  
              (mp:schedule-timer-relative (setf (timer-obj interface) (mp:make-timer 'capi:execute-with-interface interface 'timer-fn  interface)) 1 1)
              (log-event (f-stream interface) 'set-board (mapcar 'value (capi:layout-description (card-list interface))))
              (log-event (f-stream interface) 'start-trial (trial-num interface) 'game +set-board+ ))
             (t
              (aif (timer-obj interface) (mp:unschedule-timer it))
              (stop-set interface))))
       
      )))

(defmethod update-set-display (pane (self set-item-display) &rest args)
  (declare (ignore args))
  (capi:with-geometry self
  ;  (gp:with-graphics-transform (pane (gp:make-transform .8 0 0 .8))
    (gp:draw-rectangle pane (+ 0 capi:%x%)  (+ 0 capi:%y%) capi:%width% capi:%height%)
    (awhen (image self) ;(break "dislay ~S" (list capi:%x%  capi:%y% capi:%width% capi:%height%  ))
       
         (gp:draw-image pane (gp:convert-external-image pane it)  (+ 5 capi:%x%) (+ 5 capi:%y%) )))) ;)

(defmethod process-retract (data (interface set-board)) ;
"Callback function for clicking on a card check-box"
  (with-slots ( current-cards f-stream ) interface   
    (let* ((cards (get-cards))
           (card (find data cards :key #'capi:item-data)))
      (log-event f-stream 'retract data)
      (setf current-cards (remove card current-cards)))
))

(defmethod clear-current-set ((interface set-board))
  (with-slots (current-cards f-stream) interface
    (dolist (c current-cards)
      (setf (capi:button-selected c) nil))
    (log-event f-stream 'clearing-set)
    (setf current-cards nil)))

(defmethod process-card (data (interface set-board)) ;
"Callback function for clicking on a card check-box"
  (with-slots ( current-cards f-stream num-sets-found) interface   
    (let* ((cards (get-cards))
           (card (find data cards :key #'capi:item-data)))
      (when (not (member card current-cards))
        (log-event f-stream 'click data )
        (setf current-cards (cons card current-cards  )) ;(capi:display-message (format nil "~S" DATA))
        (when (eql 3 (length  current-cards))
          (let ((vals (mapcar 'value current-cards)))
            (cond ((set-p vals)
                   (cond ((duplicate-set-p vals (current-sets interface))
                          (log-event f-stream 'duplicate-set num-sets-found vals)
                          (if (null (modelp interface)) (capi:display-message "Duplicate Set")))
                         (t
                          (let ((set-disp (remove nil (capi:layout-description (sets-found interface))))
                                (start-idx (* 3 num-sets-found)))
                            (dotimes (i 3)
                              (let ((idx (+ start-idx i)))
                                (if (null (nth idx set-disp)) (log-event f-stream 'process-card-error (capi:layout-description (sets-found interface))))
                                (when (nth idx set-disp)
                                  (setf (image (nth idx set-disp)) (myimage (nth i current-cards)))  
                                  (update-set-item (value card) (nth idx set-disp) )
                                  (gp:invalidate-rectangle (nth idx set-disp))))))
                          (incf num-sets-found)
                          (push vals (current-sets interface))
                          (log-event f-stream 'set-found num-sets-found vals ))))
                  (t
                   (log-event f-stream 'not-a-set num-sets-found vals)
                   (if (null (modelp interface)) (capi:display-message "Not a Set"))))
            (clear-current-set interface)
          ) )))))

(defun duplicate-set-p (cur-set sets)
  (dolist (s sets nil)
    (if (null (set-difference cur-set s)) (return t))))


(defmethod update-set-item (val (obj set-item-display))
  (setf (value obj) val))

(defun log-event (fs event &rest data)
  (cond (fs
         (write event :stream fs) (write-char #\tab fs)
         (write (get-internal-real-time) :stream fs)  (write-char #\tab fs)
         (dolist (val data)
           (write val :stream fs)
           (write-char #\tab fs))
         (write-char #\newline fs))
        (t
         (log-info `(,event ,@data)))))
         
(defun permute-a-list (lis)
  "Return a random permutation of the list"
  (do* ((item (nth (random (length lis)) lis) (nth (random (length temp)) temp))
        (temp (remove item lis :count 1) (remove item temp :count 1))
        (result (list item) (cons item result)))
       ((null temp) result)))

(defun get-attributes (n) 
  (multiple-value-bind (a b) (floor (- n 1) 27)  
    (multiple-value-bind (c d) (floor b 9)
      (multiple-value-bind ( e f) (floor d 3)
      (list a c e f)))))  ;texture shape color number

(defun get-attributes-sym (n)
  (destructuring-bind (texture shape color number) (get-attributes n)
    (list (nth texture '(solid stripped open))
          (nth shape '(squiggle diamond oval))
          (nth color '(red purple green))
          (nth number '(1 2 3)))))

(defun set-p (n)
  (destructuring-bind (attr-1 attr-2 attr-3) (mapcar 'get-attributes n) 
    (equal  (mapcar (lambda(x y z) (or (= x y z) (and (/= x y) (/= x z) (/= y z)))) attr-1 attr-2 attr-3)
            '(t t t t))))

(defun set-solution (lst) ;lst = 1 game
  (let ((solutions nil) c1 c2 c3 lst2)
    (dotimes (i 12)
      (setq c1 (nth i lst))
      (dotimes (j 11)
        (setq c2 (nth j (setq lst2 (remove c1 lst))))
        (dotimes (k 10)
          (setq c3 (nth k (remove c2 lst2)))
          (if (set-p (list c1 c2 c3)) (setq solutions (adjoin (sort (list c1 c2 c3) #'<) solutions :test 'equal))))))
    (dolist (sol solutions)
      (format t "~%~S" (mapcar 'get-attributes-sym sol)))))
   
(let* ((set-game nil)
       (path (directory-namestring (current-pathname)))
       (files (directory (merge-pathnames "set-images/*.bmp" path)))
       (images (mapcar (lambda (f) (list (file-namestring f) (gp:read-external-image f))) files)))
  (defun path () path)
  (defun get-path () path)
  (defun set-game (&optional g) (if g (setq set-game g) set-game))
  (defun get-images () images)
)

(defun run-set-model (subj-id &key (cnd nil))
  (run-set subj-id :model t :cnd cnd))

(defun run-set-human (subj-id &key (cnd nil))
  (run-set subj-id :model nil :cnd cnd))

(defun run-set-cw ()
  (run-set-human nil :cnd (ignore-errors (read-from-string (task-condition *cw*)) )))

(let ((cw-task nil))
#+:cogworld
  (setq cw-task (register-task "Set" :run-function 'run-set-cw))
  (defun get-cw-task () cw-task)
)

(defun run-set (subj-id &key  (model t) (cnd nil))
  (let ((actr-loaded  (member :act-r-6.0 *features*))) 
    (cond ((or (null model) actr-loaded) 
           (let ((fs (if subj-id (open (concatenate 'string  "~/SET-" subj-id) :direction :output :if-exists :overwrite :if-does-not-exist :create))))
             (if cnd (setf +trial-list+ (nth (position cnd +cnds+) +latin-square+)))
             (setf +set-board+ (nth (first +trial-list+) +trial-list-to-game-map+ ))
             (set-game (make-instance 'set-board :subject-id subj-id :f-stream fs :modelp model))
               
             (show-game (set-game) model)
             (log-event fs 'start-game :condition cnd (task-condition *cw*))))
          (t
           (capi:display-message " ACT-R is not loaded" )))))

(defun stop-set (&optional (interface (set-game)))
  (capi:display-message "Experiment is Finished~%~%Thank-you!!")
  (log-event (f-stream interface) 'stop 'game)
  (awhen (f-stream interface)
         (close it)
         (setf (f-stream interface) nil))
  (capi:destroy interface)
  (awhen (get-cw-task)
    (task-finished it)
    (stop-eyetracking)))

(defmethod show-game ((win set-board) modelp)
  (declare (ignore modelp))
  (capi:display win)
  (if (get-cw-task) (start-eyetracking))
  )

(defun get-cards ()
   (remove nil (mapcar (lambda(x) (if (typep x 'card-check-box) x)) (capi:layout-description (card-list (set-game))))))

(defun get-card-displays ()
   (remove nil (mapcar (lambda(x) (if (typep x 'card-display) x)) (capi:layout-description (card-list (set-game))))))

(defun get-set-displays ()
   (remove nil (mapcar (lambda(x) (if (typep x 'set-item-display) x)) (capi:layout-description (sets-found (set-game))))))

(defun get-card-position (c)
  (capi:with-geometry c  (multiple-value-bind (x y) (capi::convert-relative-position c (capi::convert-to-screen) capi::%x% capi::%y%)
                      (list x y))))

;;;; Analysis
(defun conv-to-list (x)
  (cond (x
         (read-from-string (concatenate 'string "(" (string-trim '(#\Space #\. #\?) x) ")")))))



(defun mean (lst)
  (when (and  (plusp (length lst)) (every 'numberp lst))
     (floor (reduce '+ lst) (length lst))))




#+ACT-R-6.0 
(progn
(defun get-device () (device (get-module :device)))

(defun get-vision ()
  (get-module :vision))

(defmethod build-vis-locs-for ((dev capi:interface) (vis-mod vision-module)) 
  (let ((res nil))
    (dolist (c (get-cards))
      (push (build-vis-locs-for c vis-mod) res))
    (dolist (c (get-card-displays))
      (push (build-vis-locs-for c vis-mod) res))
    (dolist (c (get-set-displays))
      (push (build-vis-locs-for c vis-mod) res))
    
    res)) 

;texture  (solid stripped open)
;shape (squiggle diamond oval)
;color (red purple green)
;number (1 2 3)
(defun attribute->symbol (attr val)
  (let ((attrs (get-attributes val)))
    (destructuring-bind (texture shape color number) attrs
      (case attr
        (texture (nth texture '(solid stripped open)))
        (shape (nth shape '(squiggle diamond oval)))
        (color (nth color '(red purple green)))
        (number (nth number '(one two three)))))))
        
(defmethod view-loc ((self capi:pinboard-object))  ;;view
  (let ((pos (view-position self))
        (size (view-size self)))
    (vector (round (+ (point-h pos) (/ (point-h size) 2)))
            (round (+ (point-v pos) (/ (point-v size) 2))))))

(defmethod build-vis-locs-for ((self card-display) (vis-mod vision-module))
  (capi:with-geometry self  
   ; (multiple-value-bind (x y)  
   ;     (capi::convert-relative-position self (capi::element-interface self) 0 0)               
      (let* ((w/2 (floor capi::%width% 2)) (h/2 (floor capi::%height% 2))
             (feat (car (define-chunks-fct `((isa card-location 
                                                  screen-x ,(px (view-loc self)) ;;,(+ x w/2)
                                                  screen-y ,(py (view-loc self)) ;;,(- (+ y h/2 ) 22)
                                                  width ,capi::%width% 
                                                  height ,capi::%height%
                                                  value ,(value self)
                                                  color ,(attribute->symbol 'color (value self))
                                                  shape ,(attribute->symbol 'shape (value self))
                                                  texture ,(attribute->symbol 'texture (value self))
                                                  number ,(attribute->symbol 'number (value self))
                                                  kind card))))))
                             ;(incf (screen-y feat) 30) ;; (get-interface-offset (set-game)))
        (setf (chunk-visual-object feat) self)
        (setf (feat self) feat)))) ;)

(defmethod build-vis-locs-for ((self card-check-box) (vis-mod vision-module))
  (capi:with-geometry self  
    (multiple-value-bind (x y)
#+:MAC  (capi::convert-relative-position self (capi::element-interface self)  0 -22)
#+:WIN32 (capi::convert-relative-position self (capi::element-interface self)  -4 -28)      
      (let* ((w/2 (floor capi::%width% 2)) (h/2 (floor capi::%height% 2))
             (feat (car (define-chunks-fct `((isa check-box-location 
                                                  screen-x ,(+ x w/2)
                                                  screen-y ,(+ y h/2 )
                                                  width ,capi::%width% 
                                                  color ,(if (capi:button-selected self) 'black 'white)
                                                  height ,capi::%height%
                                                  value ,(value self)
                                                  selected ,(capi:button-selected self)
                                                  kind check-box))))))
                             ;(incf (screen-y feat) 30) ;; (get-interface-offset (set-game)))
        (setf (chunk-visual-object feat) self)
        (setf (feat self) feat)))))

(defmethod build-vis-locs-for ((self set-item-display) (vis-mod vision-module))
  (capi:with-geometry self  
;    (multiple-value-bind (x y)  
 ;       (capi::convert-relative-position self (capi::element-interface self) 0 0)               
      (let* ((w/2 (floor capi::%width% 2)) (h/2 (floor capi::%height% 2))
             (feat (if (value self) 
                       (car (define-chunks-fct `((isa card-location 
                                                  screen-x ,(px (view-loc self)) ;;;,(+ x w/2)
                                                  screen-y ,(py (view-loc self)) ;;;,(- (+ y h/2 ) 22)
                                                  width ,capi::%width% 
                                                  height ,capi::%height%
                                                  value ,(value self)
                                                  color ,(attribute->symbol 'color (value self))
                                                  shape ,(attribute->symbol 'shape (value self))
                                                  texture ,(attribute->symbol 'texture (value self))
                                                  number ,(attribute->symbol 'number (value self))
                                                  kind set-item))))
                     (car (define-chunks-fct `((isa card-location 
                                                  screen-x ,(px (view-loc self)) ;;;,(+ x w/2)
                                                  screen-y ,(py (view-loc self)) ;;;,(- (+ y h/2 ) 22)
                                                  width ,capi::%width% 
                                                  height ,capi::%height%
                                                  color white
                                                  kind set-item))))
                   )))
                             ;(incf (screen-y feat) 30) ;; (get-interface-offset (set-game)))
        (setf (chunk-visual-object feat) self)
        (setf (feat self) feat)))) ;)

(defmethod vis-loc-to-obj ((c card-display) loc)
  (car (define-chunks-fct `((isa card 
                                 value ,(chunk-slot-value-fct loc 'value)
                                 color ,(chunk-slot-value-fct loc 'color)
                                 height ,(chunk-slot-value-fct loc 'height)
                                 width ,(chunk-slot-value-fct loc 'width)
                                 texture ,(chunk-slot-value-fct loc 'texture) 
                                 shape ,(chunk-slot-value-fct loc 'shape) 
                                 number ,(chunk-slot-value-fct loc 'number) 
                                 
                                 )))))

(defmethod vis-loc-to-obj ((c card-check-box) loc)
  (car (define-chunks-fct `((isa check-box 
                                 value ,(chunk-slot-value-fct loc 'value)
                                 color ,(chunk-slot-value-fct loc 'color)
                                 selected ,(chunk-slot-value-fct loc 'selected)
                                 
                                 )))))

(defmethod vis-loc-to-obj ((c set-item-display) loc)
  (car (define-chunks-fct `((isa set-item
                                 value ,(chunk-slot-value-fct loc 'value)
                                 color ,(chunk-slot-value-fct loc 'color)
                                 height ,(chunk-slot-value-fct loc 'height)
                                 width ,(chunk-slot-value-fct loc 'width)
                                 texture ,(chunk-slot-value-fct loc 'texture) 
                                 shape ,(chunk-slot-value-fct loc 'shape) 
                                 number ,(chunk-slot-value-fct loc 'number) 
                                 
                                 )))))

#|
(defmethod process-card :after (data (interface set-board)) ;
"Callback function for clicking on a card check-box"
  (declare (ignore interface))
    (let* ((cards (get-cards))
           (card (find data cards :key #'capi:item-data)))
        (delete-visicon-item card) 
        (add-visicon-item card)))

(defmethod process-retract :after (data (interface set-board)) ;
"Callback function for clicking on a card check-box"
   (declare (ignore interface))
    (let* ((cards (get-cards))
           (card (find data cards :key #'capi:item-data)))
      (delete-visicon-item card)
      (add-visicon-item card)))

(defmethod update-set-item :after (val (obj set-item-display))
  (delete-visicon-item obj)
  (add-visicon-item obj))

(defmethod clear-current-set :before ((interface set-board))
  (with-slots (current-cards) interface
    (dolist (obj current-cards)
      (delete-visicon-item obj)
      (add-visicon-item obj))))
|# 
(defmethod process-card :after (data (interface set-board)) 
  (proc-display))
(defmethod process-retract :after (data (interface set-board)) 
  (proc-display))
(defmethod update-set-item :after (val (obj set-item-display))
  (proc-display))
(defmethod clear-current-set :before ((interface set-board))
  (proc-display))

(defmethod show-game :after ((win set-board) modelp)
  (when modelp  
    (install-device win)
    (proc-display)
   ; (add-visicon-item win)
    (print-visicon)
     
    (mp:process-run-function "actr" '() #'run-act +actr-run-time+  t *standard-output*)
    ))

(defun run-act (tm rt output)
  (setq *standard-output* output)
  (run tm :real-time rt))


(clear-all)
(define-model Set-game

(sgp :v t) 
(sgp :esc t)
(hand-to-mouse (get-module :motor))
(chunk-type set-game state start)
(chunk-type (card-location (:include visual-location)) texture number shape) 
(chunk-type (check-box-location (:include visual-location)) selected) 
(chunk-type (card (:include visual-object)) texture number shape)
(chunk-type (check-box (:include visual-object)) selected)
(chunk-type (set-item (:include visual-object)) texture number shape)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Put Your Code Here ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; my chunk type(s)

(define-chunks (one isa chunk) (two isa chunk) (three isa chunk))
; any add-dm stuff here
(add-dm
 (goal isa set-game state start)
 
)

; productions here

(p test1
 =goal> isa set-game state start
==>
 +visual-location> isa card-location
  number one
  :attended nil
 =goal> state next)

(p test2
 =goal> isa set-game state next
 ?visual> state free
 =visual-location> isa card-location
==>
 +visual> isa move-attention
  screen-pos =visual-location
 =goal> state next2)

(p test3
 =goal> isa set-game state next2
 =visual> isa card
 number =n texture =t shape =s color =c value =v
==>
 !output! (found =n =t =s =c =v)
  +visual-location> isa check-box-location
 :attended nil
 value =v
 =goal> state next4)



(p test5
 =goal> isa set-game state next4
 =visual-location> isa check-box-location
 ?manual> state free
 ?visual> state free
==>
 +manual> isa move-cursor
 loc =visual-location
 +visual> isa move-attention
 screen-pos =visual-location
 =goal> state next5)

(p test6
 =goal> isa set-game state next5
 ?manual> state free
 =visual> isa check-box
==>
 +manual> isa click-mouse
 =goal> state next6)

(p test7
 =goal> isa set-game state next6
 ?manual> state free
==>
 !eval! (print-visicon)
 =goal> state start)


; initialization
(setf *actr-enabled-p* t)
(goal-focus goal)

 
) ;;end of model
) ;;end of act-r stuff

(defun draw-eye-data (game-num &key (fn nil) (inc nil) (y-off 22))
  (let ((ln t) (resolution nil) (scale-x nil) (scale-y nil) (start-tm nil) (game-no 0) (cnt 0)
        (win (pinboard (set-game))))
    (with-open-file (fs (aif fn it (capi:prompt-for-file "")) :direction :input)
      (while ln
        (setq ln (ignore-errors (read-line fs)))
        (when ln
          (setq ln (convert-to-list ln)) ;(print ln)
          (case (if (stringp (fourth ln)) (read-from-string (fourth ln)) (fourth ln))
            (start-trial 
             (incf game-no)
             (setq start-tm (third ln))
             (format t "~%Game Number ~S ~S" game-no start-tm))
            (cw-event 
             (case (if (stringp (fifth ln)) (read-from-string (fifth ln)) (fifth ln))
               (screen-resolution 
                (setf resolution (list (sixth ln) (seventh ln)))
                (setq scale-x (/ (capi:screen-width (capi:convert-to-screen)) (first resolution)))
                (setq scale-y (/ (capi:screen-height (capi:convert-to-screen)) (second resolution))))))
            (eg-event
             (if (eql game-num game-no)
                 (case (if (stringp (fifth ln)) (read-from-string (fifth ln)) (fifth ln))
                   (eg-data
                    (if (plusp (sixth ln))
                        (destructuring-bind (diam x y &rest args) (subseq ln 6)
                          (when (and (<= 100 diam 200)
                                   (<= 0 x (first resolution))
                                   (<= 0 y (second resolution)))
                            (incf cnt)
                            (gp:draw-circle win (round (* x scale-x)) (+ y-off (round (* y scale-y))) 5 :foreground :green)
                            (if (and inc (zerop (mod cnt inc))) (break)))))))))))))))
               


  
