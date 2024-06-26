(in-package #:org.shirakumo.maiden.agents.silly)

(defvar *silly-functions* (make-hash-table :test 'eql))

(defun silly-function (name)
  (gethash name *silly-functions*))

(defun (setf silly-function) (func name)
  (setf (gethash name *silly-functions*) func))

(defun remove-silly-function (name)
  (remhash name *silly-functions*))

(defun dispatch-silly (sender message)
  (loop for func being the hash-values of *silly-functions*
        for result = (funcall func sender message)
        when result collect result))

(defmacro define-silly (name (sender message) &body body)
  `(setf (silly-function ',name)
         (named-lambda ,name (,sender ,message)
           (declare (ignorable ,sender))
           ,@body)))

(defmacro define-simple-silly (name (sender regex &rest args) &body body)
  (let ((message (gensym "MESSAGE")))
    `(define-silly ,name (,sender ,message)
       (cl-ppcre:register-groups-bind ,args (,(format NIL "(?i)~a" regex) ,message)
         (format NIL ,(first body) ,@(rest body))))))

(defun cut-to-first-vowel (word)
  (flet ((p (c) (position c word :test #'char-equal)))
    (subseq word (or (p #\a) (p #\e) (p #\i) (p #\o) (p #\u) (p #\y) (p #\w) 0))))

(define-simple-silly thants (sender "thanks,? ([^！？．。!¡?¿.,:;\\s]+)" thank)
  "... Th~(~a~)." (cut-to-first-vowel thank))

(define-simple-silly blants (sender "bless you,? ([^！？．。!¡?¿.,:;\\s]+)" bless)
  "... Bl~(~a~)." (cut-to-first-vowel bless))

(define-simple-silly now-we-know (sender "((i|you|he|she|it|we|they)( all)? know(s?) now|now (i|you|he|she|it|we|they)( all)? know(s?))")
  (alexandria:random-elt '("...now we know." "... oh yeah we know now." "NOW WE KNOW!" "NOW WE KNOOOW!!" "...yeah that's good. Now we know.")))

(define-simple-silly the-plan (sender "that('s| was| is) the plan")
  "... to give you a boner. AND YOU GOT ONE!")

(define-simple-silly nano-machines (sender "(how is (this|that) (even )?possible)|(how the hell)|(how in the world)|how('d| would) that even work")
  "NANO MACHINES, SON!")

(define-simple-silly nespresso (sender "what else")
  "Nespresso.")

(define-simple-silly clooney (sender "who else")
  "George Clooney.")

(define-simple-silly tomorrow (sender "when('s| is)( the next| the new) (.*?)( going to| gonna)?( be| come)?")
  "Tomorrow.")

(define-simple-silly the-answer (sender "I('ll| will) let (you|him|her|them|us) decide")
  "... but the answer is yes.")

(define-simple-silly great (sender "it(('s| is) (gonna be|going to be)|('ll| will) be) great")
  "It's gonna be great.")

(define-simple-silly galo-sengen (sender "go\\s*go\\s*go")
  "GALO SENGEN")

(define-simple-silly take-it-easy (sender "yukkuri|take it easy|ゆっくり")
  (if (< 1 (random 10))
      "ゆっくりしていってね！"
      "~
　　 _,,....,,_　 ＿人人人人人人人人人人人人人人人＿
-''\":::::::::::::｀''＞　　　ゆっくりしていってね！！！　　　＜
ヽ:::::::::::::::::::::￣^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^Ｙ^￣
　|::::::;ノ´￣＼:::::::::::＼_,. -‐ｧ　　　　　＿_　　 _____　　 ＿_____
　|::::ﾉ　　　ヽ､ヽr-r'\"´　　（.__　　　　,´　_,, '-´￣￣｀-ゝ 、_ イ、
_,.!イ_　　_,.ﾍｰｧ'二ﾊ二ヽ､へ,_7　　　'r ´　　　　　　　　　　ヽ、ﾝ、
::::::rｰ''7ｺ-‐'\"´　 　 ;　 ',　｀ヽ/｀7　,'＝=─-　　　 　 -─=＝',　i
r-'ｧ'\"´/　 /!　ﾊ 　ハ　 !　　iヾ_ﾉ　i　ｲ　iゝ、ｲ人レ／_ルヽｲ i　|
!イ´ ,' |　/__,.!/　V　､!__ﾊ　 ,'　,ゝ　ﾚﾘｲi (ﾋ_] 　　 　ﾋ_ﾝ ).| .|、i .||
`! 　!/ﾚi'　(ﾋ_] 　　 　ﾋ_ﾝ ﾚ'i　ﾉ　　　!Y!\"\"　 ,＿__, 　 \"\" 「 !ﾉ i　|
,'　 ﾉ 　 !'\"　 　 ,＿__,　 \"' i .ﾚ'　　　　L.',.　 　ヽ _ﾝ　　　　L」 ﾉ| .|
　（　　,ﾊ　　　　ヽ _ﾝ　 　人! 　　　　 | ||ヽ、　　　　　　 ,ｲ| ||ｲ| /
,.ﾍ,）､　　）＞,､ _____,　,.イ　 ハ　　　　レ ル｀ ー--─ ´ルﾚ　ﾚ´"))

(define-silly numberwang (sender message)
  (when (and (cl-ppcre:scan "^(([1-9]\\d*(('\\d+)*(\\.\\d*))?)|0)$" message)
             (< (random 100) 30))
    "That's Numberwang!"))

(define-consumer silly (agent)
  ((gun-cylinder :initform (vector NIL NIL NIL NIL NIL NIL) :accessor gun-cylinder)))

(define-handler (silly handle (and message-event passive-event)) (c ev user message)
  :class activatable-handler
  :module #.*package*
  (unless (matches (username (client ev)) (user ev))
    (dolist (response (dispatch-silly (name user) message))
      (sleep (+ 0.5 (random 3)))
      (reply ev "~a" response))))

(define-command (silly eight) (c ev)
  :command "8"
  (reply ev "Eight."))

(define-command (silly jerkcity) (c ev)
  (multiple-value-bind (content status headers uri) (request-as :html "https://www.bonequest.com/random/")
    (declare (ignore headers))
    (when (= 200 status)
      (reply ev "~a ~a"
             (lquery:$ content "title" (text) (node))
             (puri:render-uri uri NIL)))))

(define-command (silly roll) (c ev &optional (size "6") (times "1"))
  (cond
    ((or (string-equal size "infinity") (string-equal times "infinity"))
     (reply ev "~ad~a: infinity" times size))
    ((string-equal times "mom")
     (if (string-equal size "your")
         (reply ev "I would never hurt my mom!")
         (reply ev "Down the hill rolls the fatty...")))
    ((or (string-equal times "joint") (string-equal size "joint"))
     (reply ev "Drugs are bad, mkay."))
    ((string-equal size "over")
     (reply ev "No."))
    ((or (string-equal size "dice")
         (and (string-equal size "the")
              (string-equal times "dice")))
     (reply ev "1d6: ~d" (1+ (random 6))))
    ((or (string-equal size "cylinder")
         (string-equal size "gun")
         (string-equal size "russian"))
     (if (= 0 (random 6))
         (reply ev "BANG!")
         (reply ev ".... click")))
    (T
     (let ((size (parse-integer size :junk-allowed T))
           (times (parse-integer times :junk-allowed T)))
       (if (and size times)
           (reply ev "~dd~d: ~d" times size (loop for i from 0 below times summing (1+ (random size))))
           (reply ev "I don't know how to roll that."))))))

(define-command (silly pick) (c ev &rest choices)
  :command "randomly pick one of"
  (if (null choices)
      (reply ev "Uh, I'll need at least one thing to pick from, you know.")
      (reply ev "Okey, I choose... ~a!" (alexandria:random-elt choices))))

(defun profane-p (thing)
  (find thing '("shit" "ass" "fuck" "cunt" "retard" "idiot" "stupid" "cock" "dick" "autist" "scrap" "trash" "garbage" "junk" "sex")
        :test (lambda (a b) (search b a))))

(define-command (silly welcome) (c ev &string place)
  (cond ((string= place "back")
         (reply ev "Thanks, it's good to be back."))
        ((starts-with "to " place :test #'char-equal)
         (reply ev "Thanks, I'm glad to be in ~a."
                (subseq place 3)))
        (T
         (reply ev "Thanks!"))))

(define-command (silly hello) (c ev &string other)
  (cond ((profane-p other)
         (reply ev "Well fuck you, too."))
        (T
         (reply ev "Hi!"))))

(define-command (silly present) (c ev &string thing)
  :command "have a"
  (cond ((profane-p thing)
         (reply ev "... Hey!"))
        (T
         (reply ev "Thanks for the ~a!" thing))))

(define-command (silly love-you) (c ev)
  :command "I love you"
  (reply ev (alexandria:random-elt
             '("Aw shucks."
               "Haha, thanks!"
               "You're making me blush. Stop it."
               "Flirting with robots, eh?"))))

(define-command (silly you-are) (c ev &string thing)
  :command "you are"
  (cond ((profane-p thing)
         (reply ev "You must be pretty miserable not to have anything better to do than to try and insult a bot."))
        (T
         (reply ev (alexandria:random-elt
                    '("If you want me to be."
                      "If that's what you want to believe, sure."
                      "Sure."
                      "Ok?"))))))

(define-command (silly make) (c ev &string thing)
  :command "make me a"
  (cond ((search "sandwich" thing)
         (reply ev "Not even in your dreams, buddy."))
        ((profane-p thing)
         (reply ev "... Hey!"))
        (T
         (reply ev "Enjoy your ~a! It will approximately be ready in ~a"
                thing (format-relative-time (+ (get-universal-time) (random (* 60 60 24 365 1000))))))))

(define-command (silly tell-to) (c ev target to &string thing)
  :command "tell to"
  (reply ev "~a: Hey, ~a!" target (string-left-trim ".?!" thing)))

(define-command (silly say) (c ev &string thing)
  (reply ev "~a" thing))

(defparameter *fortunes*
  (with-open-file (s (asdf:system-relative-pathname :maiden-silly "fortunes.txt"))
    (loop for line = (read-line s NIL NIL)
          while line collect line)))

(defun fortune (name &optional (time (get-universal-time)))
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time time)
    (declare (ignore s m h))
    (let ((date-hash (+ (+ dd (* mm 31)) (* yy 365)))
          (nick-hash (reduce #'+ name :key #'char-code)))
      (elt *fortunes* (mod (+ date-hash nick-hash) (length *fortunes*))))))

(define-command (silly fortune) (c ev &optional name)
  (if name
      (reply ev "~@(~a~)'s fortune for today is: ~a" name (fortune name))
      (reply ev "Your fortune for today is: ~a" (fortune (name (user ev))))))

(define-command (silly fortune-for) (c ev name)
  :command "fortune for"
  (reply ev "~@(~a~)'s fortune for today is: ~a" name (fortune name)))

(define-command (silly check-gun) (c ev)
  :command "check the gun"
  (reply ev "You check the gun... it's ~[empty~:;loaded with ~:*~d bullet~:p~]!"
         (count T (gun-cylinder c))))

(define-command (silly load-gun) (c ev)
  :command "load the gun"
  (let ((count (count T (gun-cylinder c))))
    (cond ((= count (length (gun-cylinder c)))
           (reply ev "The cylinder is already fully loaded."))
          (T
           (dotimes (i (length (gun-cylinder c)))
             (unless (aref (gun-cylinder c) i)
               (setf (aref (gun-cylinder c) i) T)
               (return)))
           (incf count)
           (if (= 1 count)
               (reply ev "You load the gun. There is now one bullet in the cylinder.")
               (reply ev "You load the gun. There are now ~d bullets in the cylinder." count))))))

(define-command (silly empty-gun) (c ev)
  :command "empty the gun"
  (fill (gun-cylinder c) NIL)
  (reply ev "You empty the cylinder. The gun is now empty."))

(define-command (silly spin-gun) (c ev)
  :command "spin the gun"
  (alexandria:rotate (gun-cylinder c) (random 6))
  (reply ev "You spin the cylinder."))

(define-command (silly fire-gun) (c ev)
  :command "fire the gun"
  (let ((bullet (aref (gun-cylinder c) 0)))
    (setf (aref (gun-cylinder c) 0) NIL)
    (alexandria:rotate (gun-cylinder c))
    (reply ev "You aim the gun and ...")
    (sleep (+ 0.5 (random 1.0)))
    (reply ev "~:[click~;BANG!~]" bullet)))

(defparameter *songs*
  (with-open-file (s (asdf:system-relative-pathname :maiden-silly "songs.txt"))
    (loop for line = (read-line s NIL NIL)
          while line collect line)))

(defun fuse (parts position)
  (let ((cons (nthcdr position parts)))
    (when (rest cons)
      (setf (car cons) (format NIL "~a ~a" (first cons) (second cons)))
      (setf (cdr cons) (cddr cons)))
    parts))

(defun split-song (song)
  (let ((count (count #\Space song))
        (parts (cl-ppcre:split " " song)))
    (loop while (< 0 count)
          repeat (max 3 (ceiling count 5))
          do (setf parts (fuse parts (random count)))
             (decf count))
    parts))

(define-command (silly sing) (c ev &string song)
  :command "sing"
  (reply ev "𝄞𝄙 ~{~a~a ~a ~}~a~a𝄙𝄂"
         (loop for word in (split-song (if (string= song "")
                                           (alexandria:random-elt *songs*)
                                           song))
               collect (alexandria:random-elt #("♭" "♮" "♯" "𝆑" "" "" "" "" "" "" "" "" ""))
               collect (alexandria:random-elt "𝅝𝅗𝅥𝅘𝅥𝅘𝅥𝅮𝅘𝅥𝅯𝅘𝅥𝅰𝅘𝅥𝅱𝅘𝅥𝅲♬♫")
               collect word)
         (alexandria:random-elt #("♭" "♮" "♯" "𝆑" "" "" "" "" "" "" "" "" ""))
         (alexandria:random-elt "𝅝𝅗𝅥𝅘𝅥𝅘𝅥𝅮𝅘𝅥𝅯𝅘𝅥𝅰𝅘𝅥𝅱𝅘𝅥𝅲♬♫")))

(define-event tell-message (message-event passive-event)
  ((original-event :initarg :original-event)
   (target-user :initarg :target-user)))

(defmethod reply ((event tell-message) format &rest args)
  (reply (slot-value event 'original-event)
         "~a: ~?" (slot-value event 'target-user) format args))

(define-command (silly tell) (c ev user &string command)
  (issue (make-instance 'tell-message :message (format NIL "~a: ~a" (username (client ev)) command)
                                      :user (user ev)
                                      :client (client ev)
                                      :original-event ev
                                      :target-user user)
         (core ev)))
