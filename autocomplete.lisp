(defun auto-complete()
  "Displays matching words for user input via stdin. !q aborts."
  (loop for input = (read-line) 
     until (equal input "!q") 
     do (format t "~a~%" (words-starting-with input 5))))