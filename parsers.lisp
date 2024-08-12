(in-package :synergy)

(defun extract-code-block (string)
  "Extract the content between '```' marks and remove the output delimiter at the end if present."
  (let ((matches (ppcre:all-matches "```.*" string)))
    (when matches
      (chomp (subseq string 
		     (second matches) 
		     (nth (- (length matches) 2) matches))))))

