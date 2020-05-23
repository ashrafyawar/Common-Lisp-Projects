;; utility functions 
(load "include.lisp") ;; "c2i and "i2c"

(defun read-as-list (filename)

	(let ((paragraph '(null))(documentlist '(null))(word '(null))); declaring  local vars inside let function.
		
		(dotimes (x (+ (list-length word) 1))(pop word)); removes all elements of (word) list
		
		(let ((in (open filename :if-does-not-exist nil)));openning file and check for null file
	  		(when in
	    		(loop for text = (read in nil)
					while text do
	    				
	    				(if(eq text '!); considering ! as newline indicator in .txt files
	    					(progn
	        					(push (cdr paragraph) (cdr (last documentlist))); assign one paragraph at a time into document list
	        					(setf paragraph '(null))
	    					)
	    					(progn
	    						(with-input-from-string (is (string text));converting s-expression into string then into char list 
						    		(do ((c (read-char is) (read-char is nil 'the-end)))
						        		((not (characterp c)))
						     			(push (c2i (char-downcase c)) word)))
								(push (reverse word) (cdr (last paragraph)))
	    					)	
	    				)
	    				(dotimes (x (+ (list-length word) 1))(pop word));poping elements of word list tobe used later
	        	)
	  		)
			(close in)
  		)
  		(delete nil documentlist)
		(return-from read-as-list (cdr documentlist)); returns the cdr element of the documentlist
	)
)
(defun spell-checker-0 (word);this function takes an encoded word an looks for the dictionary(x).txt files and returns T if found otherwise NIL
	
	(let ((in (open "dictionary1.txt" :if-does-not-exist nil))(tempword '(nil)))
  		(when in
    		(loop for line = (read in nil)
        		while line do
	        		(with-input-from-string (is (string line))
			    		(do ((c (read-char is) (read-char is nil 'the-end)))
			        		((not (characterp c)))
			     			(push (c2i (char-downcase c)) tempword))
					)
					(setf tempword (cdr (reverse  tempword)))
					(if (equal word tempword)
						(return-from spell-checker-0 T))
					(setf tempword '(nil))
    		)
    	    (close in)	
 		)
 		(print "Could Not Find The Word :("); the word could not find in dictionary.txt
 		(return-from spell-checker-0 nil)
	)
)

(defun spell-checker-1 (word)
 	
	(let ((in (open "dictionary2.txt" :if-does-not-exist nil))(tempword '()))
  		
  		(when in
    		(loop for line = (read in nil)
        		while line do
	        		(with-input-from-string (is (string line))
			    		(do ((c (read-char is) (read-char is nil 'the-end)))
			        		((not (characterp c)))
			     			(push (c2i (char-downcase c)) tempword)
			    		)
					)
    		)
    	    (close in)	
 		)
 		(print "could not find the word :(")
 		(return-from spell-checker-1 nil)
	)
)
;; -----------------------------------------------------
;; DECODE FUNCTIONS
(defun decoder(paragraph);this function takes a paragraph and calls decoderword function and decodes the paragraph and returns it 

	(let((newParagraph'(null))(newWord'(null))(is-exist nil))

		(loop for word in paragraph
			while word do
				(setf newWord(DecodeWord word))
				(setf is-exist(spell-checker-0 newWord))
				(if (eq is-exist T)
					(push newWord (cdr (last newParagraph))))
				(setf is-exist nil)
		)
		(cdr newParagraph)
	)
)
(defun decoderB(paragraph);de coder funtion to GEN-DECODER-B-0 different than the decoder function

	(let((newParagraph'(null))(newWord'(null))(is-exist nil))

		(loop for word in paragraph
			while word do
				(setf newWord(DecodeWordB word))
				(setf is-exist(spell-checker-0 newWord))
				(if (eq is-exist T)
					(push newWord (cdr (last newParagraph))))
				(setf is-exist nil)
		)
		(cdr newParagraph)
	)
)
(defun DecodeWord(word);this func takes a word and decods it using brute force technique 

	(let ((w'(null))) ;define list w in order to use it as parallel lists concept
		
		(setf w(copy-list word))		
		(loop for n in word
			while n do
				(if (and (<= 0) (>= 25))
					(setf w (mapcar (lambda (lst) (subst nil n lst)) w))
				)
		)

		(loop for kar in word for k in w;loop for each character of the word
			while kar do
				(if (equal (nth(position kar word) w) nil)							
					(progn
						(cond 
							((eq kar 3)
								(setf word(nsubstitute 0 3 word :count 1))
								(setf w(nsubstitute T (nth (position 0 word)w) w :count 1)))
							((eq kar 4)
								(setf word(nsubstitute 1 4 word :count 1))
								(setf w(nsubstitute T (nth (position 1 word)w) w :count 1)))
							((eq kar 5)
								(setf word(nsubstitute 2 5 word :count 1))
								(setf w(nsubstitute T (nth (position 2 word)w) w :count 1)))
							((eq kar 15)
								(setf word(nsubstitute 3 15 word :count 1))
								(setf w(nsubstitute T (nth (position 3 word)w) w :count 1)))
							((eq kar 16)
								(setf word(nsubstitute 4 16 word :count 1))
								(setf w(nsubstitute T (nth (position 4 word)w) w :count 1)))
							((eq kar 0)
								(setf word(nsubstitute 5 0 word :count 1))
								(setf w(nsubstitute T (nth (position 5 word)w) w :count 1)))
							((eq kar 1)
								(setf word(nsubstitute 6 1 word :count 1))
								(setf w(nsubstitute T (nth (position 6 word)w) w :count 1)))
							((eq kar 10)
								(setf word(nsubstitute 7 10 word :count 1))
								(setf w(nsubstitute T (nth (position 7 word)w) w :count 1)))
							((eq kar 11)
								(setf word(nsubstitute 8 11 word :count 1))
								(setf w(nsubstitute T (nth (position 8 word)w) w :count 1)))
							((eq kar 2)
								(setf word(nsubstitute 9 2 word :count 1))
								(setf w(nsubstitute T (nth (position 9 word)w) w :count 1)))
							((eq kar 17)
								(setf word(nsubstitute 10 17 word :count 1))
								(setf w(nsubstitute T (nth (position 10 word)w) w :count 1)))
							((eq kar 18)
								(setf word(nsubstitute 11 18 word :count 1))
								(setf w(nsubstitute T (nth (position 11 word)w) w :count 1)))
							((eq kar 19)
								(setf word(nsubstitute 12 19 word :count 1))
								(setf w(nsubstitute T (nth (position 12 word)w) w :count 1)))
							((eq kar 6)
								(setf word(nsubstitute 13 6 word :count 1))
								(setf w(nsubstitute T (nth (position 13 word)w) w :count 1)))
							((eq kar 24)
								(setf word(nsubstitute 14 24 word :count 1))
								(setf w(nsubstitute T (nth (position 14 word)w) w :count 1)))
							((eq kar 25)
								(setf word(nsubstitute 15 25 word :count 1))
								(setf w(nsubstitute T (nth (position 15 word)w) w :count 1)))
							((eq kar 7)
								(setf word(nsubstitute 16 7 word :count 1))
								(setf w(nsubstitute T (nth (position 16 word)w) w :count 1)))
							((eq kar 8)
								(setf word(nsubstitute 17 8 word :count 1))
								(setf w(nsubstitute T (nth (position 17 word)w) w :count 1)))
							((eq kar 9)
								(setf word(nsubstitute 18 9 word :count 1))
								(setf w(nsubstitute T (nth (position 18 word)w) w :count 1)))
							((eq kar 12)
								(setf word(nsubstitute 19 12 word :count 1))
								(setf w(nsubstitute T (nth (position 19 word)w) w :count 1)))
							((eq kar 13)
								(setf word(nsubstitute 20 13 word :count 1))
								(setf w(nsubstitute T (nth (position 20 word)w) w :count 1)))
							((eq kar 14)
								(setf word(nsubstitute 21 14 word :count 1))
								(setf w(nsubstitute T (nth (position 21 word)w) w :count 1)))
							((eq kar 26)
								(setf word(nsubstitute 22 26 word :count 1))
								(setf w(nsubstitute T (nth (position 22 word)w) w :count 1)))
							((eq kar 21)
								(setf word(nsubstitute 23 21 word :count 1))
								(setf w(nsubstitute T (nth (position 23 word)w) w :count 1)))
							((eq kar 22)
								(setf word(nsubstitute 24 22 word :count 1))
								(setf w(nsubstitute T (nth (position 24 word)w) w :count 1)))
							((eq kar 23)
								(setf word(nsubstitute 25 23 word :count 1))
								(setf w(nsubstitute T (nth (position 25 word)w) w :count 1)))				
						)
					)
				)
		)
		(return-from DecodeWord word)
	)
)
(defun DecodeWordB(word);this func takes a word and decods it using brute force technique but different than decoderword functioin it has 6 less condition to be checked 

	(let ((w'(null)))
		
		(setf w(copy-list word))		
		(loop for n in word
			while n do
				(if (and (<= 0) (>= 25))
					(setf w (mapcar (lambda (lst) (subst nil n lst)) w))
				)
		)
		(loop for kar in word for k in w
			while kar do
				(if (equal (nth(position kar word) w) nil)							
					(progn
						(cond 
							((eq kar 3)
								(setf word(nsubstitute 0 3 word :count 1))
								(setf w(nsubstitute T (nth (position 0 word)w) w :count 1)))
							((eq kar 5)
								(setf word(nsubstitute 2 5 word :count 1))
								(setf w(nsubstitute T (nth (position 2 word)w) w :count 1)))
							((eq kar 15)
								(setf word(nsubstitute 3 15 word :count 1))
								(setf w(nsubstitute T (nth (position 3 word)w) w :count 1)))
							((eq kar 16)
								(setf word(nsubstitute 4 16 word :count 1))
								(setf w(nsubstitute T (nth (position 4 word)w) w :count 1)))
							((eq kar 1)
								(setf word(nsubstitute 6 1 word :count 1))
								(setf w(nsubstitute T (nth (position 6 word)w) w :count 1)))
							((eq kar 10)
								(setf word(nsubstitute 7 10 word :count 1))
								(setf w(nsubstitute T (nth (position 7 word)w) w :count 1)))
							((eq kar 11)
								(setf word(nsubstitute 8 11 word :count 1))
								(setf w(nsubstitute T (nth (position 8 word)w) w :count 1)))
							((eq kar 2)
								(setf word(nsubstitute 9 2 word :count 1))
								(setf w(nsubstitute T (nth (position 9 word)w) w :count 1)))
							((eq kar 17)
								(setf word(nsubstitute 10 17 word :count 1))
								(setf w(nsubstitute T (nth (position 10 word)w) w :count 1)))
							((eq kar 18)
								(setf word(nsubstitute 11 18 word :count 1))
								(setf w(nsubstitute T (nth (position 11 word)w) w :count 1)))
							((eq kar 6)
								(setf word(nsubstitute 13 6 word :count 1))
								(setf w(nsubstitute T (nth (position 13 word)w) w :count 1)))
							((eq kar 24)
								(setf word(nsubstitute 14 24 word :count 1))
								(setf w(nsubstitute T (nth (position 14 word)w) w :count 1)))
							((eq kar 25)
								(setf word(nsubstitute 15 25 word :count 1))
								(setf w(nsubstitute T (nth (position 15 word)w) w :count 1)))
							((eq kar 7)
								(setf word(nsubstitute 16 7 word :count 1))
								(setf w(nsubstitute T (nth (position 16 word)w) w :count 1)))
							((eq kar 9)
								(setf word(nsubstitute 18 9 word :count 1))
								(setf w(nsubstitute T (nth (position 18 word)w) w :count 1)))
							((eq kar 12)
								(setf word(nsubstitute 19 12 word :count 1))
								(setf w(nsubstitute T (nth (position 19 word)w) w :count 1)))
							((eq kar 26)
								(setf word(nsubstitute 22 26 word :count 1))
								(setf w(nsubstitute T (nth (position 22 word)w) w :count 1)))
							((eq kar 21)
								(setf word(nsubstitute 23 21 word :count 1))
								(setf w(nsubstitute T (nth (position 23 word)w) w :count 1)))
							((eq kar 22)
								(setf word(nsubstitute 24 22 word :count 1))
								(setf w(nsubstitute T (nth (position 24 word)w) w :count 1)))
							((eq kar 23)
								(setf word(nsubstitute 25 23 word :count 1))
								(setf w(nsubstitute T (nth (position 25 word)w) w :count 1)))				
						)
					)
				)
		)
		(return-from DecodeWordB word)
	)
)
(defun Gen-Decoder-A (paragraph)
	(return-from Gen-Decoder-A (decoder paragraph))
)
(defun count-occurence(letter paragraph);get the occurence of each letter in a paragraph

	(let((counter 0))
		(loop for word in paragraph
			while word do
			(loop for kar in word
				while kar do
				(if(eq kar letter)
					(setf counter( + counter 1)))))
		counter;return the occurence of the letter in a given paragraph
	)	
)
(defun Gen-Decoder-B-0 (paragraph)

	;in this function i have used the concept of parallel lists to access the certain index of desired list in order to be able to change the contant of the particular list
	;finding the frequency of occurence of letter and replace thire value whith ( e t a o i n ) respectively
	(let ((occurence '(nil))(letterAlphabet '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))(temp1 0)(restletters '(nil)))
		
		(dotimes(kar 26)
			(push (count-occurence kar paragraph) (cdr (last occurence))))
		
		(setf occurence(cdr occurence))
		
		(dotimes (n 6)
			(setf temp1 (nth 0 occurence))
			(dotimes (m (list-length occurence))

				(if (< temp1 (nth m occurence))
					(setf temp1 (nth m occurence))
				)
			)

			(push (nth (position temp1 occurence) letterAlphabet) restletters)
			(setf letterAlphabet(remove (nth (position temp1 occurence) letterAlphabet) letterAlphabet))
			(setf occurence (remove temp1 occurence))
		)
		(setf restletters (cdr (reverse restletters)))	

		; this part replces the six most frequently used letter in order 
		(setf paragraph(subst 4 (nth 0 restletters) paragraph))
		(setf paragraph(subst 19 (nth 1 restletters) paragraph))
		(setf paragraph(subst 0 (nth 2 restletters) paragraph))
		(setf paragraph(subst 14 (nth 3 restletters) paragraph))
		(setf paragraph(subst 8 (nth 4 restletters) paragraph))
		(setf paragraph(subst 13 (nth 5 restletters) paragraph))

		;handaling the rest part using brute force by calling the function
		(return-from Gen-Decoder-B-0 (decoderB paragraph))
	)
)

(defun Gen-Decoder-B-1 (paragraph))

(defun Code-Breaker (document decoder))
;; -----------------------------------------------------
;; Test code...

(defun Encode-Dcoument (Document);encoding document 

	(let ((AssocistedList'(null)))

		(setf AssocistedList(copy-list Document));take a copy of Dooument		
		
		(loop for paragraph in AssocistedList;set it all as nil to indicate that the particulat character has not been changed yet in the document
			while paragraph do
			(loop for word in paragraph
				while word do
				(loop for n in word
					while n do
					
						(if (and (<= 0) (>= 25))
							(setf AssocistedList (mapcar (lambda (lst) (subst nil n lst)) AssocistedList))
						)
				)
			)
		)

		(loop for paragraph in Document for p in AssocistedList;loop for document
			while paragraph do
			(loop for word in paragraph for w  in p;loop for paragraph
				while word do
				(loop for kar in word for k in w;loop for word in a paragraph
					while kar do

						(if (equal (nth(position kar word) w) nil);condition which check whether or not the particular char is replaced with cyper or not.						
							(progn								
								
								(cond 

									((eq kar 0)
										(setf word(nsubstitute 3 0 word :count 1))
										(setf w(nsubstitute T (nth (position 3 word)w) w :count 1)))
									((eq kar 1)
										(setf word(nsubstitute 4 1 word :count 1))
										(setf w(nsubstitute T (nth (position 4 word)w) w :count 1)))
									((eq kar 2)
										(setf word(nsubstitute 5 2 word :count 1))
										(setf w(nsubstitute T (nth (position 5 word)w) w :count 1)))
									((eq kar 3)
										(setf word(nsubstitute 15 3 word :count 1))
										(setf w(nsubstitute T (nth (position 15 word)w) w :count 1)))
									((eq kar 4)
										(setf word(nsubstitute 16 4 word :count 1))
										(setf w(nsubstitute T (nth (position 16 word)w) w :count 1)))
									((eq kar 5)
										(setf word(nsubstitute 0 5 word :count 1))
										(setf w(nsubstitute T (nth (position 0 word)w) w :count 1)))
									((eq kar 6)
										(setf word(nsubstitute 1 6 word :count 1))
										(setf w(nsubstitute T (nth (position 1 word)w) w :count 1)))
									((eq kar 7)
										(setf word(nsubstitute 10 7 word :count 1))
										(setf w(nsubstitute T (nth (position 10 word)w) w :count 1)))
									((eq kar 8)
										(setf word(nsubstitute 11 8 word :count 1))
										(setf w(nsubstitute T (nth (position 11 word)w) w :count 1)))
									((eq kar 9)
										(setf word(nsubstitute 2 9 word :count 1))
										(setf w(nsubstitute T (nth (position 2 word)w) w :count 1)))
									((eq kar 10)
										(setf word(nsubstitute 17 10 word :count 1))
										(setf w(nsubstitute T (nth (position 17 word)w) w :count 1)))
									((eq kar 11)
										(setf word(nsubstitute 18 11 word :count 1))
										(setf w(nsubstitute T (nth (position 18 word)w) w :count 1)))
									((eq kar 12)
										(setf word(nsubstitute 19 12 word :count 1))
										(setf w(nsubstitute T (nth (position 19 word)w) w :count 1)))
									((eq kar 13)
										(setf word(nsubstitute 6 13 word :count 1))
										(setf w(nsubstitute T (nth (position 6 word)w) w :count 1)))
									((eq kar 14)
										(setf word(nsubstitute 24 14 word :count 1))
										(setf w(nsubstitute T (nth (position 24 word)w) w :count 1)))
									((eq kar 15)
										(setf word(nsubstitute 25 15 word :count 1))
										(setf w(nsubstitute T (nth (position 25 word)w) w :count 1)))
									((eq kar 16)
										(setf word(nsubstitute 7 16 word :count 1))
										(setf w(nsubstitute T (nth (position 7 word)w) w :count 1)))
									((eq kar 17)
										(setf word(nsubstitute 8 17 word :count 1))
										(setf w(nsubstitute T (nth (position 8 word)w) w :count 1)))
									((eq kar 18)
										(setf word(nsubstitute 9 18 word :count 1))
										(setf w(nsubstitute T (nth (position 9 word)w) w :count 1)))
									((eq kar 19)
										(setf word(nsubstitute 12 19 word :count 1))
										(setf w(nsubstitute T (nth (position 12 word)w) w :count 1)))
									((eq kar 20)
										(setf word(nsubstitute 13 20 word :count 1))
										(setf w(nsubstitute T (nth (position 13 word)w) w :count 1)))
									((eq kar 21)
										(setf word(nsubstitute 14 21 word :count 1))
										(setf w(nsubstitute T (nth (position 14 word)w) w :count 1)))
									((eq kar 22)
										(setf word(nsubstitute 26 22 word :count 1))
										(setf w(nsubstitute T (nth (position 26 word)w) w :count 1)))
									((eq kar 23)
										(setf word(nsubstitute 21 23 word :count 1))
										(setf w(nsubstitute T (nth (position 21 word)w) w :count 1)))
									((eq kar 24)
										(setf word(nsubstitute 22 24 word :count 1))
										(setf w(nsubstitute T (nth (position 22 word)w) w :count 1)))
									((eq kar 25)
										(setf word(nsubstitute 23 25 word :count 1))
										(setf w(nsubstitute T (nth (position 23 word)w) w :count 1)))				
								)
							)
						)
				)
			)
		)
	)
	(return-from Encode-Dcoument Document)
)
(defun i2c-text(text);conversts an integer document into char document

	(loop for par in text
		while par do
		(loop for word in par
			while word do
		(loop for kar in word
			while kar do
			(setf text (mapcar (lambda (lst) (subst (i2c kar) kar lst)) text)))
		)
	)
	text;return char doc as whole
)
(defun test_on_test_data ()

	;IMPORTANT in order to test you can eigther uncomment PART ONE or PART TWO but Not Both At the same time.
	(print "....................................................")
	(print "Testing ....")
	(print "....................................................")
	(terpri)
	
	(let ((con (read-as-list "document1.txt"))(Newpar '(null))(tempcon '(null))(tempconn '(null))(GENB '(null))(GENBb '(null)))
		
		(setf tempcon(copy-list con));copy of the doc inorder to print the char version of the original document
		(princ(setf tempcon(i2c-text tempcon)))
		(terpri)
		
		(print "Encoding The Document...")
		(terpri)
		(setf con(Encode-Dcoument con))
		(setf tempconn(copy-list con))		

		(princ(setf tempconn(i2c-text tempconn)))
		(terpri)
		
		; PART ONE
		(print "calling Gen-Decoder-A...")
		(loop for i in con
			while i do
				(setf Newpar(Gen-Decoder-A i))
		)
		(loop for word in Newpar
			while word do
			(loop for kar in word
				while kar do
				(setf Newpar (mapcar (lambda (lst) (subst (i2c kar) kar lst)) Newpar)))
		)

		(print "Decoded Text is...")
		(terpri)
		(princ Newpar)
		;END OF PART ONE


		;PART TWO
		#||(print "calling Gen-Decoder-B-0...")
		(loop for i in con
			while i do
				(setf GENB(Gen-Decoder-B-0 i))
		)
		( print "decoding the text ...")
		(terpri)
		(setf GENBb(push GENB (cdr (last GENBb))))
		(princ(i2c-text GENBb))||#
		(terpri)
		;END OF PART TWO
	)
)
;; test code...
(test_on_test_data)