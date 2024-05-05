;;; vz-bruce --- Springsteen helpers for Musicbrainz

;;; Commentary:
; Springsteen helpers for Musicbrainz

;; Splitting setlists from Brucebase
(require 'rect)

;;; Code:

(defun vz-surround-with-quotes (ξstring &optional ξfrom ξto)
  "Surround ΞSTRING region between ΞFROM and ΞTO with quotes."
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))))
  (kill-region ξfrom ξto)
  (insert "“")
  (yank)
  (insert "”"))

(defun vz-surround-with-quotes-italic (ξstring &optional ξfrom ξto)
  "Surround ΞSTRING region between ΞFROM and ΞTO with italic quotes."
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))))
  (kill-region ξfrom ξto)
  (insert "''")
  (yank)
  (insert "''"))

(defun vz-split-setlist (ξstring &optional ξfrom ξto)
  "Split Springsteen setlists.

When called interactively, work on current paragraph or text selection.

When called in Lisp code, if ΞSTRING is non-nil, returns a changed string.
If ΞSTRING nil, change the text in the region between positions ΞFROM ΞTO."
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'paragraph)))
       (list nil (car bds) (cdr bds)))))

     (let ((ξtemp-syn-table (make-syntax-table)))
      (modify-syntax-entry ?’ "w" ξtemp-syn-table)
      (modify-syntax-entry ?' "w" ξtemp-syn-table)
      (with-syntax-table ξtemp-syn-table

	(let (workOnStringP inputStr outputStr)
	  (setq workOnStringP (if ξstring t nil))
	  (capitalize-region ξfrom ξto)
	  (vz-title-case-region-or-line ξfrom ξto)
	  (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
	  (setq outputStr
		(let ((case-fold-search t))
		  (replace-regexp-in-string " / " "\n" inputStr)))

	  (if workOnStringP
	      outputStr
	    (save-excursion
	      (delete-region ξfrom ξto)
	      (goto-char ξfrom)
	      (insert outputStr)
	      (vz-prepend-to-region-if-not-exists ξfrom (point))
	      )))))
     (save-excursion
       (while (not (looking-at "^$"))
	 (if (looking-at "^* $")
	     (progn
	       (replace-match "")
	       ))
	 (forward-line))))

(defun vz-split-setlist-region (ξstring &optional ξfrom ξto)
  "Split Springsteen setlists.

When called interactively, work on current paragraph or text selection.

When called in Lisp code, if ΞSTRING is non-nil, returns a changed string.
If ΞSTRING nil, change the text in the region between positions ΞFROM ΞTO."
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'paragraph)))
       (list nil (car bds) (cdr bds)))))

     (let ((ξtemp-syn-table (make-syntax-table)))
      (modify-syntax-entry ?’ "w" ξtemp-syn-table)
      (modify-syntax-entry ?' "w" ξtemp-syn-table)
      (with-syntax-table ξtemp-syn-table

	(let (workOnStringP inputStr outputStr)
	  (setq workOnStringP (if ξstring t nil))
	  (capitalize-region ξfrom ξto)
	  (vz-title-case-region-or-line ξfrom ξto)
	  (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
	  (setq outputStr
		(let ((case-fold-search t))
		  (replace-regexp-in-string " / " "\n" inputStr)))

	  (if workOnStringP
	      outputStr
	    (save-excursion
	      (delete-region ξfrom ξto)
	      (goto-char ξfrom)
	      (insert outputStr)
	      (vz-prepend-to-region-if-not-exists ξfrom (point))
	      )))))
     (save-excursion
       (while (not (looking-at "^$"))
	 (if (looking-at "^* Soundcheck:\\( includes\\)? ")
	     (progn
	       (replace-match "# Soundcheck")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* Pre-Set: ")
	     (progn
	       (replace-match "# Pre-set")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* Post show: ")
	     (progn
	       (replace-match "# Post show")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* Rehearsal: ")
	     (progn
	       (replace-match "# Rehearsal")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* Broadcast: ")
	     (progn
	       (replace-match "# Broadcast")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* Solo acoustic pre-set: ")
	     (progn
	       (replace-match "# Solo acoustic pre-set")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* Pre-show (solo acoustic): ")
	     (progn
	       (replace-match "# Pre-show (solo acoustic)")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* Morning: ")
	     (progn
	       (replace-match "# Morning")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* Evening: ")
	     (progn
	       (replace-match "# Evening")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* Early show: ")
	     (progn
	       (replace-match "# Early show")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* Early (First) Show: ")
	     (progn
	       (replace-match "# Early (first) show")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* Late show: ")
	     (progn
	       (replace-match "# Late show")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* Late (Second) Show: ")
	     (progn
	       (replace-match "# Late (second) show")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* First set: ")
	     (progn
	       (replace-match "# First set")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* Second set: ")
	     (progn
	       (replace-match "# Second set")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* Private session: ")
	     (progn
	       (replace-match "# Private session")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* With audience: ")
	     (progn
	       (replace-match "# With audience")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* With R.E.M.: ")
	     (progn
	       (replace-match "# With R.E.M.")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* Closing Jam: ")
	     (progn
	       (replace-match "# Closing jam")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* The following have also been reported: ")
	     (progn
	       (replace-match "# The following have also been reported")
	       (newline)
	       ;; Make sure first word is uppercased, in case there is "includes" after "Soundcheck:"
	       (save-excursion
		 (capitalize-word 1))
	       (insert "* ")))
	 (if (looking-at "^* $")
	     (progn
	       (replace-match "* ")
	       (newline)
	       (insert "# Concert")))
	 (forward-line))))

(defun vz-split-setlist-and-urlify (ξstring &optional ξfrom ξto)
  "Split Springsteen setlists and URLify for MusicBrainz usage in events.
ΞSTRING is the region between ΞFROM and ΞTO."
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'paragraph)))
       (list nil (car bds) (cdr bds)))))
  (vz-split-setlist-region nil ξfrom ξto)
  (vz-mb-fixup-titles)
  (vz-mb-urlify)
  (save-excursion
    (while (not (looking-at "^$"))
      (if (looking-at "^* $")
	  (replace-match ""))
      (forward-line))))

(defun vz-title-case-region-or-line (begin end)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of,
the, a, in, or, and, …}.  If a word already contains cap letters
such as HTTP, URL, they are left as is.

When called in a Lisp program, BEGIN END are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2015-05-07"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           p1
           p2
;          (skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
           (skipChars "^\"<>(){}[]“”‘‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward skipChars (line-beginning-position))
         (setq p1 (point))
         (skip-chars-forward skipChars (line-end-position))
         (setq p2 (point)))
       (list p1 p2))))
  (let* (
         (strPairs [
		    ;; articles
		    [" A " " a "]
		    [" An " " an "]
		    [" The " " the "]
		    ;; coordinate conjunctions
		    [" And " " and "]
		    [" But " " but "]
		    [" Or " " or "]
		    [" Nor " " nor "]
		    ;; short prepositions
		    [" As " " as "]
		    [" At " " at "]
		    [" By " " by "]
		    [" For " " for "]
		    [" In " " in "]
		    [" Of " " of "]
		    [" On " " on "]
		    [" To " " to "]
		    [" Cum " " cum "]
		    [" Mid " " mid "]
		    [" Off " " off "]
		    [" Per " " per "]
		    [" Qua " " qua "]
		    [" Re " " re "]
		    [" Via " " via "]
		    ;; apastrophe problem like "What'd", "I'll", "I'm", "She's", "Don't", "We've" etc.
		    ["’Ll " "’ll "]
		    ["’D " "’d "]
		    ["’M " "’m "]
		    ["’S " "’s "]
		    ["’T " "’t "]
		    ["’Ve " "’ve "]
		    ;; specials
		    ["'" "’"]
		    ["(With All Including" "(with all including"]
		    ["(With All Performers" "(with all performers"]
		    ["(With Jennifer Nettles and All Performers" "(with Jennifer Nettles and all performers"]
		    ["(With Pete Seeger and All Performers" "(with Pete Seeger and all performers"]
		    ["(With All" "(with all"]
		    ["(With Strings" "(with strings"]
		    ["(With Bongos" "(with bongos"]
		    ["(With the Miami Horns" "(with The Miami Horns"]
		    ["(With the Roots" "(with The Roots"]
		    ["(With Paul Mccartney" "(with Paul McCartney"]
		    ["(With" "(with"]
		    ["(All With" "(all with"]
		    [" and Intro" " and intro"]
		    [" From Here" " from here"]
		    [" Various Audience Members" " various audience members"]
		    [" With Bruce" " with Bruce"]
		    ["(Alternate Version" "(alternate version"]
		    ["(Break in Middle" "(break in middle"]
		    ["(Cut" "(cut"]
		    ["Tape Turn" "tape turn"]
		    ["Cut End" "cut end"]
		    ["(Edit" "(edit"]
		    ["(Edits" "(edits"]
		    ["(Edited" "(edited"]
		    ["(End Slightly Cut" "(end slightly cut"]
		    ["(End Cut" "(end cut"]
		    ["(End Only" "(end only"]
		    ["(Very Cut" "(very cut"]
		    ["(Fades In" "(fades in"]
		    ["(Fades" "(fades"]
		    ["(Heavily Cut" "(heavily cut"]
		    ["(Minor Cuts" "(minor cuts"]
		    ["(Power Cut" "(power cut"]
		    ["(Tape Warp" "(tape warp"]
		    ["(Splice Start" "(splice start"]
		    ["(Splice" "(splice"]
		    ["(Speed Fluctuations" "(speed fluctuations"]
		    ["(Start and End Cut, Skips" "(start and end cut, skips"]
		    ["(Start and End Cut" "(start and end cut"]
		    ["(Start Cut" "(start cut"]
		    ["(Start Clipped" "(start clipped"]
		    ["(Start Slightly Cut" "(start slightly cut"]
		    ["(Slight Cut" "(slight cut"]
		    ["(Intro Only" "(intro only"]
		    ["(Intro" "(intro"]
		    ["(Instrumental" "(instrumental"]
		    ["(Instrumental Intro Only" "(instrumental intro only"]
		    ["(instrumental, Partial" "(instrumental, partial"]
		    ["(Introduction Only" "(introduction only"]
		    ["(Long" "(long"]
		    ["(Part" "(part"]
		    ["(Reprise" "(reprise"]
		    ["(Harmonies" "(harmonies"]
		    ["(Several Takes" "(several takes"]
		    ["(Two Takes" "(two takes"]
		    ["(Several Times" "(several times"]
		    ["(Several Run-Throughs" "(several run-throughs"]
		    ["(Small Snippet" "(small snippet"]
		    ["(Multiple Takes" "(multiple takes"]
		    ["(Multiple Times" "(multiple times"]
		    ["(4 Takes" "(4 takes"]
		    ["(Vocal Ad-Lib" "(vocal ad-lib"]
		    ["String Section" "string section"]
		    ["Cbs" "CBS"]
		    ["Et Al." "et al."]
		    ;; Special song titles
		    ["Swlabr" "SWLABR"]
		    ["Unknown Gospel Song" "[unknown gospel song]"]
		    ["Unknown Instrumental" "[unknown instrumental]"]
		    ["Unknown" "[unknown]"]
		    ["Unidentified Instrumental" "[unidentified instrumental]"]
		    ["Unidentified Title" "[unidentified title]"]
		    ["Unidentified" "[unidentified]"]
		    ["Springsteen Announcements" "[Springsteen announcements]"]
		    ["Blues Improvisation" "[blues improvisation]"]
		    ["Jam Improvisation #1" "[jam improvisation #1]"]
		    ["Jam Improvisation #2" "[jam improvisation #2]"]
		    ["..." "[...]"]
		    ]))
    (save-excursion
      (save-restriction
        (narrow-to-region begin end)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda (x)
             (goto-char (point-min))
             (while
                 (search-forward (aref x 0) nil t)
               (replace-match (aref x 1) 'FIXEDCASE 'LITERAL)))
           strPairs))))))

(defun vz-prepare-for-mb-artist-search (ξstring &optional ξfrom ξto)
  "Prepare artist list for artist search with MusicBrainz API.
ΞSTRING is the region between ΞFROM and ΞTO."
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'paragraph)))
       (list nil (car bds) (cdr bds)))))
  (progn
    (beginning-of-line)
    (save-excursion
      (while (not (looking-at "^$"))
	(progn
	  (beginning-of-line)
	  (insert "(browse-url \"https://musicbrainz.org/ws/2/artist?query=artist:\\\"")
	  (end-of-line)
	  (insert "\\\"\")")
	  (forward-line))))
    (progn
      (while (not (looking-at "^$"))
	(end-of-line)
	(eval-last-sexp nil)
	(forward-line)
	(sleep-for 1)))
    ))

(defun vz-prepare-for-vz-bruce-el (ξstring &optional ξfrom ξto)
  "Prepare artist list for usage in vz-bruce.el.
ΞSTRING is the region between ΞFROM and ΞTO."
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'paragraph)))
       (list nil (car bds) (cdr bds)))))
  (progn
    (beginning-of-line)
    (save-excursion
      (while (not (looking-at "^$"))
	(progn
	  (beginning-of-line)
	  (insert "(\"\" . \"")
	  (end-of-line)
	  (insert "\")")
	  (forward-line))))
    (forward-char 2)))

(defun vz-mb-mbid-prepare ()
  "Prepare line for MBID association."
  (interactive)
  (save-excursion
    (while (looking-at "[;!] ")
      (progn
	(if (string= (string (char-after (point))) ";")
	    (forward-line 1)
	  (progn
	    (forward-char 2)
	    (let* (
		   (beg (point))
		   (end (line-end-position))
		   (track (buffer-substring beg end))
		   )
	      (progn
		(kill-region beg end)
		(delete-backward-char 2)
		(insert (concat "\(\"\" . \"" track "\"\)"))
		(beginning-of-line)))
	    (forward-line 1))
	  ))))
  (forward-char 2))

(defun vz-prepare-for-copying-artist-URL-to-MB-event-page ()
  "Prepare for copying artist URL to MB event page."
  (interactive)
  (save-excursion
    (while (looking-at "[@#] ")
      (progn
	(if (string= (string (char-after (point))) "#")
	    (forward-line 1)
	  (progn
	    (delete-char 3)
	    (insert "https://musicbrainz.org/artist/")
	    (search-forward "|")
	    (backward-char 1)
	    (kill-line)
	    (insert " - ")
	    (yank)
	    (forward-line)))))))

(defun vz-prepend-to-region-if-not-exists (beg end)
  "Prepend string to every line in region between BEG and END.
Prepend only if not already there and capitalize first and last
word in a line ALWAYS."
  (interactive "r")
  (apply-on-rectangle
   (lambda (beg end)
     (vz-capitalize-first-and-last-word)
     (if (not (looking-at "^* "))
	 (insert "* ")))
   beg end))

(defvar vz-capitalize-first-word-exception (concat
						 "\\("
						 "SWLABR"
						 "\\|"
						 "unknown]"
						 "\\|"
						 "unidentified]"
						 "\\|"
						 "instrumental]"
						 "\\)"
						 "$")
  "Words which are exceptions for capitalization of first word.")

(defvar vz-capitalize-last-word-exception (concat "\\("
						"harmonies"
						"\\|"
						"band"
						"\\|"
						"bongos"
						"\\|"
						"edit"
						"\\|"
						"here"
						"\\|"
						"instrumental"
						"\\|"
						"intro"
						"\\|"
						"long"
						"\\|"
						"members"
						"\\|"
						"middle"
						"\\|"
						"part"
						"\\|"
						"partial"
						"\\|"
						"performers"
						"\\|"
						"piano"
						"\\|"
						"song"
						"\\|"
						"only"
						"\\|"
						"cuts"
						"\\|"
						"cut"
						"\\|"
						"here"
						"\\|"
						"reprise"
						"\\|"
						"section"
						"\\|"
						"snippet"
						"\\|"
						"strings"
						"\\|"
						"takes"
						"\\|"
						"times"
						"\\|"
						"throughs"
						"\\|"
						"version"
						"\\|"
						"warp"
						"\\|"
						"all"
						"\\|"
						"al\\."
						"\\))$")
  "Words which are exceptions for capitalization of last word.")

(defun vz-capitalize-first-and-last-word ()
  "Capitalize first and last word in a line.
Do this ALWAYS, except for the above exceptions."
  (interactive)
  (save-excursion
    (let ((ξtemp-syn-table (make-syntax-table)))
      (modify-syntax-entry ?’ "w" ξtemp-syn-table)
      (with-syntax-table ξtemp-syn-table
	(goto-char (line-beginning-position))
	(forward-word)
	(backward-word)
	;; capitalize first word in any case, except for the above exceptions
	(if (not (looking-at vz-capitalize-first-word-exception))
	    (progn
	      (capitalize-word 1)
	      (goto-char (line-end-position))
	      (backward-word)
	      ;; capitalize last word in any case, except for the above exceptions
	      (if (not (looking-at vz-capitalize-last-word-exception))
		  (capitalize-word 1))
	      ))))))

(defun brucebase-download-songlist ()
  "Download studio sessions from http://brucebase.wikidot.com/Songlist."
  (interactive)
  (progn
    (setq brucebase-songlist-url "Songlist")
    (download-file (concat brucebase-url "/" brucebase-songlist-url) brucebase-download-folder brucebase-songlist-url)
    (kill-buffer brucebase-songlist-url)
    (find-file (concat brucebase-download-folder brucebase-songlist-url))
    (brucebase-extract-titles-from-songlist)
    (goto-char (point-min))
    (replace-string "&amp;" "&")
    (goto-char (point-min))
    (replace-string "&Amp;" "&")
    (goto-char (point-min))
    (let ((beg (point-min))
	  (end (point-max)))
      (sort-lines nil beg end))
    (write-file (concat brucebase-download-folder brucebase-songlist-url ".titles"))
    (let (
	  (beg (point-min))
	  (end (point-max))
	  )
      (progn
	;; Add * in front of each line and change ' to ’
	(goto-char (point-min))
	(vz-split-setlist nil beg end)
	(write-file (concat brucebase-download-folder brucebase-songlist-url ".split"))
	(vz-mb-fixup-titles)
	(write-file (concat brucebase-download-folder brucebase-songlist-url ".fixtitle"))
	(sort-lines nil beg end)
	(vz-mb-urlify)
	(write-file (concat brucebase-download-folder brucebase-songlist-url ".urlify"))
	(delete-matching-lines "^* \\[")
	(write-file (concat brucebase-download-folder brucebase-songlist-url ".missing"))))))

(defun brucebase-extract-titles-from-songlist ()
  "Extract titles from HTML songlist page."
  (interactive)
  (let
      (
       (beg (point-min))
       (end (progn
	      (re-search-forward "30 Days Out")
	      (re-search-backward "<a class=")
	      (point)))
       )
    (progn
      (goto-char (point-min))
      (delete-region beg end)))
  (progn
    (re-search-forward "Zoom Theme")
    (end-of-line)
    (newline)
    (let ((beg (point))
	  (end (point-max)))
      (progn
	(delete-region beg end)
	(goto-char (point-min)))))
  (while (not (looking-at "^$"))
    (let
      (
       (beg (point))
       (end (progn
	      (re-search-forward "<a class=")
	      (re-search-forward ">")))
       )
    (progn
      (delete-region beg end)
      (re-search-forward "<")
      (backward-char)
      (newline)
      (forward-line -1)
      (if (looking-at "^Back To Top")
	  (kill-line 2)
	(forward-line 1))))))

(defun vz-mb-catchup ()
  "Remove titles from list which have been confirmed to exist in Muscibrainz."
  (interactive)
  (progn
    (save-excursion
      (goto-char (point-min))
      (let
	  ((beg (point-min))
	   (end (point-max)))
	(progn
	  (vz-split-setlist-and-urlify nil beg end)
	  (delete-matching-lines "^* \\["))))))

(defun vz-mb-fixup-titles ()
  "Fix special title cases."
  (interactive)
  (save-excursion
    (while (looking-at "[#*] ")
      (progn
	(setq temp-fixworks mb-fixworks)
	(if (string= (string (char-after (point))) "#")
	    (forward-line 1)
	  (progn
	    (forward-char 2)
	    (while temp-fixworks
	      (setq fixwork (car temp-fixworks))
	      (setq temp-fixworks (cdr temp-fixworks))
	      (setq origtitle (car fixwork))
	      (if (looking-at origtitle)
		  (replace-match (cdr fixwork))
		))))
	(forward-line 1)
	(setq temp-fixworks mb-fixworks)))))

(defvar vz-mb-trackinfo-regexp (concat
			      ;; beginning of optional title info in brackets
			      " (\\("
			      "acoustic and electric"
			      "\\|"
			      "acoustic start only"
			      "\\|"
			      "alternate version"
			      "\\|"
			      "break in middle"
			      "\\|"
			      "mid section bridge"
			      "\\|"
			      "band"
			      "\\|"
			      "piano"
			      "\\|"
			      "\\(\\(end\\|start\\)? \\)?cut"
			      "\\|"
			      "snippet"
			      "\\|"
			      "cuts"
			      "\\|"
			      "edit"
			      "\\|"
			      "tape warp"
			      "\\|"
			      "power cut"
			      "\\|"
			      "several run-throughs"
			      "\\|"
			      "vocal ad-lib"
			      "\\|"
			      ;; if the title has really been played
			      "\\?"
			      "\\|"
			      "x[[:digit:]]"
			      "\\|"
			      "x[[:digit:]]-[[:digit:]]"
			      "\\|"
			      "#[[:digit:]]"
			      "\\|"
			      "intro x[[:digit:]]"
			      "\\|"
			      "with all"
			      "\\|"
			      "with the Roma Sinfonietta string section"
			      "\\|"
			      "with .*"
			      "\\|"
			      "Bobby Bandiera"
			      "\\|"
			      "Jon Bon Jovi"
			      "\\|"
			      "Jon Bon Jovi, Southside Johnny, Gary U.S. Bonds, and Bobby Bandiera"
			      "\\|"
			      "Jon Bon Jovi and Southside Johnny"
			      "\\|"
			      "Jimmy Vivino"
			      "\\|"
			      "Jimmy Vivino and Bruce"
			      "\\|"
			      "Nils Lofgren"
			      "\\|"
			      "Nils Lofgren and Bruce"
			      "\\|"
			      "Max Weinberg 7"
			      "\\|"
			      "Max Weinberg 7 With Danny Federici and Garry Tallent"
			      "\\|"
			      "Patti Scialfa"
			      "\\|"
			      "Richie Sambora"
			      "\\|"
			      "South Community Choir of Asbury Park"
			      "\\|"
			      "South Community Choir of Asbury Park with Bruce"
			      "\\|"
			      "Southside Johnny"
			      "\\|"
			      "Southside Johnny and Jon Bon Jovi"
			      "\\|"
			      "Southside Johnny and Steve Van Zandt"
			      "\\|"
			      "\\(Steve\\(n\\)? \\)?Van Zandt"
			      "\\|"
			      "The Miami Horns"
			      "\\|"
			      "all with Patti Scialfa, Lisa Lowell, Soozie Tyrell, Southside Johnny, Garry Tallent, and Nils Lofgren"
			      "\\|"
			      "x[[:digit:]] and intro"
			      "\\|"
			      "\\(\\(\\(end\\|start\\|start and end\\|heavily\\|very\\|minor\\) \\)?\\(slight\\(ly\\)? \\)?\\(clipped\\|cut.*\\|partial\\|splice.*\\|speed fluctuations\\|edits?\\(ed\\)?\\), \\)?[[:digit:]]+\\(\\.\\|:\\)[[:digit:]]+"
			      "\\|"
			      "fades\\( in\\)?, [[:digit:]]+\\(\\.\\|:\\)[[:digit:]]+"
			      "\\|"
			      "\\(instrumental\\|acoustic\\|part\\|[3-4] takes\\|end only\\|instrumental band segment\\|instrumental intro only\\|introduction only\\|instrumental, partial\\|multiple \\(times\\|takes\\)\\|end only\\|several takes\\|two takes\\|several times\\|long\\|harmonies\\|part\\|partial\\( takes?\\)?\\|reprise\\|\\(small \\)?snippet\\|complete\\)"
			      "\\|"
			      "\\(full band \\)?intro\\( only\\)?\\(, x[[:digit:]]\\)?"
			      ;; end of optional title info in brackets (at end of line)
			      "\\))\\'")
  "Trackinfo after title name which should NOT be changed in capitalization and NOT be part of the title hyperlink.")

;; Artistinfo after artist name which should NOT be changed in capitalization and NOT be part of the artist hyperlink
(defvar vz-mb-artistinfo-regexp (concat
			      ;; beginning of optional title info in brackets
			      " (\\("
			      "Guests?"
			      "\\|"
			      "acoustic start only"
			      ;; end of optional title info in brackets (at end of line)
			      "\\))\\'"))

(defun vz-mb-urlify ()
  "URLify song titles according to Musicbrainz setlist standard.  [mbid|name] allows linking to artists and works."
  (interactive)
  (save-excursion
    (while (looking-at "[#*] ")
      (progn
	(if (string= (string (char-after (point))) "#")
	    (forward-line 1)
	  (progn
	    (forward-char 2)
	    (let* (
		   (beg (point))
		   (end (line-end-position))
		   (track (buffer-substring beg end))
		   titleinfo
		   (titlenumber 0)
		   )
	      (progn
		(if (string-match vz-mb-trackinfo-regexp track)
		    (setq titleinfo (match-string 0 track)))
		(setq titlelist (split-string (replace-regexp-in-string vz-mb-trackinfo-regexp "" track) " - "))
		(kill-region beg end)
		(while titlelist
		  ;; Make sure first character of title is always uppercase
		  (setq title (vz-capitalize-first-char (car titlelist)))
		  (setq titlenumber (+ titlenumber 1))
		  (setq tw (rassoc title mb-works))
		  (if tw
		      (progn
			(if (> titlenumber 1)
			    (insert " / "))
			(insert (concat "[" (car tw) "|" (cdr tw) "]"))
			)
		    (progn
		      (if (> titlenumber 1)
			  (insert " / "))
		      (insert title)
		      ))
		  (setq titlelist (cdr titlelist)))
		(if titleinfo
		    (insert titleinfo))
		(forward-line 1)
		))))))))

(defun vz-mb-urlify-artists ()
  "URLify artists according to Musicbrainz standard.  [mbid|name] allows linking to artists and works."
  (interactive)
  (save-excursion
    (while (looking-at "[#@] ")
      (progn
	(if (string= (string (char-after (point))) "#")
	    (forward-line 1)
	  (progn
	    (forward-char 2)
	    (let* (
		   (beg (point))
		   (end (line-end-position))
		   (track (buffer-substring beg end))
		   titleinfo
		   (titlenumber 0)
		   )
	      (progn
		(if (string-match vz-mb-artistinfo-regexp track)
		    (setq titleinfo (match-string 0 track)))
		(setq titlelist (split-string (replace-regexp-in-string vz-mb-artistinfo-regexp "" track) " - "))
		(kill-region beg end)
		(while titlelist
		  ;; Make sure first character of artist is always uppercase
		  (setq title (vz-capitalize-first-char (car titlelist)))
		  (setq titlenumber (+ titlenumber 1))
		  (setq tw (rassoc title mb-artists))
		  (if tw
		      (progn
			(if (> titlenumber 1)
			    (insert " / "))
			(insert (concat "[" (car tw) "|" (cdr tw) "]"))
			)
		    (progn
		      (if (> titlenumber 1)
			  (insert " / "))
		      (insert title)
		      ))
		  (setq titlelist (cdr titlelist)))
		(if titleinfo
		    (insert titleinfo))
		(forward-line 1)
		))))))))

;; "\"[^\"]*\""
;; "“[^”]*”"
(defun vz-mb-urlify-gignote-works ()
  "Search for works in gignote and URLify."
  (interactive)
  (progn
    (beginning-of-line)
    (save-excursion
      (while
;	  (re-search-forward "[“\"][^”\"]*”\"" (point-at-eol) nil)
;	  (re-search-forward "\"[^\"]*\"" (point-at-eol) nil)
	  (re-search-forward "“[^”]*”" (point-at-eol) t)
	(let* (
	       (beg (match-beginning 0))
	       (end (match-end 0))
	       (cased (vz-title-case-region-or-line beg end))
	       (work (buffer-substring (+ beg 1) (+ end -1))))
	  (progn
	    (setq tw (rassoc work mb-works))
	    (if tw
		(progn
		  (kill-region beg end)
		  (insert (concat "[https://musicbrainz.org/work/" (car tw) "|" (cdr tw) "]"))))
            ))))))

(defun vz-mb-urlify-gignote-artists ()
  "Search for artists in gignote and URLify."
  (interactive)
  (progn
    (beginning-of-line)
    (save-excursion
      (while
	  (re-search-forward "“[^”]*”" (point-at-eol) t)
	(let* (
	       (beg (match-beginning 0))
	       (end (match-end 0))
	       (cased (vz-title-case-region-or-line beg end))
	       (artist (buffer-substring (+ beg 1) (+ end -1))))
	  (progn
	    (setq tw (rassoc artist mb-artists))
	    (if tw
		(progn
		  (kill-region beg end)
		  (insert (concat "[https://musicbrainz.org/artist/" (car tw) "|" (cdr tw) "]"))))
            ))))))

(defun vz-mb-urlify-gignote-release-groups ()
  "Search for release groups in gignote and URLify."
  (interactive)
  (progn
    (beginning-of-line)
    (save-excursion
      (while
	  (re-search-forward "“[^”]*”" (point-at-eol) t)
	(let* (
	       (beg (match-beginning 0))
	       (end (match-end 0))
	       (cased (vz-title-case-region-or-line beg end))
	       (release-group (buffer-substring (+ beg 1) (+ end -1))))
	  (progn
	    (setq tw (rassoc release-group mb-release-groups))
	    (if tw
		(progn
		  (kill-region beg end)
		  (insert (concat "[https://musicbrainz.org/release-group/" (car tw) "|" (cdr tw) "]"))))
            ))))))

(defun vz-mb-urlify-gignote-places ()
  "Search for places in gignote and URLify."
  (interactive)
  (progn
    (beginning-of-line)
    (save-excursion
      (while
	  (re-search-forward "“[^”]*”" (point-at-eol) t)
	(let* (
	       (beg (match-beginning 0))
	       (end (match-end 0))
	       (cased (vz-title-case-region-or-line beg end))
	       (place (buffer-substring (+ beg 1) (+ end -1))))
	  (progn
	    (setq tw (rassoc place mb-places))
	    (if tw
		(progn
		  (kill-region beg end)
		  (insert (concat "[https://musicbrainz.org/place/" (car tw) "|" (cdr tw) "]"))))
            ))))))

(defun vz-mb-urlify-gignote-areas ()
  "Search for areas in gignote and URLify."
  (interactive)
  (progn
    (beginning-of-line)
    (save-excursion
      (while
	  (re-search-forward "“[^”]*”" (point-at-eol) t)
	(let* (
	       (beg (match-beginning 0))
	       (end (match-end 0))
	       (cased (vz-title-case-region-or-line beg end))
	       (area (buffer-substring (+ beg 1) (+ end -1))))
	  (progn
	    (setq tw (rassoc area mb-areas))
	    (if tw
		(progn
		  (kill-region beg end)
		  (insert (concat "[https://musicbrainz.org/area/" (car tw) "|" (cdr tw) "]"))))
            ))))))

(defun vz-mb-urlify-gignote-series ()
  "Search for series in gignote and URLify."
  (interactive)
  (progn
    (beginning-of-line)
    (save-excursion
      (while
	  (re-search-forward "“[^”]*”" (point-at-eol) t)
	(let* (
	       (beg (match-beginning 0))
	       (end (match-end 0))
	       (cased (vz-title-case-region-or-line beg end))
	       (serie (buffer-substring (+ beg 1) (+ end -1))))
	  (progn
	    (setq tw (rassoc serie mb-series-tours))
	    (if tw
		(progn
		  (kill-region beg end)
		  (insert (concat "[https://musicbrainz.org/series/" (car tw) "|" (cdr tw) "]"))))
            ))))))

(defun vz-mb-urlify-gignote-labels ()
  "Search for labels in gignote and URLify."
  (interactive)
  (progn
    (beginning-of-line)
    (save-excursion
      (while
	  (re-search-forward "“[^”]*”" (point-at-eol) t)
	(let* (
	       (beg (match-beginning 0))
	       (end (match-end 0))
	       (cased (vz-title-case-region-or-line beg end))
	       (label (buffer-substring (+ beg 1) (+ end -1))))
	  (progn
	    (setq tw (rassoc label mb-labels))
	    (if tw
		(progn
		  (kill-region beg end)
		  (insert (concat "[https://musicbrainz.org/labels/" (car tw) "|" (cdr tw) "]"))))
            ))))))

(defun vz-mb-urlify-gignote ()
  "Search for works and artists in gignote and URLify."
  (interactive)
  (progn
    (vz-mb-urlify-gignote-works)
    (vz-mb-urlify-gignote-artists)
    (vz-mb-urlify-gignote-release-groups)
    (vz-mb-urlify-gignote-places)
    (vz-mb-urlify-gignote-areas)
    (vz-mb-urlify-gignote-series)
    (vz-mb-urlify-gignote-labels)
    ))

(defun vz-capitalize-first-char (&optional string)
  "Capitalize only the first character of the input STRING."
  (if (and string (> (length string) 0))
      (let ((first-char (substring string 0 1))
	    (rest-str   (substring string 1)))
	(concat (capitalize first-char) rest-str))
    ;; Return the empty string when input string is already empty
    string))

(defun vz-download-all-songlists ()
  "Download songlists from Brucebase and Springsteenlyrics.com."
  (interactive)
  (progn
    (if (file-exists-p "/tmp/brucebase")
	(rename-file "/tmp/brucebase" (concat "/tmp/brucebase-" (format-time-string "%m-%d-%Y-%H-%M-%S"))))
    (make-directory "/tmp/brucebase")
    ;; Brucebase
    (brucebase-download-songlist)
    (brucebase-download-studio-sessions)
    (brucebase-titelize-studio-sessions)
    ;; SpringsteenLyrics
    (springsteenlyrics-download-titles)
    (springsteenlyrics-extract-titles)
    ;; Muscibrainz
    (musicbrainz-download-titles)
    (musicbrainz-extract-songlist)
    ))

(defun download-file (&optional url download-dir download-name)
  "Download URL under DOWNLOAD-DIR with DOWNLOAD-NAME."
  (interactive)
  (let ((url (or url
                 (read-string "Enter download URL: "))))
    (let ((download-buffer (url-retrieve-synchronously url)))
      (save-excursion
        (set-buffer download-buffer)
        ;; we may have to trim the http response
        (goto-char (point-min))
        (re-search-forward "^$" nil 'move)
        (forward-char)
        (delete-region (point-min) (point))
        (write-file (concat (or download-dir
                                "~/downloads/")
                            (or download-name
                                (car (last (split-string url "/" t))))))))))

(defvar brucebase-url "http://brucebase.wikidot.com/")
(defvar brucebase-download-folder "/tmp/brucebase/")
(defvar brucebase-file-title "BruceBase")

(defvar musicbrainz-titles-url "http://musicbrainz.org/ws/2/artist/70248960-cb53-4ea4-943a-edb18f7d336f?inc=work-rels&fmt=xml")
(defvar musicbrainz-download-folder "/tmp/brucebase/")
(defvar musicbrainz-title-filename "MusicBrainzTitles")

(defvar springsteenlyrics-url "http://www.springsteenlyrics.com/lyrics.php?cmd=list")
(defvar springsteenlyrics-download-folder "/tmp/brucebase/")
(defvar springsteenlyrics-file-title "SpringsteenLyrics")

(defun springsteenlyrics-download-titles ()
  "Download lyrics page from http://www.springsteenlyrics.com."
  (interactive)
  (download-file springsteenlyrics-url springsteenlyrics-download-folder springsteenlyrics-file-title)
  (kill-buffer springsteenlyrics-file-title))

(defun brucebase-download-studio-sessions ()
  "Download studio sessions from http://brucebase.wikidot.com."
  (interactive)
  (setq brucebase-studio-sessions brucebase-studio-sessions-list)
  (while brucebase-studio-sessions
    (progn
      (setq brucebase-studio-session (car brucebase-studio-sessions))
      (setq brucebase-session-name (car brucebase-studio-session))
      (setq brucebase-session-url (cdr brucebase-studio-session))
      (setq brucebase-studio-sessions (cdr brucebase-studio-sessions))
      (download-file (concat brucebase-url brucebase-session-url) brucebase-download-folder brucebase-session-name)
      (kill-buffer brucebase-session-name))))

(defun brucebase-titelize-studio-sessions ()
  "Titelize studio sessions from http://brucebase.wikidot.com."
  (interactive)
  (if (file-exists-p (concat brucebase-download-folder brucebase-file-title))
      (delete-file (concat brucebase-download-folder brucebase-file-title)))
  (setq brucebase-studio-sessions brucebase-studio-sessions-list)
  (while brucebase-studio-sessions
    (save-excursion
      (setq brucebase-studio-session (car brucebase-studio-sessions))
      (setq brucebase-session-name (car brucebase-studio-session))
      (setq brucebase-studio-sessions (cdr brucebase-studio-sessions))
      (find-file (concat brucebase-download-folder brucebase-session-name))
      (write-file (concat brucebase-download-folder brucebase-session-name ".orig"))
      (vz-mb-extract-from-studiosessions)
      (write-file (concat brucebase-download-folder brucebase-session-name ".titles"))
      (let ((beg (point-min))
	    (end (point-max)))
	(progn
	  (sort-lines nil beg end)
	  (write-file (concat brucebase-download-folder brucebase-session-name ".titles.sort"))
	  ))
      (brucebase-trim-versions)
      (write-file (concat brucebase-download-folder brucebase-session-name ".titles.sort.trim"))
      (let ((beg (point-min))
	    (end (point-max)))
	(progn
	  (delete-duplicate-lines beg end)
	  (write-file (concat brucebase-download-folder brucebase-session-name ".titles.sort.trim.deldups"))
	  ))
      (let ((beg (point-min))
	    (end (point-max)))
	(append-to-file beg end (concat brucebase-download-folder brucebase-file-title)))
      (kill-buffer (concat brucebase-session-name ".titles.sort.trim.deldups"))))
  (progn
    (find-file (concat brucebase-download-folder brucebase-file-title))
    (let ((beg (point-min))
	  (end (point-max)))
      (progn
	(sort-lines nil beg end)
	(write-file (concat brucebase-download-folder brucebase-file-title ".sort"))
	))
    (let ((beg (point-min))
	  (end (point-max)))
      (progn
	(delete-duplicate-lines beg end)
	(write-file (concat brucebase-download-folder brucebase-file-title ".sort.deldups"))
	))
    (let ((beg (point-min))
	  (end (point-max)))
      (progn
	;; Add * in front of each line and change ' to ’
	(goto-char (point-min))
	(vz-split-setlist nil beg end)
	(write-file (concat brucebase-download-folder brucebase-file-title ".split"))
	(vz-mb-fixup-titles)
	(write-file (concat brucebase-download-folder brucebase-file-title ".fixtitle"))
	(let ((beg (point-min))
	      (end (point-max)))
	  (delete-duplicate-lines beg end))
	(vz-mb-urlify)
	(write-file (concat brucebase-download-folder brucebase-file-title ".urlify"))
	(delete-matching-lines "^* \\[")
	(write-file (concat brucebase-download-folder brucebase-file-title ".missing"))
	(brucebase-delete-wellknown-titles)
	(save-buffer)
	(kill-buffer)
	))))

(defun brucebase-delete-wellknown-titles ()
  "Delete wellknown titles.  All are a.k.a's except the dummy: Song Title."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (looking-at "^$"))
      (when (looking-at brucebase-well-known-titles)
	(kill-line 1))
      (forward-line))))

(defun brucebase-trim-versions ()
  "Trim - Vx from end of titles and also get rid of (songwriter)."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward (concat "\\("
				    "##?#?"
				    "\\|"
				    " \\((.*Mix.*) \\)?\\(-\\|–\\) V[0-9][0-9]?[a-z#]?"
				    "\\|"
				    " \\((Williamson / Springsteen)\\|(Springsteen / Theiss)\\|(Springsteen / Van Zandt)\\|(Alfano)\\|(instrumental)\\|(Live 1975-12-12)\\|(.*Mix.*)\\)"
				    "\\)$") nil t)
    (replace-match "")))

(defun springsteenlyrics-extract-titles ()
  "Extract titlelist from springsteenlyrics.com."
  (interactive)
  (progn
    (find-file (concat springsteenlyrics-download-folder springsteenlyrics-file-title))
    (goto-char (point-min))
    ;; Get rid off header and footer stuff
    (let ((beg (point))
	  (end (progn
		 (re-search-forward "<h3 class=\"heading\">All lyrics")
		 (forward-line 2)
		 (point))))
      (delete-region beg end)
      (search-forward "<hr>")
      (forward-line -1)
      (let ((beg (point))
	    (end (point-max)))
	(delete-region beg end))
      (goto-char (point-min))
      ;; Extract titles
      (save-excursion
	(while (not (looking-at "^$"))
	  (let ((beg (point))
		(end (progn
		       (re-search-forward "<a href=")
		       (re-search-forward ">")
		       (point))))
	    (delete-region beg end)
	    (re-search-forward "<b")
	    (backward-char 2)
	    (kill-line)
	    (forward-line)))
	(goto-char (point-min))
	(replace-string "&amp;" "&")
	(goto-char (point-min))
	(replace-string "&#039;" "'")
	(write-file (concat springsteenlyrics-download-folder springsteenlyrics-file-title ".html.titles"))
	(goto-char (point-min))
	(while (not (looking-at "^$"))
	  (re-search-forward "<")
	  (backward-char)
	  (kill-line)
	  (forward-line))
	(let ((beg (point-min))
	      (end (point-max)))
	  (progn
	    (sort-lines nil beg end)))
	(let ((beg (point-min))
	      (end (point-max)))
	  (progn
	    (delete-duplicate-lines beg end)))
	(write-file (concat springsteenlyrics-download-folder springsteenlyrics-file-title ".titles"))
	(let ((beg (point-min))
	      (end (point-max)))
	  (progn
	    (goto-char (point-min))
	    (vz-split-setlist nil beg end)
	    (write-file (concat springsteenlyrics-download-folder springsteenlyrics-file-title ".split"))
	    (vz-mb-fixup-titles)
	    (write-file (concat springsteenlyrics-download-folder springsteenlyrics-file-title ".fixtitle"))
	    (let ((beg (point-min))
		  (end (point-max)))
	      (delete-duplicate-lines beg end))
	    (vz-mb-urlify)
	    (write-file (concat springsteenlyrics-download-folder springsteenlyrics-file-title ".urlify"))
	    (delete-matching-lines "^* \\[")
	    (write-file (concat springsteenlyrics-download-folder springsteenlyrics-file-title ".missing"))
	    (kill-buffer (concat springsteenlyrics-file-title ".missing"))))))))

(defun vz-mb-extract-from-studiosessions ()
  "Extract titlelist from studio sessions."
  (interactive)
  (save-excursion
    (progn
      (goto-char (point-min))
      (let ((beg (point))
	    (end (progn
		   (re-search-forward "<table class=\"wiki_table")
		   (beginning-of-line)
		   (point))))
	(delete-region beg end)
	(kill-line 1))
      (while (re-search-forward "<tr>" nil t)
	(let ((beg (progn
		     (beginning-of-line)
		     (point)))
	      (end (progn
		     (if (re-search-forward "<td><tt><strong>" nil t)
			 (point)
		       (progn
			 ;; (end-of-buffer)
			 (goto-char (point-max))
			 (point))))))
	  (progn
	    (delete-region beg end)
	    (if (not (looking-at "^$"))
		(progn
		  (re-search-forward "<" nil t)
		  (backward-char 1)
		  (let ((beg (point))
			(end (progn
			       (re-search-forward "<tr>" nil t)
			       (beginning-of-line)
			       (point)
			       )))
		    (progn
		      (delete-region beg end)
		      (newline)))))))))))

(defun musicbrainz-download-titles ()
  "Download lyrics page from http://www.springsteenlyrics.com."
  (interactive)
  (download-file musicbrainz-titles-url musicbrainz-download-folder musicbrainz-title-filename)
  (kill-buffer musicbrainz-title-filename))

(defun musicbrainz-extract-songlist ()
  "Extract songlist from http://musicbrainz.org/ws/2/artist/70248960-cb53-4ea4-943a-edb18f7d336f?inc=work-rels&fmt=xml."
  (interactive)
  (find-file (concat musicbrainz-download-folder musicbrainz-title-filename))
  (progn
    (while (re-search-forward "<work id=" nil t)
      (newline)
      (re-search-forward "</")
      (backward-char 2)
      (newline))
    (goto-char (point-min))
    (while (not (looking-at "^$"))
      (when (looking-at "<")
	(kill-line 1))
      (forward-line))
    (goto-char (point-min))
    (save-excursion
      (replace-regexp "><title>" " . "))
    (goto-char (point-min))
    (replace-string "&amp;" "&")
    (goto-char (point-min))
    (replace-string "&quot;" "\\\"")
    (let ((beg (point-min))
	  (end (point-max)))
      (delete-duplicate-lines beg end))
    (goto-char (point-min)))
  (while (not (looking-at "^$"))
    (insert "(")
    (re-search-forward "\\.")
    (forward-char)
    (insert "\"")
    (end-of-line)
    (insert "\")")
    (forward-line))
  (goto-char (point-min))
  (let ((beg (point-min))
  	(end (point-max)))
    (sort-fields 1 beg end))
  (write-file (concat musicbrainz-download-folder musicbrainz-title-filename ".sort-mbid.el"))
  (goto-char (point-min))
  (let ((beg (point-min))
  	(end (point-max)))
    (sort-fields 3 beg end))
  (write-file (concat musicbrainz-download-folder musicbrainz-title-filename ".sort-title.el"))
  (goto-char (point-min))
  (insert "(setq works '(\n;; Bruce Springsteen\n")
  (end-of-buffer)
  (insert "))\n")
  (write-file (concat musicbrainz-download-folder musicbrainz-title-filename ".el"))
  (kill-buffer))

(defun brucebase-extract-soundcheck ()
  "Extract songs from soundcheck."
  (interactive)
  (find-file "~/bruce/mb.setlist")
  (goto-char (point-min))
  (while (search-forward "# Soundcheck" nil t)
    (forward-line)
    (let ((beg (point))
	  (end (progn
		 (re-search-forward "^$" nil t)
		 (point))))
      (append-to-file beg end "~/bruce/songlist.soundcheck"))))

(defun brucebase-extract-concert ()
  "Extract songs from concerts."
  (interactive)
  (find-file "~/bruce/mb.setlist")
  (goto-char (point-min))
  (search-forward "# Soundcheck" nil t)
  (beginning-of-line)
  (let ((beg (point-min))
	(end (point)))
    (append-to-file beg end "~/bruce/songlist.concert"))
  (while (search-forward "# Soundcheck" nil t)
    (re-search-forward "^$" nil t)
    (let ((beg (point))
	  (end (progn
		 (search-forward "# Soundcheck" nil t)
		 (beginning-of-line)
		 (forward-char -1)
		 (point))))
      (progn
	(append-to-file beg end "~/bruce/songlist.concert")
	(forward-char 1)
	(beginning-of-line))
      ))
  (let ((beg (point))
	(end (point-max)))
    (append-to-file beg end "~/bruce/songlist.concert")))

(defun brucebase-extract-songs-from-songlist (songlist)
  "Extract songs from SONGLIST."
  (interactive)
  (find-file songlist)
;;  (find-file "~/bruce/songlist.concert")
;;  (find-file "~/bruce/songlist.soundcheck")
  (goto-char (point-min))
  (delete-non-matching-lines ".*|.*")
  (goto-char (point-min))
  (delete-non-matching-lines "^* ")
  (goto-char (point-min))
  (replace-string "’" "'")
  (goto-char (point-min))
  (replace-string " / " "\n")
  (goto-char (point-min))
  (while (not (looking-at "^$"))
    (progn
      (beginning-of-line)
      (let ((beg (point))
	    (end (progn
		   (search-forward "|")
		   (point))))
	(progn
	  (delete-region beg end)
	  (search-forward "]")
	  (forward-char -1)
	  (kill-line)
	  (forward-line)))))
  (goto-char (point-min))
  (let ((beg (point))
	(end (point-max)))
    (sort-lines nil beg end))
  (goto-char (point-min))
  (let ((beg (point))
	(end (point-max)))
    (delete-duplicate-lines beg end))
  (goto-char (point-min))
  (let ((beg (point-min))
	(end (point-max)))
    (vz-split-setlist nil beg end)
    (write-file (concat songlist ".split"))
    (vz-mb-fixup-titles)
    (write-file (concat songlist ".fixtitle"))
    (sort-lines nil beg end))
    (vz-mb-urlify)
    (write-file (concat songlist ".urlify"))
    )

(defun brucebase-extract-event-songs ()
  "Extract songs from all events."
  (interactive)
  (brucebase-extract-soundcheck)
  (brucebase-extract-concert)
  (brucebase-extract-songs-from-songlist "~/bruce/songlist.soundcheck")
  (brucebase-extract-songs-from-songlist "~/bruce/songlist.concert"))

;; # with
;; @ [c7589842-71c8-460a-a0ae-7833b8a76fe0|The Horns of Love]

;; # &
;; @ [d6652e7b-33fe-49ef-8336-4c863b4f996f|The E Street Band]

;; # with
;; @ [7bef92eb-f2b1-4790-935a-6e411eff406e|The 1992-93 World Tour Band]

;; # with
;; @ [9a214f8b-f606-47d7-803f-f1b96854ae14|Kevin Buell]

;; # with
;; @ [42a636a0-dbe4-4d0c-abe2-1590fad9531b|Tom Morello]

;; # and
;; @ [c7589842-71c8-460a-a0ae-7833b8a76fe0|The Miami Horns]
;; # with
;; @ [dff7ce32-c867-4dcf-b84a-129bc6678fbb|John Binkley]
;; @ [288f2a2f-404f-4f89-bc45-93f3b0975243|Ed De Palma]
;; @ [c10f955a-fb44-4733-a828-c1842af110c2|Dennis Orlock]
;; @ [eda4b070-ab02-42e5-8578-9c39fa2a7f22|Steve Paraczky]

;;; 1984 - Born In The U.S.A. (-10)
;; # &
;; @ [d6652e7b-33fe-49ef-8336-4c863b4f996f|The E Street Band]
;; # with
;; @ [11d2fcfe-669d-4596-8921-e07dbdae311f|Roy Bittan]
;; @ [7e4bfa5f-a8b8-4fb0-81b5-f74f6ac72133|Clarence Clemons]
;; @ [94a88a40-8568-403e-86e6-8c01fd4b626a|Danny Federici]
;; @ [a1ef6bc8-2644-4b6d-aa21-27b630acf751|Nils Lofgren]
;; @ [f09aa40c-b613-4ea2-a8cf-6056c2657a9a|Patti Scialfa]
;; @ [42b42dd1-9263-4eae-91cd-4014a5b5d39f|Garry Tallent]
;; @ [2566ca73-1dfd-49e7-ab20-dfa5697b360e|Max Weinberg]

;;; 2003 - The Reunion Tour (-10)
;; # &
;; @ [d6652e7b-33fe-49ef-8336-4c863b4f996f|The E Street Band]
;; # with
;; @ [42b42dd1-9263-4eae-91cd-4014a5b5d39f|Garry Tallent]
;; @ [2566ca73-1dfd-49e7-ab20-dfa5697b360e|Max Weinberg]
;; @ [7e4bfa5f-a8b8-4fb0-81b5-f74f6ac72133|Clarence Clemons]
;; @ [11d2fcfe-669d-4596-8921-e07dbdae311f|Roy Bittan]
;; @ [de53495e-ad5a-4c30-82ab-05e7e3ec7b4d|Steven Van Zandt]
;; @ [a1ef6bc8-2644-4b6d-aa21-27b630acf751|Nils Lofgren]
;; @ [f09aa40c-b613-4ea2-a8cf-6056c2657a9a|Patti Scialfa]

;;; 2003 - The Rising Tour (-12)
;; # &
;; @ [d6652e7b-33fe-49ef-8336-4c863b4f996f|The E Street Band]
;; # with
;; @ [94a88a40-8568-403e-86e6-8c01fd4b626a|Danny Federici]
;; @ [42b42dd1-9263-4eae-91cd-4014a5b5d39f|Garry Tallent]
;; @ [2566ca73-1dfd-49e7-ab20-dfa5697b360e|Max Weinberg]
;; @ [7e4bfa5f-a8b8-4fb0-81b5-f74f6ac72133|Clarence Clemons]
;; @ [11d2fcfe-669d-4596-8921-e07dbdae311f|Roy Bittan]
;; @ [de53495e-ad5a-4c30-82ab-05e7e3ec7b4d|Steven Van Zandt]
;; @ [a1ef6bc8-2644-4b6d-aa21-27b630acf751|Nils Lofgren]
;; @ [f09aa40c-b613-4ea2-a8cf-6056c2657a9a|Patti Scialfa]
;; @ [065af1a2-2fa9-4864-852e-08c00c9c67d8|Soozie Tyrell]


;;; 2005 - Devils & Dust Solo and Acoustic Tour (-3)
;; # with
;; @ [6f5d1a5f-e357-4c1a-bbbe-1ec41f83e2ab|Alan Fitzgerald]
;; @ [9a214f8b-f606-47d7-803f-f1b96854ae14|Kevin Buell]


;;; 2006 - Seeger Sessions Tour (-3)
;; # with
;; @ [64a3f3a8-afbf-4658-84f4-5c5136732c76|Marc Anthony Thompson]
;; @ [f09aa40c-b613-4ea2-a8cf-6056c2657a9a|Patti Scialfa]
;; @ [596abd9c-1d81-4777-bb3e-19a81b7c6e24|Frank Bruno]
;; @ [065af1a2-2fa9-4864-852e-08c00c9c67d8|Soozie Tyrell]
;; @ [292c8a8a-4258-4f13-995d-a8e1267277f9|Sam Bardfeld]
;; @ [949e6ff1-5b51-40c1-8ea0-b4ef66f8c968|Greg Liszt]
;; @ [19ddafef-2335-41b0-a092-9d825c3d4fa7|Marty Rifkin]
;; @ [4382fa5d-03de-4ebf-baf3-df6a1d1922f0|Charles Giordano]
;; @ [11776de0-04bb-4b98-befb-6fe4ab56eba5|Jeremy Chatzky]
;; @ [687d34b2-6d05-4183-83ad-b8c66079d6ef|Larry Eagle]
;; @ [59d77428-bf32-4c9f-bc4c-7c03ec882c59|Lisa Lowell]
;; @ [7b18698b-33fb-4865-b158-e74f3457eeb7|Curtis King Jr.]
;; @ [0a84499b-cf33-43cd-b29d-8b2fb67ebbb3|Cindy Mizelle]
;; @ [198090a7-fb18-4e7a-9390-912afdccfeb0|Art Baron]
;; @ [6a3394ba-6888-4dd0-93f3-06c1e35749d8|Eddie Manion]
;; @ [6ae5508c-cec6-4d23-a3df-92e8b1e988b2|Mark Pender]
;; @ [028fd996-fe8f-41b6-a6ed-1cb4b06a23d2|Curt Ramm]
;; @ [0c59859b-9de2-43d1-aaf7-4c54cef5f14a|Richie “La Bamba” Rosenberg]
;; @ [9b303c28-f0a0-4c76-8293-c97272f6e32c|Clark Gayton]


;;; 2007 "Magic" Tour (-13)
;; # &
;; @ [d6652e7b-33fe-49ef-8336-4c863b4f996f|The E Street Band]
;; # with
;; @ [94a88a40-8568-403e-86e6-8c01fd4b626a|Danny Federici]
;; @ [42b42dd1-9263-4eae-91cd-4014a5b5d39f|Garry Tallent]
;; @ [2566ca73-1dfd-49e7-ab20-dfa5697b360e|Max Weinberg]
;; @ [7e4bfa5f-a8b8-4fb0-81b5-f74f6ac72133|Clarence Clemons]
;; @ [11d2fcfe-669d-4596-8921-e07dbdae311f|Roy Bittan]
;; @ [de53495e-ad5a-4c30-82ab-05e7e3ec7b4d|Steven Van Zandt]
;; @ [a1ef6bc8-2644-4b6d-aa21-27b630acf751|Nils Lofgren]
;; @ [f09aa40c-b613-4ea2-a8cf-6056c2657a9a|Patti Scialfa]
;; @ [065af1a2-2fa9-4864-852e-08c00c9c67d8|Soozie Tyrell]
;; @ [4382fa5d-03de-4ebf-baf3-df6a1d1922f0|Charles Giordano]


;;; 2009 Working On A Dream Tour (-14)
;; # &
;; @ [d6652e7b-33fe-49ef-8336-4c863b4f996f|The E Street Band]
;; # with
;; @ [42b42dd1-9263-4eae-91cd-4014a5b5d39f|Garry Tallent]
;; @ [2566ca73-1dfd-49e7-ab20-dfa5697b360e|Max Weinberg]
;; @ [7e4bfa5f-a8b8-4fb0-81b5-f74f6ac72133|Clarence Clemons]
;; @ [11d2fcfe-669d-4596-8921-e07dbdae311f|Roy Bittan]
;; @ [de53495e-ad5a-4c30-82ab-05e7e3ec7b4d|Steven Van Zandt]
;; @ [a1ef6bc8-2644-4b6d-aa21-27b630acf751|Nils Lofgren]
;; @ [f09aa40c-b613-4ea2-a8cf-6056c2657a9a|Patti Scialfa]
;; @ [065af1a2-2fa9-4864-852e-08c00c9c67d8|Soozie Tyrell]
;; @ [4382fa5d-03de-4ebf-baf3-df6a1d1922f0|Charles Giordano]
;; @ [447f5836-ec6f-4f17-861d-1eafb628030c|Curtis King Jr.]
;; @ [0a84499b-cf33-43cd-b29d-8b2fb67ebbb3|Cindy Mizelle]

;; @ [028fd996-fe8f-41b6-a6ed-1cb4b06a23d2|Curt Ramm]


;;; 2014 High Hopes Tour (-14)
;; # &
;; @ [d6652e7b-33fe-49ef-8336-4c863b4f996f|The E Street Band]
;; # with
;; @ [42b42dd1-9263-4eae-91cd-4014a5b5d39f|Garry Tallent]
;; @ [2566ca73-1dfd-49e7-ab20-dfa5697b360e|Max Weinberg]
;; @ [11d2fcfe-669d-4596-8921-e07dbdae311f|Roy Bittan]
;; @ [de53495e-ad5a-4c30-82ab-05e7e3ec7b4d|Steven Van Zandt]
;; @ [a1ef6bc8-2644-4b6d-aa21-27b630acf751|Nils Lofgren]
;; @ [f09aa40c-b613-4ea2-a8cf-6056c2657a9a|Patti Scialfa]
;; @ [065af1a2-2fa9-4864-852e-08c00c9c67d8|Soozie Tyrell]
;; @ [4382fa5d-03de-4ebf-baf3-df6a1d1922f0|Charles Giordano]
;; @ [7b18698b-33fb-4865-b158-e74f3457eeb7|Curtis King]
;; @ [0a84499b-cf33-43cd-b29d-8b2fb67ebbb3|Cindy Mizelle]
;; @ [9b303c28-f0a0-4c76-8293-c97272f6e32c|Clark Gayton]
;; @ [028fd996-fe8f-41b6-a6ed-1cb4b06a23d2|Curt Ramm]
;; @ [092ae7a4-0ea2-407a-a200-d1ee09383537|Barry Danielian]
;; @ [6a3394ba-6888-4dd0-93f3-06c1e35749d8|Eddie Manion]
;; @ [5c64226c-d673-4d23-a612-2bfb704edd66|Jake Clemons]
;; @ [8ddaf563-9a5d-4891-ad3c-b1576f4038f4|Everett Bradley]
;; @ [e67ac344-ce0a-4a27-b4ad-9502dab57a82|Michelle Moore]
;; @ [42a636a0-dbe4-4d0c-abe2-1590fad9531b|Tom Morello]


;;; 2016 The River Tour 2016 (-12)
;; # &
;; @ [d6652e7b-33fe-49ef-8336-4c863b4f996f|The E Street Band]
;; # with
;; @ [42b42dd1-9263-4eae-91cd-4014a5b5d39f|Garry Tallent]
;; @ [2566ca73-1dfd-49e7-ab20-dfa5697b360e|Max Weinberg]
;; @ [11d2fcfe-669d-4596-8921-e07dbdae311f|Roy Bittan]
;; @ [de53495e-ad5a-4c30-82ab-05e7e3ec7b4d|Steven Van Zandt]
;; @ [a1ef6bc8-2644-4b6d-aa21-27b630acf751|Nils Lofgren]
;; @ [f09aa40c-b613-4ea2-a8cf-6056c2657a9a|Patti Scialfa]
;; @ [065af1a2-2fa9-4864-852e-08c00c9c67d8|Soozie Tyrell]
;; @ [4382fa5d-03de-4ebf-baf3-df6a1d1922f0|Charles Giordano]
;; @ [5c64226c-d673-4d23-a612-2bfb704edd66|Jake Clemons]


;;; Vote For Change Tour (-10)
;; # &
;; @ [d6652e7b-33fe-49ef-8336-4c863b4f996f|The E Street Band]
;; # with
;; @ [42b42dd1-9263-4eae-91cd-4014a5b5d39f|Garry Tallent]
;; @ [2566ca73-1dfd-49e7-ab20-dfa5697b360e|Max Weinberg]
;; @ [11d2fcfe-669d-4596-8921-e07dbdae311f|Roy Bittan]
;; @ [de53495e-ad5a-4c30-82ab-05e7e3ec7b4d|Steven Van Zandt]
;; @ [a1ef6bc8-2644-4b6d-aa21-27b630acf751|Nils Lofgren]
;; @ [f09aa40c-b613-4ea2-a8cf-6056c2657a9a|Patti Scialfa]
;; @ [065af1a2-2fa9-4864-852e-08c00c9c67d8|Soozie Tyrell]
;; # and with
;; @ [c3649208-0ebe-449b-b1d7-4bd6c560f109|John Fogerty]


;;; Springsteen On Broadway (-1)

(defun musicbrainz-surround-event-the-rogues ()
  "Surround event with artists and line, The Rogues."
  (interactive)
  (save-excursion
    (insert
"@ [3072c9d3-3787-407c-ae2f-69e7f9846b49|The Rogues]
# with
@ [8abedd53-89d9-4a64-929a-ecddf453ed94|Craig Caprioni]
@ [a069df35-4865-40c9-b31a-c00c4b4f13e8|Jay Gibson]
@ [023cd435-dfa8-4d6b-bb24-b55fc0a99ba7|Jimmy McGuire]
@ [33d2abad-fd92-4374-929c-886c884d2ee5|Donnie Powell]
@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]

# Local Start Time ??:?? / End Time ??:??
# No set details known
# Incomplete setlist
"
)
    (re-search-forward "^$")
    (insert
"
--------------------------------------------------------------------------------

"
)
    (re-search-backward "^@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]")
    (beginning-of-line)
    (recenter-top-bottom)
    ))

(defun musicbrainz-surround-event-the-bruce-springsteen-band ()
  "Surround event with artists and line, The Bruce Springsteen Band."
  (interactive)
  (save-excursion
    (insert
"@ [1607e961-c4a7-4602-ac22-d0d87833eee3|The Bruce Springsteen Band]
# with
@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]
@ [3e2b6ee0-6ec7-4622-b039-1cbed5f55288|Vini Lopez]
@ [4401f986-51b8-407b-a898-500543df9dae|David Sancious]
@ [42b42dd1-9263-4eae-91cd-4014a5b5d39f|Garry Tallent]
@ [de53495e-ad5a-4c30-82ab-05e7e3ec7b4d|Steven Van Zandt]

# No set details known
# Incomplete setlist
"
)
    (re-search-forward "^$")
    (insert
"
--------------------------------------------------------------------------------

"
)
    (re-search-backward "^@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]")
    (beginning-of-line)
    (recenter-top-bottom)
    ))

(defun musicbrainz-surround-event-band-1973 ()
  "Surround event with artists and line, 1973."
  (interactive)
  (save-excursion
    (insert
"@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]
# &
@ [d6652e7b-33fe-49ef-8336-4c863b4f996f|The E Street Band]
# with
@ [7e4bfa5f-a8b8-4fb0-81b5-f74f6ac72133|Clarence Clemons]
@ [94a88a40-8568-403e-86e6-8c01fd4b626a|Danny Federici]
@ [3e2b6ee0-6ec7-4622-b039-1cbed5f55288|Vini Lopez]
@ [42b42dd1-9263-4eae-91cd-4014a5b5d39f|Garry Tallent]

# Scheduled: ??:?? Local Start Time ??:?? / End Time ??:??
# No set details known
# Incomplete setlist
"
)
    (re-search-forward "^$")
    (insert
"
--------------------------------------------------------------------------------

"
)
    (re-search-backward "^@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]")
    (beginning-of-line)
    (recenter-top-bottom)
    ))

(defun musicbrainz-surround-event-band-1975 ()
  "Surround event with artists and line, 1975."
  (interactive)
  (save-excursion
    (insert
"@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]
# &
@ [d6652e7b-33fe-49ef-8336-4c863b4f996f|The E Street Band]
# with
@ [11d2fcfe-669d-4596-8921-e07dbdae311f|Roy Bittan]
@ [7e4bfa5f-a8b8-4fb0-81b5-f74f6ac72133|Clarence Clemons]
@ [94a88a40-8568-403e-86e6-8c01fd4b626a|Danny Federici]
@ [42b42dd1-9263-4eae-91cd-4014a5b5d39f|Garry Tallent]
@ [de53495e-ad5a-4c30-82ab-05e7e3ec7b4d|Steven Van Zandt]
@ [2566ca73-1dfd-49e7-ab20-dfa5697b360e|Max Weinberg]

# Scheduled: ??:?? Local Start Time ??:?? / End Time ??:??
# No set details known
# Incomplete setlist
"
)
    (re-search-forward "^$")
    (insert
"
--------------------------------------------------------------------------------

"
)
    (re-search-backward "^@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]")
    (beginning-of-line)
    (recenter-top-bottom)
    ))

(defun musicbrainz-surround-event-band ()
  "Surround event with artists and line."
  (interactive)
  (save-excursion
    (insert
"= [|YouTube Playlist] ==

@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]
# &
@ [d6652e7b-33fe-49ef-8336-4c863b4f996f|The E Street Band]
# with
@ [11d2fcfe-669d-4596-8921-e07dbdae311f|Roy Bittan]
@ [a1ef6bc8-2644-4b6d-aa21-27b630acf751|Nils Lofgren]
@ [f09aa40c-b613-4ea2-a8cf-6056c2657a9a|Patti Scialfa]
@ [42b42dd1-9263-4eae-91cd-4014a5b5d39f|Garry Tallent]
@ [de53495e-ad5a-4c30-82ab-05e7e3ec7b4d|Steven Van Zandt]
@ [2566ca73-1dfd-49e7-ab20-dfa5697b360e|Max Weinberg]
@ [7e4bfa5f-a8b8-4fb0-81b5-f74f6ac72133|Clarence Clemons]
# and
@ [4382fa5d-03de-4ebf-baf3-df6a1d1922f0|Charles Giordano]
@ [065af1a2-2fa9-4864-852e-08c00c9c67d8|Soozie Tyrell]

# Scheduled: 19:30 Local Start Time ??:?? / End Time ??:??
# No set details known
# Incomplete setlist
"
)
    (re-search-forward "^$")
    (insert
"
--------------------------------------------------------------------------------

"
)
    (re-search-backward "^@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]")
    (beginning-of-line)
    (recenter-top-bottom)
    ))

(defun musicbrainz-surround-event-springsteen-on-broadway ()
  "Surround event with artists and line, Bruce Springsteen on Broadway."
  (interactive)
  (save-excursion
    (insert
"== [|YouTube Playlist] ==

@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]

@ [f09aa40c-b613-4ea2-a8cf-6056c2657a9a|Patti Scialfa] (guest)

# Scheduled: 19:30 Local Start Time ??:?? / End Time ??:??
"
)
    (re-search-forward "^$")
    (insert
"
--------------------------------------------------------------------------------

"
)
    (re-search-backward "^@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]")
    (beginning-of-line)
    (recenter-top-bottom)
    ))

(defun musicbrainz-surround-event-bruce ()
  "Surround event with artists and line."
  (interactive)
  (save-excursion
    (insert
"== [|YouTube Playlist] ==

@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]

# Scheduled: 19:30 Local Start Time ??:?? / End Time ??:??
# Complete lineup of performers is not known
# No set details known
"
)
    (re-search-forward "^$")
    (insert
"
--------------------------------------------------------------------------------

"
)
    (re-search-backward "^@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]")
    (beginning-of-line)
    (recenter-top-bottom)
    ))

(defun musicbrainz-surround-event-bruce-solo-acoustic-tour ()
  "Surround event with artists and line, Solo Acoustic Tour."
  (interactive)
  (save-excursion
    (insert
"== [|YouTube Playlist] ==

@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]
# with
@ [9a214f8b-f606-47d7-803f-f1b96854ae14|Kevin Buell] (offstage)

# Scheduled: 19:30 Local Start Time ??:?? / End Time ??:??
"
)
    (re-search-forward "^$")
    (insert
"
--------------------------------------------------------------------------------

"
)
    (re-search-backward "^@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]")
    (beginning-of-line)
    (recenter-top-bottom)
    ))

;@ [f09aa40c-b613-4ea2-a8cf-6056c2657a9a|Patti Scialfa]

(defun musicbrainz-surround-event-2023-international-tour ()
  "Surround event with artists and line, 2023 International Tour."
  (interactive)
  (save-excursion
    (insert
"== [|YouTube Playlist] ==

@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]
# &
@ [d6652e7b-33fe-49ef-8336-4c863b4f996f|The E Street Band]
# with
@ [11d2fcfe-669d-4596-8921-e07dbdae311f|Roy Bittan]
@ [a1ef6bc8-2644-4b6d-aa21-27b630acf751|Nils Lofgren]
@ [42b42dd1-9263-4eae-91cd-4014a5b5d39f|Garry Tallent]
@ [de53495e-ad5a-4c30-82ab-05e7e3ec7b4d|Steven Van Zandt]
@ [2566ca73-1dfd-49e7-ab20-dfa5697b360e|Max Weinberg]
# and
@ [4382fa5d-03de-4ebf-baf3-df6a1d1922f0|Charles Giordano]
@ [065af1a2-2fa9-4864-852e-08c00c9c67d8|Soozie Tyrell]
# and
@ [61c0a8b8-e3f1-4e7f-9f42-7b21a7bf9e4e|The E Street Horns]
# with
@ [5c64226c-d673-4d23-a612-2bfb704edd66|Jake Clemons]
@ [092ae7a4-0ea2-407a-a200-d1ee09383537|Barry Danielian]
@ [6a3394ba-6888-4dd0-93f3-06c1e35749d8|Ed Manion]
@ [ef011392-7eee-4090-ad83-660c48e9b1c1|Ozzie Melendez]
@ [028fd996-fe8f-41b6-a6ed-1cb4b06a23d2|Curt Ramm]
# and
@ [ad2232c5-2b11-4699-80c2-e5f83c56c8e4|The E Street Choir]
# with
@ [b0ab3979-4165-4e3e-b125-2d77f14080bd|Anthony Almonte]
@ [1ca07311-cbfe-4ae8-a518-aa76c8579802|Ada Dyer]
@ [7b18698b-33fb-4865-b158-e74f3457eeb7|Curtis King]
@ [59d77428-bf32-4c9f-bc4c-7c03ec882c59|Lisa Lowell]
@ [e67ac344-ce0a-4a27-b4ad-9502dab57a82|Michelle Moore]

# Scheduled: 19:30 Local Start Time ??:?? / End Time ??:??
"
)
    (re-search-forward "^$")
    (insert
"
--------------------------------------------------------------------------------

"
)
    (re-search-backward "^@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]")
    (beginning-of-line)
    (recenter-top-bottom)
    ))

(defun musicbrainz-surround-event-1984-born-in-the-usa-tour ()
  "Surround event with artists and line, Born in the U.S.A. Tour."
  (interactive)
  (save-excursion
    (insert
"== [|YouTube Playlist] ==

@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]
# &
@ [d6652e7b-33fe-49ef-8336-4c863b4f996f|The E Street Band]
# with
@ [11d2fcfe-669d-4596-8921-e07dbdae311f|Roy Bittan]
@ [7e4bfa5f-a8b8-4fb0-81b5-f74f6ac72133|Clarence Clemons]
@ [94a88a40-8568-403e-86e6-8c01fd4b626a|Danny Federici]
@ [a1ef6bc8-2644-4b6d-aa21-27b630acf751|Nils Lofgren]
@ [f09aa40c-b613-4ea2-a8cf-6056c2657a9a|Patti Scialfa]
@ [42b42dd1-9263-4eae-91cd-4014a5b5d39f|Garry Tallent]
@ [2566ca73-1dfd-49e7-ab20-dfa5697b360e|Max Weinberg]

# Scheduled: 19:30 Local Start Time ??:?? / End Time ??:??
"
)
    (re-search-forward "^$")
    (insert
"
--------------------------------------------------------------------------------

"
)
    (re-search-backward "^@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]")
    (beginning-of-line)
    (recenter-top-bottom)
    ))

(defun musicbrainz-surround-event-1992/3-world-tour-band ()
  "Surround event with artists and line, 1992-193 World Tour Band."
  (interactive)
  (save-excursion
    (insert
"== [|YouTube Playlist] ==

@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]
# and
@ [7bef92eb-f2b1-4790-935a-6e411eff406e|The 1992-93 World Tour Band]
# with
@ [64a6d00b-f62e-4f2c-bbfe-a9b15e4cee56|Zack Alford]
@ [11d2fcfe-669d-4596-8921-e07dbdae311f|Roy Bittan]
@ [98459b46-1fdc-4f83-ad96-f578ca0057c9|Gia Ciambotti]
@ [2a1d2c80-84d3-4e34-b860-498d66302e65|Carol Dennis]
@ [345bd2f6-29aa-4457-90fe-e736b5eb47d7|Shane Fontayne]
@ [07ddd361-0f2a-4819-970d-c8370deeac11|Cleopatra Kennedy]
@ [dd891d52-e0f1-4671-93d6-7be3ab19f3b9|Bobby King]
@ [7e26152e-9b87-46c5-a3ce-399cc9c4c571|Angel Rogers]
@ [919bc11f-4fdd-41fd-8570-11acbd4cc03f|Tommy Sims]
@ [93dce18b-13ca-4118-9fa1-af92e8e3d8ac|Crystal Taliefero]


# Scheduled: 19:30 Local Start Time ??:?? / End Time ??:??
# No set details known
# Incomplete setlist
# No handwritten or printed setlist available
"
)
    (re-search-forward "^$")
    (insert
"
--------------------------------------------------------------------------------

"
)
    (re-search-backward "^@ [70248960-cb53-4ea4-943a-edb18f7d336f|Bruce Springsteen]")
    (beginning-of-line)
    (recenter-top-bottom)
    ))

(global-set-key (kbd "<f9>") 'musicbrainz-surround-event-2023-international-tour)
;(global-set-key (kbd "<f9>") 'musicbrainz-surround-event-bruce-solo-acoustic-tour)
;(global-set-key (kbd "<f9>") 'musicbrainz-surround-event-1992/3-world-tour-band)
;(global-set-key (kbd "<f9>") 'musicbrainz-surround-event-the-rogues)
;(global-set-key (kbd "<f9>") 'musicbrainz-surround-event-the-bruce-springsteen-band)
;(global-set-key (kbd "<f9>") 'musicbrainz-surround-event-band-1973)
;(global-set-key (kbd "<f9>") 'musicbrainz-surround-event-band-1975)
;(global-set-key (kbd "<f9>") 'musicbrainz-surround-event-1984-born-in-the-usa-tour)
;(global-set-key (kbd "<f9>") 'musicbrainz-surround-event-the-rogues)
;(global-set-key (kbd "<f9>") 'musicbrainz-surround-event-bruce)
;(global-set-key (kbd "<f9>") 'musicbrainz-surround-event-band)
;(global-set-key (kbd "<f9>") 'musicbrainz-surround-event-springsteen-on-broadway)

;;; Bruce Springsteen songs
;; Jeannie Needs a Shooter
;; Jesse James
;; Light of Day / Just Around the Corner to the Light of Day
;; Johnny Bye Bye / Johnny Bye-Bye
;; Sad Eyes

;;; Other artists with same titles
;; (setq mb-brucebase-other-same-titles-works '(
;; ("ef334da9-4b23-3046-a321-cf93a75cd842" . "Fire")
;; ("528c846b-e3fb-4db0-b591-bb507bf95706" . "Leah")
;; ("368c59f5-a20c-4da9-af41-8e0977a86824" . "Paradise")
;; ("e2ebbad2-7eb6-4a5e-828a-6b52eba45cec" . "Party Lights")
;; ("af5094b5-ebf6-4f22-bc23-2d2f82899345" . "Angel Eyes")
;; Frank Sinatra version
;; ("6c0edf2e-3df6-31c2-855d-eb0cd62e184f" . "Angel Eyes")
;; ("c323f194-6477-4746-b4e7-a2476433a267" . "I’m in Love Again") ;; [unknown]
;; ))

;; Sorting:  sort-regexp-fields  '^.*$' ' ".*'

;; Bruce Springsteen works
(defvar mb-bruce-works '(
("2f383d48-d0d5-4c58-bd78-288d3c284836" . "(I Love) Everything About You")
("ce36dc59-1a07-4376-8156-7b4396980692" . "(She’s a) Rocker")
("b799eb91-f917-4f39-ba54-6c56fa15d590" . "100 Miles From Jackson")
("f43d247b-754c-456b-9e9c-4eb66a95ba2d" . "1945")
("fbbe88ca-ed05-3ff7-8eb2-6d1891eafa5f" . "30 Days Out")
("1f573511-eb4b-3106-8fb2-f15de52e4868" . "4th of July, Asbury Park (Sandy)")
("fcd6a523-9e52-3528-8478-5e33652a6180" . "57 Channels (and Nothin’ On)")
("f87d70c4-0312-44c4-885a-b82eaf1b7c6e" . "A Good Man Is Hard to Find (Pittsburgh)")
("463e68dc-d5d6-4541-a676-cc1d3bd7e873" . "A Love So Fine")
("3977799c-d6cf-4e50-bec2-3d7a4b3ea703" . "A Night Like This")
("a4be639d-2ea8-420e-99c3-193e5b3f2029" . "A Night With the Jersey Devil")
("de80b3c1-fac5-4983-b281-3c4a44d1c4b3" . "A Schritt vire (zwa Schritt zruck)")
("617bea8c-7685-4f35-a110-1ac63e77e52e" . "A Story")
("dd968812-9b9a-45c1-81ba-3ee1dac2dd3d" . "A Thousand Tears (William Davis)")
("483abf96-6154-32e2-982f-5f7bab0a39a7" . "Across the Border")
("329a8fe5-b455-4bee-8ce0-6aa155187957" . "Action in the Streets")
("27f570c6-0051-3047-b230-ead9e0ab9792" . "Adam Raised a Cain")
("391ad067-8ae0-4439-98f0-b989f61263c8" . "Addicted to Romance")
("087378a0-cd25-4bc8-8c31-f07a7aec05e8" . "After Dinner")
("0e89d406-ed2c-495f-a656-dc23b3bd4ccb" . "Ain’t Gonna Lose It This Time")
("560fe5b1-22ee-410d-83b1-f64464b3dcea" . "Ain’t Good Enough for You")
("61a7ed25-f3f4-3f98-bcc9-cd21bb7085fe" . "Ain’t Got You")
("8d7078c2-3e3a-4806-a754-27a899c24fff" . "All God’s Children")
("e84f6f21-b9d1-4c38-a3dc-fa9b2a2aa083" . "All I Need")
("56459730-33b3-401f-8c0b-383f04a28c12" . "All I Wanna Know")
("d23888b7-4ec2-419f-9d65-3f19900cf7dd" . "All I Want to Do Is Dance")
("bb49ff17-811f-4da0-a46c-1c216afd406a" . "All I’m Thinkin’ About")
("4b75e209-aca7-3290-bf09-13c60ed78c12" . "All That Heaven Will Allow")
("cb537e9e-3f8f-392f-bf73-d84aa3a3f513" . "All or Nothin’ at All")
("60e7e981-831b-4c92-8745-c8c2911d1e43" . "All the Way Home")
("6775ae1d-3049-4a86-99b0-2a2f3c45e6d2" . "Alone")
("73b10351-b901-4821-bb1f-ef15701d8d78" . "America Under Fire")
("c9a3e16e-0995-43bd-9306-3f0878f18849" . "American Beauty")
("4b9487ed-ed6d-49a2-aa62-98168a1daa32" . "American Dream")
("0ab49f10-0a47-3c11-a919-8d173ed6cd8d" . "American Land")
("4fb6ad0d-cd1c-4735-94c1-2343c5882b7b" . "American Man")
("5e739a38-0c79-4250-9691-e209a3e862a2" . "American Skin (41 Shots)")
("219cff12-fa70-4642-9452-8fc25b7be3ae" . "American")
("e0ee5c1a-1ea2-4baa-a415-673c70b9b659" . "Amplifier Blues")
("4495a580-176f-43c9-a0c7-d2b8d171c9cc" . "Angelina")
("6f0fe26e-77fb-48ec-b53d-501e09bf80ef" . "Angelina")
("731ce2fb-8faa-4a2b-b3a5-1fe162dc22f4" . "Angels Song")
("d78cede0-be3c-4f3c-ab20-282115239bba" . "Angelyne")
("7ae1a229-0510-426c-92cc-2d2337564efd" . "Angel’s Blues")
("497a6a77-d4bf-41a7-928a-07efda32d5ba" . "Another Thin Line")
("08cb2e32-55a4-4acc-a5e9-24fbd74fae79" . "Arabian Nights")
("521a181c-54a6-489f-8d6e-efeaabea83e6" . "Arnie")
("edee001d-572c-352a-8486-dcfe06f8fc90" . "Atlantic City")
("56b6c402-f86d-49eb-ba5f-ac158e0bb419" . "Baby & Me (Blondie)")
("db04104b-d4dd-4beb-a367-3571fb43b8b5" . "Baby Come Back")
("95b6520d-805d-417a-b232-0985d71fd817" . "Baby Doll")
("bcd818c1-4598-4da4-8791-ad1612d058b8" . "Baby Don’t Go")
("0ad3310c-a104-4691-8b74-f941a78914f2" . "Baby I")
("ab7b7c2c-8e6e-4079-a4d2-797c8f8ec7da" . "Baby I’m So Cold")
("6426282c-504f-41d8-bac0-6eaa2ae57980" . "Baby You’re Missing")
("06148968-c9bc-4141-9769-53bac0812769" . "Back in Your Arms")
("9c36e67c-efbc-3c9f-abf6-ac6bed233a24" . "Backstreets")
("4c51e80e-376b-4fe9-8ddd-db1497b1022e" . "Bad Boy")
("e8a5fe99-0aff-3e7b-840c-7ab96cb702be" . "Badlands")
("6ce583b4-ca66-3f52-a7e8-5941f7f49ae0" . "Balboa Park")
("67724436-f105-4407-a610-a8f091eaabff" . "Balboa vs. The Earth Slayer")
("b9c61d60-7fc3-4523-944d-997957345b25" . "Ballad of Elmer the Pea")
("5dd7564d-5eb9-4cba-ba44-77d686448bf2" . "Ballad of a Self‐Loading Pistol")
("990dbf50-9a27-4d92-b460-35957357ef5a" . "Be True")
("2e9dd9af-98a2-3a19-a778-58981f2824e6" . "Because the Night")
("dcd3b130-7222-40a1-a765-c90491d8969b" . "Before the Flood")
("43521f4c-f7ea-47a9-b6eb-4ce08b63a6ad" . "Bells of San Salvador")
("7bbcd0e4-686f-4361-beb2-96e965ad7a3f" . "Beneath the Floodline")
("fd130937-04a3-3f16-ab12-0db75bd56a61" . "Better Days")
("5d455b3a-e2e5-40a4-8a98-c54870df3d7f" . "Betty Jean")
("9d64130d-4050-40db-b1e3-1c82399efd62" . "Between Heaven and Earth")
("a2e18e10-660b-48ae-a6e5-09c3c5685469" . "Billy Boy")
("207fa25f-3bd2-4ac7-93fa-228bdb3a19d7" . "Bishop Danced")
("3af39235-c1a5-42a7-b180-3fcf25b72899" . "Black Cowboys")
("417fc2e0-b9d1-4950-a0d0-f6c848d954b2" . "Black Night in Babylon")
("19deddc6-deb4-4a7b-8a88-4e6f5c2471bd" . "Black Sun Rising")
("fde5f598-74e6-4ed6-afbe-717dbd39a0b1" . "Bless My Soul")
("6e71d158-9ef1-4cb4-8ff4-fb625717b1d8" . "Blind Spot")
("7a757d97-da2a-3751-8d32-94d471de2eeb" . "Blinded by the Light")
("3aaf8965-5bf1-3ee9-b04e-84e7da9e9d29" . "Blood Brothers")
("7d4cff46-5506-4087-8896-f50cb423e9ad" . "Blue Moon")
("ceb9c01a-a31a-3bf8-8289-e81f880f53d4" . "Bobby Jean")
("8f88a632-d1f4-4679-b587-6c1a096720b8" . "Body and Soul")
("d7ed438f-62ee-4a65-84b9-d0c474ca638a" . "Book of Dreams")
("04a99414-32f4-4cb4-8fbc-3d17f688020d" . "Border Guard")
("55d593ce-52cc-30ec-9494-eca1ab879f5c" . "Born in the U.S.A.")
("ad47e52c-19c8-4b37-8c9a-3e3b7613ebd5" . "Born to Be Alone")
("9893a23c-f282-3b07-a2db-b4f2f3b9f4b2" . "Born to Run")
("10262de3-3871-49df-a5fc-835ae673843e" . "Born to Win")
("220dda94-b2c7-4f30-8a5f-26a1ad5b81fa" . "Break My Heart")
("59894c92-db08-4f59-8156-78874385ff55" . "Break Out")
("6a29d640-5c11-47a8-8621-fa31586af36c" . "Breakaway")
("67268cb3-7bcb-32f2-9598-131b4f5ebd93" . "Brilliant Disguise")
("b459add3-ddbf-4e7c-91d9-ed653ca13d4a" . "Bring on the Night")
("abe7f431-5925-4235-9db3-b836c38503c3" . "Bring ’em Home")
("2ba51d23-e042-3279-b9ee-de5b552fafdb" . "Brothers Under the Bridge")
("832420ec-b0cd-4355-b362-a9e2b512f4d3" . "Brothers Under the Bridges")
("52d2446b-1a3a-4b44-9c36-4baeaa0dd898" . "Buffalo Gals")
("dac6feec-184a-46e9-b3f6-f21134cdb36d" . "Burnin’ Train")
("aa416737-7a98-47b5-bac8-b95bae42c318" . "Busted")
("13b1ff44-8389-483f-8d83-fe11778dcf82" . "By Your Side")
("7f44c910-49f8-35f5-bb65-4c6d790ad685" . "Cadillac Ranch")
("64c85628-6129-40ee-adbc-36f2d33864d5" . "California Blues")
("c7a5d6ec-06e5-43c7-9830-bb25e515c3e2" . "Calvin Jones & The 13th Apostle")
("375eae63-64d4-4acc-87eb-aecdb39b5f70" . "Camilla Horn")
("892d0647-2772-4d18-a3de-b9e44dbc3fbc" . "Candy’s Boy")
("5f9f38a1-f3fe-36ab-8a1e-b650f52e5acf" . "Candy’s Room")
("f7f5e119-a335-4edd-b656-4676cde20d7d" . "Car Wash")
("33eababe-f33d-4dce-9f0a-96d6e7cc0938" . "Casper")
("82fce225-4d17-4203-8cf1-5139c68535dd" . "Castaway")
("7b92ee39-a563-46d5-9e28-2c4c4e235727" . "Cautious Man")
("7dfa5538-033e-4f98-909c-1483bf98fe13" . "Chain Lightning")
("8c99c319-bbe3-4e41-896a-b04962081b05" . "Changing Children")
("07e23e4c-0a41-419c-86b7-47930b7d6945" . "Chasin’ Wild Horses")
("c5817f38-c6ce-43e5-bbd6-96b75b958e0f" . "Cheap Motel")
("fa3742a0-ac14-4358-b226-54231e866a8e" . "Cheap Thrills")
("dc18ba52-66c7-48d8-96d0-e405b5527fc5" . "Cherokee Queen")
("d0ca0a15-63bf-47b9-b469-0504455f4522" . "Chevrolet Deluxe")
("a758c068-9738-4a62-a85d-c923b285bda5" . "Chicken Lips and Lizard Hips")
("bf9b8b96-295d-4240-8695-09a8dcf61be9" . "Child Bride")
("2a4890bd-7fd2-4a03-95b9-e5145b09a95c" . "Cindy")
("2614f5f6-7905-41eb-9bf8-46183787e650" . "Circus Song")
("cab8629d-9735-42b6-b36b-14730d9d3157" . "Circus Town")
("f18bff38-a96f-4c4d-8a12-a6eb0af6ccab" . "City of Night")
("81af77e8-0c16-47be-adf8-30d4579dda60" . "Clint Eastwood Theme")
("40748572-b453-48fb-9978-057ccfa1a96b" . "Clouds")
("5c59b5ae-f2b4-4f66-80e0-7476751e6ecd" . "Club Soul City")
("edb5343c-aaf7-4825-aa4c-d783dc7d129d" . "Cobra Jet")
("7547a9b5-8bb6-42a0-8dfb-b7b52aeebd48" . "Code of Silence")
("fec75db0-ae41-4aec-8514-0c8986aad845" . "Cold Spot")
("b8dcd8db-30c0-442d-b855-7694f28c6839" . "Come On (Let’s Go Tonight)")
("3f3f4bae-5945-4883-85f3-f442b4d7ae3f" . "Come On")
("448c41ef-f860-4f93-b647-af378ed218db" . "Come on Billy (Break Out the Wine)")
("d49a2a63-f8bf-45df-8a0f-d5e1f6318523" . "Coming Home")
("645df730-8cf0-4430-87d0-9448a5db152a" . "Common Ground (Stay Hungry)")
("95b2936c-792c-499d-bd01-6a5a2f29b935" . "Countin’ On a Miracle")
("bd26beff-5fd3-4cb0-aea3-4a0fffd5e2e1" . "County Fair")
("13da1f8d-c08d-3d9b-891b-395e363c3a5b" . "Cover Me")
("734c7e30-269b-473a-97b8-0f5a79042345" . "Cowboys of the Sea")
("6126a962-4316-4278-9fbc-f76c9efd0fde" . "Crazy Rocker")
("fc80afcc-b3c3-3df5-9ac0-1c8c43604571" . "Cross My Heart")
("d9c65248-6853-48e1-9803-0bcaa802764b" . "Cross Roads")
("3006b320-5152-3cba-b215-51cab5dec73b" . "Crush on You")
("afca056c-b035-4079-8bfa-7e51c08347d5" . "Crystal")
("5a096400-c759-49d4-9a16-cf58e31733b2" . "Cynthia")
("190703a4-6309-4854-b083-0111e5e864e7" . "Dance Dance Dance")
("721c5512-d4f4-4462-a90b-3f27e337bf4c" . "Dance With Me All Night Long")
("f8556ea5-f89f-3c2f-922f-d5225c1a4511" . "Dancing in the Dark")
("d376c3d2-0c16-4111-ae33-2ac34bd524fc" . "Danger Zone")
("65c224da-809f-4016-98cd-26a4946e095d" . "Daniel in the Lion’s Den")
("aef61f54-4132-4321-9ed2-7c4335e72116" . "Danny Jones")
("da953211-995a-4e54-afd6-15bb3ca07156" . "Dark and Bloody Ground")
("d3cbf5a8-c80a-3273-a405-e756cf11e9ca" . "Darkness on the Edge of Town")
("8ecc135f-ce4e-3fd3-9e9f-7944bd4daa9b" . "Darlington County")
("61159389-687a-47d0-9054-869ef590c4ab" . "Daytona Mission")
("60a363ef-ef03-46ad-88ec-c8b1a8bf044c" . "Dead Man Walkin’")
("faf625d7-e8c3-4e71-9907-b1c0acff4f95" . "Death of a Good Man")
("028e665e-f76c-43d3-9e6d-25108b87dd97" . "Death to My Hometown")
("d0ceffb4-dca2-4b3e-8eca-47006c3b5af2" . "Dedication")
("9890bab3-5232-4f79-b463-22030bddfc52" . "Delivery Man")
("df9a8e73-191e-3d35-b840-71d097dca0a3" . "Deputy")
("06597fed-899d-433d-8cd5-5e98577df209" . "Devils & Dust")
("6f02c716-e6b4-4bd3-881c-ac5db56817a5" . "Devil’s Arcade")
("6986f34a-1e75-4292-b700-7db6e3f8ed33" . "Do It With a Feeling")
("94e2aa9c-3251-42e3-9d7a-805c01dc860e" . "Do You Want Me to Say Alright")
("5c22bc12-7415-3e6e-a0de-4da58125e44c" . "Does This Bus Stop at 82nd Street?")
("d2a61116-7839-41e3-9b4c-2efb34cfc525" . "Dollhouse")
("904c52a3-2cd7-4c3e-867a-340581b0971f" . "Don’t Back Down")
("39a2fe03-1255-4c0e-af95-d4a8506cddb4" . "Don’t Cross That Line")
("37d70bdc-7a30-4352-8230-942564b58b77" . "Don’t Let Your Heart Grow Cold")
("a7fbd960-234e-4ab9-be4f-8f4bd1d2a9b5" . "Don’t Look Back")
("a3d5faba-70b5-4393-bbfc-748b4ee261e2" . "Don’t Say No")
("b6c7597e-b513-46ee-b21c-2671160680cf" . "Down at the Club")
("09e9ccd9-c6bb-4b1f-9083-2fada0728157" . "Down in the Hole")
("285c29dd-0144-4960-9927-e6175b030de1" . "Down to Mexico")
("1c6fb40b-3ab7-4174-80df-a397dd8ea875" . "Down to the Riverside")
("64efb75c-59e3-3540-b05f-4dcd319d7af2" . "Downbound Train")
("3a4272a2-5a50-3a91-80ec-92147b863edb" . "Dream Baby (How Long Must I Dream)")
("89f180f3-1cf4-45cf-984f-77f93e81978e" . "Dream Baby Dream")
("8c9f4c68-f757-4d93-b325-1ebb3e6644a5" . "Dream Baby")
("ad2791e6-3f97-4369-8c68-01c72303a798" . "Dream Goes on Forever")
("94f90b99-5064-3df1-b81c-d2b00f5cc969" . "Drive All Night")
("44c877aa-1293-4141-9df0-e8916507c5ac" . "Drive Fast (The Stuntman)")
("eb5f85a1-2699-4135-881a-9e70b1bde049" . "Drop on Down and Cover Me")
("511de63f-d586-364d-b62a-0919941a41d7" . "Dry Lightning")
("36a3f8e2-c990-4e39-8171-bd6dec340c23" . "Duel")
("b4420454-02f7-40fa-b051-fa9179bd4dfd" . "Easy Money")
("4786bdf4-74b6-4ae5-8d80-0e69652848a3" . "Elijah Ford")
("615ef301-84be-43f2-86ac-7563afc78568" . "Eloise")
("177be7ce-9e92-48ff-80e7-24a9597a777b" . "Elvis Style")
("ab2abbae-9ac9-4167-8219-6cc4d785ff0b" . "Empty Sky")
("f07c2d57-9049-39d2-aa29-89d38564865f" . "Erie Canal")
("da844a9d-e30c-4f49-b3c2-94f85d1759a8" . "Evacuation of the West")
("85ebfc54-b069-4bbe-b544-528622accfad" . "Every Day and Every Night")
("10deb6cf-2112-4f11-bf33-0011b8d992cb" . "Every Little Bit")
("af75fd8f-83f4-465d-bb76-d51428a44a4b" . "Everybody Wants My Baby")
("dc9c3dc2-ea40-4c18-abd6-f43331326b02" . "Everybody’s Looking for Somebody")
("0b19dee2-a085-44f4-85af-3f49c5516d1a" . "Everyone Loves You")
("682b4d15-62f2-302b-9422-4290e21a7fa1" . "Eyes on the Prize")
("5ffdbf39-cd9e-4611-a941-e0256bbae0b1" . "Factory")
("80e1c58b-f9d6-379a-acd7-f2ee20b316c3" . "Fade Away")
("8441d3c2-484c-4e2d-8c22-75d0e60beffd" . "Fade to Black")
("c55f4895-665f-4232-ae2c-757063e46b58" . "Faithless")
("099cb6fa-8dbf-43eb-9387-73b44331f0ed" . "Family Song")
("312a5dba-95b1-4890-8921-94457361eb13" . "Father’s Day")
("43c45f87-24a8-455a-bd7f-236de142046b" . "Find It Where You Can")
("1ff5280d-974c-4473-9ecf-957f4b6a5c61" . "Fire Engines Are Returning Home")
("ba24c6eb-85cc-4c10-94dc-503d88e2b882" . "Fire on the Wing")
("f3f7023a-4486-3341-bac8-b479443da96f" . "Fire")
("c34f5725-7fe7-450e-ba67-131efac7208e" . "Fist Full of Dollars")
("921bbefd-62d6-465c-a2ae-aeb61ada82ee" . "Follow That Dream")
("709cf043-5795-422a-ab32-22612dbce0cd" . "For Never Asking")
("f73ef1a9-33fe-3f8c-8c87-16e4c0469382" . "For You")
("fc88e1d9-f29a-476a-9f1a-68af16fe6f91" . "Forever愛妳")
("f366bd83-7353-4586-b154-1eaa22614b94" . "Forward")
("7c3b1c17-273c-49b2-b4ce-861e9b83ca95" . "Frankie Fell in Love")
("29fd2c5b-149d-4a8f-9f9d-20e25c9a58ce" . "Frankie")
("6db174d2-9645-49d9-b019-fa7b3e2379cd" . "Freak II")
("f4b69fc2-d18a-4a89-8ee3-95903a0a1305" . "Froggie Went a Courtin’")
("6c028112-5e5e-38e7-be1f-70057cff7338" . "From Small Things (Big Things One Day Come)")
("aad1dcee-70bb-4ff6-9906-355c66276349" . "Fugitive’s Dream")
("0bea58aa-c4ae-4905-b7d7-8ca981ccb934" . "Funk Song")
("c454c4dc-9024-3d41-a288-637e958dd45c" . "Further On (up the Road)")
("0af60b56-55d3-374d-87ce-7463abcd7943" . "Galveston Bay")
("ad93a59c-bf70-4a9d-b290-1d0fa5e2a0ff" . "Garden State Parkway Blues")
("c733c4e6-22a2-499d-ace7-17b832356aff" . "Gary Clark Jr.")
("37d9330e-ec71-4c12-a12d-52d20384ddcd" . "Gave It a Name")
("834c62a1-d89d-4dcc-8caf-f313dc2664ca" . "Georgia")
("131f270d-67d8-4f92-8ad8-18c3147cf1ae" . "Ghosts")
("b129bcb5-dbb7-48b6-be70-74c0800505c3" . "Girls in Their Summer Clothes")
("bedbedef-23c7-4c49-a13b-fa8a6b7b1e41" . "Give the Girl a Kiss")
("15c983fe-7402-31b8-9108-30aa8c076162" . "Gloria’s Eyes")
("e59ca323-dd32-3a0b-80a9-d42746362213" . "Glory Days")
("e55168ee-76d1-4bbb-83f1-d8ff5804999c" . "Glory Road")
("b9462ef7-6b06-46f6-b6dd-9e6f5f24920c" . "Glory of Love")
("4a6a0ec1-aa2e-48ef-8164-bdf1382762fd" . "Go Away (Come Close)")
("21ee4584-6a61-482f-b2d8-5d20594b21e8" . "God Sent You")
("c16754c6-bce8-435d-80dc-b419a33d9481" . "Goin’ Back to Georgia")
("0f0f18ac-7d95-4ecd-89fc-a9f2f6efba9e" . "Goin’ Cali")
("de2292b2-4471-4908-bd4a-1c5537247618" . "Goin’ Down Slow")
("d976a2d7-9467-424f-a29e-6c1ac3fa27f1" . "Goin’ to California")
("5ad493c5-707f-45ee-8b03-a2e53a908c16" . "Gone, Gone, Gone")
("8b8bea3f-f02a-4597-af0a-eb41e854dc43" . "Good Eye")
("a1cf119b-0324-4ce5-99e3-b2b258b24a75" . "Good Lovin’ Woman")
("20246d97-9201-4f22-b09c-c7c3d2f210d4" . "Gotta Get That Feeling")
("57ae4f22-e0d3-4dd3-859c-fdfc44138f9b" . "Governor Christie Traffic Jam")
("b1528b3f-0c0e-3977-ba98-5cb5b38108db" . "Growin’ Up")
("6a75b0bf-cc90-49b4-aecb-e9cdcc577c0c" . "Gun in Every Home")
("3df573ff-0aca-466d-8ff6-72b3ad869aaa" . "Gypsy Biker")
("3695fb0c-558f-4c3a-8795-97672c46f83f" . "Happy")
("aacb33b9-583a-4b0d-bb98-bb39e526f113" . "Harry’s Place")
("3b15192d-31b8-3f96-8330-9be058838471" . "Hearts of Stone")
("3c752069-18c1-4e5b-9b99-65067807b970" . "Heaven’s Wall")
("cd72f8c4-c885-4fe8-945c-9b286d142015" . "Held Up Without a Gun")
("7623358f-7052-4508-8258-0990de6cd63c" . "Helen Blue")
("193a3729-6546-40d5-b488-5110a1c21717" . "Hello Sunshine")
("9c2f7828-5219-46ef-a598-a1d98f5f89d3" . "Henry Boy")
("c19036a4-5f3f-42ea-9646-54a31401c797" . "Here She Comes Walkin’")
("838be5c4-ce34-40ac-9f99-e78e35a7dede" . "Here She Comes")
("d0486786-8856-48ec-aaf8-092b965d89de" . "Hey Blue Eyes")
("e306daf9-304d-423a-b11f-760ed454ad8e" . "He’s Guilty (The Judge Song)")
("5368b522-ff2d-4acf-9437-b858e8f370d7" . "Hidin’ on the River")
("078979af-8fac-4d87-a56b-49b8f706394c" . "High Noon")
("c2146e4a-cc11-32fe-81ac-310073f02ad2" . "Highway 29")
("df9a8e73-191e-3d35-b840-71d097dca0a3" . "Highway Patrolman")
("9cbc1561-8a2e-4aae-be7a-d7847f92d386" . "Hitch Hikin’")
("99a4e012-5ce5-4bd5-9989-e1168bdd245f" . "Hold on (to What You Got)")
("a5830d14-4327-4702-a2ec-f23b754af15a" . "Hollywood Kids")
("67d3c66a-e5ed-4705-b4fa-545bf8b65ae8" . "Homestead")
("54be9f02-1cc7-4e69-ae5d-fdb1f23e6a1e" . "House of a Thousand Guitars")
("2b973ac7-74ff-4faa-b1f5-61f184952da7" . "House on Fire")
("0d15138e-bcc8-41e0-9d80-7899ee183e7b" . "How Can I Keep From Singing?")
("9cb737bc-1ad3-3324-b0d5-62512fb6bc7a" . "How Can a Poor Man Stand Such Times and Live?")
("00ebc03b-2708-39bc-8e9b-38dcf6315be2" . "Human Touch")
("db8f3060-b101-3877-9805-ce054f923302" . "Hungry Heart")
("545c634b-ce5a-4ca5-afeb-dff7d6d9db59" . "Hunter of Invisible Game")
("0dcb04ca-8775-4d7e-a6b7-6bc025d51d73" . "Hurry Up Sundown")
("a1f526b0-eb3d-4f21-99e4-84969a43a7dd" . "I Am the Doctor")
("87a754b2-9e18-4c38-a5b2-a4ad75047fbd" . "I Can’t Take It No More")
("31834b84-cda4-40a9-952f-6481456e30a1" . "I Don’t Care")
("54b13438-4c4a-4860-ba68-4860ee8fd453" . "I Don’t Know")
("7d8f16b8-2c85-443f-897d-725034f2aaf7" . "I Don’t Wanna Be")
("71c21500-f79a-4622-a580-cb8de10b3e62" . "I Dreamt My Love Was Lost")
("517f8a64-8a69-4253-b967-0bf7761e8dca" . "I Get Mad")
("addde257-c1f8-43af-8f21-3e311f126e40" . "I Got My Eye on You")
("218137ac-c9ae-4e5c-8ca2-b536a2f76858" . "I Gotta Be Free")
("c6d1a7f1-fa10-413c-b16e-f57b2c3fe455" . "I Hear a Train")
("47ddb1c3-ac20-40ea-be9e-5e66c28a90dc" . "I Just Can’t Change")
("d0c53b2b-53c5-4822-b51e-3a0f1e6c8086" . "I Just Can’t Think")
("47349963-34ee-4f8c-98cb-0c27b19c7385" . "I Met Her at a Tourist Trap in Tiguara")
("e8e2bfe6-9b1a-45fe-9875-598a0237f35f" . "I Need You")
("35bc7081-3cde-4cb3-a6bd-a89a5d225041" . "I Remember")
("18a8b33d-4787-420e-99bf-a317b1526bb7" . "I Wanna Be Wild")
("7d1011c9-d664-3902-bb20-28192e206edb" . "I Wanna Be With You")
("d771b3f7-38d2-3a7d-98b0-ee76403fceea" . "I Wanna Marry You")
("2a3534c2-592b-4989-a3a5-cd3936678e0d" . "I Wanna Start a New Life")
("a5147142-114e-4c62-8f3f-157b994a50cb" . "I Will Be the One")
("316a389e-6ef8-3134-8f7d-885b7b4af515" . "I Wish I Were Blind")
("517120c9-4081-4e8a-b900-878b12c8616d" . "Iceman")
("ef1df38f-9367-4d64-8022-f8425f6fa7b9" . "Ida Rose (No One Knows)")
("ea292cc9-fa47-4c6b-8f83-bd13f93b6b61" . "Idiot’s Delight")
("076e9694-b44e-4488-8b53-500628d69e6e" . "If I Had a Hammer")
("194391fb-0e28-3a81-bae1-9483d10f67e6" . "If I Should Fall Behind")
("a25a665d-6d5f-4b34-ab62-c1e7112f26f1" . "If I Was the Priest")
("2b1ef3ab-8e94-4537-ad27-d37685a7aa3a" . "In Freehold")
("1c0d424e-1006-4562-a74a-12536e2e1fdc" . "In Michigan")
("fc9d1551-f16f-4d29-830f-6fe0443935e9" . "Incident on 57th Street")
("150addd6-4092-4d13-b420-0f96135fbc3e" . "Incident on the W. Side")
("588f5e54-97ad-3450-9849-dab3438483cb" . "Independence Day")
("16b0f048-0bb3-4db8-a529-cd12ffd81f4e" . "Into the Fire")
("535bf021-39c4-41ab-ba69-6129cb874218" . "Invitation to Your Party")
("f45920b5-1488-49a1-ae0d-3d6c6849f51e" . "Is It You")
("267105cb-9dc7-4791-9e2a-317b9eb059e9" . "It’s Easy")
("5a564a66-106f-38b6-823a-101396c9ad09" . "It’s Hard to Be a Saint in the City")
("f3e6112f-069d-4398-8504-8fd39f55ffe0" . "It’s Just You")
("e304b011-8a3d-47d1-bbc8-dadfcf9831cd" . "It’s Time to Go Home")
("35f06488-e235-4730-96ed-323253628368" . "It’s a Shame")
("6a8bc935-7d7e-4220-962d-e0f8adbce03e" . "It’s the Little Things That Count")
("a090f54e-9f5b-4ba6-a46f-15eb0e1f0ffc" . "I’ll See You in My Dreams")
("b0dd8e8e-7c47-4301-a0e5-2afb36a8a2a8" . "I’ll Stand by You Always")
("9726937c-33f1-467c-bec5-93e3774e9212" . "I’ll Work for Your Love")
("eb6c37d4-86b4-4517-a526-4e8f44807f87" . "I’m Going Back")
("a62078d1-6f5e-4034-9231-41e773321270" . "I’m Goin’ Back")
("0d640b42-dd07-30a4-8d7d-d6f18202fbfa" . "I’m Goin’ Down")
("bd5de48e-be11-4a36-977f-555cd38455f1" . "I’m Gonna Be There Tonight")
("b3f81f4f-072c-4cbb-bc8d-1e9dbcca5ed1" . "I’m Gonna Rock (All Night Long)")
("b42e5873-5acf-4436-b825-e9076dbcce77" . "I’m Gonna Treat You Right (Wild Kisses)")
("b39b9a9d-e0f3-4176-918d-c1cee27c613b" . "I’m Not Sleeping")
("89e150a3-f830-4528-b538-0efbf6c6f822" . "I’m Turning Into Elvis")
("6ce106a8-6fcd-4e2b-b1b4-a6aaff17883f" . "I’m a Coward")
("62fa69a9-354c-4b15-9021-e4d164387aec" . "I’m a Ghost in My Dream")
("11aa91e7-1121-33dc-a511-cb01a54c3be6" . "I’m a Rocker")
("690a4e61-12d5-41d7-9cf3-19606d3a42df" . "I’m in Love Again")
("464c648e-223a-3180-b207-9f42f57288bb" . "I’m on Fire")
("a6c092f1-c62c-4729-b0cc-dc2a3f23f51b" . "I’m the One")
("f4c81776-18d1-4722-8e1c-aab2be63ca47" . "JJ Johnson")
("b852d9d6-2d4f-46b1-a92e-7e09251e1492" . "Jack of All Trades")
("16d30577-b539-3c38-b440-06a210d02325" . "Jackson Cage")
("e30dc83a-74a7-3ed8-8826-379cab06ea2b" . "Jacob’s Ladder")
("4753749f-5642-40ac-aa20-639c35811cd1" . "Jambalaya (Roll Over)")
("86ec3c00-b5c6-485f-8498-000a8c0d622a" . "James Lincoln Dear")
("bd3de1f0-977d-4789-ac77-a20319a17d75" . "Janey Needs a Shooter")
("be39fc88-2f2d-33f7-88de-7983f495f378" . "Janey, Don’t You Lose Heart")
("a82eed1b-dcc1-41ce-8ef2-153609d582ae" . "Jann’s Song")
("831838be-edd3-4f69-9f16-0458e23872d2" . "Jazz Musician")
("34f7ef32-91d5-4240-9f06-6085dfe35869" . "Jeannie I Want to Thank You")
("9b036346-47c2-4434-953c-169c830d8747" . "Jeannie Needs a Shooter")
("d2323c9b-2b16-416a-b3fc-7e89261027d0" . "Jeannie Needs a Shooter")
("7c4c7cf8-2001-4f2d-91b6-0f70a3e6d308" . "Jennifer")
("542872f3-155b-4e6b-b064-81fb5b6bb13b" . "Jersey Bikers")
("1006a8c0-0197-4543-8644-a64cf5f1e48b" . "Jesse James")
("9cb194bf-ee65-4403-8d98-63d028517132" . "Jesse James")
("08c7a345-7ade-471f-aa67-78285ed64e19" . "Jesse")
("7fedf591-419c-4fc7-8d40-c493ead85b8c" . "Jesus Mary and Joseph...")
("add94de9-7c0d-42a8-93f5-b9b2b1c45670" . "Jesus Was an Only Son")
("8d03bb1c-5ae8-47e5-b06d-6a4a64921dbc" . "John Henry")
("f80534bf-958d-34c0-83cb-d7edbe887cdf" . "Johnny 99")
("d49f9a8f-11bb-4471-90dc-6516a47b7ffd" . "Johnny Bye Bye")
("e2c39ab9-be7b-4248-af0f-383883d2714d" . "Johnny Bye‐Bye")
("1a6227c1-c287-4ea5-9d22-3b71eba56bac" . "Johnny Go Down")
("6f2477ad-8ebb-4ca8-a3e0-e0b17ba5fdce" . "Jon Deas")
("6abc27cd-8172-425e-99f4-710e4c9286de" . "Jr. Walker Groove")
("6ee4ff85-be1a-4f50-b6ec-e8caa1e9fd5c" . "Jungleland")
("01cbbe51-0b54-4f77-a9e9-294889e185e7" . "Just Around the Corner to the Light of Day")
("9a2ec19a-0999-4843-a82d-3e20caffca3d" . "Justin Fears")
("89d5967c-af45-4955-b3f4-d9268a539e3f" . "KT‐88")
("60066353-3264-4163-86dd-5c6666576b68" . "Killers Paradise (The Violent Ones)")
("9537f0b5-19f4-4a41-980a-0f9de1aac56b" . "King Zapata")
("b0b8e03d-ff63-402a-8b4c-5bb25c5be9ec" . "Kingdom of Days")
("6edff037-8c34-4112-a51e-330d73aa1446" . "King’s Highway")
("fdb63bfc-2429-474a-816d-281d1d33ceb3" . "King’s Rock (King’s Big Chance)")
("d58d3dce-36cd-4962-aaed-d78037b50b91" . "Kitty’s Back")
("ee682c29-c368-4106-a2d0-b54fde6378e6" . "Knife in the Back")
("1460d409-c3dd-4102-b9a8-1da4d24659ed" . "Lady Walking Down by the River")
("bc1dcaa0-999e-405b-a93c-58d09a6d06dc" . "Lady and the Doctor")
("33393e1c-d001-4bf3-b6d8-40fda21e37dd" . "Lady of Boston")
("9878138e-7ca7-4bb3-be1f-3dfeed51d4f5" . "Land of Hope and Dreams")
("c436ac05-514a-49d5-a86b-42341260706e" . "Last Man Standing")
("121d0ffe-7021-484b-9c5d-c8195b08ddaa" . "Last Night in Texas")
("7677de83-354e-4f5e-a048-441555942886" . "Last Night in Tulsa")
("abbbe945-0655-42c0-ab0d-e10e177e32e5" . "Last to Die")
("60e97859-8b06-4c6f-a2e4-5642ab443fce" . "Latin Song")
("87caeba8-3f65-4991-b1df-e493da551aa4" . "Leah")
("3dbb3809-ad48-41b4-a9ca-77f10c162c8d" . "Lean on Me")
("54c9620c-d08a-38d4-9a11-55bb71d67f92" . "Leap of Faith")
("4a753e20-4b38-4161-a0d7-95e2fd36086f" . "Leavin’ Train")
("f7d8ded8-dbdf-4974-9b28-27f494821662" . "Les Rues de Philadelphie")
("ac780fcd-cdef-4a7e-83c5-443f55072dc1" . "Let the Words")
("effe5aa8-25b9-4353-bcea-334e09edc07c" . "Letter to You")
("93451c04-1d92-404d-9f8d-4c9d87b6449d" . "Let’s Be Friends (Skin to Skin)")
("ada5c17a-b497-4fd8-93c0-1b693ce87182" . "Let’s Go")
("66357ca9-8bce-43ec-ac15-b22d4d859c0b" . "Life Itself")
("2e16b44b-e316-4085-860b-639eff5a9317" . "Lift Me Up")
("d662d712-f9f8-4118-8bb2-a821395dbf96" . "Light of Day")
("2956f304-f191-40aa-86b5-8ac8cbca43d5" . "Like a Stranger")
("d1770258-ed8e-40ba-b2dd-613ce8c211f9" . "Linda Let Me Be the One")
("fa7a0529-864a-3720-b7d8-80c3b28a9138" . "Lion’s Den")
("b1a202cc-2473-47da-a5e6-b9624ca4133a" . "Little Girl (Like You)")
("7d5f7e14-99e4-457e-81f0-1850af093d15" . "Little Girl So Fine")
("7524ea66-f723-4fe9-b926-59efcf7fec27" . "Little White Lies")
("45a8b488-134f-401b-b88f-5c1a3d8546b0" . "Living Proof")
("35ebf07c-692b-4616-b81f-bd836a78c63e" . "Living Rock and Roll")
("04f069a2-93e0-4d68-95ba-0eb1dac916f0" . "Living on the Edge of the World")
("b83ac8b8-afae-42ea-a2ec-cc3608f67e22" . "Livin’ in the Future")
("e370d3bc-020e-4a6b-af9e-3e283ba472a6" . "Livin’ in the Ghetto (Harlem)")
("36522fa3-488d-4de5-befa-c4fe00ad4b73" . "Local Hero")
("151b7a46-8059-4897-bd47-a276fa5250ff" . "Lonely Night in the Park")
("4214d96a-190b-4a22-bb2b-436cac2a3fd1" . "Lonely Street")
("9ad2a9fa-917d-4a70-a21f-11c487884795" . "Lonesome Day")
("423015a9-f252-4e58-969a-5ec95ca2d82a" . "Lonesome Town (Blondie)")
("41c38b36-3e72-37bb-bb05-487f171b0d82" . "Long Time Comin’")
("d5d5d83e-2841-4210-9be1-252341babc7e" . "Long Walk Home")
("34fad647-3134-452c-8f92-39a7c79296bf" . "Long Way Home")
("d2859a73-6975-44de-9354-7d06b99c55f4" . "Look Into My Window")
("7b97e0c3-87c9-49f5-ad56-cac7dcb2ca1a" . "Look Towards the Land")
("3c0b6b50-f4b5-41a3-bab9-861cd5b2487b" . "Loose Change")
("47bc65db-85b7-3ffb-8011-b789cb53cb50" . "Loose Ends")
("48cf6851-81ec-45b3-b9cf-0119d0136700" . "Lorraine")
("a0346c5a-79ea-4627-a9fd-61f42ee51e6e" . "Losin’ Kind")
("545da9e7-c563-31fd-8342-6f40060f0af2" . "Lost in the Flood")
("7827bd62-7343-4e43-a126-da9f70a35a78" . "Love Cycle")
("0e83263a-141a-3e13-ab75-23ee970d064f" . "Love Is All Around")
("a42bd244-9c31-4e78-a179-ce315a723315" . "Love Is a Crazy Thing")
("ca8d0cf0-a59a-4428-85e9-1066e0528114" . "Love Is a Dangerous Thing")
("4429f139-6149-42f2-b87f-0b305c00f3de" . "Love Is a Gun")
("29ed936f-8c8d-4d42-b79b-a8369bb093d7" . "Love Will Get You Down")
("8166c421-fd6f-409d-9572-ec0670c1d526" . "Love and Defiance")
("618d810e-ba76-4ca0-9173-c39f53858d0b" . "Love and Kisses")
("45785852-d36c-49d2-8703-ccb7f402351f" . "Love on the Wrong Side of Town")
("6eee5a70-6609-4c56-9a61-5928a4b8a2c4" . "Love’s Gonna Be Tonight")
("946aac00-0976-4ad1-aef4-bf730c82e70f" . "Love’s on the Line")
("4d84cbef-4739-450a-b199-cc01d481bc01" . "Lucky Man")
("032fee5f-ce38-36db-965a-ca4b4718ec66" . "Lucky Town")
("d31eb9dc-f264-493d-87a8-77e6f34916ba" . "Magic Kind of Loving")
("730b328f-964c-464e-ad46-3a98b239b42b" . "Magic")
("e3d460b9-4058-4322-9db3-660c3f2d1eca" . "Make Up Your Mind")
("f8ccd6da-3832-4827-9bc7-5d368c53f70a" . "Man at the Top")
("b1684ddf-3673-348f-847a-7a538230d5f5" . "Mansion on the Hill")
("6bd0c0d8-f7c6-30f0-b701-242def55f9f9" . "Man’s Job")
("dadd17cd-e555-40fa-916b-d33a88863480" . "Maria")
("a68d2b33-e745-4d6c-a318-d35e32bb540e" . "Maria’s Bed")
("1dc3a4dd-7adc-436a-bb61-b19aea015163" . "Marie")
("90704591-00d8-4478-88de-fd1d10667de7" . "Mary Lou")
("c4efa86e-af84-41d3-ab12-a06998dff3c9" . "Mary Louise Watson")
("d3ab59ea-df73-4f84-9f30-819999a840ed" . "Mary Mary")
("d0241567-4101-3d1c-b4a8-d48784285fb6" . "Mary Queen of Arkansas")
("77ed53b9-dfd0-4532-bf74-b8692369ed67" . "Mary’s Place")
("960718d9-b9ba-4f89-a63c-5e45e2c526e8" . "Mary’s Song")
("a2055b78-31c7-4db5-89c5-9a7a7c30002b" . "Matamoros Banks")
("f47e8109-78a3-4631-a9e8-aa6cb652e47c" . "Meet Me in the City")
("f7d49c0f-9182-417e-93b3-e9bf1de398cf" . "Meeting Across the River")
("ea9f5a8a-8d34-4da8-b5a6-dd217169b4ba" . "Michael, Row Your Boat Ashore")
("bc22ea16-2169-4535-9fbc-0d95667d28d0" . "Missie")
("455920de-a92b-4123-94b5-7eb74a5a6872" . "Missing")
("5789ea04-9d9a-4f0b-91d1-0ebd906ed5c5" . "Mistress Annie")
("fabd6d42-3a66-47c1-845e-2993fc71b014" . "Moon Beams")
("687034b9-ab9c-4814-bc96-1325e13401e8" . "Moonlight Motel")
("1c1ab6dd-1829-459e-af59-de4b3a5c686f" . "Mother")
("d3a29d18-de2e-4909-9af0-2b5d572bfd29" . "Mr. Jones")
("ef1cd12a-5714-44e0-a471-cc153df2bbaa" . "Mr. Outside")
("b65746ee-179e-4750-b45d-91e0467bd1c0" . "Mrs. McGrath")
("9e21f814-f5a2-350d-91ce-a23ce921f5d5" . "Murder Incorporated")
("1bae911c-c77f-4432-88cc-81e9eadac88f" . "My Beautiful Reward")
("dda1ae8e-04f3-32ad-88fc-0d0a24305d95" . "My Best Was Never Good Enough")
("abc923d4-6aa1-3bf2-9cbc-2307882e4bb3" . "My City of Ruins")
("e2ed6926-6b80-354c-932c-d4cf9f154bec" . "My Father’s House")
("503b0ca0-10b6-487e-9d4e-6692cab751f1" . "My Heart Is an Open Book")
("4b3b9bf5-380b-3c0f-8a7e-4f654bf84290" . "My Hometown")
("6144e584-4be0-4167-9a23-7c2532f25a4d" . "My Lady")
("6eba2aeb-9725-3a47-b774-824e6488e98e" . "My Love Will Not Let You Down")
("f57070d3-d136-4eb1-b357-6046fdf7d685" . "My Lover Man")
("98d2b0ee-4d89-428c-af7d-9b6d9dd551c9" . "My Lucky Day")
("258a4dd6-60f7-4181-86ab-805f7004fab0" . "Natural Magic")
("88bc0066-6896-36e2-9f93-bbac8aa4f839" . "Nebraska")
("21207b86-d130-4520-9930-562e71e2f40a" . "New Delinquent Lovers")
("38e984ee-4099-4ff7-815e-7e117a887508" . "New York City Serenade")
("1889691d-ca18-4fba-baab-3bfd4bec7601" . "New York Morning Love")
("f4653ae0-6284-4b4b-9d9f-ee539a908809" . "New York Song")
("a381cc39-13e7-4ea8-8283-8fa5efc693a6" . "Night Fighter")
("989ed37a-911c-45a4-bbe3-caf23f43f928" . "Night Fire")
("2afb7984-f85e-4720-82c8-9f68eb322271" . "Night Patrol")
("23669454-daab-4b99-bd87-d051b61d2d98" . "Night")
("d1952ce0-8c04-4d6b-aa5a-615ff67b7c9a" . "Nite")
("91895c47-f02e-4235-b987-d4a92d48cdc9" . "No Need")
("5282969b-4c49-3013-aa05-8af5c4be6ae9" . "No Surrender")
("03e86ff6-c0a2-424b-a58e-7e788294407a" . "No Way")
("17924dec-6e0a-45a9-88ec-57d9743828fd" . "None but the Brave")
("23454190-6f5d-4ff2-a028-9a5a0d1d9071" . "Nothing Man")
("85c80baa-53ad-4fb2-8965-c6bcf34bb892" . "Now and Forever")
("e6f8725d-713f-389d-961b-edfd7c2b224b" . "O Mary Don’t You Weep")
("4a7ec0ea-2a8e-4806-b1b2-022b2cff3682" . "Oh Angelyne")
("4405126f-bdad-41b3-9854-6629f3f37780" . "Oh Mama")
("235c0eaf-6847-3b51-9de4-16c4586b7ce5" . "Old Dan Tucker")
("29d98699-6ada-447e-bd9d-8cf3256f6bc8" . "On a Day of the Cowboys")
("8972043c-562d-4e3d-a9ea-44916b221287" . "On the Prowl")
("cb5ad4b4-8403-4268-a417-e0b1c099712a" . "On the Road") ;; probably
("bc3fcc70-4bc4-441e-9f59-6d58381952a2" . "One False Move Is All It Takes")
("e95f4e55-d306-461b-92a5-d2295328d780" . "One Love")
("006eee9a-5bc0-4c92-a394-60e561c871b5" . "One Minute You’re Here")
("3fa2cbe5-4b10-4931-b035-534ac4b51d72" . "One Step Up")
("99a04f72-8517-421f-ae80-4cc9644ab253" . "One Way Street")
("4a938f40-95cf-3d4a-a21a-f5732689238b" . "Open All Night")
("3fdcd261-211d-436a-a025-af1e41c6f782" . "Orleans")
("a4c2fa7b-4e0f-4d27-9577-b6f7b6bf48ae" . "Our Love Will Last Forever")
("35fdc4a9-fd0a-3a68-a539-5b377464a3ac" . "Out in the Street")
("8493b8f6-0e56-4eca-9086-5b1aa779f87e" . "Out of Work")
("fda8e8f0-fdcb-466b-8e99-2bddf6a06673" . "Out on the Run (Looking for Love)")
("0811ca3d-aa04-448c-9a44-b3123d8c8de5" . "Outlaw Pete")
("c7b384fd-283c-4ef9-aac1-0d111e412590" . "Outside Looking In")
("a66f18ad-0764-4ac4-b730-e3efc45f804f" . "Over the Rise")
("d0b0ff69-624d-465e-b5cc-74c89f090124" . "Paradise 1953")
("8a04abf6-69b6-3de3-9a01-4b289d0d6446" . "Paradise by the “C”")
("f277b134-d3cf-4be1-9bff-4a8ac8f0234f" . "Paradise")
("c999ecd6-fdef-3f03-b79c-58a41492566f" . "Part Man, Part Monkey")
("2283c5d2-15cb-4f4f-9d1e-c48b7d8d1b97" . "Party Lights")
("122ceb3a-3990-3caf-b873-74ccf7e89daa" . "Pay Me My Money Down")
("71d2251b-7bfc-4d63-8d09-e9210964ddb2" . "Perfect World")
("c52d60d9-fca8-465e-ac11-51ce55164baf" . "Phantoms")
("8506885d-3efa-45db-bc9f-c2d9330bb635" . "Pilgrim in the Temple of Love")
("b656ff9a-9b07-347e-b318-7bfbc5ac008e" . "Pink Cadillac")
("2e672a08-aa64-3ab1-9bcf-ab93b19ab5e4" . "Point Blank")
("3aba3a0f-9dc6-33ee-a152-ef8070736d7c" . "Pony Boy")
("6f3699fb-5169-44f5-a17f-e906f5998b40" . "Powerglide")
("e9f5f682-706b-4222-9416-eb0f17148401" . "Preacher’s Daughter")
("43594259-9ce1-429c-88c6-fcbb678bb45d" . "Precious Memories")
("2228d90f-0888-4ccb-bdcc-06c4bfc6b150" . "Pretty Baby, Will You Be Mine")
("1d5538aa-f102-405e-909c-70bef14b8251" . "Pretty Boy Floyd")
("0e0b0170-5fb1-4e2a-8877-9007f8ddb2e4" . "Pretty Thing")
("a0690034-031b-4a8a-ae47-4f9dafa90e2d" . "Prisoner of Wars")
("219babf8-ac6c-4a19-9977-0d4604c73f45" . "Prodigal Son")
("85a5c8b0-e574-47b5-82a6-0215c30e0a5e" . "Protection")
("872604b3-15d2-44fd-8631-0903b0ddb858" . "Prove It All Night")
("6e713130-2b6b-4f62-96e3-8e95ba1571f7" . "Queen of the Supermarket")
("6ef042c9-3a59-383a-9f06-94b14918cf80" . "Racing in the Street (’78)")
("6ef042c9-3a59-383a-9f06-94b14918cf80" . "Racing in the Street")
("2e20cccd-1729-405d-a496-16e76430b76a" . "Radio Nowhere")
("79772b1c-171d-47e5-9afd-b54a9cbd878f" . "Rainmaker")
("a4aaf1ec-92c9-4b95-b9a0-6e4a2636e7e4" . "Ramcharger")
("9915e1a9-13ff-3ea0-823d-56fe45f7535d" . "Ramrod")
("e3a4441c-6726-4f3f-81f2-a8218762e185" . "Randolph Street")
("aa7d91b1-db3b-38b1-8023-122694e0862d" . "Real Man")
("37d564cd-d0f2-335b-a0fd-7284863e881a" . "Real World")
("efa14865-61d7-33b6-b597-fdef24f39943" . "Reason to Believe")
("cce38f48-6d14-4f2f-80fd-a09d0cbd19c7" . "Red Headed Woman")
("8b4e284e-7ea5-4ddd-8b45-cb523c24d5f4" . "Red River Rock")
("a90d2da7-b817-47ab-bd48-a8cf6933b106" . "Refrigerator Blues")
("8e437cd5-9fe1-3258-a6a7-0f84386ab196" . "Rendezvous")
("6cc66f51-c515-463f-83ac-4c5572e35fbb" . "Reno")
("dd03a34d-969a-4d11-a179-97be82299ea5" . "Restless Nights")
("1dc283eb-9632-4c27-9867-e34b8be7ec81" . "Resurrection")
("46525bda-4c8c-404b-b0b5-a571b9dd0021" . "Richfield Whistle")
("0cebc8ac-5f2f-45de-b5d2-1092a30ec3ac" . "Ricky Wants a Man of Her Own")
("a671a8de-6f35-4b71-b729-dff4538ea724" . "Ride on Sweet William")
("59481cff-fe3a-453b-8354-e6e9e4437bab" . "Riders on the Rain")
("f69196d2-4758-4a54-808b-79112fef7c30" . "Riding Horse")
("7ef1df4b-ddfb-477d-a410-3b163b782ecd" . "Robert Ford and Jesse James")
("b81eb4fe-b634-400e-abaa-412972567968" . "Robert Ford")
("a9442deb-1b59-48b3-8cae-5b1b46f6a45a" . "Rockaway the Days")
("9ca20f90-cb25-4b3e-bc99-4963d454bd7e" . "Rocky Ground")
("880b28ae-5679-4fdb-b6f9-18b6f0120475" . "Roll Away the Stone")
("c689949e-3df6-3bf0-867d-74a83329d274" . "Roll of the Dice")
("66969327-8735-37eb-aaa1-a4b638d446b0" . "Rosalita (Come Out Tonight)")
("639c23e4-7a4a-49a3-af63-fa78ff786634" . "Roulette")
("3b9edf9d-c10c-4d64-bb6f-919f9db5fc56" . "Ruled by the Gun")
("1d6a209e-1101-451a-a51a-abad95bd6c92" . "Sad Eyes")
("ac150725-fd4c-371e-911e-18a8b139efa6" . "Sad Eyes")
("c9749725-591f-4ab0-81a1-eac6feaa880b" . "Saga of the Architect Angel")
("64f7ad14-0695-4700-9669-685bac41fe00" . "Saint Jimmy’s Dream")
("e813dea1-09ba-4e2b-938e-eba80e2004d5" . "Santa Ana")
("c9f69d03-2a90-49eb-a45e-7f22f7597e8a" . "Saturday Night at the Big House")
("6792a932-6d5b-4392-8ec7-e58a145e20d2" . "Save My Love")
("79325ac0-f02d-3491-837e-925f57d052dc" . "Savin’ Up")
("30e0fb1f-2b7d-46c2-aa09-3519f8e97046" . "Say Sons")
("e700c458-c240-42b6-8c9a-8396e066d31b" . "Seascape")
("1ca78d1b-4ffc-39cb-af8d-94a13bab0a71" . "Seaside Bar Song")
("b8136841-95a8-3ce2-9e52-710d0aa56deb" . "Secret Garden")
("1dd6783f-fb25-4491-837b-1b95ddac5725" . "Secret to the Blues")
("3ceaab7d-52b4-3957-93e6-dfb7459803df" . "Seeds")
("f79d626b-d123-4d4f-8abd-2988a4d9fd5c" . "Sell It and They Will Come")
("49290dd9-8ae5-434b-bf30-df333309407e" . "Seven Angels")
("23c1230d-95ca-4599-afd9-237bb75e2f5c" . "Seven Tears")
("3a137d0d-351b-4e20-9c09-19bb9237f449" . "Shackled and Drawn")
("ec9884df-fb5c-4b25-b136-9f97c949d46d" . "She Comes Unto My Room (Scene #1)")
("f659cb74-b4bf-448d-ad5d-c813b84a2311" . "Shenandoah")
("753caf25-3fae-483a-99d0-fb50df1e8bff" . "Sherlock Goes Holmes")
("6b68afb7-d508-3cdb-8b41-687de4773d72" . "Sherry Darling")
("c8d77c36-4873-40cf-9b98-cda7973bb3eb" . "She’s Got Nothing You Need")
("52f295c5-a87b-4e00-8c26-ecbd00bdd970" . "She’s Leaving")
("22482e38-02f3-46d4-98a9-9138f844fd96" . "She’s Not My Woman")
("12c2db62-81a2-4d44-b948-59eb56111a3b" . "She’s Sure the Girl I Love")
("9f48ff63-25b1-4a8c-bdcb-70be8620ec46" . "She’s a Woman")
("7ed9ed5d-8526-4522-a6a7-42cbcc1864f0" . "She’s the One")
("e51cca70-a21f-4106-9457-bc34fb93a06a" . "Shilo")
("f0f75207-85a1-4765-aa45-5a5ed73d3cc0" . "Shuffle")
("1eb54c0c-8272-4126-81cf-dbc8de4c37d7" . "Shut Down")
("f474f679-98c9-3191-a147-c9e9d06679b9" . "Shut Out the Light")
("aa328a10-d766-4a53-9dd4-a143b8160d45" . "Sidewalk")
("4601b32c-a313-493b-946d-ed2ffe1a6938" . "Silver Palomino")
("fb6f5b78-65a5-3d8a-b1a0-e7431d1e4b14" . "Sinaloa Cowboys")
("6f306493-d949-48ad-9f85-c234202bb1ee" . "Sister Theresa")
("888df572-ba58-4f2c-b969-c0b2b6382dd7" . "Sleepy Joe’s Café")
("c482eedc-a2c8-46ce-9639-a257e2715c60" . "Slow Fade")
("c2d098ed-4318-4bfa-9f30-45dd03ab65a2" . "Slum Sentiments")
("bfa6cb9e-8981-4a26-8057-bccb366b5afc" . "Small Town")
("01ce77f4-2772-4f6f-b5f4-459f1dd9ff07" . "So Kiss Me Tonight")
("b3a929b0-2eaf-487f-b0a0-10ea3025530d" . "So Young and in Love")
("8e7f2e3d-c7fd-4556-a739-2645a4c3c09b" . "Soldiers of Fortune")
("70c782fd-c80e-43d9-b91a-0a0250a72fe7" . "Someday (We’ll Be Together)")
("a04cb9e3-24dd-4cef-a722-f7ea8bbf250c" . "Someone Waits")
("2d09387a-a652-36f1-a910-16183bf81407" . "Something in the Night")
("b61d29ef-a8ee-4626-866a-6ea40f2f5d1c" . "Something’s Gotta Break")
("b2e0ba23-d3e0-4772-ba2f-fbad31720774" . "Somewhere North of Nashville")
("8b024f59-978b-42b3-adbb-b0f0c2587c2f" . "Song for Orphans")
("30e97f2c-843f-319c-9458-1d6ce482dd7e" . "Soul Driver")
("02e59145-df87-49f6-ab2f-61560be41ca4" . "Soul Freakout")
("7bf0a150-ed09-4380-a5db-5f2a470c0c61" . "Soul Rockin’")
("48cc42b9-32ed-420b-9d47-8558c27a8f83" . "Souls of the Departed")
("3e0eea37-9ae6-4934-9dca-69f657fa5b18" . "Southern Sun")
("6a0a0ce0-c324-48db-86f6-9d953a20569d" . "Southside Shuffle")
("7da2926e-623c-498a-925a-2b9b8a6ed219" . "Spanish Eyes")
("968abe94-c500-4d1e-b3af-892fe520a893" . "Spanish Rose")
("3fc9dea6-c527-43c4-b3bf-5d3470a721e9" . "Spare Parts")
("66bd0eda-a34e-4fe6-af75-8d7c3bed6f4b" . "Spector Song")
("d1703fed-6287-465d-ae5c-1266cb32f29d" . "Speedway")
("dba2f648-cb81-3d19-b252-1bbecc62758b" . "Spirit in the Night")
("b096fd0f-0184-4d69-9c00-057104a07acf" . "Stand on It")
("75a7b094-f011-3177-a2bc-7d59d2e8de09" . "State Trooper")
("cf8d6593-1513-4d2b-8e27-77ec88bb606c" . "Still There")
("dd42f910-7768-4e54-8b12-9146bc124930" . "Stockton Boys")
("586c7c52-63ac-45fc-97da-2358a055a610" . "Stockton Girls")
("a499af66-ab4b-3b56-aec9-474252521fb6" . "Stolen Car")
("2ee8c46c-e453-48d2-8be4-d652aea28f58" . "Stones")
("d2feb13b-bcfe-4e5d-94df-504a5fc0d90b" . "Stop the War")
("ff84940b-6940-33d0-b343-16370f33ca15" . "Straight Time")
("93fae3d0-16ba-4e74-999b-de8e0449e707" . "Stray Bullet")
("bd2005da-0e08-479d-87e0-c92de6743a23" . "Street & Strip")
("46b9fddc-e2f3-4cbb-9cee-7722f1aa1b15" . "Street Fight")
("03ff9fe8-4501-47a6-8b85-00878118e974" . "Street Queen")
("912db60b-dc5b-46ab-845f-600f950e25b2" . "Streets of Fire")
("218391f6-1d68-353b-8a25-39a5a94c95a5" . "Streets of Philadelphia")
("9df0fe86-a22d-47bc-b2bf-4c02dd6dca88" . "Sugarland")
("69730bd5-ff09-455e-91c7-89e528c322aa" . "Summer Night")
("fef4468d-5467-4129-b267-75e392ed44ee" . "Summer on Signal Hill")
("fb001783-8377-4606-8ee6-4408fa3e2d57" . "Summertime in My Mind")
("dc5e60c4-5c61-40b0-bac5-7ecbb82633d2" . "Sundown")
("315e674a-529f-417b-bd9a-d206be27cd48" . "Sure Can Feel the Pain")
("319c4f5e-1d88-4e0d-8351-83ddd00f7694" . "Surprise, Surprise")
("ca31e3c1-5871-4130-a1ab-ddb3ef297f40" . "Surrender at the Citadel")
("6d59c063-3e4d-46a2-9424-c34b691d1c75" . "Swallowed Up (In the Belly of the Whale)")
("4e1576c9-a957-451f-af31-b5545d35b202" . "Sweet Melinda")
("54c9c394-993d-4d02-a123-e6e271eda3f0" . "Swoop Man")
("da394d8c-b0c8-413b-aea2-94ae776e0771" . "TV Movie")
("a8cc3293-5317-41c4-bb9a-5447e7aa38ae" . "Take Me Down")
("c239b40d-e60d-409f-8746-757c991fb340" . "Take ’Em as They Come")
("c908e556-a77f-422a-8cf0-a0e0f0c87d73" . "Talk to Me")
("27b6d44c-b7e5-4e07-9b62-07517f370902" . "Talking About My Baby")
("25d46c20-8dd4-48f4-b7b4-405b4b547c08" . "Temporarily Out of Order")
("b43b0285-cab4-45a5-95af-b91992111946" . "Ten Commandments")
("aa151b23-5171-3d35-a487-55fc8c66131e" . "Tenth Avenue Freeze‐Out")
("93d0d543-fda3-4cda-b0cc-57e993f58166" . "Terry’s Song")
("2db42a5d-12da-4cea-95a7-4fad86408ae3" . "Texas Carnival")
("c125f6d9-b2f9-480d-b0e9-4bd2e021f0d3" . "That’s Okay")
("ea8024c6-400f-4022-9498-7b3273600bb6" . "That’s What You Get")
("6eaeba87-f301-3932-84a5-39d4d88f9c53" . "The Angel")
("e697f9c2-1690-4a56-98f4-e6d263e564c8" . "The Ballad of Jesse James")
("b79de5ae-6eac-4db3-8c96-eb30c9ed0219" . "The Band’s Just Boppin’ the Blues")
("51b56e94-4b6f-4400-bdff-83d360e3d203" . "The Big Muddy")
("d6bd547a-84c5-46bf-9a1a-b1819fe4270f" . "The Big Payback")
("a9cf258a-0aa2-4e20-9b7e-f1834250249c" . "The Break Song")
("5c9e2ef1-7278-4333-a122-08d8046ff439" . "The Brokenhearted")
("9819b7b8-c082-4ef6-8615-9f1d2901836b" . "The Chosen")
("27ea9789-d41f-3924-9ed0-20e87cc82cde" . "The Comedians")
("f57fab59-a135-42d5-87a4-d8e3cfdb5e1a" . "The E Street Shuffle")
("20f6a5b4-7f15-4de0-88c6-d3e4cb3651ae" . "The Fast Song")
("e497263c-4f15-36c0-b27c-dca99482962c" . "The Fever")
("e3882bd9-d99d-486f-8737-2d09362696a1" . "The Fuse")
("3fff3842-e572-394e-a3c0-69a9dccccd50" . "The Ghost of Tom Joad")
("cdc85e24-66a1-3fcb-8bc9-129bddbae4b2" . "The Good, the Bad and the Ugly")  ;; Listed as "Il buono, il brutto, il cattivo" at MB
("3c423f0a-b7f5-4bd8-ab5b-26822d8274ac" . "The Hitter")
("d0f4c692-8c89-4ce5-beef-66dea796fe18" . "The Honeymooners")
("dbab1a0a-62d7-440e-a3d1-2b58cf06d0b6" . "The Klansman")
("4ca37c28-0a8f-4061-8a4b-9634bd1f1180" . "The Last Carnival")
("a010652c-7e94-4556-83e6-85140ae35ec9" . "The Late Show")
("a39a5b73-5d05-304b-9e5b-3ce210900cf9" . "The Line")
("87fdc6a1-2a57-4c6e-a105-643848ef6be0" . "The Little Things (My Baby Does)")
("59bcc040-38c4-3ba5-a7d6-a34f86f85141" . "The Long Goodbye")
("e1767cd9-fa23-41c9-8e00-c6ccbf4bbb66" . "The Man Who Got Away")
("f6f886cc-c3f8-4c35-9878-d747c55bafa1" . "The Money We Didn’t Make")
("537a14e2-7ea5-3ade-a6e1-3fcd2e3a5b80" . "The New Timer")
("77255843-fb0a-4565-9e0e-3a05d1e48023" . "The Outrider")
("c4844b57-4cb0-40a9-876c-205451c73746" . "The Power of Prayer")
("a96c311b-eb3c-309a-a080-011c893bce3d" . "The Price You Pay")
("046b3942-069f-3bec-accd-c85728ca9411" . "The Promise")
("13411c83-ddd8-31ca-b968-79e4cf6db8fb" . "The Promised Land")
("4b4ab9ab-0bb9-43bb-ae09-4993c4b90ff8" . "The Rising")
("fe0f14e5-de68-3aa6-bf48-80c4cbe2a1f7" . "The River")
("10071166-e1a0-45f3-8256-62c8ecd913dc" . "The Song")
("98e1ad9b-ab18-438b-9dff-70746edda4be" . "The Street Goes on Forever")
("3a46267e-eb87-40b2-9c51-e369f3c930e6" . "The Street")
("7eb86c15-4811-3576-a80f-0d9ddeef34d8" . "The Ties That Bind")
("5a7e2cff-7d4d-4d80-952b-c601f8b87404" . "The Time That Never Was")
("1140128a-b526-40a2-abb5-9da1d2405fbb" . "The Time in Between")
("6e5ed99a-5c24-4483-aba0-2f1d8e8a8a67" . "The Train Song")
("d0db298a-abc1-40e2-a2cf-f4f377ba3187" . "The Virgin Flower")
("28cce53b-0c6f-46fd-b9d3-a1e80f3b713f" . "The Wall")
("50ac552f-f115-4564-ae7c-ca5c5395915f" . "The War Is Over")
("1137a3e3-cbab-4522-80b8-6bcdba028702" . "The War Song")
("c4f4d4f7-e65f-4e67-96c5-0900840f8232" . "The Way")
("18c6e845-58bf-4d59-af17-af1be5c7ae98" . "The Wayfarer")
("80fa907a-66d9-40bc-b28c-2b3f3d797e79" . "The Wind and the Rain")
("44a4655d-baee-4d5e-a5b7-a554f030d203" . "The Window")
("b32be50f-536f-4f8f-9ba6-051fd4756b62" . "The Wish")
("86e0a8d8-d6f7-40e1-95c1-684b74292728" . "The Wrestler")
("db9a0e24-7204-4066-919e-c44d52eb8a13" . "Theme for an Imaginary Waitress (Fontainbleu Waltz)")
("c4598e0a-6902-40b3-8d3d-5d1ba3b7c3fe" . "There Goes My Miracle")
("f59d49ca-a274-4f29-994c-fbc7969525f2" . "There Will Never Be Any Other for Me but You")
("566f2686-f42b-474f-9167-98c9613cf197" . "There’s No Livin’ Without Your Lovin’")
("72ff809c-fb50-4aca-9cf2-cc760632289a" . "They Killed Him in the Street")
("d85bc3d7-1cfe-49d2-9034-89188175226c" . "Things Ain’t That Way")
("4660a7bc-65af-4c01-8b06-2be6804f6899" . "This Depression")
("58a01740-e85b-36a6-bcee-24bf53b8b2fe" . "This Hard Land")
("e2b06c1e-59e8-49dc-99cc-492897098744" . "This Is My Confession")
("99473257-a44f-46dd-a729-fb0889c12233" . "This Is Your Sword")
("51baffe0-9946-40e0-b014-2dc5d9b97c6e" . "This Life")
("db0f18f0-ae64-3aca-a1bc-d0f117da8f63" . "This Little Girl")
("c9c3e89e-e560-44a8-b93f-55d5b0c30e00" . "Thunder Hill")
("cc8d858a-38ef-35b0-b83c-a5d26f04207b" . "Thunder Road")
("88bcce1b-40e2-421b-ae4d-90e62ee4bad1" . "Thundercrack")
("4d2a497d-afca-478b-b545-8f377d10dc96" . "Tiger Rose")
("e878c8fd-e529-4a28-aacd-034ddd661738" . "Tokyo")
("87c30f64-6a77-4dda-bfa2-05017e8d06af" . "Tomorrow Never Knows")
("9ab10ed5-607a-4a72-9925-3ba839abc0cc" . "Tonight Will Last Forever")
("53cd9fdd-8b7c-4e21-b6a5-f699ba1b6fe7" . "Tonight")
("da90db56-e9b9-3e23-9106-3b54b20e546b" . "Tougher Than the Rest")
("ec9c3c3e-9df7-4a81-a738-cf8c5f602f2f" . "Trapped Again")
("58976436-655f-4cda-8aa7-2b3f45dc1bf2" . "Trapped")
("b25e7bfb-a41b-47f4-aad7-d5d3277d4ad6" . "Triangle Song")
("b28422d0-de1a-3ab2-b41f-c9694fe9b456" . "Trouble River")
("9b3514b4-42c5-4c07-a435-1489a57b1463" . "Trouble in Paradise")
("76615ceb-55be-4ad4-8647-3d74f2ae7293" . "True Love Is Hard to Come By")
("4d55bcdc-72a6-4af9-a1da-9fc24507f4e5" . "Tucson Train")
("4cc16c46-0a41-4ad4-a727-b055d5eed148" . "Tunnel of Love")
("ba3dd10f-0319-4794-9406-edb40c2d28c0" . "Turn Around...")
("f8b31ddc-365d-4eec-ae83-067c249c99be" . "Twenty More Miles")  ;; probably
("dd9b9058-1da9-419f-9caf-8b6c696c0942" . "Two Faces")
("44f4f110-6830-4e1a-bfc3-445e4a75e300" . "Two Hearts in True Waltz Time")
("fa36af81-2bd8-3436-bf94-7df8e818d99d" . "Two Hearts")
("e03f02c1-359d-4d64-a810-014f4d6aa5b2" . "Two for the Road")
("97b867a0-3085-4a05-ace3-57bca5e9b67d" . "Under the Big Sky")
("3b5c0ff3-3926-4a56-85c0-4f3b9e7924ca" . "Under the Gun")
("35dada33-3e47-4755-bb85-e956c7f983a7" . "Unsatisfied Heart")
("16e0f361-5e5b-4684-8e49-b6dc44d442f6" . "Until the Rain Comes")
("10323a75-e6a6-4f9b-afff-c11e64cc6d70" . "Upon This Day (Eurydice)")
("591130c5-c1d0-3bec-9164-966ee293cc0b" . "Used Cars")
("acb78c89-95e3-4265-9a78-96f3e1994d57" . "Valentine’s Day")
("fc1d5837-38b8-3aa8-b8d5-d13be07e5b0c" . "Valentine’s Day")
("f14ef823-c665-400e-bfd0-6a810a7521d2" . "Vibes Man")
("3ff1b049-c5ef-4e4f-996b-1adb3ad5b4db" . "Vietnam")
("8fbd91a1-52b2-4259-9c1c-1ea965c48e2a" . "Virgin Summer Nights")
("b2fa72b4-48e3-4577-a8f1-df0fc21ae0e2" . "Vision Spirit")
("898a7995-c862-47b1-9e4d-6962048463ae" . "Visitation at Fort Horn")
("a4e07ba6-944c-4432-b276-602b084ea12d" . "Viva Las Vegas")
("3b01dc89-5d00-40c5-ba1f-3f96ded0bc98" . "Wages of Sin")
("136490c9-316b-41e0-8ef7-ad1ffe4f1278" . "Waiting on the End of the World")
("64142910-6d7c-4ad1-a4b4-6dcdf4b0b521" . "Waitin’ on a Sunny Day")
("dd334765-57b1-4c86-b9a0-82323d3ae8e9" . "Walk Like a Man")
("a7aaa1ae-91b5-49ae-a353-67cd46d08d72" . "Walking Through Midnight")
("cf7e85ed-07dc-4842-a2ee-b7e823a36de8" . "Walking in the Street")
("f64d5a80-f420-434a-89bc-fc07921aee78" . "Walking on the Avenue")
("e4275592-964e-453e-b735-5f3c92430020" . "Wanda")
("1e32c3b6-0c70-4ceb-a937-ab38b0279951" . "War Nurse")
("3bf9495d-0ea9-41e2-a6ba-e4071f62218d" . "War Roses")
("cfd634b5-e1aa-4e14-92c3-03177dc1b869" . "Water Station")
("2ed84008-e0d3-446b-9789-7b7f581b19f4" . "We Are Alive")
("2cc7a4de-8ffe-4905-be63-cfb86be57e1f" . "We Take Care of Our Own")
("d2194b92-88e9-45cc-bcb6-969ca429fceb" . "Western Stars")
("f8e9041b-2aa0-44e5-bd7b-c151c37dcc66" . "We’ll All Man the Guns")
("0a15ceb2-9ce8-4f3e-b901-3e016abc8a30" . "We’ve Got to Do It Now")
("f14c3607-3b23-4d2b-b8fc-28f64406d06b" . "What Love Can Do")
("38592b88-e1a2-4496-98ad-cab1ea80852f" . "Wheels Make the World Go Round")
("fa77ba3f-ec9b-4d19-ab9d-16cd8d56d2d6" . "When She Walks")
("2f854353-7d95-414e-b0aa-21fbb4061493" . "When You Dance")
("4202ab9d-b867-4c4a-8f36-55a07939731d" . "When You Need Me")
("3db86ecd-083f-4573-8fad-5319a34b50ea" . "When You Walk With Me")
("78237b33-96cb-4aed-a21a-3d858f93e29b" . "When You’re Alone")
("fb147ab6-b81d-491f-84a3-f72b8e7e4fc7" . "When the Lights Go Out")
("80a92148-0687-4c9b-b34f-04cfe2401c47" . "Where Was Jesus in Ohio")
("90f5bc1c-09ad-4cdd-b8ab-b88b96a7905f" . "Where You Goin’?")
("16de3a03-cda4-3fb5-b584-191d73905084" . "Where the Bands Are")
("c264533e-61d5-4dbd-b950-546b36d9106b" . "White House")
("e7cb6485-5c4b-4e68-b267-ac6a71bc6ed6" . "White Town")
("c1059086-78c2-4db7-bcc8-844ebac59275" . "Why’d You Do That")
("45e33981-5aca-47a5-82f6-075b49d76455" . "Why’s It So Hard")
("7fd7cbc5-2096-4385-bcdc-84adf9e93dd4" . "Wild Angels")
("04b8f3d9-6914-4837-bf20-3557e1b5cf0a" . "Wild Billy’s Circus Story")
("ae9e1cec-d0ee-4c02-9836-a12790002c85" . "Wild Billy’s Lullaby")
("829d2342-db09-4a81-bf9a-f4e4e4f95b3e" . "Wild Fire")
("bb0ba870-5189-4a52-ab43-37a8effb7b6c" . "Wild Ones")
("059e3ee7-1003-47f0-a58e-24cfafe26aea" . "Wild Roses")
("5ac62970-8a5a-4bc0-b2e0-8c4a0f91a44c" . "William Davis")
("38826259-e79e-48b3-973b-78ee330be79d" . "Wings for Wheels")
("8e388376-3484-45d8-9385-59397cdafb64" . "Winter Song")
("327687b5-072b-44fc-93d0-355d10276a3b" . "Wisconsin")
("b15090ee-274f-35a6-81cf-db4c2e9202a3" . "With Every Wish")
("ef3bcdec-40c6-3ec4-9d7a-658e0c895a8c" . "With a Girl Like You")
("26138701-af07-4de4-ad93-e2396a83cca7" . "Without You")
("8a79ad2b-819c-47b3-a02d-85aee7d3c1c0" . "Working on a Dream")
("90963641-d3b1-3d89-aec1-c913c6c03fc4" . "Working on the Highway")
("7d014956-9ffe-40a0-b4f4-b77ddb388d92" . "Workin’ on It")
("c97f26f0-7207-4a30-9a4e-6949a5f6b7d5" . "Worlds Apart")
("df945e07-0fdd-43be-8a92-708b78523496" . "Worried Man Blues")
("63c911f2-914f-34e8-9bb9-866384ead0f5" . "Wreck on the Highway")
("c29fbe7c-9cfa-426e-9832-314cc14990f1" . "Wrecking Ball")
("ccf27e00-8c01-4802-944f-1f85a185e75c" . "Wrong Side of the Street")
("b1223a16-de11-48e6-a4f4-a525159079bb" . "Yankees Win")
("65a51e98-ec86-4a64-ac47-8ffdf8685a48" . "You Came Down")
("09dea4c4-c009-4982-951d-a1538345615b" . "You Can Look (Not Touch)")
("dd21710e-a51b-3029-ae58-cd6a3559af26" . "You Can Look (but You Better Not Touch)")
("d4f6c05d-d498-4b96-9da1-5994e874c473" . "You Don’t Leave Me No Choice")
("9f5aa26f-d796-4c0f-b320-e67c2640cf33" . "You Gotta Be Kind")
("46706c0c-eccb-40c5-8ebb-46974d8bbf62" . "You Gotta Fight (for What You Want)")
("ae7f68a2-6940-448d-adfa-c34593337240" . "You Mean So Much to Me")
("a24dbe41-61e0-4726-9fe6-31160d459a51" . "You Say You Love Me")
("f1a1219f-5c5e-43ab-80b4-be65cdcaeea5" . "You Sure Can Dance")
("f4a77656-8cdb-365c-aef9-034c098e249f" . "Youngstown")
("2f02e45d-3fcc-4959-9abb-1d8efe41f786" . "Your Love Is All Around Me")
("7eba13f7-0994-42ce-af94-1def5913c13c" . "Your Love")
("02ff1b99-720e-47a5-a6fe-209d84639ebe" . "Your Own Worst Enemy")
("f68a0871-271b-4fa7-b49c-ebdb13a7cfbf" . "You’d Better Be Nice")
("377c2037-56fc-4302-bd0e-1ce77da525cf" . "You’ll Be Comin’ Down")
("daeece85-7a1a-48fa-b48c-77319f65bd98" . "You’re Missing")
("dd88ec5c-a087-4cb6-94a0-66c8ae5d056d" . "You’ve Got It")
("cb4ed8b0-0858-478b-9fde-edd877fd5238" . "Zero and Blind Terry")
("620588a9-d6a6-4eae-8ba6-5bf9605c5187" . "Zoom Theme")
("69c09dfa-d23c-436e-975d-26bc7fe8bcd0" . "[instrumental intro]")
("b558db16-5d0c-4eba-8a99-be1da7e48de5" . "[untitled riff]")
("772a6a04-2f9f-46e1-bafb-08f47c1c0753" . "“BBC Blues”")
))

;; Sorting:  sort-regexp-fields  '^.*$' ' ".*'

;; Other writers
(defvar mb-bruce-other-works '(
("80bdccf0-b216-40b2-8521-eee9108f0092" . "(All I Can Do Is) Dream You")
("84f986f6-9145-3d62-80d5-5e729652ca52" . "(Get Your Kicks on) Route 66")
("b8aeb29b-6c42-38da-a179-12e1fdb97ad3" . "(I Can’t Get No) Satisfaction")
("98bb1633-d172-3a1d-8fca-3286a5aec2a2" . "(I Heard That) Lonesome Whistle")
("cae76add-7e9d-4032-9479-74068daf3134" . "(We Ain’t Got) Nothin’ Yet")
("a033abbb-5c02-3d23-9990-b3c9ca363375" . "(What’s So Funny ’Bout) Peace, Love and Understanding")
("d2ba3e86-ab3f-3984-a34d-aaaa964c7508" . "(Your Love Keeps Lifting Me) Higher and Higher")
("9828a873-3969-4823-a5d6-a6d7b0d38e48" . "500 Miles")
("f44d3b21-8947-34f1-b701-46ed1cc0eb15" . "634‒5789 (Soulsville, U.S.A.)")
("d3e5f2dd-dc91-35bf-9367-a53130b756fa" . "7 Rooms of Gloom")
("b4782f4b-5316-34a9-b261-60a295fb398c" . "A Change Is Gonna Come")
("415b61e7-d351-4348-bb72-cbb89efd9bf4" . "A Dustland Fairytale")
("15dfe095-3186-4173-ab25-5f2be9c3fcc3" . "A Fine Fine Boy")
("15dfe095-3186-4173-ab25-5f2be9c3fcc3" . "A Fine Fine Girl")
("9173f10c-a5f3-4217-a4f9-a673086f3a5b" . "A Life Full of Rain")
("c3b181cf-34b2-333b-847c-f8076c7e5a6b" . "A Satisfied Mind")
("9ff3f54e-ea6c-3a57-a928-22f0b3087db7" . "A Teenager in Love")
("de163cdd-afaf-3442-99ba-b5cb0040c229" . "Achy Breaky Heart")
("784638a3-9b45-308f-a22b-8f686636cc39" . "Across the Borderline")
("954d889b-ccb7-4be2-833e-a58d14491e81" . "Ain’t It Strange")
("5afc7b32-34cb-3cf5-839d-9ef490a27b0f" . "Ain’t That Loving You Baby")  ;; in BB called "Ain’t That Lovin’ You Baby"
("d97a13df-e992-399e-9b1e-8eb4c83af1b7" . "Ain’t Too Proud to Beg")
("93a64153-6208-3f2c-92ac-02105acefd92" . "Alison")
("13b8eeb2-e3bb-35d9-b110-2cfc8ef4b4d7" . "All Along the Watchtower")
("da6d0c3b-c9ca-35b0-a0e5-9bff0ef9bf8e" . "All I Have to Do Is Dream")
("8adf65d2-b383-47c4-ad14-67ca0fe2e567" . "All I Needed Was You")
("8b1e9aef-7e87-4715-a714-4e5f505de560" . "All Just to Get to You")
("7a0222a5-d9ad-3fea-9970-bbbf91de1b00" . "All Shook Up")
("cb08dc95-6bd9-453e-bf36-6bb0e7b02e98" . "Allentown")
("d323779c-4e32-4142-a8c7-eaf8e2914dfb" . "Always a Friend")
("689e400b-59f1-3f1a-a2e3-7aa5305bf73b" . "Amen")
("95790479-3937-334f-a175-ecb435fea09e" . "America the Beautiful")  ;; probably
("6d13f2fd-69bd-3230-8dad-459df8bbac01" . "America")
("acdf6bde-908e-4091-9e3e-fc336dc5bdfd" . "American Babylon")
("c9288b16-2220-4087-87bf-75d7868a73f0" . "American Music")
("a65466b2-7932-4460-82a0-43b6896855e3" . "American Slang")
("6c0edf2e-3df6-31c2-855d-eb0cd62e184f" . "Angel Eyes")  ;; Frank Sinatra version
;;("af5094b5-ebf6-4f22-bc23-2d2f82899345" . "Angel Eyes")  ;; Little Steven version
("490bc8b3-2c9c-316e-8f30-a9a40cc573f0" . "Another Saturday Night")
("36264e2c-f574-4c8f-b94a-6af076fb2c50" . "Any Other Way")
("a81ccb0c-9713-3c9b-807b-94eb474633cd" . "Apache")
("8fe3ee6b-106f-3d53-b7dc-7f5ce350f0f7" . "Around and Around")
("1ef39ff0-baa0-4a5b-bc64-74f976bd5e1e" . "As Long as I (Can Be With You)")
("e578ab10-5751-3f4a-ae3c-9216ac376336" . "Auld Lang Syne")
("31037967-3044-4022-9ee4-9bd37b4f7220" . "Baby I’ve Been Missing You")
("3b5dcdf2-b074-4bfe-8d59-88bd53953ffa" . "Baby Workout")
("5461f0e3-d640-3902-b8fe-5496922e9d2b" . "Baby, I Love You")
("59c21df2-f1a3-49af-98e8-30becab2a5ce" . "Baby, Please Don’t Go")
("4dc1b48e-4722-30a1-8743-53526e2b5621" . "Back Door Man")
("ca2d9839-82e6-30f7-b2f5-f1f3924e567b" . "Back in the U.S.A.")
("869019ef-dc73-4a7b-9467-bd66512f11f5" . "Bad Day")
("2b226979-d658-4f76-8c74-1e352aca08ee" . "Bad Luck")
("afeba2c2-78a5-3d25-9813-33e0f97c3171" . "Bad Medicine")
("4f4cf4ef-0357-37c4-9fc7-846513f5fa08" . "Bad Moon Rising")
("5b0bc8d5-00f4-4e12-b06f-cd01406ac22d" . "Ball and Chain")
("cfb4648d-0143-34bf-a3bf-eed69a923879" . "Ballad of Easy Rider")
("29133cae-9aa8-4449-a18f-4dfd0c08ada9" . "Bama Lama Bama Loo")
("29ff18d1-f057-3c91-b2cd-814424903d0f" . "Bang a Gong (Get It On)")  ;; It's called "Get It On" on MB, has been renamed in the US
("a428fb5d-eeaf-3712-8a93-c7d9330e56d1" . "Barbara Ann")  ;; Is written as "Barbara‐Ann" in MusicBrainz
("e0d4b068-9255-345a-be44-6416316bb8dd" . "Be My Baby")
("e16e4dde-5639-4ec0-8376-00867be37c1e" . "Beast of Burden")
("0a32b6ca-882e-3b99-8796-119ddedb090d" . "Beer Barrel Polka")
("85b4062b-0074-3e2c-bf1a-c03b5fc40116" . "Better Man")
("8d022dd1-e1c1-3c00-9c6d-f57d140e84ab" . "Better Things")  ;; The Kinks
;;("81b2c81b-784a-4c46-a7a9-60ab3b10893c" . "Better Things")  ;; Tom Donovan
("5be7e03f-aa3c-4d31-af43-d45eddd641a2" . "Better to Have and Not Need")
("d54726bc-48ef-4e01-8540-dc91595cb676" . "Big Black Heaven")
("808a6456-3c26-377e-b94b-e31bf3a2f402" . "Big Boss Man")
("be39cb45-a437-43dc-b484-1cd13c4a43c3" . "Big Yellow Taxi")
("915fa3bf-4147-4ee3-89b8-1e4b86cdf0e1" . "Billy’s Waltz")
("ff58da40-892f-3542-99d6-7ee4dede83e4" . "Birthday")
("0a39455b-860f-4716-8e13-e84397408247" . "Black Books")
("b5de027f-f79e-43b7-a828-9671c9076028" . "Black Ladder")
("23dffad1-b4ba-3016-9760-5d4e2f7b78b6" . "Blood on Blood")  ;; JUST: Jon Bon Jovi
("6ad9bfb0-004a-4760-b819-a58410d143b8" . "Blowin’ Down This Road")
("f5a12d42-00a1-3bc2-8ed1-df2506ea3db2" . "Blowin’ in the Wind")
("a9aa8366-e55a-3df5-a61a-050fbb189d10" . "Blue Angel")
("653769c3-7a78-3b1b-9bcc-be7a6c335a1b" . "Blue Bayou")
("414afdb5-bd79-3349-8a93-d940da14c36e" . "Blue Christmas")
("e5e57b3d-ff8c-3def-854b-fc9d22668f7d" . "Blue Suede Shoes")
("7c3049d0-c1e8-3f7b-96e3-579617331111" . "Bo Diddley")
("5dc126c3-f0b3-3226-8a47-f9389a84cb8f" . "Bony Moronie")
("bf15918b-4880-423f-b882-424cb6e9575c" . "Boogaloo Down Broadway")
("4c4b77c7-21a1-32a9-a29b-241ff99cb397" . "Boom Boom")
("46aba62f-15ed-3642-9dab-cdd8b4a312a9" . "Born on the Bayou")
("89c91780-f1e4-39b2-9645-9802c102414b" . "Born to Be Wild")
("bd310a7f-9486-3871-8637-48a79b2b07b7" . "Bottle of Red Wine")
("ce7a1aeb-8feb-32ea-ab0f-255599a34107" . "Boys")
("0c9b4735-4e43-30db-b061-f4f4de6d70c3" . "Brand New Cadillac")
("a96bf584-ee25-3a81-beb4-85a247240ee9" . "Break On Through (to the Other Side)")
("048976c0-44b3-3da7-85cf-2da8341b1e66" . "Bridge Over Troubled Water")
("57a0e7e7-f1b8-3fee-853f-fcd0fff9be02" . "Bright Lights, Big City")
("063b951f-05b4-3cf7-a378-7fbc7faa78cb" . "Bring It on Home to Me")
("13923f65-80c9-46da-8ed4-2d144c8303b5" . "Bristol Stomp")
("a31bfb5c-c247-4183-b54f-b259981f9925" . "Broke Down Piece of Man")  ;; JUST: Southside Johnny and Bon Jovi
("8c282f81-e545-4bf6-8ded-078b2094c391" . "Broken Radio")
("44d76ac0-58b3-4c70-bc1c-daf339173527" . "Brother John Is Gone")
("0692cc42-4f43-31f5-9d95-b4ba3fd9e179" . "Brown Eyed Girl")
("a6a68a05-44fd-3347-b67d-0a73dd51bb0a" . "Burning Love")
("24ee6709-2d97-3059-89ec-8f189a287136" . "Bye Bye Johnny")
("e15c67a0-a9d1-486e-8915-47a7f580f3f4" . "C.C. Rider")
("9c56c803-4e62-3c9a-a734-28bb845ebb06" . "California Girls")
("23acf0ad-09ef-3ed8-a3e2-659714581e53" . "California Sun")
("238819bb-3897-3a6f-9385-bbddd5ea3c6f" . "Can I Get a Witness")
("8f6d1d5a-7d59-3dee-bafc-ea5c8d71f760" . "Candy Man")
("06afdb15-bfd4-388f-8d51-72531748b715" . "Can’t Help Falling in Love")
("34607acd-d7b7-3294-9bc8-8cedbe6ccd15" . "Carol")
("90fe4c45-2264-385f-8573-0c8602e03a96" . "Carolina in My Mind")
("dbdebc46-8362-333d-81a2-6c8910f4654c" . "Catch the Wind")
("308aeb8e-753c-4505-88e4-b2b175d485f4" . "Cathy’s Clown")
("18aa70b6-3288-48b7-9747-eeb03bea7c02" . "Centerfield")
("83a551e3-d8d3-4533-8871-4af752b12e88" . "Chain Smokin’")
("5ef69804-345c-4e4f-92c0-1ccb38189da5" . "Chain of Fools")
("ea5279b0-8915-44d8-a253-1b3f18d888fe" . "Chicken Shack Boogie")
("b6417ad9-7965-331a-8a0f-18c3da2f4f74" . "Chimes of Freedom")
("c0445507-0c48-49e7-bf8e-c5df780e57e1" . "Christmas (Baby Please Come Home)")
("9aa8279b-3deb-4945-bb72-aae5c28d894a" . "Christmas Day")
("32e9c74b-0003-3811-9a6a-09ec86f31446" . "Circle")  ;; On MusicBrainz this is stored as "Circles"
("b36002b4-f493-4975-a488-a79225456e6c" . "Circus March")
("8a4766d6-60a1-3179-bd6b-febbc83441af" . "Clampdown")
("8c1bb4d2-95e7-405b-8ec0-a21989c3f9cc" . "Claudette")
("be81408c-6e68-39db-ad9c-6890dfaf7f73" . "Cold Sweat") ;; probably
("8bb90de1-d285-426f-a372-c8cb5ce2a559" . "Coma Girl")
("23ea782a-f361-30f4-8d43-d456d49268e8" . "Come Softly to Me")
("bcd490e5-dac7-3b8a-b423-ae17e1209f3d" . "Come Together")
("5587820f-9171-3130-bab4-1b416c3c5cae" . "Come a Little Bit Closer")
("9cb5a89d-cfbf-4b8a-8154-7c90f230e00c" . "Come on Over to My Place")
("9fe980f6-f668-46e9-82ad-838cef792656" . "Comin’ Down Maria")
("cd5edacb-2c63-474c-ad89-f9f28c639b78" . "Could Die Young Tonight")
("fc59a3b0-b3f6-47ae-b8fb-fb4ce14d095d" . "Country Feedback")  ;; Nils Lofgren guests with R.E.M. on the following song
("8ec642c9-36cf-367b-89eb-70b3a7c4bdc1" . "Crazy Love")
("d425aad9-b29b-3b45-a81a-580678efe16c" . "Crossroads")  ;; probably - It's called "Cross Road Blues" on MB
("bac1d4f9-0a5e-3475-a4c1-ebdb9b9cf893" . "Cry to Me")
("855d05a4-7146-38c3-8b95-ce6297526e34" . "Crying in the Chapel")
("50cb0424-842f-3ef2-888c-a1adb5adc041" . "Crying in the Rain")
("773af48a-e550-3885-b575-1590dcb15dfa" . "Crying")
("90c5c3dc-6f0c-3137-a711-f9bca4a9f9b0" . "Cupid")
("aa289c4d-66c8-4d80-b770-f133576e27c0" . "Cuts Like a Knife")
("cc4d9b76-b036-4172-9c0a-8cbed864a20f" . "Da Doo Ron Ron")
("0c52bf48-fe2c-3b09-be1a-3450bec10910" . "Dancing in the Street")
("d62b3bf6-226f-3580-ab1c-7676aa2d93a9" . "Danny Boy")
("05e4b001-759f-4369-8679-b41a01109452" . "Darkness, Darkness")
("08699854-85bc-34dc-b222-cc0657b371ce" . "Dead Flowers")
("4e3a00fd-c5b4-4020-ae88-0ca417206dc8" . "Dear Lady Twist")
("fa1ba832-28bd-3b2c-ad70-4f32f0e65b21" . "Dear Mr. Fantasy")
("1ce352f8-147f-304b-8735-24bf7401cb14" . "Deck the Halls")
("143e2b7a-7ef8-4a25-aff9-10c29cf9b39c" . "Deja Vu (All Over Again)")
("da1f36f5-dcdd-4a56-8d92-4b3527fd1291" . "Detroit Medley")
("2b2b1033-27c1-4711-928a-87181ff44844" . "Devil With the Blue Dress")
("0f4c3430-2848-4d13-8677-643a5e7c52b9" . "Diamonds by the Yard")
("65b8e8f6-5058-43f6-b0e3-ef99cb0393dc" . "Did You Say Such a Thing")
("40404ff5-ad09-4ff1-9762-a077d051cdb1" . "Diddy Wah Diddy")
("0480136f-ee0b-47ce-8245-31e96189e674" . "Dimples")
("1285ce46-0263-4b22-a73a-7f8c4f0b509a" . "Dirty Rotten Shame")
("75688ca0-8b04-341b-9f84-d59d63b16cf7" . "Dirty Water")
("b53501ce-4d3b-446d-a379-ea147b637876" . "Disgusted")
("141c7e33-5401-4ef0-ae71-2638e16ecc32" . "Disorder in the House")
("6fd5043e-3169-3301-a87a-c2f483aa64aa" . "Do I Love You (Indeed I Do)")
("c940da6b-8ba1-3238-bbeb-22e574efdf7a" . "Do It Again")
("d60f4c0a-8c35-3707-bb52-407e63e33d1f" . "Do You Hear What I Hear?")
("2035da4e-ed02-3811-b134-7674fbcb92d7" . "Do You Love Me")
("e5719339-1996-358f-b71b-25e96494a071" . "Do You Want to Dance")
("b64b28c2-69ad-38c4-8446-a4a9c7ae2186" . "Domino")
("05f56ef6-7db3-3275-9df8-65c200cc35fa" . "Don’t Be Cruel")
("f0e75ada-5407-3aef-b410-7862331c34ae" . "Don’t Change")
("9939d9e3-2a5a-4ff7-aa24-026b40a4f925" . "Don’t Hang Up")
("be6bc1bb-44df-3d9e-a26a-e1be5b30ea9f" . "Don’t Let Me Be Misunderstood")
("bc18692b-e0f2-302f-9f6a-96f36821e1b8" . "Don’t Play That Song")  ;; on MB called "Don’t Play That Song (You Lied)"
("de5fa494-814d-3b4d-b22f-ba3d52b48ef9" . "Down by the River")
("7a4fc2c6-9921-321a-b3b8-eb369655a8d9" . "Down in the Valley")
("209fd9b7-e830-3127-b5c9-8bc4a5bcb11f" . "Down the Road Apiece")
("75c60a42-b49a-4837-8325-71237117ae5f" . "Drift Away")
("d7062610-4809-4004-9b53-a53cc89f8229" . "Drinkin’ Wine, Spo‐Dee‐O‐Dee")
("388613bc-d5f1-366e-896d-e50df985b796" . "Eleanor Rigby")
("e0ebd5ac-83c3-44c1-b54d-90fc78dce96f" . "Endless Night")
("f88369e2-da98-30a7-94bb-1509ff26c456" . "Every Breath You Take")
("84c3151f-6d1e-3cf8-aaef-025df5c9a3d6" . "Everybody Needs Somebody to Love")
("e0a1f5f9-c888-3bd3-bb00-4529d9fa9cb5" . "Everyday I Have the Blues")
("dcecb898-28fd-4ddc-aa2c-c4f8882c6e4d" . "Everything I Do (Leads Me Back to You)")
("e8b7d248-94dc-4a79-bed3-3d7ccc13165b" . "Everything’s Going to Work Out Right")
("cb67ceac-f65e-34af-b2b2-74912a6e58d6" . "Expressway to Your Heart")
("671e17f4-c244-4c81-9c93-438b1b11de96" . "Faith")
("a0a52d8f-bacc-32d8-9117-331d6c99a755" . "Fame")
("7d0392c9-7c33-471a-a373-bd56cb8be251" . "Farmer John")
("22890ccc-e4b0-4c7f-879c-cd89a89b5b92" . "Farther Up the Road")
("5c426c60-6af2-4341-834c-db30838b0b53" . "Fast Blues Break in G")  ;; [unknown]
("11be7395-e207-4551-bcc6-0490b5d634f1" . "Fast Blues")  ;; [unknown]
("3a91bfc9-0016-40f7-a855-82e911cd6b8d" . "Faster and Louder")
("33df6313-71f4-4f17-887c-966002954cfa" . "Fa‐Fa‐Fa‐Fa‐Fa (Sad Song)")
("f26619ec-5ab2-33c6-9186-56607b3f4568" . "Fields of Gold")
("c259b9ae-fa8e-4ea6-aa88-a9b3e55e3902" . "Fingernails")
("ef334da9-4b23-3046-a321-cf93a75cd842" . "Fire")
("c44521c8-2fed-4802-8b0c-b00a19c625a2" . "Float Away")
("8bd3625e-a993-3388-bfb2-23dc1350fa4d" . "Folsom Prison Blues")
("17e250e5-8ff2-3efd-ae95-82de60e784a3" . "For Your Love")
("f978ba1e-9678-3761-a957-fa4084c0d09f" . "Forever Young")
("54f65692-5b12-4d3e-a012-aa07cb0dd403" . "Forever")  ;; JUST: Steven Van Zandt
("31ec5d71-8104-350e-a27c-b58d025e8c25" . "Fortunate Son")
("30e368af-14bb-3472-8a69-c033583eed20" . "Foxy Lady")
("0344e14c-82c0-3bff-83d9-4910cd7db9f8" . "Free Money")
("5a6a974f-864c-4dcd-8211-f69661e44d62" . "Free Nelson Mandela")
("71884fc1-8989-4b33-82fb-45ad53e5b7c2" . "Freedom Cadence")
("d0b46ebe-9594-3449-aea9-18857d47e8cb" . "Friday on My Mind")
("67ad3571-dc6f-3c67-9d6f-636774b4db74" . "From Me to You")
("f099fa1d-9ef0-3c5b-b0dc-cec3eb159780" . "Fun, Fun, Fun")
("f8327730-4000-3530-a1c5-072467f1508b" . "Funky Broadway")
("246d57d9-014b-3b72-8bc8-aeb25e9fca49" . "Get Back")
("7a369a15-bf19-49ab-ac6f-7d761f81b0c7" . "Get Out of Denver")
("ccee2f68-d4bf-3d39-a961-64bd942462c7" . "Get Out of My Life, Woman")
("3df4f8a5-9d24-353b-a658-dae5711843c9" . "Get Ready")
("c1799562-29b9-3105-adaf-aac56cdd56ad" . "Get Up I Feel Like Being Like a Sex Machine")  ;; in BB called "Get Up I Feel Like Being Like a Sex Machine"
("00fdec8b-0f0b-32c7-91f7-284ea8b59042" . "Get Up, Stand Up")
("14f3287f-4952-33df-b699-9687f7d6945f" . "Gimme Shelter")
("84e25335-d9e9-4eee-8767-8bcdc4044f7d" . "Gimme That Wine")
("d0f2c6b0-1fbf-3fdd-9057-9e8774d51641" . "Give My Love to Rose")
("5923fa49-79c3-4251-84fe-d54a1ed9db86" . "Gloria")
("acee5958-2c21-3572-a57c-b0a578290a21" . "Gloria: In Excelsis Deo / Gloria (version)")
("dec5eb02-6d51-385d-bbe5-864a51b09511" . "Go Go Go (Down the Line)")
("37571036-ad15-441a-a545-7bc1102666bb" . "God’s Counting On Me… God’s Counting On You")
("3135d904-ae48-3068-8ab3-03844d88c354" . "Goin’ Back")
("974cbf65-1be8-338a-b7d4-c9fd42c56b57" . "Good Golly Miss Molly")
("b4c252ed-c21f-3a8f-9473-d676065cfd55" . "Good Lovin’")
("ab81d559-03a6-3766-af0f-2d41c6840cf6" . "Good Rockin’ Tonight")
("f322792d-9f7a-38f2-bddb-73731c1b2039" . "Good Times")
("375a2438-89ac-4ef3-b5f8-d2f3dd0afe93" . "Good Times, Bad Times")
("dd7e9416-e0ac-3eec-9377-768623fb37ae" . "Goodnight Irene")
("becafd87-00f1-44e8-ae9b-68a823557eae" . "Goodnite, Sweetheart, Goodnite")
("e72886af-540d-3210-a9c0-13e78402014e" . "Got My Mojo Working")
("9f8f1342-d34a-4509-bfa3-4fcfebec3a2d" . "Got to Get You Off My Mind")
("10caf751-2927-38d2-aaab-5dc23297cbab" . "Great Balls of Fire")
("a1e52585-a533-3d1f-a857-30405212a5de" . "Green Onions")
("c1b9bc08-ba81-382f-8ce0-83995035368f" . "Green River")
("0e0f6c98-e582-4b77-ac65-2a9a790b6793" . "Guess Things Happen That Way")
("a9a4860c-c524-3dab-ba2c-af4c58d87cdc" . "Guitar Man")
("5c635eba-780f-413c-9c89-bf82d1ea2bdc" . "Guitar Town")
("ed6ba382-ce15-4edc-ab89-c8f590863560" . "Gulf Coast Highway")
("37a38fa9-ff8f-44ea-97cc-0397fdf6cc34" . "Gunslinger")
("f4b0fe1e-c19e-3ba7-9979-a050a86bc973" . "Gypsy Woman")
("725f5c47-743f-49d6-9a2a-e80b41c838da" . "Hail to the Chief")
("a7baceed-e1ca-4468-9687-d8cfbee3086f" . "Hang Up My Rock and Roll Shoes")
("c175c340-1555-3f7e-8bbd-040b47336771" . "Hang on Sloopy")
("bba40a4a-c222-3d60-8f3a-95ad07a421fc" . "Happy Birthday to You")
("c5563993-5a7f-39b2-bd7d-bf00127c43ae" . "Hard Times (Come Again No More)")  ;; Is called "Hard Times Come Again No More" in MB
("6b80e488-9802-4b05-9a0f-97631a5ea6af" . "Hard Times Come Easy")  ;; JUST: Richie Sambora
("a7bb436f-47aa-4ef3-8278-38eeb42727d2" . "Hard Travelin’ Hootenanny")  ;; It's called "Hard Travelin’" on MB
("5b84507c-87b1-418f-83fb-7f8dde9908b1" . "Haunted House")
("87e0dbaa-e5ac-4849-a3fc-44a3d6671f0a" . "Hava Nagila")
("7b76ad3a-3577-4441-aadc-e8b6f306ef18" . "Have Love Will Travel")
("2ff51d26-cc88-443e-83b9-baf9cf244112" . "Have Yourself a Merry Little Christmas")
("c6222b2f-6831-4f30-8ce7-a6fb9f0aeed7" . "Have a Good Time (but Get Out Alive)")
("450b4d94-c768-3a43-9edc-253cf300199e" . "Having a Party")
("92465181-5e5b-4740-aed0-99e21ffa0d06" . "He Can’t Love You")  ;; JUST: Nils Lofgren
("135421b6-75db-306a-8082-1f133b4299ea" . "He Was a Friend of Mine")
("49eede36-8c47-3f2d-81d6-7afb5a5a1979" . "Heart Full of Soul")
("f8069a6e-1bc5-3623-b522-17dc41cbe665" . "Heartbreak Hotel")
("f5838306-a92b-33b7-9b87-b30228265c79" . "Heat Wave")
("c2896dcb-74f1-41e3-a120-9b55604949b5" . "Heaven Help the Lonely")
("265b03f1-4aad-4ae3-a6fb-8b1ba5b4ea93" . "Heavy Bertha")  ;; [unknown]
("92180ca4-4c6b-341f-8ed4-1cbd87432774" . "Heigh‐Ho")
("3a15b53b-8b87-4f40-946f-728dd70542e4" . "Help Me")
("295f4446-1cf8-3e2b-8c28-dcf2765bc78a" . "Helpless")
("864ec2e6-0aa8-36a6-9b46-8e3843665702" . "Here Comes the Night")
("8549de8e-f65b-3e64-b8a2-08c90cc5fdd1" . "Here Comes the Sun")
("b01fca31-4bda-3783-9f67-1d3feb84acb3" . "Hey Tonight")
("17906c66-17aa-4e3a-92ae-22ed267f9330" . "Hey! Bo Diddley")
("ef9f0ca3-d45d-3452-aacb-64f9b95c211f" . "Hey, Good Lookin’")
("303d7a47-612a-4c1a-8760-6ffb1a1f0411" . "Hey, Porter")
("b17cc4d0-b607-33f7-940b-56d222ffc640" . "Hey, Western Union Man")  ;; in MB called "Hey Western Union Man"
("c5d76a22-89a9-3713-8fe5-84b8a659f5a1" . "He’s Got the Whole World in His Hands")
("50def0f3-4593-4552-ad6c-a7db8a50304d" . "He’s Sure the Boy I Love")
("ef55332d-604f-4b7e-ac2c-bff3ec9d845d" . "High Hopes")
("e2fee08a-d64c-414a-aa72-43e0338c66e4" . "High School Confidential")
("2d336de3-866a-3344-a76c-87ea2676b0b9" . "Highway 61 Revisited")
("7299dc81-1aa9-3ac9-a07e-1e0e71fbf1f7" . "Highway to Hell")
("e2e8b1e9-2247-3e4b-852f-5ee4c2e77b46" . "Hippy Hippy Shake")
("cd2abace-ec92-3a74-8cc0-4131a33d1fcf" . "Hi‐Heel Sneakers")
("9b004d82-a39a-3fae-b9ab-10502658bc91" . "Hobo’s Lullaby")
("19d4ae05-391f-311f-a487-24b21d48d095" . "Hold On, I’m Comin’")
("2f5f6618-ec92-434a-9b3b-bf8b2305956f" . "Hold Out Hold Out")
("645fd65e-9c45-3e82-99b1-dfd731da44e8" . "Honky Tonk Women")
("e6c8937a-6e81-3c6c-ac28-96a3bfe0ab21" . "Honky Tonk")
("25d54247-3890-3acb-ad70-734713579cbd" . "Hoochie Coochie Man")  ;; http://musicbrainz.org/work/25d54247-3890-3acb-ad70-734713579cbd/aliases ; This is called "I’m Your Hoochie Coochie Man" on MusicBrainz
("97a0c5bb-5410-35c0-92b2-927f7e458bec" . "Hound Dog")
("29b2d5d1-1783-40e7-a50b-71c5a0c377e8" . "Hungry")
("5d86ef9d-3ca8-4cdd-8d23-84e531ff29d5" . "Hymn to Him")
("4fb71e6a-6a72-3b48-b5e3-f45d60c6c8ec" . "I Ain’t Got No Home")
("ac9c9a5a-0042-430b-becd-5df92a5e806a" . "I Can Only Give You Everything")
("855db469-8d02-3dc3-83c5-0484441158f0" . "I Can’t Help It (If I’m Still in Love With You)")
("209a62bf-4d46-3465-94ea-bdb411f4fccb" . "I Can’t Help Myself (Sugar Pie, Honey Bunch)") ;; in BB called "I Can’t Help Myself (Sugar Pie Honey Bunch)"
("298537b3-e09c-41af-8d48-7606a12da9ac" . "I Don’t Want No More of This Army Life")  ;; Also called "Army Life" in MusicBrainz
("e5500466-274a-48cd-9c75-abff7ada71c5" . "I Don’t Want to Go Home")
("01cdde8a-aef0-4caa-8e5c-9d805f0d1df8" . "I Forgot to Be Your Lover")
("f4d124a8-3c74-3885-9bec-fe0be82aea98" . "I Fought the Law")
("d0dfcb16-d373-3901-81ed-20fdc87b6086" . "I Got You (I Feel Good)")  ;; Also called "I Found You" in MusicBrainz
("631a840b-efdf-353c-b9a2-bd53b6e5684d" . "I Got You Babe")
("cbd5547e-eaf3-43cb-bafe-9af36a2e691a" . "I Happen to Love You")
("87509322-3cb5-4d6a-bceb-4c2a1c820267" . "I Have Faith in These Desolate Times")
("5a93791a-fc12-4bc2-9206-0b8983a7d338" . "I Hear You Knocking")
("13e8525b-3267-3490-9875-862cfcb57f6a" . "I Hung My Head")
("20c7178f-8ab9-3d6e-8c4f-1135d997fe8e" . "I Just Want to Make Love to You") ;; in MB called "I Just Wanna Make Love to You"
("8fd4fdf2-247a-449b-9d26-4de9d74099db" . "I Played the Fool") ;; JUST: Southside Johnny and Jon Bon Jovi
("a8a158e3-0a0e-37e1-b5b5-9480a02d816e" . "I Saw Her Standing There")
("4f1e73d3-a070-3ee3-9338-90c8e5fc1963" . "I Shall Be Released")
("241580ef-8b4d-4c44-8474-356b1e5f08a0" . "I Sold My Heart to the Junkman")
("c2f9c697-a90e-3e7d-b2c2-173a26b0d517" . "I Still Haven’t Found What I’m Looking For")
("0a97123c-960f-3186-a7aa-517ac7907b22" . "I Thank You")
("1f50b407-8928-43b7-8709-1e28d840186a" . "I Understand (Just How You Feel)")
("ba883522-bf5a-37b5-b6b8-7572e139c6d7" . "I Walk the Line")
("f61ac0ce-01a9-38f4-870f-9f2cad167650" . "I Wanna Be Sedated")
("24e04462-eb1a-337f-b20e-8ed8fee09d61" . "I Wanna Be Your Man")
("549b28ab-60aa-4ec5-8203-4f3e94661846" . "I Want You")
("518bb151-fe46-417c-bf92-3d89c5b246a4" . "I Was Born to Rock")
("bdab6595-3f44-3403-8eff-b1ecabda9530" . "I Wish It Would Rain")
("30825ee8-7ac8-439d-9713-9913bca00e1a" . "If You Leave Before Me")
("7fd47fb9-1a13-4711-a3ef-31c7e9f23c77" . "If You Need Me")
("7c0e6ef3-13a4-388f-8dc8-cef4d3b2155f" . "Iko Iko")
("660e9c82-497b-307b-8374-1f5489becdaf" . "In Dreams")
("d9d1e4d7-2181-3565-89db-377d6b031af7" . "In the Midnight Hour")
("a42847be-d200-3d61-926c-632f57c77b1c" . "In the Mood")
("55d4145e-81bd-396b-a8f0-323b05266e0d" . "In the Still of the Night")
("c65f112b-83a2-3526-bc61-b31d573d9b7c" . "Into the Mystic")
("a90eacd0-9a85-33de-a48d-476198dfe777" . "In‐A‐Gadda‐Da‐Vida")
("004bc895-8070-437d-a51c-e2e59ac91b2a" . "Is She the One")
("a94809d4-e4cf-4e51-897e-ca9b82af76db" . "Is That All to the Ball (Mr. Hall)")
("034c723f-a482-38d8-863a-39726e7823d0" . "It Takes Two")
("bd4d1773-1891-30fd-8329-b0fb008a0dde" . "It Takes a Lot to Laugh, It Takes a Train to Cry")
("9f9307a7-890c-3caf-bc64-c5fd56d93a20" . "It’s All Over Now")
("fbf79a36-170f-39dc-83dc-c190f1fcfa3d" . "It’s All Right")
("453dd62c-072c-3b5a-9704-c0a82ab196be" . "It’s Gonna Work Out Fine")
("b7b8647a-5484-38cf-a82c-49276b815ade" . "It’s My Life")
("e9c8011a-fb29-359a-b7a5-80d30cabe378" . "It’s Not Unusual")
("8e95a967-31e5-3839-9290-e9d30af110d8" . "It’s Over")
("4d8b8e01-20f8-38d4-b149-addfef1d7b7b" . "I’ll Feel a Whole Lot Better")
("2a6d5af1-2579-3dfe-8f23-bcbbd4893ba9" . "I’m Alive")
("67d273f2-be0f-3183-933f-77b2743be3f9" . "I’m Bad, I’m Nationwide")
("b39b9a9d-e0f3-4176-918d-c1cee27c613b" . "I’m Not Sleeping")
("57fa769a-2036-3a18-905c-b423eb805894" . "I’m Ready")
("9294eeaf-6da0-330d-aeaf-7e77c2158472" . "I’m So Lonesome I Could Cry")
("25d54247-3890-3acb-ad70-734713579cbd" . "I’m Your Hoochie Coochie Man")
("ae02523c-35da-37a1-af67-084319b767ad" . "I’m a Man")  ;; probably
("71d751b2-8c62-49ac-9b15-79e8d7c04817" . "I’m a Thousand Miles From Home")
;;("c323f194-6477-4746-b4e7-a2476433a267" . "I’m in Love Again")  ;; [unknown] - There is also a Bruce version
("3f39c895-3e48-4221-be8b-b237bd6fed6c" . "I’ve Got to Know")  ;; in BB called "I Got to Know"
("cddd2931-ef1f-39ef-b199-68a8a5a65b2b" . "I’ve Had It")
("c783b0b7-cbc4-3be6-89dc-a13883fe5c51" . "Jailhouse Rock")
("affbcc3b-0465-443b-b824-ab7eaf349e45" . "Jam Up")
("b089d13d-887d-39d6-9417-f30c37d9be2b" . "Jambalaya (On the Bayou)")
("4f7d358c-287a-40f0-9292-ca3c3d1a9509" . "Jenny Take a Ride")
("77b6d6e4-1cb4-47dc-876a-38c2bd445c33" . "Jenny, Jenny")
("8c806b4e-edb4-3101-bc58-2605ec309a2e" . "Jersey Girl")
("9ab3c6a7-e71a-3445-a74e-c52b2f3be283" . "Jingle Bell Rock")
("ca7bfba8-e5aa-3d25-9b4a-3b9d2626841d" . "Jingle Bells")
("f63244ad-5b8b-4254-9e17-5ffd4e591edf" . "Joe Hill")
("6671bffb-f9cc-3ea9-96e4-91cdc8d746a6" . "Johnny B. Goode")
("33142282-10e3-4b4b-92b5-cb2575a407b0" . "Johnny Come Lately")
("3089a18c-eb9e-479b-9e50-609d58a899b8" . "Jolé Blon")  ;; MusicBrainz calls it "Jole Blon"
("8531b357-339e-3cc7-9ed2-0d6b928ed12e" . "Joy to the World")
("fb0fde73-afdb-38fb-8e51-e97605f23234" . "Joy")
("a311fecc-3d06-3a4e-8822-4019ba82387c" . "Jump")
("78269644-d318-3092-9a3b-0c49c3d23d46" . "Jumpin’ Jack Flash")
("2dafe108-1a88-486c-a0f0-c8d2c525007f" . "Junior’s Bar")
("429348fe-30ef-4676-b7ac-63501e4bc7da" . "Just Like Fire Would")
("5f83fb2b-fd0d-3cf7-b37c-a9e70d09be20" . "Just Like a Woman")
("30db4ac2-72ab-4b7d-bdcf-5fa44b7e60d5" . "Just to Be With You")
("95a395d2-a515-323e-a800-2408488d5ff5" . "Kansas City / Hey‐Hey‐Hey‐Hey!")  ;; MusicBrainz calls it "Medley: Kansas City / Hey‐Hey‐Hey‐Hey!"
("6a79f7b5-771c-3897-9a28-46e139bfef90" . "Kansas City")
("705565fe-c0c7-472f-a98f-54b02836e873" . "Keep a Knockin’")
("165f5a46-0e5d-3525-9fd7-b7935e172b23" . "Keep the Car Running")
("77bcce18-a785-405d-af00-e1ed36212d9b" . "Keep the Faith") ;; JUST: Jon Bon Jovi - Probably, according to Steve from BruceBase
("d506d1f1-a789-3b41-ab4d-ee92f2bf58b8" . "Kentucky Woman")
("9b8ab63e-190a-326f-a011-89bfed2dc73f" . "Key to the Highway")
("1cb7269f-5d40-35e3-bff9-5dd429fd5f04" . "King Tut")
("74c8051c-1a44-3887-9478-44d3e5c8ddd1" . "King of Pain")
("58b02872-54a6-3b00-b50c-31422c32a302" . "Knock on Wood")
("4195adde-cfcf-3cc5-ab3f-405a9fdca8a9" . "Knockin’ on Heaven’s Door")
("017bcda3-4391-4179-a9fb-e6d4b82f75e7" . "Labor of Love")
("d697fc2b-f106-4097-b183-3a887c63c9ac" . "Land")
("958377e9-29c0-4b49-a6a2-9b7b10840b3b" . "Let It Be Me")
("ef5b9074-84d2-3e46-81ba-cdbe57898033" . "Let It Be")
("9b76a431-285c-3c40-8bab-a8e4d3de5bd1" . "Let It Bleed")
("761fc17c-b692-3c9f-b1d9-cc7105800463" . "Let the Four Winds Blow")
("8206e800-cfca-4b51-b74c-0ab618ea2f1c" . "Let the Good Times Roll")  ;; probably
("5c4b88cc-b558-30af-9776-95b1ac555a1b" . "Let’s Have a Party")
("b2744a5a-e81f-4ea0-a8b3-bbabd1e01ff2" . "Let’s Live for Today")
("856f2807-d1db-468b-b286-41dae2f43251" . "Let’s Twist Again")
("b0cc1d40-5b4e-4fda-b901-a9609029846a" . "Lighten Up")
("4f3cc611-009f-3fa9-a47c-a27d29ef0513" . "Like a Rolling Stone")
("544f855a-ba5e-4085-9556-b5cf4c70e27a" . "Linda Paloma")
("d7600c4a-e7b8-49fd-b877-99bb99cff187" . "Little Bit O’ Soul")
("fe769284-3e8e-4038-bc41-a0a67bbc5d18" . "Little Latin Lupe Lu")
("3432360d-ffca-36cf-87fe-263f0b86f11b" . "Little Queenie")
("8d10fe8b-e86a-338f-944d-710b41fb017e" . "Little Red Rooster")   ;; It's called "The Red Rooster" on MB
("3f84d22c-8377-4e01-a3a3-c7586e318a46" . "Little Sister")
("0b948ee4-1766-4fc2-9035-e4d2b99cd85e" . "Little by Little")
("623dbe1a-9850-336c-80ac-44b3d8638e64" . "Livin’ on a Prayer")   ;; JUST: Jon Bon Jovi
("56cfe2e2-309c-3e1a-90bc-46180956faf4" . "Lola")
("55737362-0b16-31a2-8a10-b7a43a391b76" . "London Calling")
("b87c7ce0-2fe9-3426-8dd5-bd5db6bf22e8" . "Lonely Teardrops")
("08de2826-6161-31ac-8e96-b18494f23b38" . "Lonesome Train (on a Lonesome Track)")  ;; in BB called "Lonesome Train"
("525dd33c-5aba-49ff-b4f8-19cfb1b6370c" . "Lonesome Valley")
("412d3909-1f2c-30e6-b009-daba36723dbd" . "Long Black Veil")  ;; http://musicbrainz.org/work/412d3909-1f2c-30e6-b009-daba36723dbd - It's called "The Long Black Veil" on MusicBrainz
("4679e271-b7c1-3331-882e-2a173f2001e4" . "Long Tall Sally")
("f8d4bf80-af18-49a9-93d6-f9ead96f44ae" . "Look Into Your Heart")
("a10a990b-476b-3574-ae17-9ded9ffb5825" . "Look Through Any Window")
("d09c6013-caa7-357a-946a-bae2c124e942" . "Louie Louie")
("6a5d091a-cc15-41f2-ac59-50352dbb4ac2" . "Love (Stand Up)")
("c6face18-15c3-3000-a78b-0f9828276a07" . "Love Me Tender")
("def9e880-3248-315f-84e1-52eef186f640" . "Love Train")
("a2b7a6e7-2013-4b82-8dfe-8647680949f1" . "Love and Mercy")
("128da040-ae0a-3502-b255-c54ddcabb2c8" . "Love of the Common People")
("99edc495-dcd3-41b4-8051-d9589673d69d" . "Love’s Glory")
("30c8831b-8d2f-3c95-a969-c316cedc0fb8" . "Loving You")
("4ca8aa2d-b1b0-3473-8f0b-2a388054e947" . "Lucille")
("fba1139f-e605-39ef-9f96-732774078e4d" . "Lucky Day") ;; probably
("06a76d9b-86a1-483a-ac3d-f30910d09a2c" . "Lucky Girl")
("8f3df6f8-d3f6-335a-9d42-ee6a936430a4" . "Macho Man")
("b85bbbc9-54f2-3aab-8b27-fb1d510c6e15" . "Mack the Knife")
("2b7e659c-5ff0-306d-a150-40116cb24fa9" . "Mama Don’t Allow No Guitar Picking")  ;; It's called "Mama Don’t Allow It" on MusicBrainz
("cee44845-f008-4542-bb88-2d0f6717fe0d" . "Man on the Moon")
("25b46bac-b4e5-42b7-a167-0a5de64ba2bc" . "Manifiesto")
("4e865631-b72a-455d-b97d-f460cc35e629" . "Many Rivers to Cross")
("e55aa84c-af08-484e-acc0-528dd0148a3a" . "May I")
("14c60955-0e11-3db1-9753-a9482a0eb01d" . "Maybellene")
("41188891-81da-3ec6-8b37-137ae2b13833" . "Mean Woman Blues")
("7f9e3429-c240-499d-9167-e21b56709b10" . "Meet Me at the End of the World")
("944b1865-8949-4785-a58c-7e315b80c2d6" . "Meeting in the Town Tonight")
("0b350225-9ddb-3c0a-aff9-26536778f85b" . "Memphis, Tennessee")
("bb13d275-a6ba-4407-b8ed-a7d3b6d710fd" . "Men Without Women")
("151aceac-3595-38e0-9341-e583279c13ae" . "Mercy, Mercy")  ;; in BB called "Mercy Mercy"
("28dbce3e-2bfc-42f7-a487-6e425d394566" . "Merry Christmas (I Don’t Want to Fight Tonight)")
("c76301db-6cc7-33e1-8b53-09e5ef25fe55" . "Merry Christmas Baby")
("12a8bda9-7daf-4b79-a4cc-0954fb9e0f8f" . "Midnight Special")
("721f7a04-f548-4107-9bb8-880f5274a284" . "Midnight Train")
("b12ea1dd-cb0c-411d-98e0-1ff53256f6ab" . "Millworker")
("7c4c825e-2581-4d97-9faa-4d5ee94547ee" . "Misery Loves Company")
("2e0de115-e833-3b42-8fba-b20314b0284f" . "Mona")
("a50f2637-33a5-3e1f-a375-101460bea6a5" . "Money (That’s What I Want)")
("92ee5d61-5265-384f-8317-ac5432ae8e37" . "Monster Mash")
("5b021efa-bafc-3b20-a747-da5c090dbc90" . "Mony Mony")
("7a3d9f3e-8697-4010-8a59-798cab00a232" . "Moon River")
("6b5bbcb6-2b75-3dc3-81e1-d42f21457608" . "Moonglow")
("c310f403-aa8b-3134-9621-955509606902" . "Morning Dew")
("92873aa3-ee11-4bd0-938d-f983a788ef5f" . "Mother‐In‐Law")
("d5ac34ca-24a9-3c79-bdf6-59af5f3fa8b6" . "Mountain of Love")
("e2dc7297-3db2-35c8-8f88-3a1a6d870eb1" . "Move on Up")
("7cd4ee03-20c8-4cad-bfdc-315e46a90353" . "Mr. Spaceman")
("034767af-af7c-3143-b2e7-4e23d9aeb1af" . "Mr. Tambourine Man")
("0dd91858-8b22-328c-a129-d37690906cb0" . "Mustang Sally")
("f0373dec-cdd9-3223-92f4-f06e6124fe90" . "My Babe") ;; probably
("b85b3a85-b955-3a99-99e8-168027489954" . "My Back Pages")
("9310b47b-c0a2-4114-b023-ac133c08b984" . "My Car Was Invisible (Case of Bruce Springsteen)")
("276cb46e-f9f2-359d-822e-05eca64b78bf" . "My Ding‐A‐Ling")
("a55b5d5b-cc47-304c-98c1-460fe91e1cd7" . "My Generation")
("567803bd-ff98-3b18-adc4-891e0a22521f" . "My Girl Josephine")
("e8f22db4-6ee7-3c9c-a3ac-84d0f9215a1b" . "My Girl")
("8046fc58-ecb9-4de7-a3a8-298d9cc393e3" . "My Kind of Town")
("60f7484a-f8c4-3576-8f9a-5f28524e6fdf" . "My Oklahoma Home")
("86e57562-9b58-4720-b03a-29ea995be03b" . "My Old Kentucky Home")
("722e7cfd-699c-4158-bd43-3da00dbd2fe9" . "My Ride’s Here")
("b6c7dd2e-7024-3b91-9a37-a3b04bdf1376" . "My Sweet Lord")
("03c47757-95e1-384b-9d11-fa7c2d459512" . "Mystery Train")
("8ff87281-8fda-4585-8db2-f40384c42f84" . "Mystic Eyes")
("4d482d18-19e4-3305-9783-d1e3f8f71157" . "Nadine (Is It You?)")
("def63ae6-0999-4783-82ad-1871bde26910" . "Native American")
("c912a864-2361-3a24-bd5d-5287adfb5fe6" . "Needles and Pins")
("ef2bc544-4701-42d7-9a4e-218b7fd4f462" . "Never Be Enough Time")
("40785ac7-84e9-347a-8001-e028c5c1bae7" . "Never on Sunday")
("6d1f6aa7-87d0-3da4-8e57-fd8cb8cef8eb" . "New Orleans")
("51c7254b-041d-490e-922b-8bb9a15243cd" . "New York Skyline")
("4f97bc50-6762-3cf5-97f7-4bba46e84c03" . "New York State of Mind")
("469090c0-7d0b-3caa-97d7-23f72487d920" . "New York, New York")  ;; Is called "Theme From New York, New York" at MB
("38c1ef81-7284-3203-9aeb-b02c0068d93c" . "Nick of Time")
("d5d4ef8f-331f-45f8-a01f-b331c67ee13a" . "Night Train")
("ba552dad-f9e4-3646-b34e-5e7790876a6b" . "Nightshift")
("0e1160d3-809a-312f-b9c4-b5e06ac328f3" . "Ninety‐Nine and a Half (Won’t Do)")
("985ebaa2-3a2f-41f4-8fd4-703fce1fadcd" . "No Money Down")
("29263fc3-22ef-36ff-a6bf-4c80b9be4c0c" . "No Particular Place to Go")
("d7a1d8b7-2101-4bf8-a8fb-780e7beae341" . "No Strings Attached")
("6394bda2-57a6-3578-badc-f3043d4f0ba8" . "Not Fade Away")
("8538c706-9f7a-41f1-9188-06dd89780ebd" . "Not Too Young to Get Married")
("934442b9-292f-4622-8764-69a509452bdc" . "Nothing’s Too Good for My Baby")
("57f86d53-3b74-3446-9bf4-4b73c678700d" . "O Come, All Ye Faithful")
("2fdaf246-808c-302d-83de-15eb6f7ae457" . "Oh, Boy!")
("73a3f4b3-29c7-389a-a778-43cccd7be945" . "Oh, Pretty Woman")
("61f7d80a-7e3b-459c-af2a-df471c682c01" . "Oh, What a Beautiful Mornin’")
("ed8f374c-de22-3f6b-a920-8e3c7ca93c63" . "Oklahoma Hills")
("3b3bd804-95f2-3494-9c20-4980d6c117ff" . "Old Time Rock & Roll")  ;; in BB called "Old Time Rock and Roll"
("2635a253-6743-4c0c-bc6c-3536fff546f4" . "Omaha")
("c5621395-2f7d-4a3c-8609-1e52db012ef0" . "On Top of Old Smoky")
("8c12be96-3090-4b9f-b920-e199bbaa9e0d" . "One Guitar")
("d6bf4f51-0352-3a44-8cb1-e9e8071c787b" . "One Headlight")
("36ff6f1f-b063-3c35-8abc-1fb667aecb8e" . "One Love / People Get Ready")
("5c971129-c3f2-3ffd-8419-45aead8c0fcf" . "One More Try")
("2d1f1032-dd50-4b05-892a-5cbe3b0f596c" . "One by One")
("ccf2f4e8-7931-34dd-b571-646567915812" . "One of Us Must Know (Sooner or Later)")
("dc345301-bfc7-4295-96bc-410442f6fc77" . "Only Lovers Left Alive")
("f4b5be3d-535c-4f53-b5e0-a4b55879b401" . "Only You Know and I Know")  ;; probably
("15c626d5-78d5-3fa7-8bfc-2d9e254eeec2" . "Only the Good Die Young")
("a7e8fc9e-f457-370c-ab68-4b9d336cdb91" . "Only the Lonely")
("43eb2b0a-88e9-465c-9de3-9b83bca6f684" . "Only the Strong Survive")
("c4e9c110-e548-37e9-8961-8d762797edf3" . "Ooby Dooby")
("af7a0bcd-cbe4-4379-ab00-15a25eca64ce" . "Our Lady of the Well")
("005a38cd-7f7d-4cc0-8641-d30de8b45ee1" . "Out of Limits")
("5effc6ee-0524-3cf2-8ee8-94ecafc3e105" . "Outside Woman Blues")
("95b0b9b2-79af-4ac4-8cbc-f9be64ad7049" . "Paralyzed")
("e2ebbad2-7eb6-4a5e-828a-6b52eba45cec" . "Party Lights")
("fc5221c5-0d83-4112-bd7a-3e991f252989" . "Peaceful Easy Feeling")
("44b41cfa-2f97-37c5-a6aa-527c99d44d46" . "People Get Ready")
("d4953b00-6246-4f50-a914-c4a257df16eb" . "People Have the Power")
("ae6cbb49-a9d7-404d-956a-662183f3400d" . "Peppermint Twist")
("b479cbf9-87fc-41a4-94bf-d41a85b5440d" . "Permanent Vacation")
("943ce2ed-91b7-33f6-b639-2fa232980920" . "Personality Crisis")
("d6764c7a-4eba-4bbc-992c-8368a04cad35" . "Photograph")
("c4093cf3-eda8-414d-ac18-b695bb72731a" . "Picture Hanging Blues")
("6ef8cd26-d0a7-36a1-84f5-5d872364d7c6" . "Pinball Wizard")
("e6ed6d51-e157-405b-a4e1-e553e9ccb2e7" . "Pink Houses")
("c33ef12f-8cf9-4a6a-a5ff-ecd17bb019bd" . "Plane Wreck at Los Gatos (Deportee)")  ;; It's called "Deportee (Plane Wreck at Los Gatos)" at MB, Bruce uses the below version
("ea934515-6f2a-332f-946e-81289f5d3dd5" . "Play With Fire")
("c4bb79ce-b942-3939-bfb1-9b57cc74ea5b" . "Please Please Me")
("a648675a-bd42-40f5-a77b-0f01d86a7fa0" . "Politician")
("054b31fa-ce10-4376-adf1-9b31359646cb" . "Powerhouse")
("b8beac43-701e-3a3b-8363-2bc095ec7539" . "Pretty Flamingo")
("cdf1f858-4903-41bb-9d13-258340e53b5a" . "Prison Grove")
("0b45a782-6a30-38fb-9087-77ed40f92d60" . "Promised Land")
("19be8846-a857-3f42-a9c6-e4984d4ef474" . "Proud Mary")
("f3cbe2e2-7375-4c14-a69a-cdbd5dd30988" . "Pumping Iron")
("a0315950-33b7-35b0-a0ca-42f1ad80b213" . "Purple Haze")
("beab7a01-eb43-3da1-86cf-127de38580a8" . "Purple Rain")
("5d6f6aa9-9f4d-4d71-904f-9980d603228e" . "Quarter to Three")
("7c34630a-869f-3572-866e-31f86fc00753" . "Que Sera, Sera")  ;; It's called "Que Será, Será" at MB
("1a5b6395-fe35-438f-8eff-89add4c43414" . "Queen of the Underworld")
("90e72c0c-5114-430b-a10f-c78ac71b81a4" . "R.O.C.K.")
("19d20c0b-e1c9-3d53-b6dd-e50b9d30929a" . "Radio Radio")  ;; It's called "Radio, Radio" at MB
("105224a2-0346-48b4-91cd-b99c24db63af" . "Radio Silence")
("c7b4aa82-4d81-4c7f-893f-ea8b91b437a1" . "Rainin’ in My Heart")
("96e9e5c2-0f54-3b8e-bd40-ad81d3378c83" . "Rainy Day Women #12 & 35")
("4c29e9ca-caf6-327f-b857-2c7bfd43f16e" . "Raise Your Hand")
("3cec167a-2b7d-4843-81e6-8b3eea13840f" . "Ramblin’ Gamblin’ Man")
("29ee549c-9036-46ff-9398-30506091b48c" . "Raunchy")
("918e5e60-be57-373f-bb6f-8259b3476754" . "Rave On")
("0135dd36-dde3-325b-8394-0a9faf6aa653" . "Ready Teddy")
("37500a15-11a1-4cf1-b4c2-797e2817b2f5" . "Rebel Music")
("dc6d909d-a39f-3932-a194-19703564743e" . "Rebel Rebel")
("9f22a383-7a11-3e05-b858-b6c2341d1cf0" . "Reconsider Baby")
("51a2e4ec-c9de-327a-a042-4d7f77185e7c" . "Redemption Day")
("20772796-ac5e-36fd-b247-7bee69aba5bc" . "Reelin’ and Rockin’")
("ec59051f-4a18-4da0-8b1c-65d32b4232db" . "Remember When the Music")
("ffd510c1-ebe5-447f-9bc1-b4731e4841df" . "Reservation Girl")
("62cd9af9-435f-3632-b885-f7d4685211a4" . "Respect")
("287d163c-1b99-3b42-8617-98e9ae690b69" . "Rhinestone Cowboy")
("ce051738-51f6-4f1d-9ac0-29d82457a95f" . "Ride On, Baby")
("f722f074-1cac-3cb8-800a-faff575da4d4" . "Ride Your Pony")
("15608aeb-5d33-3728-bfd5-440ea6a9a636" . "Riding in My Car")
("5ef73295-f861-335f-9034-c563cefa7a96" . "Right Place Wrong Time")  ;; Is called "Right Place, Wrong Time" at MB
("b00cccfb-75f1-371b-9181-8d3ed0723114" . "Ring of Fire")
("36db83df-5ca8-359d-af39-03ddab0bd1bb" . "Rip It Up")
("87f13b50-8e1e-354f-8716-befd81b4d437" . "River Deep – Mountain High")  ;; Is called "River Deep, Mountain High" at MB
("203ebaab-f8e8-483f-8032-9ed75d0e976f" . "Rock Baby Rock")  ;; I changed it at MB from "Rock, Baby, Rock"
("2dfdba5b-380b-4179-a112-822452771017" . "Rock Ballad")
("373790c8-380d-3236-b591-837a40336595" . "Rock Me Baby")
("dc896a9f-3844-3437-b436-fd0fd6aa8848" . "Rock and Roll Music")
("5b31460f-26a9-3e90-b0a9-6adcf8d1fba9" . "Rockin’ All Over the World")
("ef4f1a6d-2cf7-39f4-889d-7911f4e7f725" . "Rockin’ Robin")
("a398c48d-dfa7-32ae-9f19-04140d39fd9e" . "Rockin’ in the Free World")
("2c010641-988e-36cf-a03d-ed3a8c0a98e3" . "Rocky Mountain Way")
("3d55b4e1-36f5-3cec-b702-59d1b534611e" . "Roll Over Beethoven")
("fdb7ef6f-d2e3-4641-a612-d88fc3b068a6" . "Rose Tattoo")
("b9c0a1ed-353c-4e86-85ae-81a9e1caa2bf" . "Rose")
("b44e94f0-c5c7-4805-a593-c7d9fa696d71" . "Royals")
("04db3cb9-af4e-35a9-8709-54d63ef23f50" . "Ruby Baby")
("d1eda0c3-cf14-450b-b52c-878fa500bb51" . "Rumble Doll")
("eed200ad-828d-3d63-ad31-cfa213b95879" . "Rumble")
("a686cf82-c7c3-4032-9001-c08a0ea4c77a" . "Run Rudolph Run")
("f719cf17-53dc-3e48-a642-bd86c82c524c" . "Run Through the Jungle")
("d9dfbafa-f036-4cc9-8b60-23ac0c4d7f83" . "Run, Shaker Life")
("21017d9d-584a-30b6-9ba3-d94f9e662f4b" . "Running Scared")
("60c258e4-7ed8-4628-b3bf-f1649ee3ded3" . "Running on Empty")
("89863c39-34aa-3478-a8ac-b07c17258da3" . "Samson and Delilah")  ;; FIX in Musicbrainz
("39938bc7-7127-4d02-9385-e92fa8826007" . "San Franciscan Nights")
("b3e49473-899d-3f08-9f06-2afc70f85e2e" . "Santa Claus Is Comin’ to Town")
("fbbc3a31-711f-4b39-89f5-ee82b3ee33fc" . "Santa Claus, Santa Claus")
("4dfd1cd9-1f5d-3c6b-a63f-01ffe3f98bed" . "Satan’s Jeweled Crown")
("410b73b1-64bb-3399-b859-4e582a15242a" . "Satin Doll")
("351881ac-467c-3016-9954-2f70c7f0cc9a" . "Save the Last Dance for Me")
("33423f73-544b-40cf-b717-b5efce7fddcf" . "Say Goodbye to Hollywood")
("0cda5e19-8488-35c0-a3c8-e9a86d30ebcd" . "School Day (Ring! Ring! Goes the Bell)")
("78b0485f-5b71-4c2d-9338-efebd84eb7f2" . "School Is In")
("ff418f1b-64e3-4aeb-89a6-600016d36108" . "School Is Out")
("a2933279-5d9a-4254-8e80-3f18f91844b6" . "Sea Cruise")
("1c7ddf09-c671-3748-86a1-54572973c3ee" . "Sea of Heartbreak")
("11cc6166-bdc6-4c7c-9bbf-bc1df3fa83db" . "Searching for My Soul")
("3db48653-2a9c-3b54-9acb-54764d60c31b" . "Secret Agent Man")
("10407786-24a6-32a7-9fab-67d1c749a395" . "See My Friends")
("a7c8aa11-e064-3767-8e2b-975153d851e3" . "See See Rider Blues")
("2a2d2b64-fd9d-4823-b44c-865d638495ec" . "Sentimental Journey")
("f4ec1404-e759-45ac-ac3f-d8fdc2a5034b" . "Settle for Love")
("9a50dd49-3e54-34aa-a6d1-138ea3869a45" . "Seven Nights to Rock")
("d336015b-2f2a-429d-b41f-40c1e898fc36" . "Seventh Son")  ;; It's called "The Seventh Son" at MB
("511eb2eb-2b4b-4321-b021-62306879d71a" . "Sexy and I Know It")
("5740e2cf-979d-32ca-8ff3-7036e0bf4cef" . "Shake")
("987fa912-ca9e-3045-bd3d-cabe39c8e8bb" . "Shake, Rattle and Roll")
("27b356f5-bd8c-3d43-ac27-af1184badf85" . "Shapes of Things")
("53838f9a-6d1a-3dc3-bb15-40f80c4510e7" . "Sha‐La‐La")
("902518b7-afc3-46c2-9e2f-4d36d551e558" . "She Got Me Where She Wants Me")
("743a57f0-95ff-3e0d-a46e-4f094362a339" . "She Loves You")
("4410ae4f-ba55-4f05-a8a0-cc7f1703500a" . "She’s Cool")
("6f704aab-3373-3066-8cc7-7defae0d52e0" . "She’s Not There")
("f6fce5ac-63be-4c79-9d0a-d60e6e0ebaf2" . "Shine Silently")
("69ae3f5f-be4f-41cf-a6b7-ad381970984b" . "Shotgun")
("269038cb-2cde-382a-8491-988fab28cf5e" . "Shout")
("49cdfb88-8303-306b-94f0-693cf6537494" . "Signed, Sealed, Delivered I’m Yours")
("353c2166-2956-40a1-9bf5-6d699f86f127" . "Sittin’ on Top of the World")
("2b302edb-8a84-3079-a4b3-6b7424ff9dd8" . "Six Days on the Road")
("82d02e2b-57e8-4a6a-ab42-5e80fa4e8796" . "Slow Blues in G")  ;; [unknown]
("7a80f45e-3fbb-42c1-82ba-21125f0821a1" . "Slow Blues")  ;; [unknown]
("7d2ec07b-9c83-38e6-beb6-9ceea274fa0a" . "Slow Down")
("ff19d5ae-c512-44cf-bebc-2e0112cd7da8" . "Slow Turning")
("721e367b-0a04-42dc-90f9-4b34c6b46e8d" . "Slowly Walking Down the River")
("10d2a48d-1248-316e-b8a5-4d5ba80165c8" . "Smokestack Lightning")
("efdf7ca3-b295-47f7-a8fc-0b776e6d635e" . "Sociedade alternativa")
("ca347f00-aeac-4d30-ac5e-770a5a1742d6" . "Sock It to Me, Baby!")  ;; JUST: Jon Bon Jovi and Southside Johnny
("bbefccb9-ca8d-3f31-8e1b-d097bc2bcac8" . "Someday I’ll Be Saturday Night")
("1e1e46dc-677c-39d3-9c0c-43efef57359b" . "Someday We’ll Be Together")
("51ae43fb-1e9e-4fca-b337-f80168741167" . "Something About You")
("8af5ef71-985e-4500-9500-78a26e31876c" . "Something You Got")
("028542a9-b5d9-3e48-86f1-53e283b62d5e" . "Something")
("9109a984-655e-38cb-91a9-cc8c677f120d" . "Son of a Preacher Man")
("e46684d1-e9d5-456e-9b8e-340fbbe3e0e8" . "Soothe Me")
("a0165ecd-0cd2-4019-8e42-7a1d6d863d3c" . "Soul Days")
("8170840f-33e1-4d39-9a03-a961724ab42b" . "Soul Deep")
("69a1d8eb-395a-37b4-b018-aca653055ec0" . "Soul Man")
("01434199-5e71-4c36-b17c-cd6f9f7282f1" . "South Street")  ;; probably
("3695b255-f4d8-41e2-b227-451058d29c1a" . "Spanish Dancer")
("964e8847-41f2-3298-af96-f759b4930d71" . "Spanish Harlem")
("e68d22a4-ce72-49bf-96c6-a7b9a6fe378d" . "Spider Fingers")
("2617e21d-fb73-3260-a171-0425eb8333f1" . "Spill the Wine")
("ac60f72b-c37a-3724-91b2-6184ceea1f77" . "Spoonful")
("d8432ea4-1b3d-32e9-a36a-bc20b7a5b527" . "Stagger Lee")
("54d6831c-4447-3087-a71e-3f92aa1fad27" . "Stand by Me")
("b669dcf5-adcf-3d99-8807-fa1c141cf27e" . "Start Me Up")
("ee1138d1-eb21-3212-9839-9558e6056b95" . "Stay")
("6113d16a-8d2e-3e54-adc2-bacaeb0058be" . "Stayin’ Alive")
("e1beb4f1-33a2-45f8-a521-71b34cb38f39" . "Ste. Genevieve")
("e6ff6efd-26f6-4390-904a-0fbdc7121017" . "Stems & Seeds")  ;; in BB called "Stems and Seeds"
("e5fff3f6-0854-3318-b5a6-0be14b99b635" . "Stranger on the Shore")
("92c2df3a-2c67-34ac-a727-4887e39f84cc" . "Street Fighting Man")
("20874b52-9be9-30b5-8604-a860d53cabc3" . "Street Hassle")
("26dea81f-7fef-31fa-84ef-09699f77872b" . "Substitute")
("f9edb627-648a-4613-a02a-2c31be05c6df" . "Sufferin’ in the Land")
("3818fa67-c508-3499-9d18-0fd9a2b443ce" . "Sugar, Sugar")
("abf54062-a8aa-31b2-96a5-a648a37f81b6" . "Summertime Blues")
("986359d8-34ff-3fe2-84fe-155b5f167975" . "Summertime")  ;; probably
("28130793-61bc-4ca1-8e6f-a6ecb5a4f1ff" . "Sun City")
("b5bbb92f-770b-3f5c-9dde-e3f332fdaa84" . "Sunshine of Your Love")
("90a7e5b0-3711-35d5-b499-a90c62935a2b" . "Suspicious Minds")
("87589fa7-1924-3089-932d-f6c3dc113937" . "Suzanne")
("a0b8ea3c-9705-3507-927d-835eef5651d3" . "Sweet Baby James")
("3eadade3-65f2-3c6e-9cf5-478718b7881e" . "Sweet Little Rock and Roller")
("7050e65f-66f8-31ee-be38-61ddfb93a60a" . "Sweet Little Sixteen")
("7a4e4a26-acd7-45e4-bbc9-34875404daaa" . "Sweet Soul Music")
("555d4446-8230-34ce-8b09-606109ac2f84" . "Sweet Virginia")
("b1ce5c3b-a348-35ab-9970-efe0890da13b" . "Sólo le pido a Dios")
("2a81e797-8848-3a18-9390-9d6b235ce49c" . "Take It Easy")
("48f499bf-5fb6-4b8a-838d-5722ef77e169" . "Take Me Out to the Ball Game")
("4bad1a62-f57a-36f7-a985-165ec971cc71" . "Take Me to the River")
("d55fe786-2ab4-405c-ab73-070c701e1ad1" . "Take Out Some Insurance")
("b869d46b-ab4f-4cfa-ae05-4bde5cac916f" . "Take a Look at My Heart")
("d5a54cf2-71b0-43b3-b321-a3e7e5b1ed43" . "Talk Show")
("343b0f45-44fd-44e7-bfd7-d6f71a519f0c" . "Talk to Me Like the Rain")
("3d04d7cd-96e6-4617-b235-09870e5efc51" . "Talking to the King")
("4a3abff8-4604-3c9c-88c7-55762e11eacd" . "Tallahassee Lassie")
("d88e0e5c-51ae-4779-a20b-de00c0556b16" . "Taps")  ;; probably
("ea98da58-df7c-3bbf-8b73-9240ec3272c7" . "Teach Your Children")
("3005488d-0d7e-3299-9a73-e2b086fbb844" . "Tell Him")
("8423eef7-88aa-39b0-9c4f-11a2a113fdc6" . "Tell Me Why")
("8e40b2ce-41d4-33a6-97ea-5d1f922f91c0" . "Tell Me")  ;; probably - alias "Tell Me (You’re Coming Back)"
("db2198ee-8017-3f01-9faa-80f47bf84ecf" . "Tequila")
("fb8ec3c5-217f-4c4f-8755-bbf556b93ca1" . "Texas Blues Rocker")
("07de0f42-3f91-36a2-9bbe-f0d3edb0beed" . "The Ballad of Jed Clampett")
("3356f5c8-ddf1-474d-ad2c-cf629ae4eb4c" . "The Blues Is My Business")
("d45cd2ac-52d2-3469-b9c3-985dc15523ef" . "The Boxer")
("3e91895c-2ae4-4b9b-89ca-40ad23239c92" . "The Boy From New York City")
("bf0ba727-a331-46fe-8549-589305078a54" . "The Daring Young Man on the Flying Trapeze")
("46de0a04-473b-3a1d-bec8-a7f3656d2e4b" . "The Dark End of the Street")
("e228108e-4621-3b8b-9136-ac930dfd244b" . "The End of the Innocence")
("758a4781-726a-35b8-9fe6-a4ba175e07c6" . "The End")
("7aea1ec1-5d61-35ad-82d5-ff3b84f38bd2" . "The Harder They Come")
("d12cf1ae-7feb-3f04-88e9-c4d7df3ff754" . "The Last Time")
("ccb1d083-9111-3591-a30a-bac5eb964b28" . "The Letter")
("a675e216-0440-32c0-9b27-1353bfdfe79e" . "The Loco‐Motion")  ;; in BB called "Loco‐Motion"
("1c747133-897d-4f93-9830-8cc785c15754" . "The Monkey Time")  ;; probably
("1579402b-b15f-4f08-ac4d-e1c4971159dd" . "The Morning: Another Morning")
("30475ee9-ac59-4c07-bc00-7e664dc752a3" . "The Night They Drove Old Dixie Down")
("93a50e2c-98d4-3305-9bc2-e16a6b9e09c5" . "The Patriot Game")
("12343b92-979c-4615-8a1b-8d4593794b0a" . "The Pretender")
("101e2771-d49f-3dfd-88a9-bf341caab8f7" . "The River of Dreams")  ;; in BB called "River of Dreams"
("d336015b-2f2a-429d-b41f-40c1e898fc36" . "The Seventh Son")
("92825287-3cc3-3dd4-8635-35fa457a45b8" . "The Star‐Spangled Banner")
("a2aa22dc-be22-3192-ab88-c6c2c7b452b2" . "The Sun Ain’t Gonna Shine Anymore")  ;; in MB called "The Sun Ain’t Gonna Shine (Anymore)"
("d000a7e7-81e8-350a-96c7-ec236fe9a736" . "The Tears of a Clown")
("033e7672-ed79-3d76-97c6-289049c19d7d" . "The Third Man Theme")
("57ce97eb-cccc-348b-a096-eea199d185bb" . "The Tracks of My Tears")
("084cdff8-1c04-4f7f-89ad-d9a6bbd4cf09" . "The Twist")  ;; probably
("19476812-4ec2-46d2-b0e9-c26a11a0d0c6" . "The Wanderer")
("ef334da9-4b23-3046-a321-cf93a75cd842" . "The War Drags On")
("44ddc0a7-a490-30b8-8282-10fb605aca9a" . "The Way You Do the Things You Do")
("cc132527-02e4-4210-994e-6431784a25e4" . "The Weight")
("e9cd96bc-42d5-4ad8-852c-ea8714f65abe" . "The Yellow Rose of Texas")
("69a5bf79-9a4c-3ec5-98f0-ce5529f85648" . "Them Changes")
("6664cc6e-d4b3-30c2-aebb-3c600f18ff6d" . "Theme From Shaft")
("efd6a971-c51a-398c-b294-ef23a9d5a8fd" . "Then He Kissed Me")
("efd6a971-c51a-398c-b294-ef23a9d5a8fd" . "Then She Kissed Me")  ;; Bruce's version of "Then He Kissed Me"
("0e456f3e-86b7-3a18-86dc-642964f8881c" . "There Goes My Baby")  ;; JUST: Southside Johnny
("ca8235c6-1891-4d31-b55e-49f51ba3e613" . "This Land Is Your Land")
("0434db34-b625-3b85-b9d6-93e25e812a98" . "This Little Light of Mine")
("88857226-ac73-424a-a27e-c84e8b86cac8" . "This Time Baby’s Gone for Good")
("a2bd3a97-4faa-4e9e-8aac-e8d4aa007137" . "This Time It’s for Real")
("552f3fee-d045-3ca7-9104-d58bc127ffb5" . "Till There Was You")
("2607534c-9a80-39c1-aa76-b5b7508ad45d" . "Time Is on My Side")
("90b946ac-42a6-4b19-bd74-3f1d3083e216" . "Time Will Tell")
("b63de918-74df-3710-ae15-4b29f10d2947" . "Tired of Waiting for You")
("aa6a6154-dcd5-319f-814e-a95d6e4e11cc" . "To Love Somebody")
("e8ddc911-2219-3133-9628-d79d30b8a037" . "Toad")
("7dd0f211-0ae9-368b-ac1a-179a9c27fc69" . "Tobacco Road")
("c4f0bd0a-342e-3071-ac3b-7e5e7f17f924" . "Toccata")  ;; It's called "Toccata und Fuge d-Moll, BWV 565" at MB
("fe4ff344-dfc7-3be1-b256-bc21d6f3ba2e" . "Tom Joad")
("6209d120-7966-49a9-80dd-737bb69b3464" . "Town Called Heartbreak")
("f34f0701-3633-46e4-93ef-5501fac6fccf" . "Tragedy")
("5d2eaffd-d6e3-4674-97ee-1104e032dbf0" . "Train Ride")
("273bcf19-9c43-3e6b-91df-020680ace117" . "Travelin’ Band")
("00dfb6a5-90bb-3bf6-910f-7c77600e01eb" . "Treat Her Right")
("bff653c4-a236-4ef5-8e3c-0cc6dca8a7fb" . "True Companion")
("d3c2d2b7-ed54-30be-8698-43ce75f5bffe" . "Tumbling Dice")
("bb522f7b-0f84-3838-bfee-0c5037012a1e" . "Tupelo Honey")
("9b9315cb-b83f-4f76-9590-5338b6e5f2e2" . "Turn Back the Hands of Time")
("62543bdc-ad24-4e23-b21f-0e19bd04b59b" . "Turn on Your Love Light")
("934300eb-61fd-38c7-bdce-64db245f4849" . "Turn! Turn! Turn! (To Everything There Is a Season)")
("c3fe1dc6-21aa-3d8d-9cde-df8524fbc83c" . "Tutti Frutti")
("6d0e4a44-8b1e-386f-bef5-ad8a9d0da0ac" . "Twenty Flight Rock")
("60f5b7d0-9014-4602-95bd-5b7d798799aa" . "Twenty‐Five Miles")
("7e2dedbb-5be0-32d4-8985-079d6df1e78b" . "Twist and Shout")
("87076226-0d42-4662-ac74-03cfafca745e" . "Twist, Twist Senora")  ;; in BB called "Twist, Twist, Senora"
("cb4b92b5-eaa7-3150-98e0-baa7984c70d9" . "Under My Thumb")
("334afb17-cc73-3167-83cb-1387cce1e809" . "Under the Boardwalk")
("32a361a4-f1db-4c4f-8676-3f4a86e2f158" . "Until the Good Is Gone")
("9c11abaa-2f64-386b-a702-2c263dc64173" . "Up on Cripple Creek")
("28a099b7-fd4c-3040-9d66-77a88cfb41c6" . "Up on the Roof")
("f5e8ea1f-4aa8-4b61-8e9e-4c6e300de858" . "Up to You")
("21482f32-0d9e-45b5-9415-e49ca5ae0332" . "Uptown")  ;; Is called "Up Town" on MB
("a84c2a8b-e20c-4fec-bfb9-2a19f684c583" . "Valentine")
("47329481-2c77-30e9-bd61-f433008fc30c" . "Valerie")
("5f7839e9-2781-4247-840a-d58c638a431b" . "Vigilante Man")
("e83dd4e2-8c90-3444-b508-4c69953d55bc" . "Voodoo Child (slight return)")  ;; in BB called "Voodoo Child"
("77a1daac-fd02-3fc1-aa20-9d6adb115b36" . "Wabash Cannonball")
("c0f78ecf-6f48-4b99-943d-c9cff9cb2257" . "Wake Me, Shake Me")
("1bda291c-6b32-4c80-b5cd-fdc4cea2fab7" . "Walk, Don’t Run")
("77ef140f-d24e-3859-9ba7-5b71e454da91" . "Walking in the Rain")
("bd50f03f-3dd3-325b-b11a-0d76c532b7b4" . "Walking the Dog")
("b5440fc9-2cca-47e3-8b6f-c4dd3c5529c3" . "Waltz Across Texas")
("898b0925-5bfa-393c-bfab-2ef93c1d552e" . "Wanted Dead or Alive")
("445e9912-2ef8-3d55-8784-cfc6f8806a41" . "War")
("08a84d85-b268-47e1-b3ad-9c481f3da188" . "Warm and Tender Love")
("3867bf11-b904-40cc-8f75-c618d23e52e7" . "Wasted Days")
("0b9a2e1d-361e-3f84-89c4-e996eb7ee2eb" . "We Are the World")
("050b2993-7704-33a1-83ea-00f4bac382e4" . "We Gotta Get Out of This Place")
("bc77d8f3-c4ca-3830-8a2c-a01cb88f8a7e" . "We Shall Overcome")
("6fbc76fa-9b8f-4e42-8b81-206da821a6e9" . "We Wanna Boogie")
("faaa9bdf-260a-3844-a85c-a03cb4bde0bd" . "Wear My Ring Around Your Neck")
("535c2339-2e0b-40ae-a98a-de8c6a217e73" . "Wedding Bells")
("be06dc55-8bb6-4c6d-9bd7-c4444fbcf89c" . "Wedding March")  ;; MB: "Ein Sommernachtstraum, op. 61 no. 7: Hochzeitsmarsch: Allegro vivace"
("53ecf364-56f9-4fc0-9ee0-2859d9401b02" . "Welcome to the World")
("eeaed3b9-6fb5-42a8-a88f-446a352154b5" . "Well May the World Go")
("1e4d268d-de49-47e3-a82b-3902b139ceca" . "Wendy")
("69557d5f-3aca-4e9f-bd23-a4cc4b53eb32" . "Werewolves of London")
("32ab3c7e-75e9-43a5-89ee-a17c4f90c1e6" . "We’re Gonna Have a Real Good Time Together")
("d4ff4976-d5a3-4f6c-9702-a10b2ff9bf26" . "We’ve Got the Love")
("050b2993-7704-33a1-83ea-00f4bac382e4" . "We’ve Gotta Get Out of This Place")
("bdfc888e-c6e4-3218-b048-063039be235c" . "What Becomes of the Brokenhearted")
("41497836-145d-4e8b-8196-cc92d459685a" . "What Did You Do in the War")
("4a5dd300-0880-3ecf-a740-25f93686b700" . "When I Grow Up (to Be a Man)")
("941dc0d5-382b-47af-9bc0-fc05c7ae541e" . "When I Leave Berlin")
("ee4cc9fe-fbdb-32b7-bab8-7ded940bd443" . "When She Was My Girl")
("4d156393-8cb1-356e-8be8-e257f7ac03dd" . "When Something Is Wrong With My Baby")
("15093d39-3263-3171-be31-564f12a223ac" . "When Will I Be Loved?")
("c20f15d0-9b17-3f44-9f51-aa1e15c2504c" . "When You Walk in the Room")
("2d8bdbe9-dc42-3ce5-81a6-1ab136ccf329" . "When the Saints Go Marching In")
("6d0b966b-4f31-393d-ae32-a07e477f7744" . "Where the Streets Have No Name")
("877cd0d2-3926-4ff3-85fc-60af64b6c514" . "Whip My Hair")
("30daa999-81af-34c3-bf22-b3c1c41c8c01" . "White Christmas")
("ca50d312-4956-4197-8a88-dc4ad880236b" . "White Lines")
("4f19a475-4607-31cc-aba9-390f5a007352" . "Who Do You Love?")
("36df44f5-7cad-37ee-9aae-89a5c58dfb07" . "Whole Lotta Love")
("f0cd0c3e-2657-303f-bbe4-05e8c597a328" . "Whole Lotta Shakin’ Goin’ On")
("59d022ee-d140-3bcd-b885-593c22e3f779" . "Who’ll Stop the Rain")
("0d4c9430-7751-3748-9a72-57280037d1b4" . "Wild Thing")
("c9b882de-b380-4a57-a4a6-6aded3bc863b" . "Wild in the Streets")
("78bf38b7-9a4b-356b-ac17-ca5c77d6c9ee" . "Will You Love Me Tomorrow")
("2340a544-f3b6-4db0-b81c-782864efb751" . "Will the Circle Be Unbroken?")
("8a42bd00-92e0-3658-b1bb-300872bd09b7" . "Willie and the Hand Jive")
("91a874b5-c628-3915-8ef6-9307655785e4" . "Winchester Cathedral")
("06ed8873-bc50-39e8-9ee7-2953a8a4ddb4" . "Wipe Out")
("8388f958-0fef-30ea-a70a-f1803868ecf9" . "With a Little Help From My Friends")
("f39cf04d-7032-42f1-91b4-828b5da5109c" . "Without Love")
("7215f333-bd0a-3e4d-b6a2-977846b1af35" . "Wonderful World")
("24d776f1-b093-37ea-98df-5d4c2a57e5d1" . "Wooly Bully")
("448ec304-912b-4966-bbd4-7a70c3797a81" . "Working on the Building")
("ab0eb39b-e6a0-4482-94bb-22f15bcca01c" . "You Can Dig It")
("067c5b0c-8427-42c5-89e0-b76778b9f2ab" . "You Can’t Go Back")
("2e5bff10-df66-4760-8df4-38a95d917d2a" . "You Can’t Sit Down")
("6437b14c-e951-3a75-a5a7-db60e1eee87b" . "You Give Love a Bad Name")
("9f09a5b8-710f-48fa-a2aa-d7d37c1a0168" . "You Know My Love")
("8dcb0877-ef0b-353d-8d86-e5329a3b2f90" . "You May Be Right")
("ade02f6a-dd14-35a8-be4c-69e134f70943" . "You Never Can Tell")
("d757ab79-30ea-3de7-9d69-c4cd2d89e630" . "You Really Got Me")
("5e40f193-793b-3c48-aed9-90a3bb428cc2" . "You Send Me")
("682a296b-0eb8-3d89-9e51-8c5f83f48e73" . "You Sexy Thing")
("a5d611a1-353c-395f-9a93-b2880a3292ed" . "You Shook Me All Night Long")
("8eb32448-6f63-4d48-849a-f5a0da08ef5c" . "You Turn Me On")
("6894beb6-a189-3303-9922-0a047143cbac" . "Your Cheatin’ Heart")
("5dfc1d6b-9d86-4a76-9a00-cb060c1e0bb7" . "You’re the One That Done It")
("286d48aa-6ef1-3b30-8f4a-4287a018a9a0" . "You’ve Lost That Lovin’ Feelin’")
("e39be679-5a1e-3ddb-9f23-c469783a54ea" . "You’ve Really Got a Hold on Me")
("07a0f679-d35a-4400-99e3-e655c1f1fe27" . "Yum Yum Yum (I Want Some)")
("30ff5576-d0c3-4d5b-83c5-8f5e344fb7d9" . "chinatown")
("93a80540-9011-3969-8e2f-2708c060ab5d" . "’O sole mio")
))

;; Sorting:  sort-regexp-fields  '^.*$' ' ".*'

;; Brucebase works
(defvar mb-brucebase-works '(
("10112466-be5e-4e61-b277-f40800926696" . "(Today I Met) the Boy I’m Gonna Marry")
("78282243-c9f9-352d-a2a1-36c8567a05bc" . "96 Tears")
("c6b4dbfd-233c-422b-ae85-cf775cef9d89" . "A Good Life")
("9a55593e-1419-3aa7-8281-1ffc0bcb0ace" . "A Hard Day’s Night")
("64542129-e9c0-4111-8aea-553775661553" . "A Matter of Trust")
("9ff3f54e-ea6c-3a57-a928-22f0b3087db7" . "A Teenager in Love")
("f57738cd-9272-4267-a44b-e473d734042f" . "A Woman’s Got the Power")
("a3624a08-6612-3bd8-bdf8-fe8a21f8f0e4" . "Ain’t That Peculiar")
("f1055da9-a5a8-3b74-a836-ee809dc038ef" . "Ain’t That a Shame")
("e150c6db-6134-45be-94a4-3f8cb8cc2549" . "Baby I’m a Star")
("e1aa9ce8-f176-3267-9bbc-c610cab3a169" . "Baby What You Want Me to Do")
("bf2081f8-3714-3b4f-be55-9842f6d7a526" . "Barefootin’")
("80dc89b7-1c29-3d38-8f2d-8168b5189a30" . "Be True to Your School")
("1ef454c8-41a6-3f53-8831-f07ae5e75184" . "Be‐Bop‐a‐Lula")
("4558d24c-6d17-39c8-ab54-6b74d754a632" . "Can’t Stand Losing You")
("5df81ab1-ba72-4c7c-86a3-20f9cef93566" . "Carnival Song")
("ea5edc10-c74a-4fa5-add4-96d120efba8e" . "Come On, Let’s Go")
("65dbcf9d-5856-3585-bc69-b879e95d3957" . "Confessin’ the Blues")
("e16301f8-5554-3415-b3e7-a2e9b39953ac" . "Cotton Fields")
("d535200c-826e-4754-a508-690684c6886c" . "Crown Liquor")
("e6f5680f-874c-3ae9-affe-608db61ad4a7" . "Do You Remember Rock ’n’ Roll Radio?")
("05f56ef6-7db3-3275-9df8-65c200cc35fa" . "Don’t Be Cruel")
("7b2c1ff4-50cc-3456-b3b5-8e17c1bf3475" . "Don’t Stop Believin’")
("d644a1dd-3f19-358c-a355-80f74a6ee095" . "Double Shot (of My Baby’s Love)")
("3f3c6b84-5980-35d9-9930-dc78bd9172c5" . "Everybody’s Trying to Be My Baby")
("8ced91db-f78d-48d9-a469-08c326fba726" . "Fly Against the Wind")
("fede7c1a-1fb6-3129-b8b2-e597c979c104" . "For What It’s Worth")
("20fd2c3f-d28d-3b00-9206-4ea43c0d9d00" . "Freddie’s Dead")
("084e1720-4a6a-304f-9a6d-9e3d36629e04" . "God Don’t Make Lonely Girls")
("51b53c20-961a-3aea-8dcf-c5357d4c042e" . "Going Home (Theme From Local Hero)")  ;; Is called "Going Home: Theme of the Local Hero" on MB
("0c2d9eb6-1c6c-407f-98b6-61b40c7d68b5" . "Hail Hail Rock ’n’ Roll")
("c04fdab6-f1e2-3972-ace2-08f195ca85a4" . "Hey Joe")
("208766c1-4a7f-3fee-b236-660432c46783" . "He’s a Rebel")
("db608bf6-ee71-3fc8-981f-ef7295540176" . "I Ain’t Ever Satisfied")
("9e9b8564-c491-31cf-94c0-8b7db4feb58c" . "I Can’t Stand Up for Falling Down")
("62fb58ab-f576-3c3d-adf7-74f2fb1cc5fc" . "I Can’t Turn You Loose")
("3f1b4dca-5ec3-4f7f-8c3d-2ddafbbd3cd0" . "I Still Look Good (for Sixty)")
("f30bb75f-2f9d-3f16-928e-cf8b4ce6288a" . "If I Needed Someone")
("207cd968-182d-4f1f-96cc-c42c88e9cf7c" . "If You’re Out There")
("a90eacd0-9a85-33de-a48d-476198dfe777" . "In‐A‐Gadda‐Da‐Vida")
("5ca57b8e-6f2e-403b-99d8-d0417e394b5f" . "It Ain’t the Meat (It’s the Motion)")
("034c723f-a482-38d8-863a-39726e7823d0" . "It Takes Two")
("79f25b2f-dd32-3f14-9b04-02bd5b3142fb" . "It’s All Over Now, Baby Blue")
("2f167c1f-b5ef-4923-83e6-a565abaa0b0c" . "It’s Been a Long Time")
("31a5e4f8-0269-3a73-8392-3ad6ea3eb6e1" . "It’s Now or Never")
("ce4e16c2-deeb-48c6-b5a0-967e26be0183" . "It’s Too Late")
("d4050cab-4738-42c8-9071-986b54b80421" . "I’ll Fly Away")
("69555328-0ef6-4123-ade6-6e6792135cd6" . "I’ll Sleep When I’m Dead")
("fb1c1e7b-2def-339b-8ea3-eb64976c3dbe" . "I’m Crying")
("b7b81349-5684-3c4c-a9c8-847e2156dd20" . "I’m Free")
("7f540e4c-8f5a-4186-a48f-34dee6cfeb60" . "I’m Shipping Up to Boston")
("f6106643-4b35-40c4-b80f-2df220b64aa8" . "I’m Your Detail Man")
("95fba578-26f7-43fe-ae8c-91dd58e8da46" . "I’ve Been Working Too Hard")
("3710d794-0eae-4952-84ba-309f00e1a3d8" . "I’ve Got a Feeling (Everything’s Gonna Be Alright)")
("96ff6791-6882-4031-95ea-a8b89cb986dd" . "I’ve Got to Have You Baby")
("bc8c874c-6756-4715-8fc9-a729210001b0" . "Jeff’s Boogie")
("7c4c7cf8-2001-4f2d-91b6-0f70a3e6d308" . "Jennifer")
("98c77c85-30de-48ad-ab83-e0c29b3da0ef" . "La Bamba")
("3a96010b-1118-33d2-8644-4aca81a092dd" . "Land of 1000 Dances")
("0370682d-5393-40cf-a852-8a2abf29be2c" . "Let’s Go, Let’s Go, Let’s Go")
("fa7df21c-3155-4072-9ef1-ef0473ec7c71" . "Let’s Talk About Us")
("99edc495-dcd3-41b4-8051-d9589673d69d" . "Loves Glory")
("d4bbeea9-98ed-376d-9d4e-0a33e179a5f2" . "Message in a Bottle")
("d440df8c-77be-360c-9661-1d00aecf318b" . "Movin’ Out (Anthony’s Song)")
("499725b0-2ebb-4735-91ba-bfcdce09ccdd" . "Peg o’ My Heart")
("8c44720a-1bba-3009-a3e3-6bc69f88cb43" . "Rag Mama Rag")
("fa4eb7a5-7922-4ea0-a451-0e9a16cb6fa0" . "Revival")
("603cb5c8-583a-32bc-b722-1853a21ea0aa" . "Rockin’ Pneumonia and the Boogie Woogie Flu")
("94dc6699-291a-3841-821b-33f6e6e1479f" . "SWLABR")
("ce119449-81f2-4fb2-a5e8-699c09bf4f71" . "Sandpaper")
("c277f883-fc08-4932-8946-d822c0f9f5cb" . "Some Things Just Don’t Change")
("3cb906f6-1f5d-4be8-84dc-9a6b00b25bee" . "Steve’s Song")
("0ace8439-6ba9-35d2-aa2f-9fb225e14bbd" . "Suffragette City")
("537d49de-45fd-3b74-93f3-17a852283b63" . "Surfin’ U.S.A.")
("ca15ad18-350c-3dd8-8c11-13d92b82d8d8" . "Suzie Q")  ;; Is called "Susie Q" on MB
("a3e014ca-1ef6-4bf7-9c42-46064f3ccf25" . "The Earth Is Broken")
("3ef70fd1-cb7b-39a4-a0c1-74abb23928c1" . "The House Is Rockin’")
("59f4520e-95e1-3f77-ab62-ff9f21285bec" . "The Times They Are A‐Changin’")
("ba982afe-d4a1-4dbf-87a1-d4d543bc9b55" . "The ’59 Sound")
("bd5bfa67-37ad-434b-aafa-14c6ac50b925" . "Troubadour")
("77d8a7cf-750b-4ca1-b2c3-43b30d0fc4fd" . "Twistin’ the Night Away")
("84b5bc36-bcc0-3db9-bc09-50d152384fbb" . "Uptight (Everything’s Alright)")
("1bda291c-6b32-4c80-b5cd-fdc4cea2fab7" . "Walk, Don’t Run")
("b4a46841-4a7e-4321-875b-c7106e9c006d" . "Wayfaring Stranger")
("e86de036-db65-430e-a3c6-83f2a6691da2" . "We Wear the Mask")
("f8e9041b-2aa0-44e5-bd7b-c151c37dcc66" . "We’ll All Man the Guns")
("0a15ceb2-9ce8-4f3e-b901-3e016abc8a30" . "We’ve Got to Do It Now")
("1261f0f2-c5f0-3c51-94d8-ec7715fa8b59" . "What’d I Say")
("863ded0b-f8b1-4188-887a-9a11391a9f7e" . "What’s Your Name")
("6f618d32-f077-4166-9d38-a699b75c12c5" . "Whitetown")
("04153858-606c-42fc-8509-3e387e33d342" . "Who Says You Can’t Go Home")
("11736579-5689-4ec7-b806-6430a4cade15" . "Woman’s Got Soul")
("f00616af-fff7-3fe8-ab97-5dda1d34eded" . "Won’t Get Fooled Again")
("5ac1c053-5f4d-3e75-b6c9-6169595e784e" . "You Can’t Always Get What You Want")
("7e8340b0-86df-4a43-98ff-8c6df7c09e0f" . "You Can’t Judge a Book by the Cover")
("8c241bbe-150b-4412-b81b-1e35e71165af" . "You’re a Friend of Mine")
("286d48aa-6ef1-3b30-8f4a-4287a018a9a0" . "You’ve Lost That Lovin’ Feelin’")
("b8a60658-bca1-3a16-975d-a40a2100d862" . "Zip‐a‐Dee‐Doo‐Dah")
))

;; Sorting:
;; sort-regexp-fields
;; 1st parameter '^.*$'
;; 2nd parameter ' ".*'

;; Brucebase studio sessions written by Bruce
(defvar mb-brucebase-studio-sessions-bruce-works '(
("a782a20b-3860-4419-aa06-8e434cbaf15e" . "A Winter’s Revelation (in 9 Illusions)")
("2936e983-537b-4e52-b1a7-90260a5b5f79" . "Armed & Dangerous (with Love)")
("162e2293-5b69-42cf-aa6e-8a0aec9ed4b1" . "Boppin")
("dedbb5dd-9c36-4f93-a709-8625fa951c3f" . "Call Any Angel")
("584de46f-0be9-4e99-80c5-9c30dcf2e580" . "Gone Are the Days")
("397e32ca-1a4e-429b-a44a-3015016869e5" . "Inside the Castle Walls")
("014b8f86-d1fe-4f09-a36e-d8b91bd58789" . "Love Won’t Let You Down")
("d54923c2-c4f0-48de-94fe-8d39f4b36545" . "New Kinda Love")
("c19a7fd9-1d6f-402f-ae73-c1cb7d703471" . "Seize the Time")
("ab4364fb-3b47-473d-8f3b-d17daa4f8933" . "Shanghai")
("3d50aaed-5364-40c3-b6f7-9f3d5b6c1a45" . "Shootout in Chinatown")
("2a1a8848-0597-4c47-8d40-9b5eafde1291" . "Summer")
("a42bfe5c-ebed-4e1d-b36c-019bc49e0061" . "The Hard Way")
("a0c91b50-a315-42c3-800a-024fe9101fb8" . "The Informer")
("7ef39ce9-9085-4395-8678-0a2e185123f9" . "The Mansion")
("f8230b34-0b13-4b56-b069-4cd506fcf98a" . "Time (Man Who Got Away)")
("adc8f354-0557-43b8-ab63-933f301c1081" . "Vigilante (Out in the Stormfield)")
))
					;
;; Brucebase studio sessions NOT written by Bruce
(defvar mb-brucebase-studio-sessions-bruce-others-works '(
("9ae304e9-be3c-4d69-86f5-606bdee360fb" . "I Come and Stand at Every Door")
("fd958b05-8fa8-4279-b660-77cd4fb1e323" . "Once Upon a Time in the West")  ;; Called "C’era una volta il west: C'era una volta il west" on MusicBrainz
("4f05041d-0780-3e97-a92d-c922fa3e78c3" . "Pipeline")
))

;; SpringsteenLyrics works
(defvar mb-springsteenlyrics-works '(
("4c2f991b-c71d-4bfb-9290-37031f65906d" . "Can’t Teach an Old Dog New Tricks")
))

;; Brucebase songs not in songlist, i.e.: played in rehearsals
(defvar mb-brucebase-songlist-missing-works '(
("0637c000-ea81-3c2d-8e52-64d28e16c1c2" . "Changes")
("0009262c-a842-4463-9ca2-1e08563193bf" . "Here Comes the Bride")
))

;; The Bruce Springsteen Band / Steel Mill
(defvar mb-brucebase-steelmill-works '(
("f8388fdf-389c-482a-8758-6e81b51decaa" . "Full of Love")
("8cd71cd0-af74-4578-a514-d6c1a6a06bfb" . "Gypsy Rider")
("57b3b489-edc2-4a1c-9708-d074462c141e" . "I Hope I’m True")
("a14c0e80-0d78-4261-a971-ad860b9424e7" . "If You Want to Get High")
("df5b9679-ba60-3988-9227-a79667524b18" . "I’m Into Something Good")
("50d5e261-6517-4aa6-a318-18bc08dd726e" . "I’m a Big Girl Now")
("93e70690-dc48-42aa-9de4-7ece722ede3d" . "She’s Got the Lord")
("5b71ab46-80b0-47ee-a20f-47acb0e9b1e8" . "So Much Style")
("2393ac76-d7ac-44ef-b6f3-d26f2d684ec0" . "Sometimes at Night")
("e60b4189-581c-4c4f-ad4a-5cdcc7883422" . "Spanish Dreams")
))

;; Songs NOT performed by Bruce, but Bruce present
(defvar mb-brucebase-songlist-other-works '(
("15875086-77a1-3e37-b125-75252d9d6bef" . "Rockin’ Around the Christmas Tree")
))

(defvar mb-works (append
		mb-bruce-works
		mb-bruce-other-works
		mb-brucebase-works
		mb-brucebase-studio-sessions-bruce-works
		mb-brucebase-studio-sessions-bruce-others-works
		mb-springsteenlyrics-works
		mb-brucebase-songlist-missing-works
		mb-brucebase-steelmill-works
		mb-brucebase-songlist-other-works
		))

(defvar mb-artists-relation '(
("1ca07311-cbfe-4ae8-a518-aa76c8579802" . "Ada Dyer")
("551bc9be-1a24-4bbb-8133-a2909a48fbf2" . "Adele Springsteen")  ;; Adele Ann Springsteen on MB
("09381e15-32ed-447c-ad06-475baf3b4496" . "Al Chez")
("73bac1b2-69fc-4dff-b428-9f24ef552341" . "Albee Tellone")
("615e4dea-7f8e-469d-a455-dc9461c1387d" . "Alexa Ray Joel")
("6b064ead-91a4-4ac8-8076-b1febe4f4aac" . "Alison Krauss")
("a6410e63-8056-4447-837d-2fbf5c504979" . "Alliance Singers")
("73671c52-02f7-46b9-970b-88122367a1d8" . "Amy Tan")
("a4557bbd-f1b2-4b29-ae8f-238693d9c2e8" . "Andy York")
("7e26152e-9b87-46c5-a3ce-399cc9c4c571" . "Angel Rogers")
("7e26152e-9b87-46c5-a3ce-399cc9c4c571" . "Rogers")
("71853da7-d1e3-4649-b9e2-a74d5700badf" . "Anna Maxwell Martin")
("1d483e05-2769-46f9-a614-28682d0c7ebd" . "Anna Webber")
("b0ab3979-4165-4e3e-b125-2d77f14080bd" . "Anthony Almonte")
("40dfc5cb-158f-41a5-8013-f2bda59dde9a" . "Anthony Mason")
("9111dc59-5ccd-4bd5-a8ec-3b19fee9d9b7" . "Antoine de Caunes")
("2d46f205-9f6c-444c-89da-80b4ffa98b7a" . "Anya Taylor‐Joy")
("7ae16953-3a0a-4a95-8446-2b3e04c2dba3" . "Audie Cornish")
("0881daf1-20df-4a3e-a84f-6476a84bb172" . "Badly Drawn Boy")
("0de4d19f-05c8-4562-a3c0-7abdc144f1d5" . "Barack Obama")
("3435011c-c0d1-433b-9049-333d54b4af00" . "Bart Haynes")
("092ae7a4-0ea2-407a-a200-d1ee09383537" . "Barry Danielian")
("a391d4db-14eb-417f-9ca9-912eeed9fbee" . "Benjamin Lanz")
("3f0b02ae-bc83-4417-ab1d-055b23d829fe" . "Beaver Brown")
("6a849bbc-e2cc-422d-95ca-d3230d802a47" . "Ben Jaffe")
("aee70c97-96b1-4d6e-a53f-a4f539bf1fa2" . "Ben Mankiewicz")
("0e316a48-d924-47ac-a506-bcc438e30e5f" . "Bernard Fowler")
("0d65b949-fb80-4fb5-aab8-75a77700dece" . "Billy Chinnock")
("64b94289-9474-4d43-8c93-918ccc1920d1" . "Billy Joel")
("58e235fc-f6ea-4614-9ed4-9855a5665ae1" . "Bleachers")
("c716c897-8db9-4167-8787-940b14f6e1ca" . "Bob Berger")
("4382b934-64c3-47ac-98db-65f26d845c48" . "Bob Seeger")
("358d8fde-ad5b-4df1-bcd8-193bada6edd5" . "Bobby Bandiera")
("9df6e393-da6b-4e65-9f50-6e4df5624e63" . "Bobby Bandiera Band")
("bac9f82f-8c72-48a8-9b04-6e46ce8d4e77" . "Bobby Keys")
("dd891d52-e0f1-4671-93d6-7be3ab19f3b9" . "Bobby King")
("5818ebc1-c87d-451e-92da-ee8422f6c149" . "Boccigalupe")
("137e3419-13b5-46d6-8e0a-a9231de584b4" . "Boccigalupe & The Badboys")
("5dcdb5eb-cb72-4e6e-9e63-b7bace604965" . "Bon Jovi")
("0f0da09c-3940-4cb5-879f-4cea28907810" . "Bonnie Raitt")
("3575336b-07c0-4efb-860b-3482dc5e1124" . "Boss Bill")
("7f347782-eb14-40c3-98e2-17b6e1bfe56c" . "Bono")
("9a3d4dfa-ba58-4f01-b5e9-6ecb1c6f7996" . "Brad Pemberton")
("5b9dc76b-8574-4cb5-9496-ab1bc0d35c29" . "Brandon Flowers")
("32ad7683-0eb5-4428-acc2-4b2127601161" . "Brendan O’Brien")
("df8b4131-e876-43a9-ab11-8d505bc94ebf" . "Brendan O’Connor")
("e2259e9d-ab86-4e5a-8054-d0e3464616eb" . "Brian Kirk & The Jirks")
("20d7beef-118f-493c-99f9-3931835cb658" . "Brian Fallon")
("827cfaf3-0d59-4e0d-bd33-e4d1883ec345" . "Brian Williams")
("634fe78e-fc6b-4b2a-ba83-c8c66e13a8aa" . "Brian Wilson")
("e60dbba7-6f55-429a-bd22-5e9c6a87ae70" . "Bruce Hornsby")
("ef4242fd-b938-442d-8aff-2c989a568419" . "Bruce Kapler")
("70248960-cb53-4ea4-943a-edb18f7d336f" . "Bruce Springsteen")
("a60fa7b7-ec5c-4c0a-bec2-f1e2ae8eb4f3" . "Bruce Willis")
("b97b3247-9a45-45c1-be0f-c4dc3a681840" . "CBS Orchestra Horns")
("e737612d-3e51-4741-80d5-00a31d979413" . "CC Smugglers")
("2a1d2c80-84d3-4e34-b860-498d66302e65" . "Carol Dennis]")  ;; "Carolyn Dennis" on MB
("2a1d2c80-84d3-4e34-b860-498d66302e65" . "Dennis")
("8893617e-e3c2-406c-afc3-ec390e040ed2" . "Cats on a Smooth Surface")
("4382fa5d-03de-4ebf-baf3-df6a1d1922f0" . "Charles Giordano")
("f581a3dc-3319-4656-8b97-66d560482f4a" . "Charlie Watts")
("bb5e019b-a9e6-4ee7-96c3-226c40a0627e" . "Charlotte Ryan")
("a5ab77f2-da3b-454c-aa5f-239f5c8ea096" . "Chris Anderson")
("479497d4-e7c2-4e78-972e-56e78fac3995" . "Chris Isaak")
("fde0d322-1493-4087-a6cb-73e5d8d4461b" . "Chris Jordan")
("f6ef6771-591c-4782-b03e-f3fe6101dcd8" . "Chris Masterson")
("8d6aa484-ca76-4535-9cc6-a5777e7a8438" . "Chris Phillips")
("c77c62d3-f025-429c-bcae-91e83dd3f516" . "Chuck Leavell")
("7e4bfa5f-a8b8-4fb0-81b5-f74f6ac72133" . "Clarence Clemons")
("e070d092-5bd5-4379-ac55-b3534832e2a5" . "Clarence Clemons & The Red Bank Rockers")
("07ddd361-0f2a-4819-970d-c8370deeac11" . "Cleopatra Kennedy")
("07ddd361-0f2a-4819-970d-c8370deeac11" . "Kennedy")
("8f45e863-c2c8-4f10-b9bf-4d36774bb0b5" . "Clive Davis")
("cc197bad-dc9c-440d-a5b5-d52ba2e14234" . "Coldplay")
("210c1047-0e88-4b73-81ac-13a026cefb01" . "Conan O’Brien")
("a0327dc2-dc76-44d5-aec6-47cd2dff1469" . "Counting Crows")
("8abedd53-89d9-4a64-929a-ecddf453ed94" . "Craig Caprioni")
("6a62a034-cee3-490c-a2ad-621eca2f25a4" . "Craig Finn")
("b36e1a45-2f2c-44a5-b8d8-9dc12159fab5" . "Craig Werner")
("93dce18b-13ca-4118-9fa1-af92e8e3d8ac" . "Crystal Taliefero")
("028fd996-fe8f-41b6-a6ed-1cb4b06a23d2" . "Curt Ramm")
("7b18698b-33fb-4865-b158-e74f3457eeb7" . "Curtis King")
("cb914df3-0925-4078-bc0e-c23c864898af" . "D.J. Fontana")
("2efbcc42-4438-4594-a4b6-99214d82e35a" . "Danny Alexander")
("a1979995-66bc-4e61-9880-2e9f0484b29a" . "Danny Clinch")
("94a88a40-8568-403e-86e6-8c01fd4b626a" . "Danny Federici")
("06f587e2-79ca-455d-a06a-64691a04a786" . "Danny Gochnour")
("18442b0a-e156-4cb2-a1b1-f894d9bc8d17" . "Danny Hyland")
("af953ba2-283b-41ef-93bc-b4cfc5feeebc" . "Darlene Love")
("b5b6880b-9c35-44be-ba09-acb3df6dc8fd" . "Darryl Jones")
("4d5f891d-9bce-45ae-ad86-912dd27252fa" . "Dave Grohl")
("d70c2ae1-911a-4bea-8ef4-63564a150ff2" . "Dave Mason")
("029c91a5-8e46-4c96-a4e8-3878417fb599" . "Dave Marsh")
("1ca7845a-78e8-48e7-8f1b-68ffd2159342" . "David Bryan")
("4401f986-51b8-407b-a898-500543df9dae" . "David Sancious")
("a3940ff8-c898-4747-b7a4-58ed3c251a14" . "Dee Holmes")
("8686d765-bd89-4448-81e2-2cd73a07eb27" . "Dermot O’Leary")
("89efeadd-8080-4cf9-af33-2e68a173df02" . "Dion")
("b60527cc-54f3-4bbe-a01b-dcf34c95ae14" . "Donna Summer")
("33d2abad-fd92-4374-929c-886c884d2ee5" . "Donnie Powell")
("e8374874-4178-4869-b92e-fef6bf30dc04" . "Dropkick Murphys")
("fe0e1895-aa84-47d9-8e5b-7930fc20709b" . "Dr. John")
("b66771cc-45fa-4a32-b14f-5337d7223d7a" . "D’Angelo")
("4f29fd17-613e-46f3-94e9-f8f508033ab0" . "Earth")
("6a3394ba-6888-4dd0-93f3-06c1e35749d8" . "Ed Manion")
("1a60d6dd-9d3e-40fc-a66d-3184f9ee0d61" . "Eddie Vedder")
("d46897c5-f814-4efc-bd94-8e72b2ecd06b" . "Edith Bowman")
("ab4bf5ea-4225-4b31-b0db-f630da0b737c" . "Eleanor Whitmore")
("8a338e06-d182-46f2-bd16-30a09bc840ba" . "Elvis Costello")
("1350f186-0fbf-4ea7-896a-340d52a29f40" . "Elysian Fields")
("35ef61ca-43db-4772-ba27-0489e9ebcb69" . "Emmylou Harris")
("618b6900-0618-4f1e-b835-bccb17f84294" . "Eric Clapton")
("15856215-cc51-4423-879c-9ac955ccb11a" . "Eric Church")
("2549e110-9933-4331-bd8d-a632f0cd639c" . "Ernest Carter")
("6e6bb7ee-2df1-4402-bcae-d26884eafd81" . "Evan Smith")
("a5dcab6e-0c18-4237-9f8d-1f207430237a" . "Exit 105")
("df64ba7e-957d-4873-b195-7e08ba039a1d" . "Felix Cavalieri")
("b9ad1f8f-0dad-4e76-9de5-48076d533959" . "Fernando González González")
("88798869-338b-418e-9c47-97365444b734" . "Florence Welch")
("6eb1eda0-25af-4154-a560-1995a8c30e93" . "Frank Marziotti")
("c904e534-acda-4ad0-8b6b-7a047f586e5c" . "Fredrik Skavlan")
("16ee848d-7f63-4109-8d55-47db5d8211b9" . "Freedom Bremner")
("3b7c9a50-2e5b-4350-a39a-fd7b6ef3274f" . "Gayle King")
("6123044a-f551-4c68-8d28-64c40035172c" . "Garland Jeffreys")
("42b42dd1-9263-4eae-91cd-4014a5b5d39f" . "Garry Tallent")
("0f1083d8-fca9-40e3-b7bb-6cca7f15eff0" . "Gary “U.S.” Bonds")
("eb0cd72f-0235-47ec-9d0b-f244a2afc3b7" . "Geoffrey Oryema")
("ed30e048-a8fb-47f0-8ad1-3e9b3ededaaa" . "George Stroumboulopoulos")
("3dfbd1e7-c9d7-4a1f-850a-87a0e594d0a0" . "George Theiss")
("87b1c4d4-b144-4ed2-8c55-334e3b72f23e" . "Gerard Ekdom")
("98459b46-1fdc-4f83-ad96-f578ca0057c9" . "Gia Ciambotti")
("98459b46-1fdc-4f83-ad96-f578ca0057c9" . "Ciambotti")
("d5c51c36-ac67-4727-91ed-ee9f0df81a2d" . "Gloria Gaynor")
("68d1ae09-7c4b-460d-8123-5338536da48e" . "Graham Bright")
("95bc0175-e0f2-493f-a1db-bab94a405074" . "Graham Norton")
("a283af93-2b93-4b62-be2e-0ae6e14a2622" . "Henry Gross")
("360f2e5e-e321-402a-b33a-3e0d6b07ce79" . "Highway 9")
("9e047ced-bb57-4554-a756-5f528d72723a" . "Hoda Kotb")
("40d9d880-f976-4879-ab1f-83c93cffa871" . "Holiday Express")
("e2714df1-4613-4dee-8e0e-f4f333200914" . "Howard Stern")
("5285ae34-3461-4911-a1ce-108cba753260" . "Hugh McDonald")
("11982f70-7b8c-4009-b27c-395c17ecf379" . "Humberto Tan")
("bfa8dc1f-7f94-4c82-a88e-d41d67d42ee8" . "Ingo Zamperoni")
("710c7ffa-977e-4431-9f00-c8d4eddf6808" . "Irma Thomas")
("a35294cc-95b3-4fe7-8530-22de8b178bea" . "Iron City Houserockers")
("df54cc89-4432-47bb-8138-1a8e4e1f1179" . "J.R. Carroll")
("1155a7a9-a7b4-46ba-ba27-6b6c98be487e" . "Jack Antonoff")
("bac82cac-a752-4a4e-9ad6-fe3395e924f5" . "Jack Daley")
("88527d26-7496-47c5-8358-ebdb1868a90f" . "Jackson Browne")
("6ffcc727-833e-4afb-83b3-9b1357e30b7e" . "Jackson Smith")
("40bccd7f-a0ec-4314-87a0-d909ecd249fe" . "Jay Weinberg")
("d4ec3315-ddff-422a-9efd-73eb29f1bbbb" . "Jake Blanton")
("5c64226c-d673-4d23-a612-2bfb704edd66" . "Jake Clemons")
("ebee61b4-bbdd-4087-9bcb-d5aa099088ec" . "James Blunt")
("107d0c22-d051-4d98-8206-4e14de02132a" . "James Taylor")
("a7ff504b-bbca-49f9-950b-ad02f2a2b092" . "Jann Wenner")
("bf257f0b-c221-4d25-b090-7c35040ad3b4" . "Jane Scarpantoni")
("a069df35-4865-40c9-b31a-c00c4b4f13e8" . "Jay Gibson")
("ec30619b-713c-4f8a-9b2a-da4c7e4b15a7" . "Jeff Garrison")
("681e7510-d6bf-4515-942a-7443d967b46e" . "Jeff Hill")
("b8eb9721-5b9d-4207-99e9-75abaf26555c" . "Jeff Lubin Band")
("db0fc330-1fa1-43f8-a33f-33eaf6a97d78" . "Jesse Malin")
("bde64de3-01d4-4ed9-afe9-88b7d9db922d" . "Jessye Norman")
("f0882d14-446e-4172-9736-98fbc1395fce" . "Jesse Paris Smith")
("22ab1efe-2d1e-494f-af04-09abad060a03" . "Jim Axelrod")
("4f39dcff-2128-48b2-b00d-9020625ada95" . "Jim Rotolo")
("78b7c31c-97cb-4f06-be5c-5c2e5a7c87bc" . "Jimmy Fallon")
("023cd435-dfa8-4d6b-bb24-b55fc0a99ba7" . "Jimmy McGuire")
("f970e8eb-42ea-4cfc-bdb3-e007cf9c4dba" . "Jimmy Iovine")
("3c41ccec-2a7c-4928-a21d-15201218d18a" . "Jimmy Kimmel")
("620a9ca2-b254-403f-9b03-7601b039c010" . "Jimmy Vivino")
("4250835c-5cf8-4db4-95b6-6ba0001f7db7" . "JoBonanno & The Godsons of Soul")
("f376828a-b438-4fda-bb2e-dcd5fbe81f83" . "Joan Jett")
("d33bff2e-65ee-4107-b067-20b22f8cb698" . "Joe Bellia")
("d23955c0-8e4a-42ac-9178-229ccd311cd7" . "Joe D’Urso")
("91fb24e5-727c-4272-b6ba-e6b47cd80a15" . "Joe D’Urso & Stone Caravan")
("4682ba8c-e15d-4543-9747-c72860392c76" . "Joe Ely")
("ce48dd78-903e-4737-aee7-5e70bba7e15d" . "Joe Grushecky")
("60890a33-fc2b-4d06-b8a0-a4599437144e" . "Joe Grushecky & The Houserockers")
("b4fe540d-d245-4d96-9005-63de295edc79" . "Joe Piscopo")
("3644f94c-f682-4fcc-a7fc-b64a4f9cb096" . "Joey Stann")
("dca91a6a-65d5-4eec-a9cc-80bc53b6ea9d" . "Joffo Simmons")
("3e045881-56a5-4b62-8f99-cb3982cab577" . "John Eddie")
("db9023d4-d836-4983-befb-ebed46de8159" . "John Eddie and The Front Street Runners")
("c3649208-0ebe-449b-b1d7-4bd6c560f109" . "John Fogerty")
("44f34fb6-f759-41ec-b1f4-cb5e5928a04f" . "John Stamos")
("da567441-3ab0-4bc8-9ca6-c339600f48d8" . "John McEnroe")
("4530bdd0-9707-442e-b37e-578def6a0926" . "Jon Landau")
("75a72702-a5ef-4513-bca5-c5b944903546" . "John Legend")
("685ec09c-79ee-43eb-b197-aa102fca0adc" . "John Leventhal")
("0aad6b52-fd93-4ea4-9c5d-1f66e1bc9f0a" . "John Mellencamp")
("e86492c1-0376-4df0-8042-8ba058c83960" . "John Prine")
("7f139e42-8426-42c2-ba7c-66e4f365f48c" . "John Scarpulla")
("fd4f1eb1-e8d3-4c59-a916-5470a262fb91" . "Johnny Grushecky")
("5ace9b0c-cc21-4d8f-8e08-a70234439f2a" . "Jon Bon Jovi")
("e670cd1e-abea-4174-8a42-6a159bec4eba" . "Jon E. Gee")  ;; John Gunnell
("4530bdd0-9707-442e-b37e-578def6a0926" . "Jon Landau")
("3c3ebf04-68b7-4140-bef3-7ed0aeda07f0" . "Jon Stewart")
("c58d9270-d021-4755-b1dd-4106e6cc5ef0" . "Kate Capshaw")
("23c1c76d-17d7-473a-b352-630977af574d" . "Kevin Kavanaugh")
("f0ed72a3-ae8f-4cf7-b51d-2696a2330230" . "Keith Richards")
("d56d92ef-c60e-4aad-bb17-88e5a67334ab" . "Kyle Resnick")
("dcaf7d3d-fc8d-43b4-b54e-4af75bdbc27a" . "La Bamba and the Hubcaps")
("b7539c32-53e7-4908-bda3-81449c367da6" . "Lana Del Rey")
("4d5299ce-9223-45c0-a1c5-83ac7bfe8813" . "Lance Larson")
("62db200f-a5ff-4451-8a0d-dc26b7d70f0e" . "Lauren Onkey")
("e8414012-4a1c-4ad4-be5e-fc55294e28cc" . "Lauryn Hill")
("486b576c-56a6-481b-ad8d-84a7457fc901" . "Laiya St. Clair")
("236fca70-1195-4842-a1a0-0e8a0aff98fc" . "Layonne Holmes")
("3cb25fb2-5547-4b05-adec-1a5e37830d46" . "Lionel Richie")
("f887110a-ddd9-4a4e-b9ea-e567523ef6e2" . "Lisa Fischer")
("b4cadc83-7307-4196-9f2f-29ebf1650045" . "Lisa Germano")
("59d77428-bf32-4c9f-bc4c-7c03ec882c59" . "Lisa Lowell")
("9659f4dc-358c-4eb3-8a16-27fb983a9650" . "Little Steven")
("b81c5f3a-4659-469c-bbf4-3d2a3488d3ec" . "Louis Lahav")  ;; לואי להב
("8ee379d7-d299-4182-8b37-17226025c1b1" . "Lucas Ruge‐Jones")
("21685b15-3074-446e-aa1d-ff7157014f53" . "Lucinda Williams")
("dde26295-8cd4-474c-8740-3edb801b2776" . "Maggie Rogers")
("a8950eed-e308-4588-bd76-3b74752e93c2" . "Malcolm Gladwell")
("ed7e8db7-749e-460b-bd28-eb1de46dccda" . "Marah")
("d0d37e37-fcc5-425b-ae08-200f6a537e59" . "Marion Vinyard")
("e49f69da-17d5-4c5c-bac0-dadcb0e588f5" . "Mark Knopfler")
("8e3a27d7-9ba2-4670-bb11-10550870762c" . "Mark Radcliffe")
("68868bf5-7b90-4367-ba5d-a26871b5c206" . "Mark Rivera")
("7962d3cd-83e7-46ea-8352-531a65cf2d29" . "Martin Scorsese")
("f1bc3b70-000a-4d50-b6c8-ac8acd25a24d" . "Massimo Cotto")
("1ec61857-c0a8-41f2-959c-a05df5d465c5" . "Matt Savage")
("6c7d78f6-afb2-4784-b236-bccee5278839" . "Matthew McConaughey")
("2566ca73-1dfd-49e7-ab20-dfa5697b360e" . "Max Weinberg")
("b3ae82c2-e60b-4551-a76d-6620f1b456aa" . "Melissa Etheridge")
("92fbcd5d-3a3e-4653-b4ff-0bd99ee61d4f" . "Michael Blyth")
("cf11ca6d-fc48-4cd5-aa0c-d8c51d50ecb0" . "Michael J. Fox")
("e67ac344-ce0a-4a27-b4ad-9502dab57a82" . "Michelle Moore")
("c19b8adb-734e-4e2f-948b-f97e01741e79" . "Michelle Obama")
("b5ffc3aa-b868-4b88-905f-d73d51dbe51c" . "Mick Jagger")
("9dd447fe-6c6b-4f27-b2ef-6f97550d0fad" . "Mike Batlan")
("8b8991c9-1adb-4614-a0d6-08ad0547d8bf" . "Mike Domanski")
("078e680d-6d6c-46ce-8134-0bddeb9e5e35" . "Mike Mancini")
("45d7e6e8-c5bf-4c59-a79e-c7bb2e9b2cc9" . "Mike Ness")
("3f14041e-7e12-4cbc-954f-c72b519feeba" . "Mike Riddleberger")
("a3f9349f-d0ec-4d03-b00b-0c556e6b52d5" . "Mike Wanchic")  ;; Michael B. Wanchic
("bf083680-545a-43fc-92cf-99bc93f2a5b0" . "Mike Wilson")
("4274bd31-7306-4aaf-8a09-4186237f6668" . "Mikey Freedom Hart")
("346f8e40-4be4-4dc8-9b8b-d4896324d1af" . "Mo Gilligan")
("0e073997-d11b-43f2-84e1-1675201e9896" . "Muddy Shews")
("23b3c1dc-7334-4deb-be55-3ed978ead05c" . "Nicole Lawrence")
("a1ef6bc8-2644-4b6d-aa21-27b630acf751" . "Nils Lofgren")
("93784fe1-8b8a-47ab-b00a-e54ca9f224be" . "No Spring Chickens")
("31bed9ef-ce55-4b54-810a-5f1b8162d2f7" . "Noah Le Gros")
("af78c8ec-0d33-4082-98f5-23281b65b109" . "Nona Hendryx")
("8bdaad41-5c6e-4165-97d2-44599c828e3e" . "Nora Guthrie")
("ef011392-7eee-4090-ad83-660c48e9b1c1" . "Ozzie Melendez")
("23a3f71a-d1ce-424d-80c1-27598585f9fd" . "Pamela Springsteen")
("f09aa40c-b613-4ea2-a8cf-6056c2657a9a" . "Patti Scialfa")
("2944725e-3dcb-432e-b575-0bed5cef3504" . "Paul Shaffer")
("05517043-ff78-4988-9c22-88c68588ebb9" . "Paul Simon")
("ba550d0e-adac-4864-b88b-407cab5e76af" . "Paul McCartney")
("e6acd6a9-95b0-4492-8f87-15f45afa341b" . "Paul Rudd")
("83701bad-b130-4c19-9783-59061ca79aa0" . "Peter Wolf")
("75d1bf84-030d-46e1-8466-4f7e32b6265d" . "Phil X")
("d3986756-f1dd-4635-9510-2d116c0deddd" . "Phoebe Snow")
("136986bb-6e39-4e79-9945-cf9aa37a4a0b" . "Phonte")
("3599a39e-4e10-4cb5-90d4-c8a015ebc73b" . "Portugal. The Man")
("f5f40219-8929-485a-aeb1-1f8093ee1c24" . "Purpul Dyneste")
("9436fbf8-5498-43e9-9a8d-eccbe53df6ad" . "Questlove")
("1c09b9e6-a12a-4cea-b80b-97415d19381b" . "Raina Douris")
("2e59d801-d364-4ef6-9f8d-02d069251247" . "Ralph Lauren")
("14440b07-0e02-49b1-9b0f-feb730d9c070" . "Randy Moore")
("714491b8-1e97-4edf-a304-a8b74737fb6e" . "Read Connolly")
("072fc124-6f78-4ecc-9cc5-9ce82c2acf3f" . "Rebecca Manzoni")
("07aebfa0-55d6-47e0-a284-12330e3eae0d" . "Rick Rubin")
("473c0979-030a-4524-8ff0-7b84aa733fd0" . "Ricky Ray Jackson")
("a0920ae6-2968-4f92-ac14-67448452bd18" . "Robbie Robertson")
("5cc59119-95ca-415a-8a14-3eac45670afe" . "Robert De Niro")
("5613f03b-10a9-4556-b02c-27ef89be4e36" . "Robert Santelli")
("6b96e1da-02c0-4426-937e-3898e615455b" . "Robin Quivers")
("536e8e61-8040-40a1-8b35-a2c6996dc44f" . "Robin Williams")
("89729b97-90a3-4f84-9e88-e16f96cab350" . "Ron Aniello")
("92ed8183-8f22-42b2-af4e-d44137610fa0" . "Ron Wood")
("5ff6f6eb-31ad-4903-a3c3-4c9283fcde8b" . "Rosanne Cash")
("11d2fcfe-669d-4596-8921-e07dbdae311f" . "Roy Bittan")
("5870003e-ce01-45cc-8e1c-53709407685b" . "Ryan Tubridy")
("868114f1-906b-4926-9daa-3bb046d0ea06" . "Sam Moore")
("fedf79de-ca39-47a7-bbce-b14f15e4a305" . "Sarina Bellissimo")
("c03b5c82-39d8-49c1-8753-902832226cee" . "Sean Hutchinson")
("3bcbedbf-f835-4d8a-af91-627c17c7fd39" . "Seth Meyers")
("345bd2f6-29aa-4457-90fe-e736b5eb47d7" . "Shane Fontayne")
("345bd2f6-29aa-4457-90fe-e736b5eb47d7" . "Fontayne")
("80ccfede-c258-4575-a7ad-c982e9932e0f" . "Sheryl Crow")
("3fa031e3-0d93-4bb1-b3b2-14e06b147a0f" . "Sienna Miller")
("a9100753-f539-43cf-bcc9-579566fb512e" . "Simply Red")
("e1e05cce-3922-44e1-8f20-015abe5e309d" . "Social Distortion")
("bdc377f8-5701-4dc2-9180-bb635aa4ff5c" . "Sonny Burgess")
("1386d33e-ce83-40ec-b92f-886f3725178c" . "Sonny Kenn")
("0885bc49-99bb-4b7b-b4c1-4a8797439727" . "Sonny Kenn & The Wild Ideas")
("065af1a2-2fa9-4864-852e-08c00c9c67d8" . "Soozie Tyrell")
("5b5fb19c-906d-46b3-a4da-f6088c49d251" . "South Community Choir of Asbury Park")
("f826b4d1-2dc3-4c4f-ba70-69b472320338" . "Southside Johnny")
("d7bc97fb-4bd3-453b-98a1-9081f89c52f0" . "Southside Johnny & The Asbury Jukes")
("cd9053ff-7870-4c7e-a03c-d7aa8f6fa9f3" . "Steel Mill")
("0213d82c-ab77-410e-ab80-1905376faeb4" . "Steve Clark")
("ec863030-7c13-45a3-a025-a69195d3a020" . "Steve Earle")
("59e1d123-844c-4e92-8a8f-94d4e058314e" . "Steve Earle & the Dukes")
("bc7ceac9-a524-42bd-996a-380d2f1eb567" . "Steve Shews")
("7dd40d4f-7af2-4233-8b87-785254646bbe" . "Steve Hargrave")
("5642774a-72c0-4099-8da4-7c1ab36378a8" . "Stephen Stills")
("5b7c47b7-fcad-450f-bf63-48e2c43fc4e2" . "Stephen Colbert")
("de53495e-ad5a-4c30-82ab-05e7e3ec7b4d" . "Steven Van Zandt")
("7944ed53-2a58-4035-9b93-140a71e41c34" . "Sting")
("0e763b87-f3cd-4a63-986f-bebc4189f1f0" . "Stray Cats")
("059e03e3-e426-4ac0-b4df-68118cc2ac8f" . "Suga Steve")
("14472e26-9503-4c62-92a9-9bea28d4508c" . "Gordon Vinyard")
("7bef92eb-f2b1-4790-935a-6e411eff406e" . "The 1992–93 World Tour Band")
("7bef92eb-f2b1-4790-935a-6e411eff406e" . "1992–93 Touring Band")
("e4d62b29-a67e-4700-ba16-35c436cc1f84" . "The Alliance Choir")
("1607e961-c4a7-4602-ac22-d0d87833eee3" . "The Bruce Springsteen Band")
("3d6009da-fb0d-4b63-8bde-47cde79dd7f5" . "The Castiles")
("3248ed2d-bada-41b5-a7b6-ac88faa1f1ac" . "The Chicks")
("c99af15d-d3fb-4845-840f-4c85cac987c0" . "The Dogs")
("4fa97aab-aa25-4d12-ba0d-37b49e714cbc" . "The Danny White Band")
("d6652e7b-33fe-49ef-8336-4c863b4f996f" . "The E Street Band")
("ad2232c5-2b11-4699-80c2-e5f83c56c8e4" . "The E Street Choir")
("61c0a8b8-e3f1-4e7f-9f42-7b21a7bf9e4e" . "The E Street Horns")
("9df6e393-da6b-4e65-9f50-6e4df5624e63" . "The Bobby Bandiera Band")
("a94e530f-4e9f-40e6-b44b-ebec06f7900e" . "The Edge")
("1adfbbca-340a-46dd-a47a-8ba0b879cb68" . "The Max Weinberg 7")
("c7589842-71c8-460a-a0ae-7833b8a76fe0" . "The Miami Horns")
("3072c9d3-3787-407c-ae2f-69e7f9846b49" . "The Rogues")
("b071f9fa-14b0-4217-8e97-eb41da73f598" . "The Rolling Stones")
("80b3cf5e-18fe-4c59-98c7-e5bb87210710" . "The Roots")
("7d5c39ac-5d48-4fdb-81df-6212e38353b3" . "The Sessions Band")
("fae2cd4c-f847-4cb3-b311-60c8906cdb0b" . "The Smithereens")
("3d49e36a-cc9e-411e-93c6-d1646ba5bd3a" . "The Staple Singers")
("91037ad4-4ee5-4d8b-98aa-41df93ddd92e" . "The Tangiers Blues Band")
("15fa2639-cea9-4f13-8ed7-88ad594fdfb9" . "The Van Jets")
("3cf1bde9-1191-4220-9ce0-15413361e89f" . "Thom Powers")
("eea2b7c6-443c-49ae-8643-1210ccac6831" . "Thom Zimny")
("9cdaf9e0-1b4e-4b35-bb0a-4735a506cd09" . "Tico Torres")
("08ebf27c-3057-461f-b8fe-8d3ec6619816" . "Tim Ries")
("bd78fa2d-24f5-4c4b-a756-dcd793b2d390" . "Torsten Groß")
("7e5cfc9a-e9e1-46f1-b81a-861b12049488" . "Tim McGraw")
("288baac9-20b0-47ea-ac18-3feba612aeea" . "Tim McLoone and the Shirleys")
("e3c5fab4-caa0-4201-897a-46f8255b1695" . "Timepiece")
("c78fe0c7-70d7-4c21-af77-ae1caa2f7af2" . "Toby Emmerich")
("59798d85-df1b-4cc5-91f8-f266eadb9ccb" . "Tom \"Bones\" Malone")
("979acd47-df26-4863-943d-8c44f52cddc0" . "Tom Chapin")
("42a636a0-dbe4-4d0c-abe2-1590fad9531b" . "Tom Morello")
("e7d49e3f-2bc0-421f-ae55-bd710155b1ab" . "Tom Power")
("919bc11f-4fdd-41fd-8570-11acbd4cc03f" . "Tommy Sims")
("919bc11f-4fdd-41fd-8570-11acbd4cc03f" . "Sims")
("b9c724b5-0772-49c7-a8bc-73527afe9059" . "Tom Cunningham")
("422fc30a-5343-4aad-b094-50b6d16b1484" . "Tony Orlando")
("850054f3-11ce-437b-a82b-5917719c78dd" . "Tony Shanahan")
("f6365088-96ba-441d-9524-ebd4cc5b467f" . "Treves Blues Band")
("c27296f1-857c-4555-9e81-2c6f1cb1baf4" . "Trevor Noah")
("ef5064d8-2b19-4a9f-a2ef-3d92cdf1ab93" . "Troye Kinnett")
("a3cb23fc-acd3-4ce0-8f36-1e5aa6a18432" . "U2")
("8e434ed7-69a7-489b-905c-6735870befcb" . "Victorious Gospel Choir")
("3e2b6ee0-6ec7-4622-b039-1cbed5f55288" . "Vini Lopez")
("06fc0de7-1647-4c0d-94bf-a6580ad0643b" . "Virginia Springsteen")
("98c852f0-4938-43db-ba5d-7a945ad43f89" . "Waleed Aly")
("970fb29f-e288-403e-a388-d2a7889bfa47" . "Warren Zevon")
("40fb02de-afc3-441c-a6ec-21fee861d478" . "Willie Nile")
("fc6214b3-6d82-4803-be74-01ece1723e42" . "Wilson Pickett")
("507226f4-0842-4f43-950e-81a7017230ec" . "Zac Brown Band")
("64a6d00b-f62e-4f2c-bbfe-a9b15e4cee56" . "Zack Alford")  ;; "Zachary Alford" on MB
("64a6d00b-f62e-4f2c-bbfe-a9b15e4cee56" . "Alford")
("51e90731-08c0-4f60-89b6-5b78e5844de8" . "Zach Bryan")
("60c693ce-2121-47c5-bc20-975e43137e48" . "Zane Lowe")
("84212e42-f154-4dbd-becd-8ddd7549b6ee" . "Zem Audu")
("59f8cf5d-38bd-4474-b012-0df1937b0c21" . "Zephyr Avalon")
("b9d71e60-f447-4bb5-b46c-58e89781bacb" . "Zoe Ball")
("345881e6-c293-43d2-bf59-c9598efad3a9" . "The Disciples of Soul")
("7a606d62-de1f-412f-9d5b-57ab4c17b5bc" . "Stan Harrison")
("ea772005-b24c-4368-be0f-3b44e8fa8f36" . "Marc Ribler")
)
  "Brucebase artists with a relation to Bruce Springsteen.")

(defvar mb-artists-mentioned '(
("72c536dc-7137-4477-a521-567eeb840fa8" . "Bob Dylan")
("ed2ac1e9-d51d-4eff-a2c2-85e81abd6360" . "Bob Marley")
("6c7e61bd-ee82-4bef-b7fc-59461011afcf" . "Bill Graham")
("f81d592c-993f-4693-b53a-57db5a6257b9" . "Blood, Sweat & Tears")
("d264a026-7485-4d3a-b4f1-f13fea5c76e2" . "Brandy Clark")
("8e66081a-58f8-4537-b15b-c645b184142d" . "Brittney Spencer")
("7e9dde4a-7d17-4995-818a-fe08704924b4" . "Charlie Bird")
("faf475a7-7db4-437c-a170-9e014c80ccf4" . "Choir of Trinity Wall Street")
("172805a7-6f84-4bb3-8b3b-6dc5bc33b103" . "Chris Rock")
("852c198a-75ee-4e2b-9aba-4453c52260b2" . "Damiano David")
("3021c0be-208c-415d-a59e-46433950cab3" . "Dane Clark")
("27f77df6-42c5-4411-b748-92814a25feae" . "Dion DiMucci")
("9b29b47f-5004-4863-92c0-058b621dcea8" . "Little Steven & The Disciples of Soul")
("b463a3e3-9af7-4f1b-9097-0a4e242eee0a" . "Douglas Sutphin")
("1c893468-37a9-4fb7-909f-929751a8ebed" . "Dr. Hook & the Medicine Show")
("5314f352-9e3d-47dd-8829-099284e283a3" . "Emily Rose Marcus")
("b7be8029-5ebe-4e4b-bc0c-a4d4756463c7" . "Fantastic Negrito")
("aca5718f-4f49-4439-b8f6-209db3f11757" . "Frank Turner")
("5ab2cf34-517e-48eb-b3a5-34e1cbcd600a" . "George Strait")
("68f644b2-42ed-4d11-8bc7-633d5250721b" . "Gladys Knight")
("e2c00c56-8365-4160-9f40-a64682917633" . "Goo Goo Dolls")  ;; "The Goo Goo Dolls" on MB
("9d3889fb-c6b7-47a8-a325-d22ea54292e3" . "Greil Marcus")
("695d3270-3462-4149-b1c1-c869228e597c" . "Isla Fisher")
("ff6e677f-91dd-4986-a174-8db0474b1799" . "Jack Johnson")
("6a997121-0919-482b-a91b-c6fe1b55a4b9" . "James Bay")
("4c501a41-5f23-41c9-a36a-5b86e894e08d" . "Jason Isbell")
("ab5a7a60-1b71-4e5b-b622-e0ae2ce84c32" . "Jelly Roll")
("91b6382f-f2af-4a97-be5b-86471567594d" . "Jim Gaffigan")
("5848aa2a-a0a4-4a10-8741-6e75dbdd2d71" . "Jimmy Carr")
("6098314f-75ce-4a61-9db3-81f725e1bed0" . "Joe Biden")
("144ef525-85e9-40c3-8335-02c32d0861f3" . "John Mayer")
("33246356-b591-41a2-b904-62f849839e3d" . "John Sayles")
("d5563742-bec6-4f58-8db6-3ba276a5e3fa" . "Jonathan Demme")
("12be5b16-915f-44bc-978a-8ddfab235b79" . "Josh Groban")
("3c004c98-aab6-4b63-a2df-e07c98e73b0a" . "José Feliciano")
("2fddb92d-24b2-46a5-bf28-3aed46f4684c" . "Kylie Minogue")
("650e7db6-b795-4eb5-a702-5ea2fc46c848" . "Lady Gaga")
("a6e613d6-ff65-4972-beff-303a1b10ab05" . "Lainey Wilson")
("ec5436c4-c44a-47f5-a640-da8d376a3871" . "Larkin Poe")
("e2190b35-1181-44f6-8587-4c04123a6f30" . "Lars Ulrich")
("0ef3f425-9bd2-4216-9dd2-219d2fe90f1f" . "Lenny Kravitz")
("10ce4bfe-0927-405d-b3f4-b0b7078ee695" . "Mammoth VWH")
("955bc3c8-36d4-47e8-9648-f6728c613583" . "Marcus King")
("c9e6c7e2-bd00-40b0-a3b7-3406b43fba87" . "Mark Pellington")
("9a9fa20a-507f-4a60-9986-54ae5146816a" . "Maureen Van Zandt")
("04f57a2d-2449-400e-8fff-5b1c5af9560b" . "Mavis Staples")
("3862342a-43c4-4cdb-8250-bfdbfb5e1419" . "Måneskin")
("65f4f0c5-ef9e-490c-aee3-909e7ae6b2ab" . "Metallica")
("f27ec8db-af05-4f36-916e-3d57f91ecf5e" . "Michael Jackson")
("4aae17a7-9f0c-487b-b60e-f8eafb410b1d" . "Nick Cave")
("c7a99436-6da3-4190-8ce7-9cd44d62fcd4" . "Mick Taylor")
("64c8fe79-1f93-483f-aace-b6a6e379e7d2" . "New Riders of the Purple Sage")
("5b7e7e75-9590-4fd7-9cb0-89771c7f428f" . "Pat Monahan")
("8e66ea2b-b57b-47d9-8df0-df4630aeb8e5" . "Peter Gabriel")
("01c7aec0-21f3-4ae1-a757-f32be9e365ba" . "Rebecca Weinberg")
("beb68abb-51b4-41d1-9645-d0c8306cf588" . "Rita Wilson")
("91892ad6-e645-441b-bab0-12f17f447161" . "Roger Federer")
("5046ce09-8e26-4845-9caf-ef49ca5ec30c" . "Ronny Chieng")
("3478a5d1-bd96-4584-aa54-4d6e0a329658" . "Sam Fender")
("e68b06a4-d47f-432e-89d8-db43c48a2726" . "Sammy Hagar")
("963de1cf-c099-4c00-8d22-e0d262cf0407" . "Shane Gillis")
("29bec046-e58b-4b9e-9853-c0bdeb84a5c7" . "Shane MacGowan")
("faabb55d-3c9e-4c23-8779-732ac2ee2c0d" . "Shania Twain")
("d325f5ea-dab8-4545-b186-5914f36d88c8" . "Starcastle")
("b3adc16a-abdb-4668-9b05-baabbf942db6" . "Tash Sultana")
("d15721d8-56b4-453d-b506-fc915b14cba2" . "The Black Keys")
("e5257dc5-1edd-4fca-b7e6-1158e00522c8" . "The Jacksons")
("f0c4e2c4-10f2-45d8-8189-88570e39dbe3" . "The Legend")
("d41a6875-b626-4c0f-89a1-aecb643d29ff" . "The Pogues")
("771a76c8-ca35-46bb-a80b-0ab71a29596e" . "The Sleeping Souls")
("7d8fb539-811e-4491-a3f7-f56d3ef35e7a" . "The Teskey Brothers")
("c9e99d40-4a2c-4ca7-ac5b-e842264ee271" . "The Wailers")
("db15e7aa-7b0b-400a-b8e3-dcb17e646e4a" . "The War and Treaty")
("db15e7aa-7b0b-400a-b8e3-dcb17e646e4a" . "The War & Treaty")
("a3f36ee1-c236-49eb-b598-078c788781d8" . "The White Buffalo")
("b6dd3d6e-54e4-43e3-b952-98cc9a046733" . "Thomas Raggi")
("065d44ce-48cf-4b1c-85ad-555d0345ba6e" . "Tom Hanks")
("2d0523b9-816f-4b4e-bae1-6a4beff50c0f" . "Tracy Morgan")
("b3e73938-5249-4d47-9db4-5edabbbb8c95" . "Triggerfinger")
("a41ac10f-0a56-4672-9161-b83f9b223559" . "Van Morrison")
("f1f82bfb-3464-44fc-b3d5-f225ace2d982" . "Warren Zanes")
("19857f51-ef8b-4455-9e67-a066ae340034" . "Woody Harrelson")
("fd1020c5-1dce-4e0e-9aba-7e728c143753" . "Yair Nitzani")
("f208f09e-b5b3-4b06-87cd-f7230fae17e3" . "The Gaslight Anthem")
("822b1e81-d1a0-47fb-b7f9-ef614b913603" . "Trey Anastasio Band")
("985c709c-7771-4de3-9024-7bda29ebe3f9" . "Norah Jones")
("2469950a-f0ca-425a-bfbc-baf55ea3afde" . "Kool & the Gang")
("031bc934-28c9-491a-8648-c078450187dc" . "Action Bronson")
("f47fc54d-b334-4321-8218-00c5b11d4dd1" . "Gogol Bordello")
("a2abdca1-2041-45b9-8d01-e34fdef7922f" . "Joy Oladokun")
("48f2694e-c226-4e01-a0d4-d7ea007ccd47" . "The Aces")
("ba0b9dc6-bd61-42c7-a28f-5179b1c04391" . "Eggy")
("e9177de4-27de-44b2-8aa7-c78bd5340587" . "Illiterate Light")
("a60a28e9-d4bb-4223-9e99-419f1913bd58" . "Bertha")
("85bc30f2-f9d2-4971-8fa4-1c194f4b9ef2" . "Sunshine Spazz")
("ddf327d9-1ac0-4206-b069-5c40e054934a" . "Rachel Ana Dobken")
("6f6c13d4-080e-4967-a51e-b4c27133d97f" . "Glen Burtnik")
("44975f73-6ce7-45f0-b4dd-27f1a7db3074" . "Al Carty")  ;; "Al “Boogie” Carty" on MB
("3c877dcd-ac6c-452b-9b42-34534f275a59" . "Odell Davis")
("6d0ccfcc-14b4-417f-9fff-ae0040ff639a" . "JC Maillard")  ;; "Jean‐Christophe Maillard" on MB
("2722977e-da76-408c-ade7-779f1a36170f" . "Mark Masefield")
("b3f95524-9db8-4deb-8a3a-5b2c35b7a94c" . "JaQuita May")
("08e0e925-5b24-4c9a-9dae-0d025e56f97f" . "Khadijah Islah Mohammed")  ;; Khadijah Mohammed
("a4c2a26d-4aa8-47cd-8f81-5e54da1f627e" . "Marc Muller")
("7355733a-33a5-4636-8e15-f375e1da955a" . "Reagan Richards")
("2230c8d1-a8f9-470d-ad46-70781d790776" . "Matt Wade")
("c757b4d4-5fb2-4e12-9e98-ec949e7b062a" . "The Maximum Horns")
("45208d9b-2d99-4018-8f19-aca16b9c0a74" . "Nicole DeMaio")
("d92713c3-1872-4f96-adcc-69d1efc707b0" . "Howard Freund")
("75c75041-11d8-4b9d-b724-e64f14e57489" . "John Martin")
("c5dd92ef-8fd6-44df-84d9-7b63ce0c9e99" . "Amanda Morden")
("9591045d-f411-4b1b-9b3d-442465ea197c" . "Max Morden")
("712545cc-d42f-4283-bdaf-20c282bd8778" . "The Barefoot Strings")
("2ce0769b-8af4-4af7-9c23-0258e5cc2e00" . "Linda Heffentrager")
("c96c51e8-83c1-45ec-87b0-fea0ea16b20a" . "Dana Marchioni")
("daaa4ef9-5cb4-4d41-b8ab-55422ae5a266" . "Reenat Pinchas")
("504f603c-6475-4397-b831-674425b2b03f" . "Jim Babjak")
("fe0da2ff-3c09-4b03-a592-29d006b7a1d9" . "Dennis Diken")
("3a389027-814a-4f99-8f12-44e5276595cf" . "Marshall Crenshaw")
("bc3300c0-8c5f-4ea4-a187-eed6e1711145" . "Graham Maby")
("c22b8e88-240d-4f98-a898-ac76c7c0a996" . "Joe Kearsing")
("f2222d82-9d0a-47a0-8cf0-8aba0b858f53" . "John Conte")
("0b7d3e2e-aa6a-4fa9-a378-c87ae6bf146c" . "Sara Devine")
("09730ce5-47d1-40f0-a87c-07bee84102b0" . "Steve Jankowski")
("2ce6733d-1068-4104-8ceb-c479f48280d2" . "Tania Jones")
("286b737a-71b1-4499-9c1a-9e86a47c9647" . "Rich Mercurio")
("84761ad3-8637-4772-8d9a-b0ce15e8301f" . "Brian Mitchell")
("535f0c88-70ac-4486-8aea-6e2fbb34b16c" . "Ron Tooley")
("7051f147-0f98-4679-a929-0ca365de9793" . "Jessie Wagner")  ;; "Jessica Wagner‐Cowan" in MB
)
  "Brucebase artists mentioned in gignotes.")

(defvar mb-artist-aliases '(
;; Bruce Springsteen
("70248960-cb53-4ea4-943a-edb18f7d336f" . "Bruce")
("70248960-cb53-4ea4-943a-edb18f7d336f" . "Springsteen")
;; The Castiles
("3d6009da-fb0d-4b63-8bde-47cde79dd7f5" . "Castiles")
;; Charles Giordano
("4382fa5d-03de-4ebf-baf3-df6a1d1922f0" . "Charlie")
;; Clarence Clemons
("7e4bfa5f-a8b8-4fb0-81b5-f74f6ac72133" . "Big Man")
("7e4bfa5f-a8b8-4fb0-81b5-f74f6ac72133" . "Clarence")
;; Ernest Carter
("2549e110-9933-4331-bd8d-a632f0cd639c" . "Ernest “Boom” Carter")
;; Garry Tallent
("42b42dd1-9263-4eae-91cd-4014a5b5d39f" . "Garry")
;; Gordon Vinyard
("14472e26-9503-4c62-92a9-9bea28d4508c" . "Tex Vinyard")
("14472e26-9503-4c62-92a9-9bea28d4508c" . "Gordon \"Tex\" Vinyard")
;; Jake Clemons
("5c64226c-d673-4d23-a612-2bfb704edd66" . "Jake")
;; Marion Vinyard
("d0d37e37-fcc5-425b-ae08-200f6a537e59" . "Marion")
("d0d37e37-fcc5-425b-ae08-200f6a537e59" . "Marion Joy Vinyard")
;; Mike Batlan
("9dd447fe-6c6b-4f27-b2ef-6f97550d0fad" . "Michael Mike Rasta Batlan")
;; Nils Lofgren
("a1ef6bc8-2644-4b6d-aa21-27b630acf751" . "Nils")
;; Pamela Springsteen
("23a3f71a-d1ce-424d-80c1-27598585f9fd" . "Pamela")
;; Patti Scialfa
("f09aa40c-b613-4ea2-a8cf-6056c2657a9a" . "Patti")
;; Paul McCartney
("ba550d0e-adac-4864-b88b-407cab5e76af" . "Sir Paul")
;; Rebecca Weinberg
("01c7aec0-21f3-4ae1-a757-f32be9e365ba" . "Rebecca (Becky)")
;; The Pogues
("d41a6875-b626-4c0f-89a1-aecb643d29ff" . "Pogues")
;; Ron Wood
("92ed8183-8f22-42b2-af4e-d44137610fa0" . "Ronnie Wood")
;; Roy Bittan
("11d2fcfe-669d-4596-8921-e07dbdae311f" . "Bittan")
("11d2fcfe-669d-4596-8921-e07dbdae311f" . "Roy")
;; Soozie Tyrell
("065af1a2-2fa9-4864-852e-08c00c9c67d8" . "Soozie")
;; Steven van Zandt
("de53495e-ad5a-4c30-82ab-05e7e3ec7b4d" . "Miami Steve Van Zandt")
("de53495e-ad5a-4c30-82ab-05e7e3ec7b4d" . "Steve")
("de53495e-ad5a-4c30-82ab-05e7e3ec7b4d" . "Stevie")
("de53495e-ad5a-4c30-82ab-05e7e3ec7b4d" . "Stevie Van Zandt")
;; The E Street Band
("d6652e7b-33fe-49ef-8336-4c863b4f996f" . "E Street Band")
("d6652e7b-33fe-49ef-8336-4c863b4f996f" . "The Band")
("d6652e7b-33fe-49ef-8336-4c863b4f996f" . "Band")
;; The E Street Horns
("61c0a8b8-e3f1-4e7f-9f42-7b21a7bf9e4e" . "Horn Section")
;; The Wailers
("c9e99d40-4a2c-4ca7-ac5b-e842264ee271" . "Wailers")
;; Virginia Springsteen
("06fc0de7-1647-4c0d-94bf-a6580ad0643b" . "Virginia")
)
  "Brucebase artist aliases.")

(defvar mb-artists
  (append
   mb-artists-relation
   mb-artists-mentioned
   mb-artist-aliases
   ))

(defvar mb-release-groups '(
;; Bruce Springsteen
("3c39a076-8e52-38f2-8076-260d0672fb23" . "Born in the U.S.A.")
("c497fc44-ddaf-3cce-a9b4-bfec958a0f3c" . "Greetings From Asbury Park, N.J.")
("48e335f3-8e4a-4e38-b2df-e1514b9f9126" . "Letter to You")
("324aee0c-36ec-35c1-9c93-b67a1d428037" . "Nebraska")
("d6c1b942-edc7-4bca-bd34-6c43760272af" . "Only the Strong Survive")
("02c32e8c-4748-3961-b026-8ba5943d840e" . "The Lost Masters XVI: Hollywood Hills Garage Tapes (Unreleased Masters, Volume I)")
("02c32e8c-4748-3961-b026-8ba5943d840e" . "The Lost Masters Hollywood Hills Garage Sessions")
("4b443e66-52a7-3cc9-a522-0dc64a6a51dc" . "The Wild, the Innocent & The E Street Shuffle")
("d875c2f0-27d5-4946-81f9-3ebaf6636ccd" . "Wrecking Ball")
;; The Gaslight Anthem
("88e34862-743e-4d48-bdc0-0399c2b20f73" . "History Books")
;; The Rolling Stones
("4838a3c9-fd2b-30a5-83eb-e32545b5d7fc" . "Exile on Main St.")
)
  "Musicbrainz release groups.")

(defvar mb-series-tours '(
;; Bruce Springsteen & The E Street Band
("d83e44e6-758f-4a0c-87fe-e30a7240a47e" . "Born in the U.S.A. Tour")
("64f3e25f-bfac-48bc-a79d-6acd33a031af" . "The River Tour")
("db03e180-5b76-4fae-8ca8-52ec6ac9de1f" . "2023 International Tour")
("db03e180-5b76-4fae-8ca8-52ec6ac9de1f" . "2023")
("db03e180-5b76-4fae-8ca8-52ec6ac9de1f" . "2024 Tour")
;; Bruce Springsteen and The 1992–93 World Tour Band
("31acf44d-e5e7-4147-8c0c-cb559743ddd6" . "The 1992–93 World Tour")
("31acf44d-e5e7-4147-8c0c-cb559743ddd6" . "1992–93 World Tour")  ;; alias in gignotes
;; The Rolling Stones
("f25cee65-8067-48aa-ad24-4b457c1344b4" . "50 & Counting")
("f25cee65-8067-48aa-ad24-4b457c1344b4" . "50th Anniversary Tour")
("bcbf2e57-ce31-43d3-b40c-731a100ce869" . "TW Classic")
;; Stand Up for Heroes Benefit
("a9327d10-9998-4af4-a3e8-ef95745b9423" . "Stand Up for Heroes Benefit")
("e9f4d050-a77a-438e-a75b-647aa7c96be0" . "New Jersey Hall of Fame Induction Ceremony")
;; Radio shows
("6a0ec067-aa67-485d-accb-d59efaadca79" . "E Street Radio Show")
)
  "Musicbrainz tour series.")

(defvar mb-areas '(
("10fa66f7-aa08-4823-8af8-52108f350a5a" . "Asbury Park")
("caac77d1-a5c8-3e6e-8e27-90b44dcc1446" . "Austria")
("5b8a5ee5-0bb3-34cf-9a75-c27c44e341fc" . "Belgium")
("ae0110b6-13d4-4998-9116-5b926287aa23" . "California")
("b8a2776a-eedf-48ea-a6f3-1a9070f0b823" . "Cologne")
("e0e3c82a-aea8-48d3-beda-9e587db0b969" . "Copenhagen")
("4757b525-2a60-324a-b060-578765d2c993" . "Denmark")
("462e7952-4fa9-43cd-bc24-2c5c9cd5dd47" . "Dublin")
("6658f787-692d-417f-852c-dcca728d5849" . "Edinburgh")
("9d5dd675-3cf4-4296-9e39-67865ebee758" . "England")
("89a675c2-3e37-3518-b83c-418bad59a85a" . "Europe")
("89a675c2-3e37-3518-b83c-418bad59a85a" . "European")
("42f86940-0f68-4ce0-8876-d070d424d91c" . "Freehold")
("85752fda-13c4-31a3-bee5-0e5cb1f51dad" . "Germany")
("8f6c316e-9924-48ea-967b-16757dd82399" . "Gothenburg")
("11a44e18-a2e5-43a9-bee9-aa4f7c83f967" . "Hamburg")
("1b420c08-51a5-4bdd-9b0e-cd601703d20b" . "Hawaii")
("03691455-bb46-37e3-91d2-cb064a35ffcc" . "Israel")
("c6500277-9a3d-349b-bf30-41afdbf42add" . "Italy")
("cd22d0ba-c79b-45b3-a8e0-617b240df5f0" . "Las Vegas")
("ef1b7cc0-cd26-36f4-8ea0-04d9623786c7" . "Netherlands")
("a36544c1-cb40-4f44-9e0e-7a5a69e403a8" . "New Jersey")
("a36544c1-cb40-4f44-9e0e-7a5a69e403a8" . "NJ")
("ef1b7cc0-cd26-36f4-8ea0-04d9623786c7" . "The Netherlands")
("85c7cd5f-6fe2-4195-a44d-69fa390bd6ec" . "Newark")
("6743d351-6f37-3049-9724-5041161fff4d" . "Norway")
("7342d046-e9c4-457a-9ec3-b03d4b2eb5b1" . "Oakland")
("dc10c22b-e510-4006-8b7f-fecb4f36436e" . "Paris")
("0eeb01c2-6e31-46ad-96b8-319749f731d2" . "Philadelphia")
("7f1c8f3f-69a9-454a-8633-c3d3a628858b" . "Phoenix")
("82f3a697-ba65-404d-a1ed-360147af7d10" . "San Diego")
("c3d840b4-a3d2-4565-acdc-f918be73b3d9" . "San Mateo")
("6fa1c7da-6689-4cec-85f9-680f853e8a08" . "Scotland")
("471c46a7-afc5-31c4-923c-d0444f5053a4" . "Spain")
("471c46a7-afc5-31c4-923c-d0444f5053a4" . "Spanish")
("23d10872-f5ae-3f0c-bf55-332788a16ecb" . "Sweden")
("1333ff06-8e3d-3c8e-9f3a-13a2a38b41df" . "Switzerland")
("489ce91b-6658-3307-9877-795b68554c98" . "United States")
("489ce91b-6658-3307-9877-795b68554c98" . "USA")
("afff1a94-a98b-4322-8874-3148139ab6da" . "Vienna")
)
  "Musicbrainz areas.")

(defvar mb-places '(
("12a15aee-de37-48e2-b83b-3fb4b198f822" . "Ahmanson Theatre")
("a570085a-9ae2-4fe4-9141-eccfc189cb02" . "The Boardwalk")
("a570085a-9ae2-4fe4-9141-eccfc189cb02" . "Asbury Park Boardwalk")
("5db3a73d-246f-41fe-8dfa-2e7bdf4fc0ea" . "Cinema City Hall")
("7514f7fd-9379-4f8b-b867-ee4633b6a4c4" . "Convention Hall")
("e0875bdd-0f69-45bb-b400-f498b7774364" . "East Room")
("bc5d4673-bff2-4f9a-867c-38317e4dc9ed" . "Electric Lady Studios")
("efcca572-206f-4ca6-b689-2031e5b4db0c" . "Little Caesars Arena")
("573aa838-074a-434a-870d-56b748751e97" . "Monmouth University")
("47967a50-425b-4abf-95cf-e4746a1000d5" . "Colts Neck, NJ Farmhouse")  ;; Springsteen Residence (Swimming River Reservoir)
("47967a50-425b-4abf-95cf-e4746a1000d5" . "Colts Neck Home")
("bd2bddd6-c1a2-44a7-b4f9-ff7ce03cde94" . "Hollywood Hills Residence")
("362e1557-bf27-495e-982f-783a0faaf2c7" . "Max’s Kansas City")
("e4fd123d-6ef4-4f6f-96fa-f65e10f77bbb" . "Paramount Theatre")
("62907fbf-b967-48bc-8045-3f63e540276a" . "Point Pleasant Beach")
("8aedc195-5495-4dab-a876-368ab4796c8b" . "Residence")
("3139a534-a52f-4035-9809-eb92ad51b882" . "San Diego Sports Arena")
("83f22bb6-4631-443c-bace-9fae8540362a" . "San Francisco")
("2f7a7e06-5d72-4a02-884f-58893c47f0cb" . "Studio Ferber")
("47967a50-425b-4abf-95cf-e4746a1000d5" . "The Ranch House")
("b28cdc4f-a531-480d-a6c4-3794fddd9d98" . "The Burrow")
("655fae6c-d5c1-42ba-926f-b81977b8f7d6" . "UBS Arena")
("5df4ac64-d016-4ecf-96b6-13c91aabaa00" . "White House")
("9bcf265f-5b31-4a2c-b31e-482c97c5e155" . "Windmill Lane Recording Studios")
)
  "Musicbrainz places.")

(defvar mb-labels '(
("b8d33bec-92cc-40d9-bd92-4eb089b401a9" . "CBS")
("1a4ef578-2416-4275-aa7f-bbce0861dc8f" . "ACUM")  ;; Israeli rights society
)
  "Musicbrainz labels.")

(defvar mb-fixworks '(
;; special work titles to fix
("(What’s So Funny ’bout) Peace, Love and Understanding" . "(What’s So Funny ’Bout) Peace, Love and Understanding")
("57 Channels (And Nothin’ On)" . "57 Channels (and Nothin’ On)")
("A Winter’s Revelation (In 9 Illusions)" . "A Winter’s Revelation (in 9 Illusions)")
("Armed &Amp; Dangerous (with Love)" . "Armed & Dangerous (with Love)")
("Baby &Amp; Me (Blondie)" . "Baby & Me (Blondie)")
("Balboa Vs. the Earth Slayer" . "Balboa vs. The Earth Slayer")
("Break on Through (To the Other Side)" . "Break On Through (to the Other Side)")
("Calvin Jones &Amp; the 13th Apostle" . "Calvin Jones & The 13th Apostle")
("Come On Over to My Place" . "Come on Over to My Place")
("Come On Billy (Break Out the Wine)" . "Come on Billy (Break Out the Wine)")
("Come on (Let’s Go Tonight)" . "Come On (Let’s Go Tonight)")
("Come on" . "Come On")
("Devils &Amp; Dust" . "Devils & Dust")
("Double Shot (Of My Baby’s Love)" . "Double Shot (of My Baby’s Love)")
("Freak Ii" . "Freak II")
("(Get Your Kicks On) Route 66" . "(Get Your Kicks on) Route 66")
("Got to Get You off My Mind" . "Got to Get You Off My Mind")
("Have a Good Time (But Get Out Alive)" . "Have a Good Time (but Get Out Alive)")
("Hold on (To What You Got)" . "Hold on (to What You Got)")
("I Still Look Good (For Sixty)" . "I Still Look Good (for Sixty)")
("Lonesome Train (On a Lonesome Track)" . "Lonesome Train (on a Lonesome Track)")
("(Love Is Like A) Heat Wave" . "(Love Is Like a) Heat Wave")
("Mrs. Mcgrath" . "Mrs. McGrath")
("Paradise by the ’’c’’" . "Paradise by the “C”")
("Paradise by the &Quot;C&Quot;" . "Paradise by the “C”")
("Peg O’ My Heart" . "Peg o’ My Heart")
("Kt-88" . "KT-88")
("Sociedade Alternativa" . "Sociedade alternativa")
("Sólo Le Pido a Dios" . "Sólo le pido a Dios")
("Tenth Avenue Freeze-Out" . "Tenth Avenue Freeze‐Out")
("Tv Movie" . "TV Movie")
("You Can Look (But You Better Not Touch)" . "You Can Look (but You Better Not Touch)")
("When I Grow Up (To Be a Man)" . "When I Grow Up (to Be a Man)")
("When the Saints Go Marching in" . "When the Saints Go Marching In")
("Whole Lotta Shakin’ Goin’ on" . "Whole Lotta Shakin’ Goin’ On")
("Zip-A-Dee-Doo-Dah" . "Zip-a-Dee-Doo-Dah")
))

(defvar brucebase-studio-sessions-list '(
("Castiles-SteelMill-BruceSpringsteenBand" . "/Castiles%2C+Steel+Mill%2C+BS+Band+-+Studio+Sessions")
("Demo" . "/Demo+-+Studio+Sessions")
("GreetingsFromAsburyPark" . "/Greetings+From+Asbury+Park+-+Studio+Sessions")
("TheWildTheInnocentAndTheEStreetShuffle" . "/The+Wild%2C+The+Innocent+%26+The+E+Street+Shuffle+-+Studio+Sessions")
("BornToRun" . "/Born+To+Run+-+Studio+Sessions")
("DarknessOnTheEdgeOfTown" . "/Darkness+On+The+Edge+Of+Town+-+Studio+Sessions")
("1979-Solo" . "/1979+solo+-+Studio+Sessions")
("TheRiver" . "/The+River+-+Studio+Sessions")
("1982-Solo" . "/1981+solo+-+Studio+Sessions")
("Nebraska" . "/Nebraska+-+Studio+Sessions")
("BornInTheUSA" . "/Born+In+The+USA+-+Studio+Sessions")
("TunnelOfLove" . "/Tunnel+Of+Love+-+Studio+Sessions")
("HumanTouch" . "/Human+Touch+-+Studio+Sessions")
("LuckyTown" . "/Lucky+Town+-+Studio+Sessions")
("1993-Solo" . "/1993+solo+-+Studio+Sessions")
("GreatestHits" . "/Greatest+Hits+-+Studio+Sessions")
("TheGhostOfTomJoad" . "/The+Ghost+Of+Tom+Joad+-+Studio+Sessions")
("Tracks" . "/Tracks+-+Studio+Sessions")
("TheRising" . "/The+Rising+-+Studio+Sessions")
("TheEssential" . "/The+Essential+-+Studio+Sessions")
("DevilsAndDust" . "/Devils+%26+Dust+-+Studio+Sessions")
("SeegerSessions" . "/The+Seeger+Sessions+-+Studio+Sessions")
("Magic" . "/Magic+-+Studio+Sessions")
("WorkingOnADream" . "/Working+On+A+Dream+-+Studio+Sessions")
;; No additional song info
;;("ThePromise" . "/The+Promise+-+Studio+Sessions")
("WreckingBall" . "/Wrecking+Ball+-+Studio+Sessions")
("HighHopes" . "/High+Hopes+-+Studio+Sessions")
;; Check now and then
;;("TheTiesThatBind" . "/The+Ties+That+Bind+-+Studio+Sessions")
)
;"Alist of Springsteen studio session pages at Brucebase"
)

(defvar brucebase-well-known-titles "^* \\(Loose End\\|Song Title\\|The Mark\\|Waitin’ for an Angel\\|Do (You) Want Me to Say All Right\\|The Glory of Love\\)")

(global-set-key (kbd "<f6>") 'vz-surround-with-quotes-italic)
(global-set-key (kbd "<f7>") 'vz-mb-urlify-gignote)
(global-set-key (kbd "<f8>") 'vz-surround-with-quotes)

(provide 'vz-bruce)
;;; vz-bruce.el ends here
