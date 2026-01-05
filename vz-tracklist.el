;; 1. Define the association list as a constant variable
;; (defconst my-tracklist-replacement-map
;;   '(("\\b[Ii]ntro\\b" . "[intro]")    ; Matches Intro or intro as a whole word (case-insensitive)
;;     ("\\b[Ss]peech\\b" . "[speech]")) ; Matches Speech or speech as a whole word (case-insensitive)
;;   "Association list for tracklist metadata replacements.
;; Keys are regex patterns (case-insensitive and using word boundaries) and values are the replacement strings.")

(defconst my-tracklist-replacement-map
  '(
    ("Bruce talk" . "[Bruce talk]")
    ("[Ii]ntro" . "[intro]")
    ("[Ss]peech" . "[speech]")
    ("[Ii]ntroduction" . "[introduction]")
    )
  "Association list for tracklist metadata replacements.
Keys are regex patterns (case-insensitive and using word boundaries) and values are the replacement strings.")

(defun my--replace-metadata-in-region (replacement-alist)
  "Performs replacements within the current region based on the given ALIST.
Each element of ALIST should be a (REGEX_PATTERN . REPLACEMENT_STRING) pair.
The function expects the buffer to be narrowed to the region of interest.
Returns the total count of replacements made."
  (let ((total-count 0))
    ;; Loop through the association list and perform replacements
    (dolist (item replacement-alist)
      (let ((pattern (car item))
            (replacement (cdr item)))
        ;; replace-regexp returns the number of replacements made, but might return nil if interrupted/error.
        ;; We use (or ... 0) to ensure a numerical value is always added to total-count.
        (setq total-count 
              (+ total-count 
                 (or (replace-regexp pattern replacement nil (point-min) (point-max)) 0)))))
    total-count))

(defun my-tracklist-fix-metadata ()
  "Parses the active region and replaces occurrences based on the
'my-tracklist-replacement-map' association list, using the 
'my--replace-metadata-in-region' helper function. 
Operates only on the currently active region."
  (interactive)
  (if (region-active-p)
      (save-excursion
        ;; save-restriction saves and restores the buffer's narrowing state.
        (save-restriction
          ;; 1. Restrict the buffer to the current region
          (narrow-to-region (region-beginning) (region-end))

          ;; 2. Call the helper function with the global alist
          (let ((total-count (my--replace-metadata-in-region my-tracklist-replacement-map)))
            
            ;; 3. Report the total number of replacements
            (message "Tracklist fixed: Replaced %d occurrence(s) of metadata tags." total-count))))
    ;; Error if no region is selected
    (error "Region must be active to run this command (C-x C-x to activate/deactivate)")))


;; Write an emacs lisp fuction which prepends tracklist numbers (when lower than 10 prefix with 0) to a region of tracks like the following

(defun my-tracklist-number-region ()
  "Prepends sequential track numbers (01-, 02-, 03-, etc.) to each line
in the active region. Numbers are zero-padded up to 99."
  (interactive)
  (if (region-active-p)
      (save-excursion
        (save-restriction
          ;; Restrict the buffer to the current region
          (narrow-to-region (region-beginning) (region-end))
          ;; Start at the beginning of the restricted area
          (goto-char (point-min))
          (let ((track-num 1)
                (count 0))
            
            ;; Loop while the cursor is not at the end of the restricted buffer
            (while (not (eobp))
              ;; Move to the very start of the current line
              (beginning-of-line)
              
              ;; Format the number with zero-padding (e.g., 1 -> "01- ")
              (let ((number-string (format "%02d- " track-num)))
                ;; Insert the formatted string
                (insert number-string)
                
                ;; Increment counters
                (setq track-num (1+ track-num))
                (setq count (1+ count)))

              ;; Move to the beginning of the next line (or EOB if last line)
              (forward-line 1))

            (message "Added numbers to %d track(s)." count))))
    ;; Error if no region is selected
    (error "Region must be active to run this command (C-x C-x to activate/deactivate)")))

(defun my-tracklist-merge-blocks ()
  "Merges two adjacent blocks of text in the region, assuming the first block
contains Titles (e.g., '01- Title') and the second contains Timings 
(e.g., '01 (1:02)'). The function appends the time string from the second 
block to the corresponding line in the first block."
  (interactive)
  (if (not (region-active-p))
      (error "Region must be active to run this command (C-x C-x to activate/deactivate)"))
  
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      
      (goto-char (point-min))
      
      ;; 1. Find the delimiter (first empty line)
      (if (re-search-forward "^[ \t]*$" nil t)
          (let ((split-point (point)))
            
            ;; 2. Extract Block A (Titles) and Block B (Timings)
            (let* ((block-a (buffer-substring (point-min) split-point))
                   (lines-a (split-string block-a "\n" t)) ; Titles
                   (block-b (buffer-substring split-point (point-max)))
                   (lines-b (split-string block-b "\n" t)) ; Timings
                   (merged-lines ()))

              (if (/= (length lines-a) (length lines-b))
                  (error "The two blocks must have the same number of non-empty lines: %d vs %d" 
                         (length lines-a) (length lines-b)))

              ;; 3. Merge and reformat
              (dotimes (i (length lines-a))
                (let* ((line-a (nth i lines-a)) ; e.g., "01- [intro]"
                       (line-b (nth i lines-b)) ; e.g., "01 (1:02)"
                       ;; Regex to find the time string including parentheses, e.g., "(1:02)"
                       (time-match (string-match "(\\([0-9]+\\):[0-9][0-9])" line-b)) 
                       (time-string (if time-match (match-string 0 line-b) "")))
                  
                  (if (not (string= time-string ""))
                      ;; Concatenate: "01- [intro]" + " (1:02)"
                      (setq merged-lines (cons (concat line-a " " time-string) merged-lines))
                    (error "Error: Time format not found in line: %s" line-b))))
              
              ;; 4. Replace original region content with merged content
              (erase-region (region-beginning) (region-end))
              (insert (mapconcat 'identity (nreverse merged-lines) "\n"))
              (message "Merged %d track pairs." (length merged-lines)))
        
        (error "Could not find a blank line separating the two blocks in the selected region.")))))
