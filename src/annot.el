;;; annot.el --- a global annotator/highlighter for GNU Emacs

;; Copyright (C) 2010 tkykhs

;; Author:     tkykhs <tkykhs@gmail.com>
;; Maintainer: tkykhs
;; Created:    March 28, 2010
;; Keywords:   annotations

;; This file is not (yet) part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Description:
;;
;; `annot.el' is a general and scalable annotation manager that works on GNU
;; Emacs.  It lets you add/remove/edit annotations and highlights on any file
;; and manages them for later use. All annotations are stored separately for
;; each annotated file and get reproduced when the file is opened again. You can
;; even store annotations on non-editable files.  Because annot keeps track of
;; md5 checksums of annotated files, annotations won't disappear even when file
;; names are changed.
;; 
;; Requirement:
;; 
;; GNU Emacs 23 or higher.
;; 
;; Installation:
;;
;; Insert the following line to your .emacs:
;;
;;  (require 'annot)
;; 
;; Keybindings:
;;
;; * [C-x a]    -  add a new annotation/highlight or edit an existing annotation/highlight.
;;                 You can also use [C-x C-a]. (annot-edit/add)
;; * [C-x r]    -  remove annotation at point. (annot-remove)
;; * [C-x w]    -  insert an image at point. (annot-add-image)
;;
;; User Commands:
;; 
;; * `annot-edit/add'  - either edit the annotation at point, if there is,
;;                       or else add a new annotation or highlight.
;; * `annot-remove'    - remove the annotation/highlight at point.
;; * `annot-add'       - add a new annotation/highlight at point.
;; * `annot-edit'      - edit the annotation at point.
;; * `annot-add-image' - insert an image at point.

;;; Todo:

;; * Sticky recovery when :pos does not match:
;;   - sort annotations (a list of ov-plist) first
;;     (setq annotations (sort annotations
;;                         (lambda (op1 op2)
;;                           (< (or (plist-get op1 :beg) (plist-get op1 :pos))
;;                              (or (plist-get op2 :beg) (plist-get op2 :pos))))))
;;   - if md5 is the same, just do the normal integrity checking.
;;   - if not, do the normal integrity checking upto the point where the check fails first.
;;   - from the last successful position (:beg or :pos), iterate the following:
;;     - search for :next subsequence from the last successful position. if found, check :prev at the point.
;;     - if matched, create an overlay for it and mark the position as successful.
;; * Kill-ring support: `kill-region', `yank'
;; * Primitive undo management.

;;; Code:

(defgroup annot nil
  "Annotation manager in Emacs."
  :prefix "annot-"
  :group 'tools)

(defcustom annot-directory "~/.annot"
  "Directory under which all annotations are, and will be, saved."
  :type  'string
  :group 'annot)

(defcustom annot-text-decoration-function 'annot-decorate-text
  "Function to decorate an annotation text."
  :type  'symbol
  :group 'annot)

(defcustom annot-md5-max-chars 300000
  "Max number of characters to sample for getting the md5 checksum.
DO NOT MODIFY THIS VARIABLE WHEN YOU HAVE ANNOTATIONS YOU DO NOT
WANT TO LOSE. Depending on what you specify here, some md5's of
annotated files might also change \(for long files especially),
resulting in loss of annotations.  Sampling is from the top of
the buffer. Keep in mind that there is a speed-reliability
tradeoff here."
  :risky t
  :type '(choice (const   :tag "No limit" nil)
                 (integer :tag "Number of chars"))
  :group 'annot)

(defcustom annot-load-disabled-modes '(hexl-mode)
  "A list of major-modes for which file-buffer annot load should not take place."
  :type '(repeat (symbol :tag "Major mode"))
  :group 'annot)

(defcustom annot-enable-symlinking nil
  "Whether to enable symlink support.
Depending on your editing style, this makes annot more robust in
reproducing annotations."
  :type 'boolean
  :group 'annot)

(defcustom annot-enable-strict-integrity-checking nil
  "If enabled, check the forward string as well.
By default, annot only checks backward subsequence relative to
annotation's position."
  :type 'boolean
  :group 'annot)

(defcustom annot-image-directory "~/" 
  "Default image directory for `annot-add-image'."
  :type 'string
  :group 'annot)

(defface annot-text-face
  '((((class color) (background light)) (:foreground "red" :background "yellow"))
    (((class color) (background dark)) (:foreground "red" :background "white")))
  "Face for annotation text."
  :group 'annot)

(defface annot-highlighter-face
  '((((class color) (background light)) (:background "yellow"))
    (((class color) (background dark)) (:foreground "white" :background "red2")))
  "Face for highlighted text."
  :group 'annot)

(defconst annot-subsequence-length 24
  "Number of characters to be stored before and after \
annotation's position in a file buffer.")

(defconst annot-contents-dirname "contents"
  "Name of the annotation directory.")

(defconst annot-symlinks-dirname "symlinks"
  "Name of the annotation symlink directory.")

(defvar annot-buffer-overlays nil
  "List of overlays in the buffer.")
(make-variable-buffer-local 'annot-buffer-overlays)

(defvar annot-buffer-plist nil
  "Plist containing annotation information in the buffer.
Unfortunately `make-variable-buffer-local' does not make the
symbol plist buffer-local; so this variable has to exist
separately.")
(make-variable-buffer-local 'annot-buffer-plist)


;;; User commands.

(defun annot-add (&optional text/image)
  "Add an annotation on the current point.
If a marked region is present, highlight it."
  (interactive)
  (let* ((text/image/region (if (region-active-p)
                                `(,(region-beginning) . ,(region-end))
                              (or text/image
                                  (read-string "Annotation: "))))
         (ov-list (annot-create-new text/image/region)))
    (when ov-list
      (dolist (ov ov-list)
        (push ov annot-buffer-overlays))
      (annot-save-annotations)
      ;; When on indirect buffer, sync with its base buffer as well.
      (annot-base-buffer-add text/image/region))))


(defun annot-add-image (&optional image-filename)
  "Insert an image on the current point."
  (interactive)
  (if (and window-system (display-images-p))
      (let ((image-filename
             (or image-filename
                 (car (let ((default-directory
                              (or (and (file-directory-p annot-image-directory)
                                       annot-image-directory)
                                  default-directory)))
                        (find-file-read-args "Image: " t))))))
        (annot-add (propertize image-filename 'display
                               (create-image (expand-file-name image-filename)))))
    (message "You are not on a window-system that can display images.")))


(defun annot-edit (&optional ov)
  "Edit a nearby annotation on the current line."
  (interactive)
  (let ((p (point))
        (ov (or ov (annot-get-annotation-at-point))))
    (cond
     ((null ov)
      (message "No annotation to edit at point."))
     ((region-active-p)
      (message "Highlight cannot be edited."))
     (t 
      (let ((text (read-string "Annotation: "
                               (annot-trim
                                (substring-no-properties
                                 (or (overlay-get ov 'before-string) ""))))))
        (if (zerop (length (annot-trim text)))
            (annot-remove ov)
          (overlay-put ov 'before-string
                       (funcall annot-text-decoration-function text))
          (overlay-put ov :modtime (float-time))
          (annot-save-annotations)
          (annot-base-buffer-edit text)))))))


(defun annot-remove (&optional ov silent)
  "Remove a nearby annotation on the current line.
If a regioin is specified, remove all annotations and highlights within it."
  (interactive)
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (deactivate-mark t)    ;; Avoid endless recursion.
        (annot-delete-annotations-region beg end))
    (let ((ov (or ov (annot-get-annotation-at-point))))
      (when ov
        (setq annot-buffer-overlays (delq ov annot-buffer-overlays))
        (delete-overlay ov)
        (annot-save-annotations)
        (annot-base-buffer-remove)
        (unless silent
          (message "Annotation removed."))))))


(defun annot-edit/add ()
  "Either edit the annotation at point, if there is, or else add a new one.
If a region is specified, a highlight annotation will be added or edited."
  (interactive)
  (let (ov)
    (cond
     ((region-active-p)
      (annot-add)
      (deactivate-mark t))
     ((and (setq ov (annot-get-annotation-at-point))
           (not (annot-highlight-p ov)))
      (annot-edit ov))
     (t (annot-add)))))


(defun annot-load-annotations ()
  "Load the annotation file corresponding to the current buffer.
If current `annot-buffer-overlays' looks newer \(which shouldn't
happen as long as you keep using annot), it asks whether to load
the file or not."
  (interactive)
  (unless (member major-mode annot-load-disabled-modes)
    (let (filename)
      (if (or (file-readable-p 
               (setq filename (annot-get-annot-filename (annot-md5 (current-buffer)))))
              ;; If md5 fails, try symlink.
              (and (setq filename (annot-get-symlink (buffer-file-name)))
                   (file-readable-p filename)))
          (load-file filename)))))


;;; Functions for synching up with an indirect buffer's annotations (but not the
;;; oppositve - i.e. upward direction only).  This supports no more than single
;;; indirect buffer: multiple indirect buffers pointing to the same base buffer
;;; may not be fully reflected.

(defun annot-base-buffer-add (text/image/region)
  (let ((base-buffer (buffer-base-buffer)))
    (when base-buffer
      (let ((p (point)))
        (with-current-buffer base-buffer
          (save-excursion
            (goto-char p)
            (let ((ov-list (annot-create-new text/image/region)))
              (when ov-list
                (dolist (ov ov-list)
                  (push ov annot-buffer-overlays))
                (annot-save-annotations)))))))))


(defun annot-base-buffer-remove ()
  (let ((base-buffer (buffer-base-buffer)))
    (when base-buffer
      (let ((p (point)) ov)
        (with-current-buffer base-buffer
          (save-excursion
            (goto-char p)
            (when (setq ov (annot-get-annotation-at-point))
              (setq annot-buffer-overlays (delq ov annot-buffer-overlays))
              (delete-overlay ov)
              (annot-save-annotations))))))))


(defun annot-base-buffer-edit (text)
  (let ((base-buffer (buffer-base-buffer)))
    (when (and base-buffer (not (zerop (length (annot-trim text)))))
      (let ((p (point)))
        (with-current-buffer base-buffer
          (save-excursion
            (goto-char p)
            (overlay-put (annot-get-annotation-at-point) 'before-string
                         (funcall annot-text-decoration-function text))
            (annot-save-annotations)))))))


;;; Internal functions.

(defsubst annot-trim (s)
  "Trim non-graphic chars from both ends of string s." 
  (replace-regexp-in-string
   "\\`[^[:graph:]]+\\|[^[:graph:]]+\\'"  "" s))


(defsubst annot-contents-directory ()
  (format "%s/%s" annot-directory annot-contents-dirname))


(defsubst annot-symlinks-directory ()
  (format "%s/%s" annot-directory annot-symlinks-dirname))


(defsubst annot-get-annot-filename (md5)
  "Return the full path of the annotation filename."
  (expand-file-name (format "%s/%s"
                            (annot-contents-directory) md5)))


(defsubst annot-md5 (&optional buffer)
  "Get md5 of the buffer content with max chars `annot-md5-max-chars'.
If `annot-md5-max-chars' is nil, no limit is imposed." 
  (let ((buffer (or buffer (current-buffer))))
    (md5 buffer nil
         (if (null annot-md5-max-chars)
             nil
           (min annot-md5-max-chars (point-max)))
         nil t)))


(defsubst annot-argmax (L fn)
  (let* ((best (car L))
         (best-score (funcall fn best)) score)
    (dolist (e L)
      (when (> (setq score (funcall fn e))
               best-score)
        (setq best-score score
              best e)))
    best))


(defsubst annot-highlight-p (ov)
  "non-nil if an overlay `ov' is of type highlight."
  (equal (overlay-get ov :type) 'highlight))


(defun annot-create-new (text/image/region)
  "Create a new list of overlay(s) depending of the content of `text/image/region'.
In particular, a text highlight may yield multiple overlays depending on
the region ends."
  (cond
   ((or (null text/image/region)
        (stringp text/image/region))
    (let ((text/image (or text/image/region (read-string "Annotation: "))))
      (unless (zerop (length (annot-trim text/image)))
        (list (annot-create-overlay (point) text/image)))))
   ((listp text/image/region)
    (let ((beg (car text/image/region))
          (end (cdr text/image/region))
          (modtime (float-time))  ov-list a b)
      (save-excursion
        (goto-char beg)
        (while (and
                (< (point) end)
                (re-search-forward "[[:graph:]]" end t)
                (setq a (goto-char (match-beginning 0))))
          (if (and
               (re-search-forward "[[:graph:]][^[:graph:]]*?$" end t)
               (setq b (goto-char (1+ (match-beginning 0)))))
              (push (annot-create-highlight-overlay a b modtime) ov-list)
            (goto-char end)
            (save-excursion
              (when (re-search-backward "[[:graph:]]" beg t)
                (push (annot-create-highlight-overlay a (match-end 0) modtime) ov-list))))))
      ov-list))))


(defun annot-file-exists-p ()
  "Returns an annotation filename if current buffer accompanies with it."
  (let ((annot-filename (annot-get-annot-filename (annot-md5 (current-buffer)))))
    (and (file-exists-p annot-filename) annot-filename)))


(defun annot-decorate-text (text)
  "Decorate an annotation text."
  (let* ((n (length text))
         (last-newline-p (string= (substring text (max 0 (1- n)) n) "\n"))
         (text (annot-trim text))
         (text-is-image-p (get-text-property 0 'display text))
         (annot-text (format "%s%s%s"
                             (if text-is-image-p "" " ")
                             text
                             (or (and last-newline-p "\n")
                                 (and text-is-image-p "")
                                 " "))))
    (format "%s%s" (if (bolp) "" " ")
            (propertize annot-text 'face 'annot-text-face))))


(defun annot-get-annotation-at-point ()
  "Get annotation \(equiv. overlay) at point, or if none found, 1+ point." 
  (let ((p (point)) ovs)
    (car (or (overlays-in p p)
             (overlays-in (min (point-max) (1+ p))
                          (min (point-max) (1+ p)))
             (and
              (setq ovs (overlays-in (max (point-min) (1- p)) p))
              (annot-highlight-p (car ovs))
              ovs)))))


(defun annot-create-overlay (pos text/image)
  "Create a text overlay or image overlay."
  ;; (assert (not (zerop (length (annot-trim text/image)))))
  (let ((ov (make-overlay pos pos nil nil nil)))
    (overlay-put ov 'before-string
                 (funcall annot-text-decoration-function text/image))
    (overlay-put ov :pos pos)    ;; it will be saved later anyways...
    (overlay-put ov :prev (buffer-substring-no-properties
                           (max (point-min) (- pos annot-subsequence-length)) pos))
    (overlay-put ov :next (buffer-substring-no-properties
                           pos (min (point-max) (+ pos annot-subsequence-length))))
    (if (get-text-property 0 'display text/image)
        (overlay-put ov :type 'image)
      (overlay-put ov :type 'text))
    (overlay-put ov :modtime (float-time))
    ov))


(defun annot-create-highlight-overlay (beg end &optional modtime)
  "Create a highlight overlay starting from `beg' to `end'."
  (let ((ov (make-overlay beg end nil nil t)))
    (overlay-put ov 'face 'annot-highlighter-face)
    (overlay-put ov 'evaporate t)
    (overlay-put ov :prev (buffer-substring-no-properties
                           (max (point-min) (- beg annot-subsequence-length)) beg))
    (overlay-put ov :next (buffer-substring-no-properties
                           end (min (point-max) (+ end annot-subsequence-length))))
    (overlay-put ov :beg beg)
    (overlay-put ov :end end)
    (overlay-put ov :type 'highlight)
    (overlay-put ov :modtime (or modtime (float-time)))
    ov))


(defun annot-format-overlays (md5 filename bufname annot-filename modtime)
  "Generate a string containing all information necessary to reproduce annotations."
  (let ((ov-plists-s
         (let ((print-escape-newlines t))    ;; newline is represented as "\n"
           (mapconcat (lambda (ov)
                        (format "%S" (overlay-properties ov)))
                      annot-buffer-overlays "\n"))))
    (format ";;; -*- mode: emacs-lisp -*-
\(annot-recover-annotations '\(
:md5 %S
:filename %S
:bufname %S
:annot-filename %S
:modtime %S
:annotations \(
%s
)))" md5 filename bufname annot-filename modtime ov-plists-s)))


(defun annot-save-annotations ()
  "Save all annotations in the buffer, updating all information.
It updates `annot-buffer-plist' information as well.
If the new filename \(or equivalently md5) is different from
previous filename, return delete the previous file."
  (setq annot-buffer-plist
        (plist-put annot-buffer-plist :modtime (float-time)))
  
  (cond
   ;; In case no annotations are left, delete the
   ;; associated annot file.
   ((null annot-buffer-overlays)
    (let ((md5 (plist-get annot-buffer-plist :md5))
          (filename (plist-get annot-buffer-plist :filename))
          old-annot-filename symlink)
      (when md5
        (setq old-annot-filename (annot-get-annot-filename md5))
        (if (file-readable-p old-annot-filename)
            (delete-file old-annot-filename)))
      (when filename
        (setq symlink (annot-get-symlink filename))
        (if (file-symlink-p symlink)
            (delete-file symlink)))))
   
   (t
    (let* ((buffer (current-buffer))
           (prev-md5 (plist-get annot-buffer-plist :md5))
           (prev-filename (plist-get annot-buffer-plist :filename))
           (md5 (annot-md5 buffer))
           (filename (buffer-file-name))
           (annot-filename (annot-get-annot-filename md5))
           (modtime (or (plist-get annot-buffer-plist :modtime) (float-time)))
           (bufname (buffer-name)) s)
      ;; Update ((:beg/:end)|:pos) and :prev/:next of each overlay
      (dolist (ov annot-buffer-overlays)
        (let ((beg (overlay-start ov))
              (end (overlay-end ov)))
          (if (annot-highlight-p ov)
              (progn
                (overlay-put ov :beg beg)
                (overlay-put ov :end end))
            (overlay-put ov :pos end))
          (overlay-put ov :prev
                       (buffer-substring-no-properties
                        (max (point-min) (- beg annot-subsequence-length)) beg))
          (overlay-put ov :next
                       (buffer-substring-no-properties
                        end (min (point-max) (+ end annot-subsequence-length))))))
      
      ;; Delete previous symlink, if any, before creating one.
      (when (and prev-md5 prev-filename (not (string= prev-md5 md5)))
        (if (file-symlink-p (annot-get-symlink prev-filename))
            (delete-file (annot-get-symlink prev-filename))))
      
      ;; Get the S-expression and save the annotations if it is a file-buffer.
      ;; It is possible to load annotations for non-file-buffer, but it is
      ;; not supported yet just to play safe.
      (when filename
        (setq s (annot-format-overlays md5 filename bufname annot-filename modtime))
        (condition-case error
            (progn
              (annot-save-content s annot-filename)
              (when annot-enable-symlinking
                (annot-save-symlink md5 filename)))
          (error
           (warn "annot-save-annotations: %s" (error-message-string error)))))
      
      ;; Update `annot-buffer-plist'
      (dolist (e '(md5 filename bufname annot-filename modtime))
        (setq annot-buffer-plist
              (plist-put annot-buffer-plist
                         (intern (format ":%S" e)) (symbol-value e))))
      
      ;; If md5 doesn't match the previous one, and both files
      ;; refer to the same file, delete the old annotation file.
      ;; In the case of
      ;; $ cp a b ; emacs b   # then modify b and maybe add/edit/remove annotation/highlight.
      ;; we still want to keep annotations for a (and b).
      ;; On the other hand, we do not want to keep obsolete annotations
      ;; if prev and current versions both point to the same file.
      (when (and prev-md5
                 (not (string= prev-md5 md5))
                 (string= prev-filename filename))
        (let ((old-annot-filename (annot-get-annot-filename prev-md5)))
          (if (file-readable-p old-annot-filename)
              (delete-file old-annot-filename))))
      annot-filename))))


(defun annot-save-content (content annot-filename)
  "Write `content' into a `annot-filename'.
Create the annot content directory if it does not exist."
  (unless (file-exists-p (annot-contents-directory))
    (make-directory (annot-contents-directory) t))
  (with-temp-file annot-filename
    (erase-buffer)
    (insert content)))


(defun annot-save-symlink (md5 filename)
  "Make a symbolic link pointing to an annot-filename."
  (let ((symlinks-dir (annot-symlinks-directory)))
    (unless (file-exists-p symlinks-dir)
      (make-directory symlinks-dir))
    (make-symbolic-link
     (format "../%s/%s" annot-contents-dirname md5)
     (annot-get-symlink filename)
     'ok-if-already-exists)))


(defun annot-get-symlink (filename)
  "Return symlink path."
  (when filename
  (let ((backup-directory-alist `(("." . ,(annot-symlinks-directory)))))
    (make-backup-file-name (expand-file-name filename)))))


(defun annot-recover-annotations (annotations-info)
  "Recover annotations.
Only annotation files use this function internally."
  (let ((annotations (plist-get annotations-info :annotations))
        (modtime     (plist-get annotations-info :modtime))
        (var-modtime (plist-get annot-buffer-plist :modtime)))
    (when (or (null var-modtime)
              (not (< modtime var-modtime))
              ;; If annot-buffer-overlays has updated modtime, ask.
              (y-or-n-p (concat "Modification time for stored annotations"
                                " appears be older. Load them anyways? ")))
      ;; If by any chance `annot-buffer-overlays' contains some overlays,
      ;; delete them all.
      (when annot-buffer-overlays
        (dolist (ov annot-buffer-overlays)
          (delete-overlay ov))
        (setq annot-buffer-overlays nil))
      ;; Mmmkay, let's reproduce annotations.
      (dolist (ov-plist annotations)
        (let ((prev-string (plist-get ov-plist :prev)) beg end)
          (if (setq beg (plist-get ov-plist :beg))
              (setq end (plist-get ov-plist :end))
            (setq beg (plist-get ov-plist :pos)
                  end beg))
          ;; Integrity checking:
          (when (and
                 (if (equal (plist-get ov-plist :type) 'highlight)
                     (< (plist-get ov-plist :beg) (plist-get ov-plist :end))
                   (> (length (plist-get ov-plist 'before-string)) 0))
                 (string= prev-string
                          (buffer-substring-no-properties
                           (max (point-min) (- beg (length prev-string))) beg))
                 (or (null annot-enable-strict-integrity-checking)
                     (and annot-enable-strict-integrity-checking
                          (let ((next-string (plist-get ov-plist :next)))
                            (string= next-string
                                     (buffer-substring-no-properties
                                      end (min (point-max)
                                               (+ end (length next-string)))))))))
            ;; OK to reproduce.
            (push (annot-recover-overlay ov-plist beg end) annot-buffer-overlays))))
      (setq annot-buffer-plist
            `(:md5 ,(plist-get annotations-info :md5)
                   :filename       ,(plist-get annotations-info :filename)
                   :bufname        ,(plist-get annotations-info :bufname)
                   :annot-filename ,(plist-get annotations-info :annot-filename)
                   :modtime        ,modtime)))))


(defun annot-recover-overlay (ov-plist beg end)
  "Recover an overlay."
  (let* ((ov (make-overlay beg end nil nil
                           (equal (plist-get ov-plist :type) 'highlight))))
    (dotimes (i (length ov-plist))
      (when (eq (logand i 1) 0)
        (overlay-put ov (nth i ov-plist) (nth (1+ i) ov-plist))))
    ov))


(defun annot-delete-annotations-region (r-beg r-end)
  "Delete all annotations in region."
  (interactive "r")
  (dolist (ov annot-buffer-overlays)
    (let ((beg (overlay-start ov))
          (end (overlay-end ov)))
      (when (or (null beg) (null end)
                (and (>= r-end end) (>= beg r-beg)))
        (annot-remove ov t)))))


;;; Keybindings.

(define-key ctl-x-map "a"    'annot-edit/add)
(define-key ctl-x-map "\C-a" 'annot-edit/add)
(define-key ctl-x-map "r"    'annot-remove)
(define-key ctl-x-map "w"    'annot-add-image)


;;; Hooks and Advices.

(defvar annot-buffer-modified-p nil)
(make-variable-buffer-local 'annot-buffer-modified-p)

(defun annot-before-save-hook ()
  (setq annot-buffer-modified-p (buffer-modified-p)))
(add-hook 'before-save-hook 'annot-before-save-hook)

(defun annot-after-save-hook ()
  (when annot-buffer-modified-p
    (annot-save-annotations)
    ;; Let's make it a rule that if the current buffer is modified and the last
    ;; annotation in the buffer is of the form "after-save: <s-exp>", then
    ;; evaluate <s-exp> just after save-buffer.
    (let (last-ov s s-exp)
      (when (and annot-buffer-overlays
                 (setq last-ov (annot-argmax annot-buffer-overlays
                                             (lambda (ov) (overlay-start ov))))
                 (setq s (overlay-get last-ov 'before-string))
                 (string-match "\\` *after-save: *\\(.+\\)" s))
        (message "'after-save' annotation found. Evaluating: %S"
                 (setq s-exp (match-string 1 s)))
        (eval (read s-exp)))))
  (setq annot-buffer-modified-p nil))
(add-hook 'after-save-hook 'annot-after-save-hook)
(add-hook 'find-file-hook 'annot-load-annotations)

(defadvice delete-region (before annot-delete-region activate)
  "Enable deletion of annotations within the specified region."
  (annot-delete-annotations-region start end))


(provide 'annot)
;;; annot.el ends here
