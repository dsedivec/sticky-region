;;; sticky-region.el --- Preserve region between commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Some commands will remove your region when they complete.  That's
;; annoying if you wanted to keep the region.  When global minor mode
;; `sticky-region-mode' is active, you can run `sticky-region-toggle'
;; after losing your region to bring it back and make it persist over
;; multiple following commands.

;;; Code:

(require 'ring)

(defvar sticky-region-history-size 20
  "Number of previous regions stored in each buffer's history.")

(defvar-local sticky-region--region-history nil
  "Ring of (mark . point) markers.")

(defvar-local sticky-region--current-region nil
  "(mark . point) saved in `pre-command-hook'.")

(defvar-local sticky-region--active nil
  "If true, region will be reactivated in `post-command-hook' if necessary.")

(defun sticky-region--pre-command-hook ()
  (when (use-region-p)
    (if (not sticky-region--current-region)
        (setq sticky-region--current-region (cons (copy-marker (mark-marker))
                                                  (point-marker)))
      (set-marker (car sticky-region--current-region) (mark))
      (set-marker (cdr sticky-region--current-region) (point)))))

(defun sticky-region--post-command-hook ()
  (let* ((region-inactive (or deactivate-mark
                              ;; Check if someone already deactivated
                              ;; the region.
                              ;;
                              ;; Force Emacs to consider an empty
                              ;; region active.  This prevents a weird
                              ;; case where you've activated sticky
                              ;; region and you're moving the cursor
                              ;; in such a way that the region becomes
                              ;; empty, which causes sticky regions to
                              ;; fight you by restoring the region
                              ;; (including point) every time you try
                              ;; to move point onto the mark.
                              (let ((use-empty-active-region t))
                                (not (use-region-p)))))
         (deactivate (and
                      ;; We're active.  Should we deactivate?
                      sticky-region--active
                      (or
                       ;; Deactivate if multiple-cursors just got
                       ;; activated.
                       (bound-and-true-p multiple-cursors-mode)
                       ;; C-g deactivates sticky mark.
                       (eq this-command 'keyboard-quit)
                       ;; Deactivate if our stored region has been
                       ;; zeroed out.  This is possible because we
                       ;; save the region as markers, so in the case
                       ;; of something like `delete-selection-mode',
                       ;; we may suddenly find that the region we
                       ;; saved is empty.
                       (and region-inactive
                            ;; Special case:
                            ;; `sticky-region-pop-region' won't
                            ;; deactivate sticky regions.
                            (not (eq this-command 'sticky-region-pop-region))
                            sticky-region--current-region
                            (sticky-region--current-region-empty-p)))))
         (restore (and
                   ;; We've been asked to make the region sticky.
                   (not deactivate)
                   sticky-region--active
                   ;; The region isn't active now.
                   region-inactive
                   ;; We have a region we could restore.
                   sticky-region--current-region))
         (save (and
                ;; We'll never both restore and save a region to
                ;; history at the same time.
                (not restore)
                ;; Region is gone.
                region-inactive
                ;; We have a saved region that we could save to
                ;; history.  At one point I was also considering not
                ;; putting an empty saved region into history (see
                ;; `sticky-region--current-region-empty-p').  However,
                ;; I'm thinking that it may nonetheless be useful to
                ;; save a region in the case of undos?  I honestly
                ;; don't know how that works.
                sticky-region--current-region)))
    (when deactivate
      (setq sticky-region--active nil)
      (message "Sticky region deactivated"))
    (when restore
      (sticky-region--restore-current-region)
      (setq deactivate-mark nil))
    (when save
      (unless sticky-region--region-history
        ;; First time we're using the ring in this buffer, initialize
        ;; it.
        (setq sticky-region--region-history
              (make-ring sticky-region-history-size)))
      (ring-insert sticky-region--region-history sticky-region--current-region)
      (setq sticky-region--current-region nil))))

(defun sticky-region--current-region-empty-p ()
  (zerop (- (car sticky-region--current-region)
            (cdr sticky-region--current-region))))

(defun sticky-region--restore-current-region ()
  (push-mark (car sticky-region--current-region) t t)
  (goto-char (cdr sticky-region--current-region)))

(defun sticky-region-pop-region (&optional noerror)
  "Pop the next region off of region history for this buffer.
Raises an error if there are no more regions to pop, unless
NOERROR is true."
  (interactive)
  (unless sticky-region-mode
    (error "`sticky-region-mode' is not on"))
  (if (or (null sticky-region--region-history)
          (ring-empty-p sticky-region--region-history))
      (unless noerror
        (error "No region to pop"))
    (setq sticky-region--current-region
          (ring-remove sticky-region--region-history 0))
    (sticky-region--restore-current-region)))

(defun sticky-region-activate (&optional pop-region)
  "Activate sticky region mode, and possibly the region as well.
If no region is active then the last region from history is
activated, if any.  If called with a prefix arg, or if called
from lisp with POP-REGION true, the next region will be popped
from history unconditionally, overwriting any current region."
  (interactive "P")
  (unless sticky-region-mode
    (error "`sticky-region-mode' is not on"))
  (when multiple-cursors-mode
    (error "Sticky regions don't work with multiple-cursors"))
  (setq sticky-region--active t
        pop-region (or pop-region (not (use-region-p))))
  (when pop-region
    (sticky-region-pop-region t))
  (if (and pop-region
           (called-interactively-p 'any)
           ;; Make sure this command was invoked with a keyboard
           ;; event.  AFAIK any non-keyboard event will be something
           ;; other than an integer or a symbol... except for maybe
           ;; help-echo or Windows language changes...
           (memq (type-of last-command-event) '(integer symbol)))
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap (vector last-input-event) #'sticky-region-pop-region)
        (set-transient-map keymap t)
        (message "Sticky region activated, %s will continue popping"
                 (single-key-description last-command-event)))
    (message "Sticky region activated")))

(defvar sticky-region-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; C-x C-z is normally bound to `suspend-frame', but that's also
    ;; bound on C-z by default, so I don't feel too bad about
    ;; clobbering it.
    (define-key keymap (kbd "C-x C-z") 'sticky-region-activate)
    keymap))

;;;###autoload
(define-minor-mode
    sticky-region-mode
    "Preserve region through multiple commands.
Useful if you want to run multiple commands on the same region,
such as search/replace or indent/de-indent."
  :global t
  :keymap sticky-region-mode-map

  (let ((add-remove-hook (if sticky-region-mode
                             #'add-hook
                           #'remove-hook)))
    (funcall add-remove-hook 'pre-command-hook
             #'sticky-region--pre-command-hook)
    (funcall add-remove-hook 'post-command-hook
             #'sticky-region--post-command-hook)))

(provide 'sticky-region)
;;; sticky-region.el ends here
