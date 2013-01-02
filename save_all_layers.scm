; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

;; Command is installed in "File->Save all layers..."
;;
;; A template string should be provided which fits the form: prefix~~~~.ext
;; where prefix is a character string (optionally null).
;;       ~~~~ represents the digits of the frame number, one ~ per digit
;;       ext is the filename extension (which also specifies the format)
;; the tildas are optional and four digits will be assumed if omitted.
;; an extension of .png is assumed if one is not provided
;; the period is significant, if PNG is not to be assumed
;;
;; A checkbox provides the option of using the layernames for the
;; filenames. The extension given in the template is used to determine
;; the file type. Animation settings (delay and frame disposal) are not
;; included as part of the filename.
;;
;; When saving to GIF files, the GIMP's default values are used to
;; convert to INDEXED mode (255 color palette, no dithering).
;; Note: this is done on a layer-by-layer basis, so more colors may result
;; than if the entire image were converted to INDEXED before saving.

;;; Prepend Folder mod by JL
;;; @see http://chiselapp.com/user/saulgoode/repository/script-fu/artifact/252cedfc2d5c5f39955f6525c24d63fa5528d0af

(define (sg-save-all-layers orig-image drawable
                                    template
                                    rename?)
  (define (save-layer orig-image layer name)
    (let ((buffer (car (gimp-edit-named-copy layer "temp-copy"))))
      (let ((image (car (gimp-edit-named-paste-as-new buffer))))
        (gimp-buffer-delete buffer)
        (set! buffer (strbreakup name "."))
        (unless (> (length buffer) 1)
          (set! name (string-append name ".png")) )
        (when (and (not (= (car (gimp-image-base-type image)) INDEXED))
                   (string-ci=? (car (last (strbreakup name "."))) "gif"))
          (gimp-image-convert-indexed image
                                      NO-DITHER
                                      MAKE-PALETTE
                                      255
                                      FALSE
                                      FALSE
                                      "" ))
        (gimp-file-save RUN-NONINTERACTIVE 
                        image 
                        (car (gimp-image-get-active-layer image)) 
                        name 
                        name )
        (gimp-image-delete image) )))
  (let* ((layers nil)
         (fullname "")
         (basename "")
         (layername "")
         (format "")
         (layerpos 1)
         (framenum "")
         (settings "")
         (default-extension "png")
         (extension "png")
         (orig-selection 0) )
    (gimp-image-undo-disable orig-image)
    (set! orig-selection (car (gimp-selection-save orig-image)))
    (gimp-selection-none orig-image)

    (set! extension (strbreakup template "."))
    (set! extension (if (> (length extension) 1)
                      (car (last extension))
                      default-extension))
    (when (= (string-length extension) 0)
      (set! default-extension "png"))
    (when (= rename? TRUE)
      (set! format (strbreakup template "~"))
      (if (> (length format) 1)
        (begin
          (set! basename (car format))
          (set! format (cdr format))
          (set! format (cons "" (butlast format)))
          (set! format (string-append "0" (unbreakupstr format "0"))) )
        (begin
          (set! basename (car (strbreakup template ".")))
          (set! format "0000") )))
    (set! layers (reverse (vector->list (cadr (gimp-image-get-layers orig-image)))))
    (while (pair? layers)
      (if (= rename? TRUE)
        (begin
          (set! framenum (number->string layerpos))
          (set! framenum (string-append
                (substring format 0 (- (string-length format)
                                       (string-length framenum))) framenum))
          (set! fullname (string-append basename framenum "." extension)) )
        (begin
          (set! fullname (string-append template (car (strbreakup
                           (car (gimp-drawable-get-name (car layers))) "("))))
          ;;(gimp-drawable-set-name (car layers) fullname)
          ;;(set! fullname (car (gimp-drawable-get-name (car layers)))) 
  	  )
      )
      (save-layer orig-image (car layers) fullname)
      (set! layers (cdr layers))
      (set! layerpos (+ layerpos 1))
      )
    (gimp-selection-load orig-selection)
    (gimp-image-remove-channel orig-image orig-selection)
    (gimp-image-undo-enable orig-image)
    )
  )

(script-fu-register "sg-save-all-layers"
 "Save all layers..."
 "Save each layer to a file."
 "Saul Goode"
 "Saul Goode"
 "11/16/2008"
 "*"
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
 SF-STRING "Name Template (~ replaced by layer position) or Prepend Folder" "c:\\tmp\\layer\\"
 SF-TOGGLE "Rename (e.g frame~~~~.png) or Prepend Folder" FALSE
 )

(script-fu-menu-register "sg-save-all-layers"
                         "<Image>/File/")
