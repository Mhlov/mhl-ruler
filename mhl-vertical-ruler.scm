;mhl-vertical-ruler.scm
;==============================================================================
;MHL-Vertical Ruler
;Create a vertical proportionally divided ruler from the selection.
;Copyright (C) 2019 Melon (https://github.com/Mhlov)
;
; LICENSE
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;==============================================================================
;Tested on GIMP 2.10.8


(define (mhl-add-layer group
					   width
					   height
					   name
					   color
					   pos-x
					   pos-y)
  (define image (car (gimp-item-get-image group))); get image
  (define layer (car (gimp-layer-new image
									 width
									 height
									 RGB
									 name
									 100
									 LAYER-MODE-NORMAL))); create new layer
  (gimp-layer-set-offsets layer
						  pos-x
						  pos-y); set layer position
  (gimp-image-insert-layer image
						   layer
						   group
						   0); add layer to the group
  (gimp-context-set-background color); change background color
  (gimp-drawable-edit-fill layer
						   FILL-BACKGROUND); fill layer with background color
;  (gimp-drawable-fill layer FILL-BACKGROUND); fill layer with background color
;  (gimp-edit-fill layer FILL-BACKGROUND); fill layer with background color
)


(define (mhl-add-layers group
					    selection
					    colors
						parts
						width-as-height
						centered)
  (define layer-height (floor (/ (- (nth 3 selection)
									(nth 1 selection))
								 parts)))
  (define layer-width (if (and (= TRUE width-as-height)
							   (< layer-height (- (nth 2 selection)
												  (car selection)))); layer-height < selection-width
						  (begin layer-height)
						  (begin (- (nth 2 selection)
									(car selection)))))
  (define pos-x (if (and (= TRUE centered)
						 (= TRUE width-as-height)
						 (< layer-width
							(- (nth 2 selection)
							   (car selection))))
					(begin
					  (+ (car selection); selection x position
						 (round (/ (- (- (nth 2 selection)
										 (car selection)); selection-width
									  layer-width); selection-width - layer-width
								   2))))
					(begin
					  (car selection))))

  (define i 0)
  (while (< i parts)
		 (set! i (+ i 1))
		 (mhl-add-layer group
						layer-width									; width
						layer-height								; height
						(string-append "part " (number->string i))	; name
						(if (= TRUE (remainder i 2))				; color
							(car colors)		; first-color
							(cadr colors))		; second-color
						pos-x										; pos-x
						(+ (cadr selection)							; pos-y
						   (* layer-height (- i 1)))
		 ))
)


(define (mhl-main image
				  first-color
				  second-color
				  parts
				  width-as-height
				  centered
				  opacity)
  (if (= TRUE (car (gimp-selection-is-empty image)))
	  (begin ; hasn't selection
		(gimp-message "That script doesn't work without selection :("))
	  (begin
		(gimp-image-undo-group-start image); Starts a group undo

		; create layer group
		(define group (car (gimp-layer-group-new image)))
	    (gimp-layer-set-name group
							 (string-append (number->string parts)
											" parts"))
		(gimp-image-insert-layer image
								 group
								 0
								 0)
		(if (< opacity 100)
			(gimp-layer-set-opacity group
									opacity))

		(define selection (cdr (gimp-selection-bounds image)))

		(mhl-add-layers group
					    selection
					    (list first-color second-color)
						parts
						width-as-height
						centered)

		(gimp-image-undo-group-end image); Finish a group undo
		(gimp-displays-flush); Flush all internal changes to the user interface
		))
)


(script-fu-register "mhl-main"
					_"<Image>/Script-Fu/MHL-Vertical Ruler"
                    "Create a vertical proportionally divided ruler from the selection"
                    "MHL <mhl@localhost>"
                    "MHL"
                    "2019"
                    "*"
                    SF-IMAGE "Image" 0
					SF-COLOR "First color" '(255 109 109)
					SF-COLOR "Second color" '(219 0 219)
					SF-VALUE "Number of parts" "4"
					SF-TOGGLE "Width as height" TRUE
					SF-TOGGLE "Centered" TRUE
					SF-VALUE "Opacity" "50"
)

