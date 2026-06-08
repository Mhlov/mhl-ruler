;mhl-ruler.scm
;==============================================================================
;MHL-Ruler
;Create a proportionally divided ruler from the selection.
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
;Tested on GIMP 2.10.32


(define (mhl-add-layer group
                       width
                       height
                       name
                       colour
                       pos-x
                       pos-y)

  ; get image
  (define image (car (gimp-item-get-image group)))

  ; create new layer
  (define layer (car (gimp-layer-new image
                                     width
                                     height
                                     RGB
                                     name
                                     100
                                     LAYER-MODE-NORMAL)))

  ; set layer position
  (gimp-layer-set-offsets layer
                          pos-x
                          pos-y)

  ; add layer to the group
  (gimp-image-insert-layer image
                           layer
                           group
                           0)

  ; change background colour
  (gimp-context-set-background colour)

  ; fill layer with background colour
  (gimp-drawable-edit-fill layer
                           FILL-BACKGROUND)

  ; fill layer with background colour
  ;(gimp-drawable-fill layer FILL-BACKGROUND)
  ; fill layer with background colour
  ;(gimp-edit-fill layer FILL-BACKGROUND)
)


(define (mhl-add-layers group
                        selection
                        colours
                        parts
                        width-as-height
                        centered
                        type
                        scaling
                        lines)

  (define selection-height (- (nth 3 selection)
                              (nth 1 selection)))
  (define selection-width (- (nth 2 selection)
                             (car selection)))

  (define layer-height 0)
  (define layer-width 0)

  (if
    ; if Vertical
    (= 0 type)

    ; then
    (begin
      (set! layer-height
            (floor (/ selection-height
                      parts)))

      (set! layer-width
            (if
              ; if layer-height < selection-width
              (and (= TRUE width-as-height)
                   (< layer-height
                      selection-width))

              ; then
              layer-height

              ;else
              selection-width)))

      ; else
      (if
        ; if Horizontal
        (= 1 type)

        ;then
        (begin
          (set! layer-width
            (floor (/ selection-width
                      parts)))

          (set! layer-height
            (if
              ; if layer-width < selection-height
              (and (= TRUE width-as-height)
                   (< layer-width
                      selection-height))

              ; then
              layer-width

              ;else
              selection-height)))))

  (define pos-x (if
                  (and (= TRUE centered)
                       (= TRUE width-as-height)
                       (< layer-width
                          (- (nth 2 selection)
                             (car selection))))
                  ; then
                  (begin
                    (+ (car selection)                  ; selection x position
                       (round (/ (- (- (nth 2 selection)
                                       (car selection)) ; selection-w
                                    layer-width)        ; selection-w - layer-w
                                 2))))
                  ; else
                  (begin
                    (car selection))))

  (define pos-y (if
                  (and (= TRUE centered)
                       (= TRUE width-as-height)
                       (< layer-height
                          (- (nth 3 selection)
                             (nth 1 selection))))
                  ;then
                  (begin
                    (+ (nth 1 selection)                    ; selection y pos
                       (round (/ (- (- (nth 3 selection)
                                       (nth 1 selection))   ; selection-h
                                    layer-height)           ; sel-h - layer-h
                                 2))))
                  ;else
                  (begin
                    (nth 1 selection))))

  (let*
    ( (i 0) )
    (while (< i parts)
           (set! i (+ i 1))
           (mhl-add-layer group
                          (if (and (= TRUE lines) (= 1 type))         ; width
                            ; if lines and horizontal type then
                            1
                            ; else
                            layer-width)
                          (if (and (= TRUE lines) (= 0 type))         ; height
                            ; if lines and vertical type then
                            1
                            ; else
                            layer-height)
                          (string-append "part " (number->string i))  ; name
                          (if (= TRUE (remainder i 2))                ; colour
                            (car colours)                           ; 1st-colour
                            (cadr colours))                         ; 2nd-colour
                          (if (= 0 type)                            ; pos-x
                            ; if vertical then
                            pos-x
                            ; else if horizontal
                            (if (= 1 type)
                              ; then
                              (+ (car selection)
                                 (* layer-width (- i 1)))))
                          (if (= 1 type)                            ; pos-y
                            ; if horizontal then
                            pos-y
                            ; else if vertical
                            (if (= 0 type)
                              ; then
                              (+ (cadr selection)
                                 (* layer-height (- i 1))))))))

  ; add last line
  (if (= TRUE lines)
    ; then
    (mhl-add-layer group
                   (if (= 1 type)                                        ; width
                     ; if horizontal type then
                     1
                     ; else
                     layer-width)
                   (if (= 0 type)                                        ; height
                     ; if vertical type then
                     1
                     ; else
                     layer-height)
                   (string-append "part " (number->string (+ parts 1)))  ; name
                   (if (= TRUE (remainder (+ parts 1) 2))                ; colour
                     (car colours)                                       ; 1st-colour
                     (cadr colours))                                     ; 2nd-colour
                   (if (= 0 type)                                        ; pos-x
                     ; if vertical then
                     pos-x
                     ; else if horizontal
                     (if (= 1 type)
                       ; then
                       (+ (car selection)
                          (- (* layer-width (- (+ parts 1) 1)) 1))))
                   (if (= 1 type)                                        ; pos-y
                     ; if horizontal then
                     pos-y
                     ; else if vertical
                     (if (= 0 type)
                       ; then
                       (+ (cadr selection)
                          (- (* layer-height (- (+ parts 1) 1)) 1))))))

  ; Scale the group as needed
  (if (and (= TRUE scaling)
           (= 0 type)
           (> selection-height
              (* layer-height
                 parts)))
    ;if vertical and sel-h > group-h then
    (begin
      (gimp-layer-scale group
                        layer-width
                        selection-height
                        TRUE)
      (gimp-layer-set-offsets group
                              pos-x
                              (cadr selection)))

    ;else
    (if (and (= TRUE scaling)
             (= 1 type)
             (> selection-width
                (* layer-width
                   parts)))
      ;if horizontal and sel-w > group-w then
      (begin
        (gimp-layer-scale group
                         selection-width
                         layer-height
                         TRUE)
        (gimp-layer-set-offsets group
                                (car selection)
                                pos-y)))))

(define (mhl-main image
                  first-colour
                  second-colour
                  type
                  parts
                  width-as-height
                  centered
                  scaling
                  lines
                  opacity)

  (if
    (= TRUE (car (gimp-selection-is-empty image)))

    ; then
    (begin
      (gimp-message "That script doesn't work without selection"))

    ; else
    (begin
      ; Start undo group
      (gimp-image-undo-group-start image)

      ; create layer group
      (define group (car (gimp-layer-group-new image)))
      (gimp-layer-set-name group
                           (string-append (number->string parts)
                                          " parts"))
      (gimp-image-insert-layer image
                               group
                               0
                               0)
      (if
        (< opacity 100)
        ; then
        (gimp-layer-set-opacity group
                                opacity))

      (define selection (cdr (gimp-selection-bounds image)))

      (mhl-add-layers group
                      selection
                      (list first-colour second-colour)
                      parts
                      width-as-height
                      centered
                      type
                      scaling
                      lines)

      ; End undo group
      (gimp-image-undo-group-end image)

      ; Flush all internal changes to the user interface
      (gimp-displays-flush))))


(script-fu-register
  "mhl-main"
  _"<Image>/Script-Fu/MHL-Ruler"
  "Create a proportionally divided ruler from the selection"
  "MHL <mhl@localhost>"
  "MHL"
  "2019"
  "*"
  SF-IMAGE "Image" 0
  SF-COLOR "First colour" '(0 87 184)
  SF-COLOR "Second colour" '(255 215 0)
  SF-OPTION "Type" '("Vertical" "Horizontal")
  SF-VALUE "Number of parts" "4"
  SF-TOGGLE "Width as height" TRUE
  SF-TOGGLE "Centered" TRUE
  SF-TOGGLE "Scale as needed" TRUE
  SF-TOGGLE "Divisions as lines" FALSE
  SF-VALUE "Opacity" "50")
