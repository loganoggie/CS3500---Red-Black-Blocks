; Logan Nielsen CS3500 Homework 6
; black/red blocks LISP program

; this is the 'main' function to be called
(defun placeRedBlocks (rowSize)
    ; minimum set of consecutive blocks that are red
    (setf minRedBlockUnitSize 3)

    ; one solution is to have no red blocks
    (setf allBlack (blackRow rowSize))

    ; instantiate the solution list that will contain the solutions
    (setf solution_list (cons allBlack nil))

    ; build solution_list
    (defvar tmp 3)
    (loop for tmp from minRedBlockUnitSize to rowSize by 1
        do (loop for start from 0 to rowSize by 1
            do (progn (placeRedBlockUnit minRedBlockUnitSize tmp rowSize start allBlack))))

    ; had to add this to print the results to the screen / return the solution_list
    (return-from placeRedBlocks solution_list)
)

; recursively places the red blocks into the array
(defun placeRedBlockUnit (minRedBlockUnitSize redBlockUnitSize rowSize start prevString)
    ; create a copy of the prevString into newConfig
    (setf newConfig (copy-list prevString))
    (if (>= rowSize (+ redBlockUnitSize start))
        (progn (loop for k from 0 to (- redBlockUnitSize 1) by 1
                  do (progn (setf (nth (+ start k) newConfig) 'R)))

            ; add newConfig to solution_list
            (setf solution_list (cons newConfig solution_list))

            ; additionally, try R block units of other sizes
            (loop for otherUnitSize from minRedBlockUnitSize to rowSize by 1
              do (placeRedBlockUnit minRedBlockUnitSize otherUnitSize rowSize (+ start redBlockUnitSize 1) newConfig))
        )
    )
)

; creates the list of size rowSize and fills each element with character 'B'
(defun blackRow(rowSize)
    (let* ((row ()))
        (dotimes (i rowSize)
            (setf row (cons 'B row))
        ) row
    )
)
