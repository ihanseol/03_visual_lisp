(defun get-dxf-files (dir)
  ;; Get a list of DXF files in the specified directory
  (vl-directory-files dir "*.dxf" 1)
)

(defun insert-dxf (filename)
  ;; Insert the DXF file into the drawing
  (command "_.INSERT" filename pause pause pause)
)

(defun repeat-insert ()
  ;; Specify the Downloads folder path
  (setq folder "C:/Users/minhwasoo/Downloads/")  ;; Change to your username
  ;; Get the list of DXF files
  (setq fileList (get-dxf-files folder))
  
  ;; Loop through the first 9 files (or fewer if there aren't 9)
  (if (> (length fileList) 0)
    (repeat (min 9 (length fileList))
      ;; Insert each DXF file
      (insert-dxf (strcat folder (car fileList)))
      ;; Remove the inserted file from the list
      (setq fileList (cdr fileList))
    )
  )
)
(repeat-insert)
