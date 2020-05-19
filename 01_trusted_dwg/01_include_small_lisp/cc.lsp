
(defun c:cc(/ ss num pt)
	(princ "\nSelect Objecet")
	(setq ss (ssget))
	(setq num (getint "\nHOw many?"))
	(setq pt (getpoint "\nBase Point?"))

	(command "copy" ss "" pt "a" num)
)


