; (load "/Users/Hp/Downloads/ProyectoLisp/parseo.lisp")

(load "~/quicklisp/setup.lisp")

(ql:quickload "str")

; Lectura de diccionarios -------------------------------------------------------------------------------------------------

(defun bubblesort (lst)
  (dotimes (i (length lst))
    (dotimes (j (- (length lst) 1))
      (when (string< (nth (1+ j) lst) (nth j lst))
        (rotatef (nth j lst) (nth (1+ j) lst)))))
  lst)

(defun DiccLec (archivo)
  (with-open-file (stream (str:concat "/Users/Hp/Downloads/ProyectoLisp/Diccionario/" archivo) :direction :input)
    (let ((lect '()))
      (loop for linea = (read-line stream nil)
            while linea do
            (push (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) linea)) lect))
      (bubblesort lect))))

(defun busqueda(token diccionario)
    (cond ((equal diccionario nil) nil)
          ((string= (string-downcase token) (car diccionario)) T )
          (T (busqueda token (cdr diccionario)))
    )
)

;(sort Determinantes #'sb-unicode:unicode<)


; Funciones de utlierias ----------------------------------------------------------------------------------------------------


(defun tokens (oracion)
    (str:split " " oracion))


(defun QuitarNull (oracion)
  (let ((result '()))
    (loop for item in oracion do
          (if (and item (not (string= item "")))  ; Verifica si el item no es nil ni una cadena vacía
              (push item result)))
    (nreverse result)))


(defun split-last-char (oracion)
  (let ((last (car (last oracion))))
    (if (> (length last) 1)
        (append (butlast oracion)
                (list (subseq last 0 (1- (length last))) 
                      (subseq last (1- (length last)))))
        oracion)))


(defun puntoFin(oracion)
    (if (not (equal (car (last oracion) ) "."))
        (progn (print "A tu oracion le hace falta un punto al final") nil) oracion))

;(defun PuntoAlFinal (oracion)
 ;   (if (not (equal (nth (- (length oracion) 1) oracion) "."))
  ;      (progn (print "A tu oracion le hace falta un punto al final") nil) oracion))

;(defun existe (elemento lista)
  ;(member (string-downcase elemento) lista :test #'string=))

(defun proces1(oracion)
    (let ((Answ (puntoFin (split-last-char (QuitarNull (tokens oracion)))))) Answ  ))


; ------------------------------------------------------------------------------------------

(defun concordanciaNeu (oracion)
  (cond
    ((and (or (busqueda (car oracion) (DiccLec "Deter/Fem/DeterSinFem.txt")) 
              (busqueda (car oracion) (DiccLec "Deter/Masc/DeterSinMasc.txt")))
          (busqueda (cadr oracion) (DiccLec "Sust/Neutros/NeutrosSin.txt"))) T)

    ((and (or (busqueda (car oracion) (DiccLec "Deter/Fem/DeterPluFem.txt")) 
              (busqueda (car oracion) (DiccLec "Deter/Masc/DeterPluMasc.txt")))
          (busqueda (cadr oracion) (DiccLec "Sust/Neutros/NeutrosPlu.txt"))) T)

    (t nil)))


(defun concordanciaAdj (oracion)
  (cond
    ((and (busqueda (car oracion) (DiccLec "Deter/Fem/DeterSinFem.txt")) 
          (busqueda (nth 2 oracion) (DiccLec "Adj/Fem/AdjFemSin.txt"))) T)

    ((and (busqueda (car oracion) (DiccLec "Deter/Fem/DeterPluFem.txt")) 
          (busqueda (nth 2 oracion) (DiccLec "Adj/Fem/AdjFemPlu.txt"))) T)

    ((and (busqueda (car oracion) (DiccLec "Deter/Masc/DeterSinMasc.txt")) 
          (busqueda (nth 2 oracion) (DiccLec "Adj/Masc/AdjMascSin.txt"))) T)

    ((and (busqueda (car oracion) (DiccLec "Deter/Masc/DeterPluMasc.txt")) 
          (busqueda (nth 2 oracion) (DiccLec "Adj/Masc/AdjMascPlu.txt"))) T)

    (t nil)))


(defun concordancia (oracion)
  (cond
    ((concordanciaNeu oracion) T)

    ((and (busqueda (car oracion) (DiccLec "Deter/Fem/DeterSinFem.txt")) 
          (busqueda (cadr oracion) (DiccLec "Sust/Fem/SustSinFem.txt"))) T)

    ((and (busqueda (car oracion) (DiccLec "Deter/Fem/DeterPluFem.txt")) 
          (busqueda (cadr oracion) (DiccLec "Sust/Fem/SustPluFem.txt"))) T)

    ((and (busqueda (car oracion) (DiccLec "Deter/Masc/DeterSinMasc.txt")) 
          (busqueda (cadr oracion) (DiccLec "Sust/Masc/SustSinMasc.txt"))) T)

    ((and (busqueda (car oracion) (DiccLec "Deter/Masc/DeterPluMasc.txt")) 
          (busqueda (cadr oracion) (DiccLec "Sust/Masc/SustPluMasc.txt"))) T)

    (t nil)))

; ------------------------------------------------------------------------------------------

(defun prep(token)
    (if (busqueda token (DiccLec "Prep/Prep.txt")) T NIL))

(defun det(token)
    (if (busqueda token (DiccLec "Deter/Deter.txt")) T NIL))

(defun sust(token)
    (if (busqueda token (DiccLec "Sust/Sust.txt")) T NIL))

(defun adj(token)
    (if (busqueda token (DiccLec "Adj/Adj.txt")) T NIL))

(defun verbo(token)
    (if (busqueda token (DiccLec "Verbos/Verbos.txt")) T NIL))

(defun adv(token)
    (if (busqueda token (DiccLec "Adv/Adv.txt")) T NIL))

(defun terminar(token)
    (if (string= token ".") T NIL))

; ------------------------------------------------------------------------------------------

(declaim (ftype function sv2 sv1 sn3 sn2 sn1))

(defun sv2(oracion)
    (cond ((adv (car oracion)) (sv2 (cdr oracion)))
          ((or (prep (car oracion)) (det (car oracion))) (sn1 oracion))
          ((terminar (car oracion)) "SV2: Chido")
          (T (str:concat "SV2: No puedo validar '"(car oracion) "' verifica que este bien escrito, dentro del diccionario, asi como leer la estrctura del lenguaje"))
    )
)

(defun sv1(oracion)
    (cond ((verbo (car oracion)) (sv1 (cdr oracion)))
          ((adv (car oracion)) (sv2 (cdr oracion)))
          ((or (prep (car oracion)) (det (car oracion))) (sn1 oracion))
          ((terminar (car oracion)) "SV1: Chido")
          (T (str:concat "SV1: No puedo validar '"(car oracion) "' verifica que este bien escrito, dentro del diccionario, asi como leer la estrctura del lenguaje"))
    )
)

(defun sn3(oracion)
    (cond ((sust (car oracion)) (sn3 (cdr oracion)))
          ((adj (car oracion)) (sv1 (cdr oracion)))
          ((verbo (car oracion)) (sv1 oracion))
          ((terminar (car oracion)) "SN3: Chido")
          (T (str:concat "SN3: No puedo validar '"(car oracion) "' verifica que este bien escrito, dentro del diccionario, asi como leer la estrctura del lenguaje"))
    )
)

(defun sn2(oracion)
    (cond ((det (car oracion)) (if (adj (nth 2 oracion))
                                    (if (concordanciaAdj oracion) 
                                    (sn2 (cdr oracion)) 
                                    (str:concat "'" (car oracion) " " (cadr oracion) " " (nth 2 oracion)"' no coincide, verifica que esten bien escritas y que sea coherente la concordancia del adjetivo con el articulo")) 
                                    (sn2 (cdr oracion))))
          ((sust (car oracion)) (sn3 oracion))
          ((verbo (car oracion)) (sv1 oracion))
          ((terminar (car oracion)) "SN2: Chido")
          
          (T (str:concat "SN2: No puedo validar '"(car oracion) "' verifica que este bien escrito, dentro del diccionario, asi como leer la estrctura del lenguaje"))
    )
)

(defun sn1(oracion)
    (cond ((prep (car oracion)) (if (det (cadr oracion)) (sn1 (cdr oracion)) "No encuentro tu articulo"))
          ((det (car oracion)) (if (concordancia oracion) 
                                    (sn2 oracion) 
                                    (str:concat "'" (car oracion) "' y '" (cadr oracion) "' no coinciden, verifica que esten bien escritas y y la concordancia del articulo con el sustantivo")))
          ((verbo (car oracion)) (sv1 (cdr oracion)))
          ((terminar (car oracion)) "SN1: Chido")
          ((equal (car oracion) NIL) "ERROR")
          (T (str:concat "SN1: No puedo validar '"(car oracion) "' verifica que este bien escrito, dentro del diccionario, asi como leer la estrctura del lenguaje"))
    )
)

; ------------------------------------------------------------------------------------------

(defun main(oracion)
    (let ((Respuesta (sn1 (proces1 oracion)))) Respuesta))


(defun inp ()
  (format t "-------------------------------------~%")
  (format t "-           Bienvenido              -~%")
  (format t "-------------------------------------~%~%")
  (loop (format t "~%Porfavor Ingresa una oración (o 'x' para salir): ")
        (finish-output)
        (let ((ans (read-line)))
            (if (string= ans "x")
                (return) (format t "~%~a~%" (main ans))))))

(inp)



; (load "/Users/Hp/Downloads/ProyectoLisp/parseo.lisp")

; (main '("el" "motor" "robusto" "es" "peor" "que" "el" "motor" "liviano" "."))