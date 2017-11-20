;fonction de création de la vm
;nomVm: nom de la vm sur laquelle faire l'opération
;tailleMem: la taille de la memoire allouer a la vm
(defun make-vm (nomVm tailleMem)
  (progn

      ;création des registres spéciaux
      (setf (get nomVm 'FP) 0) ;Frame Pointer
      (setf (get nomVm 'SP) 0) ;Stack Pointer
      (setf (get nomVm 'PC) 0) ;Program Counter

      ;création des registres de données
      (setf (get nomVm 'R0) 0)
      (setf (get nomVm 'R1) 0)
      (setf (get nomVm 'R2) 0)


      ;création des drapeaux

      ;création de la memoire
      (setf (get nomVm 'memoire) (make-array tailleMem :initial-element ())) ;https://www.tutorialspoint.com/lisp/lisp_arrays.htm



    nomVm))






