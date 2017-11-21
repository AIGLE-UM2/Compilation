;#########################################################################################################;
;                                     >>  Implémentation de la VM  <<                                     ;
;                                                                                                         ;
;                                          > Réalisé par :                                                ;
;                                              Mostafa HASHEMI                                            ;
;                                              Rémi DE WISPELAERE                                         ;
;                                              François JOURDAIN                                          ;
;                                                                                                         ;
;                                          > Sous la direction de :                                       ;
;                                               Monsieur David Delahaye                                   ;
;                                               Monsieur Mathieu Lafourcade                               ;
;                                                                                                         ;
;#########################################################################################################;


;-----------------------------------------------------------------------------------------------------------;
;     1_1: *make-vm* : création une machine virtuelle, avec un nom, une certaine taille de mémoire, etc.    ;
;-----------------------------------------------------------------------------------------------------------;
;nomVM: nom de la vm sur laquelle faire l'opération
;tailleMem: la taille de la memoire allouer a la vm
(defun make-vm (nomVM tailleMem)
  (progn

      ;création des registres spéciaux
      (setf (get nomVM 'FP) 0) ;Frame Pointer
      (setf (get nomVM 'SP) 0) ;Stack Pointer
      (setf (get nomVM 'PC) 0) ;Program Counter

      ;création des registres de données
      (setf (get nomVM 'R0) 0)
      (setf (get nomVM 'R1) 0)
      (setf (get nomVM 'R2) 0)


      ;création des drapeaux

      ;création de la memoire
      (setf (get nomVM 'memoire) (make-array tailleMem :initial-element ()))   

    nomVM))


;-----------------------------------------------------------------------------------------------------------;
;         1_2:  Fonctions *get-registe/set-register* pour accéder au contenu des registres                  ;  
;-----------------------------------------------------------------------------------------------------------;
;fonction d'acces en lecture aux registres
;reg: nom du registre a lire
(defun get-register (nomVM reg)
  (get nomVM reg))

;fonction d'acces en ecritureaux registres
;new: nouvelle valeur à affecter au registre
(defun set-register (nomVM reg new)
  (setf(get nomVM reg) new))


;-----------------------------------------------------------------------------------------------------------;
;       1_3: Fonctions *get-memory/set-memory* pour accéder au contenu de la mémoire à une adresse donnée.  ;              
;-----------------------------------------------------------------------------------------------------------;

;fonction d'acces en lecture à la memoire
;pos: position de l'information à lire dans la mémoire
(defun get-memory (nomVM pos)
  (aref (get nomVM 'memoire) pos))  
  

;fonction d'acces en ecriture à la memoire
;new: nouvelle valeur à affecter au registre
(defun set-memory (nomVM pos new)
  (setf(aref (get nomVM 'memoire) pos) new))



;-----------------------------------------------------------------------------------------------------------;
;                     2_1: Fonction *move* de registre/valeur immédiate à registre                          ;
;-----------------------------------------------------------------------------------------------------------;

(defun registerp (expression)
  (member expression '(R0 R1 R2 FP SP PC)))

;fonction de copie de R0 dans R1
;R0: donnée à copier, peut être soit un registre soit une valeur
;R1: registre de reception de copie
(defun move(nomVM R0 R1)
  (if(registerp R1)
      (if(registerp R0)
    (set-register nomVM R1 (get-register nomVM R0))
    (set-register nomVM R1 r0))
    (print '"ERREUR: Le recepteur de la copie (MOVE) n'est pas un registre")))


;___________________________________________________________________________________________________________;
;                                        * Bibliographie *                                                  ;
;                                                                                                           ;         
;     ▪ https://www.tutorialspoint.com/lisp/lisp_symbols.htm                                                ;
;     ▪ https://www.tutorialspoint.com/lisp/lisp_arrays.htm                                                 ;
;___________________________________________________________________________________________________________;