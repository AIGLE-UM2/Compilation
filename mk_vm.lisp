;▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓;
;                                     ►►   Implémentation de la VM  ◄◄                                    ;
;                                                                                                         ;
;                                          ☻ Réalisé par :                                                ;
;                                              Mostafa HASHEMI                                            ;
;                                              Rémi DE WISPELAERE                                         ;
;                                              François JOURDAIN                                          ;
;                                                                                                         ;
;                                          ● Sous la direction de :                                       ;
;                                               Monsieur David Delahaye                                   ;
;                                               Monsieur Mathieu Lafourcade                               ;
;                                                                                                         ;
;▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓▒░▓;


;-----------------------------------------------------------------------------------------------------------;
;       1_1: make-vm : création une machine virtuelle, avec un nom, une certaine taille de mémoire, etc.    ;
;-----------------------------------------------------------------------------------------------------------;
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
      (setf (get nomVm 'memoire) (make-array tailleMem :initial-element ()))   ;https://www.tutorialspoint.com/lisp/lisp_arrays.htm

    nomVm))


;-----------------------------------------------------------------------------------------------------------;
;           1_2:  Fonctions get-registe/set-register pour accéder au contenu des registres                  ;  
;-----------------------------------------------------------------------------------------------------------;
;fonction d'acces en lecture aux registres
;reg: nom du registre a lire
(defun get-register (nomVm reg)
  (get nomVm reg))

;fonction d'acces en ecritureaux registres
;new: nouvelle valeur à affecter au registre
(defun set-register (nomVm reg new)
  (setf(get nomVm reg) new))


;-----------------------------------------------------------------------------------------------------------;
;         1_3: Fonctions get-memory/set-memory pour accéder au contenu de la mémoire à une adresse donnée.  ;              ;
;-----------------------------------------------------------------------------------------------------------;

;fonction d'acces en lecture à la memoire
;pos: position de l'information à lire dans la mémoire
(defun get-memory (nomVm pos)
  (aref (get nomVm 'memoire) pos))  
  

;fonction d'acces en ecriture à la memoire
;new: nouvelle valeur à affecter au registre
(defun set-memory (nomVm pos new)
  (setf(aref (get nomVm 'memoire) pos) new))







;▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒;
;                                        ♣ Bibliographie ♣                                                  ;
;                                                                                                           ;         
;     ▪ https://www.tutorialspoint.com/lisp/lisp_symbols.htm                                                ;
;     ▪                                                                                                     ;
;▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒░▒;

