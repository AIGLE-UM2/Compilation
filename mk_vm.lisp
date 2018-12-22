;#########################################################################################################;
;                                     >>  Implémentation de la VM  <<                                     ;
;                                                                                                         ;
;                          	               > Réalisé par :  											                        ;
;                                             	Mossi HASHEMI  								           	              ;
;                                                                                                         ;
;                                          > Sous la direction de :                                       ;
;                                               Monsieur Mathieu LAFOURCADE                               ;
;																										                                                      ;									
;#########################################################################################################;

;----------------------------------------------------------------------------------------------------------------;
; > Nom du fichier à charger : « final.lisp » 																	 ;
; >	(make-vm nomVM tailleMemoire)																				 ;
; >	Créer une nouvelle machine virtuelle nommée « nomVM » avec une taille mémoire de « tailleMemoire » cellules  ;
; >	ATTENTION : taille de pile fixe (100 cellules)  															 ;
; > Pile placée à la fin de la mémoire et de type « décroissante »												 ;
;----------------------------------------------------------------------------------------------------------------;

(defun make-vm (nomVM memSize) 

		(setf 
			(get nomVM 'R0) 0
			(get nomVM 'R1) 0
			(get nomVM 'R2) 0
			(get nomVM 'R3) 0

			(get nomVM 'BP) memSize   		 ;Commencement de la pile (fixe)
			(get nomVM 'SP) memSize	  		 ;Sommet de la pile (courant)
			(get nomVM 'MP) (- memSize 101)  ;Limite de la pile en mémoire
			(get nomVM 'FP) memSize 		 ;frame pointer
			(get nomVM 'EOF) 0				 ;Dernière instruction du code en mémoire

			(get nomVM 'PC) 0

			(get nomVM 'FL) 0
			(get nomVM 'FE) 0
			(get nomVM 'FG) 0

			(get nomVM 'mem) (make-array memSize :initial-element 0)
			(get nomVM 'FUNCTIONS) (make-hash-table)


		)
)

;(make-vm 'vm 500)
;-----------------------------------------------------------------------------------------------------------;
;               Fonctions *get-registe/set-register* pour accéder au contenu des registres                  ;  
;-----------------------------------------------------------------------------------------------------------;

(defun get-reg (nomVM reg)
	(get nomVM reg)
)


(defun set-reg (nomVM reg value)
	(setf (get nomVM reg) value)

)

;-----------------------------------------------------------------------------------------------------------;
;            Fonctions *get-memory/set-memory* pour accéder au contenu de la mémoire à une adresse donnée.  ;              
;-----------------------------------------------------------------------------------------------------------;

(defun get-mem (nomVM pos)
	(aref (get nomVM 'mem) pos)
)

(defun set-mem 	(nomVM pos value)
	(setf (aref (get nomVM 'mem) pos) value )
)


;-----------------------------------------------------------------------------------------------------------;
;                          Fonction *move* de registre/valeur immédiate à registre                          ;
;-----------------------------------------------------------------------------------------------------------;
 
(defun isReg (reg)
	(or
		(equal reg 'R0) 
		(equal reg 'R1) 
		(equal reg 'R2)
		(equal reg 'R3) 
		(equal reg 'BP)
		(equal reg 'SP)
		(equal reg 'MP)
		(equal reg 'PC)
		(equal reg 'FP)
		(equal reg 'FL)
		(equal reg 'FE)
		(equal reg 'FG)
		(equal reg 'EOF)
	)
)


; (move <src><dest>) 
(defun move (nomVM reg1 reg2)
	(if (isReg reg2)
		(let ((tmp))
			(if (isReg reg1) 
				(setq tmp (get nomVM reg1) )
				(setq tmp reg1)
			)
			(setf (get nomVM reg2) tmp)
		)
		(error "ERREUR: ~S n'est pas un registre" reg2)
	)
)

;-----------------------------------------------------------------------------------------------------------;
;                          load / store , de mémoire à registre ou de registre/valeur à mémoire             ;
;-----------------------------------------------------------------------------------------------------------;
;(LOAD <src> <dest>)
(defun lload (nomVM mem reg)
	(if (isReg reg)
		(if (isReg mem)
			(set-reg nomVM reg (get-mem nomVM (get-reg nomVM mem)))
			(if (integerp mem)
				(set-reg nomVM reg (get-mem nomVM mem))
			)
		)
				(error "ERREUR: ~S n'est pas un registre" reg)

    )
)

(defun shift (nomVM reg val)
	(set-reg nomVM reg (+ (get-reg nomVM reg) val))
)



(defun lstore (nomVM reg mem)
	(if (isReg reg)
		(set-mem nomVM mem (get-reg nomVM reg))
		(set-mem nomVM mem reg)
	)

)

;-----------------------------------------------------------------------------------------------------------;
;                          push / pop pour la pile (se ramène aux deux séries précédentes)                  ;            
;-----------------------------------------------------------------------------------------------------------;

(defun lpush (nomVM reg)
	(if (=(get-reg nomVM 'SP) (get-reg nomVM 'MP))
		(error "ERREUR: Stack OverFlow ! ")
		(progn
			(dec nomVM 'SP)
			(lstore nomVM reg (get-reg nomVM 'SP))
			(show-memory nomVM)
		)
	)

)
(defun lpop (nomVM reg)
	(if (= (get-reg nomVM 'SP) (get-reg nomVM 'BP))
		(error "ERREUR: Stack UnderFlow ! ")
		(progn
			(lload nomVM (get-reg nomVM 'SP) reg)
			(set-mem nomVM (get-reg nomVM 'SP) 0 )
			(inc nomVM 'SP)

			(show-memory nomVM)
		)
	)
)

;-----------------------------------------------------------------------------------------------------------;
;                          opérations arithmétiques élémentaires, de registres/valeurs à registre           ;            
;-----------------------------------------------------------------------------------------------------------;


(defun add(nomVM reg1 reg2)
	(if (isReg reg2)
		(if (isReg reg1)
			(set-reg nomVM reg2 (+ (get-reg nomVM reg1) (get-reg nomVM reg2)) )
			(set-reg nomVM reg2 (+ (get-reg nomVM reg2) reg1 ))
		)
		(error "ERREUR: ~S doit etre un registre!" reg2)
	)
)


(defun sub(nomVM reg1 reg2)
	(if (isReg reg2)
		(if (isReg reg1)
			(set-reg nomVM reg2 (- (get-reg nomVM reg1) (get-reg nomVM reg2)) )
			(set-reg nomVM reg2 (- (get-reg nomVM reg2) reg1))
		)
		(error "ERREUR: ~S doit etre un registre!" reg2)
	)
)

(defun mul(nomVM reg1 reg2)
	(if (isReg reg2)
		(if (isReg reg1)
			(set-reg nomVM reg2 (* (get-reg nomVM reg1) (get-reg nomVM reg2)) )
			(set-reg nomVM reg2 (* (get-reg nomVM reg2) reg1))
		)
		(error "ERREUR: ~S doit etre un registre!" reg2)
	)
)

(defun div(nomVM reg1 reg2)
	(if (isReg reg2)
		(if (isReg reg1)
			(set-reg nomVM reg2 (/ (get-reg nomVM reg1) (get-reg nomVM reg2)) )
			(set-reg nomVM reg2 (/ (get-reg nomVM reg2) reg1))
		)
		(error "ERREUR: ~S doit etre un registre!" reg2)
	)
)

(defun inc (nomVM reg)
	(if (isReg reg)
		(set-reg nomVM reg (+ (get-reg nomVM reg) 1))
		 (progn
		 	(set-reg nomVM 'r2 (+ reg 1))
		 )
	)
)
(defun dec (nomVM reg)
	(if (isReg reg)
		(set-reg nomVM reg (- (get-reg nomVM reg) 1))
		 (progn
		 	(set-reg nomVM 'r2 (- reg 1))
		 )
	)
)


;-----------------------------------------------------------------------------------------------------------;
;                                sauts inconditionnels, avec et sans retour, et retour                      ;            
;-----------------------------------------------------------------------------------------------------------;

(defun JMP (nomVM adresse)
	
	(move nomVM adresse 'PC)

)

(defun RTN (nomVM)
	(lpop nomVM 'PC)
	(lpop nomVM 'FP)
	(lpop nomVM 'R3)
	(MOVE nomVM 'R3 'SP)
)

(defun JSR (nomVM address)
	;save old FP
	(move nomVM 'FP 'R2) 	
	;Set FP to the current parameters
	(move nomVM 'SP 'FP)
	; Get the current SP to recalculate the old SP
	(move nomVM 'SP 'R3)	
	(add nomVM (get-mem nomVM (get-reg nomVM 'FP) ) 'R3)	
	(inc nomVM 'R3)

	; push old SP 
	(lpush nomVM 'R3)	
	; push old FP
	(lpush nomVM 'R2)
	;Push old PC	
	(lpush nomVM 'PC)	
	(if (numberp address)

		(jmp nomVM address)
		
		(progn
			(if (nth-value 1 (gethash address (get nomVM 'FUNCTIONS)))
				(jmp nomVM (gethash address (get nomVM 'FUNCTIONS)))
				(let ((tmp) (i (get-mem nomVM 'FP)))
					(loop while (> i 0) 
						do
						(setq tmp (cons (get-mem nomVM (+ (get nomVM 'FP) i) ) tmp) )
						(setq i (- i 1) )
					)	
					(print tmp)
					(move nomVM (apply address tmp) 'R0)
					(rtn nomVM)
				)
			)
		)
		

	)	

)


;-----------------------------------------------------------------------------------------------------------;
;                          comparaison (cmp), qui positionne des flags                                      ;            
;-----------------------------------------------------------------------------------------------------------;

(defun cmp (nomVM reg1 reg2)
	(if (isReg reg2)
		(progn
			(if(isReg reg1)
				(progn
					(if (= (get nomVM reg1) (get nomVM reg2))
						(progn 
							(set-reg nomVM 'fe 1)
							(set-reg nomVM 'fg 0)
							(set-reg nomVM 'fl 0)
							

						)
					)
					(if (> (get nomVM reg1) (get nomVM reg2))
						(progn 
							(set-reg nomVM 'fe 0)
							(set-reg nomVM 'fg 1)
							(set-reg nomVM 'fl 0)
							
						)
					)
					(if (< (get nomVM reg1) (get nomVM reg2))
						(progn 
							(set-reg nomVM 'fe 0)
							(set-reg nomVM 'fg 0)
							(set-reg nomVM 'fl 1)
							

						)
					)
				)
			)
			(if (numberp reg1)
				(progn
					(if (= reg1 (get nomVM reg2))
						(progn 
							(set-reg nomVM 'fe 1)
							(set-reg nomVM 'fg 0)
							(set-reg nomVM 'fl 0)
							

						)
					)
					(if (> reg1 (get nomVM reg2))
						(progn 
							(set-reg nomVM 'fe 0)
							(set-reg nomVM 'fg 1)
							(set-reg nomVM 'fl 0)
							
						)
					)
					(if (< reg1 (get nomVM reg2))
						(progn 
							(set-reg nomVM 'fe 0)
							(set-reg nomVM 'fg 0)
							(set-reg nomVM 'fl 1)

						)
					)
				)
			)
		)
	)
	(if (numberp reg2)
		(progn
			(if(isReg reg1)
				(progn
					(if (= (get nomVM reg1) reg2)
						(progn 
							(set-reg nomVM 'fe 1)
							(set-reg nomVM 'fg 0)
							(set-reg nomVM 'fl 0)
							(show-regs nomVM)

						)
					)
					(if (> (get nomVM reg1) reg2)
						(progn 
							(set-reg nomVM 'fe 0)
							(set-reg nomVM 'fg 1)
							(set-reg nomVM 'fl 0)
							(show-regs nomVM)
						)
					)
					(if (< (get nomVM reg1) reg2)
						(progn 
							(set-reg nomVM 'fe 0)
							(set-reg nomVM 'fg 0)
							(set-reg nomVM 'fl 1)
							(show-regs nomVM)

						)
					)
				)
			)
			(if (numberp reg1)
				(progn
					(if (= reg1 reg2)
						(progn 
							(set-reg nomVM 'fe 1)
							(set-reg nomVM 'fg 0)
							(set-reg nomVM 'fl 0)
							(show-regs nomVM)

						)
					)
					(if (> reg1 reg2)
						(progn 
							(set-reg nomVM 'fe 0)
							(set-reg nomVM 'fg 1)
							(set-reg nomVM 'fl 0)
							(show-regs nomVM)
						)
					)
					(if (< reg1 reg2)
						(progn 
							(set-reg nomVM 'fe 0)
							(set-reg nomVM 'fg 0)
							(set-reg nomVM 'fl 1)
							(show-regs nomVM)

						)
					)
				)
			)
		)
	)
)


;-----------------------------------------------------------------------------------------------------------;
;                          saut conditionnels, suivant la position des flags.                               ;            
;-----------------------------------------------------------------------------------------------------------;

(defun JLT (nomVM label)
	(if (= (get nomVM 'FL) 1)
		(JMP nomVM label)
	)
)

(defun JLE (nomVM label)
	(if ( and(= (get nomVM 'FL) 1) (= (get nomVM 'FE) 1))
		(JMP nomVM label)
	)
)

(defun JGT (nomVM label)
	(if (= (get nomVM 'FG) 1)
		(JMP nomVM label)
	)
)

(defun JGE (nomVM label)
	(if ( and (= (get nomVM 'FG) 1) (= (get nomVM 'FE) 1))
		(JMP nomVM label)
	)
)

(defun JEQ (nomVM label)
	(if  (= (get nomVM 'FE) 1)
		(JMP nomVM label)
	)
)

(defun JNE (nomVM label)
	(if ( and(= (get nomVM 'FL) 1)  (= (get nomVM 'FG) 1))
		(JMP nomVM label)
	)
)
(defun llabel (nomVM label))



;============================================================================================================================;
;                          >>>>>>>>>>>>>>>>>       Chargement de la VM      <<<<<<<<<<<<<<<<<<<<<<     						 ;
;																															 ;
; >	(loader nomVM nomCode)																									 ;
; >	Charge dans la mémoire de la machine virtuelle « nomVM » le code assembleur « nomCode »									 ;
; >	Utilise deux hashmap pour résoudre les étiquettes                    												     ;            				
;============================================================================================================================;
(defun loader (nomVM code)
		(setq LABELS (make-hash-table))
		(setq TEMP (make-hash-table))

	(let ( (fnctNamed NIL) )

		(dolist (ins code)

			(if (null fnctNamed )
				(progn
					(if (string-equal (car ins) 'label )
						(progn
							(if (nth-value 1 (gethash (car (cdr ins)) (get nomVM 'FUNCTIONS)))
								(error "la function existe déja : ~S" (car (cdr ins)))
								(setf (gethash (car (cdr ins)) (get nomVM 'FUNCTIONS)) (get-reg nomVM 'EOF))
							)
						)
						(error "Need function name")
					)
					(setq fnctNamed T)
				)
				(progn
					(if (string-equal (car ins) 'label )
						(progn
							(if (nth-value 1 (gethash (car (cdr ins)) LABELS))
								(error "le LABEL existe déja : ~S" (car (cdr instr)))
								(setf (gethash (car (cdr ins)) LABELS) (get-reg nomVM 'EOF))
							)
							(if (string-equal (car (cdr ins ) ) 'MAIN)
								(move nomVM 'EOF 'PC)
							)
						)

						(progn
							(if (or 
								(string-equal (car ins) 'JMP )
								(string-equal (car ins) 'JLT)
								(string-equal (car ins) 'JEQ)
								(string-equal (car ins) 'JGT)
								(string-equal (car ins) 'JLE)
								(string-equal (car ins) 'JGE)
								(string-equal (car ins) 'JNE)
								(string-equal (car ins) 'JTRUE)
								(string-equal (car ins) 'JNIL)

							)

								(if (nth-value 1 (gethash (car (cdr ins)) LABELS))
									(setf (car (cdr ins)) (gethash (car (cdr ins)) LABELS))	
									(setf (gethash (get-reg nomVM 'EOF) TEMP) (car (cdr ins)))
								)
							)
						)
					)
				)
			)
			
			(lstore nomVM ins (get-reg nomVM 'EOF))
			(inc nomVM 'EOF)
		)
		
			;Vérification des labels non résolus lors du premier passage
			;Applique la lambda suivante pour tous les labels non résolus	
			(maphash
				#'(lambda (key value)
				(if (nth-value 1 (gethash value LABELS) )
					(setf
						(car (cdr (get-mem nomVM key) ) )
						(gethash value LABELS)
					)
					(error "Undefined Label : ~S" value )
				)
				)
				TEMP 
			)
			(show-memory nomVM)
		)	
)


;============================================================================================================================;
;                             >>>>>>>>>>>>>>>>>       Exécution       <<<<<<<<<<<<<<<<<<<<<<  			    				 ;
;                 																											 ;
; >> (exe nomVM #T)                                                                                                          ;
; 		> Exécute le code en mémoire de la machine virtuelle « nomVM »                                                       ;
; 		> Paramètre optionnel #T : affichage de toutes les étapes de l’exécution dans la console                             ;
; 		> Exemple : (exe 'vm) ou (exe 'vm T)                                                                                 ;            
;============================================================================================================================;

(defun leval (nomVM ins  &optional (show NIL))
	(if show
		(show-ins nomVM ins)
	)
	(let ((call))
		(case (car ins)
			( 'MOVE (setq call 'move) )
			( 'LOAD (setq call 'lload) )
			( 'STORE (setq call 'lstore) )
			( 'PUSH (setq call 'lpush) )
			( 'POP (setq call 'lpop) )
			( 'INCR (setq call 'inc) )
			( 'DECR (setq call 'dec) )
			( 'ADD (setq call 'add) )
			( 'SUB (setq call 'sub) )
			( 'MULT (setq call 'mul) )
			( 'DIV (setq call 'div) )
			( 'CMP (setq call 'cmp) )
			( 'JMP (setq call 'jmp) )
			( 'JLT (setq call 'jlt) )
			( 'JEQ (setq call 'jeq) )
			( 'JGT (setq call 'jgt) )
			( 'JLE (setq call 'jle) )
			( 'JGE (setq call 'jge) )
			( 'JNE (setq call 'jne) )
			( 'JSR (setq call 'jsr) )
			( 'RTN (setq call 'rtn) )
			( 'LABEL (setq call 'llabel) )
			( 'HALT (setq call 'halt) )
		)
		
		(if (equal call NIL)
			(error "~S : Instruction inconnue" (car ins) )
			(apply call (cons nomVM (cdr ins)))
		)
	)	
)

(defun exe (nomVM &optional (show NIL))
	(if (= (get nomVM 'PC) 0)
		(error "no main function")
		(progn
			(if show
				(show-inss nomvm)
			)
			(loop while  (< (get nomVM 'PC) (get nomVM 'EOF))    ;exécution du code en mémoire
			do
				(leval nomVM (get-mem nomVM (get nomVM 'PC)) show)
				(inc nomVM 'PC)
			)
		)
	)
	(get nomVM 'R0)
)




;...........................................................................................................;
;                                                Outils                                                     ;
;...........................................................................................................;
	(defun halt (nomVM)
	())

;	>> (show-memory nomVM)
;		Affiche toute la mémoire de la machine virtuelle « nomVM »
	(defun show-memory (nomVM)
	  (get nomVM 'mem))

;	>> (show-regs nomVM)
;			Affiche la liste de tous les registres de la machine virtuelle « nomVM » et de leur contenu
	(defun show-regs(nomVM)
	   (setq r0 (get nomVM 'R0))
	   (setq r1 (get nomVM 'R1))
	   (setq r2 (get nomVM 'R2))
	   (setq r3 (get nomVM 'R3))
	   (setq bp (get nomVM 'BP))
	   (setq sp (get nomVM 'SP))
	   (setq mp (get nomVM 'MP))
	   (setq eof (get nomVM 'EOF))	
	   (setq pc (get nomVM 'PC))
	   (setq fp (get nomVM 'FP))
	   (setq fl (get nomVM 'FL))
	   (setq fe (get nomVM 'FE))
	   (setq fg (get nomVM 'FG))

	   (format t "All Registers :~% R0 : ~D ~% R1 : ~D ~% R2 : ~D ~% R3 : ~D ~% bp : ~D ~% sp : ~D ~% mp : ~D ~% eof : ~D ~% pc : ~D ~% fp : ~D ~% fl : ~D ~% fe : ~D ~% fg : ~D" r0 r1 r2 r3 bp sp mp eof pc fp fl fe fg)
	)


	(defun show-labels (nomvm)
		(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) labels)
	)

	(defun show-functions (nomVM)
		(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) (get nomVM 'FUNCTIONS))
	)

;	>>(show-ins nomVM ins)
;	     Affiche les registres et l’état de la pile
;	     ATTENTION : est utilisée principalement lors d’une exécution avec affichage

(defun show-ins (nomVM ins)
	
	(show-regs nomVM)

	(format t "~%Stack : [")

	(loop for i from 1 to 20
	do
		(format t "~S, " (get-mem nomVm (- (get-reg nomVM 'BP) i) ))
	)
	(format t "]~%~%~S~%" ins)
)

;	>> (show-inss nomVM)
;          Affiche la liste de toutes les cellules de la mémoire de la machine virtuelle contenant du code
(defun show-inss (nomVM)
	(loop for i from 0 to (- (get nomVM 'EOF) 1)
	do
		(format t "~S : ~S~%" i (get-mem nomVM i))
	)
	(format t "---------------------~%(RUN VM)~%")
)


;...........................................................................................................;
;                                             C O D E S  TEST                                               ;
;...........................................................................................................;

; >> code
; 		Petits test d’un saut conditionnel et d’un appel de fonction
(setf code '(

		(LABEL CODE)
		(LABEL BLABLA)

		(move 12 R0)
		(move 13 R1)
		(ADD R0 R1)

		(JMP ENDCODE)
		(LABEL MAIN)
		
		(MOVE 3 R0)
		(CMP 3 R0)
		(JEQ BLABLA)

		(LABEL ENDCODE)
		

	)
		
)
;..................................
; >> Factoriel de 5 itératif 
(setq factIT
	'(
		(LABEL FACT)

		(MOVE 1 R0)
		(MOVE FP R1)
		(INCR R1)
		(LOAD R1 R2)

		(LABEL BOUCLE)
		(MULT R2 R0)
		(DECR R2)
		(CMP 0 R2)
		(JLT BOUCLE)
		(RTN)
		(HALT)

		(LABEL MAIN)
		;Factoriel de 5 
		(MOVE 5 R0)
		(PUSH R0)
		(MOVE 1 R0)
		(PUSH R0)

		(JSR FACT)
	)
)
;.................................
; (defun fact (n)
; 	(if (= n 0)
; 		1
; 		(* n (fact ( -n 1)))
; 	)
; )
;
; >> Factoriel de 5 récursif

(setq fact
	'(
		(LABEL FACT)

		(MOVE FP R1)
		(INCR R1)
		(LOAD R1 R1)

		(CMP 0 R1)
		(JEQ ENDFACT)

		(PUSH R1)


		(DECR R1)
		(PUSH R1)
		(PUSH 1)

		(JSR FACT)

		(POP R1)

		(MULT R1 R0)

		(LABEL ENDFACT)

		(RTN)

		(HALT)

		(LABEL MAIN)
		;	Factoriel de 5	
		(MOVE 5 R0)
		(PUSH R0)
		(MOVE 1 R0)
		(PUSH R0)

		(JSR FACT)
	)
)

;.......................................
; (defun fibo (n)
; 	(if (n = 0)
; 		0
; 		(if (= n 1)
; 			1
; 		)
; 		(fibo (+ (- n 1) (- n 2)))
; 	) 
; )
;
; >>  fibonacci de 5 non terminal

(setq fiboNT
	'(
		(LABEL FIBO)

		(MOVE FP R1)
		(INCR R1)
		(LOAD R1 R1)

		;Conditions d'arrêt
		(MOVE 0 R0)
		(CMP 0 R1)
		(JEQ END_FIBO)

		(MOVE 1 R0)
		(CMP 1 R1)
		(JEQ END_FIBO)

		;Appel Fibo(n-1)

		(PUSH R1)

		(SUB 1 R1)
		
		(PUSH R1)
		(MOVE 1 R1)
		(PUSH R1)

		(JSR FIBO)

		(POP R1)
		
		(PUSH R0)

		;Appel Fibo(n-2)

		(SUB 2 R1)
		
		(PUSH R1)
		(MOVE 1 R1)
		(PUSH R1)

		(JSR FIBO)

		(POP R1)

		(ADD R1 R0)

		(LABEL END_FIBO)

		(RTN)

		(LABEL MAIN)
		; fibonacci de 5
		(MOVE 5 R0)
		(PUSH R0)
		(MOVE 1 R0)
		(PUSH R0)

		(JSR FIBO)

		(LABEL EOF)
		(HALT)
	)	

)
