; defparameter para criar vari�veis inicializ�veis v�rias vezes
(defparameter *MAPA*  '( 
	(PortoAlegre Florianopolis)
						  (Florianopolis Curitiba)
						  (Curitiba SaoPaulo)
						  (Curitiba CampoGrande)
						  (SaoPaulo Rio)
						  (SaoPaulo BH)
						  (SaoPaulo CampoGrande)
						  (Rio BH)
						  (Rio Vitoria)
						  (BH Vitoria)
						  (BH Goiania)
						  (Vitoria Salvador)
						  (Salvador BH)
						  (BH Salvador)
						  (Salvador Aracaju)
						  (Salvador Maceio)
						  (Salvador Recife)
						  (Salvador Palmas)
						  (Salvador Teresina)
						  (Salvador Goiania)
						  (Aracaju Maceio)
						  (Maceio Recife)
						  (Recife JoaoPessoa)
						  (Recife Fortaleza)
						  (Recife Teresina)
						  (JoaoPessoa Natal)
						  (JoaoPessoa Fortaleza)
						  (Natal Fortaleza)
						  (Fortaleza Teresina)
						  (Teresina SaoLuis)
						  (Teresina Palmas)
						  (SaoLuis Palmas)
						  (SaoLuis Belem)
						  (Palmas Goiania)
						  (Palmas Cuiaba)
						  (Cuiaba CampoGrande)
						  (Cuiaba Goiania)
						  (Belem Palmas)
						  (Belem Macapa)
						  (Belem Manaus)
						  (Belem BoaVista)
						  (BoaVista Manaus)
						  (Manaus RioBranco)
						  (Manaus PortoVelho)
						  (Manaus Cuiaba)
						  (PortoVelho Cuiaba)
						  (RioBranco PortoVelho)
						  (Belem Cuiaba)
						  (CampoGrande Goiania)
					    )
)

(defun adjacentes (cidade)
	(
		let 
			( (adj nil) )
			(dolist (l *MAPA*) 
				(cond 
					(	; A�AO 1
						(equal cidade (car l)) ; SE a cidade for igual a cabe�a da lista pega no dolist ENT�O
							(setq adj (append adj (list  (cadr l) ))) ; Adiciono o corpo na lista de adjacentes
					)
				
					( ; A�AO 2
						(equal cidade (car (cdr l))) ; SE a cidade for igual ao corpo da lista pega no dolist ENT�O
							(setq adj (append adj (list  (car l) ))) ; Adiciono a cabe�a na lista de adjacentes
					)
				)
			)
		adj 		
	)
)


; teste feito (busca_profundidade 'curitiba 'curitiba '() '())
; (busca_profundidade 'curitiba 'recife '() '())
; Profundidade
(defun profundidade (ca cf l_cam l_vis)  
	(cond
		(  	; A��O 1 - - - DESEMPILHA
			(equal ca cf) (setq l_vis  (append l_vis (list ca) ) ) 
		) 	; se for a mesma cidade pego a ca e coloco na l_vis
			
		( 	; A��O 2 - - - EMPILHA
			(eq (find ca l_vis )  nil) ; Verifica se a cidade ja nao pertence a lista de visitados
			(dolist (corrente (adjacentes ca) ) ; percorre a lista
				(setq l_cam (profundidade corrente cf l_cam (append l_vis (list ca )) )) ; Pega os adjacentes de cada cidade e colocando na lista de resultado
				(when l_cam (return l_cam))
			)
		)
	)
)

; BUSCA EM LARGURA
(defun largura (caminho_corrente destino lista_caminhos VISITADOS PERCORRIDOS)
	( ; Inicio do cond . . . 
		cond
		
		(   ; AC�O 1  - - - Se A��O 1 true n�o executo A��O 2
			(equal caminho_corrente destino) (setq VISITADOS (append VISITADOS (list caminho_corrente) ) ) 
		 	; Se o caminho corrente for igual ao meu destino coloco o meu caminho corrente na lista de visitados
			; Por consequencia essa linha me retornar� true, e como estou dentro do cond 
			; eu n�o irei cair em outra chamada recursiva. Eu vou desempilhar essa chamada recursiva
		)
		
		(   ; AC�O 2 - - - A��O 2 FAZ CHAMADA RECURSIVA
			
			(
				eq (find caminho_corrente VISITADOS ) nil
			) ; Verifica se a cidade corrente n�o esta na minha lista de visitados 
									  
			; Coloca na lista de percorrer
			
			(	
				dolist (c (adjacentes caminho_corrente) ) ; c recebe os adjacentes da cidade corrente
				
				(
					cond 
					( 
						(and (eq (find c PERCORRIDOS ) nil) (eq (find c VISITADOS) nil) ) (setq PERCORRIDOS (append PERCORRIDOS (list c) ))
					) ; Se a cidade corrente nao estiver na lista de percorridos
				) 	  ; AND 
					  ; Se a cidade corrente nao estiver na lista de visitados, eu coloco ela na lista visitados
			)
			
			(setq lista_caminhos (largura (car PERCORRIDOS) destino lista_caminhos (append VISITADOS (list caminho_corrente)) (cdr PERCORRIDOS) )) 
			; Ap�s eu ter atualizado o meu caminho percorrido, eu fa�o uma nova chamada recursiva
			; Primeiro argumento = A cabe�a dos caminhos 
			; Segundo Argumento = Caminho Final
			; Terceiro Argumento = Caminhos . . .Ele permanece intacto at� eu come�ar a desempilhar as chamadas recursivas
			; Quarto Argumento = Ele � atualizado com a cabe�a da lista da chamada recursiva anterior, Ou seja, ele � meus visitados
			; Quinto Argumento = destino_final recebe todos os adjacentes da cidade corrente. Ou seja, ele � o meu leque de 
			; possibilidades aonde eu posso OU n�o visitar com a minha lista de visitados 
			; . . . Porque se a A��O 1 retornar true eu vou iniciar o desempilhamento das chamadas
						
		)
	) ; Final do cond . . . 
)



(defun distancia_d (cidade d)
	(setq lista_RESULTADO nil) ; inicia a lista com nil
	(setq lista_caminhos nil) ; inicia a lista com nil
	(distancia_cidade cidade d)
	lista_RESULTADO ; retorna a lista
)

#|
Parada: 
- Quando d for 0, coloco true no meu resultado, logo depois
verifico se a cidade corrente ja n�o esta na lista de resultados.
- Se n�o no segundo progn percorro os adjacentes da cidade e vou decrementando o d em 1 ate cair na condi��o de parada,
para desempilhar as chamadas recursivas e retornar o meu resultado
|#

(defun distancia_cidade (cidade d)
	(let ((resultado nil) (adjacencia) (lista_caminhos nil)(lista_RESULTADO nil)) ; vari�veis da chamada recursiva
		(setq adjacencia (adjacentes cidade)) ; Obter os adjacentes da cidade corrente
		(
			if (eq d 0) ; Quando d for 0, eu cheguei no meu objetivo, e n�o irei mais fazer chamadas recursivas
				(;{
					progn ; A��O 1 - - - Quando cheguei no meu objetivo que � d = 0
						(setq resultado t) ; Resultado recebe verdadeiro quando d = 0
						(
							if (eq (find cidade lista_RESULTADO) nil) ; Verifica se a cidade corrente ja n�o esta na lista de resultados
								(setq lista_RESULTADO (append lista_RESULTADO (list cidade))) ; Coloca a cidade corrente na lista caso ela n�o esteja
						)
				);}
				(;{ Simula um else
				progn ; A��O 2 - - - Fazendo chamadas recursivas enquanto d != 0 e percorrendo os adjacentes da cidade da chamada recursiva em quest�o
					(
						dolist (cidade adjacencia) ; Percorre a lista de adjacentes da cidade corrente
						(setq resultado (distancia_cidade cidade (- d 1))) 
						; A cada cidade que eu passo dist � decrementado e quando d = 0 � a hora de desempilhar as chamadas recursivas
					)
				);}
		)
		resultado 
		; Esse resultado ser� T quando d for igual a 0, e eu s� vou chegar nessa linha quando eu n�o cair em uma chamada
		; recursiva . . . E essa possibilidade s� � possivel quando meu d � igual a 0
	)
)


; lista
; Parametros: Cidade inicial e cidade final
(defun distancias (ca destino_final)
	(setq lista_RESULTADO nil)
	(setq lista_caminhosinho nil)
	(busca_e_Distancia ca destino_final 0 nil) ; Chama a fun��o que busca a distancia
	lista_RESULTADO ; Retorna o resultado
)



#|condi��o de parada: se ciade atual igual a cidade final que estou procurando,
coloco verdadeiro na variavel resultado e depois testo se a distancia ja pertence ou nao a lista_RESULTADO(lista resultado) senao tiver,
coloca na lista de resultado.
no else percorro os adjacentes da cidade atual, a cada cidade que eu percorrer aumento(incremento) a variavel dist em 1 |#

; Recebe: cidade inicial, cidade final, distancia e lista de cidades visitadas
(defun busca_e_Distancia (ca destino_final dist visitados)
	(let ((resultado nil) (adjacencia)) ; Em cada chamada recursiva resultado � nil mas se eu atingir o objetivo eu coloco t nele
		(setq adjacencia (adjacentes ca)) ; adjacencia recebe os adjacentes da cidade corrente
		
		(if (eq (find ca visitados) nil);{ (inicio do primeiro if) se a cidade atual n�o tiver sido visitada ainda, ent�o (vai procurar se a cidade que eu estou ja nao esta na lista de visitados)
			(progn
				(setq visitados (append visitados (list ca))) ; coloca a cidade na lista de visitadas
				(if (eq ca destino_final) ;{ Testa se a cidade corrente � igual a cidade destino, ou seja, meu crit�rio de parada
					( ; A��O 1
						progn
							(setq resultado t) ; Quando eu encontro a dist�ncia eu coloco t em resultado
								(
									if (eq (find dist lista_RESULTADO) nil) ; Testa se a distancia j� existe na lista, ou seja, vou procurar se a distancia que eu achei ja nao esta na lista de resultado
										(
											setq lista_RESULTADO (append lista_RESULTADO (list dist)) ; Coloca a distancia na lista
										) 
								)
					);}
					
					(; A��O 2 else{
						progn 
							(dolist (cidade adjacencia) ; percorre a lista de adjacencias da cidade atual
								(setq resultado (busca_e_Distancia cidade destino_final (+ dist 1) visitados)) ; a dist�ncia � incrementada na chamada  recursiva
							)
					);}
					
				)
			)
		);} fim do primeiro if . . .
		
		resultado
		; Quando eu cair na A��O 1 � porque o meu caminho inicial � igual ao o meu caminho final . . .
		; E como a A��O 1 e 2 est�o dentro do primeiro if e a A��O 1 n�o tem chamada recursiva quando a A��O 1 terminar
		; eu irei sair do primeiro if e cair na linha aonde tenho resultado . . . E resultado ser� retornada . . .
		; No primeiro desempilhamento resultado ser� true . . . Mas depois o resultado recebera o valor das dist�ncias . . .
		; entre as chamadas recursivas
	)
)
