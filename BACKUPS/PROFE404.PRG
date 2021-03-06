/* SIMA - CONTABILIDAD PROFESORES

MODULO      : RECUPERACIONES
SUBMODULO...: MODIFICAR

**************************************************************************
* TITULO..: MODIFICACION DE RECUPERACIONES                               *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: MAY 30/2002 JUE A
       Colombia, Bucaramanga        INICIO: 04:00 AM   MAY 30/2002 JUE

OBJETIVOS:

1- Permite modificar las recuperaciones

2- Retorna NIL

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Profe_404(aP1,aP2,aP3)

*>>>>DESCRIPCION DE PARAMETROS
/*     aP1                                  // Parametros Generales
       aP2                                  // Parametros Generales
       aP3                                  // Parametros Generales */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       #INCLUDE "CAMPOS\ARC-SPR.PRG"       // Archivos del Sistema

       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL cSavLin := ''                  // Salvar Linea
       LOCAL lHayErr := .F.                 // .T. Hay Error
     *�Variables generales

       LOCAL cSavCab := ''                  // Salvar Encabezado
       LOCAL       i := 0                   // Contador

       LOCAL nFilSup := 0                   // Columna Superior
       LOCAL nColSup := 0                   // Columna Superior
       LOCAL nInfFil := 0                   // Fila Inferior
       LOCAL nInfCol := 0                   // Columna Inferior

       LOCAL lOtrFil := .F.                 // .T. Otra Fila
       LOCAL nNroFil := 0                   // Fila de lectura
       LOCAL nNroCol := 1                   // Columna de lectura

       LOCAL nFilIni := 0                   // Fila Inicial
       LOCAL nColIni := 0                   // Columna Inicial
       LOCAL nFilFin := 19                   // Fila Final de Control

       LOCAL lIndNiv := .F.                 // .T. Indicadores de Nivel
       LOCAL lEvaInd := .F.                 // .T. Evaluar el Indicador
       LOCAL cEvaInd := ''                  // Evaluar el Indicador
       LOCAL lNotNum := .F.                 // .T. Nota N�merica .F. Nota Cualitativa
       LOCAL cValNot := ''                  // Validaci�n de la nota

       LOCAL nTotPer := 5                   // Total de Periodos
       LOCAL nNroPer := 0                   // N�mero del periodo

       LOCAL aNroCol := {}                  // N�meros de Columnas
       LOCAL aLenNom := {}                  // Longitud de nombres
       LOCAL nLenNom := 0                   // Longitud del nombre
       LOCAL nTotInd := 20                  // Total de Indicadores
       LOCAL nNroInd := 0                   // N�mero de indicadores
       LOCAL nIndMin := 0                   // Indicador M�nimo para control
       LOCAL nNroLog := 0                   // N�mero de logro escogido
*      LOCAL cNoNota := ''                  // Espacios de la nota. Marca Error el la Macro al declararse local
*      LOCAL nInicio := 0                   // Puntero de Inicio. Marca Error el la Macro al declararse local
       LOCAL lAllNot := .F.                 // .T. Todas las notas grabadas
       LOCAL lExiNot := .F.                 // .T. Existen notas grabadas
       LOCAL lOtrEst := .F.                 // .T. Otro Estudiante
       LOCAL cCodLog := ''                  // C�digo del logro
       LOCAL cLogros := ''                  // Logros aplicados
       LOCAL cNalias := ''                  // Alias del Maestro
       LOCAL nLenCod := 6                   // Longitud del C�digo

       LOCAL cLogNot := ''                  // Logros de Notas
       LOCAL cNotInd := ''                  // Nota de los Indicadores
       LOCAL cNotDef := ''                  // Nota Definitiva en Notas
       LOCAL cNotRec := ''                  // Nota Recuperaci�n en Notas

       LOCAL cLogNoP := ''                  // Logros de Notas del Profesor
       LOCAL cNtINoP := ''                  // Nota de los Indicadores en Profesor
       LOCAL cNoPDef := ''                  // Nota Definitiva en Profesor
       LOCAL cNoPRec := ''                  // Nota Recuperaci�n en Notas
       LOCAL cMsgTxt := ''                  // Mensaje Temporal

       LOCAL lHayNot := .F.                 // .T. Hay Nota

       LOCAL cRecupe := ''                  // Indicadores por Recuperar
       LOCAL nPosInd := ''                  // Posici�n del Indicador
       LOCAL lRecNot := .F.                 // .T. Recuperar Nota

       LOCAL aGenLog := {}                  // Campos Generales
       LOCAL aCamLog := {}                  // Campos del Log
       LOCAL cTxtLog := ''                  // Texto del Log
       LOCAL cHorIni := ''                  // Hora de Inicio
       LOCAL lHayLog := .F.                 // .T. Hay Log
       LOCAL lLeeRec := .T.                 // .T. Leer Recuperaci�n
     *�Variables del Log

       LOCAL GetList := {}                  // Variable del Sistema
     *�Variables espec�ficas 

       LOCAL cCodigoTes := ''               // C�digo del estudiante
       LOCAL cNombreTes := ''               // Nombre del estudiante
       LOCAL cCodigoTma := ''               // C�digo de la Materia
       LOCAL cCodigoTgr := ''               // C�digo del grupo
       LOCAL cCodigoTni := ''               // C�digo del nivel
       LOCAL cCodigoTcl := ''               // C�digo Clase del indicador
       LOCAL nTipCarTca := 0                // Tipo de carga

       LOCAL cCamIndNot := ''               // Campo Indicadores en Notas
       LOCAL cCamNtINot := ''               // Campo de Nota de Indicadores
       LOCAL cCamNotDef := ''               // Campo Notas Definitivas
       LOCAL cCamNotRec := ''               // Campo Notas de Recuperaci�n

       LOCAL cCamIndNoP := ''               // Campo Indicadores en Notas Profesor
       LOCAL cCamNtINop := ''               // Campo de Nota de Indicadores
       LOCAL cCamNoPDef := ''               // Campo Nota Definitvia en Notas Profesor
       LOCAL cCamNoPRec := ''               // Campo Nota Recuperaci�n en Notas Profesor

       LOCAL lMatSelTma := .F.              // .T. Materia Selectiva
       LOCAL cIndic1Tlo := ''               // Indicadores Antes
     *�Variables de Campo
*>>>>FIN DECLARACION DE VARIABLES

*>>>>DECLARACION PARAMETROS GENERALES
       LOCAL lShared := xPrm(aP1,'lShared') // .T. Sistema Compartido
       LOCAL nModCry := xPrm(aP1,'nModCry') // Modo de Protecci�n
       LOCAL cCodSui := xPrm(aP1,'cCodSui') // C�digo del Sistema
       LOCAL cNomSis := xPrm(aP1,'cNomSis') // Nombre del Sistema
     *�Detalles del Sistema

       LOCAL cEmpPal := xPrm(aP1,'cEmpPal') // Nombre de la Empresa principal
       LOCAL cNitEmp := xPrm(aP1,'cNitEmp') // Nit de la Empresa
       LOCAL cNomEmp := xPrm(aP1,'cNomEmp') // Nombre de la Empresa
       LOCAL cNomSec := xPrm(aP1,'cNomSec') // Nombre de la Empresa Secundario
       LOCAL cCodEmp := xPrm(aP1,'cCodEmp') // C�digo de la Empresa
     *�Detalles de la Empresa

       LOCAL cNomUsr := xPrm(aP1,'cNomUsr') // Nombre del Usuario
       LOCAL cAnoUsr := xPrm(aP1,'cAnoUsr') // A�o del usuario
       LOCAL cAnoSis := xPrm(aP1,'cAnoSis') // A�o del sistema
       LOCAL cPatSis := xPrm(aP1,'cPatSis') // Path del sistema
     *�Detalles del Usuario

       LOCAL PathW01 := xPrm(aP1,'PathW01') // Sitio del Sistema No.01
       LOCAL PathW02 := xPrm(aP1,'PathW02') // Sitio del Sistema No.02
       LOCAL PathW03 := xPrm(aP1,'PathW03') // Sitio del Sistema No.03
       LOCAL PathW04 := xPrm(aP1,'PathW04') // Sitio del Sistema No.04
       LOCAL PathW05 := xPrm(aP1,'PathW05') // Sitio del Sistema No.05
       LOCAL PathW06 := xPrm(aP1,'PathW06') // Sitio del Sistema No.06
       LOCAL PathW07 := xPrm(aP1,'PathW07') // Sitio del Sistema No.07
       LOCAL PathW08 := xPrm(aP1,'PathW08') // Sitio del Sistema No.08
       LOCAL PathW09 := xPrm(aP1,'PathW09') // Sitio del Sistema No.09
       LOCAL PathW10 := xPrm(aP1,'PathW10') // Sitio del Sistema No.10
     *�Sitios del Sistema

       LOCAL PathUno := xPrm(aP1,'PathUno') // Path de Integraci�n Uno
       LOCAL PathDos := xPrm(aP1,'PathDos') // Path de Integraci�n Dos
       LOCAL PathTre := xPrm(aP1,'PathTre') // Path de Integraci�n Tres
       LOCAL PathCua := xPrm(aP1,'PathCua') // Path de Integraci�n Cuatro
     *�Path de Integraci�n

       LOCAL cMaeAlu := xPrm(aP1,'cMaeAlu') // Maestros habilitados
       LOCAL cMaeAct := xPrm(aP1,'cMaeAct') // Maestro Activo
       LOCAL cJorTxt := xPrm(aP1,'cJorTxt') // Jornada escogida
     *�Detalles Acad�micos

       LOCAL nFilPal := xPrm(aP1,'nFilPal') // Fila Inferior Men� principal
       LOCAL nFilInf := xPrm(aP1,'nFilInf') // Fila Inferior del SubMen�
       LOCAL nColInf := xPrm(aP1,'nColInf') // Columna Inferior del SubMen�
     *�Detalles Tecnicos
*>>>>FIN DECLARACION PARAMETROS GENERALES

*>>>>DECLARACION DE PARAMETROS ESPECIFICOS
       LOCAL cCodPro := xPrm(aP1,'cCodPro') // C�digo del Profesor
       LOCAL cPatPro := xPrm(aP1,'cPatPro') // Path del Profesor
       LOCAL cNomPro := xPrm(aP1,'cNomPro') // Nombre del Profesor
       CloseAll()
*>>>>DECLARACION DE PARAMETROS ESPECIFICOS


*>>>>SELECION DE LAS AREAS DE TRABAJO
       IF !lUseMae(lShared,PathUno+'\'+cPatSis,cMaeAct,cAnoSis)              .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileMat,'MAT',NIL,lShared)                .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileNiv+cAnoSis+ExtFile,'NIV',NIL,lShared) .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileInd+cMaeAct+cAnoSis+ExtFile,'IND',;
		       PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FNtxInd+cMaeAct+cAnoSis+cExtNtx,lShared)   .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileCla,'CLA',NIL,lShared)                 .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileEva,'EVA',NIL,lShared)                 .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileJvf,'JVF',NIL,lShared)                 .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FConTbl+cMaeACt+ExtFile,'TCO',NIL,lShared) .OR.;
	  !lUseDbf(.T.,cPatPro+'\'+FileLog,'LOG',NIL,lShared)     .OR.;
	  !lUseDbf(.T.,cPatSis+'\'+;
		       fSprAno+cAnoUsr+ExtFile,'PRA',NIL,lShared) .OR.;
	  !lUseDbf(.T.,cPatSis+'\'+cMaeAct+'\'+;
		       FProTbl+cMaeAct+ExtFile,'TPR',NIL,lShared) .OR.;
	  !lUseDbf(.T.,cPatPro+'\'+;
		       FCarPro+cAnoSis+ExtFile,'CAP',NIL,lShared)
	  cError('ABRIENDO ARCHIVOS')
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN SELECION DE LAS AREAS DE TRABAJO

*>>>>VALIDACION DE CONTENIDOS DE ARCHIVOS
       lHayErr := .T.
       SELECT &cMaeAct
       DO CASE
       CASE RECCOUNT() == 0
	    cError('NO EXISTEN ESTUDIANTS GRABADOS')

       CASE MAT->(RECCOUNT()) == 0
	    cError('NO EXISTEN MATERIAS GRABADAS')

       CASE NIV->(RECCOUNT()) == 0
	    cError('NO EXISTEN NIVELES GRABADOS')

       CASE IND->(RECCOUNT()) == 0
	    cError('NO EXISTEN INDICADORES DE LA CONTABILIDAD')

       CASE CLA->(RECCOUNT()) == 0
	    cError('NO EXISTEN LAS CLASES DE LOGROS')

       CASE EVA->(RECCOUNT()) == 0
	    cError('NO EXISTEN LOS CODIGOS DE EVALUACION')

       CASE TCO->(RECCOUNT()) == 0
	    cError('NO EXISTEN CONFIGURACION PARA EL NIVEL DE LA CONTABILIDAD')

       CASE PRA->(RECCOUNT()) == 0
	    cError('NO EXISTE CONFIGURACION DEL A�O')

       CASE CAP->(RECCOUNT()) == 0
	    cError('NO EXISTE CARGA ACADEMICA DEL PROFESOR')
       OTHERWISE
	    lHayErr :=.F.
       ENDCASE
       IF lHayErr
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN VALIDACION DE CONTENIDOS DE ARCHIVOS

*>>>>SELECCION DE LA MATERIA DEL GRUPO
       SELECT CAP
       IF CAP->(RECCOUNT()) # 0

**********INICIALIZACION DE LAS COORDENADAS
	    nFilSup := nFilInf+2
	    nColSup := nColInf
	    IF nFilSup+RECCOUNT() > 22
	       nInfFil := 21
	    ELSE
	       nInfFil := nFilSup + RECCOUNT()-1
	    ENDIF
	    nInfCol := nColSup+LEN(CAP->cNombreMat)+2
**********FIN INICIALIZACION DE LAS COORDENADAS

**********BROWSE DE SELECCION
	    DO WHILE .T.

*--------------SELECCION DEL REGISTRO
		 SELECT CAP
		 IF nBrowseDbf(nFilSup,02,nInfFil,78,;
			      {||SUBS(CAP->cCodigoCar,5,4)+' '+;
			       Materia()}) == 0
		    CLOSE ALL
		    RETURN NIL
		 ENDIF
		 lLocCodigo('cCodigoMat','MAT',SUBS(CAP->cCodigoCar,1,4))
		 lMatSelTma := MAT->lMatSelMat
*--------------FIN SELECCION DEL REGISTRO

*--------------PREGUNTA DE DECISION
		 cMsgTxt := 'ESCOGIO '+ALLTRIM(CAP->cNombreMat)+' '+;
			    'DEL GRUPO: '+SUBS(CAP->cCodigoCar,5,4)
		 cMsgTxt := SUBS(cMsgTxt,1,50)

		 IF lPregunta(cMsgTxt+' DESEA CONTINUAR? Si No')
		    EXIT
		 ENDIF
*--------------FIN PREGUNTA DE DECISION

	    ENDDO
**********FIN BROWSE DE SELECCION

       ENDIF
       cCodigoTgr := SUBS(CAP->cCodigoCar,5,4)
       cCodigoTni := SUBS(cCodigoTgr,1,2)
       cCodigoTma := SUBS(CAP->cCodigoCar,1,4)
       nTipCarTca := CAP->nTipCarCar
*>>>>FIN SELECCION DE LA MATERIA DEL GRUPO

*>>>>LECTURA DEL NUMERO DEL BIMESTRE
       nNroPer := nLeeNroPer(nFilInf+1,nColInf,nFilPal,nTotPer)

       lHayErr := .T.
       DO CASE
       CASE nNroPer == 0
       CASE nNroPer == TCO->nPerActTbl .AND. TCO->nRecPerTbl == 0
	    cError('NO SE PERMITEN RECUPERACIONES DEL PERIODO ACTUAL')

       CASE nNroPer > TCO->nPerActTbl
	    cError('NO SE PERMITEN RECUPERACIONES DE PERIODOS POSTERIORES AL PERIODO ACTUAL')

       OTHERWISE
	    lHayErr :=.F.
       ENDCASE
       IF lHayErr
	  CloseAll()
	  RETURN NIL
       ENDIF
*>>>>FIN ANALISIS PARA LA SALIDA DEL PROGRAMA

*>>>>CALCULO DEL CAMPO DE INDICADORES DE NOTAS
       IF !lMatSelTma
	  cCamIndNot := cCamJuiNot(lShared,cAnoUsr,cMaeAct,;
				   PathUno+'\'+cPatSis,nNroPer,cCodigoTgr,;
				   cCodigoTma,NIL,@cCamNotDef,;
				   @cCamNotRec,@cCamNtINot)

	  IF EMPTY(cCamIndNot) .OR.;
	     EMPTY(cCamNotDef) .OR. EMPTY(cCamNotRec)
	     CloseAll()
	     RETURN NIL
	  ENDIF

       ENDIF
       cCamIndNoP := 'NOP->cIndic'+STR(nNroPer,1,0)+'NoP'
       cCamNtINoP := 'NOP->cIndnt'+STR(nNroPer,1,0)+'NoP'
       cCamNoPDef := 'NOP->cDefin'+STR(nNroPer,1,0)+'NoP'
       cCamNoPRec := 'NOP->cDefRe'+STR(nNroPer,1,0)+'NoP'
*>>>>FIN CALCULO DEL CAMPO DE INDICADORES DE NOTAS

*>>>>SELECION DE LAS AREAS DE TRABAJO
       IF !lUseDbf(.T.,cPatPro+'\'+;
		       CAP->cCodigoCar+ExtFile,'NOP',NIL,lShared)

	  cError('ABRIENDO EL ARCHIVO DE NOTAS DEL DEL PROFESOR '+;
		 CAP->cCodigoCar)
	  CloseAll()
	  RETURN NIL
       ENDIF
*>>>>FIN SELECION DE LAS AREAS DE TRABAJO

*>>>>IMPRESION DE LOS ENCABEZADOS
       nNroFil := nMarco(nFilPal+1,;
			 ALLTRIM(MAT->cNombreMat),22,'�')
       @ nNroFil,01 SAY 'CODIGO'
       @ nNroFil,08 SAY 'NOMBRE'
*>>>>FIN IMPRESION DE LOS ENCABEZADOS

*>>>>LECTURA DE LA CONFIGURACION DEL NIVEL DE LA CONTABILIDAD
       lNotNum := IF(SUBS(TCO->cNotNumTbl,nNroPer,1)='1',.T.,.F.)
       lIndNiv := TCO->lIndNivTbl
       lRecNot := TCO->lRecNotTbl
       IF lNotNum
	  cValNot := ALLTRIM(TCO->cValNumTbl)+SPACE(01)
       ELSE
	  cValNot := ALLTRIM(TCO->cValNotTbl)+SPACE(01)
       ENDIF
*>>>>FIN LECTURA DE LA CONFIGURACION DEL NIVEL DE LA CONTABILIDAD

*>>>>DETERMINACION DE COLS Y LONGITUD DE NOMBRE
       aNroCol := {37,31,25,19,37}
       aLenNom := {31,25,18,13,28}

       nIndMin := 10
       nNroCol := IF(nIndMin < 7,aNroCol[5],aNroCol[nIndMin-6])
      *Escoge la columna seg�n el n�mero de notas

       nLenNom := IF(nIndMin < 7,aLenNom[5],aLenNom[nIndMin-6])
      *Escoge la longitud del nombre

       nFilIni := 06
       nColIni := nNroCol
       nNroFil := nFilIni
*>>>>FIN DETERMINACION DE COLS Y LONGITUD DE NOMBRE

*>>>>IMPRESION DE LOS ENCABEZADOS DE LAS NOTAS
       i := 1
       DO WHILE i <= nIndMin
	  @ nNroFil,nNroCol SAY (i<10,'INDI','IND')+LTRIM(STR(i))
	  i++
	  nNroCol := nNroCol + 6
       ENDDO

       nNroCol = nNroCol - (i-1)*6
       nNroFil = nNroFil + 1
      *Ubica la fila primera de grabacion

       cSavCab := SAVESCREEN(nNroFil-1,0,nNroFil-1,79)
       cSavLin := SAVESCREEN(21,0,22,79)
*>>>>FIN IMPRESION DE LOS ENCABEZADOS DE LAS NOTAS

*>>>>GRABACION DE LAS NOTAS DE LOS ESTUDIANTES
       lOtrEst := .T.
       DO WHILE lOtrEst

**********LECTURA DEL CODIGO DEL ESTUDIANTE
	    cHorIni := TIME()
	    cCodigoTes := cLeeCodEst(nNroFil,01,cMaeAct,@cNalias)
	    RESTSCREEN(21,0,22,79,cSavLin)
	    IF EMPTY(cCodigoTes) .OR. cNalias == NIL
	       lOtrEst := .F.
	       LOOP
	    ENDIF
	    cNombreTes := RTRIM(&cNalias->cApelliEst)+' '+;
			  RTRIM(&cNalias->cNombreEst)

	    @ nNroFil,08 SAY SUBS(cNombreTes,1,nLenNom)  PICT '@A'
**********FIN LECTURA DEL CODIGO DEL ESTUDIANTE

**********CALCULO DEL CAMPO DE INDICADORES DE NOTAS
	    IF lMatSelTma
	       CloseDbf('PLA')
	       CloseDbf('NOT')

	       cCamIndNot := cCamJuiNot(lShared,cAnoUsr,cMaeAct,;
					PathUno+'\'+cPatSis,;
					nNroPer,&cNalias->cCodigoGru,;
					cCodigoTma,lMatSelTma,;
					@cCamNotDef,@cCamNotRec,@cCamNtINot)

	       IF EMPTY(cCamIndNot)
		  LOOP
	       ENDIF

	    ENDIF
**********FIN CALCULO DEL CAMPO DE INDICADORES DE NOTAS

**********LOCALIZACION DEL ESTUDIANTE EN NOTAS
	    cLogNot := ''
	    cNotInd := ''
	    cNotDef := ''
	    cNotRec := ''

	    IF lLocCodigo('cCodigoEst','NOT',cCodigoTes)

	       cLogNot := &cCamIndNot

	       cNotDef := SUBS(&cCamNotDef,nNroPer*4-3,4)
	       cNotRec := SUBS(&cCamNotRec,nNroPer*4-3,4)
	       IF !EMPTY(cNotRec)
		  cNotDef := cNotRec
	       ENDIF

	       IF SUBS(TCO->cEvaIndTbl,nNroPer,1) == '2'
		  cNotInd := &cCamNtINot
	       ENDIF

	    ENDIF
	    IF EMPTY(cLogNot)
	       cError('NO APARECEN INDICADORES EN NOTAS')
	       LOOP
	    ENDIF
**********FIN LOCALIZACION DEL ESTUDIANTE EN NOTAS

**********LOCALIZACION DEL ESTUDIANTE EN NOTAS PROFESOR
	    cLogNoP := ''
	    cNtINoP := ''
	    cNoPDef := ''
	    cNoPRec := ''

	    IF lLocCodigo('cCodigoEst','NOP',cCodigoTes)

	       cLogNoP := &cCamIndNoP
	       cNtINoP := &cCamNtINoP

	       cNoPDef := &cCamNoPDef
	       cNoPRec := &cCamNoPRec
	       IF !EMPTY(cNoPRec)
		  cNoPDef := cNoPRec
	       ENDIF

	    ENDIF

	    IF EMPTY(cLogNoP)
	       IF lMatSelTma
		  cError('NO EXISTEN INDICADORES VERIFIQUE '+;
			 'SI EL ESTUDIANTE FUE TRASLADADO')
	       ELSE
		  cError('NO EXISTEN INDICADORES APLICADOS PARA EL '+;
			 'PERIODO No. '+STR(nNroPer,1))
	       ENDIF
	       LOOP
	    ENDIF
**********FIN LOCALIZACION DEL ESTUDIANTE EN NOTAS PROFESOR

**********VALIDACION DE LOS CODIGOS
	    IF cLogNot # cLogNoP
	       cError('EXISTE INCONSISTENCIA EN LOS INDICADORES. '+;
		      'FAVOR INFORMAR.')
	       LOOP
	    ENDIF
**********FIN VALIDACION DE LOS CODIGOS

**********VALIDACION DE LOS REGISTROS
	    IF NOT->cCodigoEst # NOP->cCodigoEst
	       cError('LOS CODIGOS:'+NOT->cCodigoEst+':'+NOP->cCodigoEst+' '+;
		      'DEL ESTUDIANTE NO SON IGUALES')
	       LOOP
	    ENDIF
**********FIN VALIDACION DE LOS REGISTROS

**********ANALISIS DE LOS INDICADORES POR RECUPERAR
	    cRecupe := ''
	    nNroInd := 0
	    FOR i := 1 TO 20

		cCodLog := SUBS(cLogNot,nPosIni(i,nLenCod),nLenCod)

		IF SUBS(TCO->cEvaIndTbl,nNroPer,1) == '0'

*------------------RECUPERACIONES POR CLASE DE INDICADOR
		     IF lLocCodigo('cCodigoCla','CLA',SUBS(cCodLog,1,1)).AND.;
			CLA->lRecupeCla

			cRecupe += cCodLog
			nNroInd++

		     ENDIF
*------------------FIN RECUPERACIONES POR CLASE DE INDICADOR

		ELSE

*------------------RECUPERACIONES POR TIPO DE EVALUACION
		     IF lLocCodigo('cCodigoEva','EVA',SUBS(cCodLog,5,1)).AND.;
			EVA->lRecupeEva

			cRecupe += cCodLog
			nNroInd++

		     ENDIF
*------------------FIN RECUPERACIONES POR TIPO DE EVALUACION

		ENDIF
	    ENDFOR
	    IF EMPTY(cRecupe) .AND. !lRecNot
	       cError('NO APARECEN INDICADORES POR '+;
		      'RECUPERAR DEL PERIODO No. '+STR(nNroPer,1))
	       LOOP
	    ENDIF
**********FIN ANALISIS DE LOS INDICADORES POR RECUPERAR

**********LECTURA DE LAS NOTAS ACTUALES
	    cLogros := cRecupe
	    cEvaInd := SUBS(TCO->cEvaIndTbl,nNroPer,1)

	    lHayNot := .F.
	    IF lRecNot
	       IF lHayItvJvf()
		  lHayNot := IF(cNotDefJvf(VAL(cNotDef)) $ 'ID',.T.,.F.)
	       ELSE
		  lHayNot := IF(ALLTRIM(cNotDef) $ 'ID',.T.,.F.)
	       ENDIF
	    ENDIF

	    IF !EMPTY(cLogros)
	       DisplayLog(nNroFil,nColIni,cEvaInd,;
			  cLogros,cNotInd,nNroInd,lHayNot,cNotDef)
	    ENDIF
**********FIN LECTURA DE LAS NOTAS ACTUALES

**********VALIDACION DE LOS LOGROS APLICADOS
	    IF EMPTY(cLogros) .AND. !lRecNot
	       cError('NO APLICADO LOS LOGROS')
	       CloseAll()
	       RETURN NIL
	    ENDIF
**********FIN VALIDACION DE LOS LOGROS APLICADOS

**********ANALISIS DEL CAMBIO DE LA FILA
	    IF lOtrFil
	       IF i > 10
		  nNroFil--
		  nNroCol = nColIni
	       ENDIF
	    ENDIF
**********FIN ANALISIS DEL CAMBIO DE LA FILA

**********CAPTURA Y VALIDACION DE LOS LOGROS
	    IF !lLeeEva(cNomSis,@nNroFil,nColIni,nNroPer,lIndNiv,cEvaInd,;
			cCodigoTgr,cCodigoTni,nNroInd,nNroLog,;
			@cLogros,@cNotInd,lHayNot,lNotNum,@cNotDef,;
			cValNot,cSavLin,lLeeRec,cLogNot)
	       lOtrEst := .F.
	       LOOP
	    ENDIF
	    IF nNroLog == 0
	       RESTSCREEN(21,0,22,79,cSavLin)
	    ENDIF
**********FIN CAPTURA Y VALIDACION DE LOS LOGROS

**********CORREGIR LAS NOTAS
	    IF !lPregunta('Desea Grabar? Si No')

	       IF nNroInd > 10
		  nNroFil--
		  nNroCol := nColIni
	       ENDIF

	       LOOP
	    ELSE
	       RESTSCREEN(21,0,22,79,cSavLin)
	    ENDIF
**********FIN CORREGIR LAS NOTAS

**********REEMPLAZO DE LOS INDICADORES
	    FOR i := 1 TO nNroInd
		cCodLog := SUBS(cLogros,nPosIni(i,nLenCod),nLenCod)
		nPosInd := AT(SUBS(cCodLog,1,4),cLogNot)
		cLogNot := STUFF(cLogNot,nPosInd+5,1,SUBS(cCodLog,6,1))
	    NEXT
**********FIN REEMPLAZO DE LOS INDICADORES

**********GRABACION DE LAS RECUPERACIONES EN NOTAS
	    cNotDef := IF(VAL(cNotDef)==0,SPACE(04),cNotDef)

	    SELECT NOT
	    IF NOT->(lRegLock(lShared,.F.))

	       cIndic1Tlo := &cCamIndNoP
	       REPLACE &cCamIndNot WITH cLogNot

	       IF SUBS(TCO->cEvaIndTbl,nNroPer,1) == '2'
		  REPLACE &cCamNtINot WITH cNotInd
		 *Grabaci�n de los notas de los indicadores
	       ENDIF

	       IF lRecNot
		  REPL &cCamNotRec WITH STUFF(&cCamNotRec,nNroPer*4-3,4,cNotDef)
		 *Grabaci�n de la nota de Recuperaci�n.
	       ENDIF

	       NOT->(DBCOMMIT())
	       lHayLog := .T.
	    ELSE
	       cError('NO SE GRABA LOS LOGROS DEL ESTUDIANTE')
	    ENDIF
	    IF lShared
	       NOT->(DBUNLOCK())
	    ENDIF
**********FIN GRABACION DE LAS RECUPERACIONES EN NOTAS

**********GRABACION DE LAS RECUPERACIONES EN NOTAS DEL PROFESOR
	    SELECT NOP
	    IF NOP->(lRegLock(lShared,.F.))

	       REPLACE &cCamIndNoP WITH cLogNot

	       IF cEvaInd == '2'

		  REPLACE &cCamNtINoP WITH cNotInd
		 *Grabaci�n de los notas de los indicadores

	       ENDIF

	       IF lRecNot
		  REPLACE &cCamNoPRec WITH cNotDef
		 *Grabaci�n de la nota de Recuperaci�n
	       ENDIF

	       NOP->(DBCOMMIT())
	       lHayLog := .T.
	    ELSE
	       cError('NO SE GRABA LOS LOGROS DEL ESTUDIANTE')
	    ENDIF
	    IF lShared
	       NOP->(DBUNLOCK())
	    ENDIF
**********FIN GRABACION DE LAS RECUPERACIONES EN NOTAS DEL PROFESOR

**********GRABACION DEL LOG
	    IF lHayLog
	       AADD(aGenLog,{'nCodigoLog',4})
	       AADD(aGenLog,{'cOpcionLog','<MODIFICAR RECUPERACIONES>'})
	       AADD(aGenLog,{'cMaeActLog',cMaeAct})
	       AADD(aGenLog,{'nNroPerLog',nNroPer})

	       AADD(aGenLog,{'cNomUsrLog',cNomUsr})
	       AADD(aGenLog,{'cHorIniLog',cHorIni})
	       AADD(aGenLog,{'cHorFinLog',TIME()})

	       AADD(aGenLog,{'cCodigoPro',SUBS(cPatPro,LEN(cPatPro)-5,6)})
	       AADD(aGenLog,{'cCodigoEst',cCodigoTes})
	       AADD(aGenLog,{'cIndic1Log',cIndic1Tlo})
	       AADD(aGenLog,{'cIndic2Log',cLogNot})

	       cTxtLog := ALLTRIM(MAT->cNombreMat)+' GRUPO: '+cCodigoTgr
	       AADD(aCamLog,{'cObsMatLog',cTxtLog})

/*
	       IF cIndic1Tlo # cLogNoP
		  cTxtLog := 'Modific� Indicadores '
	       ELSE
		  cTxtLog := 'No Modific� Indicadores '
	       ENDIF
*/
	       cTxtLog := ''
	       AADD(aCamLog,{'cObservLog',cTxtLog})

	       aCamLog := aGraProLog(lShared,'LOG',aCamLog,aGenLog)
	       lHayLog := .F.
	    ENDIF
**********FIN GRABACION DEL LOG

**********INCREMENTO DE LAS FILAS
	    lOtrFil := .F.
	    nNroFil++
	    IF nNroFil > nFilFin

*--------------IMPRESION DE LOS ENCABEZADOS
		 nNroFil := nMarco(nFilPal+1,;
				   MAT->cNombreMat+' ',22,'�')

		 @ nNroFil,01 SAY 'NRO'
		 @ nNroFil,05 SAY 'NOMBRE'
		 RESTSCREEN(nNroFil,0,nNroFil,79,cSavCab)
*--------------IMPRESION DE LOS ENCABEZADOS

*--------------IMPRESION DE LOS DATOS DEL ULTIMO ESTUDIANTE
		 nNroFil := nFilIni + 1
		 @ nNroFil,01 SAY cCodigoTes
		 @ nNroFil,08 SAY SUBS(cNombreTes,1,nLenNom) PICT '@A'
		 SELECT NOT
*--------------FIN IMPRESION DE LOS DATOS DEL ULTIMO ESTUDIANTE

*--------------IMPRESION DE LOS LOGROS DEL ULTIMO ESTUDIANTE
		 IF !EMPTY(cLogros)
		    DisplayLog(@nNroFil,nColIni,cEvaInd,;
			       cLogros,cNotInd,nNroInd,lHayNot,cNotDef)
		 ENDIF
		 nNroCol := nColIni
		 nNroFil++
*--------------FIN IMPRESION DE LA NOTA DEFINITIVA

	    ENDIF
**********FIN INCREMENTO DE LAS FILAS

       ENDDO
       CloseAll()
       RETURN NIL
*>>>>FIN GRABACION DE LAS NOTAS DE LOS ESTUDIANTES