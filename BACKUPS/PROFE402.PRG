/* SIMA - CONTABILIDAD PROFESORE

MODULO      : RECUPERACIONES
SUBMODULO...: CONSULTAR                                MODULO No. 402

**************************************************************************
* TITULO..: CONSULTA DE LAS RECUPERACIONES                               *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: SEP 16/2002 VIE A
       Colombia, Bucaramanga        INICIO: 01:00 PM   SEP 16/2002 VIE


OBJETIVOS:

1- Permite consultar las recuperaciones

2- Retorna NIL

SINTAXIS:


*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Profe_402(aP1,aP2,aP3,lGrabar)

*>>>>DESCRIPCION DE PARAMETROS
/*     aP1                                  // Parametros Generales
       aP2                                  // Parametros Generales
       aP3                                  // Parametros Generales
       lGrabar                              // .T. Grabar Actualizacion */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       #INCLUDE "CAMPOS\ARC-SPR.PRG"       // Archivos del Sistema

       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL cSavLin := ''                  // Salvar Linea
       LOCAL lHayErr := .F.                 // .T. Hay Error
     *�Variables generales

       LOCAL       i := 0                   // Contador

       LOCAL nFilSup := 0                   // Columna Superior
       LOCAL nColSup := 0                   // Columna Superior
       LOCAL nInfFil := 0                   // Fila Inferior
       LOCAL nInfCol := 0                   // Columna Inferior

       LOCAL nNroFil := 0                   // Fila de lectura
       LOCAL nNroCol := 1                   // Columna de lectura
       LOCAL nFilIni := 0                   // Fila Inicial
       LOCAL nColIni := 0                   // Columna Inicial

       LOCAL nTotPer := 5                   // Total de Periodos
       LOCAL nNroPer := 0                   // N�mero del periodo

       LOCAL cNalias := ''                  // Alias del Maestro
       LOCAL cMsgTxt := ''                  // Mensaje Temporal

       LOCAL cLogNot := ''                  // Logros de Notas
       LOCAL cDefNot := ''                  // Juicio Valorativo
       LOCAL cRecNot := ''                  // Juicios Valorativo Recuperaci�n

       LOCAL cLogNoP := ''                  // Logros de Notas del Profesor
       LOCAL cDefNoP := ''                  // Juicio Valorativo
       LOCAL cRecNoP := ''                  // Juicio Valorativo Recuperacion

       LOCAL nGrabar := 0                   // Opci�n de Grabaci�n
       LOCAL GetList := {}                  // Variable del Sistema
     *�Variables espec�ficas 

       LOCAL cCodigoTes := ''               // C�digo del estudiante
       LOCAL cNombreTes := ''               // Nombre del estudiante
       LOCAL cCodigoTma := ''               // C�digo de la Materia
       LOCAL cCodigoTgr := ''               // C�digo del grupo
       LOCAL cCodigoTni := ''               // C�digo del nivel

       LOCAL cCamIndNot := ''               // Campo Indicadore en Notas
       LOCAL cCamDefNot := ''               // Campo Nota Definitiva en Notas
       LOCAL cCamRecNot := ''               // Campo Recuperaci�n en Notas

       LOCAL cCamIndNoP := ''               // Campo Indicadores profesor
       LOCAL cCamDefNoP := ''               // Campo Definitiva profesor
       LOCAL cCamRecNoP := ''               // Campo Recuperaci�n profesor

       LOCAL lMatSelTma := .F.              // .T. Materia Selectiva
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
       IF !lUseMae(lShared,PathUno+'\'+cPatSis,cMaeAct,cAnoSis)               .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileMat,'MAT',NIL,lShared)                 .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileNiv+cAnoSis+ExtFile,'NIV',NIL,lShared) .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FConTbl+cMaeACt+ExtFile,'TCO',NIL,lShared) .OR.;
	  !lUseDbf(.T.,cPatSis+'\'+;
		       fSprAno+cAnoUsr+ExtFile,'PRA',NIL,lShared) .OR.;
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
       lGrabar := IF(lGrabar=NIL,.F.,lGrabar)
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
*>>>>FIN SELECCION DE LA MATERIA DEL GRUPO

*>>>>LECTURA DEL NUMERO DEL PERIODO
       nNroPer := nLeeNroPer(nFilInf+1,nColInf,nFilPal,nTotPer)

       lHayErr := .T.
       DO CASE
       CASE nNroPer == 0

       CASE nNroPer > TCO->nPerActTbl
	    cError('NO SE PERMITEN CONSULTAS DE PERIODOS POSTERIORES AL PERIODO ACTUAL')

       OTHERWISE
	    lHayErr :=.F.
       ENDCASE
       IF lHayErr
	  CloseAll()
	  RETURN NIL
       ENDIF
*>>>>FIN LECTURA DEL NUMERO DEL PERIODO

*>>>>CALCULO DEL CAMPO DE INDICADORES DE NOTAS
       IF !lMatSelTma
	  cCamIndNot := cCamJuiNot(lShared,cAnoUsr,cMaeAct,;
				   PathUno+'\'+cPatSis,;
				   nNroPer,cCodigoTgr,cCodigoTma,.F.,;
				   @cCamDefNot,@cCamRecNot)
	  IF EMPTY(cCamIndNot)
	     CloseAll()
	     RETURN NIL
	  ENDIF
       ENDIF
       cCamIndNoP := 'NOP->cIndic'+STR(nNroPer,1,0)+'NoP'
       cCamDefNoP := 'NOP->cDefin'+STR(nNroPer,1)+'NoP'
       cCamRecNoP := 'NOP->cDefRe'+STR(nNroPer,1,0)+'NoP'
*>>>>FIN CALCULO DEL CAMPO DE INDICADORES DE NOTAS

*>>>>MANTENIMIENTO DEL ARCHIVO
       ManDbfNoP(aP1,aP2,aP3,;
		 cPatPro+'\'+CAP->cCodigoCar+ExtFile,nNroPer,cCodigoTma,;
		 SUBS(TCO->cEvaIndTbl,nNroPer,1),;
		 lMatSelTma,.F.,.T.)
       RETURN NIL
*>>>>FIN MANTENIMIENTO DEL ARCHIVO

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

*>>>>DETERMINACION DE COLS Y LONGITUD DE NOMBRE
       nNroCol := 08
       nFilIni := 06
       nColIni := nNroCol
       nNroFil := nFilIni
*>>>>FIN DETERMINACION DE COLS Y LONGITUD DE NOMBRE

*>>>>GRABACION DE LAS NOTAS DE LOS ESTUDIANTES
       nNroFil = nNroFil + 1
       cSavPan := SAVESCREEN(0,0,24,79)
       DO WHILE .T.

**********LECTURA DEL CODIGO DEL ESTUDIANTE
	    cCodigoTes := cLeeCodEst(nNroFil,01,cMaeAct,@cNalias)
	    IF EMPTY(cCodigoTes) .OR. cNalias == NIL
	       EXIT
	    ENDIF
	    cNombreTes := RTRIM(&cNalias->cApelliEst)+' '+;
			  RTRIM(&cNalias->cNombreEst)
	    cNombreTes += ' DEL GRUPO '+&cNalias->cCodigoGru

	    nNroFil := nFilIni + 1
	    @ nNroFil,08 SAY cNombreTes  PICT '@A'
**********FIN LECTURA DEL CODIGO DEL ESTUDIANTE

**********CALCULO DEL CAMPO DE INDICADORES DE NOTAS
	    IF lMatSelTma
	       CloseDbf('PLA')
	       CloseDbf('NOT')
	       cCamIndNot := cCamJuiNot(lShared,cAnoUsr,cMaeAct,;
					PathUno+'\'+cPatSis,;
					nNroPer,&cNalias->cCodigoGru,;
					cCodigoTma,.T.,;
					@cCamDefNot,@cCamRecNot)
	       IF EMPTY(cCamIndNot)
		  LOOP
	       ENDIF
	    ENDIF
**********FIN CALCULO DEL CAMPO DE INDICADORES DE NOTAS

**********LOCALIZACION DEL ESTUDIANTE EN NOTAS
	    cLogNot := ''
	    cDefNot := ''
	    cRecNot := ''
	    IF lLocCodigo('cCodigoEst','NOT',cCodigoTes)
	       cLogNot := &cCamIndNot
	       cDefNot := &cCamDefNot
	       cRecNot := &cCamRecNot
	    ELSE
	       cError('EL ESTUDIANTE NO EXISTE EN NOTAS DEL GRUPO '+;
		      &cNalias->cCodigoGru)
	       LOOP
	    ENDIF
	    IF EMPTY(cLogNot)
	       cError('NO APARECEN INDICADORES EN NOTAS')
	    ENDIF
**********FIN LOCALIZACION DEL ESTUDIANTE EN NOTAS

**********LOCALIZACION DEL ESTUDIANTE EN NOTAS PROFESOR
	    cLogNoP := ''
	    cDefNoP := ''
	    cRecNoP := ''
	    IF lLocCodigo('cCodigoEst','NOP',cCodigoTes)
	       cLogNoP := &cCamIndNoP
	       cDefNoP := &cCamDefNoP
	       cRecNoP := &cCamRecNoP
	    ELSE
	       cError('EL ESTUDIANTE NO EXISTE EN LA MATERIA DEL PROFESOR')
	       LOOP
	    ENDIF
	    IF EMPTY(cLogNoP)
	       cError('NO APARECEN INDICADORES EN EL PROFESOR')
	    ENDIF
**********FIN LOCALIZACION DEL ESTUDIANTE EN NOTAS PROFESOR

**********VALIDACION DE LOS CODIGOS
	    IF cLogNot # cLogNoP
	       IF lMatSelTma
		  cError('NO EXISTEN INDICADORES VERIFIQUE '+;
			 'SI EL ESTUDIANTE FUE TRASLADADO')
	       ELSE
		  cError('NO EXISTEN INDICADORES APLICADOS PARA EL '+;
			 'PERIODO No. '+STR(nNroPer,1))
	       ENDIF
	    ELSE
	       lGrabar := .F.
	    ENDIF
**********FIN VALIDACION DE LOS CODIGOS

**********VALIDACION DE LOS REGISTROS
	    IF NOT->cCodigoEst # NOP->cCodigoEst
	       cError('LOS CODIGOS:'+NOT->cCodigoEst+':'+NOP->cCodigoEst+' '+;
		      'DEL ESTUDIANTE NO SON IGUALES')
	       LOOP
	    ENDIF
**********FIN VALIDACION DE LOS REGISTROS

**********IMPRESION DE LOS ENCABEZADOS DE LAS NOTAS
	    @ nNroFil+2,01 SAY 'INDICADORES EN CONTABILIDAD:'
	    i := 1
	    DO WHILE i <= 10
	       @ nNroFil+3,nNroCol SAY (i<10,'INDI','IND')+LTRIM(STR(i))
	       i++
	       nNroCol := nNroCol + 7
	    ENDDO

	    nNroCol = nNroCol - (i-1)*7
	    nNroFil = nNroFil + 1
	   *Ubica la fila primera de grabacion
**********FIN IMPRESION DE LOS ENCABEZADOS DE LAS NOTAS

**********CONSULTA DE LOS INDICADORES EN CONTABILIDAD
	    DispLogRec(nNroFil+3,nColIni,cLogNot)
**********FIN CONSULTA DE LOS INDICADORES EN CONTABILIDAD

**********IMPRESION DE LOS ENCABEZADOS DEL PROFESOR
	    @ nNroFil+5,01 SAY 'INDICADORES EN EL PROFESOR:'
	    i := 1
	    DO WHILE i <= 10
	       @ nNroFil+6,nNroCol SAY (i<10,'INDI','IND')+LTRIM(STR(i))
	       i++
	       nNroCol := nNroCol + 7
	    ENDDO
**********FIN IMPRESION DE LOS ENCABEZADOS DEL PROFESOR

**********CONSULTA DE LOS INDICADORES DEL PROFESOR
	    DispLogRec(nNroFil+7,nColIni,cLogNop)
**********FIN CONSULTA DE LOS INDICADORES DEL PROFESOR

**********CONSULTA DE JUICIOS VALORATIVOS
	    cSay(nNroFil+10,01,'JUICIOS VALORATIVOS EN SIMACONT:')
	    SET COLOR TO I
	    cSay(nNroFil+10,33,cDefNot)
	    SET COLOR TO

	    cSay(nNroFil+11,01,'JUICIOS VALORATIVOS EN SIMAPROF:')
	    SET COLOR TO I
	    cSay(nNroFil+11,33,cDefNoP)
	    SET COLOR TO
	    cSay(nNroFil+11,42,'Periodo No.',STR(nNroPer,1))

	    IF !EMPTY(cRecNot) .OR. !EMPTY(cRecNoP)
	       cSay(nNroFil+12,01,'JUICIOS RECUPERADOS EN SIMACONT:')
	       SET COLOR TO I
	       cSay(nNroFil+12,33,cRecNot)
	       SET COLOR TO
	    ENDIF

	    IF !EMPTY(cRecNot) .OR. !EMPTY(cRecNoP)
	       cSay(nNroFil+13,01,'JUICIOS RECUPERADOS EN SIMAPROF:')
	       SET COLOR TO I
	       cSay(nNroFil+13,33,cRecNoP)
	       SET COLOR TO
	       cSay(nNroFil+13,42,'Periodo No.',STR(nNroPer,1))
	    ENDIF
**********FIN CONSULTA DE JUICIOS VALORATIVOS

**********GRABACION ACTUALIZACION DE LAS INCONSISTENCIAS
	    IF lGrabar
	       nGrabar := nLeeOpcion('ACTUALIZAR: '+;
			 '1<CONTABILIDAD=>PROFESOR> '+;
			 '2<PROFESOR=>CONTABILIDAD> '+;
			 '3<ABANDONAR>?',3,'0')
	       DO CASE
	       CASE nGrabar == 1
		    SELECT NOP
		    IF NOP->(lRegLock(lShared,.F.))
		       REPL &cCamIndNoP WITH cLogNot
		       REPL &cCamDefNoP WITH SUBS(cDefNot,nNroPer*4-3,4)
		       REPL &cCamRecNoP WITH SUBS(cRecNot,nNroPer*4-3,4)
		       NOP->(DBCOMMIT())
		    ELSE
		       cError('NO SE GRABA LOS LOGROS DEL ESTUDIANTE')
		    ENDIF
		    IF lShared
		       NOP->(DBUNLOCK())
		    ENDIF

	       CASE nGrabar == 2
		    SELECT NOT
		    IF NOT->(lRegLock(lShared,.F.))
		       REPL &cCamIndNot WITH cLogNoP
		       REPL &cCamDefNot WITH STUFF(&cCamDefNot,nNroPer*4-3,4,cDefNoP)
		       REPL &cCamRecNot WITH STUFF(&cCamRecNot,nNroPer*4-3,4,cRecNoP)
		       NOT->(DBCOMMIT())
		    ELSE
		       cError('NO SE GRABA LOS LOGROS DEL ESTUDIANTE')
		    ENDIF
		    IF lShared
		       NOT->(DBUNLOCK())
		    ENDIF
	       OTHERWISE
		    cError('SE ABANDONA EL PROCESO DE ACTUALIZACION')
	       ENDCASE
	    ENDIF
	    cPausa()
	    RESTSCREEN(0,0,24,79,cSavPan)
	    nNroFil := nFilIni+1
	    nNroCol := nColIni
**********FIN GRABACION ACTUALIZACION DE LAS INCONSISTENCIAS

       ENDDO
       CloseAll()
       RETURN NIL
*>>>>FIN GRABACION DE LAS NOTAS DE LOS ESTUDIANTES
