!         ***********************************************************************************
!         * Launch_BLTMS.DLL - Copyright (2005-2006) Sandia Corporation. Under the          *
!         * terms of Contract DE-AC04-94AL85000, there is a non-exclusive license for use of* 
!         * this work by or on behalf of the U.S. Government. Export of this program may    *
!         * require a license from the United States Government.                            * 
!         *                                                                                 *
!         ***********************************************************************************!  

!                                            NOTICE:


! For five (5) years from February 28, 2006, the United States Government is granted for itself and 
! others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide license for this data
! to reproduce, prepare derivative works, and perform publicly and display publicly, by or on behalf
! of the Government. There is provision for the possible extension of the term of this license.
! Subsequent to that period or any extension granted, the United States Government is granted for 
! itself and others action on its behalf a paid-up, nonexclusive, irrevocable worldwide license for 
! this data to reproduce, prepare derivative works, and perform publicly and display publicly, and
! to permit others to do so. The specific term of the license can be identified by inquiry made to
! Sandia Corporation or DOE.

! NEITHER THE UNITED STATES GOVERNMENT, NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR SANDIA
! CORPORATION, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES
! ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY 
! INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT
! INFRINGE PRIVATELY OWNED RIGHTS.

! Any licensee of this software has the obligation and responsibility to abide by the
! applicable export control laws, regulations, and general prohibitions relating to the
! export of technical data. Failure to obtain and export control license or other authority
! from the Government may result in criminal liability under U.S. Laws.

!                                      (End of Notice)
!
!                /************************************************************************
!                 *                                                                      *
!                 *   Patrick Mattie - SANDIA NATIONAL LABORATORIES                      *
!                 *   -------------------------------------------------                  *
!                 *                                                                      *
!                 *                                                                      *
!                 *                          LAUNCH_BLTMS.DLL                            *
!                 *                                                                      *
!                 *                                                                      *
!                 * DLL developed for launching the BLTMS code and send results to       *
!                 * Goldsim                                                              *
!                 *                                                                      *
!                 ************************************************************************
!                 *                                                                      *
!                 *               SUBROUTINE : LAUNCH_BLTMS.f90                          *
!    *************************************************************************************************/
!    *                                                                                               *
!    *                                                                                               *
!    *    Author(s) |Version 1.0 |   Date     | Comments                                             *
!    *-----------------------------------------------------------------------------------------------*
!    * P. Mattie       |  001    | 09/22/2005 | -Create dll to launch BLT_MS from GoldSim.           *
!    * P. Mattie       |  002    | 10/10/2005 | -Added code to pass back concentration results to    *
!    *                 |         |            |  GoldSim from BLT-MS output files.                   *
!    * P. Mattie       |  003    | 10/12/2005 | -Added code to pass back flux results to GoldSim from*
!    *                 |         |            | BLT-MS output files. Removed from ver. 004           *
!    * P. Mattie       |  004    | 10/18/2005 | -Added code to pass back total cumulative mass to    *
!    *                 |         |            | GoldSim from BLTMS output files.                     *
!    * P. Mattie       |  Final  | 12/8/2005  | - Final changes										 *
!    *                 |         |            |                                                      *
!    *************************************************************************************************/
!    *                                                                                               *
!    *    Author(s) |Version 1.1 |   Date     | Comments                                             *
!    *-----------------------------------------------------------------------------------------------*
!    * P. Mattie       |  001    | 01/02/2006 | -Create .bat file to save a copy of concent.dat each *
!    *                 |         |            | realization. Added 1 input passed from GoldSim in(7) *
!    *                 |         |            |  to turn the feature on and off.                     *
!    *                 |         |            |                                                      *
!    * P. Mattie       |  002    | 02/20/2006 | -Create IF/ELSE switch to change to generic name for *
!    *                 |         |            | BLTMS.inp and BLTMS.out for realization during networked*
!    *                 |         |            | simulations to enable the FileCapture.DLL to copy    *
!    *                 |         |            | these files. Added 1 input passed from GoldSim in(8) *
!    *                 |         |            | to indicate on/off for renaming input/output files.  *
!    *                 |         |            |                                                      *
!    * P. Mattie       |  003    | 02/22/2006 | -Added feature that sums the cumr data for selected  *
!    *                 |         | 02/27/2006 | nodes for use in the 1-D transport model in GoldSim. *
!    *                 |         |            | The 1-D   *model needs the cumr data only from the   *
!    *                 |         |            | nodes that have a container. Each container is placed*
!    *                 |         |            | in a release zone (currently there are 3 zones)the   *
!    *                 |         |            | new feature sums the cumr from the containers in each*
!    *                 |         |            | zone using a list of nodes passed by GoldSim.        * 
!    *                 |         |            |                                                      *
!    * P. Mattie       |  004    | 02/27/2006 |(1)Change Cumr(i,j,k) logic for new version of BLTMS_xp.*
!    *                 |         |  completed | allocate arry sizes for the increase in number of    *
!    *                 |         | 02/28/2006 | containers from 100 to 5000 and nodes to 30,000 from *
!    *                 |         |            | 600 (2) cleaned-up source code to remove relic features*
!    *                 |         |            | and comment out debug statements used druing code    *
!    *                 |         |            |	development.(tested on bltms_xpl version only).      *
!    *                 |         |            |                                                      *
!    * P. Mattie       |  005    |02/28/2006  | needed to implement format changes made in output    *
!    *                 |         |            |  files in BLT_xp.exe required by as a result of      *
!    *                 |         |            | increasing the maximum array sizes in BLTMS_xp       *
!    *                 |         |            |                                                      *!
!    * P. Mattie       |  006    |03/02/2006  |1)removed feature added in version 1.1.001that saves  *
!    *                 |         |            | copy of concent.dat. Instead using FileCaptureDLL to *
!    *                 |         |            | save the output file. (2) optimized logic for        *
!    *                 |         |            | passing concentration data to GoldSim.Since grid size*
!    *                 |         |            | enhanced to 30,000, this required updating method.   *
!    *                 |         |            | Added feature that sums the concent data for selected*
!    *                 |         |            | elements for use in the biosphere model in GoldSim.  *
!    *                 |         |            | The Biosphere model needs the concent data from the  *
!    *                 |         |            | elements intersecting a well the new method sums the *
!    *                 |         |            | concent data for given zone using a list of elements *
!    *                 |         |            | passed by GoldSim. Much like cumr subroutine.However *
!    *                 |         |            | max number of concentration traces set to 100 and 10 *
!    *                 |         |completed   | zones. It can be increased by increasing ntrcec_list *
!    *                 |         |03/06/2006  | and increasing the input array in Goldsim.           *
!    *                 |         |            |                                                      *!
!    * P. Mattie       |  007    |03/24/2006  |Had to edit the fixed format read statments for trace *
!    *                 |         |            | files to capture the larger node numbers. Also fixed *
!    *                 |         |            | BAREA data table. Reordered the data so NELCON number*
!    *                 |         |            | is from lowest to highest in the 2-D columns. GoldSim*
!    *                 |         |            | will reject the table if column values are not       *
!    *                 |         |            | ordered from lowest to highest.                      *
!    * P. Mattie       |  008    |03/29/2006  |Increased max zones to 50 for the source regions used *
!    *                 |         |            | to sum the cumr from the containers.                 *
!    *                 |         |            |                                                      *
!    * P.Mattie        |  009    |04/03/2006  |Correct fixed format read for Tracecn.dat file data   *
!    *                 |         |            |                                                      *
!    * P.Mattie        |  010    |05/04/2006  |Increased output array to 750,000 to handle 1000 time *
!    *                 |         |            | steps and more trace nodes.                          *
!    *                 |         |            |                                                      *
!    * P.Mattie        |  011    |07/21/2006  |Increased output array to 6,750,000 to handle 5000    *
!    *                 |         |            | time steps and more trace nodes. In addition added   *
!    *                 |         |            | a time step cleaning routine to account for runs     *
!    *                 |         |            | that have non-montonically increasing time steps     *
!    *                 |         |            | printed in the trace files due to truncation in the  *
!    *                 |         |            | format statements in BLTMS. GoldSim tables must have *
!    *                 |         |            | monotonically increaseing time steps so the routine  *
!    *                 |         |            | was added to correctly label the time steps.         *
!    *************************************************************************************************/
!           ***********************************************************************************
!           * Launch_BLTMS.DLL - Copyright (2005-2006) Sandia Corporation. Under the          *
!           * terms of Contract DE-AC04-94AL85000, there is a non-exclusive license for use of* 
!           * this work by or on behalf of the U.S. Government. Export of this program may    *
!           * require a license from the United States Government.                            * 
!           *                                                                                 *
!           ***********************************************************************************!  
!

!  ========================
!  SUBROUTINE LAUNCH_INPAGN
!  ========================

   SUBROUTINE bltms(method, state, in, out)

       !DEC$ ATTRIBUTES dllexport,c            :: bltms
	   !DEC$ ATTRIBUTES alais : "bltms" : "BLTMS"
	   !DEC$ ATTRIBUTES value                  :: method
       !DEC$ ATTRIBUTES reference              :: state
       !DEC$ ATTRIBUTES reference              :: in
       !DEC$ ATTRIBUTES reference              :: out

!      ==================
!      Declare statements
!      ==================

       REAL(8)                          :: in(*), out(*)
       INTEGER(4)                       :: method, state, file_index, nelcon_list(5011), ntrcec_list(111), num_trace(10)
       INTEGER                          :: A,B,E,F,I,J,K,L,N,M,O, Blocks, nelcon_index(:), ntrcec_index(:), flag, temp, sort(:),barea_index(:)
       CHARACTER                        :: in_file*36, out_file*11 
	   CHARACTER(LEN=80)				:: COMMENT
	   CHARACTER(LEN=10)				:: index1, index2
	   INTEGER(4)						:: ntrcec(:)
	   INTEGER                          :: niso,isot, ntrcn, ntime_steps,ncon,NELCON(:), n_zones, zone(50)
	   REAL								:: time_step(:), x_location(:), z_location(:), time_step_length
	   REAL(8)							:: conc_data(:,:,:)
	   REAL								:: TIMEYR(:),CUMR(:,:,:),TRINSE(:,:,:),TDIF(:,:,:),TDIS(:,:,:),SOSL(:,:,:)
	   REAL								:: RINSER(:,:,:),DIFR(:,:,:),DISR(:,:,:),BAREA(:,:), CUMR_sum(:,:,:),conc_sum(:,:,:)

	   ALLOCATABLE						:: TIMEYR, CUMR, TRINSE,TDIF,TDIS,SOSL,RINSER,DIFR,DISR,BAREA,CUMR_sum,conc_sum, ntrcec
	   ALLOCATABLE						:: time_step,conc_data, x_location, z_location, nelcon_index, NELCON, ntrcec_index, sort,barea_index
	   CHARACTER                        :: infile*15, title*250, radnuc*10(10)


!      ===========================
!      method = 0 : Initialization
!      ===========================

       IF     (method == 0) THEN
              continue

!      ===========================
!      method = 2 : Report version
!      ===========================

       ELSEIF (method == 2) THEN
              out(1) = 1.10  !version number of the code
              

!      =====================================
!      method = 3 : Report arguments
!                   out(1) = nber of inputs
!                   out(2) = nber of outputs 
!		*(nber will have to be increased if you save more that 60 nodes x 10 isotopes)
!      =====================================

       ELSEIF (method==3) THEN
              out(1) = 5129
              out(2) = 6750000

!      ===============================
!      method = 1 : Calculation
!                   Saving the results
!                   in the array 'out'
!      ===============================

       ELSEIF (method==1) THEN



	OPEN (3,FILE='inputfiles.dat',ACCESS='sequential',FORM='formatted',&
          STATUS='REPLACE')

	file_index=in(1)				! in(1) from GoldSim is the realization number
		

    IF (in(7)==1) THEN				! in(7)==1 indicates a networked simulation and gives inp/out files generic names for every realization.
			WRITE (3,1001) 			! Passes the BLT-MS input and output file
1001		FORMAT('BLTMS.inp')		! names for a given realization to a 
     		WRITE (3,1002) 			! .dat file that the DLL will use to launch
1002		FORMAT('BLTMS.out')		! the BLT-MS .exe.
 
			OPEN (1,FILE='copy.bat',ACCESS='sequential',FORM='formatted',&
			 STATUS='REPLACE')
			WRITE (1,1003) file_index
1003        FORMAT('Copy BLTMS',I5.5,'.inp BLTMS.inp')                   
			CLOSE(UNIT=1)
			CALL SYSTEM("copy.bat")

	ELSE
			WRITE (3,1011) file_index			! Passes the BLT-MS input and output file
1011		FORMAT('BLTMS',I5.5,'.inp')			! names for a given realization to a 
     		WRITE (3,1012) file_index			! .dat file that the DLL will use to launch
1012		FORMAT('BLTMS',I5.5,'.out')			! the BLT-MS .exe.
	
	ENDIF

	CLOSE (UNIT=3)

			! ==================================================== !
			! =     LAUNCH BLTMS.EXE FOR SELECTED REALIZATION    = !              
			! ==================================================== !

   CALL SYSTEM("BLTMS.exe <inputfiles.dat")
			  
   OPEN(UNIT=41,FILE='passOutn1.txt', status='replace')        !*_*_*_DEBUG STATEMENT_*_*_*

   write(41,*) 'STATUS: BLTMS.EXE completed sucessfully.'

			! ==================================================== !
			! =     INITIALIZE AND READ GOLDSIM INPUT DATA       = !              
			! ==================================================== !
	write(41,*) '---***--- SELECTED INPUT VALUES FROM GOLDSIM ---***---'
    niso=in(2)						! in(2) from Goldsim is the number of isotopes in the run
	write(41,*) 'NISO:', niso
    ntime_steps=in(3)				! in(3) from GoldSim is the nubmer of time steps in the model run
	write(41,*) 'Number of time steps', in(3)
	ntime_steps=ntime_steps/in(4)	! in(4) from GoldSim is the number of steps between output to the trace files [NSTPTR DATA SET 22]
	write(41,*) 'Number of time steps in trace file output', ntime_steps
	ntrcn=in(5)						! in(5) from Goldsim is the number of node concentration traces
	write(41,*) 'NTRCN:',ntrcn
	ncon=in(6)
	i=8								! in(6) from Goldsim is the number of containers
	write(41,*) 'NCON:',ncon
	DO k=1, ncon+51
	nelcon_list(k)=in(i)			! in(8) to in(5058) is an array of container locations (5000 max containers + 50 max zones + 1 end of list value)
										!*DEBUG*	write(41,*) nelcon_list(k)		!*_*_*_DEBUG STATEMENT_*_*_*
	i=i+1
	ENDDO
	i=5059
	DO k=1, ntrcn+11
	ntrcec_list(k)=in(i)			! in(5019) to in(5129) is an array of element locations (100 max elements + 10 max zones + 1 end of list value)
										!*DEBUG*	write(41,*) ntrcec_list(k)		!*_*_*_DEBUG STATEMENT_*_*_*
	i=i+1
	ENDDO
    i=1

    infile='tracen0.dat'			! name of trace file, initialized here

	write(41,*) '---***----------------- END --------------------***---'
		
	ALLOCATE(TIMEYR(ntime_steps), CUMR(niso,ncon,ntime_steps),TRINSE(niso,ncon,ntime_steps),TDIF(niso,ncon,ntime_steps),TDIS(niso,ncon,ntime_steps),SOSL(niso,ncon,ntime_steps))
	ALLOCATE(RINSER(niso,ncon,ntime_steps),DIFR(niso,ncon,ntime_steps),DISR(niso,ncon,ntime_steps),BAREA(ncon,ntime_steps), CUMR_sum(niso,50,ntime_steps))
	ALLOCATE(time_step(ntime_steps),conc_data(ntrcn,ntime_steps,niso), x_location(ntrcn), z_location(ntrcn),nelcon_index(ncon), NELCON(ncon), barea_index(ncon))
	ALLOCATE(ntrcec(ntrcn), ntrcec_index(ntrcn),conc_sum(niso,10,ntime_steps), sort(ncon))

    O=0
   write(41,*) 'STATUS: Arrays Allocated sucessfully.'

			! ==================================================== !
			! =     READ BLTMS OUTPUT TRACE FILES INTO ARRAYS    = !              
			! ==================================================== !

IF (NTRCN.NE.0) THEN
	DO k=1,niso
		O=O+1
	  IF(O.eq.10) THEN
	  infile='tracec10.dat'
	  ENDIF

	  IF(O.lt.10) THEN
	  WRITE (infile,1111) k
	  ENDIF

1111  FORMAT('tracecn',I1,'.dat')

	  WRITE(41,'(A12)') infile
																										!debug statement
	
	OPEN(UNIT=21,FILE=infile, STATUS='old')

         READ(21,'(A80)') TITLE
										!*DEBUG* WRITE(41,'(A80)') TITLE																													!debug statement
		 READ(21,'(A34,1X,I5)') COMMENT, NTRCN
										!*DEBUG* WRITE(41,'(A80,I3)') COMMENT, NTRCN																							!debug statement
		 DO i=1, NTRCN
		 READ(21,'(A30,2X,I5,A6,1X,1pe9.2,A1,1X,1pe9.2)')COMMENT, ntrcec(i),index1, x_location(i), index2 , z_location(i)
										!*DEBUG* WRITE(41,'(A30,A1,I5,A3,1pe9.2,A1,1pe9.2)')COMMENT,' ', ntrcec(i), index1, x_location(i), index2 , z_location(i)			!debug statement
		 ENDDO
		 READ(21,'(A80)') COMMENT
										!*DEBUG* WRITE(41,'(A80)') COMMENT																									!debug statement
		 IF (NTRCN.GT.20) THEN
			DO i=1, ntime_steps
				!INTEGER Blocks, M, e, f
				Blocks=(NTRCN/20)
				M=MOD(NTRCN,20)
				f=1
				e=20
										!*DEBUG* 
										!*DEBUG*WRITE(41,*) 'Blocks=',Blocks,'Mod=',M
				DO a=1, Blocks
					READ(21,'(1PE8.2, 2X,6(1PE11.3E3,1X),/,(10x,6(1pE11.3E3,1x)))') time_step(i) , (conc_data(j,i,k), j=f, e)
										!*DEBUG*
										!*DEBUG*	do j=f,e 
										!*DEBUG*		WRITE(41,*) conc_data(j,i,k)
										!*DEBUG*	enddo
					f=f+20
					e=e+20
				ENDDO
					e=f-1+M
					!*DEBUG*WRITE(41,*) 'F=',f,'E=',e	
					READ(21,'(1PE8.2, 2X,6(1PE11.3E3,1X),/,(10x,6(1pE11.3E3,1x)))') time_step(i) , (conc_data(j,i,k), j=f, e)
										!*DEBUG* 	do j=f,e 
										!*DEBUG*		WRITE(41,*) conc_data(j,i,k)
										!*DEBUG*	enddo
			ENDDO
			
			ELSE
				DO i=1, ntime_steps
				READ(21,'(1PE8.2, 2X,6(1PE11.3E3,1X),/,(10x,6(1pE11.3E3,1x)))') time_step(i) , (conc_data(j,i,k), j=1, NTRCN)
										!*DEBUG* WRITE(41,'(I3,1X,1PE8.2,2X,6(1PE12.3E3,1X),/,(10x,6(1pE12.3E3,1x)))') i, time_step(i),  (conc_data(j,i,k), j=1, NTRCN)	!debug statement
				ENDDO
		 ENDIF
	  write(41,*) 'STATUS: Read Sucessfully.'
    ENDDO

	CLOSE(21)
ENDIF




			! ==================================================== !
			! = READ BLTMS OUTPUT LEACHRL*.DAT FILES INTO ARRAYS = !              
			! ==================================================== !

    infile='LEACHRL0.dat'				! name of file, initialized here
 
IF (NCON.NE.0) THEN						! if you have no containers NCON=0, Skip reading the file
		
	DO k=1,niso
	  IF(k.eq.10)THEN
		infile='LEACHR10.DAT'
	  ENDIF
	  IF(k.LT.10) THEN
	  WRITE (infile,1112) k
1112  FORMAT('LEACHRL',I1,'.DAT')
	  ENDIF	
										!*DEBUG* WRITE(41,'(A12)') infile  																								!debug statement
	  OPEN(UNIT=31,FILE=infile, STATUS='old')
	  WRITE(41,'(A12)') infile

         READ(31,'(A80)') TITLE
										!*DEBUG* WRITE(41,'(A80)') TITLE
		 READ(31,'(A80)') COMMENT
										!*DEBUG* WRITE(41,'(A80)') COMMENT																									!debug statement
		 DO i=1, ntime_steps
			DO j=1, ncon
			  READ(31,1100) TIMEYR(i),CUMR(k,j,i),TRINSE(k,j,i),TDIF(k,j,i),TDIS(k,j,i),SOSL(k,j,i), RINSER(k,j,i),DIFR(k,j,i),DISR(k,j,i),BAREA(j,i),NELCON(j)
										!*DEBUG* WRITE(41,1100) TIMEYR(i),CUMR(k,j,i),TRINSE(k,j,i),TDIF(k,j,i),TDIS(k,j,i),SOSL(k,j,i), RINSER(k,j,i),DIFR(k,j,i),DISR(k,j,i),BAREA(j,i),NELCON(j)
1100 FORMAT(1X,F10.2,2X,9(1PE10.3,2X),I5)
			ENDDO

		  ENDDO
	  write(41,*) 'STATUS: Read Sucessfully.'
    ENDDO

	CLOSE(31)
ENDIF

! ==================================================================================
!                    Read Concentration Data back in to Goldsim
! ==================================================================================
IF (NTRCN.NE.0) THEN						! if you have no trace files NTRCN=0, Skip 

!!-------------CREATE INDEX FOR NTRCEC_LIST VALUES------------------!!!
j=1
k=1
DO WHILE (ntrcec_list(j).NE.-99)
	IF (ntrcec_list(j).GT.0) THEN
		Do o=1, ntrcn 
			IF (ntrcec_list(j).EQ.NTRCEC(o)) THEN
										!*DEBUG* WRITE(41,*) ntrcec_list(j),'/',o	!Debug Statement										
				ntrcec_index(k)=o
				k=k+1
										!*DEBUG* WRITE(41,*) ntrcec_index
			ENDIF						
		End do
	ENDIF										
j=j+1	
										!*DEBUG* WRITE(41,*) 'j=',j		
ENDDO
WRITE(41,*) 'STATUS: NTREC_LIST Completed.'

										!*DEBUG* 	WRITE(41,*) 'ntrcec_list(j)'	!Debug Statement
										!*DEBUG* 	WRITE(41,*) ntrcec_list			!Debug Statement
										

!!-------------NUMBER OF ELEMENTS PER ZONE FOR CONCENTRATION TRACE AVERAGES------------------!!!
num_trace=1								!initialize array =1
j=1
m=1
DO WHILE (ntrcec_list(j).NE.-99)
		IF(ntrcec_list(j).LT.0) THEN
		j=j+1
		ENDIF
		k=0
		Do WHILE (ntrcec_list(j).GT.0) 
			k=k+1
			J=J+1						
		End do
		num_trace(m)=k					!num_trace counts how many element traces for each zone to calculate the average concentation
																			
										!*DEBUG*	write(41,*) 'num_trace(m)=',m,k
		m=m+1
ENDDO
WRITE(41,*) 'STATUS: NUM_TRACE Completed.'


!!-------------------SUM CONC DATA FOR EACH ZONE ------------------!!
conc_sum=0								!initialize the conc_sum array to zero
j=1										!j is the index for the nelcon array (could have a max of 5000 containers)
k=0
m=1										!k is the index for the 1-D source zone (GoldSim Array begins with -1, set up for a max of 10 zones))
DO WHILE (ntrcec_list(j).NE.-99)		!a value of -99 in the array passed by GoldSim means you are at the end of the list
	
	Do While (ntrcec_list(j).GT.0)
										!*DEBUG* write(41,*) 'xCONTAINER: ',ntrcec_list(j)
										!*DEBUG* write(41,*) 'Well Zone: ', k
		DO h=1, niso
										!*DEBUG* write(41,*) 'ISO: ',h
										!*DEBUG* write(41,*) 'ntrcec_index(m)',ntrcec_index(m)
										!*DEBUG* write(41,*) 'ntime_steps:', ntime_steps
			DO l=1, ntime_steps
				conc_sum(h,k,l)=(conc_sum(h,k,l)+conc_data(ntrcec_index(m),l,h)) ! sum of the concentration values from all elements for a given zone
               !conc_sum(iso,zone,timestep)      conc_data(ntrcec,timestep,iso)
										 !*DEBUG*  write(41,*) 'conc_sum(',h,',',k,',',l,')/Time Step(',l,')'
										 !*DEBUG*  write(41,*) conc_sum(h,k,l),'/', l
			ENDDO
		ENDDO
		j=j+1
		m=m+1
	enddo
 if(ntrcec_list(j).NE.-99) then
	k=k+1
	zone(k)=ABS(ntrcec_list(j))
	j=j+1

 Endif
										!*DEBUG*  Write(41,*) 'J= ',j

 ENDDO
n_zones=k
WRITE(41,*) 'STATUS: CONC_SUM Completed.'

										 !*DEBUG* DO n=1,n_zones
										 !*DEBUG* 	write(41,*) 'Zone: ',zone(n)
										 !*DEBUG* 		DO h=1, niso
										 !*DEBUG* 		write(41,*) 'ISO: ', h
										 !*DEBUG* 			DO l=1, ntime_steps
										 !*DEBUG* 				write(41,*) 'conc_sum(',h,',',n,',',l,')/Time Step(',l,')'
										 !*DEBUG* 				write(41,*) conc_sum(h,n,l), ',',time_step(l)
										 !*DEBUG* 			ENDDO
										 !*DEBUG* 		ENDDO
										 !*DEBUG* 	 ENDDO
ENDIF	
!!-------------------END OF SUM CONCENTRATION ROUTINE------------------------------------!!

			! ==================================================== !
			! =        PASS CONCENTRATION DATA  TO GOLDSIM       = !              
			! ==================================================== !

IF (NTRCN.NE.0) THEN
WRITE(41,*) 'Estimated out(i) index size for Avg_Conc Table:', ((ntime_steps+1)*niso*n_zones)
										!*DEBUG* 	WRITE(41,*)'------------------------------------------------------'		!debug statement
										!*DEBUG* 	WRITE(41,*)'----------------GOLDSIM CONC OUTPUT-------------------'		!debug statement
										!*DEBUG* 	WRITE(41,*)'------------------------------------------------------'		!debug statement

!!------Routine to re-number time steps to ensure we don't have repeating numbers as in trace files from truncation-----!!
!!------ (ROW values must be monotonically increasing for tables in GoldSim)  ------------------
		
		time_step_length=(time_step(ntime_steps)/ntime_steps)
		time_step(1)=time_step_length
		do k=2,ntime_steps
		time_step(k)=(time_step(k-1)+time_step_length)
		enddo
		
		WRITE(41,*) 'Concentration Time Steps Cleaning Completed'

!!-----------------------------------END Time step cleaning routine ---------------------------------------!!


	i=1									!The following define the 3-D table as the output type to pass back concentration data to GoldSim
	out(i)=3							!
	i=i+1								!
	out(i)=ntime_steps+1				!number of rows = number of time steps you have saved + 1 initial 0 year time point
	i=i+1								!
	out(i)=niso 						!number of columns = number of ISO you have saved conc. data 
	i=i+1								!
	out(i)=n_zones						!number of layers = number of zones you have cumulative data
	i=i+1
	
	! send row labels (time step values)
	out(i)=0							!send an initial time-step of 0 years to GoldSim
	
	do k=1,ntime_steps
		out(i+k)=time_step(k)
	enddo
	i=i+ntime_steps

	! send column labels (Iso Number for cumulative concentration trace )	
	do k=1,niso
		out(i+k)=k
	enddo
	i=i+niso
	
	! send layer labels (zone number)
	do k=1,n_zones
		out(i+k)=k
	enddo
	i=i+n_zones

										!*DEBUG* do k=1, i							!debug statement
										!*DEBUG*  	WRITE(41,*) out(k)				!debug statement
										!*DEBUG* enddo								!debug statement


    do k=1, n_zones
		do j=1, niso
			out(i+j)=0
		enddo
		i=i+niso

 		do j=1, ntime_steps
										!*DEBUG* 	WRITE(41,*) 'Isotope Number =', k, ' / Time Step Number=', j,' Index (i)=',i		!debug statment
 			do l=1, niso
				i=i+1
										!*DEBUG* 		WRITE(41,*) i																									!debug statement
				out(i)=conc_sum(l,k,j)/num_trace(k)		!sum divided by the number of elements = average concentration in each zone.
						
										!*DEBUG* 		WRITE(41,'(I4,A1,I3,A1,I2,A1,I1,2X,6(1PE9.2,1X))') i,',', j,',',l,',',k, out(i)	!debug statement
										!*DEBUG*		WRITE(41,*) out(i)
			enddo
		enddo
	enddo
ENDIF

IF (NTRCN.EQ.0) THEN
	
	i=1 								!The following define the 3-D table if there is no concentration data saved
	out(i)=3							!
	i=i+1								!
	out(i)=1							!number of rows 
	i=i+1								!
	out(i)=1							!number of columns 
	i=i+1								!
	out(i)=1							!number of layers = number of isotopes you have saved the data
	i=i+1
	
	out(i)=0							!row label default of 0
	i=i+1
	out(i)=1							!send default value of 1 for column label
	i=i+1
	out(i)=1							!send default value of 1 for layer label
	i=i+1
	out(i)=0							!send default value of 0 for data
	
ENDIF
WRITE(41,*) 'STATUS: AVG CONC Table Loaded.'

!			! ==================================================================== !
!			! =             PASS CUMULATIVE MASS OUTPUT ARRAYS  TO GOLDSIM       = !              
!			! ==================================================================== !
!
										!*DEBUG* 	WRITE(41,*)'-----------------------------------------------------------'
										!*DEBUG* 	WRITE(41,*)'----------------LEACHRL*.DAT OUTPUT------------------------'
										!*DEBUG* 	WRITE(41,*)'-----------------------------------------------------------'

!!-------------CREATE INDEX FOR NELCON_LIST VALUES------------------!!!
j=1
k=1
DO WHILE (nelcon_list(j).NE.-99)
	IF (nelcon_list(j).GT.0) THEN
		Do o=1, ncon 
			IF (nelcon_list(j).EQ.NELCON(o)) THEN
										!*DEBUG* 	WRITE(41,*) nelcon_list(j),'/',o	!Debug Statement
				nelcon_index(k)=o
				k=k+1
			ENDIF						
		End do
	ENDIF
j=j+1			
ENDDO
										!*DEBUG* 	write(41,*) nelcon_index
										!*DEBUG* 	Do n=1, ncon							!Debug Statement
										!*DEBUG* 		WRITE(41,*) 'J / Nelcon_Index(j)'	!Debug Statement
										!*DEBUG* 		WRITE(41,*) n, nelcon_index(n)		!Debug Statement
										!*DEBUG* 	Enddo									!Debug Statement

WRITE(41,*) 'STATUS: NELCON_INDEX Created.'

!!-------------------SUM CUMR DATA FOR EACH 1-D SOURCE REGION ------------------!!
cumr_sum=0
j=1										!j is the index for the nelcon array (could have a max of 5000 containers)
k=0
m=1										!k is the index for the 1-D source zone (GoldSim Array begins with -1, set up for a max of 10 zones))
DO WHILE (nelcon_list(j).NE.-99)		!a value of -99 in the array passed by GoldSim means you are at the end of the list

	Do While (nelcon_list(j).GT.0)
										!*DEBUG* 	write(41,*) 'xCONTAINER: ',nelcon_list(j)
										!*DEBUG* 	write(41,*) '1-D Source Zone: ', k
		DO h=1, niso
										!*DEBUG* 		write(41,*) 'ISO: ',h
			DO l=1, ntime_steps
				cumr_sum(h,k,l)=cumr_sum(h,k,l)+ CUMR(h,nelcon_index(m),l)  ! sum the cumulate realeases from all containers for a given source zone
               !cumr_sum(iso,zone,timestep)      CUMRi(iso,nelcon,timestep)
										!*DEBUG*   write(41,*) 'cumr_sum(',h,',',k,',',l,')'
										!*DEBUG*	write(41,*) cumr_sum(h,k,l)
										!*DEBUG*    write(41,*) 'M=',m
			ENDDO
		ENDDO
		j=j+1
		m=m+1
	enddo
 if(nelcon_list(j).NE.-99) then
	k=k+1
	zone(k)=ABS(nelcon_list(j))
	j=j+1

 Endif
										!*DEBUG*  Write(41,*) 'J= ',j

 ENDDO
n_zones=k

										!*DEBUG*  DO n=1,n_zones
										!*DEBUG* 	write(41,*) 'Zone: ',zone(n)
										!*DEBUG* 	DO h=1, niso
										!*DEBUG* 	write(41,*) 'ISO: ', h
										!*DEBUG* 		DO l=1, ntime_steps
										!*DEBUG* 			write(41,*) 'cumr_sum(',h,',',n,',',l,')/Time Step(',l,')'
										!*DEBUG* 			write(41,*) cumr_sum(h,n,l), ',',timeyr(l)
										!*DEBUG* 		ENDDO
										!*DEBUG* 	ENDDO
										!*DEBUG*  ENDDO
WRITE(41,*) 'STATUS: SUM CUMR DATA Completed.'	
!!-------------------END OF SUM CUMR ROUTINE------------------------------------!!





!--------- Breached Area DATA Table -----------------

!!-------------CREATE INDEX TO RE-ORDER NELCON VALUES (lowest to highest)------------------!!!

j=1
k=1
flag=1
sort=nelcon
Do While (Flag.GT.0)
	flag=1
	Do k=1, ncon-1
		
		IF(sort(k).GT.sort(k+1)) THEN
			temp=sort(k)
			sort(k)=sort(k+1)
			sort(k+1)=temp
			flag=flag+1
		ENDIF
	ENDDO
	flag=flag-1
ENDDO

										!*DEBUG*  write(41,*) 'Breached Area Sort Routine'
										!*DEBUG*  write(41,*) 'NELCON ARRAY'
										!*DEBUG*  write(41,*) NELCON
										!*DEBUG*  write(41,*) 'SORT ARRAY'
										!*DEBUG*  write(41,*) sort


j=1
k=1
flag=ncon
DO j=1, ncon
		Do o=1, ncon 
			IF (sort(j).EQ.NELCON(o)) THEN										
				barea_index(k)=o
				k=k+1
			ENDIF						
		End do			
ENDDO
WRITE(41,*) 'STATUS: BAREA_INDEX Completed.'
										!*DEBUG*  write(41,*) 'BAREA_INDEX ARRAY'
										!*DEBUG*  write(41,*) barea_index

!--------- Send Container Breached Area  to GoldSim-----------------
IF (NCON.NE.0) THEN
WRITE(41,*) 'Estimated out(i) index size for BAREA:', ((ntime_steps)*ncon)
										!*DEBUG* 	write(41,*) 'Breached Area 2-D Table'
	i=i+1
	out(i)=2							! 2-D table
										!*DEBUG*  	write(41,*) out(i) 
	i=i+1								!
	out(i)=ntime_steps !+1				!number of rows = number of time steps you have saved cumulative data
										!*DEBUG*  	write(41,*) out(i)
	i=i+1								!
	out(i)=ncon							!number of columns  = number of  elements you have saved cumulative data
										!*DEBUG*  	write(41,*) out(i)
								!

! send row labels (time step values)
!	i=i+1
!	out(i)=0							!send an initial time-step of 0 years to GoldSim
										!*DEBUG*  write(41,*) out(i)
	
	do k=1,ntime_steps
		out(i+k)=timeyr(k)
										!*DEBUG*  write(41,*) out(i+k)
	enddo
	i=i+ntime_steps

	! send column labels (Element Number where container is located)	
	do k=1,ncon
		out(i+k)=nelcon(barea_index(k))
										!*DEBUG*  write(41,*) out(i+k)
	enddo
	i=i+ncon
	 
	! send breached area data
	!DO j=1, ncon
	!	out(i+j)=0						!Send 0 value for intial time step
	!									!*DEBUG*  write(41,*) out(i+j)
	!ENDDO 
	!i=i+ncon
	DO j=1, ntime_steps				
		DO k=1, ncon
				i=i+1
				out(i)= BAREA(barea_index(k),j)
										!*DEBUG*  write(41,*) out(i)
		ENDDO
	ENDDO
ENDIF

IF (NCON.EQ.0) THEN


		!Send BreachArea 2-D Table
		i=i+1 							!The following define the 2-D table if there is no data saved
		out(i)=2						!
		i=i+1							!
		out(i)=1						!number of rows 
		i=i+1							!
		out(i)=1						!number of columns 
		i=i+1							!

			
		out(i)=0						!row label default of 0
		i=i+1
		out(i)=1						!send default value of 1 for column label
		i=i+1
		out(i)=0						!send default value of 0 for data
ENDIF
WRITE(41,*) 'STATUS: BAREA Table Loaded.'


										!*DEBUG*	write(41,*)'CUMR Table Values'

!			! ==================================================================== !
!			! =             3-D Table with cumr_sum  TO GOLDSIM                  = !              
!			! ==================================================================== !
WRITE(41,*) 'Estimated out(i) index size for Cumr_Sum Table:', ((ntime_steps+1)*niso*n_zones)
	i=i+1
	out(i)=3							!
										!*DEBUG* 	write(41,*) out(i)
	i=i+1								!
	out(i)=ntime_steps !+1				!number of rows = number of time steps you have saved cumulative data
										!*DEBUG*	write(41,*) out(i)
	i=i+1								!
	out(i)=niso							!number of columns  = number of  isotopes you have saved cumulative data
										!*DEBUG*	write(41,*) out(i)
	i=i+1								!
	out(i)=n_zones						!number of layers = number of zones you have created for the 1-D pipe model
										!*DEBUG*	write(41,*) out(i)

	! send row labels (time step values)
	!i=i+1
	!out(i)=0							!send an initial time-step of 0 years to GoldSim
										!*DEBUG*	write(41,*) out(i)
										!*DEBUG*	write(41,*) time_step
	
	do k=1, ntime_steps
		out(i+k)=timeyr(k)
										!*DEBUG*	write(41,*) out(i+k)
	enddo
	i=i+ntime_steps

!	! send column labels (ISO Number)	
	do j=1, niso
		out(i+j)=j
										!*DEBUG*	write(41,*) out(i+j)
	enddo
	i=i+niso
	
	! send layer labels (1-D Zone number)
	do k=1, n_zones
		out(i+k)=zone(k)
										!*DEBUG*	write(41,*) out(i+k)
		enddo
	i=i+n_zones

	!SEND cumr_sum Data

	DO k=1, n_zones
		!DO j=1, niso
		!	out(i+j)=0
		!enddo
		!	i=i+niso
		
		DO j=1, ntime_steps				
			DO l=1, niso
				i=i+1
				 out(i)= cumr_sum(l,k,j)	!3-D Table: the total cumulative mass released
										!*DEBUG*				 write(41,*) out(i)
			enddo			
		enddo
	enddo								
WRITE(41,*) 'STATUS: CUMR_SUM Table Loaded.'
!          
!  =========================================================================
!               DEBUG FILE:                        
!  =========================================================================
!	  
	  WRITE(41,*) 'INDEX NUMBER (i) =', i		!debug statement

      CLOSE(41)									!debug statement
!  ==========================================================================




!      =====================
!      method = 99 : Cleanup
!      =====================

       ELSEIF (method==99) THEN


		ENDIF

		RETURN

   END SUBROUTINE BLTMS