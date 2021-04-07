!
!
!
!
module module_sf_jssflux

    USE module_model_constants    

contains


!
!-------------------------------------------------------------------
!
   subroutine jsscd(xtime,starthour,startday,startmonth,startyear,		&
				     sfcwrap, dom,               &
                     cd, cdread0, cdread1,					                    &
                     ids, ide, jds, jde, kds, kde,                              &
                     ims, ime, jms, jme, kms, kme,                              &
                     its, ite, jts, jte, kts, kte   )
!-------------------------------------------------------------------
      implicit none
!-------------------------------------------------------------------
!
   integer, intent(in)   ::                       ids, ide, jds, jde, kds, kde, &
                                                  ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
!   
   real, intent(in)      ::         xtime
   integer, intent(in)      ::      sfcwrap, starthour, startday, startmonth, startyear
   integer, intent(in)      ::      dom
!
 !
   real, dimension( ims:ime, jms:jme )                                        , &
            intent(inout)::                                                cd, &
																	cdread0, &
																	cdread1

! local vars

   character*256 :: filesh0, filesh1, filelh0, filelh1,fractout
   character(len=2) :: hr0,hr1,mn0,mn1,dy0,dy1,mo0,mo1,domc
   character(len=4) :: yr
   integer, parameter    ::                                       n_max = 1200
   integer               ::                                 i, j, n, nm, nt, m
   integer               ::                                 newread, month0, &
														    month1
   real :: mn, fract
   integer :: hr,day
   real                  ::                         julian_s, julian_e, fc_int, &
                                                            fm, fh, ch, dtdiff
   !real, dimension( 1:n_max ) ::                     fc_qfx, fc_hfx, fc_julian !JP 0 ->1
   real, dimension(ids:ide, jds:jde) :: fc_hfx, fc_hfx0, fc_hfx1, fc_lh, fc_lh0, fc_lh1
   real                       ::                     qfx_interp,hfx_interp ! JP
   real, dimension( its:ite, jts:jte) ::                                   u2d, &
                                                                           v2d, &
                                                                           t2d, &
                                                                          qv2d, &
                                                                           p2d, &
                                                                        dz8w1d, &
                                                                            za, &
                                                                           thx, &
                                                                          thgb
   logical               ::                                        end_of_file
!
!-----open scmflx_bdy and read the julian_s, julian_e, fc_int
!


	hr = int(xtime / 60.0) 
	mn = xtime - hr*60
	day = startday
	month0 = startmonth

	hr = hr + starthour
	if (hr.ge.24) then
		hr = hr - 24
		day = day + 1
		call jss_monthwrap( startmonth, month0, day )
	elseif (hr.ge.48) then
		hr = hr - 48
		day = day + 2
		call jss_monthwrap( startmonth, month0, day )
	endif

	write(yr,'(I4.4)') startyear
	write(mo0,'(I2.2)') month0
	write(hr0,'(I2.2)') hr
	write(dy0,'(I2.2)') day

	write(domc,'(I2.2)') dom

	if (hr.eq.23) then
		write(hr1,'(I2.2)') 0
		write(dy1,'(I2.2)') day + 1
		call jss_monthwrap( startmonth, month1, day+1 )
		write(mo1,'(I2.2)') month1
	else
		write(hr1,'(I2.2)') hr + 1
		write(dy1,'(I2.2)') day
		write(mo1,'(I2.2)') month0
	endif



	newread = 0
	if (mn.eq.0) then
		newread = 1
	endif

	!print *, yr,mo,dy0,hr0
	!print *, yr,mo,dy1,hr1
	!print *, hr,mn,day

	fract = modulo(xtime,60.0) / 60.0

	if (newread.eq.1) then
		filesh0 = 'jsscm_bdy_'//trim(domc)//'_'//trim(yr)//'-'//trim(mo0)//'-'//trim(dy0)//'-'//trim(hr0)//'-00'
		filesh1 = 'jsscm_bdy_'//trim(domc)//'_'//trim(yr)//'-'//trim(mo1)//'-'//trim(dy1)//'-'//trim(hr1)//'-00'

		print *, trim(filesh0)
		print *, trim(filesh1)
		!print *, trim(filelh0)
		!print *, trim(filelh1)
		!call wrf_debug(100, filesh0)
		!call wrf_debug(100, filesh1)
		!call wrf_debug(100, filelh0)
		!call wrf_debug(100, filelh1)

		open(unit=11, file=filesh0, form='formatted', status='old')
		read(11,*) fc_hfx0
		close(11)

		open(unit=11, file=filesh1, form='formatted', status='old')
		read(11,*) fc_hfx1
		close(11)

		!open(unit=11, file=filelh0, form='formatted', status='old')
		!read(11,*) fc_lh0
		!close(11)

		!open(unit=11, file=filelh1, form='formatted', status='old')
		!read(11,*) fc_lh1
		!close(11)

		!print *, 'fract',fract

		fc_hfx0 = fc_hfx0(:,jde:jds:-1)
		fc_hfx1 = fc_hfx1(:,jde:jds:-1)


	endif

	!fc_hfx = fc_hfx0 + (fc_hfx1 - fc_hfx0) * fract
	!fc_lh = fc_lh0 + (fc_lh1 - fc_lh0) *  fract


	!fc_hfx = fc_hfx(:,jde:jds:-1)
	!fc_lh = fc_lh(:,jde:jds:-1)

!
   do j=jts, min(jte,jde-1)
     do i=its, min(ite,ide-1)

!
!-----compute surface moisture flux
!
	   !lh(i,j) = fc_lh(i,j)
	   !qfx(i,j) = fc_lh(i,j)/xlv
!


!-----compute surface heat flux
!
		!cd(i,j)=fc_hfx(i,j) 

		if (newread.eq.1) then
			cd(i,j)=fc_hfx0(i,j)  + (fc_hfx1(i,j) - fc_hfx0(i,j))*fract
			cdread0(i,j) = fc_hfx0(i,j)
			cdread1(i,j) = fc_hfx1(i,j)
		else
			cd(i,j)=cdread0(i,j)  + (cdread1(i,j) - cdread0(i,j))*fract
		endif

     enddo
   enddo


	if (sfcwrap.eq.1) then

	   CALL jss_wrapper_surface(  cd,				 &
								  ids, ide, jds, jde, kds, kde,      &
								  ims, ime, jms, jme, kms, kme,      &
							 	  its, ite, jts, jte, kts, kte      )


	  endif

   
   end subroutine jsscd




!
!-------------------------------------------------------------------
!
   subroutine jsshfx(xtime,starthour,startday,startmonth,startyear,			&
				     sfcwrap, dom,    &
                     hfx, shread0, shread1,					                    &
                     ids, ide, jds, jde, kds, kde,                              &
                     ims, ime, jms, jme, kms, kme,                              &
                     its, ite, jts, jte, kts, kte   )
!-------------------------------------------------------------------
      implicit none
!-------------------------------------------------------------------
!
   integer, intent(in)   ::                       ids, ide, jds, jde, kds, kde, &
                                                  ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
!   
   real, intent(in)      ::         xtime
   integer, intent(in)      ::      sfcwrap, starthour, startday, startmonth, startyear
   integer, intent(in)      ::      dom
!
 !
   real, dimension( ims:ime, jms:jme )                                        , &
            intent(inout)::                                             hfx, &
																	shread0, &
																	shread1

! local vars

   character*256 :: filesh0, filesh1, filelh0, filelh1,fractout
   character(len=2) :: hr0,hr1,mn0,mn1,dy0,dy1,mo0,mo1,domc
   character(len=4) :: yr
   integer, parameter    ::                                       n_max = 1200
   integer               ::                                 i, j, n, nm, nt, m
   integer               ::                                 newread, month0, &
														    month1
   real :: mn, fract
   integer :: hr, day
   real                  ::                         julian_s, julian_e, fc_int, &
                                                            fm, fh, ch, dtdiff
   !real, dimension( 1:n_max ) ::                     fc_qfx, fc_hfx, fc_julian !JP 0 ->1
   real, dimension(ids:ide, jds:jde) :: fc_hfx, fc_hfx0, fc_hfx1, fc_lh, fc_lh0, fc_lh1
   real                       ::                     qfx_interp,hfx_interp ! JP
   real, dimension( its:ite, jts:jte) ::                                   u2d, &
                                                                           v2d, &
                                                                           t2d, &
                                                                          qv2d, &
                                                                           p2d, &
                                                                        dz8w1d, &
                                                                            za, &
                                                                           thx, &
                                                                          thgb
   logical               ::                                        end_of_file
!
!-----open scmflx_bdy and read the julian_s, julian_e, fc_int
!

	hr = int(xtime / 60.0) 
	mn = xtime - hr*60
	day = startday
	month0 = startmonth

	hr = hr + starthour
	if (hr.ge.24) then
		hr = hr - 24
		day = day + 1
		call jss_monthwrap( startmonth, month0, day )
	elseif (hr.ge.48) then
		hr = hr - 48
		day = day + 2
		call jss_monthwrap( startmonth, month0, day )
	endif

	write(yr,'(I4.4)') startyear
	write(mo0,'(I2.2)') month0
	write(hr0,'(I2.2)') hr
	write(dy0,'(I2.2)') day

	write(domc,'(I2.2)') dom

	if (hr.eq.23) then
		write(hr1,'(I2.2)') 0
		write(dy1,'(I2.2)') day + 1
		call jss_monthwrap( startmonth, month1, day+1 )
		write(mo1,'(I2.2)') month1
	else
		write(hr1,'(I2.2)') hr + 1
		write(dy1,'(I2.2)') day
		write(mo1,'(I2.2)') month0
	endif



	newread = 0
	if (mn.eq.0) then
		newread = 1
	endif

	!print *, yr,mo,dy0,hr0
	!print *, yr,mo,dy1,hr1
	!print *, hr,mn,day

	fract = modulo(xtime,60.0) / 60.0

	if (newread.eq.1) then
		filesh0 = 'jsssh_bdy_'//trim(domc)//'_'//trim(yr)//'-'//trim(mo0)//'-'//trim(dy0)//'-'//trim(hr0)//'-00'
		filesh1 = 'jsssh_bdy_'//trim(domc)//'_'//trim(yr)//'-'//trim(mo1)//'-'//trim(dy1)//'-'//trim(hr1)//'-00'

		print *, trim(filesh0)
		print *, trim(filesh1)
		!print *, trim(filelh0)
		!print *, trim(filelh1)
		!call wrf_debug(100, filesh0)
		!call wrf_debug(100, filesh1)
		!call wrf_debug(100, filelh0)
		!call wrf_debug(100, filelh1)

		open(unit=11, file=filesh0, form='formatted', status='old')
		read(11,*) fc_hfx0
		close(11)

		open(unit=11, file=filesh1, form='formatted', status='old')
		read(11,*) fc_hfx1
		close(11)

		!open(unit=11, file=filelh0, form='formatted', status='old')
		!read(11,*) fc_lh0
		!close(11)

		!open(unit=11, file=filelh1, form='formatted', status='old')
		!read(11,*) fc_lh1
		!close(11)

		!print *, 'fract',fract

		fc_hfx0 = fc_hfx0(:,jde:jds:-1)
		fc_hfx1 = fc_hfx1(:,jde:jds:-1)


	endif

	!fc_hfx = fc_hfx0 + (fc_hfx1 - fc_hfx0) * fract
	!fc_lh = fc_lh0 + (fc_lh1 - fc_lh0) *  fract


	!fc_hfx = fc_hfx(:,jde:jds:-1)
	!fc_lh = fc_lh(:,jde:jds:-1)

!
   do j=jts, min(jte,jde-1)
     do i=its, min(ite,ide-1)

!
!-----compute surface moisture flux
!
	   !lh(i,j) = fc_lh(i,j)
	   !qfx(i,j) = fc_lh(i,j)/xlv
!


!-----compute surface heat flux
!
		!cd(i,j)=fc_hfx(i,j) 

		if (newread.eq.1) then
			hfx(i,j)=fc_hfx0(i,j)  + (fc_hfx1(i,j) - fc_hfx0(i,j))*fract
			shread0(i,j) = fc_hfx0(i,j)
			shread1(i,j) = fc_hfx1(i,j)
		else
			hfx(i,j)=shread0(i,j)  + (shread1(i,j) - shread0(i,j))*fract
		endif

     enddo
   enddo


	if (sfcwrap.eq.1) then

	   CALL jss_wrapper_surface(  hfx,				 &
								  ids, ide, jds, jde, kds, kde,      &
								  ims, ime, jms, jme, kms, kme,      &
							 	  its, ite, jts, jte, kts, kte      )


	  endif

   
   end subroutine jsshfx




!
!-------------------------------------------------------------------
!
   subroutine jssqfx(xtime,starthour,startday,startmonth,startyear,			&
				     sfcwrap, dom,           &
                     qfx, lh, lhread0, lhread1,				                    &
                     ids, ide, jds, jde, kds, kde,                              &
                     ims, ime, jms, jme, kms, kme,                              &
                     its, ite, jts, jte, kts, kte   )
!-------------------------------------------------------------------
      implicit none
!-------------------------------------------------------------------
!
   integer, intent(in)   ::                       ids, ide, jds, jde, kds, kde, &
                                                  ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
!   
   real, intent(in)      ::         xtime
   integer, intent(in)      ::      sfcwrap, starthour, startday, startmonth, startyear
   integer, intent(in)      ::      dom
!
 !
   real, dimension( ims:ime, jms:jme )                                        , &
            intent(inout)::                                                qfx,&
																			lh,&
																		lhread0,&
																		lhread1

! local vars

   character*256 :: filesh0, filesh1, filelh0, filelh1,fractout
   character(len=2) :: hr0,hr1,mn0,mn1,dy0,dy1,mo0,mo1,domc
   character(len=4) :: yr
   integer, parameter    ::                                       n_max = 1200
   integer               ::                                 i, j, n, nm, nt, m
   integer               ::                                 newread, month0, &
														    month1
   real :: mn, fract
   integer :: hr,day
   real                  ::                         julian_s, julian_e, fc_int, &
                                                            fm, fh, ch, dtdiff
   !real, dimension( 1:n_max ) ::                     fc_qfx, fc_hfx, fc_julian !JP 0 ->1
   real, dimension(ids:ide, jds:jde) :: fc_hfx, fc_hfx0, fc_hfx1, fc_lh, fc_lh0, fc_lh1
   real                       ::                     qfx_interp,hfx_interp ! JP
   real, dimension( its:ite, jts:jte) ::                                   u2d, &
                                                                           v2d, &
                                                                           t2d, &
                                                                          qv2d, &
                                                                           p2d, &
                                                                        dz8w1d, &
                                                                            za, &
                                                                           thx, &
                                                                          thgb
   logical               ::                                        end_of_file
!
!-----open scmflx_bdy and read the julian_s, julian_e, fc_int
!


	hr = int(xtime / 60.0) 
	mn = xtime - hr*60
	day = startday
	month0 = startmonth

	hr = hr + starthour
	if (hr.ge.24) then
		hr = hr - 24
		day = day + 1
		call jss_monthwrap( startmonth, month0, day )
	elseif (hr.ge.48) then
		hr = hr - 48
		day = day + 2
		call jss_monthwrap( startmonth, month0, day )
	endif

	write(yr,'(I4.4)') startyear
	write(mo0,'(I2.2)') month0
	write(hr0,'(I2.2)') hr
	write(dy0,'(I2.2)') day

	write(domc,'(I2.2)') dom

	if (hr.eq.23) then
		write(hr1,'(I2.2)') 0
		write(dy1,'(I2.2)') day + 1
		call jss_monthwrap( startmonth, month1, day+1 )
		write(mo1,'(I2.2)') month1
	else
		write(hr1,'(I2.2)') hr + 1
		write(dy1,'(I2.2)') day
		write(mo1,'(I2.2)') month0
	endif

	newread = 0
	if (mn.eq.0) then
		newread = 1
	endif

	!print *, yr,mo,dy0,hr0
	!print *, yr,mo,dy1,hr1
	!print *, hr,mn,day

	fract = modulo(xtime,60.0) / 60.0

	if (newread.eq.1) then
		filesh0 = 'jsslh_bdy_'//trim(domc)//'_'//trim(yr)//'-'//trim(mo0)//'-'//trim(dy0)//'-'//trim(hr0)//'-00'
		filesh1 = 'jsslh_bdy_'//trim(domc)//'_'//trim(yr)//'-'//trim(mo1)//'-'//trim(dy1)//'-'//trim(hr1)//'-00'


		print *, trim(filesh0)
		print *, trim(filesh1)
		!print *, trim(filelh0)
		!print *, trim(filelh1)
		!call wrf_debug(100, filesh0)
		!call wrf_debug(100, filesh1)
		!call wrf_debug(100, filelh0)
		!call wrf_debug(100, filelh1)

		open(unit=11, file=filesh0, form='formatted', status='old')
		read(11,*) fc_hfx0
		close(11)

		open(unit=11, file=filesh1, form='formatted', status='old')
		read(11,*) fc_hfx1
		close(11)

		!open(unit=11, file=filelh0, form='formatted', status='old')
		!read(11,*) fc_lh0
		!close(11)

		!open(unit=11, file=filelh1, form='formatted', status='old')
		!read(11,*) fc_lh1
		!close(11)

		!print *, 'fract',fract

		fc_hfx0 = fc_hfx0(:,jde:jds:-1)
		fc_hfx1 = fc_hfx1(:,jde:jds:-1)


	endif

	!fc_hfx = fc_hfx0 + (fc_hfx1 - fc_hfx0) * fract
	!fc_lh = fc_lh0 + (fc_lh1 - fc_lh0) *  fract


	!fc_hfx = fc_hfx(:,jde:jds:-1)
	!fc_lh = fc_lh(:,jde:jds:-1)

!
   do j=jts, min(jte,jde-1)
     do i=its, min(ite,ide-1)

!
!-----compute surface moisture flux
!
	   !lh(i,j) = fc_lh(i,j)
	   !qfx(i,j) = fc_lh(i,j)/xlv
!


!-----compute surface heat flux
!
		!cd(i,j)=fc_hfx(i,j) 

		if (newread.eq.1) then
			lh(i,j)=fc_hfx0(i,j)  + (fc_hfx1(i,j) - fc_hfx0(i,j))*fract
			lhread0(i,j) = fc_hfx0(i,j)
			lhread1(i,j) = fc_hfx1(i,j)
		else
			lh(i,j)=lhread0(i,j)  + (lhread1(i,j) - lhread0(i,j))*fract
		endif

		qfx(i,j) = lh(i,j)/xlv

     enddo
   enddo




	if (sfcwrap.eq.1) then

	   CALL jss_wrapper_surface(  qfx,				 &
								  ids, ide, jds, jde, kds, kde,      &
								  ims, ime, jms, jme, kms, kme,      &
							 	  its, ite, jts, jte, kts, kte      )

	   CALL jss_wrapper_surface(  lh, 				 &
								  ids, ide, jds, jde, kds, kde,      &
								  ims, ime, jms, jme, kms, kme,      &
							 	  its, ite, jts, jte, kts, kte      )

	  endif

   
   end subroutine jssqfx



subroutine jssskintemp(xtime,starthour,startday,startmonth,startyear, &
					 sfcwrap, dom, tsk, albedo, emiss, lwup, &
					 lwread0, lwread1, &
					 alread0, alread1, &
					 emread0, emread1, &
                     ids, ide, jds, jde, kds, kde,                              &
                     ims, ime, jms, jme, kms, kme,                              &
                     its, ite, jts, jte, kts, kte   )
!-------------------------------------------------------------------
      implicit none
!-------------------------------------------------------------------
!
   integer, intent(in)   ::                       ids, ide, jds, jde, kds, kde, &
                                                  ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte        
!   
   real, intent(in)      ::         xtime
   integer, intent(in)      ::      sfcwrap, starthour, startday, startmonth, startyear
   integer, intent(in)      ::      dom
!
 !
   real, dimension( ims:ime, jms:jme )                                        , &
            intent(inout)::                                                tsk,&
																		albedo,&
																		emiss,&
																		lwup,&
																		lwread0,&
																		lwread1,&
																		alread0,&
																		alread1,&
																		emread0,&
																		emread1
                                                                           
! local vars

   character*256 :: filelw0, filelw1, fileal0, fileal1, fileem0, fileem1
   character(len=2) :: hr0,hr1,mn0,mn1,dy0,dy1,mo0,mo1,domc
   character(len=4) :: yr
   integer               ::                                 i, j, n, nm, nt, m
   integer               ::                                 newread, month0, &
															month1
   real :: mn, fract
   integer :: hr, day
   real, dimension(ids:ide, jds:jde) :: fc_lw0, fc_lw1
   real, dimension(ids:ide, jds:jde) :: fc_al0, fc_al1
   real, dimension(ids:ide, jds:jde) :: fc_em0, fc_em1
   logical    ::    end_of_file


	hr = int(xtime / 60.0) 
	mn = xtime - hr*60
	day = startday
	month0 = startmonth

	hr = hr + starthour
	if (hr.ge.24) then
		hr = hr - 24
		day = day + 1
		call jss_monthwrap( startmonth, month0, day )
	elseif (hr.ge.48) then
		hr = hr - 48
		day = day + 2
		call jss_monthwrap( startmonth, month0, day )
	endif

	write(yr,'(I4.4)') startyear
	write(mo0,'(I2.2)') month0
	write(hr0,'(I2.2)') hr
	write(dy0,'(I2.2)') day

	write(domc,'(I2.2)') dom

	if (hr.eq.23) then
		write(hr1,'(I2.2)') 0
		write(dy1,'(I2.2)') day + 1
		call jss_monthwrap( startmonth, month1, day+1 )
		write(mo1,'(I2.2)') month1
	else
		write(hr1,'(I2.2)') hr + 1
		write(dy1,'(I2.2)') day
		write(mo1,'(I2.2)') month0
	endif



	newread = 0
	if (mn.eq.0) then
		newread = 1
	endif

	!print *, yr,mo,dy0,hr0
	!print *, yr,mo,dy1,hr1
	!print *, hr,mn,day

	fract = modulo(xtime,60.0) / 60.0

	if (newread.eq.1) then
		filelw0 = 'jsslw_bdy_'//trim(domc)//'_'//trim(yr)//'-'//trim(mo0)//'-'//trim(dy0)//'-'//trim(hr0)//'-00'
		filelw1 = 'jsslw_bdy_'//trim(domc)//'_'//trim(yr)//'-'//trim(mo1)//'-'//trim(dy1)//'-'//trim(hr1)//'-00'
		fileal0 = 'jssal_bdy_'//trim(domc)//'_'//trim(yr)//'-'//trim(mo0)//'-'//trim(dy0)//'-'//trim(hr0)//'-00'
		fileal1 = 'jssal_bdy_'//trim(domc)//'_'//trim(yr)//'-'//trim(mo1)//'-'//trim(dy1)//'-'//trim(hr1)//'-00'
		fileem0 = 'jssem_bdy_'//trim(domc)//'_'//trim(yr)//'-'//trim(mo0)//'-'//trim(dy0)//'-'//trim(hr0)//'-00'
		fileem1 = 'jssem_bdy_'//trim(domc)//'_'//trim(yr)//'-'//trim(mo1)//'-'//trim(dy1)//'-'//trim(hr1)//'-00'

		print *, trim(filelw0)
		print *, trim(filelw1)
		print *, trim(fileal0)
		print *, trim(fileal1)
		print *, trim(fileem0)
		print *, trim(fileem1)
		!print *, trim(filelh0)
		!print *, trim(filelh1)
		!call wrf_debug(100, filesh0)
		!call wrf_debug(100, filesh1)
		!call wrf_debug(100, filelh0)
		!call wrf_debug(100, filelh1)

		open(unit=11, file=filelw0, form='formatted', status='old')
		read(11,*) fc_lw0
		close(11)

		open(unit=11, file=filelw1, form='formatted', status='old')
		read(11,*) fc_lw1
		close(11)

		open(unit=11, file=fileal0, form='formatted', status='old')
		read(11,*) fc_al0
		close(11)

		open(unit=11, file=fileal1, form='formatted', status='old')
		read(11,*) fc_al1
		close(11)

		open(unit=11, file=fileem0, form='formatted', status='old')
		read(11,*) fc_em0
		close(11)

		open(unit=11, file=fileem1, form='formatted', status='old')
		read(11,*) fc_em1
		close(11)

		fc_lw0 = fc_lw0(:,jde:jds:-1)
		fc_lw1 = fc_lw1(:,jde:jds:-1)
		fc_al0 = fc_al0(:,jde:jds:-1)
		fc_al1 = fc_al1(:,jde:jds:-1)
		fc_em0 = fc_em0(:,jde:jds:-1)
		fc_em1 = fc_em1(:,jde:jds:-1)


	endif

	do j=jts, min(jte,jde-1)
	 do i=its, min(ite,ide-1)

		if (newread.eq.1) then
			lwup(i,j)=fc_lw0(i,j)  + (fc_lw1(i,j) - fc_lw0(i,j))*fract
			lwread0(i,j) = fc_lw0(i,j)
			lwread1(i,j) = fc_lw1(i,j)

			albedo(i,j)=fc_al0(i,j)  + (fc_al1(i,j) - fc_al0(i,j))*fract
			alread0(i,j) = fc_al0(i,j)
			alread1(i,j) = fc_al1(i,j)

			emiss(i,j)=fc_em0(i,j)  + (fc_em1(i,j) - fc_em0(i,j))*fract
			emread0(i,j) = fc_em0(i,j)
			emread1(i,j) = fc_em1(i,j)
		else
			lwup(i,j)=lwread0(i,j)  + (lwread1(i,j) - lwread0(i,j))*fract
			albedo(i,j)=alread0(i,j)  + (alread1(i,j) - alread0(i,j))*fract
			emiss(i,j)=emread0(i,j)  + (emread1(i,j) - emread0(i,j))*fract
		endif

	 enddo
	enddo



	if (sfcwrap.eq.1) then

	   CALL jss_wrapper_surface(  lwup,			 &
								  ids, ide, jds, jde, kds, kde,      &
								  ims, ime, jms, jme, kms, kme,      &
							 	  its, ite, jts, jte, kts, kte      )

	   CALL jss_wrapper_surface(  albedo,				 &
								  ids, ide, jds, jde, kds, kde,      &
								  ims, ime, jms, jme, kms, kme,      &
							 	  its, ite, jts, jte, kts, kte      )

	   CALL jss_wrapper_surface(  emiss,				 &
								  ids, ide, jds, jde, kds, kde,      &
								  ims, ime, jms, jme, kms, kme,      &
							 	  its, ite, jts, jte, kts, kte      )

	endif

   do j=jts, min(jte,jde-1)
     do i=its, min(ite,ide-1)

		tsk(i,j) = (lwup(i,j)/STBOLT/emiss(i,j))**0.25

     enddo
   enddo

   
end subroutine jssskintemp

SUBROUTINE jss_wrapper_surface( flux,                  &
								ids,ide, jds,jde, kds,kde,           &
								ims,ime, jms,jme, kms,kme,           &
								its,ite, jts,jte, kts,kte           )

!-----------------------------------------------------------------------
! Begin declarations.

	use module_dm, only: wrf_dm_sum_real !jss
    USE module_configure, ONLY : grid_config_rec_type, model_config_rec

   IMPLICIT NONE

   INTEGER ,          INTENT(IN   )           :: ids, ide, jds, jde, kds, kde, &
                                                 ims, ime, jms, jme, kms, kme, &
                                                 its, ite, jts, jte, kts, kte

   REAL, DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::     flux

! LOCAL VARS

   INTEGER :: i, j, k
   REAL    :: totflux, meanflux, np2d

! End declarations.
!-----------------------------------------------------------------------

	totflux = 0.0

	DO j = jts, min(jte,jde-1)
	DO i = its, min(ite,ide-1)
		totflux = totflux + flux(i,j)
	ENDDO
	ENDDO

	np2d = float((ide-ids)*(jde-jds))

	totflux = wrf_dm_sum_real( totflux )

	meanflux = totflux / np2d

	DO j = jts, min(jte,jde-1)
	DO i = its, min(ite,ide-1)
		flux(i,j) = meanflux
	ENDDO
	ENDDO


END SUBROUTINE jss_wrapper_surface

SUBROUTINE jss_monthwrap( mo0,mo1,day )

!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   INTEGER ,          INTENT(IN   )           :: mo0,day
   INTEGER ,          INTENT(INOUT)           :: mo1


! LOCAL VARS

   INTEGER :: lastday

! End declarations.
!-----------------------------------------------------------------------


	if (mo0.eq.1) then !jan
		lastday = 31
	elseif (mo0.eq.2) then !feb
		lastday = 28
	elseif (mo0.eq.3) then !mar
		lastday = 31
	elseif (mo0.eq.4) then !apr
		lastday = 30
	elseif (mo0.eq.5) then !may
		lastday = 31
	elseif (mo0.eq.6) then !jun
		lastday = 30
	elseif (mo0.eq.7) then !jul
		lastday = 31
	elseif (mo0.eq.8) then !aug
		lastday = 31
	elseif (mo0.eq.9) then !sep
		lastday = 30
	elseif (mo0.eq.10) then !oct
		lastday = 31
	elseif (mo0.eq.11) then !nov
		lastday = 30
	elseif (mo0.eq.12) then !dec
		lastday = 31
	endif

	mo1 = mo0
	if (day.gt.lastday) then
		mo1 = mo0 + 1
	endif

END SUBROUTINE jss_monthwrap


end module module_sf_jssflux
