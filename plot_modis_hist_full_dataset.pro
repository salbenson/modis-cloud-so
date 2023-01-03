;***********************
;  Make some plots from the full dataset
;***********************

pro plot_modis_hist_full_dataset

;  imac or chpc
path_prefix='/Volumes/' 
;path_prefix='/uufs/chpc.utah.edu/common/home/'

do_traj_plot='no'
;do_traj_plot='yes'

;  increased box
ulat=-40 & llat=-70
llon=50 & rlon=170

;  Choose aqua or terra
;eos='MYD'
;eos='MOD'
eos='MOYD'  ;both

;  Time range to analyze=List of months
julian_day_1m=timegen(start=julday(11,1,2014,0,0,0),final=julday(2,28,2019,23,59,59),units='months',step_size=1)
;julian_day_1m=timegen(start=julday(11,1,2016,0,0,0),final=julday(2,28,2019,23,59,59),units='months',step_size=1)

;  SEASON Nov 2018-Feb 2019
;  SEASON Nov 2017-Feb 2018
;  SEASON Nov 2016-Feb 2017
;  SEASON Nov 2015-Feb 2016
;  SEASON Nov 2014-Feb 2015

;  number and date strings of monthly time array
numtimes_1m=n_elements(julian_day_1m)
caldat,julian_day_1m,mm_1m,dd_1m,yy_1m,hh_1m,mi_1m,ss_1m

;  String of the time range
time_range_str=string(yy_1m[0],format='(I4)')+string(mm_1m[0],format='(I02)')+'-'+$
    string(yy_1m[-1],format='(I4)')+string(mm_1m[-1],format='(I02)')

;  String of search box limits
box_str=string(llat,format='(I03)')+'_'+string(ulat,format='(I03)')+'lat.'+$
  string(llon,format='(I03)')+'_'+string(rlon,format='(I03)')+'lon'

julian_day=!NULL
mean_nd=!NULL 
;  Loop through the months
for i=0,numtimes_1m-1 do begin
    
  ;  Get the monthly histogram file
  if eos eq 'MOYD' then begin
    file_str='*.histograms.'+string(yy_1m[i],format='(I4)')+string(mm_1m[i],format='(I02)')+'*cdf'
  endif else begin
    file_str=eos+'.histograms.'+string(yy_1m[i],format='(I4)')+string(mm_1m[i],format='(I02)')+'*cdf'
  endelse
  files=file_search(file_str,count=num_files)
  if num_files gt 0 then print,num_files,files   
  
  ;  If there is a monthly histogram file
  if num_files gt 0 then begin    
    ;  Loop through the monthly histogram files and put the data in one array
    for j=0,num_files-1 do begin
      print,files[j]
      fid=ncdf_open(files[j])

      vid=ncdf_varid(fid,'julian_day') & ncdf_varget,fid,vid,julian_day1
      vid=ncdf_varid(fid,'count_1km') & ncdf_varget,fid,vid,count_1km1
      vid=ncdf_varid(fid,'count_5km') & ncdf_varget,fid,vid,count_5km1
      vid=ncdf_varid(fid,'center_latitude') & ncdf_varget,fid,vid,center_lat1
      vid=ncdf_varid(fid,'center_longitude') & ncdf_varget,fid,vid,center_lon1
      vid=ncdf_varid(fid,'mean_latitude') & ncdf_varget,fid,vid,mean_lat1
      vid=ncdf_varid(fid,'mean_longitude') & ncdf_varget,fid,vid,mean_lon1
      vid=ncdf_varid(fid,'mean_nd') & ncdf_varget,fid,vid,mean_nd1
      vid=ncdf_varid(fid,'mean_re') & ncdf_varget,fid,vid,mean_re1
      vid=ncdf_varid(fid,'mean_lwp') & ncdf_varget,fid,vid,mean_lwp1
      vid=ncdf_varid(fid,'mean_tau') & ncdf_varget,fid,vid,mean_tau1
      vid=ncdf_varid(fid,'mean_solar_zenith') & ncdf_varget,fid,vid,mean_sza1
      vid=ncdf_varid(fid,'mean_sw_alb') & ncdf_varget,fid,vid,mean_sw_alb1
      vid=ncdf_varid(fid,'mean_sw_nalb') & ncdf_varget,fid,vid,mean_sw_nalb1
      ;vid=ncdf_varid(fid,'mean_cloud_top_temp') & ncdf_varget,fid,vid,all_mean_temp
      ;vid=ncdf_varid(fid,'median_cloud_top_temp') & ncdf_varget,fid,vid,all_median_temp
      ;vid=ncdf_varid(fid,'tenth_percentile_cloud_top_temp') & ncdf_varget,fid,vid,all_tenth_temp
      ;vid=ncdf_varid(fid,'cold_flag') & ncdf_varget,fid,vid,all_cold_flag
      vid=ncdf_varid(fid,'chlor_a') & ncdf_varget,fid,vid,chlor_a1
      vid=ncdf_varid(fid,'chlor_a_lat') & ncdf_varget,fid,vid,chlor_a_lat1
      vid=ncdf_varid(fid,'chlor_a_lon') & ncdf_varget,fid,vid,chlor_a_lon1
      if do_traj_plot eq 'yes' then begin
        vid=ncdf_varid(fid,'julian_day_traj') & ncdf_varget,fid,vid,julian_day_traj1
        vid=ncdf_varid(fid,'lat_traj') & ncdf_varget,fid,vid,lat_traj1
        vid=ncdf_varid(fid,'lon_traj') & ncdf_varget,fid,vid,lon_traj1
        vid=ncdf_varid(fid,'num_histograms') & ncdf_varget,fid,vid,num_histograms1
      endif
      ncdf_close,fid
        
      if mean_nd eq !NULL then begin
        julian_day=julian_day1
        count_1km=count_1km1
        count_5km=count_5km1
        center_lat=center_lat1
        center_lon=center_lon1
        mean_lat=mean_lat1
        mean_lon=mean_lon1
        mean_nd=mean_nd1
        mean_re=mean_re1
        mean_lwp=mean_lwp1
        mean_tau=mean_tau1
        mean_sza=mean_sza1
        mean_sw_alb=mean_sw_alb1
        mean_sw_nalb=mean_sw_nalb1
        chlor_a=chlor_a1
        chlor_a_lat=chlor_a_lat1
        chlor_a_lon=chlor_a_lon1
        if do_traj_plot eq 'yes' then begin
          julian_day_traj=julian_day_traj1
          lat_traj=lat_traj1
          lon_traj=lon_traj1
        endif
      endif else begin
        julian_day=[julian_day,julian_day1]
        count_1km=[count_1km,count_1km1]
        count_5km=[count_5km,count_5km1]
        center_lat=[center_lat,center_lat1]
        center_lon=[center_lon,center_lon1]
        mean_lat=[mean_lat,mean_lat1]
        mean_lon=[mean_lon,mean_lon1]
        mean_nd=[mean_nd,mean_nd1]
        mean_re=[mean_re,mean_re1]
        mean_lwp=[mean_lwp,mean_lwp1]
        mean_tau=[mean_tau,mean_tau1]
        mean_sza=[mean_sza,mean_sza1]
        mean_sw_alb=[mean_sw_alb,mean_sw_alb1]
        mean_sw_nalb=[mean_sw_nalb,mean_sw_nalb1]
        chlor_a=[chlor_a,chlor_a1]
        chlor_a_lat=[chlor_a_lat,chlor_a_lat1]
        chlor_a_lon=[chlor_a_lon,chlor_a_lon1]
        if do_traj_plot eq 'yes' then begin
          julian_day_traj=[julian_day_traj,julian_day_traj1]
          lat_traj=[lat_traj,lat_traj1]
          lon_traj=[lon_traj,lon_traj1]
        endif
      endelse
    endfor  ;end of loop through monthly files
  endif  ;end of found monthly files
endfor  ;end of loop through month time array

;  Calculate lon
mean_colon=mean_lon
r=where(mean_lon gt 180.0,c)
if c gt 0 then mean_lon[r]=mean_lon[r]-360.0

;  Subset the histogram data in the search box above
if 1 eq 1 then begin
  r=where(mean_lat ge llat and mean_lat le ulat and $
    mean_colon ge llon and mean_colon le rlon,c)
  julian_day=julian_day[r]
  mean_lat=mean_lat[r]
  mean_lon=mean_lon[r]
  mean_nd=mean_nd[r]
  amean_colon=mean_colon[r]
  chlor_a=chlor_a[r]
  chlor_a_lat=chlor_a_lat[r]
  chlor_a_lon=chlor_a_lon[r]
  mean_tau=mean_tau[r]
  mean_re=mean_re[r]
  mean_lwp=mean_lwp[r]
  mean_sza=mean_sza[r]
  mean_sw_alb=mean_sw_alb[r]
  mean_sw_nalb=mean_sw_nalb[r]
  if do_traj_plot eq 'yes' then begin
    julian_day_traj=julian_day_traj[r,*]
    lat_traj=lat_traj[r,*]
    lon_traj=lon_traj[r,*]
  endif
  print,min(mean_lat),max(mean_lat)
  print,min(mean_colon),max(mean_colon)
endif else begin
  ulat=max(mean_lat)
  llat=min(mean_lat)
  stop
  llon=0
  rlon=180
endelse

;  Time array strings
caldat,julian_day,smm,sdd,syy,shh,smi,sss
      
;*************************
;   Calculate quartiles of nd
;*************************

nd_median=median(mean_nd)
nd_low_median=median(mean_nd[where(mean_nd le nd_median)])
nd_high_median=median(mean_nd[where(mean_nd gt nd_median)])

hi_idx=where(mean_nd ge nd_high_median,hi_count)
;hi_idx=where(mean_nd ge 100,hi_count)
hi_nd_mean_ovp=mean_nd[hi_idx]
hi_lwp_mean_ovp=mean_lwp[hi_idx]
hi_julian_day_ovp=julian_day[hi_idx]
hi_lat_mean_ovp=mean_lat[hi_idx]
hi_lon_mean_ovp=mean_lon[hi_idx]
hi_chlor_a=chlor_a[hi_idx]
hi_chlor_a_lat=chlor_a_lat[hi_idx]
hi_chlor_a_lon=chlor_a_lon[hi_idx]
hi_tau=mean_tau[hi_idx]
hi_sza=mean_sza[hi_idx]
hi_sw_alb=mean_sw_alb[hi_idx]
hi_sw_nalb=mean_sw_nalb[hi_idx]


lo_idx=where(mean_nd le nd_low_median,lo_count)
lo_nd_mean_ovp=mean_nd[lo_idx]
lo_lwp_mean_ovp=mean_lwp[lo_idx]
lo_julian_day_ovp=julian_day[lo_idx]
lo_lat_mean_ovp=mean_lat[lo_idx]
lo_lon_mean_ovp=mean_lon[lo_idx]
lo_chlor_a=chlor_a[lo_idx]
lo_chlor_a_lat=chlor_a_lat[lo_idx]
lo_chlor_a_lon=chlor_a_lon[lo_idx]
lo_tau=mean_tau[lo_idx]
lo_sza=mean_sza[lo_idx]
lo_sw_alb=mean_sw_alb[lo_idx]
lo_sw_nalb=mean_sw_nalb[lo_idx]

md_idx=where(mean_nd gt nd_low_median and mean_nd lt nd_high_median,md_count)
md_nd_mean_ovp=mean_nd[md_idx]
md_lwp_mean_ovp=mean_lwp[md_idx]
md_julian_day_ovp=julian_day[md_idx]
md_lat_mean_ovp=mean_lat[md_idx]
md_lon_mean_ovp=mean_lon[md_idx]
md_chlor_a=chlor_a[md_idx]
md_chlor_a_lat=chlor_a_lat[md_idx]
md_chlor_a_lon=chlor_a_lon[md_idx]
md_tau=mean_tau[md_idx]
md_sza=mean_sza[md_idx]
md_sw_alb=mean_sw_alb[md_idx]
md_sw_nalb=mean_sw_nalb[md_idx]


;*****************
;  Colortable
;******************
;  Top is the last color to scale 256 colors, 0-255
top_color=252
;  Colortable  0-252  253=white
;mytable=colortable(39,ncolors=254)
mytable=colortable(33,ncolors=253)
;254=hot pink               ;gray=255
mytable=[mytable,transpose([255,255,255]),transpose([238,18,137]),transpose([230,230,230])]
mycbtable=mytable[0:top_color,*]

;*************************
;  Figure2 - 1d histogram of Nd,Lat,lwp,nalb - all years, and then filtered by chlor_a
;*************************
if 1 eq 0 then begin
  print,'figure 2, 1d histogram of Nd,lat,lwp,nalb'
  pxdim=900 & pydim=500
  xl=0.09 & xr=0.95
  yb=0.14 & yt=0.85
  sx=0.11
  sy=0.13
  numplots_x=2
  numplots_y=2
  position_plots,xl,xr,yb,yt,sx,sy,numplots_x,numplots_y,pos
  dx=0.01 & dy=0.01
  fs1=12  ;font_size
  fs2=12
  fs3=10

  ;***  All data
  ; Nd
  pnum=2
  data_var=mean_nd
  start_bin=0.0;min(mean_nd)
  end_bin=300.0;max(mean_nd)
  dbin=(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/buffer,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xtitle='Nd (#/cm!U3!N)',ytitle='Frequency',font_size=fs1,/hist,/xstyle,/ystyle)
  p1=plot([nd_median,nd_median],[0,max(data_freq)],color='red',/overplot)
  p1=plot([nd_low_median,nd_low_median],[0,max(data_freq)],color='blue',/overplot)
  p1=plot([nd_high_median,nd_high_median],[0,max(data_freq)],color='green',/overplot)
  t0=text(pos[pnum,0]+14.0*dx,pos[pnum,3]-5.0*dy,'Nd Lower Quartile',color='blue',font_size=fs3)
  t0=text(pos[pnum,0]+27.0*dx,pos[pnum,3]-5.0*dy,string(nd_low_median,format='(F9.2)'),color='blue',font_size=fs3)
  t0=text(pos[pnum,0]+14.0*dx,pos[pnum,3]-8.0*dy,'Nd Median',color='red',font_size=fs3)
  t0=text(pos[pnum,0]+27.0*dx,pos[pnum,3]-8.0*dy,string(nd_median,format='(F9.2)'),color='red',font_size=fs3)
  t0=text(pos[pnum,0]+14.0*dx,pos[pnum,3]-11.0*dy,'Nd Upper Quartile',color='green',font_size=fs3)
  t0=text(pos[pnum,0]+27.0*dx,pos[pnum,3]-11.0*dy,string(nd_high_median,format='(F9.2)'),color='green',font_size=fs3)
  t0=text(pos[pnum,0]-3*dx,pos[pnum,3]+3*dy,'(a)',font_size=14)
  print,'plot ND'
  ; Lat
  pnum=3
  data_var=mean_lat
  start_bin=llat;min(mean_lat)
  end_bin=ulat;max(mean_lat)
  dbin=0.5;(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],/xstyle,/ystyle,$
    xtitle='Latitude',ytitle='Frequency',font_size=fs1,/hist,/nodata)
  data_var=hi_lat_mean_ovp
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='green',/hist)
  data_var=lo_lat_mean_ovp
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='blue',/hist)
  ;data_var=md_lat_mean_ovp
  ;hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  ;p1=plot(bins,data_freq,/overplot,color='red',/hist)
  t0=text(pos[pnum,0]+14.0*dx,pos[pnum,3]-5.0*dy,'Nd < '+string(nd_low_median,format='(F6.2)'),color='blue',font_size=fs3)
  ;t0=text(pos[pnum,0]+13.5*dx,pos[pnum,3]-8.0*dy,string(nd_low_median,format='(F6.2)')+' < Nd < '+string(nd_high_median,format='(F6.2)'),color='red',font_size=fs3)
  t0=text(pos[pnum,0]+14.0*dx,pos[pnum,3]-8.0*dy,'Nd > '+string(nd_high_median,format='(F6.2)'),color='green',font_size=fs3)
  t0=text(pos[pnum,0]-3*dx,pos[pnum,3]+3*dy,'(b)',font_size=14)
  print,'plot lat'
  ; LWP
  pnum=0
  data_var=mean_lwp
  start_bin=0.0;min(mean_lwp)
  end_bin=250.0;max(mean_lwp)
  dbin=5.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xrange=[start_bin,end_bin],$
    xtitle='LWP (g/m!U2!N)',ytitle='Frequency',font_size=fs1,/hist,/ystyle,/nodata)
  data_var=hi_lwp_mean_ovp
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='green',/hist)
  data_var=lo_lwp_mean_ovp
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='blue',/hist)
  ;data_var=md_lwp_mean_ovp
  ;hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  ;p1=plot(bins,data_freq,/overplot,color='red',/hist)
  t0=text(pos[pnum,0]-3*dx,pos[pnum,3]+3*dy,'(c)',font_size=14)
  ;  SW albedo
  pnum=1
  data_var=mean_sw_nalb
  start_bin=0.0;min(mean_sw_nalb)
  end_bin=1.0;max(mean_sw_nalb)
  dbin=0.01
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xrange=[start_bin,end_bin],/nodata,$
    xtitle='Normalized SW Albedo',ytitle='Frequency',font_size=fs1,/hist,/ystyle)
  data_var=hi_sw_nalb
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='green',/hist)
  data_var=lo_sw_nalb
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='blue',/hist)
  ;data_var=md_sw_nalb
  ;hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  ;p1=plot(bins,data_freq,/overplot,color='red',/hist)
  t0=text(pos[pnum,0]-3*dx,pos[pnum,3]+3*dy,'(d)',font_size=14)

  ;  Title of time range
  yy_1m_str=string(yy_1m,format='(I4)')
  tstr='NDJF '+yy_1m_str[0]+'-'+yy_1m_str[-1]
  pnum=2
  t2=text((pos[pnum,2]+pos[pnum,0])/2.0,pos[pnum,3]+1.0*dy,tstr,font_size=fs2,$
    alignment=0.5)
  pnum=3
  tstr=string(llat,format='(I03)')+string(176B)+' to '+string(ulat,format='(I03)')+string(176B)+' Lat,'+$
    string(llon,format='(I3)')+string(176B)+' to '+string(rlon,format='(I3)')+string(176B)+' Lon'
  t2=text((pos[pnum,2]+pos[pnum,0])/2.0,pos[pnum,3]+1.0*dy,tstr,font_size=fs2,$
    alignment=0.5)
  
  ;p0.save,'nd_quartiles_lat_dist.'+time_range_str+'.'+box_str+'.png',height=pydim
  p0.save,'figure2.nd_lat.'+time_range_str+'.'+box_str+'.png',height=pydim
  p0.save,'figure2.nd_lat.'+time_range_str+'.'+box_str+'.eps'
stop
  ;*** Chlor filter
  ; Nd
  pnum=0
  data_var=mean_nd
  start_bin=0.0;min(mean_nd)
  end_bin=300.0;max(mean_nd)
  dbin=(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/buffer,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xtitle='Nd (#/cm3)',ytitle='Frequency',font_size=fs1,/hist,/xstyle,/ystyle)
  p1=plot([nd_median,nd_median],[0,max(data_freq)],color='red',/overplot)
  p1=plot([nd_low_median,nd_low_median],[0,max(data_freq)],color='blue',/overplot)
  p1=plot([nd_high_median,nd_high_median],[0,max(data_freq)],color='green',/overplot)

  r=where(chlor_a ne -32767)
  data_var=mean_nd[r]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p2=plot(bins,data_freq,/overplot,/hist,color='gray')
  t0=text(pos[pnum,0]+14.0*dx,pos[pnum,3]-15.0*dy,'chlor_a value',color='gray',font_size=fs3)

  t0=text(pos[pnum,0]+14.0*dx,pos[pnum,3]-5.0*dy,'Nd Lower Quartile',color='blue',font_size=fs3)
  t0=text(pos[pnum,0]+27.0*dx,pos[pnum,3]-5.0*dy,string(nd_low_median,format='(F9.2)'),color='blue',font_size=fs3)
  t0=text(pos[pnum,0]+14.0*dx,pos[pnum,3]-8.0*dy,'Nd Median',color='red',font_size=fs3)
  t0=text(pos[pnum,0]+27.0*dx,pos[pnum,3]-8.0*dy,string(nd_median,format='(F9.2)'),color='red',font_size=fs3)
  t0=text(pos[pnum,0]+14.0*dx,pos[pnum,3]-11.0*dy,'Nd Upper Quartile',color='green',font_size=fs3)
  t0=text(pos[pnum,0]+27.0*dx,pos[pnum,3]-11.0*dy,string(nd_high_median,format='(F9.2)'),color='green',font_size=fs3)
  t0=text(pos[pnum,0]-3*dx,pos[pnum,3]+5*dy,'a.',font_size=14)

  ; Lat
  pnum=1
  data_var=mean_lat
  start_bin=llat;min(mean_lat)
  end_bin=ulat;max(mean_lat)
  dbin=0.5;(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],/xstyle,/ystyle,$
    xtitle='Latitude',ytitle='Frequency',font_size=fs1,/hist);,/nodata)
  data_var=hi_lat_mean_ovp
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='green',/hist)
  data_var=lo_lat_mean_ovp
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='blue',/hist)

  r=where(chlor_a ne -32767)
  data_var=mean_lat[r]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p2=plot(bins,data_freq,/overplot,/hist,color='gray')
  r=where(hi_chlor_a ne -32767)
  data_var=hi_lat_mean_ovp[r]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p2=plot(bins,data_freq,/overplot,/hist,color='medium sea green')
  r=where(lo_chlor_a ne -32767)
  data_var=lo_lat_mean_ovp[r]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p2=plot(bins,data_freq,/overplot,/hist,color='dodger blue')

  t0=text(pos[pnum,0]+14.0*dx,pos[pnum,3]-5.0*dy,'chlor_a value',color='gray',font_size=fs3)
  t0=text(pos[pnum,0]+14.0*dx,pos[pnum,3]-8.0*dy,'chlor_a value',color='medium sea green',font_size=fs3)
  t0=text(pos[pnum,0]+14.0*dx,pos[pnum,3]-11.0*dy,'chlor_a value',color='dodger blue',font_size=fs3)
  t0=text(pos[pnum,0]-3*dx,pos[pnum,3]+5*dy,'b.',font_size=14)

  ;  Title of time range
  tstr='NDJF '+yy_1m_str[0]+'-'+yy_1m_str[-1]
  pnum=0
  t2=text((pos[pnum,2]+pos[pnum,0])/2.0,pos[pnum,3]+1.0*dy,tstr,font_size=fs2,$
    alignment=0.5)
  pnum=1
  t2=text((pos[pnum,2]+pos[pnum,0])/2.0,pos[pnum,3]+1.0*dy,tstr,font_size=fs2,$
    alignment=0.5)

  p0.save,'nd_quartiles_lat_dist.chlora.'+time_range_str+'.'+box_str+'.png',height=pydim
  stop
endif

;**********************************************
; 2d histogram of lwp, nd
;**********************************************
if 1 eq 0 then begin
  pxdim=900 & pydim=900
  xl=0.09 & xr=0.95
  yb=0.05 & yt=0.95
  sx=0.07
  sy=0.09
  numplots_x=4
  numplots_y=4
  position_plots,xl,xr,yb,yt,sx,sy,numplots_x,numplots_y,pos
  ;  Colorbar position
  cbpos=pos
  cbpos[*,0]=pos[*,2]+0.02
  cbpos[*,2]=cbpos[*,0]+0.01
  dx=0.01 & dy=0.01
  fs1=10  ;font_size
  pnum=0
  p0=plot([0,1],[0,1],position=pos[pnum,*],/buffer,dimensions=[pxdim,pydim],axis_style=4,/nodata)
  d=p0.convertcoord([pos[pnum,0],pos[pnum,2]],[pos[pnum,1],pos[pnum,3]],/normal,/to_device)
  isx=(d[0,1,0]-d[0,0,0])
  isy=(d[1,1,0]-d[1,0,0])
  c_value_array=[-2.5,-2.7,-3.0,-3.5,-4]
  c_color_array=['black','red','blue','green','purple']
  
  pnum=0
  x_data=mean_lwp
  x_start_bin=0
  x_end_bin=250
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_data=mean_nd
  y_start_bin=0
  y_end_bin=300
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  r=where(data_freq gt 0,c)
  if c gt 0 then data_freq[r]=alog10(data_freq[r])
  r=where(data_freq eq 0,c)
  if c gt 0 then data_freq[r]=!Values.f_nan
  c0=contour(data_freq,x_bins,y_bins,/current,position=pos[pnum,*],$
    xtitle='LWP',ytitle='Nd',title='All Cases',$
    c_value=c_value_array,c_color=c_color_array,c_label_show=1,$
    /xstyle,/ystyle,xtickdir=1,ytickdir=1,axis_style=2,font_size=fs1)
  
  pnum=1
  x_data=lo_lwp_mean_ovp
  x_start_bin=0.
  x_end_bin=250.
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_data=lo_nd_mean_ovp
  y_start_bin=0.
  y_end_bin=50.
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  r=where(data_freq gt 0,c)
  if c gt 0 then data_freq[r]=alog10(data_freq[r])
  r=where(data_freq eq 0,c)
  if c gt 0 then data_freq[r]=!Values.f_nan
  c0=contour(data_freq,x_bins,y_bins,/current,position=pos[pnum,*],$
    xtitle='LWP',ytitle='Nd',title='Low Nd Cases',$
    c_value=c_value_array,c_color=c_color_array,c_label_show=1,$
    /xstyle,/ystyle,xtickdir=1,ytickdir=1,axis_style=2,font_size=fs1)
    
  pnum=2
  x_data=hi_lwp_mean_ovp
  x_start_bin=0.
  x_end_bin=250.
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_data=hi_nd_mean_ovp
  y_start_bin=100.
  y_end_bin=300.
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  r=where(data_freq gt 0,c)
  if c gt 0 then data_freq[r]=alog10(data_freq[r])
  r=where(data_freq eq 0,c)
  if c gt 0 then data_freq[r]=!Values.f_nan
  c0=contour(data_freq,x_bins,y_bins,/current,position=pos[pnum,*],$
    xtitle='LWP',ytitle='Nd',title='High Nd Cases',$
    c_value=c_value_array,c_color=c_color_array,c_label_show=1,$
    /xstyle,/ystyle,xtickdir=1,ytickdir=1,axis_style=2,font_size=fs1) 
  
  ;  north,south of -60S
  r_n60=where(mean_lat gt -60 and mean_lat le ulat,c_n60) ;lo north of -60S
  pnum=4  
  x_data=mean_lwp[r_n60]
  x_start_bin=0.
  x_end_bin=250.
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_data=mean_nd[r_n60]
  y_start_bin=0.
  y_end_bin=300.
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  r=where(data_freq gt 0,c)
  if c gt 0 then data_freq[r]=alog10(data_freq[r])
  r=where(data_freq eq 0,c)
  if c gt 0 then data_freq[r]=!Values.f_nan
  c0=contour(data_freq,x_bins,y_bins,/current,position=pos[pnum,*],$
    xtitle='LWP',ytitle='Nd',title='North of 60 Cases',$
    c_value=c_value_array,c_color=c_color_array,c_label_show=1,$
    /xstyle,/ystyle,xtickdir=1,ytickdir=1,axis_style=2,font_size=fs1) 
  
  pnum=5
  r_s60=where(mean_lat ge llat and mean_lat le -60,c_s60) ;hi north of -60S
  x_data=mean_lwp[r_s60]
  x_start_bin=0
  x_end_bin=250
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_data=mean_nd[r_s60]
  y_start_bin=0
  y_end_bin=300
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  r=where(data_freq gt 0,c)
  if c gt 0 then data_freq[r]=alog10(data_freq[r])
  r=where(data_freq eq 0,c)
  if c gt 0 then data_freq[r]=!Values.f_nan
  c0=contour(data_freq,x_bins,y_bins,/current,position=pos[pnum,*],$
    xtitle='LWP',ytitle='Nd',title='South of 60 Cases',$
    c_value=c_value_array,c_color=c_color_array,c_label_show=1,$
    /xstyle,/ystyle,xtickdir=1,ytickdir=1,axis_style=2,font_size=fs1)
  
  ;  SOUTH NOV
  pnum=8
  r_s60=where(mean_lat ge llat and mean_lat le -60 and smm eq 11,c_s60) ;hi north of -60S
  x_data=mean_lwp[r_s60]
  x_start_bin=0
  x_end_bin=250
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_data=mean_nd[r_s60]
  y_start_bin=0
  y_end_bin=300
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  r=where(data_freq gt 0,c)
  if c gt 0 then data_freq[r]=alog10(data_freq[r])
  r=where(data_freq eq 0,c)
  if c gt 0 then data_freq[r]=!Values.f_nan
  c0=contour(data_freq,x_bins,y_bins,/current,position=pos[pnum,*],$
    xtitle='LWP',ytitle='Nd',title='NOV South of 60 Cases',$
    c_value=c_value_array,c_color=c_color_array,c_label_show=1,$
    /xstyle,/ystyle,xtickdir=1,ytickdir=1,axis_style=2,font_size=fs1)
  t1=text(pos[pnum,0]+0.0*dx,pos[pnum,3]+2.0*dy,'LWP='+string(mean(mean_lwp[r_s60]),format='(F4.1)'),font_size=fs1)
  
  ;  SOUTH DEC
  pnum=9
  r_s60=where(mean_lat ge llat and mean_lat le -60 and smm eq 12,c_s60) ;hi north of -60S
  x_data=mean_lwp[r_s60]
  x_start_bin=0
  x_end_bin=250
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_data=mean_nd[r_s60]
  y_start_bin=0
  y_end_bin=300
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  r=where(data_freq gt 0,c)
  if c gt 0 then data_freq[r]=alog10(data_freq[r])
  r=where(data_freq eq 0,c)
  if c gt 0 then data_freq[r]=!Values.f_nan
  c0=contour(data_freq,x_bins,y_bins,/current,position=pos[pnum,*],$
    xtitle='LWP',ytitle='Nd',title='DEC South of 60 Cases',$
    c_value=c_value_array,c_color=c_color_array,c_label_show=1,$
    /xstyle,/ystyle,xtickdir=1,ytickdir=1,axis_style=2,font_size=fs1)
  t1=text(pos[pnum,0]+0.0*dx,pos[pnum,3]+2.0*dy,'LWP='+string(mean(mean_lwp[r_s60]),format='(F4.1)'),font_size=fs1)
 
  ;  SOUTH JAN
  pnum=10
  r_s60=where(mean_lat ge llat and mean_lat le -60 and smm eq 01,c_s60) ;hi north of -60S
  x_data=mean_lwp[r_s60]
  x_start_bin=0
  x_end_bin=250
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_data=mean_nd[r_s60]
  y_start_bin=0
  y_end_bin=300
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  r=where(data_freq gt 0,c)
  if c gt 0 then data_freq[r]=alog10(data_freq[r])
  r=where(data_freq eq 0,c)
  if c gt 0 then data_freq[r]=!Values.f_nan
  c0=contour(data_freq,x_bins,y_bins,/current,position=pos[pnum,*],$
    xtitle='LWP',ytitle='Nd',title='JAN South of 60 Cases',$
    c_value=c_value_array,c_color=c_color_array,c_label_show=1,$
    /xstyle,/ystyle,xtickdir=1,ytickdir=1,axis_style=2,font_size=fs1)
  t1=text(pos[pnum,0]+0.0*dx,pos[pnum,3]+2.0*dy,'LWP='+string(mean(mean_lwp[r_s60]),format='(F4.1)'),font_size=fs1)

  ;  SOUTH FEB
  pnum=11
  r_s60=where(mean_lat ge llat and mean_lat le -60 and smm eq 02,c_s60) ;hi north of -60S
  x_data=mean_lwp[r_s60]
  x_start_bin=0
  x_end_bin=250
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_data=mean_nd[r_s60]
  y_start_bin=0
  y_end_bin=300
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  r=where(data_freq gt 0,c)
  if c gt 0 then data_freq[r]=alog10(data_freq[r])
  r=where(data_freq eq 0,c)
  if c gt 0 then data_freq[r]=!Values.f_nan
  c0=contour(data_freq,x_bins,y_bins,/current,position=pos[pnum,*],$
    xtitle='LWP',ytitle='Nd',title='FEB South of 60 Cases',$
    c_value=c_value_array,c_color=c_color_array,c_label_show=1,$
    /xstyle,/ystyle,xtickdir=1,ytickdir=1,axis_style=2,font_size=fs1)
  t1=text(pos[pnum,0]+0.0*dx,pos[pnum,3]+2.0*dy,'LWP='+string(mean(mean_lwp[r_s60]),format='(F4.1)'),font_size=fs1)

 
 ;  north -60S
  r_n60=where(mean_lat gt -60 and mean_lat le ulat and smm eq 11,c_n60) ;lo north of -60S
  pnum=12
  x_data=mean_lwp[r_n60]
  x_start_bin=0
  x_end_bin=250
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_data=mean_nd[r_n60]
  y_start_bin=0
  y_end_bin=300
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  r=where(data_freq gt 0,c)
  if c gt 0 then data_freq[r]=alog10(data_freq[r])
  r=where(data_freq eq 0,c)
  if c gt 0 then data_freq[r]=!Values.f_nan
  c0=contour(data_freq,x_bins,y_bins,/current,position=pos[pnum,*],$
    xtitle='LWP',ytitle='Nd',title='NOV North of 60 Cases',$
    c_value=c_value_array,c_color=c_color_array,c_label_show=1,$
    /xstyle,/ystyle,xtickdir=1,ytickdir=1,axis_style=2,font_size=fs1)
  t1=text(pos[pnum,0]+0.0*dx,pos[pnum,3]+2.0*dy,'LWP='+string(mean(mean_lwp[r_n60]),format='(F4.1)'),font_size=fs1)

  ;  north -60S
  r_n60=where(mean_lat gt -60 and mean_lat le ulat and smm eq 12,c_n60) ;lo north of -60S
  pnum=13
  x_data=mean_lwp[r_n60]
  x_start_bin=0
  x_end_bin=250
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_data=mean_nd[r_n60]
  y_start_bin=0
  y_end_bin=300
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  r=where(data_freq gt 0,c)
  if c gt 0 then data_freq[r]=alog10(data_freq[r])
  r=where(data_freq eq 0,c)
  if c gt 0 then data_freq[r]=!Values.f_nan
  c0=contour(data_freq,x_bins,y_bins,/current,position=pos[pnum,*],$
    xtitle='LWP',ytitle='Nd',title='DEC North of 60 Cases',$
    c_value=c_value_array,c_color=c_color_array,c_label_show=1,$
    /xstyle,/ystyle,xtickdir=1,ytickdir=1,axis_style=2,font_size=fs1) 
  t1=text(pos[pnum,0]+0.0*dx,pos[pnum,3]+2.0*dy,'LWP='+string(mean(mean_lwp[r_n60]),format='(F4.1)'),font_size=fs1)
  
  ;  north -60S
  r_n60=where(mean_lat gt -60 and mean_lat le ulat and smm eq 01,c_n60) ;lo north of -60S
  pnum=14
  x_data=mean_lwp[r_n60]
  x_start_bin=0
  x_end_bin=250
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_data=mean_nd[r_n60]
  y_start_bin=0
  y_end_bin=300
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  r=where(data_freq gt 0,c)
  if c gt 0 then data_freq[r]=alog10(data_freq[r])
  r=where(data_freq eq 0,c)
  if c gt 0 then data_freq[r]=!Values.f_nan
  c0=contour(data_freq,x_bins,y_bins,/current,position=pos[pnum,*],$
    xtitle='LWP',ytitle='Nd',title='JAN North of 60 Cases',$
    c_value=c_value_array,c_color=c_color_array,c_label_show=1,$
    /xstyle,/ystyle,xtickdir=1,ytickdir=1,axis_style=2,font_size=fs1)   
  t1=text(pos[pnum,0]+0.0*dx,pos[pnum,3]+2.0*dy,'LWP='+string(mean(mean_lwp[r_n60]),format='(F4.1)'),font_size=fs1)
  
  ;  north -60S
  r_n60=where(mean_lat gt -60 and mean_lat le ulat and smm eq 02,c_n60) ;lo north of -60S
  pnum=15
  x_data=mean_lwp[r_n60]
  x_start_bin=0
  x_end_bin=250
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_data=mean_nd[r_n60]
  y_start_bin=0
  y_end_bin=300
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  r=where(data_freq gt 0,c)
  if c gt 0 then data_freq[r]=alog10(data_freq[r])
  r=where(data_freq eq 0,c)
  if c gt 0 then data_freq[r]=!Values.f_nan
  c0=contour(data_freq,x_bins,y_bins,/current,position=pos[pnum,*],$
    xtitle='LWP',ytitle='Nd',title='FEB North of 60 Cases',$
    c_value=c_value_array,c_color=c_color_array,c_label_show=1,$
    /xstyle,/ystyle,xtickdir=1,ytickdir=1,axis_style=2,font_size=fs1)   
  t1=text(pos[pnum,0]+0.0*dx,pos[pnum,3]+2.0*dy,'LWP='+string(mean(mean_lwp[r_n60]),format='(F4.1)'),font_size=fs1)
  
  pnum=6
  t1=text(pos[pnum,0]+0.0*dx,pos[pnum,1]+2.*5*dy,'LOG(Frequency) contours',font_size=fs1+1)
  for i=0,4 do begin
  t1=text(pos[pnum,0]+0.0*dx,pos[pnum,1]+2.*i*dy,string(c_value_array[i],format='(F4.1)'),$
    font_size=fs1+1,color=c_color_array[*,i])
  endfor
  
  p0.save,'lwp_analysis.'+time_range_str+'.'+box_str+'.png';,height=pydim
  stop
endif

;*************************
;  figure 3 - 2d histogram of high nd cases on a map
;*************************
if 1 eq 1 then begin
  print,'figure 3,2d hist of high nd cases on a map'
  pxdim=900 & pydim=500
  xl=0.09 & xr=0.92
  yb=0.20 & yt=0.80
  sx=0.12
  sy=0.13
  numplots_x=1
  numplots_y=1
  position_plots,xl,xr,yb,yt,sx,sy,numplots_x,numplots_y,pos
  ;  Colorbar position
  cbpos=pos
  cbpos[*,0]=pos[*,2]-0.02
  cbpos[*,2]=cbpos[*,0]+0.01
  dx=0.01 & dy=0.01
  fs1=12  ;font_size
  fs2=12
  fs3=10
  congrid_factor=25L
  
  limit_vector=[llat,llon,ulat,rlon]

  ;  Map grid for histogram
  x_start_bin_map=llon
  x_end_bin_map=rlon
  x_dbin_map=1.0
  y_start_bin_map=llat
  y_end_bin_map=ulat
  y_dbin_map=0.5

  ;  Time array strings
  caldat,hi_julian_day_ovp,smm,sdd,syy,shh,smi,sss

  ;  Map histogram
  ;r=where(smm eq 11,count_nov)  ;all nov
  x_data=hi_lon_mean_ovp;[r]
  y_data=hi_lat_mean_ovp;[r]
  histogram_2d,x_data,y_data,x_start_bin_map,x_end_bin_map,x_dbin_map,x_bins,$
    y_start_bin_map,y_end_bin_map,y_dbin_map,y_bins,data_freq,data_counts_hi_map

  pnum=0
  lon_bins=x_bins
  lat_bins=y_bins
  dmin_map=0
  dmax_map=max(data_counts_hi_map)
  var_image=bytscl(data_counts_hi_map,min=dmin_map,max=dmax_map,top=top_color)
  r=where(data_counts_hi_map eq 0,c)
  if c gt 0 then var_image[r]=253
  ;s=size(var_image,/dimensions)
  ;var_image=congrid(var_image,congrid_factor*s[0],congrid_factor*s[1])
  p0=image(var_image,lon_bins,lat_bins,/buffer,rgb_table=mytable,$
    dimensions=[pxdim,pydim],$
    position=pos[pnum,*],$
    map_projection='Mercator',/box_axes,$
    label_position=0,$
    limit=limit_vector,grid_units='degrees',$
    ;label_position=1,clip=0, grid_units='degrees',$
    font_size=fs1)
  mc=mapcontinents(/hires,color='black',thick=2)
  cb=colorbar(rgb_table=mycbtable,/border,title='Counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
  t1=text(pos[pnum,0]+6*dx,pos[pnum,3]+3*dy,$
    'All Years, High Nd Quartile Locations ('+string(total(data_counts_hi_map),format='(I6)')+')',font_size=fs1)
  ;t1=text(pos[pnum,0]+6*dx,pos[pnum,3]+3*dy,$
  ;  'All Novembers, High Nd Quartile Locations ('+string(total(data_counts_hi_map),format='(I5)')+')',font_size=fs1)
  ;p0.save,'figure3.hi_nd_traj_start.nov.png',height=pydim
  p0.save,'figure3.hi_nd.map.png',height=pydim
  p0.save,'figure3.hi_nd.map.eps'
  stop
  ;*** Chlor a only ***

  ;  Map histogram
  ;r=where(hi_chlor_a ne -32767,count_chlor_a)
  r=where(hi_chlor_a ne -32767 and smm eq 11,count_chlor_a_nov)
  x_data=hi_lon_mean_ovp[r]
  y_data=hi_lat_mean_ovp[r]
  histogram_2d,x_data,y_data,x_start_bin_map,x_end_bin_map,x_dbin_map,x_bins,$
    y_start_bin_map,y_end_bin_map,y_dbin_map,y_bins,data_freq,data_counts_hi_map

  pnum=0
  lon_bins=x_bins
  lat_bins=y_bins
  dmin_map=0
  dmax_map=max(data_counts_hi_map)
  var_image=bytscl(data_counts_hi_map,min=dmin_map,max=dmax_map,top=top_color)
  r=where(data_counts_hi_map eq 0,c)
  if c gt 0 then var_image[r]=253
  p0=image(var_image,lon_bins,lat_bins,/buffer,rgb_table=mytable,$
    dimensions=[pxdim,pydim],$
    position=pos[pnum,*],$
    map_projection='Mercator',/box_axes,$
    label_position=0,$
    limit=limit_vector,grid_units='degrees',$
    ;label_position=1,clip=0, grid_units='degrees',$
    font_size=fs1)
  mc=mapcontinents(/hires,color='black',thick=2)
  cb=colorbar(rgb_table=mycbtable,/border,title='Counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
  ;t1=text(pos[pnum,0]+6*dx,pos[pnum,3]+3*dy,$
  ;  'All Years, High Nd Quartile Locations ('+string(total(data_counts_hi_map),format='(I5)')+')',font_size=fs1)
  t1=text(pos[pnum,0]+6*dx,pos[pnum,3]+3*dy,$
    'All Novembers, High Nd Quartile Locations ('+string(total(data_counts_hi_map),format='(I5)')+')',font_size=fs1)  
  t1=text(pos[pnum,0]+52*dx,pos[pnum,3]+3*dy,'chlor_a value',font_size=fs1)
  p0.save,'figure3.hi_nd_traj_start.chlor_a.'+time_range_str+'.'+box_str+'.png',height=pydim
  stop
endif  ;end of figure 2

;**********************************************
;  albedo plot
;**********************************************
if 1 eq 0 then begin
  pxdim=1000 & pydim=1000
  xl=0.08 & xr=0.90
  yb=0.10 & yt=0.90
  sx=0.1
  sy=0.1
  numplots_x=2
  numplots_y=3
  position_plots,xl,xr,yb,yt,sx,sy,numplots_x,numplots_y,pos
  ;  Colorbar position
  cbpos=pos
  cbpos[*,0]=pos[*,2]+0.01
  cbpos[*,2]=cbpos[*,0]+0.009
  dx=0.01 & dy=0.01
  pnum=0
  p0=plot([0,1],[0,1],position=pos[pnum,*],/buffer,dimensions=[pxdim,pydim],axis_style=4,/nodata)
  d=p0.convertcoord([pos[pnum,0],pos[pnum,2]],[pos[pnum,1],pos[pnum,3]],/normal,/to_device)
  isx=(d[0,1,0]-d[0,0,0])
  isy=(d[1,1,0]-d[1,0,0])
  fs1=10  ;font_size

  xhi_nd=100.0
  xlo_nd=50.0
  ;  Time array strings
  caldat,julian_day,smm,sdd,syy,shh,smi,sss
  
  min_alb=0.0 & max_alb=1.0
  min_tau=0.0 & max_tau=30.0
  min_nd=0.0 & max_nd=300.0
  min_re=0.0 & max_re=50.0
  min_lwp=0.0 & max_lwp=300.0
  lo_idx=where(mean_nd le xlo_nd,c)
  hi_idx=where(mean_nd ge xhi_nd,c)
  
  pnum=0
  data_var=mean_sw_nalb[lo_idx]
  start_bin=min_alb
  end_bin=max_alb
  dbin=0.01
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xrange=[start_bin,end_bin],$
    xtitle='Normalized SW Albedo',ytitle='Frequency',font_size=fs1,/hist,/ystyle)
  ;data_var=mean_sw_alb[lo_idx]
  ;hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  ;p1=plot(bins,data_freq,/overplot,/hist,color='black',linestyle=2)
  data_var=mean_sw_nalb[hi_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='red')
  ;data_var=mean_sw_alb[hi_idx]
  ;hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  ;p1=plot(bins,data_freq,/overplot,/hist,color='red',linestyle=2)
  
  pnum=1
  data_var=mean_tau[lo_idx]
  start_bin=min_tau
  end_bin=max_tau
  dbin=0.5
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xrange=[start_bin,end_bin],$
    xtitle='Modis Optical Depth',ytitle='Frequency',font_size=fs1,/hist,/ystyle)
  data_var=mean_tau[hi_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='red')
  
  pnum=2
  data_var=mean_nd[lo_idx]
  start_bin=min_nd
  end_bin=max_nd
  dbin=5.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xrange=[start_bin,end_bin],$
    xtitle='Number Concentration',ytitle='Frequency',font_size=fs1,/hist,/ystyle)
  data_var=mean_nd[hi_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='red')
  
  pnum=3
  data_var=mean_re[lo_idx]
  start_bin=min_re
  end_bin=max_re
  dbin=0.5
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xrange=[start_bin,end_bin],$
    xtitle='Effective Radius',ytitle='Frequency',font_size=fs1,/hist,/ystyle)
  data_var=mean_re[hi_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='red')
  
  pnum=4
  data_var=mean_lwp[lo_idx]
  start_bin=min_lwp
  end_bin=max_lwp
  dbin=5.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xrange=[start_bin,end_bin],$
    xtitle='LWP',ytitle='Frequency',font_size=fs1,/hist,/ystyle)
  data_var=mean_lwp[hi_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='red')
  
  pnum=5
  data_var=mean_lat[lo_idx]
  start_bin=llat;min(mean_lat)
  end_bin=ulat;max(mean_lat)
  dbin=0.5;(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],/xstyle,/ystyle,$
    xtitle='Latitude',ytitle='Frequency',font_size=fs1,/hist)
  data_var=mean_lat[hi_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='red',/hist)
  ;t0=text(pos[pnum,0]-3*dx,pos[pnum,3]+5*dy,'b.',font_size=14)
  ;
  ;  Title of time range
  yy_1m_str=string(yy_1m,format='(I4)')
  tstr='NDJF '+yy_1m_str[0]+'-'+yy_1m_str[-1]
  pnum=4
  t2=text((pos[pnum,2]+pos[pnum,0])/2.0,pos[pnum,3]+1.0*dy,tstr,font_size=fs2,$
    alignment=0.5)
  pnum=5
  tstr=string(llat,format='(I03)')+' to '+string(ulat,format='(I03)')+' Lat,'+$
    string(llon,format='(I03)')+' to '+string(rlon,format='(I03)')+' Lon'
  t2=text((pos[pnum,2]+pos[pnum,0])/2.0,pos[pnum,3]+1.0*dy,tstr,font_size=fs2,$
    alignment=0.5)
    
  t0=text(xl+0.2,0.97,'High Nd (>100)',font_size=14,color='red')  
  t0=text(xl,0.97,'Low Nd (<50)',font_size=14,color='black')
  
  p0.save,'albedo_plot.'+time_range_str+'.'+box_str+'.png',height=pydim
  ;p0.save,'figure2.nd_lat.'+time_range_str+'.'+box_str+'.eps'
  stop
  
;  ;  Hi and Lo south 60, alldata
;  pnum=8
;  start_bin=llat
;  end_bin=-60.0
;  dbin=(end_bin-start_bin)/20.0
;   r_lo_south_60_nov_chlor=where(mean_lat le -60.0 and mean_nd le xlo_nd and smm eq 11 and chlor_a ne -32767) ;lo south -60
;  r_lo_south_60=where(mean_lat le -60.0 and mean_nd le xlo_nd) ;lo south -60
;  r_hi_south_60=where(mean_lat le -60.0 and mean_nd ge xhi_nd) ;hi south -60
;  r_lo_south_60_chlor=where(mean_lat le -60.0 and mean_nd le xlo_nd and chlor_a ne -32767) ;lo south -60
;  r_hi_south_60_chlor=where(mean_lat le -60.0 and mean_nd ge xhi_nd and chlor_a ne -32767) ;hi south -60
;  data_var=mean_lat[r_lo_south_60]
;  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
;  p0=plot(bins,data_freq,/hist,color='blue',position=pos[pnum,*],/buffer,/xstyle,$
;    yrange=[0,0.30],dimensions=[pxdim,pydim],$
;    xtitle='Latitude',ytitle='Frequency',font_size=fs1,title='South of -60S, All Data')
;  data_var=mean_lat[r_lo_south_60_chlor]
;  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
;  p1=plot(bins,data_freq,/overplot,/hist,color='purple')
;  data_var=mean_lat[r_hi_south_60]
;  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
;  p1=plot(bins,data_freq,/overplot,/hist,color='red')
;  data_var=mean_lat[r_hi_south_60_chlor]
;  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
;  p1=plot(bins,data_freq,/overplot,/hist,color='orange')
;  t1=text(pos[pnum,0]+1*dx,pos[pnum,3]-2*dy,'Hi Nd',color='red',font_size=fs1)
;  t1=text(pos[pnum,0]+1*dx,pos[pnum,3]-4*dy,'Hi Nd chlor',color='orange',font_size=fs1)
;  t1=text(pos[pnum,0]+1*dx,pos[pnum,3]-6*dy,'Lo Nd',color='blue',font_size=fs1)
;  t1=text(pos[pnum,0]+1*dx,pos[pnum,3]-8*dy,'Lo Nd chlor',color='purple',font_size=fs1)
  
endif

;**********************************************
;  Plot of hi and lo nd parcels north and south of -60S
;**********************************************
if 1 eq 0 then begin
  print,'figure hi and lo north and south of 60S
  pxdim=1000 & pydim=1000  
  xl=0.07 & xr=0.90
  yb=0.07 & yt=0.87
  sx=0.07
  sy=0.07
  numplots_x=4
  numplots_y=3
  position_plots,xl,xr,yb,yt,sx,sy,numplots_x,numplots_y,pos
  dx=0.01 & dy=0.01
  fs1=10
  fs2=12

  xhi_nd=100.0
  xlo_nd=50.0
  ;  Time array strings
  caldat,julian_day,smm,sdd,syy,shh,smi,sss

  start_bin=llat
  end_bin=ulat
  dbin=1.0 

  ;  Hi and Lo south 60, alldata
  pnum=8
  start_bin=llat
  end_bin=-60.0
  dbin=(end_bin-start_bin)/20.0
  r_lo_south_60=where(mean_lat le -60.0 and mean_nd le xlo_nd) ;lo south -60
  r_hi_south_60=where(mean_lat le -60.0 and mean_nd ge xhi_nd) ;hi south -60
  r_lo_south_60_chlor=where(mean_lat le -60.0 and mean_nd le xlo_nd and chlor_a ne -32767) ;lo south -60
  r_hi_south_60_chlor=where(mean_lat le -60.0 and mean_nd ge xhi_nd and chlor_a ne -32767) ;hi south -60
  data_var=mean_lat[r_lo_south_60]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/hist,color='blue',position=pos[pnum,*],/buffer,/xstyle,$
    yrange=[0,0.30],dimensions=[pxdim,pydim],$
    xtitle='Latitude',ytitle='Frequency',font_size=fs1,title='South of -60S, All Data')
  data_var=mean_lat[r_lo_south_60_chlor]  
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='purple')  
  data_var=mean_lat[r_hi_south_60]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='red')
  data_var=mean_lat[r_hi_south_60_chlor]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='orange')
  t1=text(pos[pnum,0]+1*dx,pos[pnum,3]-2*dy,'Hi Nd',color='red',font_size=fs1)
  t1=text(pos[pnum,0]+1*dx,pos[pnum,3]-4*dy,'Hi Nd chlor',color='orange',font_size=fs1)
  t1=text(pos[pnum,0]+1*dx,pos[pnum,3]-6*dy,'Lo Nd',color='blue',font_size=fs1)
  t1=text(pos[pnum,0]+1*dx,pos[pnum,3]-8*dy,'Lo Nd chlor',color='purple',font_size=fs1)

  ;  Hi and Lo south 60, NOV
  pnum=0
  r_lo_south_60_nov=where(mean_lat le -60.0 and mean_nd le xlo_nd and smm eq 11) ;lo south -60
  r_hi_south_60_nov=where(mean_lat le -60.0 and mean_nd ge xhi_nd and smm eq 11) ;hi south -60
  r_lo_south_60_nov_chlor=where(mean_lat le -60.0 and mean_nd le xlo_nd and smm eq 11 and chlor_a ne -32767) ;lo south -60
  r_hi_south_60_nov_chlor=where(mean_lat le -60.0 and mean_nd ge xhi_nd and smm eq 11 and chlor_a ne -32767) ;hi south -60
  data_var=mean_lat[r_lo_south_60_nov]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/hist,color='blue',position=pos[pnum,*],/current,/xstyle,$
    yrange=[0,0.30],$
    xtitle='Latitude',ytitle='Frequency',font_size=fs1,title='South of -60S, All Nov')
  data_var=mean_lat[r_lo_south_60_nov_chlor]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='purple')
  data_var=mean_lat[r_hi_south_60_nov]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='red')
  data_var=mean_lat[r_hi_south_60_nov_chlor]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='orange')

  ;  Hi and Lo south 60, DEC
  pnum=1
  r_lo_south_60_dec=where(mean_lat le -60.0 and mean_nd le xlo_nd and smm eq 12) ;lo south -60
  r_hi_south_60_dec=where(mean_lat le -60.0 and mean_nd ge xhi_nd and smm eq 12) ;hi south -60
  r_lo_south_60_dec_chlor=where(mean_lat le -60.0 and mean_nd le xlo_nd and smm eq 12 and chlor_a ne -32767) 
  r_hi_south_60_dec_chlor=where(mean_lat le -60.0 and mean_nd ge xhi_nd and smm eq 12 and chlor_a ne -32767) 
  data_var=mean_lat[r_lo_south_60_dec]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/hist,color='blue',position=pos[pnum,*],/current,/xstyle,$
    yrange=[0,0.30],$
    xtitle='Latitude',ytitle='Frequency',font_size=fs1,title='South of -60S, All Dec')
  data_var=mean_lat[r_lo_south_60_dec_chlor]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='purple')  
  data_var=mean_lat[r_hi_south_60_dec]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='red')
  data_var=mean_lat[r_hi_south_60_dec_chlor]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='orange')

  ;  Hi and Lo south 60, JAN
  pnum=2
  r_lo_south_60_jan=where(mean_lat le -60.0 and mean_nd le xlo_nd and smm eq 1) ;lo below -60
  r_hi_south_60_jan=where(mean_lat le -60.0 and mean_nd ge xhi_nd and smm eq 1) ;hi below -60
  r_lo_south_60_jan_chlor=where(mean_lat le -60.0 and mean_nd le xlo_nd and smm eq 1 and chlor_a ne -32767)
  r_hi_south_60_jan_chlor=where(mean_lat le -60.0 and mean_nd ge xhi_nd and smm eq 1 and chlor_a ne -32767)
  data_var=mean_lat[r_lo_south_60_jan]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/hist,color='blue',position=pos[pnum,*],/current,/xstyle,$
    yrange=[0,0.30],$
    xtitle='Latitude',ytitle='Frequency',font_size=fs1,title='South of -60S, All Jan')
  data_var=mean_lat[r_lo_south_60_jan_chlor]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='purple')  
  data_var=mean_lat[r_hi_south_60_jan]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='red')
  data_var=mean_lat[r_hi_south_60_jan_chlor]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='orange')

  ;  Hi and Lo below 60, Feb
  pnum=3
  r_lo_south_60_feb=where(mean_lat le -60.0 and mean_nd le xlo_nd and smm eq 2) ;lo below -60
  r_hi_south_60_feb=where(mean_lat le -60.0 and mean_nd ge xhi_nd and smm eq 2) ;hi below -60
  r_lo_south_60_feb_chlor=where(mean_lat le -60.0 and mean_nd le xlo_nd and smm eq 2 and chlor_a ne -32767)
  r_hi_south_60_feb_chlor=where(mean_lat le -60.0 and mean_nd ge xhi_nd and smm eq 2 and chlor_a ne -32767)
  data_var=mean_lat[r_lo_south_60_feb]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/hist,color='blue',position=pos[pnum,*],/current,/xstyle,$
    yrange=[0,0.30],$
    xtitle='Latitude',ytitle='Frequency',font_size=fs1,title='South of -60S, All Feb')
  data_var=mean_lat[r_lo_south_60_feb_chlor]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='purple')  
  data_var=mean_lat[r_hi_south_60_feb]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='red')
  data_var=mean_lat[r_hi_south_60_feb_chlor]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='orange')

;**********************************
start_bin=-60.0
end_bin=ulat
dbin=(end_bin-start_bin)/20.0

;  Hi and Lo north 60S, alldata
pnum=9
r_lo_north_60=where(mean_lat gt -60.0 and mean_nd le xlo_nd) ;lo above -60
r_hi_north_60=where(mean_lat gt -60.0 and mean_nd ge xhi_nd) ;hi above -60
r_lo_north_60_chlor=where(mean_lat gt -60.0 and mean_nd le xlo_nd and chlor_a ne -32767) 
r_hi_north_60_chlor=where(mean_lat gt -60.0 and mean_nd ge xhi_nd and chlor_a ne -32767) 
data_var=mean_lat[r_lo_north_60]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p2=plot(bins,data_freq,/hist,color='blue',position=pos[pnum,*],/current,/xstyle,$
  yrange=[0,0.14],$
  xtitle='Latitude',ytitle='Frequency',font_size=fs1,title='North of -60S, AllData')
data_var=mean_lat[r_lo_north_60_chlor]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p1=plot(bins,data_freq,/overplot,/hist,color='purple')  
data_var=mean_lat[r_hi_north_60]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p3=plot(bins,data_freq,/overplot,/hist,color='red')
data_var=mean_lat[r_hi_north_60_chlor]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p3=plot(bins,data_freq,/overplot,/hist,color='orange')

;  Hi and Lo north 60S, nov
pnum=4
r_lo_north_60_nov=where(mean_lat gt -60.0 and mean_nd le xlo_nd and smm eq 11) ;lo above -60
r_hi_north_60_nov=where(mean_lat gt -60.0 and mean_nd ge xhi_nd and smm eq 11) ;hi above -60
r_lo_north_60_nov_chlor=where(mean_lat gt -60.0 and mean_nd le xlo_nd and smm eq 11 and chlor_a ne -32767)
r_hi_north_60_nov_chlor=where(mean_lat gt -60.0 and mean_nd ge xhi_nd and smm eq 11 and chlor_a ne -32767)
data_var=mean_lat[r_lo_north_60_nov]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p2=plot(bins,data_freq,/hist,color='blue',position=pos[pnum,*],/current,/xstyle,$
  yrange=[0,0.14],$
  xtitle='Latitude',ytitle='Frequency',font_size=fs1,title='North of -60S, All Nov')
data_var=mean_lat[r_lo_north_60_nov_chlor]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p1=plot(bins,data_freq,/overplot,/hist,color='purple')  
data_var=mean_lat[r_hi_north_60_nov]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p3=plot(bins,data_freq,/overplot,/hist,color='red')
data_var=mean_lat[r_hi_north_60_nov_chlor]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p3=plot(bins,data_freq,/overplot,/hist,color='orange')

;  Hi and Lo north 60S, dec
pnum=5
r_lo_north_60_dec=where(mean_lat gt -60.0 and mean_nd le xlo_nd and smm eq 12) 
r_hi_north_60_dec=where(mean_lat gt -60.0 and mean_nd ge xhi_nd and smm eq 12) 
r_lo_north_60_dec_chlor=where(mean_lat gt -60.0 and mean_nd le xlo_nd and smm eq 12 and chlor_a ne -32767)
r_hi_north_60_dec_chlor=where(mean_lat gt -60.0 and mean_nd ge xhi_nd and smm eq 12 and chlor_a ne -32767)
data_var=mean_lat[r_lo_north_60_dec]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p2=plot(bins,data_freq,/hist,color='blue',position=pos[pnum,*],/current,/xstyle,$
  yrange=[0,0.14],$
  xtitle='Latitude',ytitle='Frequency',font_size=fs1,title='North of -60S, All Dec')
data_var=mean_lat[r_lo_north_60_dec_chlor]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p1=plot(bins,data_freq,/overplot,/hist,color='purple')  
data_var=mean_lat[r_hi_north_60_dec]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p3=plot(bins,data_freq,/overplot,/hist,color='red')
data_var=mean_lat[r_hi_north_60_dec_chlor]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p3=plot(bins,data_freq,/overplot,/hist,color='orange')

;  Hi and Lo north 60S, jan
pnum=6
r_lo_north_60_jan=where(mean_lat gt -60.0 and mean_nd le xlo_nd and smm eq 1) 
r_hi_north_60_jan=where(mean_lat gt -60.0 and mean_nd ge xhi_nd and smm eq 1) 
r_lo_north_60_jan_chlor=where(mean_lat gt -60.0 and mean_nd le xlo_nd and smm eq 1 and chlor_a ne -32767)
r_hi_north_60_jan_chlor=where(mean_lat gt -60.0 and mean_nd ge xhi_nd and smm eq 1 and chlor_a ne -32767)
data_var=mean_lat[r_lo_north_60_jan]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p2=plot(bins,data_freq,/hist,color='blue',position=pos[pnum,*],/current,/xstyle,$
  yrange=[0,0.14],$
  xtitle='Latitude',ytitle='Frequency',font_size=fs1,title='North of -60S, All Jan')
data_var=mean_lat[r_lo_north_60_jan_chlor]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p1=plot(bins,data_freq,/overplot,/hist,color='purple')  
data_var=mean_lat[r_hi_north_60_jan]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p3=plot(bins,data_freq,/overplot,/hist,color='red')
data_var=mean_lat[r_hi_north_60_jan_chlor]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p3=plot(bins,data_freq,/overplot,/hist,color='orange')
  
;  Hi and Lo north 60S, feb
pnum=7
r_lo_north_60_feb=where(mean_lat gt -60.0 and mean_nd le xlo_nd and smm eq 2) 
r_hi_north_60_feb=where(mean_lat gt -60.0 and mean_nd ge xhi_nd and smm eq 2) 
r_lo_north_60_feb_chlor=where(mean_lat gt -60.0 and mean_nd le xlo_nd and smm eq 2 and chlor_a ne -32767)
r_hi_north_60_feb_chlor=where(mean_lat gt -60.0 and mean_nd ge xhi_nd and smm eq 2 and chlor_a ne -32767)
data_var=mean_lat[r_lo_north_60_feb]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p2=plot(bins,data_freq,/hist,color='blue',position=pos[pnum,*],/current,/xstyle,$
  yrange=[0,0.14],$
  xtitle='Latitude',ytitle='Frequency',font_size=fs1,title='North of -60S, All Feb')
data_var=mean_lat[r_lo_north_60_feb_chlor]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p1=plot(bins,data_freq,/overplot,/hist,color='purple')  
data_var=mean_lat[r_hi_north_60_feb]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p3=plot(bins,data_freq,/overplot,/hist,color='red')
data_var=mean_lat[r_hi_north_60_feb_chlor]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p3=plot(bins,data_freq,/overplot,/hist,color='orange')

;********************************

;  Hi and Lo below 60, alldjf
pnum=10
start_bin=llat
end_bin=-60.0
dbin=(end_bin-start_bin)/20.0
r_lo_south_60_djf=where(mean_lat le -60.0 and mean_nd le xlo_nd and $
  (smm eq 12 or smm eq 1 or smm eq 2))
r_hi_south_60_djf=where(mean_lat le -60.0 and mean_nd ge xhi_nd and $
  (smm eq 12 or smm eq 1 or smm eq 2))
r_lo_south_60_djf_chlor=where(mean_lat le -60.0 and mean_nd le xlo_nd and chlor_a ne -32767 and $
  (smm eq 12 or smm eq 1 or smm eq 2))
r_hi_south_60_djf_chlor=where(mean_lat le -60.0 and mean_nd ge xhi_nd and chlor_a ne -32767 and $
  (smm eq 12 or smm eq 1 or smm eq 2))  
data_var=mean_lat[r_lo_south_60_djf]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p0=plot(bins,data_freq,/hist,color='blue',position=pos[pnum,*],/current,/xstyle,$
  yrange=[0,0.30],$
  xtitle='Latitude',ytitle='Frequency',font_size=fs1,title='South of -60S, All DJF')
data_var=mean_lat[r_lo_south_60_djf_chlor]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p1=plot(bins,data_freq,/overplot,/hist,color='purple')  
data_var=mean_lat[r_hi_south_60_djf]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p1=plot(bins,data_freq,/overplot,/hist,color='red')
data_var=mean_lat[r_hi_south_60_djf_chlor]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p1=plot(bins,data_freq,/overplot,/hist,color='orange')

;  Hi and Lo north 60S, alldjf
pnum=11
start_bin=-60.0
end_bin=ulat
dbin=(end_bin-start_bin)/20.0
r_lo_north_60_djf=where(mean_lat gt -60.0 and mean_nd le xlo_nd and $
  (smm eq 12 or smm eq 1 or smm eq 2)) 
r_hi_north_60_djf=where(mean_lat gt -60.0 and mean_nd ge xhi_nd and $
  (smm eq 12 or smm eq 1 or smm eq 2)) 
r_lo_north_60_djf_chlor=where(mean_lat gt -60.0 and mean_nd le xlo_nd and chlor_a ne -32767 and $
  (smm eq 12 or smm eq 1 or smm eq 2))
r_hi_north_60_djf_chlor=where(mean_lat gt -60.0 and mean_nd ge xhi_nd and chlor_a ne -32767 and $
  (smm eq 12 or smm eq 1 or smm eq 2))  
data_var=mean_lat[r_lo_north_60_djf]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p2=plot(bins,data_freq,/hist,color='blue',position=pos[pnum,*],/current,/xstyle,$
  yrange=[0,0.14],$
  xtitle='Latitude',ytitle='Frequency',font_size=fs1,title='North of -60S, All DJF')
data_var=mean_lat[r_lo_north_60_djf_chlor]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p1=plot(bins,data_freq,/overplot,/hist,color='purple')  
data_var=mean_lat[r_hi_north_60_djf]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p3=plot(bins,data_freq,/overplot,/hist,color='red')
data_var=mean_lat[r_hi_north_60_djf_chlor]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
p1=plot(bins,data_freq,/overplot,/hist,color='orange')

;  Titles
;if day_flag eq '3day' then begin
;  t1=text(xl,0.97,'Fraction of 3-day back trajectory in Lat bin',font_size=fs1)
;endif else if day_flag eq '5day' then begin
;  t1=text(xl,0.97,'Fraction of 5-day back trajectory in Lat bin',font_size=fs1)
;endif
t1=text(xl,0.94,'The observed parcel is located in the region indicated at the top of the histogram panel.',$
  font_size=fs1)

p0.save,'hist_north_south_60_hi_lo'+time_range_str+'.'+box_str+'.png',height=pydim

stop
endif

;**********************************
;  Trajectory plot
;**********************************
print,'trajectory plot'
;  Set up the positions
pxdim=1000 & pydim=1000
xl=0.05 & xr=0.97
yb=0.04 & yt=0.92
sx=0.05
sy=0.05
numplots_x=4
if yy_1m[0] eq 2015 then numplots_y=4
if yy_1m[0] eq 2014 then numplots_y=5

position_plots,xl,xr,yb,yt,sx,sy,numplots_x,numplots_y,pos
;  Colorbar position
cbpos=pos
cbpos[*,0]=pos[*,2]-0.03
cbpos[*,2]=cbpos[*,0]+0.009
dx=0.01 & dy=0.01
pnum=0
p0=plot([0,1],[0,1],position=pos[pnum,*],/buffer,dimensions=[pxdim,pydim],axis_style=4,/nodata)
d=p0.convertcoord([pos[pnum,0],pos[pnum,2]],[pos[pnum,1],pos[pnum,3]],/normal,/to_device)
isx=(d[0,1,0]-d[0,0,0])
isy=(d[1,1,0]-d[1,0,0])
;  Date format for plotting
dummy=label_date(date_format=['%M/%D!C%Y'])
fs1=10  ;font_size
fs2=14
;
; **SW=-76,40  NE=-45,152
ulat=-30.0
llat=-78.0
llon=30.0
rlon=172.0
limit_vector=[llat,llon,ulat,rlon]

;  Map grid for histogram
x_start_bin_map=llon
x_end_bin_map=rlon
x_dbin_map=1.0
y_start_bin_map=llat
y_end_bin_map=ulat
y_dbin_map=0.5

;  all_mean_lat and lat_traj_start,lon_traj_start
lat_traj_start=lat_traj[*,0]
lon_traj_start=lon_traj[*,0]
julian_day_traj_start=julian_day_traj[*,0]
caldat,julian_day_traj_start,tmm,tdd,tyy,thh,tmi,tss

;  loop through the seasons
if yy_1m[0] eq 2015 then begin
  date_array=['201811','201812','201901','201902',$
    '201711','201712','201801','201802',$
    '201611','201612','201701','201702',$
    '201511','201512','201601','201602'] 
endif 
if yy_1m[0] eq 2014 then begin
  date_array=['201811','201812','201901','201902',$
              '201711','201712','201801','201802',$
              '201611','201612','201701','201702',$
              '201511','201512','201601','201602',$
              '201411','201412','201501','201502']
endif
            
for i=0,n_elements(date_array)-1 do begin
   
   pyear=strmid(date_array[i],0,4)
   pmonth=strmid(date_array[i],4,2)
   print,'plot hi ',pyear,pmonth
  ;  Trajectory locations
  ridx=where(tmm eq pmonth and tyy eq pyear and mean_nd ge 100.0,cidx) 
  print,cidx   
  if cidx gt 0 then begin
  ;  Map histogram
  x_data=lon_traj[ridx,*]
  y_data=lat_traj[ridx,*]
  histogram_2d,x_data,y_data,x_start_bin_map,x_end_bin_map,x_dbin_map,x_bins,$
    y_start_bin_map,y_end_bin_map,y_dbin_map,y_bins,data_freq,data_counts_all_map
  pnum=i
  lon_bins=x_bins
  lat_bins=y_bins
  dmin_map=0
  if eos eq 'MOYD' then begin
    dmax_map=1800.0;max(data_counts_all_map)
  endif else begin
    dmax_map=900.0  ;MOD or MYD
  endelse
  var_image=bytscl(data_counts_all_map,min=dmin_map,max=dmax_map,top=top_color)
  r=where(data_counts_all_map eq 0,c)
  if c gt 0 then var_image[r]=253
  p0=image(var_image,lon_bins,lat_bins,/current,rgb_table=mytable,$
    position=pos[pnum,*],map_projection='Mercator',/box_axes,clip=0,$
    label_position=0,limit=limit_vector,grid_units='degrees',font_size=fs1)
  mc=mapcontinents()
  ;cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
  ;  orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
  caldat,julian_day_traj_start[ridx[0]],smm,sdd,syy,shh,smi,sss
  t1=text(pos[pnum,0],pos[pnum,3]+0*dy,$
    string(syy,format='(I4)')+string(smm,format='(I02)'),font_size=fs2)
  t1=text(pos[pnum,0]+6*dx,pos[pnum,3]+0*dy,string(dmax_map),font_size=fs1)
  endif
endfor

cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
  orientation=0,$
  textpos=1,$
  ;position=cbpos[pnum,*],$
  position=[0.6,0.95,0.8,0.96],$
  range=[dmin_map,dmax_map])
t1=text(xl,0.97,eos+' 5-day back trajectories for High Nd cases',font_size=14)
      
p0.save,'traj_comparison.'+eos+'.'+time_range_str+'.'+box_str+'.hi.png',height=pydim

p0=plot([0,1],[0,1],position=pos[pnum,*],/buffer,dimensions=[pxdim,pydim],axis_style=4,/nodata)

for i=0,n_elements(date_array)-1 do begin

  pyear=strmid(date_array[i],0,4)
  pmonth=strmid(date_array[i],4,2)
  print,'plot lo ',pyear,pmonth
  ;  Trajectory locations
  ridx=where(tmm eq pmonth and tyy eq pyear and mean_nd le 50.0,cidx)
  print,cidx
  if cidx gt 0 then begin
    ;  Map histogram
    x_data=lon_traj[ridx,*]
    y_data=lat_traj[ridx,*]
    histogram_2d,x_data,y_data,x_start_bin_map,x_end_bin_map,x_dbin_map,x_bins,$
      y_start_bin_map,y_end_bin_map,y_dbin_map,y_bins,data_freq,data_counts_all_map
    pnum=i
    lon_bins=x_bins
    lat_bins=y_bins
    dmin_map=0
    if eos eq 'MOYD' then begin
      dmax_map=1000.0;max(data_counts_all_map)
    endif else begin
      dmax_map=500.0  ;MOD or MYD
    endelse
    var_image=bytscl(data_counts_all_map,min=dmin_map,max=dmax_map,top=top_color)
    r=where(data_counts_all_map eq 0,c)
    if c gt 0 then var_image[r]=253
    p0=image(var_image,lon_bins,lat_bins,/current,rgb_table=mytable,$
      position=pos[pnum,*],map_projection='Mercator',/box_axes,clip=0,$
      label_position=0,limit=limit_vector,grid_units='degrees',font_size=fs1)
    mc=mapcontinents()
    ;cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    ;  orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
    caldat,julian_day_traj_start[ridx[0]],smm,sdd,syy,shh,smi,sss
    t1=text(pos[pnum,0],pos[pnum,3]+0*dy,$
      string(syy,format='(I4)')+string(smm,format='(I02)'),font_size=fs2)
    t1=text(pos[pnum,0]+6*dx,pos[pnum,3]+0*dy,string(dmax_map),font_size=fs1)
  endif
endfor

cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
  orientation=0,$
  textpos=1,$
  ;position=cbpos[pnum,*],$
  position=[0.6,0.95,0.8,0.96],$
  range=[dmin_map,dmax_map])
t1=text(xl,0.97,eos+' 5-day back trajectories for Low Nd cases',font_size=14)
p0.save,'traj_comparison.'+eos+'.'+time_range_str+'.'+box_str+'.lo.png',height=pydim

stop






;*********************************************
;  This is the wrong plot - not updated for chlor_a
;create a cumulative distribution and a combined PDF 
;combining all decembers and Januarys for north of 60?
;**********************************************
pxdim=900 & pydim=900
xl=0.09 & xr=0.95
yb=0.20 & yt=0.80
sx=0.12
sy=0.13
numplots_x=2
numplots_y=2
position_plots,xl,xr,yb,yt,sx,sy,numplots_x,numplots_y,pos

caldat,all_julian_day,mm_all,dd,yy,hh,mi,ss
caldat,hi_julian_day_ovp,mm_hi,dd,yy,hh,mi,ss
caldat,lo_julian_day_ovp,mm_lo,dd,yy,hh,mi,ss

;plot_flag='dec_jan.n70'
;plot_flag='dec_jan.n60'
;plot_flag='n60'
plot_flag='n70'

if plot_flag eq 'dec_jan.n70' then begin
r_all=where((mm_all eq 12 or mm_all eq 1) and (all_mean_lat gt llat),count)
r_hi=where((mm_hi eq 12 or mm_hi eq 1) and (hi_lat_mean_ovp gt llat),count)
r_lo=where((mm_lo eq 12 or mm_lo eq 1) and (lo_lat_mean_ovp gt llat),count)
endif else if plot_flag eq 'dec_jan.n60' then begin
r_all=where((mm_all eq 12 or mm_all eq 1) and (all_mean_lat gt -60),count)
r_hi=where((mm_hi eq 12 or mm_hi eq 1) and (hi_lat_mean_ovp gt -60),count)
r_lo=where((mm_lo eq 12 or mm_lo eq 1) and (lo_lat_mean_ovp gt -60),count)
endif else if plot_flag eq 'n60' then begin
r_all=where((all_mean_lat gt -60),count)
r_hi=where((hi_lat_mean_ovp gt -60),count)
r_lo=where((lo_lat_mean_ovp gt -60),count)
endif else if plot_flag eq 'n70' then begin
r_all=where((all_mean_lat gt llat),count)
r_hi=where((hi_lat_mean_ovp gt llat),count)
r_lo=where((lo_lat_mean_ovp gt llat),count)
endif

start_bin=llat;-60.0;llat;floor(min(data_var))
end_bin=ulat;ceil(max(data_var))
dbin=0.5;(end_bin-start_bin)/100.0

data_var=all_mean_lat[r_all]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
data_freq_all=data_freq
data_counts_all=data_counts

data_var=hi_lat_mean_ovp[r_hi]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
data_freq_hi=data_freq
data_counts_hi=data_counts

data_var=lo_lat_mean_ovp[r_lo]
hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
data_freq_lo=data_freq
data_counts_lo=data_counts

pnum=0
p0=plot(bins,data_counts_all,/buffer,position=pos[pnum,*],/xstyle,/ystyle,$
  dimensions=[pxdim,pydim],$
  xtitle='Latitude',ytitle='counts',font_size=fs1,/hist)
p1=plot(bins,data_counts_hi,/overplot,color='green',/hist)
p1=plot(bins,data_counts_lo,/overplot,color='blue',/hist)  
  
pnum=1
p0=plot(bins,data_freq_all,/current,position=pos[pnum,*],/xstyle,/ystyle,$
  xtitle='Latitude',ytitle='Frequency',font_size=fs1,/hist)
p1=plot(bins,data_freq_hi,/overplot,color='green',/hist)
p1=plot(bins,data_freq_lo,/overplot,color='blue',/hist)  
  
  
pnum=2
;cdf=total(data_counts,/cumulative) / total(data_counts)
cdf_all=make_array(n_elements(data_counts_all),/float)
cdf_hi=make_array(n_elements(data_counts_hi),/float)
cdf_lo=make_array(n_elements(data_counts_lo),/float)
for i=0,n_elements(data_counts_all)-1 do begin
  cdf_all[i]=total(data_counts_all[0:i]);/total(data_counts)
  cdf_hi[i]=total(data_counts_hi[0:i]);/total(data_counts)
  cdf_lo[i]=total(data_counts_lo[0:i]);/total(data_counts)
endfor

p0=plot(bins,cdf_all,/current,position=pos[pnum,*],/xstyle,/ystyle,$
  xtitle='Latitude',ytitle='Cumulative Counts',font_size=fs1,/hist)
p1=plot(bins,cdf_hi,/overplot,color='green',/hist)
p1=plot(bins,cdf_lo,/overplot,color='blue',/hist)

pnum=3
p0=plot(bins,cdf_all/total(data_counts_all),/current,position=pos[pnum,*],/xstyle,/ystyle,$
  xtitle='Latitude',ytitle='Cumulative Frequency',font_size=fs1,/hist)
p1=plot(bins,cdf_hi/total(data_counts_hi),/overplot,color='green',/hist)
p1=plot(bins,cdf_lo/total(data_counts_lo),/overplot,color='blue',/hist)

if plot_flag eq 'dec_jan' or plot_flag eq 'dec_jan.n60' then $
t1=text(0.1,0.90,'Dec and Jan only',font_size=fs1)


;  String of search box limits
box_str2=string(start_bin,format='(I03)')+'_'+string(end_bin,format='(I03)')+'lat.'+$
  string(llon,format='(I03)')+'_'+string(rlon,format='(I03)')+'lon'
  
p0.save,'cdf.'+box_str2+'.'+plot_flag+'.png'  


stop


end