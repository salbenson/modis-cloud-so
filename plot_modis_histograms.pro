;*******************************
;  Reads in all the individual histogram files and puts the 
;  data into one file for each month
;  
;  Plots:
;  Nd quartiles and latitude distribution, final version in plot_modis_hist_full_dataset.pro
;  This is figure 1 in the paper.
;  
;  Histogram diagnostic with histograms of Nd,lon,lat,tau and maps of high and low.
;*******************************

pro plot_modis_histograms

;  imac or chpc
path_prefix='/Volumes/' 
;path_prefix='/uufs/chpc.utah.edu/common/home/'

;  Histogram directory
hdir=path_prefix+'mace-group4/modis/hysplit/modis_histograms_sm/'

;  Choose aqua or terra
;eos='MYD'
eos='MOD'
;eos='MOYD'  ;don't need to do both for this step

;  Time range to analyze
;  SEASON Nov 2018-Feb 2019
julian_day_1d=timegen(start=julday(11,1,2018,0,0,0),final=julday(11,30,2018,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(12,1,2018,0,0,0),final=julday(12,31,2018,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(1,1,2019,0,0,0),final=julday(1,31,2019,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(2,1,2019,0,0,0),final=julday(2,28,2019,23,59,59),units='days',step_size=1)
  
;  SEASON Nov 2017-Feb 2018
;julian_day_1d=timegen(start=julday(11,1,2017,0,0,0),final=julday(11,30,2017,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(12,1,2017,0,0,0),final=julday(12,31,2017,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(1,1,2018,0,0,0),final=julday(1,31,2018,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(2,1,2018,0,0,0),final=julday(2,28,2018,23,59,59),units='days',step_size=1)

;  SEASON Nov 2016-Feb 2017
;julian_day_1d=timegen(start=julday(11,1,2016,0,0,0),final=julday(11,30,2016,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(12,1,2016,0,0,0),final=julday(12,31,2016,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(1,1,2017,0,0,0),final=julday(1,31,2017,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(2,1,2017,0,0,0),final=julday(2,28,2017,23,59,59),units='days',step_size=1)

;  SEASON Nov 2015-Feb 2016
;julian_day_1d=timegen(start=julday(11,1,2015,0,0,0),final=julday(11,30,2015,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(12,1,2015,0,0,0),final=julday(12,31,2015,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(1,1,2016,0,0,0),final=julday(1,31,2016,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(2,1,2016,0,0,0),final=julday(2,29,2016,23,59,59),units='days',step_size=1)

;  SEASON Nov 2014-Feb 2015
;julian_day_1d=timegen(start=julday(11,1,2014,0,0,0),final=julday(11,30,2014,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(12,1,2014,0,0,0),final=julday(12,31,2014,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(1,1,2015,0,0,0),final=julday(1,31,2015,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(2,1,2015,0,0,0),final=julday(2,28,2015,23,59,59),units='days',step_size=1)

;  SEASON Nov 2006-Feb 2007
;julian_day_1d=timegen(start=julday(11,1,2006,0,0,0),final=julday(11,30,2006,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(12,1,2006,0,0,0),final=julday(12,31,2006,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(1,1,2007,0,0,0),final=julday(1,31,2007,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(2,1,2007,0,0,0),final=julday(2,28,2007,23,59,59),units='days',step_size=1)

;  Get day of year array
numtimes=n_elements(julian_day_1d)
caldat,julian_day_1d,mm,dd,yy,hh,mi,ss
doy=make_array(/int,numtimes,value=-9999)
for i=0,n_elements(julian_day_1d)-1 do begin
  ;print,yy[i],mm[i],dd[i],hh[i],mi[i],ss[i]
  julian_date,yy[i],mm[i],dd[i],doy1
  doy[i]=doy1
endfor

;  String of the time range
time_range_str=string(yy[0],format='(I4)')+string(mm[0],format='(I02)')+string(dd[0],format='(I02)')+'-'+$
  string(yy[-1],format='(I4)')+string(mm[-1],format='(I02)')+string(dd[-1],format='(I02)')

;  Name of output file for the time range
output_file=eos+'.histograms.'+time_range_str+'.cdf'
print,output_file

;  If the output file does not exist, then create it
if file_test(output_file) eq 0 or $
   file_test(output_file) eq 1 then begin

;if file_test(output_file) eq 0 then begin

  print,'making output file'

  ;  Now get the data for the time range
  all_mean_re=!NULL
  j=0
  ;  Loop through the days
  for i=0,numtimes-1 do begin
    
    ;  Get a list of histogram files for the day
    fdir=hdir+string(yy[i],format='(I4)')+'/'+string(doy[i],format='(I03)')+'/'
    if eos eq 'MOYD' then begin
      file_str='*06_L2.A*_histo.cdf'
    endif else begin
      file_str=eos+'06_L2.A*_histo.cdf'
    endelse
    files=file_search(fdir+file_str,count=num_files)
    print,num_files,'num_files'
    
    ;  If there are histogram files for the day
    if num_files gt 0 then begin
      
      ;  Make arrays to hold the values for each histogram
      num_traj=121
      julian_day=make_array(/double,num_files,value=-9999)
      center_lat=make_array(/float,num_files,value=-9999)
      center_lon=make_array(/float,num_files,value=-9999)
      count_1km=make_array(/float,num_files,value=-9999)
      count_5km=make_array(/float,num_files,value=-9999)
      mean_nd=make_array(/float,num_files,value=-9999)
      mean_re=make_array(/float,num_files,value=-9999)
      mean_lwp=make_array(/float,num_files,value=-9999)
      mean_tau=make_array(/float,num_files,value=-9999)
      mean_lat=make_array(/float,num_files,value=-9999)
      mean_lon=make_array(/float,num_files,value=-9999)
      mean_temp=make_array(/float,num_files,value=-9999)
      median_temp=make_array(/float,num_files,value=-9999)
      tenth_temp=make_array(/float,num_files,value=-9999)
      cold_flag=make_array(/float,num_files,value=-9999)
      chlor_a=make_array(/float,num_files,value=-9999)
      chlor_a_lat=make_array(/float,num_files,value=-9999)
      chlor_a_lon=make_array(/float,num_files,value=-9999)
      julian_day_traj=make_array(/float,num_files,num_traj,value=-9999)
      lat_traj=make_array(/float,num_files,num_traj,value=-9999)
      lon_traj=make_array(/float,num_files,num_traj,value=-9999)
      mean_sza=make_array(/float,num_files,value=-9999)
      mean_sw_up=make_array(/float,num_files,value=-9999)
      mean_sw_toa=make_array(/float,num_files,value=-9999)
      filename=make_array(/string,num_files)
  
      if all_mean_re eq !NULL then begin
        all_julian_day=julian_day
        all_center_lat=center_lat
        all_center_lon=center_lon
        all_count_1km=count_1km
        all_count_5km=count_5km
        all_mean_nd=mean_nd
        all_mean_re=mean_re
        all_mean_lwp=mean_lwp
        all_mean_tau=mean_tau
        all_mean_lat=mean_lat
        all_mean_lon=mean_lon
        all_mean_temp=mean_temp
        all_median_temp=median_temp
        all_tenth_temp=tenth_temp
        all_cold_flag=cold_flag
        all_chlor_a=chlor_a
        all_chlor_a_lat=chlor_a_lat
        all_chlor_a_lon=chlor_a_lon
        all_julian_day_traj=julian_day_traj
        all_lat_traj=lat_traj
        all_lon_traj=lon_traj
        all_num_histograms=num_files
        all_mean_sza=mean_sza
        all_mean_sw_up=mean_sw_up
        all_mean_sw_toa=mean_sw_toa
        all_filename=filename        
      endif else begin
        all_julian_day=[all_julian_day,julian_day]
        all_center_lat=[all_center_lat,center_lat]
        all_center_lon=[all_center_lon,center_lon]
        all_count_1km=[all_count_1km,count_1km]
        all_count_5km=[all_count_5km,count_5km]
        all_mean_nd=[all_mean_nd,mean_nd]
        all_mean_re=[all_mean_re,mean_re]
        all_mean_lwp=[all_mean_lwp,mean_lwp]
        all_mean_tau=[all_mean_tau,mean_tau]
        all_mean_lat=[all_mean_lat,mean_lat]
        all_mean_lon=[all_mean_lon,mean_lon]
        all_mean_temp=[all_mean_temp,mean_temp]
        all_median_temp=[all_median_temp,median_temp]
        all_tenth_temp=[all_tenth_temp,tenth_temp]
        all_cold_flag=[all_cold_flag,cold_flag]
        all_chlor_a=[all_chlor_a,chlor_a]
        all_chlor_a_lat=[all_chlor_a_lat,chlor_a_lat]
        all_chlor_a_lon=[all_chlor_a_lon,chlor_a_lon]
        all_julian_day_traj=[all_julian_day_traj,julian_day_traj]
        all_lat_traj=[all_lat_traj,lat_traj]
        all_lon_traj=[all_lon_traj,lon_traj]
        all_num_histograms=[all_num_histograms,num_files]
        all_mean_sza=[all_mean_sza,mean_sza]
        all_mean_sw_up=[all_mean_sw_up,mean_sw_up]
        all_mean_sw_toa=[all_mean_sw_toa,mean_sw_toa]
        all_filename=[all_filename,filename]
      endelse

      for f=0,num_files-1 do begin
        if f mod 200 eq 0 then print,files[f]
        fid=ncdf_open(files[f])
        vid=ncdf_varid(fid,'julian_day') & ncdf_varget,fid,vid,julian_day1
        ;vid=ncdf_varid(fid,'nd_bins') & ncdf_varget,fid,vid,nd_bins
        ;vid=ncdf_varid(fid,'nd_histo') & ncdf_varget,fid,vid,nd_histo
        ;vid=ncdf_varid(fid,'re_bins') & ncdf_varget,fid,vid,re_bins
        ;vid=ncdf_varid(fid,'re_histo') & ncdf_varget,fid,vid,re_histo
        ;vid=ncdf_varid(fid,'lwp_bins') & ncdf_varget,fid,vid,lwp_bins
        ;vid=ncdf_varid(fid,'lwp_histo') & ncdf_varget,fid,vid,lwp_histo
        ;vid=ncdf_varid(fid,'solar_zenith_bins') & ncdf_varget,fid,vid,solar_zenith_bins
        ;vid=ncdf_varid(fid,'solar_zenith_histo') & ncdf_varget,fid,vid,solar_zenith_histo
        ;vid=ncdf_varid(fid,'view_zenith_bins') & ncdf_varget,fid,vid,view_zenith_bins
        ;vid=ncdf_varid(fid,'view_zenith_histo') & ncdf_varget,fid,vid,view_zenith_histo
        vid=ncdf_varid(fid,'center_latitude') & ncdf_varget,fid,vid,center_lat1
        vid=ncdf_varid(fid,'center_longitude') & ncdf_varget,fid,vid,center_lon1
        vid=ncdf_varid(fid,'count_5km') & ncdf_varget,fid,vid,count_5km1
        vid=ncdf_varid(fid,'count_1km') & ncdf_varget,fid,vid,count_1km1
        vid=ncdf_varid(fid,'mean_latitude') & ncdf_varget,fid,vid,mean_lat1
        vid=ncdf_varid(fid,'mean_longitude') & ncdf_varget,fid,vid,mean_lon1
        vid=ncdf_varid(fid,'mean_nd') & ncdf_varget,fid,vid,mean_nd1
        vid=ncdf_varid(fid,'mean_re') & ncdf_varget,fid,vid,mean_re1
        vid=ncdf_varid(fid,'mean_lwp') & ncdf_varget,fid,vid,mean_lwp1
        vid=ncdf_varid(fid,'mean_cloud_top_temp') & ncdf_varget,fid,vid,mean_temp1
        vid=ncdf_varid(fid,'median_cloud_top_temp') & ncdf_varget,fid,vid,median_temp1
        vid=ncdf_varid(fid,'tenth_percentile_cloud_top_temp') & ncdf_varget,fid,vid,tenth_temp1
        ;vid=ncdf_varid(fid,'mode_cloud_phase') & ncdf_varget,fid,vid,mode_cloud_phase1
        vid=ncdf_varid(fid,'cold_flag') & ncdf_varget,fid,vid,cold_flag1
        vid=ncdf_varid(fid,'mean_solar_zenith') & ncdf_varget,fid,vid,mean_sza1
        vid=ncdf_varid(fid,'mean_tau') & ncdf_varget,fid,vid,mean_tau1
        vid=ncdf_varid(fid,'mean_sw_up') 
        if vid ne -1 then begin
          ncdf_varget,fid,vid,mean_sw_up1
          vid=ncdf_varid(fid,'mean_sw_toa') & ncdf_varget,fid,vid,mean_sw_toa1
          rr=where(mean_sw_toa1 lt 0 and mean_sw_toa1  ne -9999,cc)
          if cc gt 0 then stop
        endif else begin
          mean_sw_up1=-9999
          mean_sw_toa1=-9999
        endelse
        vid=ncdf_varid(fid,'L3m_8D_CHL_chlor_a_4km') & ncdf_varget,fid,vid,chlor_a1
        vid=ncdf_varid(fid,'L3m_8D_CHL_chlor_a_4km_lat') & ncdf_varget,fid,vid,chlor_a_lat1
        vid=ncdf_varid(fid,'L3m_8D_CHL_chlor_a_4km_lon') & ncdf_varget,fid,vid,chlor_a_lon1
        vid=ncdf_varid(fid,'julian_day_traj')
        if vid ne -1 then begin
          ncdf_varget,fid,vid,julian_day_traj1
          vid=ncdf_varid(fid,'lat_traj') & ncdf_varget,fid,vid,lat_traj1
          vid=ncdf_varid(fid,'lon_traj') & ncdf_varget,fid,vid,lon_traj1
          all_julian_day_traj[j+f,*]=julian_day_traj1
          all_lat_traj[j+f,*]=lat_traj1
          all_lon_traj[j+f,*]=lon_traj1
        endif  
        ncdf_close,fid
  
        all_julian_day[j+f]=julian_day1
        all_center_lat[j+f]=center_lat1
        all_center_lon[j+f]=center_lon1
        all_count_1km[j+f]=count_1km1
        all_count_5km[j+f]=count_5km1
        all_mean_nd[j+f]=mean_nd1
        all_mean_re[j+f]=mean_re1
        all_mean_lwp[j+f]=mean_lwp1
        all_mean_tau[j+f]=mean_tau1
        all_mean_lat[j+f]=mean_lat1
        all_mean_lon[j+f]=mean_lon1
  
        all_mean_temp[j+f]=mean_temp1
        all_median_temp[j+f]=median_temp1
        all_tenth_temp[j+f]=tenth_temp1
        all_cold_flag[j+f]=cold_flag1  
        all_chlor_a[j+f]=chlor_a1 
        all_chlor_a_lat[j+f]=chlor_a_lat1
        all_chlor_a_lon[j+f]=chlor_a_lon1
        all_mean_sza[j+f]=mean_sza1
        all_mean_sw_up[j+f]=mean_sw_up1
        all_mean_sw_toa[j+f]=mean_sw_toa1
        all_filename[j+f]=file_basename(files[f])
      endfor
      j=j+f
    endif  ;found files for this day
  endfor  ;end of loop through days
  num_hists=n_elements(all_julian_day)
  num_files=n_elements(all_num_histograms)

  ;  Calculate scattering parameter - not good enough albedo
  ;all_mean_g=make_array(n_elements(all_julian_day),/float,value=-9999)
  ;wl=0.55
  ;for i=0,n_elements(all_mean_g)-1 do begin
  ;  lwp=all_mean_lwp[i]
  ;  re=all_mean_re[i]
  ;  tau=all_mean_tau[i]
  ;  ccm3_radiative_param_shortwave, wl, lwp, re, tau, omega, g
  ;  all_mean_g[i]=g
  ;endfor
  ; Platnick and Twomey (1994; journal of applied meteorology, pg 334)
  ;all_mean_sw_alb_eqn=((1.-all_mean_g)*all_mean_tau)/(2.+((1.-all_mean_g)*all_mean_tau))

  ;  Ceres direct measurment albedo
  all_mean_sw_alb=all_mean_sw_up/all_mean_sw_toa
  ; Minnis albedo parametarization
  mls_minnis_albedo,result,const
  plot_mu0=cos(all_mean_sza*!dtor)
  regress_alb=const+(result[0]*(sqrt(plot_mu0)))+(result[1]*(alog(all_mean_tau)))
  plot_mu0=cos(45.0*!dtor)
  regress_alb_45=const+(result[0]*(sqrt(plot_mu0)))+(result[1]*(alog(all_mean_tau)))
  ;  Ratio
  ratio=regress_alb_45/regress_alb
  all_mean_sw_nalb=ratio*all_mean_sw_alb
  ;  Put missing flag in albedo values
  r=where(all_mean_sw_up eq -9999,c)
  if c gt 0 then begin
    all_mean_sw_alb[r]=-9999
    all_mean_sw_nalb[r]=-9999
  endif
  ;  Small tau gives a neg for alog(all_mean_tau)
  ;r=where(all_mean_sw_alb gt 1.0 or (all_mean_sw_alb lt 0.0 and all_mean_sw_alb ne -9999),c)
  ;if c gt 0 then begin
  ;  print,all_mean_sw_alb[r]
  ;  stop
  ;endif
  
  ; write the data into a file
  cdfid=ncdf_create(output_file,/clobber)
  num_did=ncdf_dimdef(cdfid,'num',num_hists)
  traj_did=ncdf_dimdef(cdfid,'traj',num_traj)
  num_files_did=ncdf_dimdef(cdfid,'num_files',num_files)
  julian_day_id=ncdf_vardef(cdfid,'julian_day',num_did,/double)
  center_lat_id=ncdf_vardef(cdfid,'center_latitude',num_did,/float)
  center_lon_id=ncdf_vardef(cdfid,'center_longitude',num_did,/float)  
  count_1km_id=ncdf_vardef(cdfid,'count_1km',num_did,/float)
  count_5km_id=ncdf_vardef(cdfid,'count_5km',num_did,/float)
  mean_lat_id=ncdf_vardef(cdfid,'mean_latitude',num_did,/float)
  mean_lon_id=ncdf_vardef(cdfid,'mean_longitude',num_did,/float)
  mean_nd_id=ncdf_vardef(cdfid,'mean_nd',num_did,/float)
  mean_re_id=ncdf_vardef(cdfid,'mean_re',num_did,/float)
  mean_lwp_id=ncdf_vardef(cdfid,'mean_lwp',num_did,/float)
  mean_tau_id=ncdf_vardef(cdfid,'mean_tau',num_did,/float)
  mean_temp_id=ncdf_vardef(cdfid,'mean_cloud_top_temp',num_did,/float)
  median_temp_id=ncdf_vardef(cdfid,'median_cloud_top_temp',num_did,/float)
  tenth_temp_id=ncdf_vardef(cdfid,'tenth_percentile_cloud_top_temp',num_did, /float)
  cold_flag_id=ncdf_vardef(cdfid,'cold_flag',num_did,/short)
  chlor_a_id=ncdf_vardef(cdfid,'chlor_a',num_did,/float)
  chlor_a_lat_id=ncdf_vardef(cdfid,'chlor_a_lat',num_did,/float)
  chlor_a_lon_id=ncdf_vardef(cdfid,'chlor_a_lon',num_did,/float)
  mean_sza_id=ncdf_vardef(cdfid,'mean_solar_zenith',num_did,/float)
  mean_sw_up_id=ncdf_vardef(cdfid,'mean_sw_up',num_did,/float)
  mean_sw_toa_id=ncdf_vardef(cdfid,'mean_sw_toa',num_did,/float)
  mean_sw_alb_id=ncdf_vardef(cdfid,'mean_sw_alb',num_did,/float)
  mean_sw_nalb_id=ncdf_vardef(cdfid,'mean_sw_nalb',num_did,/float)
  julian_day_traj_id=ncdf_vardef(cdfid,'julian_day_traj',[num_did,traj_did],/float)
  lat_traj_id=ncdf_vardef(cdfid,'lat_traj',[num_did,traj_did],/float)
  lon_traj_id=ncdf_vardef(cdfid,'lon_traj',[num_did,traj_did],/float)
  num_histograms_id=ncdf_vardef(cdfid,'num_histograms',num_files_did,/float)
  ncdf_control, cdfid, /endef

  ncdf_varput,cdfid,julian_day_id,all_julian_day
  ncdf_varput,cdfid,center_lat_id,all_center_lat
  ncdf_varput,cdfid,center_lon_id,all_center_lon
  ncdf_varput,cdfid,count_1km_id,all_count_1km
  ncdf_varput,cdfid,count_5km_id,all_count_5km
  ncdf_varput,cdfid,mean_lat_id,all_mean_lat
  ncdf_varput,cdfid,mean_lon_id,all_mean_lon
  ncdf_varput,cdfid,mean_nd_id,all_mean_nd
  ncdf_varput,cdfid,mean_re_id,all_mean_re
  ncdf_varput,cdfid,mean_lwp_id,all_mean_lwp
  ncdf_varput,cdfid,mean_tau_id,all_mean_tau
  ncdf_varput,cdfid,mean_temp_id,all_mean_temp
  ncdf_varput,cdfid,median_temp_id,all_median_temp
  ncdf_varput,cdfid,tenth_temp_id,all_tenth_temp
  ncdf_varput,cdfid,cold_flag_id,all_cold_flag
  ncdf_varput,cdfid,chlor_a_id,all_chlor_a
  ncdf_varput,cdfid,chlor_a_lat_id,all_chlor_a_lat
  ncdf_varput,cdfid,chlor_a_lon_id,all_chlor_a_lon
  ncdf_varput,cdfid,mean_sza_id,all_mean_sza
  ncdf_varput,cdfid,mean_sw_up_id,all_mean_sw_up
  ncdf_varput,cdfid,mean_sw_toa_id,all_mean_sw_toa
  ncdf_varput,cdfid,mean_sw_alb_id,all_mean_sw_alb
  ncdf_varput,cdfid,mean_sw_nalb_id,all_mean_sw_nalb
  ncdf_varput,cdfid,julian_day_traj_id,all_julian_day_traj
  ncdf_varput,cdfid,lat_traj_id,all_lat_traj   
  ncdf_varput,cdfid,lon_traj_id,all_lon_traj   
  ncdf_varput,cdfid,num_histograms_id,all_num_histograms

  ncdf_close, cdfid
  rr=where(all_mean_sw_toa lt 0 and all_mean_sw_toa ne -9999,cc)
  print,cc,'sw toa lt 0'
;  Read previously created output file
endif else begin
  print,'found output file'
  fid=ncdf_open(output_file)
  
  vid=ncdf_varid(fid,'julian_day') & ncdf_varget,fid,vid,all_julian_day
  vid=ncdf_varid(fid,'count_1km') & ncdf_varget,fid,vid,all_count_1km
  vid=ncdf_varid(fid,'count_5km') & ncdf_varget,fid,vid,all_count_5km
  vid=ncdf_varid(fid,'center_latitude') & ncdf_varget,fid,vid,all_center_lat
  vid=ncdf_varid(fid,'center_longitude') & ncdf_varget,fid,vid,all_center_lon
  vid=ncdf_varid(fid,'mean_latitude') & ncdf_varget,fid,vid,all_mean_lat
  vid=ncdf_varid(fid,'mean_longitude') & ncdf_varget,fid,vid,all_mean_lon
  vid=ncdf_varid(fid,'mean_nd') & ncdf_varget,fid,vid,all_mean_nd
  vid=ncdf_varid(fid,'mean_re') & ncdf_varget,fid,vid,all_mean_re
  vid=ncdf_varid(fid,'mean_lwp') & ncdf_varget,fid,vid,all_mean_lwp
  vid=ncdf_varid(fid,'mean_tau') & ncdf_varget,fid,vid,all_mean_tau
  vid=ncdf_varid(fid,'mean_solar_zenith') & ncdf_varget,fid,vid,all_mean_sza
  vid=ncdf_varid(fid,'mean_cloud_top_temp') & ncdf_varget,fid,vid,all_mean_temp
  vid=ncdf_varid(fid,'median_cloud_top_temp') & ncdf_varget,fid,vid,all_median_temp
  vid=ncdf_varid(fid,'tenth_percentile_cloud_top_temp') & ncdf_varget,fid,vid,all_tenth_temp
  vid=ncdf_varid(fid,'cold_flag') & ncdf_varget,fid,vid,all_cold_flag
  vid=ncdf_varid(fid,'chlor_a') & ncdf_varget,fid,vid,all_chlor_a
  vid=ncdf_varid(fid,'chlor_a_lat') & ncdf_varget,fid,vid,all_chlor_a_lat
  vid=ncdf_varid(fid,'chlor_a_lon') & ncdf_varget,fid,vid,all_chlor_a_lon
  vid=ncdf_varid(fid,'mean_solar_zenith') & ncdf_varget,fid,vid,all_mean_sza
  vid=ncdf_varid(fid,'mean_sw_up') & ncdf_varget,fid,vid,all_mean_sw_up
  vid=ncdf_varid(fid,'mean_sw_toa') & ncdf_varget,fid,vid,all_mean_sw_toa
  vid=ncdf_varid(fid,'mean_sw_alb') & ncdf_varget,fid,vid,all_mean_sw_alb
  vid=ncdf_varid(fid,'mean_sw_nalb') & ncdf_varget,fid,vid,all_mean_sw_nalb
  vid=ncdf_varid(fid,'julian_day_traj') & ncdf_varget,fid,vid,all_julian_day_traj
  vid=ncdf_varid(fid,'lat_traj') & ncdf_varget,fid,vid,all_lat_traj
  vid=ncdf_varid(fid,'lon_traj') & ncdf_varget,fid,vid,all_lon_traj
  vid=ncdf_varid(fid,'num_histograms') & ncdf_varget,fid,vid,all_num_histograms
  ncdf_close,fid
endelse

all_mean_colon=all_mean_lon
r=where(all_mean_lon gt 180,c)
if c gt 0 then all_mean_lon[r]=all_mean_lon[r]-360.0

all_colon_traj=all_lon_traj
r=where(all_lon_traj gt 180,c)
if c gt 0 then all_lon_traj[r]=all_lon_traj[r]-360.0

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
;  Cloud albedo Plots
;*************************
;  Do this so I can plot regression albedos and ratio
mls_minnis_albedo,result,const
plot_mu0=cos(all_mean_sza*!dtor)
regress_alb=const+(result[0]*(sqrt(plot_mu0)))+(result[1]*(alog(all_mean_tau)))
plot_mu0=cos(45.0*!dtor)
regress_alb_45=const+(result[0]*(sqrt(plot_mu0)))+(result[1]*(alog(all_mean_tau)))
;  Ratio
ratio=regress_alb_45/regress_alb
;  HI LO indexes
hi_idx=where(all_mean_nd ge 100.0,hi_count)
lo_idx=where(all_mean_nd le 50.0,lo_count)

pxdim=1000 & pydim=1000
xl=0.08 & xr=0.90
yb=0.10 & yt=0.90
sx=0.16
sy=0.13
numplots_x=3
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

;  1D Regressed albedo minnis fig 7
if 1 eq 1 then begin
  pnum=0                  
  r=where(all_mean_tau ge 7.7 and all_mean_tau le 8.3,c)
  yvar1=regress_alb[r]
  xvar1=cos(all_mean_sza[r]*!dtor)
  p1=plot(xvar1,yvar1,/current,position=pos[pnum,*],linestyle=6,symbol='o',/sym_filled,sym_size=0.3,color='blue',$
    xrange=[0,1],yrange=[0,1],xtitle='Cosine of the Solar Zenith Angle',ytitle='Regression Albedo',font_size=fs1)
  t1=text(pos[pnum,0],pos[pnum,3]+5.0*dy,'tau=8',color='blue')
  r=where(all_mean_tau ge 0.2 and all_mean_tau le 0.8,c)
  if c gt 0 then begin
    yvar1=regress_alb[r]
    xvar1=cos(all_mean_sza[r]*!dtor)
    p1=plot(xvar1,yvar1,/overplot,linestyle=6,symbol='o',/sym_filled,sym_size=0.3,color='orange')
    t1=text(pos[pnum,0],pos[pnum,3]+1.0*dy,'tau=0.5',color='orange')
  endif
  r=where(all_mean_tau ge 1.7 and all_mean_tau le 2.3,c)  
  if c gt 0 then begin
    yvar1=regress_alb[r]
    xvar1=cos(all_mean_sza[r]*!dtor) 
    p1=plot(xvar1,yvar1,/overplot,linestyle=6,symbol='o',/sym_filled,sym_size=0.3,color='black')
    t1=text(pos[pnum,0],pos[pnum,3]+3.0*dy,'tau=2',color='black')
  endif  
  r=where(all_mean_tau ge 31.7 and all_mean_tau lt 50.0,c)
  if c gt 0 then begin
    yvar1=regress_alb[r]
    xvar1=cos(all_mean_sza[r]*!dtor)
    p1=plot(xvar1,yvar1,/overplot,linestyle=6,symbol='o',/sym_filled,sym_size=0.3,color='red')
    t1=text(pos[pnum,0],pos[pnum,3]+7.0*dy,'tau=32',color='red')
  endif
  r=where(all_mean_tau ge 63.7,c)
  if c gt 0 then begin
    yvar1=regress_alb[r]
    xvar1=cos(all_mean_sza[r]*!dtor)
    p1=plot(xvar1,yvar1,/overplot,linestyle=6,symbol='o',/sym_filled,sym_size=0.3,color='green')
    t1=text(pos[pnum,0],pos[pnum,3]+9.0*dy,'tau=64',color='green')
  endif
endif

;  1D solar zenith angle
if 1 eq 1 then begin
  pnum=1
  data_var=all_mean_sza
  start_bin=min(all_mean_sza)
  end_bin=max(all_mean_sza)
  dbin=(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xtitle='Solar Zenith Angle',ytitle='Frequency',font_size=fs1,/hist,/ystyle,thick=2)
  data_var=all_mean_sza[hi_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='purple')
  data_var=all_mean_sza[lo_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='green')
  t1=text(pos[pnum,0],pos[pnum,3]+1.0*dy,'High Nd',color='purple')
  t1=text(pos[pnum,0],pos[pnum,3]+3.0*dy,'Low Nd',color='green')
endif

;  1D tau
if 1 eq 1 then begin
  pnum=2
  data_var=all_mean_tau
  start_bin=min(all_mean_tau)
  end_bin=max(all_mean_tau)
  dbin=(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xtitle='Tau',ytitle='Frequency',font_size=fs1,/hist,/ystyle,thick=2)
  data_var=all_mean_tau[hi_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='purple')
  data_var=all_mean_tau[lo_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,/hist,color='green')
  t1=text(pos[pnum,0],pos[pnum,3]+1.0*dy,'High Nd',color='purple')
  t1=text(pos[pnum,0],pos[pnum,3]+3.0*dy,'Low Nd',color='green')
endif

;  1D albedo
if 1 eq 1 then begin
  pnum=3
  all_alb=[all_mean_sw_alb,regress_alb,regress_alb_45,all_mean_sw_nalb]
  r=where(all_alb ne -9999)
  start_bin=min(all_alb[r])
  end_bin=max(all_alb)
  dbin=(end_bin-start_bin)/100.0
  data_var=all_mean_sw_alb
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xtitle='SW Albedo',ytitle='Frequency',font_size=fs1,/hist,/ystyle,thick=2)
  data_var=regress_alb
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/overplot,/hist,color='red')
  data_var=regress_alb_45
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/overplot,/hist,color='blue')
  data_var=all_mean_sw_nalb
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/overplot,/hist,color='orange')
  t1=text(pos[pnum,0],pos[pnum,3]+1.0*dy,'Regress Albedo',color='red')
  t1=text(pos[pnum,0],pos[pnum,3]+3.0*dy,'Regress Albedo @ 45deg',color='blue') 
  t1=text(pos[pnum,0],pos[pnum,3]+5.0*dy,'SW Albedo (ceres)',color='black')  
  t1=text(pos[pnum,0],pos[pnum,3]+7.0*dy,'SW Albedo (ceres) * ratio',color='orange')  
endif

;  1D ratio
if 1 eq 1 then begin
  pnum=4
  data_var=ratio
  start_bin=min(data_var)
  end_bin=max(data_var)
  dbin=(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xtitle='Ratio',ytitle='Frequency',font_size=fs1,/hist,/ystyle)
  t1=text(pos[pnum,0],pos[pnum,3]+1.0*dy,'ratio=(regress @ 45) / (regress at SZA)',color='black')
endif

;  1D albedo HIGH Nd
if 1 eq 1 then begin
  pnum=6
  all_alb=[all_mean_sw_alb,regress_alb,regress_alb_45,all_mean_sw_nalb]
  r=where(all_alb ne -9999)
  start_bin=min(all_alb[r])
  end_bin=max(all_alb)
  dbin=(end_bin-start_bin)/100.0
  data_var=all_mean_sw_alb[hi_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xtitle='SW Albedo High Nd',ytitle='Frequency',font_size=fs1,/hist,/ystyle,thick=2)
  data_var=regress_alb[hi_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/overplot,/hist,color='red')
  data_var=regress_alb_45[hi_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/overplot,/hist,color='blue')
  data_var=all_mean_sw_nalb[hi_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/overplot,/hist,color='orange')
  t1=text(pos[pnum,0],pos[pnum,3]+1.0*dy,'Regress Albedo',color='red')
  t1=text(pos[pnum,0],pos[pnum,3]+3.0*dy,'Regress Albedo @ 45deg',color='blue')
  t1=text(pos[pnum,0],pos[pnum,3]+5.0*dy,'SW Albedo (ceres)',color='black')
  t1=text(pos[pnum,0],pos[pnum,3]+7.0*dy,'SW Albedo (ceres) * ratio',color='orange')
endif

;  1D albedo HIGH Nd
if 1 eq 1 then begin
  pnum=7
  all_alb=[all_mean_sw_alb,regress_alb,regress_alb_45,all_mean_sw_nalb]
  r=where(all_alb ne -9999)
  start_bin=min(all_alb[r])
  end_bin=max(all_alb)
  dbin=(end_bin-start_bin)/100.0
  data_var=all_mean_sw_alb[lo_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xtitle='SW Albedo Low Nd',ytitle='Frequency',font_size=fs1,/hist,/ystyle,thick=2)
  data_var=regress_alb[lo_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/overplot,/hist,color='red')
  data_var=regress_alb_45[lo_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/overplot,/hist,color='blue')
  data_var=all_mean_sw_nalb[lo_idx]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/overplot,/hist,color='orange')
  t1=text(pos[pnum,0],pos[pnum,3]+1.0*dy,'Regress Albedo',color='red')
  t1=text(pos[pnum,0],pos[pnum,3]+3.0*dy,'Regress Albedo @ 45deg',color='blue')
  t1=text(pos[pnum,0],pos[pnum,3]+5.0*dy,'SW Albedo (ceres)',color='black')
  t1=text(pos[pnum,0],pos[pnum,3]+7.0*dy,'SW Albedo (ceres) * ratio',color='orange')
endif

imagename=eos+'.albedo1d.'+time_range_str+'.png'
p0.save,imagename;,height=pydim
  
;  2D Tau vs SZA
if 1 eq 1 then begin
  pnum=0
  x_data=all_mean_tau
  y_data=all_mean_sza
  x_start_bin=min(x_data)
  x_end_bin=max(x_data)
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_start_bin=min(y_data)
  y_end_bin=max(y_data)
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  dmin=min(data_counts)  
  dmax=max(data_counts)
  var_image=bytscl(data_counts,min=dmin,max=dmax,top=top_color)
  r=where(data_counts eq 0,c)
  if c gt 0 then var_image[r]=253
  p0=image(var_image,x_bins,y_bins,/buffer,position=pos[pnum,*],$
    dimensions=[pxdim,pydim],$
    rgb_table=mytable,image_dimensions=[isx,isy],font_size=fs1)
  c0=contour(data_counts,x_bins,y_bins,/nodata,position=pos[pnum,*],$
    ytitle='Solar Zenith Angle',/current,xstyle=1,ystyle=1,xtickdir=1,$
    ytickdir=1,font_size=fs1,xtitle='Tau',axis_style=2)
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin,dmax])
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'total cases='+string(total(data_counts)),font_size=fs1)
endif

;  2D SW Albedo vs SZA
if 1 eq 1 then begin
  pnum=1
  x_data=all_mean_sza
  y_data=all_mean_sw_alb
  x_start_bin=min(x_data)
  x_end_bin=max(x_data)
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_start_bin=0.;min(y_data)
  y_end_bin=1.;max(y_data)
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  dmin=min(data_counts)
  dmax=max(data_counts)
  var_image=bytscl(data_counts,min=dmin,max=dmax,top=top_color)
  r=where(data_counts eq 0,c)
  if c gt 0 then var_image[r]=253
  p0=image(var_image,x_bins,y_bins,/current,position=pos[pnum,*],$
    rgb_table=mytable,image_dimensions=[isx,isy],font_size=fs1)
  c0=contour(data_counts,x_bins,y_bins,/nodata,position=pos[pnum,*],$
    ytitle='SW Albedo (ceres)',/current,xstyle=1,ystyle=1,xtickdir=1,$
    ytickdir=1,font_size=fs1,xtitle='Solar Zenith Angle',axis_style=2)
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin,dmax])
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'total cases='+string(total(data_counts)),font_size=fs1)
endif

;  2D ratio Albedo vs SZA
if 1 eq 1 then begin
  pnum=2
  x_data=all_mean_sza
  y_data=all_mean_sw_nalb
  x_start_bin=min(x_data)
  x_end_bin=max(x_data)
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_start_bin=0.;min(y_data)
  y_end_bin=1.;max(y_data)
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
   y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  dmin=min(data_counts)
  dmax=max(data_counts)
  var_image=bytscl(data_counts,min=dmin,max=dmax,top=top_color)
  r=where(data_counts eq 0,c)
  if c gt 0 then var_image[r]=253
  p0=image(var_image,x_bins,y_bins,/current,position=pos[pnum,*],$
    rgb_table=mytable,image_dimensions=[isx,isy],font_size=fs1)
  c0=contour(data_counts,x_bins,y_bins,/nodata,position=pos[pnum,*],$
    ytitle='SW Albedo (ceres) * ratio',/current,xstyle=1,ystyle=1,xtickdir=1,$
    ytickdir=1,font_size=fs1,xtitle='Solar Zenith Angle',axis_style=2)
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin,dmax])
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'total cases='+string(total(data_counts)),font_size=fs1)
endif

;  2D regress Albedo vs SZA
if 1 eq 1 then begin
  pnum=3
  x_data=all_mean_sza
  y_data=regress_alb
  x_start_bin=min(x_data)
  x_end_bin=max(x_data)
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_start_bin=0.;min(y_data)
  y_end_bin=1.;max(y_data)
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  dmin=min(data_counts)
  dmax=max(data_counts)
  var_image=bytscl(data_counts,min=dmin,max=dmax,top=top_color)
  r=where(data_counts eq 0,c)
  if c gt 0 then var_image[r]=253
  p0=image(var_image,x_bins,y_bins,/current,position=pos[pnum,*],$
    rgb_table=mytable,image_dimensions=[isx,isy],font_size=fs1)
  c0=contour(data_counts,x_bins,y_bins,/nodata,position=pos[pnum,*],$
    ytitle='Regress Albedo',/current,xstyle=1,ystyle=1,xtickdir=1,$
    ytickdir=1,font_size=fs1,xtitle='Solar Zenith Angle',axis_style=2)
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin,dmax])
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'total cases='+string(total(data_counts)),font_size=fs1)
endif

;  2D SW Albedo vs regression albedo
if 1 eq 1 then begin
  pnum=4
  x_data=all_mean_sw_alb
  y_data=regress_alb
  x_start_bin=0.0;min(x_data)
  x_end_bin=1.0;max(x_data)
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_start_bin=0.;min(y_data)
  y_end_bin=1.;max(y_data)
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  dmin=min(data_counts)
  dmax=max(data_counts)
  var_image=bytscl(data_counts,min=dmin,max=dmax,top=top_color)
  r=where(data_counts eq 0,c)
  if c gt 0 then var_image[r]=253
  p0=image(var_image,x_bins,y_bins,/current,position=pos[pnum,*],$
    rgb_table=mytable,image_dimensions=[isx,isy],font_size=fs1)
  c0=contour(data_counts,x_bins,y_bins,/nodata,position=pos[pnum,*],$
    ytitle='Regress Albedo',/current,xstyle=1,ystyle=1,xtickdir=1,$
    ytickdir=1,font_size=fs1,xtitle='SW Albedo',axis_style=2)
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin,dmax])
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'total cases='+string(total(data_counts)),font_size=fs1)
endif

;  2D Tau vs SW Albedo 
if 1 eq 1 then begin
  pnum=5
  x_data=all_mean_tau
  y_data=all_mean_sw_alb
  x_start_bin=min(x_data)
  x_end_bin=max(x_data)
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_start_bin=0.;min(y_data)
  y_end_bin=1.;max(y_data)
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  dmin=min(data_counts)
  dmax=max(data_counts)
  var_image=bytscl(data_counts,min=dmin,max=dmax,top=top_color)
  r=where(data_counts eq 0,c)
  if c gt 0 then var_image[r]=253
  p0=image(var_image,x_bins,y_bins,/current,position=pos[pnum,*],$
    rgb_table=mytable,image_dimensions=[isx,isy],font_size=fs1)
  c0=contour(data_counts,x_bins,y_bins,/nodata,position=pos[pnum,*],$
    ytitle='SW Albedo',/current,xstyle=1,ystyle=1,xtickdir=1,$
    ytickdir=1,font_size=fs1,xtitle='Tau',axis_style=2)
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin,dmax])
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'total cases='+string(total(data_counts)),font_size=fs1)
endif

;  2D Tau vs Regress Albedo 
if 1 eq 1 then begin
  pnum=6
  x_data=all_mean_tau
  y_data=regress_alb
  x_start_bin=min(x_data)
  x_end_bin=max(x_data)
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_start_bin=0.;min(y_data)
  y_end_bin=1.;max(y_data)
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  dmin=min(data_counts)
  dmax=max(data_counts)
  var_image=bytscl(data_counts,min=dmin,max=dmax,top=top_color)
  r=where(data_counts eq 0,c)
  if c gt 0 then var_image[r]=253
  p0=image(var_image,x_bins,y_bins,/current,position=pos[pnum,*],$
    rgb_table=mytable,image_dimensions=[isx,isy],font_size=fs1)
  c0=contour(data_counts,x_bins,y_bins,/nodata,position=pos[pnum,*],$
    ytitle='Regress Albedo',/current,xstyle=1,ystyle=1,xtickdir=1,$
    ytickdir=1,font_size=fs1,xtitle='Tau',axis_style=2)
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin,dmax])
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'total cases='+string(total(data_counts)),font_size=fs1)
endif

;  2D Tau vs SZA HIGH ND
if 1 eq 1 then begin
  pnum=7
  x_data=all_mean_tau[hi_idx]
  y_data=all_mean_sza[hi_idx]
  x_start_bin=min(x_data)
  x_end_bin=max(x_data)
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_start_bin=min(y_data)
  y_end_bin=max(y_data)
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  dmin=min(data_counts)
  dmax=max(data_counts)
  var_image=bytscl(data_counts,min=dmin,max=dmax,top=top_color)
  r=where(data_counts eq 0,c)
  if c gt 0 then var_image[r]=253
  p0=image(var_image,x_bins,y_bins,/current,position=pos[pnum,*],$
    dimensions=[pxdim,pydim],$
    rgb_table=mytable,image_dimensions=[isx,isy],font_size=fs1)
  c0=contour(data_counts,x_bins,y_bins,/nodata,position=pos[pnum,*],$
    ytitle='Solar Zenith Angle (High)',/current,xstyle=1,ystyle=1,xtickdir=1,$
    ytickdir=1,font_size=fs1,xtitle='Tau (High)',axis_style=2)
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin,dmax])
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'total cases='+string(total(data_counts)),font_size=fs1)
endif

;  2D Tau vs SZA LOW ND cases
if 1 eq 0 then begin
  pnum=8
  x_data=all_mean_tau[lo_idx]
  y_data=all_mean_sza[lo_idx]
  x_start_bin=min(x_data)
  x_end_bin=max(x_data)
  x_dbin=(x_end_bin-x_start_bin)/100.0
  y_start_bin=min(y_data)
  y_end_bin=max(y_data)
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  dmin=min(data_counts)
  dmax=max(data_counts)
  var_image=bytscl(data_counts,min=dmin,max=dmax,top=top_color)
  r=where(data_counts eq 0,c)
  if c gt 0 then var_image[r]=253
  p0=image(var_image,x_bins,y_bins,/current,position=pos[pnum,*],$
    dimensions=[pxdim,pydim],$
    rgb_table=mytable,image_dimensions=[isx,isy],font_size=fs1)
  c0=contour(data_counts,x_bins,y_bins,/nodata,position=pos[pnum,*],$
    ytitle='Solar Zenith Angle (LOW)',/current,xstyle=1,ystyle=1,xtickdir=1,$
    ytickdir=1,font_size=fs1,xtitle='Tau (LOW)',axis_style=2)
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin,dmax])
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'total cases='+string(total(data_counts)),font_size=fs1)
endif

imagename=eos+'.albedo2d.'+time_range_str+'.png'
p0.save,imagename;,height=pydim
stop

;*************************
;   Calculate quartiles of nd
;*************************
  
nd_median=median(all_mean_nd)
nd_low_median=median(all_mean_nd[where(all_mean_nd le nd_median)])
nd_high_median=median(all_mean_nd[where(all_mean_nd gt nd_median)])

hi_idx=where(all_mean_nd ge nd_high_median,hi_count)
hi_mean_nd=all_mean_nd[hi_idx]
hi_mean_lwp=all_mean_lwp[hi_idx]
hi_mean_tau=all_mean_tau[hi_idx]
hi_julian_day=all_julian_day[hi_idx]
hi_mean_lat=all_mean_lat[hi_idx]
hi_mean_lon=all_mean_lon[hi_idx]
hi_chlor_a=all_chlor_a[hi_idx]
hi_mean_sw_up=all_mean_sw_up[hi_idx]
hi_mean_sw_toa=all_mean_sw_toa[hi_idx]
hi_mean_sw_alb=all_mean_sw_alb[hi_idx]
hi_mean_sw_alb_eqn=all_mean_sw_alb_eqn[hi_idx]

lo_idx=where(all_mean_nd le nd_low_median,lo_count)
lo_mean_nd=all_mean_nd[lo_idx]
lo_mean_lwp=all_mean_lwp[lo_idx]
lo_mean_tau=all_mean_tau[lo_idx]
lo_julian_day=all_julian_day[lo_idx]
lo_mean_lat=all_mean_lat[lo_idx]
lo_mean_lon=all_mean_lon[lo_idx]
lo_chlor_a=all_chlor_a[lo_idx]
lo_mean_sw_up=all_mean_sw_up[lo_idx]
lo_mean_sw_toa=all_mean_sw_toa[lo_idx]
lo_mean_sw_alb=all_mean_sw_alb[lo_idx]
lo_mean_sw_alb_eqn=all_mean_sw_alb_eqn[lo_idx]

if 1 eq 0 then begin
;***********************
;  Paper figure - now in plot_full_dataset
;***********************

  pxdim=900 & pydim=400
  xl=0.08 & xr=0.95
  yb=0.10 & yt=0.90
  sx=0.09
  sy=0.13
  numplots_x=2
  numplots_y=1
  position_plots,xl,xr,yb,yt,sx,sy,numplots_x,numplots_y,pos
  dx=0.01 & dy=0.01
  fs1=12  ;font_size

  ;********************************
  ;  Nd
  ;********************************
  pnum=0
  data_var=all_mean_nd
  start_bin=min(all_mean_nd)
  end_bin=max(all_mean_nd)
  dbin=(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/buffer,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xtitle='Nd #/cm3',ytitle='Frequency',font_size=fs1,/hist,/ystyle)
  p1=plot([nd_median,nd_median],[0,max(data_freq)],color='red',/overplot)
  p1=plot([nd_low_median,nd_low_median],[0,max(data_freq)],color='blue',/overplot)
  p1=plot([nd_high_median,nd_high_median],[0,max(data_freq)],color='green',/overplot)
  t0=text(pos[pnum,0]+27.0*dx,pos[pnum,3]-3.0*dy,string(nd_low_median,format='(F9.3)'),color='blue',font_size=fs1)
  t0=text(pos[pnum,0]+14.0*dx,pos[pnum,3]-3.0*dy,'Nd Low Median',color='blue',font_size=fs1)
  t0=text(pos[pnum,0]+27.0*dx,pos[pnum,3]-6.0*dy,string(nd_median,format='(F9.3)'),color='red',font_size=fs1)
  t0=text(pos[pnum,0]+14.0*dx,pos[pnum,3]-6.0*dy,'Nd Median',color='red',font_size=fs1)
  t0=text(pos[pnum,0]+27.0*dx,pos[pnum,3]-9.0*dy,string(nd_high_median,format='(F9.3)'),color='green',font_size=fs1)
  t0=text(pos[pnum,0]+14.0*dx,pos[pnum,3]-9.0*dy,'Nd High Median',color='green',font_size=fs1)

  t0=text(pos[pnum,0],pos[pnum,3]+3*dy,'a.',font_size=14)
  ;****************************************
  ;  Lat
  ;****************************************
  pnum=1
  data_var=all_mean_lat
  start_bin=min(all_mean_lat)
  end_bin=max(all_mean_lat)
  dbin=(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],$
    xtitle='Latitude',ytitle='Frequency',font_size=fs1,/hist,/nodata)
  data_var=hi_mean_lat
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='green',/hist)
  data_var=lo_mean_lat
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='blue',/hist)
  t0=text(pos[pnum,0],pos[pnum,3]+3*dy,'b.',font_size=14)

  p0.save,'nd_quartiles_lat_dist.v2.png',height=pydim
  stop
endif

if 1 eq 0 then begin
  ;***********************
  ;  Data diagnostic
  ;***********************
  pxdim=1000 & pydim=1000
  xl=0.08 & xr=0.95
  yb=0.06 & yt=0.95
  sx=0.05
  sy=0.08
  numplots_x=2
  numplots_y=5
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

  ;********************************
  ;  Nd
  ;********************************
  pnum=0
  data_var=all_mean_nd
  start_bin=min(all_mean_nd)
  end_bin=max(all_mean_nd)
  dbin=(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/buffer,position=pos[pnum,*],$
    dimensions=[pxdim,pydim],$
    xtitle='Nd #/cm3',ytitle='Frequency',font_size=fs1,/hist)
  p1=plot([nd_median,nd_median],[0,max(data_freq)],color='red',/overplot)
  p1=plot([nd_low_median,nd_low_median],[0,max(data_freq)],color='blue',/overplot)
  p1=plot([nd_high_median,nd_high_median],[0,max(data_freq)],color='green',/overplot)
  t0=text(pos[pnum,0]+16.0*dx,pos[pnum,3]-3.0*dy,'Nd Low Median',color='blue',font_size=fs1)
  t0=text(pos[pnum,0]+29.0*dx,pos[pnum,3]-3.0*dy,string(nd_low_median,format='(F6.2)'),color='blue',font_size=fs1)
  t0=text(pos[pnum,0]+16.0*dx,pos[pnum,3]-6.0*dy,'Nd Median',color='red',font_size=fs1)
  t0=text(pos[pnum,0]+29.0*dx,pos[pnum,3]-6.0*dy,string(nd_median,format='(F6.2)'),color='red',font_size=fs1)
  t0=text(pos[pnum,0]+16.0*dx,pos[pnum,3]-9.0*dy,'Nd High Median',color='green',font_size=fs1)
  t0=text(pos[pnum,0]+29.0*dx,pos[pnum,3]-9.0*dy,string(nd_high_median,format='(F6.2)'),color='green',font_size=fs1)
  t0=text(pos[pnum,0]+0*dx,pos[pnum,3]+1*dy,'Nd from all histograms',color='black',font_size=fs1)

  ;****************************************
  ;  Lon
  ;****************************************
  pnum=2
  data_var=all_mean_lon
  start_bin=-180.0;min(data_var)
  end_bin=180.0;max(data_var)
  dbin=(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],/xstyle,$
    xtitle='Longitude',ytitle='Frequency',font_size=fs1,/hist)
  data_var=hi_mean_lon
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='green',/hist)
  data_var=lo_mean_lon
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='blue',/hist)

  ;****************************************
  ;  Lat
  ;****************************************
  pnum=4
  data_var=all_mean_lat
  start_bin=min(all_mean_lat)
  end_bin=max(all_mean_lat)
  dbin=(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],$
    xtitle='Latitude',ytitle='Frequency',font_size=fs1,/hist)
  data_var=hi_mean_lat
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='green',/hist)
  data_var=lo_mean_lat
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='blue',/hist)

  ;****************************************
  ;  Chlor_a
  ;****************************************
  ;pnum=6
  ;data_var=all_chlor_a
  ;r=where(data_var eq -32767,count_fill)
  ;start_bin=0.0;min(data_var)
  ;end_bin=1.0;max(data_var)
  ;dbin=(end_bin-start_bin)/100.0
  ;hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  ;p0=plot(bins,data_freq,/current,position=pos[pnum,*],/xstyle,$
  ;  xtitle='Chlor_a',ytitle='Frequency',font_size=fs1,/hist)
  ;t1=text(pos[pnum,2]-17*dx,pos[pnum,3]-3*dy,strcompress(count_fill)+'=fill',font_size=fs1)
  ;data_var=hi_chlor_a
  ;hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  ;p1=plot(bins,data_freq,/overplot,color='green',/hist)
  ;data_var=lo_chlor_a
  ;hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  ;p1=plot(bins,data_freq,/overplot,color='blue',/hist)

  ;****************************************
  ;  tau
  ;****************************************
  ;pnum=6
  ;data_var=all_mean_tau
  ;r=where(data_var eq -9999,count_fill)
  ;start_bin=min(data_var)
  ;end_bin=max(data_var)
  ;dbin=(end_bin-start_bin)/100.0
  ;hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  ;p0=plot(bins,data_freq,/current,position=pos[pnum,*],/xstyle,$
  ;  xtitle='Tau',ytitle='Frequency',font_size=fs1,/hist)
  ;t1=text(pos[pnum,2]-17*dx,pos[pnum,3]-3*dy,strcompress(count_fill)+'=fill',font_size=fs1)
  ;data_var=hi_mean_tau
  ;hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  ;p1=plot(bins,data_freq,/overplot,color='green',/hist)
  ;data_var=lo_mean_tau
  ;hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  ;p1=plot(bins,data_freq,/overplot,color='blue',/hist)

  ;****************************************
  ;  sw up
  ;****************************************
  pnum=6
  all_data=[all_mean_sw_up,all_mean_sw_toa]
  data_var=all_mean_sw_up
  r=where(data_var eq -9999,count_fill)
  r=where(all_data ne -9999,c)
  start_bin=min(all_data[r])
  end_bin=max(all_data[r])
  dbin=(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],/xstyle,$
    xtitle='sw_up W/m2  sw_toa=dashed',ytitle='Frequency',font_size=fs1,/hist)
  t1=text(pos[pnum,2]-17*dx,pos[pnum,3]-3*dy,strcompress(count_fill)+'=fill',font_size=fs1)
  data_var=hi_mean_sw_up
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='green',/hist)
  data_var=lo_mean_sw_up
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='blue',/hist)

  data_var=all_mean_sw_toa
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='black',linestyle=2,/hist)
  data_var=hi_mean_sw_toa
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='green',linestyle=2,/hist)
  data_var=lo_mean_sw_toa
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='blue',linestyle=2,/hist)

  ;****************************************
  ;  sw albedo
  ;****************************************
  pnum=8
  data_var=all_mean_sw_alb
  r=where(all_mean_sw_up eq -9999,count_fill)
  r=where(all_mean_sw_up ne -9999,c)
  start_bin=min(data_var[r])
  end_bin=max(data_var[r])
  dbin=(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],/xstyle,$
    xtitle='sw_alb Albedo',ytitle='Frequency',font_size=fs1,/hist)
  t1=text(pos[pnum,2]-17*dx,pos[pnum,3]-3*dy,strcompress(count_fill)+'=fill',font_size=fs1)
  data_var=hi_mean_sw_alb
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='green',/hist)
  data_var=lo_mean_sw_alb
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='blue',/hist)
  ;  Equation value
  data_var=all_mean_sw_alb_eqn
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='black',/hist,linestyle=2)
  data_var=hi_mean_sw_alb_eqn
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='green',/hist,linestyle=2)
  data_var=lo_mean_sw_alb_eqn
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='blue',/hist,linestyle=2)

  ;*************
  ;  All counts
  ;*************
  ; **SW=-76,40  NE=-45,152
  ulat=-45.0
  llat=-76.0
  llon=40.0
  rlon=152.0
  limit_vector=[llat,llon,ulat,rlon]

  ;  Map grid for histogram
  x_start_bin_map=llon
  x_end_bin_map=rlon
  x_dbin_map=1.0
  y_start_bin_map=llat
  y_end_bin_map=ulat
  y_dbin_map=0.5

  ;  Map histogram 
  x_data=all_mean_lon
  y_data=all_mean_lat
  histogram_2d,x_data,y_data,x_start_bin_map,x_end_bin_map,x_dbin_map,x_bins,$
    y_start_bin_map,y_end_bin_map,y_dbin_map,y_bins,data_freq,data_counts_all_map

  pnum=1
  lon_bins=x_bins
  lat_bins=y_bins
  dmin_map=0
  dmax_map=max(data_counts_all_map)
  var_image=bytscl(data_counts_all_map,min=dmin_map,max=dmax_map,top=top_color)
  r=where(data_counts_all_map eq 0,c)
  if c gt 0 then var_image[r]=253
  if 1 eq 1 then begin
    p0=image(var_image,lon_bins,lat_bins,/current,rgb_table=mytable,$
      position=pos[pnum,*],clip=0,$
      map_projection='Mercator',/box_axes,$
      label_position=0,$
      limit=limit_vector,grid_units='degrees',$
      ;label_position=1,clip=0, grid_units='degrees',$
      font_size=fs1)
    mc=mapcontinents()
    cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
      orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
  endif else begin
    p0=image(var_image,lon_bins,lat_bins,/current,$
      position=pos[pnum,*],rgb_table=cimage,$
      image_dimensions=[isx,isy],font_size=10)
    c0=contour(data_counts_all_map,lon_bins,lat_bins,/nodata,position=pos[pnum,*],ytitle='Lat',$
      /current,xstyle=1,ystyle=1,xtickdir=1,ytickdir=1,font_size=12,xtitle='Lon')
    cb=colorbar(rgb_table=cbimage,/border,title='counts',font_size=12,$
      orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
  endelse
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'total cases='+string(total(data_counts_all_map)),font_size=fs1)

  ;***************
  ;  Hi map counts
  ;***************

  ;  Map histogram 
  x_data=hi_mean_lon
  y_data=hi_mean_lat
  histogram_2d,x_data,y_data,x_start_bin_map,x_end_bin_map,x_dbin_map,x_bins,$
    y_start_bin_map,y_end_bin_map,y_dbin_map,y_bins,data_freq,data_counts_hi_map

  pnum=3
  lon_bins=x_bins
  lat_bins=y_bins
  dmin_map=0
  dmax_map=max(data_counts_hi_map)
  var_image=bytscl(data_counts_hi_map,min=dmin_map,max=dmax_map,top=top_color)
  r=where(data_counts_hi_map eq 0,c)
  if c gt 0 then var_image[r]=253
  if 1 eq 1 then begin
    p0=image(var_image,lon_bins,lat_bins,/current,rgb_table=mytable,$
      position=pos[pnum,*],clip=0,$
      map_projection='Mercator',/box_axes,$
      label_position=0,$
      limit=limit_vector,grid_units='degrees',$
      ;label_position=1,clip=0, grid_units='degrees',$
      font_size=fs1)
    mc=mapcontinents()
    cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
      orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
  endif else begin
    p0=image(var_image,lon_bins,lat_bins,/current,$
      position=pos[pnum,*],rgb_table=cimage,$
      image_dimensions=[isx,isy],font_size=10)
    c0=contour(data_counts_all_map,lon_bins,lat_bins,/nodata,position=pos[pnum,*],ytitle='Lat',$
      /current,xstyle=1,ystyle=1,xtickdir=1,ytickdir=1,font_size=12,xtitle='Lon')
    cb=colorbar(rgb_table=cbimage,/border,title='counts',font_size=12,$
      orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
  endelse
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'>Nd High Median cases='+string(total(data_counts_hi_map)),font_size=fs1)

  ;***************
  ;  Lo map counts
  ;***************

  ;  Map histogram 
  x_data=lo_mean_lon
  y_data=lo_mean_lat
  histogram_2d,x_data,y_data,x_start_bin_map,x_end_bin_map,x_dbin_map,x_bins,$
    y_start_bin_map,y_end_bin_map,y_dbin_map,y_bins,data_freq,data_counts_lo_map

  pnum=5
  lon_bins=x_bins
  lat_bins=y_bins
  dmin_map=0
  dmax_map=max(data_counts_lo_map)
  var_image=bytscl(data_counts_lo_map,min=dmin_map,max=dmax_map,top=top_color)
  r=where(data_counts_lo_map eq 0,c)
  if c gt 0 then var_image[r]=253
  if 1 eq 1 then begin
    p0=image(var_image,lon_bins,lat_bins,/current,rgb_table=mytable,$
      position=pos[pnum,*],clip=0,map_projection='Mercator',/box_axes,$
      label_position=0,limit=limit_vector,grid_units='degrees',$
      font_size=fs1)
    mc=mapcontinents()
    cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
      orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
  endif else begin
    p0=image(var_image,lon_bins,lat_bins,/current,position=pos[pnum,*],$
      rgb_table=cimage,image_dimensions=[isx,isy],font_size=10)
    c0=contour(data_counts_all_map,lon_bins,lat_bins,/nodata,position=pos[pnum,*],ytitle='Lat',$
      /current,xstyle=1,ystyle=1,xtickdir=1,ytickdir=1,font_size=12,xtitle='Lon')
    cb=colorbar(rgb_table=cbimage,/border,title='counts',font_size=12,$
      orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
  endelse
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'< Nd Low Median cases='+string(total(data_counts_lo_map)),font_size=fs1)

  ;***************
  ;  chlor_a map counts
  ;***************
  if 1 eq 0 then begin
    ;  Map histogram
    r=where(all_chlor_a ne -32767.0,c)
    x_data=all_chlor_a_lon[r]
    y_data=all_chlor_a_lat[r]
    histogram_2d,x_data,y_data,x_start_bin_map,x_end_bin_map,x_dbin_map,x_bins,$
      y_start_bin_map,y_end_bin_map,y_dbin_map,y_bins,data_freq,data_counts_chlor_a
    pnum=7
    lon_bins=x_bins
    lat_bins=y_bins
    dmin_map=0
    dmax_map=max(data_counts_chlor_a)
    var_image=bytscl(data_counts_chlor_a,min=dmin_map,max=dmax_map,top=top_color)
    r=where(data_counts_chlor_a eq 0,c)
    if c gt 0 then var_image[r]=253
    if 1 eq 1 then begin
      p0=image(var_image,lon_bins,lat_bins,/current,rgb_table=mytable,$
        position=pos[pnum,*],clip=0,map_projection='Mercator',/box_axes,$
        label_position=0,limit=limit_vector,grid_units='degrees',$
        font_size=fs1)
      mc=mapcontinents()
      cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
        orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
    endif else begin
      p0=image(var_image,lon_bins,lat_bins,/current,position=pos[pnum,*],$
        rgb_table=cimage,image_dimensions=[isx,isy],font_size=10)
      c0=contour(data_counts_all_map,lon_bins,lat_bins,/nodata,position=pos[pnum,*],ytitle='Lat',$
        /current,xstyle=1,ystyle=1,xtickdir=1,ytickdir=1,font_size=12,xtitle='Lon')
      cb=colorbar(rgb_table=cbimage,/border,title='counts',font_size=12,$
        orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
    endelse
    t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'total chlor_a cases='+string(total(data_counts_chlor_a)),font_size=fs1)
  endif  ;turn off chlor

  ;***************
  ;  SW ALBEDO
  ;***************
  pnum=7
  r=where(all_mean_sw_up ne -9999,c)
  data_var=all_mean_sw_alb[r]
  data_lat=all_mean_lat[r]
  data_lon=all_mean_lon[r]
  dmin_sw=min(data_var)
  dmax_sw=max(data_var)
  if 1 eq 1 then begin
    p0=image(data_var,data_lon,data_lat,/current,rgb_table=mycbtable,$
      max_value=dmax_sw,min_value=dmin_sw,position=pos[pnum,*],clip=0,$
      map_projection='Mercator',/box_axes,label_position=0,$
      limit=limit_vector,grid_units='degrees',font_size=fs1)
    mc=mapcontinents()
    cb=colorbar(rgb_table=mycbtable,/border,title='SW Albedo',font_size=fs1,$
      orientation=1,textpos=1,position=reform(cbpos[pnum,*]),target=p0)
  endif else begin
    p0=image(var_image,lon_bins,lat_bins,/current,position=pos[pnum,*],$
      rgb_table=cimage,image_dimensions=[isx,isy],font_size=10)
    c0=contour(data_counts_all_map,lon_bins,lat_bins,/nodata,position=pos[pnum,*],$
      ytitle='Lat',/current,xstyle=1,ystyle=1,xtickdir=1,ytickdir=1,font_size=12,$
      xtitle='Lon')
    cb=colorbar(rgb_table=cbimage,/border,title='counts',font_size=12,$
      orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
  endelse
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'total alb cases='+string(n_elements(all_mean_sw_alb)),font_size=fs1)

  ;***************
  ;  SW TOA
  ;***************
  pnum=9
  r=where(all_mean_sw_toa ne -9999,c)
  data_var=all_mean_sw_toa[r]
  data_lat=all_mean_lat[r]
  data_lon=all_mean_lon[r]
  dmin_sw=min(data_var)
  dmax_sw=max(data_var)
  if 1 eq 1 then begin
    p0=image(data_var,data_lon,data_lat,/current,rgb_table=mycbtable,$
      max_value=dmax_sw,min_value=dmin_sw,position=pos[pnum,*],clip=0,$
      map_projection='Mercator',/box_axes,label_position=0,$
      limit=limit_vector,grid_units='degrees',font_size=fs1)
    mc=mapcontinents()
    cb=colorbar(rgb_table=mycbtable,/border,title='TOA SW W/m2',font_size=fs1,$
    orientation=1,textpos=1,position=reform(cbpos[pnum,*]),target=p0)
  endif else begin
    p0=image(var_image,lon_bins,lat_bins,/current,position=pos[pnum,*],$
      rgb_table=cimage,image_dimensions=[isx,isy],font_size=10)
    c0=contour(data_counts_all_map,lon_bins,lat_bins,/nodata,position=pos[pnum,*],ytitle='Lat',$
      /current,xstyle=1,ystyle=1,xtickdir=1,ytickdir=1,font_size=12,xtitle='Lon')
    cb=colorbar(rgb_table=cbimage,/border,title='counts',font_size=12,$
      orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
  endelse
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'total toa cases='+string(n_elements(all_mean_sw_toa)),font_size=fs1)

  t1=text(0.1,0.97,eos,font_size=fs1)
  t1=text(0.2,0.97,time_range_str,font_size=fs1)

  imagename=eos+'.histograms.'+time_range_str+'.png' 
  p0.save,imagename;,height=pydim
endif
stop
;*******************************************
;*******************************************
;  Trajectory figure
;*******************************************
;*******************************************
if 1 eq 0 then begin
  ; **SW=-76,40  NE=-45,152
  ulat=-45.0 & llat=-76.0
  llon=40.0 & rlon=152.0
  limit_vector=[llat,llon,ulat,rlon]

  ;  Map grid for histogram
  x_start_bin_map=llon
  x_end_bin_map=rlon
  x_dbin_map=1.0
  y_start_bin_map=llat
  y_end_bin_map=ulat
  y_dbin_map=0.5

  ;  all_mean_lat and lat_traj_start,lon_traj_start
  lat_traj_start=all_lat_traj[*,0]
  lon_traj_start=all_lon_traj[*,0]

  ;*************
  ;  Latitude
  ;*************
  pnum=0
  data_var=all_mean_lat
  start_bin=min(all_mean_lat)
  end_bin=max(all_mean_lat)
  dbin=(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/buffer,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    xtitle='Latitude',ytitle='Frequency',font_size=fs1,/hist)
  data_var=all_lat_traj
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='red',/hist)
  rhi=where(all_mean_nd gt 100,chi)
  data_var=all_mean_lat[rhi]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='purple',/hist)
  data_var=all_lat_traj[rhi,*]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='blue',/hist)
  t1=text(pos[pnum,0]+15*dx,pos[pnum,3]-2*dy,'Lat Start',font_size=fs1)
  t1=text(pos[pnum,0]+25*dx,pos[pnum,3]-2*dy,string(n_elements(all_mean_lat)),font_size=fs1)
  t1=text(pos[pnum,0]+15*dx,pos[pnum,3]-4*dy,'Lat Traj',color='red',font_size=fs1)
  t1=text(pos[pnum,0]+25*dx,pos[pnum,3]-4*dy,string(n_elements(all_lat_traj)),color='red',font_size=fs1)
  t1=text(pos[pnum,0]+15*dx,pos[pnum,3]-6*dy,'Hi Start',color='purple',font_size=fs1)
  t1=text(pos[pnum,0]+25*dx,pos[pnum,3]-6*dy,string(n_elements(all_mean_lat[rhi])),color='purple',font_size=fs1)
  t1=text(pos[pnum,0]+15*dx,pos[pnum,3]-8*dy,'Hi Traj',color='blue',font_size=fs1)
  t1=text(pos[pnum,0]+25*dx,pos[pnum,3]-8*dy,string(n_elements(all_lat_traj[rhi,*])),color='blue',font_size=fs1)

  ;*************
  ;  Longitude
  ;*************
  pnum=2
  data_var=all_mean_lon
  start_bin=-180.0;min(all_mean_lon)
  end_bin=180.0;max(all_mean_lon)
  dbin=(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],dimensions=[pxdim,pydim],$
    /xstyle,xtitle='Longitude',ytitle='Frequency',font_size=fs1,/hist)
  data_var=all_lon_traj
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='red',/hist)
  rhi=where(all_mean_nd gt 100,chi)
  data_var=all_mean_lon[rhi]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='purple',/hist)
  data_var=all_lon_traj[rhi,*]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='blue',/hist)
  t1=text(pos[pnum,0]+5*dx,pos[pnum,3]-2*dy,'Lon Start',font_size=fs1)
  t1=text(pos[pnum,0]+5*dx,pos[pnum,3]-4*dy,'Lon Traj',color='red',font_size=fs1)
  t1=text(pos[pnum,0]+5*dx,pos[pnum,3]-6*dy,'Hi Start',color='purple',font_size=fs1)
  t1=text(pos[pnum,0]+5*dx,pos[pnum,3]-8*dy,'Hi Traj',color='blue',font_size=fs1)

  ;*************
  ;  Start locations
  ;*************
  ;  Map histogram
  x_data=all_mean_lon
  y_data=all_mean_lat
  histogram_2d,x_data,y_data,x_start_bin_map,x_end_bin_map,x_dbin_map,x_bins,$
    y_start_bin_map,y_end_bin_map,y_dbin_map,y_bins,data_freq,data_counts_all_map
  pnum=1
  lon_bins=x_bins
  lat_bins=y_bins
  dmin_map=0
  dmax_map=max(data_counts_all_map)
  var_image=bytscl(data_counts_all_map,min=dmin_map,max=dmax_map,top=top_color)
  r=where(data_counts_all_map eq 0,c)
  if c gt 0 then var_image[r]=253
  p0=image(var_image,lon_bins,lat_bins,/current,rgb_table=mytable,$
    position=pos[pnum,*],map_projection='Mercator',/box_axes,clip=0,$
    label_position=0,limit=limit_vector,grid_units='degrees',font_size=fs1)
  mc=mapcontinents()
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,$
    'Start Locations='+string(total(data_counts_all_map)),font_size=fs1)

  ;*************
  ;  Trajectory locations
  ;*************
  ;  Map histogram
  x_data=all_lon_traj
  y_data=all_lat_traj
  histogram_2d,x_data,y_data,x_start_bin_map,x_end_bin_map,x_dbin_map,x_bins,$
    y_start_bin_map,y_end_bin_map,y_dbin_map,y_bins,data_freq,data_counts_all_map
  pnum=3
  lon_bins=x_bins
  lat_bins=y_bins
  dmin_map=0
  dmax_map=max(data_counts_all_map)
  var_image=bytscl(data_counts_all_map,min=dmin_map,max=dmax_map,top=top_color)
  r=where(data_counts_all_map eq 0,c)
  if c gt 0 then var_image[r]=253
  p0=image(var_image,lon_bins,lat_bins,/current,rgb_table=mytable,$
    position=pos[pnum,*],map_projection='Mercator',/box_axes,clip=0,$
    label_position=0,limit=limit_vector,grid_units='degrees',font_size=fs1)
  mc=mapcontinents()
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,$
    'All Trajectories='+string(total(data_counts_all_map)),font_size=fs1)

  ;*************
  ;  HI Trajectory locations
  ;*************
  rhi=where(all_mean_nd ge 100)
  ;  Map histogram
  x_data=all_lon_traj[rhi,*]
  y_data=all_lat_traj[rhi,*]
  histogram_2d,x_data,y_data,x_start_bin_map,x_end_bin_map,x_dbin_map,x_bins,$
    y_start_bin_map,y_end_bin_map,y_dbin_map,y_bins,data_freq,data_counts_all_map
  pnum=5
  lon_bins=x_bins
  lat_bins=y_bins
  dmin_map=0
  dmax_map=max(data_counts_all_map)
  var_image=bytscl(data_counts_all_map,min=dmin_map,max=dmax_map,top=top_color)
  r=where(data_counts_all_map eq 0,c)
  if c gt 0 then var_image[r]=253
  p0=image(var_image,lon_bins,lat_bins,/current,rgb_table=mytable,$
    position=pos[pnum,*],map_projection='Mercator',/box_axes,clip=0,$
    label_position=0,limit=limit_vector,grid_units='degrees',font_size=fs1)
  mc=mapcontinents()
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,$
    'High Trajectories='+string(total(data_counts_all_map)),font_size=fs1)

  ;*************
  ;  LO Trajectory locations
  ;*************
  rhi=where(all_mean_nd le 50)
  ;  Map histogram
  x_data=all_lon_traj[rhi,*]
  y_data=all_lat_traj[rhi,*]
  histogram_2d,x_data,y_data,x_start_bin_map,x_end_bin_map,x_dbin_map,x_bins,$
    y_start_bin_map,y_end_bin_map,y_dbin_map,y_bins,data_freq,data_counts_all_map
  pnum=7
  lon_bins=x_bins
  lat_bins=y_bins
  dmin_map=0
  dmax_map=max(data_counts_all_map)
  var_image=bytscl(data_counts_all_map,min=dmin_map,max=dmax_map,top=top_color)
  r=where(data_counts_all_map eq 0,c)
  if c gt 0 then var_image[r]=253
  p0=image(var_image,lon_bins,lat_bins,/current,rgb_table=mytable,$
    position=pos[pnum,*],map_projection='Mercator',/box_axes,clip=0,$
    label_position=0,limit=limit_vector,grid_units='degrees',font_size=fs1)
  mc=mapcontinents()
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin_map,dmax_map])
  t1=text(pos[pnum,0],pos[pnum,3]+1*dy,$
    'Low Trajectories='+string(total(data_counts_all_map)),font_size=fs1)

  ;*************
  ;  Latitude
  ;*************
  pnum=4
  rhi=where(all_mean_nd ge 100,chi)
  data_var=all_mean_lat[rhi]
  r=where(all_lat_traj ne -9999)
  start_bin=min(all_lat_traj[r])
  end_bin=max(all_lat_traj[r])
  dbin=(end_bin-start_bin)/100.0
  print,start_bin,end_bin,dbin
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],$
    dimensions=[pxdim,pydim],/nodata,title='all latitudes',$
    xtitle='Latitude',ytitle='Frequency',font_size=fs1,/hist)
  data_var=all_lat_traj[rhi,*]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='red',/hist)
  rlo=where(all_mean_nd le 50,clo)
  data_var=all_lat_traj[rlo,*]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='blue',/hist)
  t1=text(pos[pnum,0]+25*dx,pos[pnum,3]-4*dy,'Hi Traj',color='red',font_size=fs1)
  t1=text(pos[pnum,0]+25*dx,pos[pnum,3]-8*dy,'Lo Traj',color='blue',font_size=fs1)

  ;*************
  ;  Latitude
  ;*************
  pnum=6
  rhi=where(all_mean_nd ge 100 and all_mean_lat gt -60,chi)
  data_var=all_mean_lat[rhi]
  r=where(all_lat_traj ne -9999)
  start_bin=min(all_lat_traj[r])
  end_bin=max(all_lat_traj[r])
  dbin=(end_bin-start_bin)/100.0
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p0=plot(bins,data_freq,/current,position=pos[pnum,*],$
    dimensions=[pxdim,pydim],/nodata,title='all latitude north of 60S',$
    xtitle='Latitude',ytitle='Frequency',font_size=fs1,/hist)
  data_var=all_lat_traj[rhi,*]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='red',/hist)
  rlo=where(all_mean_nd le 50 and all_mean_lat gt -60,clo)
  data_var=all_lat_traj[rlo,*]
  hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
  p1=plot(bins,data_freq,/overplot,color='blue',/hist)
  t1=text(pos[pnum,0]+25*dx,pos[pnum,3]-4*dy,'Hi Traj',color='red',font_size=fs1)
  t1=text(pos[pnum,0]+25*dx,pos[pnum,3]-8*dy,'Lo Traj',color='blue',font_size=fs1)

  t1=text(0.1,0.97,eos,font_size=fs1)
  t1=text(0.2,0.97,time_range_str,font_size=fs1)

  imagename=eos+'.histograms.'+time_range_str+'.traj.png'
  p0.save,imagename,height=pydim
endif  
stop



end