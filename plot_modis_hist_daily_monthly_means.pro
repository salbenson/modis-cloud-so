;***************************************
;  This program reads in the month files of histograms and 
;  calculates monthly and daily means and sdev
;  Makes the plot of the daily means
;***************************************

pro plot_modis_hist_daily_monthly_means

;  imac or chpc
path_prefix='/Volumes/' 
;path_prefix='/uufs/chpc.utah.edu/common/home/'

;  increased box
ulat=-40 & llat=-70
llon=50 & rlon=170

;  Choose aqua or terra
eos='MYD'
;eos='MOD'
;eos='MOYD'  ;both

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
  
;  Name of output file for the time range
output_file=eos+'.histograms_mean.'+time_range_str+'.'+box_str+'.tau.cmonth2.cdf'
print,output_file

;  Arrays of variables to loop through
months=[11,12,1,2]  ;composite plot for these months
num_months=n_elements(months)
month_name=['nov','dec','jan','feb']

region=['box','n60','s60','ln60','hn60','ls60','hs60']

var_mod=['julian_day','count_1km','count_5km','center_latitude','center_longitude',$
  'mean_nd','mean_re','mean_lwp','mean_tau','mean_solar_zenith']
varname_mod=['nd','re','lwp','tau','solar_zenith']
varmin_mod=[  2.0, 4.2, 1.0,  0.1,  0.0]  ;bin center
varmax_mod=[300.0,30.0, 300.0,35.0, 90.0]
vardbin_mod=[1.0,  0.1, 1.5,  0.2,  0.5]  ;1/2 bin width

var_cer=['mean_sw_up','mean_sw_toa','mean_sw_alb','mean_sw_nalb']
varname_cer=['sw_up','sw_toa','sw_alb','sw_nalb']
varmin_cer=[0.0,    500.0,   0.0,     0.0]  ;bin center
varmax_cer=[900.0,  1500.0,  1.0,     1.0]
vardbin_cer=[5.0,    5.0,     0.005,     0.005]  ;1/2 bin width

varname=[varname_mod,varname_cer]
var=[var_mod,var_cer]

;  If the output file does not exist, then create it
;if file_test(output_file) eq 0 then begin
if file_test(output_file) eq 1 or file_test(output_file) eq 0 then begin
  print,'making output file'
  
  all_julian_day=!NULL
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
        for k=0,n_elements(var)-1 do begin
          xstr='vid=ncdf_varid(fid,"'+var[k]+'")'
          result=execute(xstr)
          xstr='ncdf_varget,fid,vid,'+var[k]+'1'
          result=execute(xstr)
        endfor
        ncdf_close,fid
        if j eq 0 then begin
          print,j,'j ',i,'i ','new array'
          for k=0,n_elements(var)-1 do begin
            xstr=var[k]+'='+var[k]+'1'
            result=execute(xstr)
          endfor
        endif else begin
          print,j,'j ',i,'i ','append'
          for k=0,n_elements(var)-1 do begin
            xstr=var[k]+'=['+var[k]+','+var[k]+'1]'
            result=execute(xstr)
          endfor
        endelse
      endfor  ;end of loop through monthly files
      
      ;  Now calculate daily means
      julian_day_1d=timegen(start=min(julian_day),final=max(julian_day),units='days',step_size=1)
      caldat,julian_day_1d,mm_1d,dd_1d,yy_1d,hh_1d,mi_1d,ss_1d
      num_1d=n_elements(julian_day_1d)
      
      ;  Create arrays to hold daily means
      print,'create arrays'
      for r=0,n_elements(region)-1 do begin
        for v=0,n_elements(varname)-1 do begin
          xstr='daily_mean_'+varname[v]+'_'+region[r]+'=make_array(num_1d,/float,value=-9999)'
          result=execute(xstr)
          xstr='daily_sdev_'+varname[v]+'_'+region[r]+'=make_array(num_1d,/float,value=-9999)'
          result=execute(xstr)
        endfor
        xstr='daily_count_'+region[r]+'=make_array(num_1d,/float,value=-9999)'
        result=execute(xstr)
        xstr='daily_count_cer_'+region[r]+'=make_array(num_1d,/float,value=-9999)'
        result=execute(xstr)
      endfor
      
      ;  Loop through the days to get the mean value for each day  
      ;region=['box','n60','s60','ln60','hn60','ls60','hs60']
      for k=0,num_1d-1 do begin
        ;*** MODIS Variables ***
        r_1d=where(julian_day ge julday(mm_1d[k],dd_1d[k],yy_1d[k],0,0,0) and $
                    julian_day le julday(mm_1d[k],dd_1d[k],yy_1d[k],23,59,59) and $
                    center_latitude ge llat and center_latitude le ulat and $
                    center_longitude ge llon and center_longitude le rlon,c_1d)  
        if c_1d gt 0 then begin
          ;  Values for the day
          sub_mean_nd=mean_nd[r_1d]
          sub_mean_re=mean_re[r_1d]
          sub_mean_lwp=mean_lwp[r_1d]
          sub_mean_tau=mean_tau[r_1d]
          sub_mean_solar_zenith=mean_solar_zenith[r_1d]
          sub_center_lat=center_latitude[r_1d]
          sub_center_lon=center_longitude[r_1d]
          ;  box
          r_box=where(sub_center_lat ge llat and sub_center_lat le ulat,c_box)
          ;  north,south of -60S
          r_n60=where(sub_center_lat gt -60 and sub_center_lat le ulat,c_n60) ;lo north of -60S
          r_s60=where(sub_center_lat ge llat and sub_center_lat le -60,c_s60) ;hi north of -60S
          ;  Hi,Lo north,south of -60S
          r_ln60=where(sub_center_lat gt -60 and sub_center_lat le ulat and sub_mean_nd le 50,c_ln60) ;lo north of -60S
          r_hn60=where(sub_center_lat gt -60 and sub_center_lat le ulat and sub_mean_nd ge 100,c_hn60) ;hi north of -60S
          r_ls60=where(sub_center_lat ge llat and sub_center_lat le -60 and sub_mean_nd le 50,c_ls60) ;lo south of -60S
          r_hs60=where(sub_center_lat ge llat and sub_center_lat le -60 and sub_mean_nd ge 100,c_hs60) ;hi south of -60S
          ;  Daily means
          for r=0,n_elements(region)-1 do begin
            for v=0,n_elements(varname_mod)-1 do begin
              xstr='daily_mean_'+varname_mod[v]+'_'+region[r]+'[k]=mean(sub_mean_'+varname_mod[v]+'[r_'+region[r]+'])'
              result=execute(xstr)
              xstr='daily_sdev_'+varname_mod[v]+'_'+region[r]+'[k]=stddev(sub_mean_'+varname_mod[v]+'[r_'+region[r]+'])'
              result=execute(xstr)
            endfor
            xstr='daily_count_'+region[r]+'[k]=c_'+region[r]
            result=execute(xstr)
          endfor
        endif  ;end of modis variables
      
        ;*** CERES Variables ***
        r_1d=where(julian_day ge julday(mm_1d[k],dd_1d[k],yy_1d[k],0,0,0) and $
          julian_day le julday(mm_1d[k],dd_1d[k],yy_1d[k],23,59,59) and $
          center_latitude ge llat and center_latitude le ulat and $
          mean_sw_toa ne -9999 and $  ;don't average in these values
          center_longitude ge llon and center_longitude le rlon,c_1d)
        if c_1d gt 0 then begin
          ;  Values for the day
          sub_mean_sw_up=mean_sw_up[r_1d]
          sub_mean_sw_toa=mean_sw_toa[r_1d]
          sub_mean_sw_alb=mean_sw_alb[r_1d]
          sub_mean_sw_nalb=mean_sw_nalb[r_1d]
          sub_center_lat=center_latitude[r_1d]
          sub_mean_nd=mean_nd[r_1d]
          ;  box
          r_box=where(sub_center_lat ge llat and sub_center_lat le ulat,c_box)
          ;  north,south of -60S
          r_n60=where(sub_center_lat gt -60 and sub_center_lat le ulat,c_n60) ;lo north of -60S
          r_s60=where(sub_center_lat ge llat and sub_center_lat le -60,c_s60) ;hi north of -60S
          ;  Hi,Lo north,south of -60S
          r_ln60=where(sub_center_lat gt -60 and sub_center_lat le ulat and sub_mean_nd le 50,c_ln60) ;lo north of -60S
          r_hn60=where(sub_center_lat gt -60 and sub_center_lat le ulat and sub_mean_nd ge 100,c_hn60) ;hi north of -60S
          r_ls60=where(sub_center_lat ge llat and sub_center_lat le -60 and sub_mean_nd le 50,c_ls60) ;lo south of -60S
          r_hs60=where(sub_center_lat ge llat and sub_center_lat le -60 and sub_mean_nd ge 100,c_hs60) ;hi south of -60S
          ;  Daily means
          for r=0,n_elements(region)-1 do begin
            for v=0,n_elements(varname_cer)-1 do begin
              xstr='daily_mean_'+varname_cer[v]+'_'+region[r]+'[k]=mean(sub_mean_'+varname_cer[v]+'[r_'+region[r]+'])'
              result=execute(xstr)
              xstr='daily_sdev_'+varname_cer[v]+'_'+region[r]+'[k]=stddev(sub_mean_'+varname_cer[v]+'[r_'+region[r]+'])'
              result=execute(xstr)
            endfor
            xstr='daily_count_cer_'+region[r]+'[k]=c_'+region[r]
            result=execute(xstr)
          endfor
        endif  ;end of ceres variables
      endfor  ;end of loop through days
      
      ;***************
      ;  Now calculate the monthly means
      ;*** MODIS Variables ***
      r_box=where(center_latitude ge llat and center_latitude le ulat and $
                  center_longitude ge llon and center_longitude le rlon,c_box)
      ;  north,south of -60S
      r_n60=where(center_latitude gt -60 and center_latitude le ulat and $
                  center_longitude ge llon and center_longitude le rlon,c_n60) ;north of -60S
      r_s60=where(center_latitude ge llat and center_latitude le -60 and $
                  center_longitude ge llon and center_longitude le rlon,c_s60) ;south of -60S
      ;  Hi,Lo north,south of -60S
      r_ln60=where(center_latitude gt -60 and center_latitude le ulat and $
                  center_longitude ge llon and center_longitude le rlon and mean_nd le 50,c_ln60) ;lo north of -60S
      r_hn60=where(center_latitude gt -60 and center_latitude le ulat and $
                  center_longitude ge llon and center_longitude le rlon and mean_nd ge 100,c_hn60) ;hi north of -60S
      r_ls60=where(center_latitude ge llat and center_latitude le -60 and $
                  center_longitude ge llon and center_longitude le rlon and mean_nd le 50,c_ls60) ;lo south of -60S
      r_hs60=where(center_latitude ge llat and center_latitude le -60 and $
                  center_longitude ge llon and center_longitude le rlon and mean_nd ge 100,c_hs60) ;hi south of -60S            
                  
      for r=0,n_elements(region)-1 do begin
        for v=0,n_elements(varname_mod)-1 do begin
          xstr='month_mean_'+varname_mod[v]+'_'+region[r]+'=mean(mean_'+varname_mod[v]+'[r_'+region[r]+'])'
          result=execute(xstr)
          xstr='month_sdev_'+varname_mod[v]+'_'+region[r]+'=stddev(mean_'+varname_mod[v]+'[r_'+region[r]+'])'
          result=execute(xstr)
        endfor
        xstr='month_count_'+region[r]+'=c_'+region[r]
        result=execute(xstr)
        xstr='sub_mean_nd=mean_nd[r_'+region[r]+']'
        result=execute(xstr)
        nd_median=median(sub_mean_nd)
        xstr='month_nd_median_'+region[r]+'=nd_median'
        result=execute(xstr)
        xstr='month_nd_lo_median_'+region[r]+'=median(sub_mean_nd[where(sub_mean_nd le nd_median)])'
        result=execute(xstr)
        xstr='month_nd_hi_median_'+region[r]+'=median(sub_mean_nd[where(sub_mean_nd gt nd_median)])'
        result=execute(xstr)
      endfor
      
      ;*** CERES Variables ***
      r_box=where(center_latitude ge llat and center_latitude le ulat and $
        mean_sw_toa ne -9999 and $  ;don't average in these values
        center_longitude ge llon and center_longitude le rlon,c_box)
      ;  north,south of -60S
      r_n60=where(center_latitude gt -60 and center_latitude le ulat and $
        mean_sw_toa ne -9999 and $  ;don't average in these values
        center_longitude ge llon and center_longitude le rlon,c_n60) ;north of -60S
      r_s60=where(center_latitude ge llat and center_latitude le -60 and $
        mean_sw_toa ne -9999 and $  ;don't average in these values
        center_longitude ge llon and center_longitude le rlon,c_s60) ;south of -60S
      ;  Hi,Lo north,south of -60S
      r_ln60=where(center_latitude gt -60 and center_latitude le ulat and $
        mean_sw_toa ne -9999 and $  ;don't average in these values
        center_longitude ge llon and center_longitude le rlon and mean_nd le 50,c_ln60) ;lo north of -60S
      r_hn60=where(center_latitude gt -60 and center_latitude le ulat and $
        mean_sw_toa ne -9999 and $  ;don't average in these values
        center_longitude ge llon and center_longitude le rlon and mean_nd ge 100,c_hn60) ;hi north of -60S
      r_ls60=where(center_latitude ge llat and center_latitude le -60 and $
        mean_sw_toa ne -9999 and $  ;don't average in these values
        center_longitude ge llon and center_longitude le rlon and mean_nd le 50,c_ls60) ;lo south of -60S
      r_hs60=where(center_latitude ge llat and center_latitude le -60 and $
        mean_sw_toa ne -9999 and $  ;don't average in these values
        center_longitude ge llon and center_longitude le rlon and mean_nd ge 100,c_hs60) ;hi south of -60S

      for r=0,n_elements(region)-1 do begin
        for v=0,n_elements(varname_cer)-1 do begin
          xstr='month_mean_'+varname_cer[v]+'_'+region[r]+'=mean(mean_'+varname_cer[v]+'[r_'+region[r]+'])'
          result=execute(xstr)
          xstr='month_sdev_'+varname_cer[v]+'_'+region[r]+'=stddev(mean_'+varname_cer[v]+'[r_'+region[r]+'])'
          result=execute(xstr)
        endfor
        xstr='month_count_cer_'+region[r]+'=c_'+region[r]
        result=execute(xstr)
      endfor
      
      ;******************************
      ;  Save everything in one big array
      if all_julian_day eq !NULL then begin
        all_julian_day=julian_day_1m[i]
        all_julian_day_1d=julian_day_1d  
        
        for r=0,n_elements(region)-1 do begin
          for v=0,n_elements(varname)-1 do begin
            xstr='all_month_mean_'+varname[v]+'_'+region[r]+'=month_mean_'+varname[v]+'_'+region[r]
            result=execute(xstr)
            xstr='all_month_sdev_'+varname[v]+'_'+region[r]+'=month_sdev_'+varname[v]+'_'+region[r]
            result=execute(xstr)
            
            xstr='all_daily_mean_'+varname[v]+'_'+region[r]+'=daily_mean_'+varname[v]+'_'+region[r]
            result=execute(xstr)
            xstr='all_daily_sdev_'+varname[v]+'_'+region[r]+'=daily_sdev_'+varname[v]+'_'+region[r]
            result=execute(xstr)
          endfor
          xstr='all_month_count_'+region[r]+'=month_count_'+region[r]
          result=execute(xstr)
          xstr='all_month_count_cer_'+region[r]+'=month_count_cer_'+region[r]
          result=execute(xstr)
          
          xstr='all_month_nd_median_'+region[r]+'=month_nd_median_'+region[r]
          result=execute(xstr)
          xstr='all_month_nd_lo_median_'+region[r]+'=month_nd_lo_median_'+region[r]
          result=execute(xstr)
          xstr='all_month_nd_hi_median_'+region[r]+'=month_nd_hi_median_'+region[r]
          result=execute(xstr)
          
          xstr='all_daily_count_'+region[r]+'=daily_count_'+region[r]
          result=execute(xstr)
          xstr='all_daily_count_cer_'+region[r]+'=daily_count_cer_'+region[r]
          result=execute(xstr)
        endfor  
   
      endif else begin
        all_julian_day=[all_julian_day,julian_day_1m[i]]
        all_julian_day_1d=[all_julian_day_1d,julian_day_1d]
        
        for r=0,n_elements(region)-1 do begin
          for v=0,n_elements(varname)-1 do begin
            xstr='all_month_mean_'+varname[v]+'_'+region[r]+'=[all_month_mean_'+varname[v]+'_'+region[r]+',month_mean_'+varname[v]+'_'+region[r]+']'
            result=execute(xstr)
            xstr='all_month_sdev_'+varname[v]+'_'+region[r]+'=[all_month_sdev_'+varname[v]+'_'+region[r]+',month_sdev_'+varname[v]+'_'+region[r]+']'
            result=execute(xstr)

            xstr='all_daily_mean_'+varname[v]+'_'+region[r]+'=[all_daily_mean_'+varname[v]+'_'+region[r]+',daily_mean_'+varname[v]+'_'+region[r]+']'
            result=execute(xstr)
            xstr='all_daily_sdev_'+varname[v]+'_'+region[r]+'=[all_daily_sdev_'+varname[v]+'_'+region[r]+',daily_sdev_'+varname[v]+'_'+region[r]+']'
            result=execute(xstr)
          endfor
          xstr='all_month_count_'+region[r]+'=[all_month_count_'+region[r]+',month_count_'+region[r]+']'
          result=execute(xstr)
          xstr='all_month_count_cer_'+region[r]+'=[all_month_count_cer_'+region[r]+',month_count_cer_'+region[r]+']'
          result=execute(xstr)
          
          xstr='all_month_nd_median_'+region[r]+'=[all_month_nd_median_'+region[r]+',month_nd_median_'+region[r]+']'
          result=execute(xstr)
          xstr='all_month_nd_lo_median_'+region[r]+'=[all_month_nd_lo_median_'+region[r]+',month_nd_lo_median_'+region[r]+']'
          result=execute(xstr)
          xstr='all_month_nd_hi_median_'+region[r]+'=[all_month_nd_hi_median_'+region[r]+',month_nd_hi_median_'+region[r]+']'
          result=execute(xstr)

          xstr='all_daily_count_'+region[r]+'=[all_daily_count_'+region[r]+',daily_count_'+region[r]+']'
          result=execute(xstr)
          xstr='all_daily_count_cer_'+region[r]+'=[all_daily_count_cer_'+region[r]+',daily_count_cer_'+region[r]+']'
          result=execute(xstr)
        endfor   
      endelse
    endif  ;if there are histogram files
  endfor  ;loop through list of dates
  
  ;*****************************
  ;  Create a composite seasonal plot
  ;*****************************
  
;  ;  Arrays of variables to loop through
;  months=[11,12,1,2]  ;composite plot for these months
;  num_months=n_elements(months)
;  varname=['nd','re','lwp','tau']
;  varmin=[2.0,4.2,1.0,0.1]  ;bin center
;  varmax=[300.0,30.0,300.0,35.0]
;  vardbin=[1.0,0.1,1.5,0.2]  ;1/2 bin width
;  region=['box','n60','s60','ln60','hn60','ls60','hs60']
;  var=['julian_day','count_1km','count_5km','center_latitude','center_longitude',$
;    'mean_nd','mean_re','mean_lwp','mean_tau']
    
  ;  Create arrays to hold composite monthly means
  print,'create arrays'
  for r=0,n_elements(region)-1 do begin
    for v=0,n_elements(varname)-1 do begin
      xstr='cmonth_mean_'+varname[v]+'_'+region[r]+'=make_array(num_months,/float,value=-9999)'
      result=execute(xstr)
      xstr='cmonth_sdev_'+varname[v]+'_'+region[r]+'=make_array(num_months,/float,value=-9999)'
      result=execute(xstr)
    endfor
    xstr='cmonth_count_'+region[r]+'=make_array(num_months,/float,value=-9999)'
    result=execute(xstr)
    xstr='cmonth_count_cer'+region[r]+'=make_array(num_months,/float,value=-9999)'
    result=execute(xstr)
    xstr='cmonth_nd_median_'+region[r]+'=make_array(num_months,/float,value=-9999)'
    result=execute(xstr)
    xstr='cmonth_nd_lo_median_'+region[r]+'=make_array(num_months,/float,value=-9999)'
    result=execute(xstr)
    xstr='cmonth_nd_hi_median_'+region[r]+'=make_array(num_months,/float,value=-9999)'
    result=execute(xstr)
  endfor
  
  ;  Loop through the months
  for i=0,num_months-1 do begin
    month=months[i]
    month_str=string(month,format='(I02)')
    ;  Get the monthly histogram file names
    if eos eq 'MOYD' then begin
      file_str='*.histograms.*'+month_str+'01-*'+'*cdf'
    endif else begin
      file_str=eos+'.histograms.*'+month_str+'01-*'+'*cdf'
    endelse
    files=file_search(file_str,count=num_files)
    if num_files gt 0 then print,num_files,files
    if num_files gt 0 then begin
      ;  Loop through the monthly histogram files and put the data in one array
      for j=0,num_files-1 do begin
        print,files[j]
        fid=ncdf_open(files[j])
        for k=0,n_elements(var)-1 do begin
          xstr='vid=ncdf_varid(fid,"'+var[k]+'")'
          result=execute(xstr)
          xstr='ncdf_varget,fid,vid,'+var[k]+'1'
          result=execute(xstr)
        endfor
        ncdf_close,fid
        if j eq 0 then begin
          for k=0,n_elements(var)-1 do begin
            xstr=var[k]+'='+var[k]+'1'
            result=execute(xstr)
          endfor
        endif else begin
          for k=0,n_elements(var)-1 do begin
            xstr=var[k]+'=['+var[k]+','+var[k]+'1]'
            result=execute(xstr)
          endfor         
        endelse      
      endfor  ;end of loop through files
      
      ;*** MODIS VALUES ***
      ;  Now calculate the monthly means
      r_box=where(center_latitude ge llat and center_latitude le ulat and $
        center_longitude ge llon and center_longitude le rlon,c_box)
      ;  north,south of -60S
      r_n60=where(center_latitude gt -60 and center_latitude le ulat and $
        center_longitude ge llon and center_longitude le rlon,c_n60) ;north of -60S
      r_s60=where(center_latitude ge llat and center_latitude le -60 and $
        center_longitude ge llon and center_longitude le rlon,c_s60) ;south of -60S
      ;  Hi,Lo north,south of -60S
      r_ln60=where(center_latitude gt -60 and center_latitude le ulat and $
        center_longitude ge llon and center_longitude le rlon and mean_nd le 50,c_ln60) ;lo north of -60S
      r_hn60=where(center_latitude gt -60 and center_latitude le ulat and $
        center_longitude ge llon and center_longitude le rlon and mean_nd ge 100,c_hn60) ;hi north of -60S
      r_ls60=where(center_latitude ge llat and center_latitude le -60 and $
        center_longitude ge llon and center_longitude le rlon and mean_nd le 50,c_ls60) ;lo south of -60S
      r_hs60=where(center_latitude ge llat and center_latitude le -60 and $
        center_longitude ge llon and center_longitude le rlon and mean_nd ge 100,c_hs60) ;hi south of -60S  
      for r=0,n_elements(region)-1 do begin  
        ;cmonth_mean_nd=mean(mean_nd[ridx])
        ;cmonth_sdev_nd=stddev(mean_nd[ridx])
        ;cmonth_nd_median=nd_median
        ;cmonth_nd_lo_median=median(sub_mean_nd[where(sub_mean_nd le nd_median)])
        ;cmonth_nd_hi_median=median(sub_mean_nd[where(sub_mean_nd gt nd_median)])
        for k=0,n_elements(varname_mod)-1 do begin
          xstr='cmonth_mean_'+varname_mod[k]+'_'+region[r]+'[i]=mean(mean_'+varname_mod[k]+'[r_'+region[r]+'])'
          result=execute(xstr)
          xstr='cmonth_sdev_'+varname_mod[k]+'_'+region[r]+'[i]=stddev(mean_'+varname_mod[k]+'[r_'+region[r]+'])'
          result=execute(xstr)
          ;  Make a histogram
          xstr='data_var=mean_'+varname_mod[k]+'[r_'+region[r]+']'
          result=execute(xstr)
          start_bin=varmin_mod[k]
          end_bin=varmax_mod[k]
          dbin=vardbin_mod[k]
          hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
          ;xstr='cmonth_bins_'+month_name[i]+'_'+varname[k]+'_'+region[r]+'=bins'
          xstr='cmonth_bins_'+varname_mod[k]+'=bins'
          ;print,xstr
          result=execute(xstr)
          xstr='cmonth_count_'+month_name[i]+'_'+varname_mod[k]+'_'+region[r]+'=data_counts'
          ;print,xstr
          result=execute(xstr)
        endfor ;end of loop through variables
       
        xstr='sub_mean_nd=mean_nd[r_'+region[r]+']'
        result=execute(xstr)
        nd_median=median(sub_mean_nd)
        xstr='cmonth_nd_median_'+region[r]+'[i]=nd_median'
        result=execute(xstr)
        xstr='cmonth_nd_lo_median_'+region[r]+'[i]=median(sub_mean_nd[where(sub_mean_nd le nd_median)])'
        result=execute(xstr)
        xstr='cmonth_nd_hi_median_'+region[r]+'[i]=median(sub_mean_nd[where(sub_mean_nd gt nd_median)])'
        result=execute(xstr)
        xstr='cmonth_count_'+region[r]+'[i]=c_'+region[r]
        result=execute(xstr)
      endfor ;end of loop through region
      ;*** CERES Variables
      ;  Now calculate the monthly means
      r_box=where(center_latitude ge llat and center_latitude le ulat and $
        mean_sw_toa ne -9999 and $  ;don't average in these values
        center_longitude ge llon and center_longitude le rlon,c_box)
      ;  north,south of -60S
      r_n60=where(center_latitude gt -60 and center_latitude le ulat and $
        mean_sw_toa ne -9999 and $  ;don't average in these values
        center_longitude ge llon and center_longitude le rlon,c_n60) ;north of -60S
      r_s60=where(center_latitude ge llat and center_latitude le -60 and $
        mean_sw_toa ne -9999 and $  ;don't average in these values
        center_longitude ge llon and center_longitude le rlon,c_s60) ;south of -60S
      ;  Hi,Lo north,south of -60S
      r_ln60=where(center_latitude gt -60 and center_latitude le ulat and $
        mean_sw_toa ne -9999 and $  ;don't average in these values
        center_longitude ge llon and center_longitude le rlon and mean_nd le 50,c_ln60) ;lo north of -60S
      r_hn60=where(center_latitude gt -60 and center_latitude le ulat and $
        mean_sw_toa ne -9999 and $  ;don't average in these values
        center_longitude ge llon and center_longitude le rlon and mean_nd ge 100,c_hn60) ;hi north of -60S
      r_ls60=where(center_latitude ge llat and center_latitude le -60 and $
        mean_sw_toa ne -9999 and $  ;don't average in these values
        center_longitude ge llon and center_longitude le rlon and mean_nd le 50,c_ls60) ;lo south of -60S
      r_hs60=where(center_latitude ge llat and center_latitude le -60 and $
        mean_sw_toa ne -9999 and $  ;don't average in these values
        center_longitude ge llon and center_longitude le rlon and mean_nd ge 100,c_hs60) ;hi south of -60S
      for r=0,n_elements(region)-1 do begin
        ;cmonth_mean_nd=mean(mean_nd[ridx])
        ;cmonth_sdev_nd=stddev(mean_nd[ridx])
        ;cmonth_nd_median=nd_median
        ;cmonth_nd_lo_median=median(sub_mean_nd[where(sub_mean_nd le nd_median)])
        ;cmonth_nd_hi_median=median(sub_mean_nd[where(sub_mean_nd gt nd_median)])
        for k=0,n_elements(varname_cer)-1 do begin
          xstr='cmonth_mean_'+varname_cer[k]+'_'+region[r]+'[i]=mean(mean_'+varname_cer[k]+'[r_'+region[r]+'])'
          ;print,xstr
          result=execute(xstr)
          xstr='cmonth_sdev_'+varname_cer[k]+'_'+region[r]+'[i]=stddev(mean_'+varname_cer[k]+'[r_'+region[r]+'])'
          ;print,xstr
          result=execute(xstr)
          ;  Make a histogram
          xstr='data_var=mean_'+varname_cer[k]+'[r_'+region[r]+']'
          ;print,xstr
          result=execute(xstr)
          print, min(data_var),max(data_var),' ',varname_cer[k],' month',month,' region ',region[r]
          start_bin=varmin_cer[k]
          end_bin=varmax_cer[k]
          ;dbin=(end_bin-start_bin)/300.0
          dbin=vardbin_cer[k]
          hist_generic,data_var,start_bin,end_bin,dbin,bins,data_freq,data_counts
          ;xstr='cmonth_bins_'+month_name[i]+'_'+varname[k]+'_'+region[r]+'=bins'
          xstr='cmonth_bins_'+varname_cer[k]+'=bins'
          ;print,xstr
          result=execute(xstr)
          xstr='cmonth_count_'+month_name[i]+'_'+varname_cer[k]+'_'+region[r]+'=data_counts'
          ;print,xstr
          result=execute(xstr)
        endfor ;end of loop through variables

        xstr='sub_mean_nd=mean_nd[r_'+region[r]+']'
        result=execute(xstr)
        nd_median=median(sub_mean_nd)
        xstr='cmonth_nd_median_'+region[r]+'[i]=nd_median'
        ;print,xstr
        result=execute(xstr)
        xstr='cmonth_nd_lo_median_'+region[r]+'[i]=median(sub_mean_nd[where(sub_mean_nd le nd_median)])'
        ;print,xstr
        result=execute(xstr)
        xstr='cmonth_nd_hi_median_'+region[r]+'[i]=median(sub_mean_nd[where(sub_mean_nd gt nd_median)])'
        ;print,xstr
        result=execute(xstr)
        xstr='cmonth_count_cer_'+region[r]+'[i]=c_'+region[r]
        ;print,xstr
        result=execute(xstr)
      endfor ;end of loop through region
  
    endif  ;end of found month histogram files
  endfor  ;end of loop through months
  
  ;*****************************
  ;  write the data into a file
  ;*****************************
  do_write='yes'
  if do_write eq 'yes' then begin
    num=n_elements(all_julian_day)
    num_1d=n_elements(all_julian_day_1d)
    
    fid=ncdf_create(output_file,/clobber)
    num_1m_did=ncdf_dimdef(fid,'num_1m',num)
    num_1d_did=ncdf_dimdef(fid,'num_1d',num_1d)
    num_cmonth_did=ncdf_dimdef(fid,'num_cmonth',num_months)
    
    julian_day_1m_id=ncdf_vardef(fid,'julian_day_monthly',num_1m_did,/double)
    julian_day_1d_id=ncdf_vardef(fid,'julian_day_daily',num_1d_did,/double)
    
    ;  Create monthly variables
    for r=0,n_elements(region)-1 do begin
      for k=0,n_elements(varname)-1 do begin
        xstr='monthly_mean_'+varname[k]+'_'+region[r]+'_id=ncdf_vardef(fid,"monthly_mean_'+$
          varname[k]+'_'+region[r]+'",num_1m_did)'
        result=execute(xstr)
        xstr='monthly_sdev_'+varname[k]+'_'+region[r]+'_id=ncdf_vardef(fid,"monthly_sdev_'+$
          varname[k]+'_'+region[r]+'",num_1m_did)'
        result=execute(xstr)
      endfor ;end of loop through variables
      xstr='monthly_nd_median_'+region[r]+'_id=ncdf_vardef(fid,"monthly_nd_median_'+region[r]+'",num_1m_did)'
      result=execute(xstr)
      xstr='monthly_nd_lo_median_'+region[r]+'_id=ncdf_vardef(fid,"monthly_nd_lo_median_'+region[r]+'",num_1m_did)'
      result=execute(xstr)
      xstr='monthly_nd_hi_median_'+region[r]+'_id=ncdf_vardef(fid,"monthly_nd_hi_median_'+region[r]+'",num_1m_did)'
      result=execute(xstr)
      xstr='monthly_count_'+region[r]+'_id=ncdf_vardef(fid,"monthly_count_'+region[r]+'",num_1m_did)'
      result=execute(xstr)
      xstr='monthly_count_cer_'+region[r]+'_id=ncdf_vardef(fid,"monthly_count_cer_'+region[r]+'",num_1m_did)'
      result=execute(xstr)
    endfor ;end of loop through region 
    
    ;  Create daily variables
    for r=0,n_elements(region)-1 do begin
      for k=0,n_elements(varname)-1 do begin
        xstr='daily_mean_'+varname[k]+'_'+region[r]+'_id=ncdf_vardef(fid,"daily_mean_'+$
          varname[k]+'_'+region[r]+'",num_1d_did)'
        result=execute(xstr)
        xstr='daily_sdev_'+varname[k]+'_'+region[r]+'_id=ncdf_vardef(fid,"daily_sdev_'+$
          varname[k]+'_'+region[r]+'",num_1d_did)'
        result=execute(xstr)
      endfor ;end of loop through variables
      xstr='daily_count_'+region[r]+'_id=ncdf_vardef(fid,"daily_count_'+region[r]+'",num_1d_did)'
      result=execute(xstr)     
      xstr='daily_count_cer_'+region[r]+'_id=ncdf_vardef(fid,"daily_count_cer_'+region[r]+'",num_1d_did)'
      result=execute(xstr)
    endfor ;end of loop through region
  
    ;  Create composite month variables
    ;  Create dimensions and bins for histograms
    for k=0,n_elements(varname)-1 do begin
      xstr='num_bins_'+varname[k]+'_did=ncdf_dimdef(fid,"num_bins_'+varname[k]+'",'+$
        'n_elements(cmonth_bins_'+varname[k]+'))'
      result=execute(xstr)
      xstr='cmonth_bins_'+varname[k]+'_id=ncdf_vardef(fid,"cmonth_bins_'+$
        varname[k]+'",num_bins_'+varname[k]+'_did)'
      result=execute(xstr) 
    endfor
    ;  Create variables
    for r=0,n_elements(region)-1 do begin
      for k=0,n_elements(varname)-1 do begin
        xstr='cmonth_mean_'+varname[k]+'_'+region[r]+'_id=ncdf_vardef(fid,"cmonth_mean_'+$
          varname[k]+'_'+region[r]+'",num_cmonth_did)'
        result=execute(xstr)
        xstr='cmonth_sdev_'+varname[k]+'_'+region[r]+'_id=ncdf_vardef(fid,"cmonth_sdev_'+$
          varname[k]+'_'+region[r]+'",num_cmonth_did)'
        result=execute(xstr)
        for m=0,num_months-1 do begin
          xstr='cmonth_count_'+month_name[m]+'_'+varname[k]+'_'+region[r]+'_id=ncdf_vardef(fid,"cmonth_count_'+$
            month_name[m]+'_'+varname[k]+'_'+region[r]+'",num_bins_'+varname[k]+'_did)'
          result=execute(xstr)
        endfor  ;end of loop through months
      endfor ;end of loop through variables
      xstr='cmonth_nd_median_'+region[r]+'_id=ncdf_vardef(fid,"cmonth_nd_median_'+region[r]+'",num_cmonth_did)'
      result=execute(xstr)
      xstr='cmonth_nd_lo_median_'+region[r]+'_id=ncdf_vardef(fid,"cmonth_nd_lo_median_'+region[r]+'",num_cmonth_did)'
      result=execute(xstr)
      xstr='cmonth_nd_hi_median_'+region[r]+'_id=ncdf_vardef(fid,"cmonth_nd_hi_median_'+region[r]+'",num_cmonth_did)'
      result=execute(xstr)
      xstr='cmonth_count_'+region[r]+'_id=ncdf_vardef(fid,"cmonth_count_'+region[r]+'",num_cmonth_did)'
      result=execute(xstr)
      xstr='cmonth_count_cer_'+region[r]+'_id=ncdf_vardef(fid,"cmonth_count_cer_'+region[r]+'",num_cmonth_did)'
      result=execute(xstr)
    endfor ;end of loop through region
    ncdf_control,fid,/endef
  
    ncdf_varput,fid,julian_day_1m_id,all_julian_day
    ncdf_varput,fid,julian_day_1d_id,all_julian_day_1d

    ;  Write monthly variables
    for r=0,n_elements(region)-1 do begin
      for k=0,n_elements(varname)-1 do begin
        xstr='ncdf_varput,fid,monthly_mean_'+varname[k]+'_'+region[r]+'_id,all_month_mean_'+$
          varname[k]+'_'+region[r]
        result=execute(xstr)
        xstr='ncdf_varput,fid,monthly_sdev_'+varname[k]+'_'+region[r]+'_id,all_month_sdev_'+$
          varname[k]+'_'+region[r]
        result=execute(xstr)
      endfor ;end of loop through variables
      xstr='ncdf_varput,fid,monthly_nd_median_'+region[r]+'_id,all_month_nd_median_'+region[r]
      result=execute(xstr)
      xstr='ncdf_varput,fid,monthly_nd_lo_median_'+region[r]+'_id,all_month_nd_lo_median_'+region[r]
      result=execute(xstr)
      xstr='ncdf_varput,fid,monthly_nd_hi_median_'+region[r]+'_id,all_month_nd_hi_median_'+region[r]
      result=execute(xstr)
      xstr='ncdf_varput,fid,monthly_count_'+region[r]+'_id,all_month_count_'+region[r]
      result=execute(xstr)
      xstr='ncdf_varput,fid,monthly_count_cer_'+region[r]+'_id,all_month_count_cer_'+region[r]
      result=execute(xstr)
    endfor ;end of loop through region
  
    ;  Write daily variables
    for r=0,n_elements(region)-1 do begin
      for k=0,n_elements(varname)-1 do begin
        xstr='ncdf_varput,fid,daily_mean_'+varname[k]+'_'+region[r]+'_id,all_daily_mean_'+$
          varname[k]+'_'+region[r]
        result=execute(xstr)
        xstr='ncdf_varput,fid,daily_sdev_'+varname[k]+'_'+region[r]+'_id,all_daily_sdev_'+$
          varname[k]+'_'+region[r]
        result=execute(xstr)
      endfor ;end of loop through variables
      xstr='ncdf_varput,fid,daily_count_'+region[r]+'_id,all_daily_count_'+region[r]
      result=execute(xstr)
      xstr='ncdf_varput,fid,daily_count_cer_'+region[r]+'_id,all_daily_count_cer_'+region[r]
      result=execute(xstr)
    endfor ;end of loop through region  
  
    ;  Write composite month variables
    for k=0,n_elements(varname)-1 do begin
      xstr='ncdf_varput,fid,cmonth_bins_'+varname[k]+'_id,cmonth_bins_'+varname[k]
      result=execute(xstr)
    endfor
    for r=0,n_elements(region)-1 do begin
      for k=0,n_elements(varname)-1 do begin
        xstr='ncdf_varput,fid,cmonth_mean_'+varname[k]+'_'+region[r]+'_id,cmonth_mean_'+$
          varname[k]+'_'+region[r]
        result=execute(xstr)
        xstr='ncdf_varput,fid,cmonth_sdev_'+varname[k]+'_'+region[r]+'_id,cmonth_sdev_'+$
          varname[k]+'_'+region[r]
        result=execute(xstr)
        for m=0,num_months-1 do begin
          xstr='ncdf_varput,fid,cmonth_count_'+month_name[m]+'_'+varname[k]+'_'+region[r]+'_id,cmonth_count_'+$
            month_name[m]+'_'+varname[k]+'_'+region[r]
          result=execute(xstr)
        endfor  ;end of loop through months
      endfor ;end of loop through variables
      xstr='ncdf_varput,fid,cmonth_nd_median_'+region[r]+'_id,cmonth_nd_median_'+region[r]
      result=execute(xstr)
      xstr='ncdf_varput,fid,cmonth_nd_lo_median_'+region[r]+'_id,cmonth_nd_lo_median_'+region[r]
      result=execute(xstr)
      xstr='ncdf_varput,fid,cmonth_nd_hi_median_'+region[r]+'_id,cmonth_nd_hi_median_'+region[r]
      result=execute(xstr)
      xstr='ncdf_varput,fid,cmonth_count_'+region[r]+'_id,cmonth_count_'+region[r]
      result=execute(xstr)
      xstr='ncdf_varput,fid,cmonth_count_cer_'+region[r]+'_id,cmonth_count_cer_'+region[r]
      result=execute(xstr)
    endfor ;end of loop through region
    ncdf_close,fid
    
  endif  ;end of do_write eq 'yes'
endif else begin ;need to make the output file
  ;*************************************
  ;  Read previously created output file
  print,'found output file'
  fid=ncdf_open(output_file)
  vid=ncdf_varid(fid,'julian_day_monthly') & ncdf_varget,fid,vid,all_julian_day
  vid=ncdf_varid(fid,'julian_day_daily') & ncdf_varget,fid,vid,all_julian_day_1d
  
  ;  Monthly variables
  for r=0,n_elements(region)-1 do begin
    for k=0,n_elements(varname)-1 do begin
      xstr='vid=ncdf_varid(fid,"monthly_mean_'+varname[k]+'_'+region[r]+'")'
      result=execute(xstr)
      xstr='ncdf_varget,fid,vid,monthly_mean_'+varname[k]+'_'+region[r]
      result=execute(xstr)
      xstr='vid=ncdf_varid(fid,"monthly_sdev_'+varname[k]+'_'+region[r]+'")'
      result=execute(xstr)
      xstr='ncdf_varget,fid,vid,monthly_sdev_'+varname[k]+'_'+region[r]
      result=execute(xstr)
    endfor ;end of loop through variables
    xstr='vid=ncdf_varid(fid,"monthly_nd_median_'+region[r]+'")'
    result=execute(xstr)
    xstr='ncdf_varget,fid,vid,monthly_nd_median_'+region[r]
    result=execute(xstr)
    xstr='vid=ncdf_varid(fid,"monthly_nd_lo_median_'+region[r]+'")'
    result=execute(xstr)
    xstr='ncdf_varget,fid,vid,monthly_nd_lo_median_'+region[r]
    result=execute(xstr)
    xstr='vid=ncdf_varid(fid,"monthly_nd_hi_median_'+region[r]+'")'
    result=execute(xstr)
    xstr='ncdf_varget,fid,vid,monthly_nd_hi_median_'+region[r]
    result=execute(xstr)
    xstr='vid=ncdf_varid(fid,"monthly_count_'+region[r]+'")'
    result=execute(xstr)
    xstr='ncdf_varget,fid,vid,monthly_count_'+region[r]
    result=execute(xstr)
  endfor ;end of loop through region
  
  ;  Create daily variables
  for r=0,n_elements(region)-1 do begin
    for k=0,n_elements(varname)-1 do begin
      xstr='vid=ncdf_varid(fid,"daily_mean_'+varname[k]+'_'+region[r]+'")'
      result=execute(xstr)
      xstr='ncdf_varget,fid,vid,all_daily_mean_'+varname[k]+'_'+region[r]
      result=execute(xstr)
      xstr='vid=ncdf_varid(fid,"daily_sdev_'+varname[k]+'_'+region[r]+'")'
      result=execute(xstr)
      xstr='ncdf_varget,fid,vid,all_daily_sdev_'+varname[k]+'_'+region[r]
      result=execute(xstr)
    endfor ;end of loop through variables
    xstr='vid=ncdf_varid(fid,"daily_count_'+region[r]+'")'
    result=execute(xstr)
    xstr='ncdf_varget,fid,vid,all_daily_count_'+region[r]
    result=execute(xstr) 
  endfor ;end of loop through region
  
  ncdf_close,fid
endelse
;********************************************
;  Daily means plot
;********************************************

;  Creating monthly tick marks for the daily data
numtimes=n_elements(all_julian_day_1d)
caldat,all_julian_day_1d,mm,dd,yy,hh,mi,ss
mname=strarr(numtimes)
r=where(mm eq 11,c)
mname[r]='Nov'
r=where(mm eq 12,c)
mname[r]='Dec'
r=where(mm eq 1,c)
mname[r]='Jan'
r=where(mm eq 2,c)
mname[r]='Feb'

;xticknames=mname+' '+string(yy,format='(I4)')
idx=make_array(25,/int,value=-1)
idx_year=make_array(25,/string)
idx_month=make_array(25,/string)
for i=0,n_elements(all_julian_day)-1 do begin
  r=where(all_julian_day_1d ge all_julian_day[i],c)
  idx[i]=r[0]
  idx_year[i]=string(yy[r[0]],format='(I4)')
  idx_month[i]=mname[r[0]]
endfor
idx=idx[0:i-1]
idx_year=idx_year[0:i-1]
idx_month=idx_month[0:i-1]
xticknames=idx_month+' '+idx_year
blanks=make_array(n_elements(xticknames),/string)

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
;  Daily time series
;*************************
pxdim=900 & pydim=900
xl=0.09 & xr=0.95
yb=0.10 & yt=0.95
sx=0.08
sy=0.01
numplots_x=1
numplots_y=9
position_plots,xl,xr,yb,yt,sx,sy,numplots_x,numplots_y,pos
;  Colorbar position
cbpos=pos
cbpos[*,0]=pos[*,2]-0.02
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

pnum=0
r=where(all_daily_count_box ne -9999,c)
dmin=min(all_daily_count_box[r])
dmax=max(all_daily_count_box[r])
xarray=findgen(numtimes)
p0=plot(xarray,all_daily_count_box,/current,position=pos[pnum,*],$
  /xstyle,ytitle='# boxes!Cper day',font_size=fs1,yrange=[dmin,dmax],$
  ;xtickformat='label_date',xtickunits='Months',xtickinterval=1,$
  xtickname=xticknames,xtext_orientation=90,xtickvalues=idx,$
  sym_size=0.3,symbol='o',/sym_filled)
r=where(mm eq 11,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_count_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='red',linestyle=6)  
endif  
r=where(mm eq 12,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_count_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='green',linestyle=6)
endif  
r=where(mm eq 1,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_count_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='blue',linestyle=6)
endif
r=where(mm eq 2,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_count_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='purple',linestyle=6)
endif

pnum=1
r=where(all_daily_mean_nd_box ne -9999,c)
dmin=min(all_daily_mean_nd_box[r])
dmax=max(all_daily_mean_nd_box[r])
xarray=findgen(numtimes)
p0=plot(xarray,all_daily_mean_nd_box,/current,position=pos[pnum,*],$
  /xstyle,ytitle='Nd',font_size=fs1,yrange=[dmin,dmax],$
  ;xtickformat='label_date',xtickunits='Months',xtickinterval=1,$
  xtickname=blanks,xtext_orientation=90,xtickvalues=idx,$
  sym_size=0.3,symbol='o',/sym_filled)
r=where(mm eq 11,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_nd_box[r],$
  ;p1=errorplot(xarray[r],all_daily_mean_nd[r],all_daily_sdev_nd[r],$
    /overplot,sym_size=0.3,symbol='o',/sym_filled,color='red',linestyle=6)
endif
r=where(mm eq 12,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_nd_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='green',linestyle=6)
endif
r=where(mm eq 1,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_nd_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='blue',linestyle=6)
endif
r=where(mm eq 2,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_nd_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='purple',linestyle=6)
endif

pnum=2
r=where(all_daily_mean_re_box ne -9999,c)
dmin=min(all_daily_mean_re_box[r])
dmax=max(all_daily_mean_re_box[r])
xarray=findgen(numtimes)
p0=plot(xarray,all_daily_mean_re_box,/current,position=pos[pnum,*],$
  /xstyle,ytitle='Re',font_size=fs1,yrange=[dmin,dmax],$
  ;xtickformat='label_date',xtickunits='Months',xtickinterval=1,$
  xtickname=blanks,xtext_orientation=90,xtickvalues=idx,$
  sym_size=0.3,symbol='o',/sym_filled)
r=where(mm eq 11,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_re_box[r],$
    ;p1=errorplot(xarray[r],all_daily_mean_nd[r],all_daily_sdev_nd[r],$
    /overplot,sym_size=0.3,symbol='o',/sym_filled,color='red',linestyle=6)
endif
r=where(mm eq 12,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_re_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='green',linestyle=6)
endif
r=where(mm eq 1,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_re_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='blue',linestyle=6)
endif
r=where(mm eq 2,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_re_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='purple',linestyle=6)
endif

pnum=3
r=where(all_daily_mean_lwp_box ne -9999,c)
dmin=min(all_daily_mean_lwp_box[r])
dmax=max(all_daily_mean_lwp_box[r])
xarray=findgen(numtimes)
p0=plot(xarray,all_daily_mean_lwp_box,/current,position=pos[pnum,*],$
  /xstyle,ytitle='LWP',font_size=fs1,yrange=[dmin,dmax],$
  ;xtickformat='label_date',xtickunits='Months',xtickinterval=1,$
  xtickname=blanks,xtext_orientation=90,xtickvalues=idx,$
  sym_size=0.3,symbol='o',/sym_filled)
r=where(mm eq 11,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_lwp_box[r],$
    ;p1=errorplot(xarray[r],all_daily_mean_nd[r],all_daily_sdev_nd[r],$
    /overplot,sym_size=0.3,symbol='o',/sym_filled,color='red',linestyle=6)
endif
r=where(mm eq 12,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_lwp_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='green',linestyle=6)
endif
r=where(mm eq 1,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_lwp_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='blue',linestyle=6)
endif
r=where(mm eq 2,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_lwp_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='purple',linestyle=6)
endif

pnum=4
r=where(all_daily_mean_tau_box ne -9999,c)
dmin=min(all_daily_mean_tau_box[r])
dmax=max(all_daily_mean_tau_box[r])
xarray=findgen(numtimes)
p0=plot(xarray,all_daily_mean_tau_box,/current,position=pos[pnum,*],$
  /xstyle,ytitle='TAU',font_size=fs1,yrange=[dmin,dmax],$
  ;xtickformat='label_date',xtickunits='Months',xtickinterval=1,$
  xtickname=blanks,xtext_orientation=90,xtickvalues=idx,$
  sym_size=0.3,symbol='o',/sym_filled)
r=where(mm eq 11,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_tau_box[r],$
    ;p1=errorplot(xarray[r],all_daily_mean_nd[r],all_daily_sdev_nd[r],$
    /overplot,sym_size=0.3,symbol='o',/sym_filled,color='red',linestyle=6)
endif
r=where(mm eq 12,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_tau_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='green',linestyle=6)
endif
r=where(mm eq 1,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_tau_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='blue',linestyle=6)
endif
r=where(mm eq 2,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_tau_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='purple',linestyle=6)
endif

if 1 eq 1 then begin
pnum=5
r=where(all_daily_mean_solar_zenith_box ne -9999,c)
dmin=min(all_daily_mean_solar_zenith_box[r])
dmax=max(all_daily_mean_solar_zenith_box[r])
xarray=findgen(numtimes)
p0=plot(xarray,all_daily_mean_solar_zenith_box,/current,position=pos[pnum,*],$
  /xstyle,ytitle='SZA',font_size=fs1,yrange=[dmin,dmax],$
  ;xtickformat='label_date',xtickunits='Months',xtickinterval=1,$
  xtickname=blanks,xtext_orientation=90,xtickvalues=idx,$
  sym_size=0.3,symbol='o',/sym_filled)
r=where(mm eq 11,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_solar_zenith_box[r],$
    ;p1=errorplot(xarray[r],all_daily_mean_nd[r],all_daily_sdev_nd[r],$
    /overplot,sym_size=0.3,symbol='o',/sym_filled,color='red',linestyle=6)
endif
r=where(mm eq 12,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_solar_zenith_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='green',linestyle=6)
endif
r=where(mm eq 1,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_solar_zenith_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='blue',linestyle=6)
endif
r=where(mm eq 2,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_solar_zenith_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='purple',linestyle=6)
endif

pnum=6
r=where(all_daily_mean_sw_toa_box ne -9999,c)
dmin=min(all_daily_mean_sw_toa_box[r])
dmax=max(all_daily_mean_sw_toa_box[r])
xarray=findgen(numtimes)
p0=plot(xarray,all_daily_mean_sw_toa_box,/current,position=pos[pnum,*],$
  /xstyle,ytitle='SW TOA',font_size=fs1,yrange=[dmin,dmax],$
  ;xtickformat='label_date',xtickunits='Months',xtickinterval=1,$
  xtickname=blanks,xtext_orientation=90,xtickvalues=idx,$
  sym_size=0.3,symbol='o',/sym_filled)
r=where(mm eq 11,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_toa_box[r],$
    ;p1=errorplot(xarray[r],all_daily_mean_nd[r],all_daily_sdev_nd[r],$
    /overplot,sym_size=0.3,symbol='o',/sym_filled,color='red',linestyle=6)
endif
r=where(mm eq 12,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_toa_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='green',linestyle=6)
endif
r=where(mm eq 1,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_toa_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='blue',linestyle=6)
endif
r=where(mm eq 2,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_toa_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='purple',linestyle=6)
endif

pnum=7
r=where(all_daily_mean_sw_up_box ne -9999,c)
dmin=min(all_daily_mean_sw_up_box[r])
dmax=max(all_daily_mean_sw_up_box[r])
xarray=findgen(numtimes)
p0=plot(xarray,all_daily_mean_sw_up_box,/current,position=pos[pnum,*],$
  /xstyle,ytitle='SW UP',font_size=fs1,yrange=[dmin,dmax],$
  ;xtickformat='label_date',xtickunits='Months',xtickinterval=1,$
  xtickname=blanks,xtext_orientation=90,xtickvalues=idx,$
  sym_size=0.3,symbol='o',/sym_filled)
r=where(mm eq 11,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_up_box[r],$
    ;p1=errorplot(xarray[r],all_daily_mean_nd[r],all_daily_sdev_nd[r],$
    /overplot,sym_size=0.3,symbol='o',/sym_filled,color='red',linestyle=6)
endif
r=where(mm eq 12,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_up_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='green',linestyle=6)
endif
r=where(mm eq 1,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_up_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='blue',linestyle=6)
endif
r=where(mm eq 2,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_up_box[r],/overplot,sym_size=0.3,symbol='o',$
    /sym_filled,color='purple',linestyle=6)
endif

pnum=8
r=where(all_daily_mean_sw_alb_box ne -9999,c)
dmin=min(all_daily_mean_sw_alb_box[r])
dmax=max(all_daily_mean_sw_alb_box[r])
xarray=findgen(numtimes)
p0=plot(xarray,all_daily_mean_sw_alb_box,/current,position=pos[pnum,*],$
  /xstyle,ytitle='SW Albedo',font_size=fs1,yrange=[dmin,dmax],$
  ;xtickformat='label_date',xtickunits='Months',xtickinterval=1,$
  xtickname=blanks,xtext_orientation=90,xtickvalues=idx)
r=where(mm eq 11,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_alb_box[r],$
    ;p1=errorplot(xarray[r],all_daily_mean_nd[r],all_daily_sdev_nd[r],$
    /overplot,sym_size=0.3,symbol='o',color='red',linestyle=6)
endif
r=where(mm eq 12,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_alb_box[r],/overplot,sym_size=0.3,symbol='o',$
    color='green',linestyle=6)
endif
r=where(mm eq 1,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_alb_box[r],/overplot,sym_size=0.3,symbol='o',$
    color='blue',linestyle=6)
endif
r=where(mm eq 2,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_alb_box[r],/overplot,sym_size=0.3,symbol='o',$
    color='purple',linestyle=6)
endif

p2=plot(xarray,all_daily_mean_sw_nalb_box,/overplot)
r=where(mm eq 11,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_nalb_box[r],$
    /overplot,sym_size=0.4,symbol='triangle',color='red',linestyle=6)
endif
r=where(mm eq 12,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_nalb_box[r],/overplot,sym_size=0.4,symbol='triangle',$
    color='green',linestyle=6)
endif
r=where(mm eq 1,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_nalb_box[r],/overplot,sym_size=0.4,symbol='triangle',$
    color='blue',linestyle=6)
endif
r=where(mm eq 2,c)
if c gt 0 then begin
  p1=plot(xarray[r],all_daily_mean_sw_nalb_box[r],/overplot,sym_size=0.4,symbol='triangle',$
    color='purple',linestyle=6)
endif
t1=text(pos[pnum,0]+23*dx,pos[pnum,3]+1*dy,'triangle=normalized albedo',font_size=fs2)
endif

;t1=text(pos[pnum,0],pos[pnum,3]+1*dy,eos+' Daily Means',font_size=fs2)
t1=text(xl,yt+1*dy,eos+' Daily Means',font_size=fs2)

p0.save,'histograms_daily.'+time_range_str+'.'+box_str+'.'+eos+'.3.png'

stop


end