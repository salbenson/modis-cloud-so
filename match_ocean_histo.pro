;***************************
;  Code to match chlor_a to Nd
;***************************

pro match_ocean_histo

;path_prefix='/Volumes/'  ;imac
path_prefix='/uufs/chpc.utah.edu/common/home/'  ;chpc

;  Ocean color product
;cproduct='L3m_DAY_CHL_chlor_a_4km'  ;first results
;cproduct='L3m_DAY_CHL_chlor_a_9km'
cproduct='L3m_8D_CHL_chlor_a_4km'

;  Ocean color directory
cdir=path_prefix+'mace-group6/modis/ocean_color/'+cproduct+'/'

;  Modis histogram directory
hdir=path_prefix+'mace-group4/modis/hysplit/modis_histograms_sm/'

;  Choose aqua or terra
;eos='MYD'
eos='MOD'
;eos='MOYD'  ;both don't use this one for this step

;  Time range to analyze
;  SEASON Nov 2018-Feb 2019
;julian_day_1d=timegen(start=julday(11,1,2018,0,0,0),final=julday(11,30,2018,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(12,1,2018,0,0,0),final=julday(12,31,2018,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(1,1,2019,0,0,0),final=julday(1,31,2019,23,59,59),units='days',step_size=1)
julian_day_1d=timegen(start=julday(2,14,2019,0,0,0),final=julday(2,14,2019,23,59,59),units='days',step_size=1)

;  SEASON Nov 2017-Feb 2018
;julian_day_1d=timegen(start=julday(11,3,2017,0,0,0),final=julday(11,3,2017,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(12,1,2017,0,0,0),final=julday(12,1,2017,23,59,59),units='days',step_size=1)
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
output_file=eos+'.chlor_match.'+cproduct+'.'+time_range_str+'.test.cdf'
print,output_file

;  If using 8 day files, create 8 day time array
if cproduct eq 'L3m_8D_CHL_chlor_a_4km' then begin
  julian_day_8d=timegen(start=julday(1,1,yy[0],0,0,0),final=julday(12,31,yy[0],23,0,0),units='days',step_size=8)
  numtimes_8d=n_elements(julian_day_8d)
  caldat,julian_day_8d,mm8,dd8,yy8,hh8,mi8,ss8
  doy8=make_array(/int,numtimes_8d,value=-9999)
  for i=0,n_elements(julian_day_8d)-1 do begin
    julian_date,yy8[i],mm8[i],dd8[i],doy1
    doy8[i]=doy1
    ;print,yy8[i],mm8[i],dd8[i],doy8[i]
  endfor
endif

;  If the output file does not exist, then create it
if file_test(output_file) eq 1 or file_test(output_file) eq 0 then begin

  print,'making output file'

  ;  Now get the data for the time range
  all_mean_nd=!NULL
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
    print,num_files,' histogram files for ',doy[i]
    
    ;  If there are histogram files for the day
    if num_files gt 0 then begin
      
      ;  Get the ocean color file data for the day
      chlor_a1=!NULL
      clat1=!NULL
      clon1=!NULL
      if cproduct eq 'L3m_DAY_CHL_chlor_a_4km' then begin
        cname='A'+string(yy[i],format='(I4)')+string(doy[i],format='(I03)')+'.'+cproduct+'.nc'
      endif else if cproduct eq 'L3m_8D_CHL_chlor_a_4km' then begin
        ;print,yy[i],mm[i],dd[i],hh[i],mi[i],ss[i]
        r=where(julian_day_1d[i] ge julian_day_8d,c)
        idx=r[-1]
        ;print,yy8[idx],mm8[idx],dd8[idx],doy8[idx]
        cname='A'+string(yy8[idx],format='(I4)')+string(doy8[idx],format='(I03)')+'*.'+cproduct+'.nc'
        cnames=file_search(cdir+cname,count=num_cnames)
        cname=file_basename(cnames[0])
      endif
      print,cname
      fid=ncdf_open(cdir+cname)
      vid=ncdf_varid(fid,'chlor_a') & ncdf_varget,fid,vid,chlor_a1
      vid=ncdf_varid(fid,'lat') & ncdf_varget,fid,vid,clat1
      vid=ncdf_varid(fid,'lon') & ncdf_varget,fid,vid,clon1
      ncdf_close,fid

      ;  Make arrays to hold the values for each histogram
      julian_day=make_array(/double,num_files,value=-9999)
      mean_nd=make_array(/float,num_files,value=-9999)
      ;mean_re=make_array(/float,num_files,value=-9999)
      ;mean_lwp=make_array(/float,num_files,value=-9999)
      mean_lat=make_array(/float,num_files,value=-9999)
      mean_lon=make_array(/float,num_files,value=-9999)
      ;mean_temp=make_array(/float,num_files,value=-9999)
      ;median_temp=make_array(/float,num_files,value=-9999)
      ;tenth_temp=make_array(/float,num_files,value=-9999)
      ;cold_flag=make_array(/float,num_files,value=-9999)
      clat=make_array(/float,num_files,value=-9999)
      clon=make_array(/float,num_files,value=-9999)
      chlor_a=make_array(/float,num_files,value=-9999)
      filename=make_array(/string,num_files)

      if all_mean_nd eq !NULL then begin
        all_julian_day=julian_day
        all_mean_nd=mean_nd
        ;all_mean_re=mean_re
        ;all_mean_lwp=mean_lwp
        all_mean_lat=mean_lat
        all_mean_lon=mean_lon
        ;all_mean_temp=mean_temp
        ;all_median_temp=median_temp
        ;all_tenth_temp=tenth_temp
        ;all_cold_flag=cold_flag
        all_clat=clat
        all_clon=clon
        all_chlor_a=chlor_a
        all_filename=filename
      endif else begin
        all_julian_day=[all_julian_day,julian_day]
        all_mean_nd=[all_mean_nd,mean_nd]
        ;all_mean_re=[all_mean_re,mean_re]
        ;all_mean_lwp=[all_mean_lwp,mean_lwp]
        all_mean_lat=[all_mean_lat,mean_lat]
        all_mean_lon=[all_mean_lon,mean_lon]
        ;all_mean_temp=[all_mean_temp,mean_temp]
        ;all_median_temp=[all_median_temp,median_temp]
        ;all_tenth_temp=[all_tenth_temp,tenth_temp]
        ;all_cold_flag=[all_cold_flag,cold_flag]
        all_clat=[all_clat,clat]
        all_clon=[all_clon,clon]
        all_chlor_a=[all_chlor_a,chlor_a]
        all_filename=[all_filename,filename]
      endelse
      for f=0,num_files-1 do begin
        if f mod 200 eq 0 then print,files[f]
        fid=ncdf_open(files[f],/write)
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
        ;=ncdf_varid(fid,'view_zenith_histo') & ncdf_varget,fid,vid,view_zenith_histo
        vid=ncdf_varid(fid,'mean_latitude') & ncdf_varget,fid,vid,mean_lat1
        vid=ncdf_varid(fid,'mean_longitude') & ncdf_varget,fid,vid,mean_lon1
        vid=ncdf_varid(fid,'mean_nd') & ncdf_varget,fid,vid,mean_nd1
        ;vid=ncdf_varid(fid,'mean_re') & ncdf_varget,fid,vid,mean_re1
        ;vid=ncdf_varid(fid,'mean_lwp') & ncdf_varget,fid,vid,mean_lwp1
        ;vid=ncdf_varid(fid,'mean_cloud_top_temp') & ncdf_varget,fid,vid,mean_temp1
        ;vid=ncdf_varid(fid,'median_cloud_top_temp') & ncdf_varget,fid,vid,median_temp1
        ;vid=ncdf_varid(fid,'tenth_percentile_cloud_top_temp') & ncdf_varget,fid,vid,tenth_temp1
        ;vid=ncdf_varid(fid,'cold_flag') & ncdf_varget,fid,vid,cold_flag1
        
        ;  convert mean_lon1 from colon to lon
        mean_colon1=mean_lon1
        if mean_colon1 gt 180 then mean_lon1=mean_lon1-360.0
        
        ;  Find the matching ocean color for the histogram
        lat_close=min(abs(mean_lat1-clat1),lat_idx)
        lon_close=min(abs(mean_lon1-clon1),lon_idx)
        ;print,mean_lat1,clat1[lat_idx],mean_lon1,clon1[lon_idx]
        
        ;  Write the matching ocean color to the file
        cid=ncdf_varid(fid,cproduct)
        if cid eq -1 then begin
          ncdf_control,fid,/redef
          cid=ncdf_vardef(fid,cproduct)
          ncdf_attput,fid,cid,"long_name","Chlorophyll Concentration, OCI Algorithm"
          ncdf_attput,fid,cid,"units","mg m^-3"
          ncdf_attput,fid,cid,"file",cname
          ncdf_control,fid,/endef
        endif
        la_id=ncdf_varid(fid,cproduct+'_lat')
        if la_id eq -1 then begin
          ncdf_control,fid,/redef
          la_id=ncdf_vardef(fid,cproduct+'_lat')
          ncdf_control,fid,/endef
        endif
        lo_id=ncdf_varid(fid,cproduct+'_lon')
        if lo_id eq -1 then begin
          ncdf_control,fid,/redef
          lo_id=ncdf_vardef(fid,cproduct+'_lon')
          ncdf_control,fid,/endef
        endif
        ncdf_varput,fid,cid,chlor_a1[lon_idx,lat_idx]
        ncdf_varput,fid,la_id,clat1[lat_idx]
        ncdf_varput,fid,lo_id,clon1[lon_idx]
        ncdf_close,fid
        
        if lat_close ge 0.02 and lon_close ge 0.02 then print,lat_close,lon_close
        if lat_close lt 0.02 and lon_close lt 0.02 then begin
         
          all_clat[j+f]=clat1[lat_idx]
          all_clon[j+f]=clon1[lon_idx]
          all_chlor_a[j+f]=chlor_a1[lon_idx,lat_idx]

          all_julian_day[j+f]=julian_day1
          all_mean_nd[j+f]=mean_nd1
          ;all_mean_re[j+f]=mean_re1
          ;all_mean_lwp[j+f]=mean_lwp1
          all_mean_lat[j+f]=mean_lat1
          all_mean_lon[j+f]=mean_lon1

          ;all_mean_temp[j+f]=mean_temp1
          ;all_median_temp[j+f]=median_temp1
          ;all_tenth_temp[j+f]=tenth_temp1
          ;all_cold_flag[j+f]=cold_flag1
          all_filename[j+f]=file_basename(files[f])
        endif
      endfor
      j=j+f
    endif  ;found files for this day
  endfor  ;end of loop through days
  num_hists=n_elements(all_julian_day)

  ; write the data into a file
  cdfid=ncdf_create(output_file,/clobber)
  num_did=ncdf_dimdef(cdfid,'num',num_hists)
  julian_day_id=ncdf_vardef(cdfid,'julian_day',num_did,/double)
  mean_lat_id=ncdf_vardef(cdfid, 'mean_latitude',num_did, /float)
  mean_lon_id=ncdf_vardef(cdfid, 'mean_longitude',num_did, /float)
  mean_nd_id=ncdf_vardef(cdfid, 'mean_nd',num_did, /float)
  ;mean_re_id=ncdf_vardef(cdfid, 'mean_re',num_did, /float)
  ;mean_lwp_id=ncdf_vardef(cdfid, 'mean_lwp',num_did, /float)
  ;mean_temp_id=ncdf_vardef(cdfid, 'mean_cloud_top_temp',num_did, /float)
  ;median_temp_id=ncdf_vardef(cdfid, 'median_cloud_top_temp',num_did, /float)
  ;tenth_temp_id=ncdf_vardef(cdfid, 'tenth_percentile_cloud_top_temp',num_did, /float)
  ;cold_flag_id=ncdf_vardef(cdfid,'cold_flag',num_did,/short)
  clat_id=ncdf_vardef(cdfid,'clat',num_did,/float)
  clon_id=ncdf_vardef(cdfid,'clon',num_did,/float)
  chlor_a_id=ncdf_vardef(cdfid,'chlor_a',num_did,/float)
  ncdf_control, cdfid, /endef

  ncdf_varput, cdfid, julian_day_id,all_julian_day
  ncdf_varput, cdfid, mean_lat_id,all_mean_lat
  ncdf_varput, cdfid, mean_lon_id,all_mean_lon
  ncdf_varput, cdfid, mean_nd_id,all_mean_nd
  ;ncdf_varput, cdfid, mean_re_id,all_mean_re
  ;ncdf_varput, cdfid, mean_lwp_id, all_mean_lwp
  ;ncdf_varput, cdfid, mean_temp_id,all_mean_temp
  ;ncdf_varput, cdfid, median_temp_id,all_median_temp
  ;ncdf_varput, cdfid, tenth_temp_id,all_tenth_temp
  ;ncdf_varput,cdfid,cold_flag_id,all_cold_flag
  ncdf_varput, cdfid, clat_id,all_clat
  ncdf_varput, cdfid, clon_id,all_clon
  ncdf_varput, cdfid, chlor_a_id,all_chlor_a
  ncdf_close, cdfid
  
  ;  Read previously created output file
endif else begin
  print,'found output file'
  fid=ncdf_open(output_file)

  vid=ncdf_varid(fid,'julian_day') & ncdf_varget,fid,vid,all_julian_day
  vid=ncdf_varid(fid,'mean_latitude') & ncdf_varget,fid,vid,all_mean_lat
  vid=ncdf_varid(fid,'mean_longitude') & ncdf_varget,fid,vid,all_mean_lon
  vid=ncdf_varid(fid,'mean_nd') & ncdf_varget,fid,vid,all_mean_nd
  ;vid=ncdf_varid(fid,'mean_re') & ncdf_varget,fid,vid,all_mean_re
  ;vid=ncdf_varid(fid,'mean_lwp') & ncdf_varget,fid,vid,all_mean_lwp
  ;vid=ncdf_varid(fid,'mean_cloud_top_temp') & ncdf_varget,fid,vid,all_mean_temp
  ;vid=ncdf_varid(fid,'median_cloud_top_temp') & ncdf_varget,fid,vid,all_median_temp
  ;vid=ncdf_varid(fid,'tenth_percentile_cloud_top_temp') & ncdf_varget,fid,vid,all_tenth_temp
  ;vid=ncdf_varid(fid,'cold_flag') & ncdf_varget,fid,vid,all_cold_flag
  vid=ncdf_varid(fid,'clat') & ncdf_varget,fid,vid,all_clat
  vid=ncdf_varid(fid,'clon') & ncdf_varget,fid,vid,all_clon
  vid=ncdf_varid(fid,'chlor_a') & ncdf_varget,fid,vid,all_chlor_a
  ncdf_close,fid

  ;  Get the ocean color file data for the day
  if 1 eq 0 then begin
    cname='A'+string(yy,format='(I4)')+string(doy1,format='(I03)')+'.L3m_DAY_CHL_chlor_a_4km.nc'
    print,cname
    fid=ncdf_open(cdir+cname)
    vid=ncdf_varid(fid,'chlor_a') & ncdf_varget,fid,vid,chlor_a1
    vid=ncdf_varid(fid,'lat') & ncdf_varget,fid,vid,clat1
    vid=ncdf_varid(fid,'lon') & ncdf_varget,fid,vid,clon1
    ncdf_close,fid
  endif
endelse
  
;*****************
;  Colortable
;******************
;  Top is the last color to scale 256 colors, 0-255
top_color=252
;  Colortable  0-252  253=white
;mytable=colortable(39,ncolors=254)
mytable=colortable(43,ncolors=253)
;mytable=colortable(33,ncolors=253)
;254=hot pink               ;gray=255
mytable=[mytable,transpose([255,255,255]),transpose([238,18,137]),transpose([230,230,230])]
mycbtable=mytable[0:top_color,*]

;*************************
;  Set up the positions
;*************************
pxdim=1000 & pydim=1000
xl=0.08 & xr=0.92
yb=0.07 & yt=0.92
sx=0.17
sy=0.08
numplots_x=2
numplots_y=4
position_plots,xl,xr,yb,yt,sx,sy,numplots_x,numplots_y,pos
;  Colorbar position
cbpos=pos
cbpos[*,0]=pos[*,2]+0.02
cbpos[*,2]=cbpos[*,0]+0.009
dx=0.01 & dy=0.01

pnum=0
p0=plot([0,1],[0,1],position=pos[pnum,*],/buffer,dimensions=[pxdim,pydim],axis_style=4,/nodata)
d=p0.convertcoord([pos[pnum,0],pos[pnum,2]],[pos[pnum,1],pos[pnum,3]],/normal,/to_device)
isx=(d[0,1,0]-d[0,0,0])
isy=(d[1,1,0]-d[1,0,0])
;  Date format for plotting
dummy=label_date(date_format=['%M/%D!C%Y'])
fs1=12  ;font_size

;*************************
;*** Map of chlor_a ***
;*************************
long_name = 'Chlorophyll Concentration, OCI Algorithm'
units = 'mg m^-3'
if 1 eq 0 then begin
  ; Process fill value.
  idx=where(chlor_a1 eq -32767, cnt)
  if cnt gt 0 then chlor_a1[idx] = !Values.F_NAN
  ; Generate the plot.
  m = MAP('Geographic', TITLE=file_name, FONT_SIZE=fs1, /current,position=pos[pnum,*])
  ct = COLORTABLE(72, /reverse)
  t1 = TEXT(pos[pnum,0],pos[pnum,3], long_name+' (log_10 scale)')
  c1 = CONTOUR(ALOG10(chlor_a1), clon1, clat1, OVERPLOT=m, $
    /FILL,RGB_TABLE=ct,GRID_UNITS='degrees', POSITION=pos[pnum,*]) 
  mc = MAPCONTINENTS()
  cb = COLORBAR(TARGET=c1, /BORDER, ORIENTATION=1, TEXTPOS=1,  $ ;0=horizontal
    Position=reform(cbpos[pnum,*]), TITLE=units)
endif  
  
;*******************
;  Lat-lon scatter
;*******************  
r=where(all_mean_lat eq -9999,count)
print,'found lat=-9999 ',count
r=where(all_mean_lat ne -9999,count)

pnum=1
p2=plot(all_mean_lat[r],all_clat[r],/current,position=pos[pnum,*],$
  font_size=fs1,/sym_filled,/xstyle,/ystyle,$
  linestyle=6,symbol='o',sym_size=0.3,xtitle='Hist Latitude',ytitle='Chlor Latitude')  

pnum=3
p2=plot(all_mean_lon[r],all_clon[r],/current,position=pos[pnum,*],$
  font_size=fs1,/sym_filled,/xstyle,/ystyle,$
  linestyle=6,symbol='o',sym_size=0.3,xtitle='Hist Longitude',ytitle='Chlor longitude')

;*****************************
;  Chlor histograms
;*****************************    
pnum=0
fill_value=-32767
r=where(all_chlor_a eq fill_value,count_fill)
r=where(all_chlor_a ne fill_value,count_chlor)

r_good=where(all_chlor_a ne fill_value,count_good)
r_hi=where(all_chlor_a ne fill_value and all_mean_nd ge 100,count_hi)
r_lo=where(all_chlor_a ne fill_value and all_mean_nd le 50 and all_mean_nd ne -9999,count_lo)

start_bin=0.0;min(data_var)
end_bin=1.0;max(data_var)
dbin=(end_bin-start_bin)/100.0

data_var=all_chlor_a[r_good]
hist_generic,data_var,start_bin,end_bin,dbin,good_bins,good_data_freq,good_data_counts
data_var=all_chlor_a[r_hi]
hist_generic,data_var,start_bin,end_bin,dbin,hi_bins,hi_data_freq,hi_data_counts
data_var=all_chlor_a[r_lo]
hist_generic,data_var,start_bin,end_bin,dbin,lo_bins,lo_data_freq,lo_data_counts
p4=plot(good_bins,good_data_counts,/current,position=pos[pnum,*],$
  font_size=fs1,/xstyle,$
  /hist,xtitle='mg m^-3',ytitle='counts')
p7=plot(hi_bins,hi_data_counts,/overplot,/hist,color='red')
p7=plot(lo_bins,lo_data_counts,/overplot,/hist,color='blue')  
  
t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'Chlorophyll Concentration',font_size=fs1)
t1=text(pos[pnum,0]+12*dx,pos[pnum,3]-3*dy,'chlor values='+string(count_chlor,format='(I6)'),$
  font_size=fs1)
t1=text(pos[pnum,0]+12*dx,pos[pnum,3]-5*dy,'no match values='+string(count_fill,format='(I6)'),$
  font_size=fs1)
t1=text(pos[pnum,0]+12*dx,pos[pnum,3]-7*dy,'hi Nd chlor values='+string(count_hi,format='(I6)'),$
  font_size=fs1,color='red')  
t1=text(pos[pnum,0]+12*dx,pos[pnum,3]-9*dy,'lo Nd chlor values='+string(count_lo,format='(I6)'),$
  font_size=fs1,color='blue')  

;**********
;  Chlor nd scatter now 2d histograms
;**********
  
pnum=2
if 1 eq 0 then begin
  r=where(all_chlor_a ne fill_value,count_chlor)
  x_data=all_mean_nd[r]
  x_start_bin=0.0
  x_end_bin=300.0
  x_dbin=(x_end_bin-x_start_bin)/150.0
  y_data=all_chlor_a[r]  
  y_start_bin=0.0
  y_end_bin=1.0
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  dmin=0
  dmax=max(data_counts)
  var_image=bytscl(data_counts,min=dmin,max=dmax,top=top_color)
  r=where(data_counts eq 0,c)
  if c gt 0 then var_image[r]=253
  p1=image(var_image,x_bins,y_bins,/current,image_dimensions=[isx,isy],$
    position=pos[pnum,*],rgb_table=mytable,font_size=fs1)
  c1=contour(data_counts,x_bins,y_bins,/nodata,position=pos[pnum,*],$
    ytitle='Chlor concen',xtitle='Hist Nd',axis_style=2,$
    /current,xstyle=1,ystyle=1,xtickdir=1,ytickdir=1,font_size=fs1)
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin,dmax])
endif else begin
  r=where(all_chlor_a ne fill_value and all_mean_nd ne -9999,count_chlor)
  good_chlor=all_chlor_a[r]  
  good_mean_nd=all_mean_nd[r]
  p2=plot(good_mean_nd,good_chlor,/current,position=pos[pnum,*],$
    yrange=[0,1],$
    font_size=fs1,/sym_filled,$
    linestyle=6,symbol='.',xtitle='Hist Nd',ytitle='Chlor concen')
endelse

pnum=4
if 1 eq 0 then begin
  r=where(all_chlor_a ne fill_value and all_mean_nd le 50 and all_mean_nd ne -9999,count_chlor)
  x_data=all_mean_nd[r]
  x_start_bin=0.0
  x_end_bin=50.0
  x_dbin=(x_end_bin-x_start_bin)/150.0
  y_data=all_chlor_a[r]
  y_start_bin=0.0
  y_end_bin=1.0
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  dmin=0
  dmax=max(data_counts)
  var_image=bytscl(data_counts,min=dmin,max=dmax,top=top_color)
  r=where(data_counts eq 0,c)
  if c gt 0 then var_image[r]=253
  p1=image(var_image,x_bins,y_bins,/current,image_dimensions=[isx,isy],$
    position=pos[pnum,*],rgb_table=mytable,font_size=fs1)
  c1=contour(data_counts,x_bins,y_bins,/nodata,position=pos[pnum,*],$
    ytitle='Chlor concen',xtitle='Hist Nd',axis_style=2,$
    /current,xstyle=1,ystyle=1,xtickdir=1,ytickdir=1,font_size=fs1)
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin,dmax])
endif else begin
  r=where(all_chlor_a ne fill_value and all_mean_nd le 50 and all_mean_nd ne -9999,count_chlor)
  good_chlor=all_chlor_a[r]
  good_mean_nd=all_mean_nd[r]
  p2=plot(good_mean_nd,good_chlor,/current,position=pos[pnum,*],$
    yrange=[0,1],$
    font_size=fs1,/sym_filled,$
    linestyle=6,symbol='.',xtitle='Hist Nd',ytitle='Chlor concen')
endelse

pnum=5
if 1 eq 0 then begin
  r=where(all_chlor_a ne fill_value and all_mean_nd ge 100,count_chlor)
  x_data=all_mean_nd[r]
  x_start_bin=100.0
  x_end_bin=300.0
  x_dbin=(x_end_bin-x_start_bin)/150.0
  y_data=all_chlor_a[r]
  y_start_bin=0.0
  y_end_bin=1.0
  y_dbin=(y_end_bin-y_start_bin)/100.0
  histogram_2d,x_data,y_data,x_start_bin,x_end_bin,x_dbin,x_bins,$
    y_start_bin,y_end_bin,y_dbin,y_bins,data_freq,data_counts
  dmin=0
  dmax=max(data_counts)
  var_image=bytscl(data_counts,min=dmin,max=dmax,top=top_color)
  r=where(data_counts eq 0,c)
  if c gt 0 then var_image[r]=253
  p1=image(var_image,x_bins,y_bins,/current,image_dimensions=[isx,isy],$
    position=pos[pnum,*],rgb_table=mytable,font_size=fs1)
  c1=contour(data_counts,x_bins,y_bins,/nodata,position=pos[pnum,*],$
    ytitle='Chlor concen',xtitle='Hist Nd',axis_style=2,$
    /current,xstyle=1,ystyle=1,xtickdir=1,ytickdir=1,font_size=fs1)
  cb=colorbar(rgb_table=mycbtable,/border,title='counts',font_size=fs1,$
    orientation=1,textpos=1,position=cbpos[pnum,*],range=[dmin,dmax])
endif else begin
  r=where(all_chlor_a ne fill_value and all_mean_nd ge 100,count_chlor)
  good_chlor=all_chlor_a[r]
  good_mean_nd=all_mean_nd[r]
  p2=plot(good_mean_nd,good_chlor,/current,position=pos[pnum,*],$
    yrange=[0,1],$
    font_size=fs1,/sym_filled,$
    linestyle=6,symbol='.',xtitle='Hist Nd',ytitle='Chlor concen')
endelse

;********************
;  Nd histograms
;********************

pnum=6
start_bin=0.0;min(data_var)
end_bin=300.0;max(data_var)
dbin=(end_bin-start_bin)/100.0

data_var=all_mean_nd[r_good]
hist_generic,data_var,start_bin,end_bin,dbin,good_bins,good_data_freq,good_data_counts
data_var=all_mean_nd[r_hi]
hist_generic,data_var,start_bin,end_bin,dbin,hi_bins,hi_data_freq,hi_data_counts
data_var=all_mean_nd[r_lo]
hist_generic,data_var,start_bin,end_bin,dbin,lo_bins,lo_data_freq,lo_data_counts

p0=plot(good_bins,good_data_counts,/current,dimensions=[pxdim,pydim],$
  position=pos[pnum,*],/hist,font_size=12,xtitle='Mean Nd',$
  ytitle='Counts',color='black',/xstyle)
p2=plot(lo_bins,lo_data_counts,/overplot,color='blue',/hist)
p2=plot(hi_bins,hi_data_counts,/overplot,color='red',/hist)

;t1=text(pos[pnum,0]+1*dx,pos[pnum,3]-4*dy,'high Nd',color='red',font_size=12)
;t1=text(pos[pnum,0]+1*dx,pos[pnum,3]-6*dy,'low Nd',color='blue',font_size=12)
;t1=text(pos[pnum,0]+1*dx,pos[pnum,3]-8*dy,'all Nd',color='black',font_size=12)

;*************************
;  Chlor_a frequency histograms
;*************************
pnum=7
r_good=where(all_chlor_a ne fill_value,count_good)
r_hi=where(all_chlor_a ne fill_value and all_mean_nd ge 100,count_hi)
r_lo=where(all_chlor_a ne fill_value and all_mean_nd le 50 and all_mean_nd ne -9999,count_lo)

start_bin=0.0;min(data_var)
end_bin=1.0;max(data_var)
dbin=(end_bin-start_bin)/100.0

data_var=all_chlor_a[r_good]
hist_generic,data_var,start_bin,end_bin,dbin,good_bins,good_data_freq,good_data_counts
data_var=all_chlor_a[r_hi]
hist_generic,data_var,start_bin,end_bin,dbin,hi_bins,hi_data_freq,hi_data_counts
data_var=all_chlor_a[r_lo]
hist_generic,data_var,start_bin,end_bin,dbin,lo_bins,lo_data_freq,lo_data_counts
p4=plot(good_bins,good_data_freq,/current,position=pos[pnum,*],$
  font_size=fs1,/xstyle,$
  /hist,xtitle='mg m^-3',ytitle='Frequency',/nodata)
p7=plot(hi_bins,hi_data_freq,/overplot,/hist,color='red')
p7=plot(lo_bins,lo_data_freq,/overplot,/hist,color='blue')

t1=text(pos[pnum,0],pos[pnum,3]+1*dy,'Chlorophyll Concentration',font_size=fs1)
;t1=text(pos[pnum,0]+12*dx,pos[pnum,3]-3*dy,'chlor values='+string(count_chlor,format='(I6)'),$
;  font_size=fs1)
;t1=text(pos[pnum,0]+12*dx,pos[pnum,3]-5*dy,'no match values='+string(count_fill,format='(I6)'),$
;  font_size=fs1)
;t1=text(pos[pnum,0]+12*dx,pos[pnum,3]-7*dy,'hi Nd chlor values='+string(count_hi,format='(I6)'),$
;  font_size=fs1,color='red')
;t1=text(pos[pnum,0]+12*dx,pos[pnum,3]-9*dy,'lo Nd chlor values='+string(count_lo,format='(I6)'),$
;  font_size=fs1,color='blue')




;if eos eq '*' then eos='MOYD'
t1=text(0.1,0.97,'Chlorophyll Match '+cproduct,font_size=12)
t1=text(0.53,0.97,eos,font_size=12)  
t1=text(0.6,0.97,time_range_str,font_size=12)  
;png = cname + '.idl2.png'
png=eos+'.chlor_match.'+cproduct+'.'+time_range_str+'.png'
;c1.save, png, HEIGHT=600, WIDTH=800
;m.save, png, HEIGHT=600, WIDTH=800
p0.save, png, HEIGHT=pydim;, WIDTH=800

stop

end