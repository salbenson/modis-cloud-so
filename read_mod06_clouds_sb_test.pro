;  Directly download granules
;  Dec 2018, Jan and Feb 2019
;https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/61/

;  Search for granules in a date time range
;https://search.earthdata.nasa.gov/search
;  In the search box put MYD06_L2 and select the dataset.
;  Enter time using calendar button.  Enter box using spacial rectangle SW=-76,58  NE=-49,152
;                                                                     **SW=-76,40  NE=-45,152  ;bigger antarctic
;                                                                     ***order mod03 for trajectory analysis
;                                                                     SW=-80,20   NE=-25,179
;                                                                       
;  Click download all
;  direct download
;  click download data
;  view/download data links
;  download links file

;short Cloud_Water_Path(Cell_Along_Swath_1km:mod06, Cell_Across_Swath_1km:mod06) ;
;Cloud_Water_Path:valid_range = 0s, 10000s ;
;Cloud_Water_Path:_FillValue = -9999s ;
;Cloud_Water_Path:long_name = "Column Water Path two-band retrieval using band 7 and either band 1, 2, or 5 (specified in Quality_Assurance_1km)from best points: not failed in any way, not marked for clear sky restoral" ;
;Cloud_Water_Path:units = "g/m^2" ;
;Cloud_Water_Path:scale_factor = 1. ;
;Cloud_Water_Path:add_offset = 0. ;
;Cloud_Water_Path:Parameter_Type = "Output" ;
;Cloud_Water_Path:Cell_Along_Swath_Sampling = 1, 2040, 1 ;
;Cloud_Water_Path:Cell_Across_Swath_Sampling = 1, 1354, 1 ;
;Cloud_Water_Path:Geolocation_Pointer = "External MODIS geolocation product" ;

pro read_mod06_clouds_sb_test

;  imac or chpc
;path_prefix='/Volumes/'
path_prefix='/uufs/chpc.utah.edu/common/home/'

;  Choose Aqua or Terra
;eos='MYD'
eos='MOD'

;  Time range to analyze
;  SEASON Nov 2018-Feb 2019
;julian_day_1d=timegen(start=julday(11,1,2018,0,0,0),final=julday(11,30,2018,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(12,30,2018,0,0,0),final=julday(12,30,2018,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(1,1,2019,0,0,0),final=julday(1,31,2019,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(2,1,2019,0,0,0),final=julday(2,28,2019,23,59,59),units='days',step_size=1)

;  SEASON Nov 2017-Feb 2018
;julian_day_1d=timegen(start=julday(11,1,2017,0,0,0),final=julday(11,30,2017,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(12,1,2017,0,0,0),final=julday(12,31,2017,23,59,59),units='days',step_size=1)
julian_day_1d=timegen(start=julday(1,1,2018,0,0,0),final=julday(1,31,2018,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(2,1,2018,0,0,0),final=julday(2,28,2018,23,59,59),units='days',step_size=1)

;  SEASON Nov 2016-Feb 2017
;julian_day_1d=timegen(start=julday(11,1,2016,0,0,0),final=julday(11,30,2016,23,59,59),units='days',step_size=1)
;julian_day_1d=timegen(start=julday(12,1,2016,0,0,0),final=julday(12,1,2016,23,59,59),units='days',step_size=1)
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

;  Choose year
;syear='2014'
syear=string(yy[0],format='(I4)')
;smonth='11'
smonth=string(mm[0],format='(I02)')

;  To run transition cases, uncomment the date you want to run
;sdate='20171112'
;sdate='20171124'
;sdate='20171231'
;sdate='20180105'
;sdate='20180106'
;sdate='20180129'
;sdate='20180131'
;sdate='20180201'
;sdate='20180202'
;sdate='20180218'
;sdate='20180219'

;  Output path
if sdate eq !NULL then begin
  output_path=path_prefix+'mace-group4/modis/hysplit/modis_histograms_sm/'  ;&&&&  
endif else begin
  output_path=path_prefix+'mace-group4/modis/transitions/'+sdate+'/'
endelse

;  Modis hdf file directory
if sdate eq !NULL then begin
  fdir06=path_prefix+'mace-group6/modis/'+eos+'06_L2/'+syear+smonth+'/'  ;&&&&
  fdir03=path_prefix+'mace-group6/modis/'+eos+'03/'+syear+smonth+'/'     ;&&&&
endif else begin
  fdir06=output_path+'hdf_files/'
  fdir03=output_path+'hdf_files/'
endelse

;  Loop through a range of days
for n=0,numtimes-1 do begin
;for n=329,329 do begin  ;old loop through doy number
;for n=1,1 do begin
  
  print,'doy',doy[n],'*************'
  nstr=string(doy[n],format='(I03)')
  if sdate eq !NULL then begin
    ;mod06_files=file_search(fdir06+'MYD06_L2.A2017*hdf',count=num_mod06)    
    mod06_files=file_search(fdir06+eos+'06_L2.A'+syear+nstr+'*hdf',count=num_mod06)
    ;mod06_files=file_search(fdir06+eos+'06_L2.A'+syear+nstr+'.2355.'+'*hdf',count=num_mod06)
    ;mod06_files=file_search(fdir06+'MYD06_L2.A2017354*hdf',count=num_mod06)
    ;mod06_files=file_search(fdir06+'MYD06_L2.A2017001.0630.061.2018029080414.hdf',count=num_mod06)
  endif else begin
    ;  need the doy of sdate=nstr
    mod06_files=file_search(fdir06+eos+'06_L2.A'+strmid(sdate,0,4)+nstr+'*1115*hdf',count=num_mod06)
  endelse
  print,'num_mod06',num_mod06
  
  ;  Loop through the modis granules for that day of the year
  for f=0,num_mod06-1 do begin
    
    mod06_file=mod06_files[f]
    print, mod06_file

    ;  Pulls out the string  'MYD06_L2.A2018006.0625.061.2018006194906'
    output_file_string=strmid(file_basename(mod06_file),0,40)

    ;  Pick up the matching myd03 file
    parts=strsplit(file_basename(mod06_file),'.',/extract)
    syear=strmid(parts[1],1,4)
    sdoy=strmid(parts[1],5,3)
    mod03_file=file_search(fdir03+eos+'03.'+parts[1]+'.'+parts[2]+'.'+parts[3]+'*hdf',count=numfiles)
    mod03_file=mod03_file[0]    
    
    ;  If found a matching myd03 file    
    if numfiles eq 1 then begin
      print,'found ',mod03_file  

      ;  Read myd03 and myd06 hdf files
      ;MOD03.A2018051.0010.061.2018051070825.hdf has -999.000 values in lat_1km,lon_1km
      file_id=hdf_sd_start(mod03_file)
      x_id=hdf_sd_nametoindex(file_id,'Latitude')
      x_id2=hdf_sd_select(file_id,x_id)
      hdf_sd_getdata,x_id2,lat_1km
      hdf_sd_endaccess,x_id2

      x_id=hdf_sd_nametoindex(file_id,'Longitude')
      x_id2=hdf_sd_select(file_id,x_id)
      hdf_sd_getdata,x_id2,lon_1km
      hdf_sd_endaccess,x_id2
      hdf_sd_end,file_id

      file_id=hdf_sd_start(mod06_file)
      x_id=hdf_sd_nametoindex(file_id,'Latitude')
      x_id2=hdf_sd_select(file_id,x_id)
      hdf_sd_getdata,x_id2,lat_5km
      hdf_sd_endaccess,x_id2

      x_id=hdf_sd_nametoindex(file_id,'Longitude')
      x_id2=hdf_sd_select(file_id,x_id)
      hdf_sd_getdata,x_id2,lon_5km
      hdf_sd_endaccess,x_id2

      x_id=hdf_sd_nametoindex(file_id,'Scan_Start_Time')
      x_id2=hdf_sd_select(file_id,x_id)
      hdf_sd_getdata,x_id2,scan_start_time
      hdf_sd_endaccess,x_id2

      day=julday(1,1,1993,0,0,0) & julian_day=day+(scan_start_time/86400.d)
      caldat, julian_day[0,0], smm, sdd, syy, shh, smi, sss
      print,'first scan time',syy,smm,sdd,shh,smi,sss

      ;Null value of -327.670 
      x_id=hdf_sd_nametoindex(file_id,'Solar_Zenith')
      x_id2=hdf_sd_select(file_id,x_id)
      hdf_sd_getdata,x_id2,solar_zenith_angle
      hdf_sd_endaccess,x_id2
      solar_zenith_angle=float(solar_zenith_angle)*0.01
      
      ;Null value of -327.670 
      x_id=hdf_sd_nametoindex(file_id,'Sensor_Zenith')
      x_id2=hdf_sd_select(file_id,x_id)
      hdf_sd_getdata,x_id2,sensor_zenith_angle
      hdf_sd_endaccess,x_id2
      sensor_zenith_angle=float(sensor_zenith_angle)*0.01
      
      ;  scale_factor=0.01  offset=0  fillvalue=-9999 1KM
      x_id=hdf_sd_nametoindex(file_id,'Cloud_Effective_Radius')
      x_id2=hdf_sd_select(file_id,x_id)
      hdf_sd_getdata,x_id2,cloud_effective_radius2
      hdf_sd_endaccess,x_id2
      scaleid=hdf_sd_attrfind(x_id2,'scale_factor')
      hdf_sd_attrinfo,x_id2,scaleid,data=scale_factor  ;0.01
      scale_factor=scale_factor[0]
      cloud_effective_radius=float(cloud_effective_radius2)*scale_factor
      r=where(cloud_effective_radius2 eq -9999,c)
      if c gt 0 then cloud_effective_radius[r]=-9999

      ;  scale_factor=0.01  offset=-15000  fillvalue=-32768 5KM
      x_id=hdf_sd_nametoindex(file_id,'Cloud_Top_Temperature')
      x_id2=hdf_sd_select(file_id,x_id)
      hdf_sd_getdata,x_id2,cloud_top_temperature2
      hdf_sd_endaccess,x_id2
      scaleid=hdf_sd_attrfind(x_id2,'scale_factor')
      hdf_sd_attrinfo,x_id2,scaleid,data=scale_factor  ;0.01
      scale_factor=scale_factor[0]
      offsetid=hdf_sd_attrfind(x_id2,'add_offset')
      hdf_sd_attrinfo,x_id2,offsetid,data=offset  ;-15000.000 
      offset=offset[0]
      cloud_top_temperature=((float(cloud_top_temperature2)-offset)*scale_factor)  ;modis
      ;cloud_top_temperature=((float(cloud_top_temperature2))*scale_factor)+150.0  ;jay
      cloud_top_temperature=cloud_top_temperature-273.  ;convert to celcius
      r=where(cloud_top_temperature2 eq -32768,c)
      if c gt 0 then cloud_top_temperature[r]=-9999

      ;  scale_factor=0.01  offset=-15000  fillvalue=-32768
      x_id=hdf_sd_nametoindex(file_id,'Surface_Temperature')
      x_id2=hdf_sd_select(file_id,x_id)
      hdf_sd_getdata,x_id2,surface_temperature2
      hdf_sd_endaccess,x_id2
      scaleid=hdf_sd_attrfind(x_id2,'scale_factor')
      hdf_sd_attrinfo,x_id2,scaleid,data=scale_factor  ;0.01
      scale_factor=scale_factor[0]
      offsetid=hdf_sd_attrfind(x_id2,'add_offset')
      hdf_sd_attrinfo,x_id2,offsetid,data=offset  ;-15000.000
      offset=offset[0]
      surface_temperature=((float(surface_temperature2)-offset)*scale_factor)  ;modis
      ;surface_temperature=((float(surface_temperature2))*scale_factor)+150.  ;jay
      surface_temperature=surface_temperature-273.  ;convert to celcius

      ;  scale_factor=0.1  offset=0  fillvalue=-32768 5KM
      x_id=hdf_sd_nametoindex(file_id,'Cloud_Top_Pressure')
      x_id2=hdf_sd_select(file_id,x_id)
      hdf_sd_getdata,x_id2,cloud_top_pressure2
      hdf_sd_endaccess,x_id2
      scaleid=hdf_sd_attrfind(x_id2,'scale_factor')
      hdf_sd_attrinfo,x_id2,scaleid,data=scale_factor  ;0.1
      scale_factor=scale_factor[0]
      cloud_top_pressure=(float(cloud_top_pressure2)*scale_factor)*100. ; pa
      r=where(cloud_top_pressure2 eq -32768,c)
      if c gt 0 then cloud_top_pressure[r]=-9999

      ;  scale_factor=0.01  offset=0  fillvalue=-9999 1KM
      x_id=hdf_sd_nametoindex(file_id,'Cloud_Optical_Thickness')
      x_id2=hdf_sd_select(file_id,x_id)
      hdf_sd_getdata,x_id2,cloud_optical_thickness2
      hdf_sd_endaccess,x_id2
      scaleid=hdf_sd_attrfind(x_id2,'scale_factor')
      hdf_sd_attrinfo,x_id2,scaleid,data=scale_factor  ;0.01
      scale_factor=scale_factor[0]
      cloud_optical_thickness=float(cloud_optical_thickness2)*scale_factor
      r=where(cloud_optical_thickness2 eq -9999,c)
      if c gt 0 then cloud_optical_thickness[r]=-9999

      ;  scale_factor=1.0  offset=0  fillvalue=0  1KM
      x_id=hdf_sd_nametoindex(file_id,'Cloud_Phase_Optical_Properties')
      x_id2=hdf_sd_select(file_id,x_id)
      hdf_sd_getdata,x_id2,cloud_phase
      hdf_sd_endaccess,x_id2

      ;  scale_factor=1  offset=0  fillvalue=-9999
      x_id=hdf_sd_nametoindex(file_id,'Cloud_Water_Path')
      x_id2=hdf_sd_select(file_id,x_id)
      hdf_sd_getdata,x_id2, cloud_water_path
      hdf_sd_endaccess,x_id2
      cloud_water_path=float(cloud_water_path)
      
      ;  Close file
      hdf_sd_end,file_id
      print,'finished reading data'

      ;  The size of the data arrays
      s=size(lat_1km,/dimensions)
      numx_1km=s[0]  ;lat_1km[*,0]
      numy_1km=s[1]  ;lat_1km[0,*]
      s=size(lat_5km,/dimensions)
      numx_5km=s[0]  ;lon_low_res[*,0]
      numy_5km=s[1]  ;lat_low_res[0,*]

      ;  Calculate colongitude
      colon_1km=lon_1km
      result=where(colon_1km lt 0 and colon_1km ne -999.00,count)
      if count gt 0 then colon_1km[result]=360.0+colon_1km[result]
      colon_5km=lon_5km
      result=where(colon_5km lt 0,count)
      if count gt 0 then colon_5km[result]=360.0+colon_5km[result]

      ;****************************************************
      ;  Create an Nd array on the MODIS grid
      ;****************************************************

      print,'start nd array'

      ;  Increase resolution from 5km to 1km
      cloud_top_temp_1km=congrid(cloud_top_temperature,numx_1km,numy_1km)
      sensor_zenith_angle_1km=congrid(sensor_zenith_angle,numx_1km,numy_1km)
      solar_zenith_angle_1km=congrid(solar_zenith_angle,numx_1km,numy_1km)

      ;p0=contour(cloud_top_temp_1km,lon_1km,lat_1km,irregular=0)
      ;p1=contour(cloud_top_temperature,lon_5km,lat_5km,color='red',irregular=0)

      ;  These arrrays are calculated
      nd_array=make_array(/float,numx_1km,numy_1km,value=-9999)
      cw=make_array(/float,numx_1km,numy_1km,value=-9999)

      r=where(cloud_effective_radius gt 0 and cloud_water_path gt 0 and cloud_phase eq 2,count)
      if count gt 0 then begin
        cw[r]=0.6+((0.9/20.)*((cloud_top_temp_1km[r])+20.)) ; parameterized fro Wood's figure 1
        r2=where(cw lt 0.4 and cw ne -9999,c2)
        if c2 gt 0 then begin
          cw[r2]=0.4
        endif
        nd_array[r]=((sqrt(5.)/(2.*!pi*0.8))*sqrt((0.8*cw[r]*(1.e-6)*$
          cloud_optical_thickness[r])/(2.*1000.*(((1.e-6)*(cloud_effective_radius[r]))^5))))*1.e-6 ; Grosvenor et al 2018, eq 11 converted to 1/cm3
      endif
      print,'end nd array'

;****************************************************************
;  Carve out the microphysics for histogram writing in lat-lon regions.  
;  Only liquid phase cloud data with LWP < 250 g/m2 is used.  
;  Require view zenith to be less than 30 and solar zenith to be less than 60 
;****************************************************************

      ;  Check to see if there is any data that meets our conditions
      ;  0<re< 50, phase=2(liq), sensor zenith < 30 and solar zenith < 60
      good_data=make_array(/float,numx_1km,numy_1km,value=0)
      result=where(cloud_effective_radius gt 0. and cloud_effective_radius lt 50. and $
        cloud_water_path lt 300.0 and $  ;added this  
        cloud_phase eq 2 and $
        sensor_zenith_angle_1km lt 30. and sensor_zenith_angle_1km ne -327.670 and $
        solar_zenith_angle_1km lt 60. and solar_zenith_angle_1km ne -327.670,count_good)
      print,count_good,'count_good'

      ;  Continue on if there is good data
      ;if 1 eq 1 then begin ;  runs all files
      if count_good gt 0 then begin  ;only runs files with retrievals  ;&&&&

        ;  histogram Output directory
        if sdate eq !NULL then begin
          out_dir=output_path+syear+'/'+sdoy+'/' ;&&&&
        endif else begin
          out_dir=output_path+'/histograms/'
        endelse
        file_mkdir,out_dir
        
        good_data[result]=1  ;good data
        r=where(sensor_zenith_angle_1km gt 30.0,c)
        if c gt 0 then good_data[r]=2  ;mark these excluded values
        r=where(solar_zenith_angle_1km gt 60.0,c)
        if c gt 0 then good_data[r]=3        
        r=where(solar_zenith_angle_1km gt 60.0 and sensor_zenith_angle_1km gt 30,c)
        if c gt 0 then good_data[r]=4
        
        ;  Bins for histograms
        max_lwp=300. & dlwp=20.
        lwp_bins=0. & while max(lwp_bins) lt max_lwp do lwp_bins=[lwp_bins, max(lwp_bins)+dlwp] & histo_lwp=fltarr(n_elements(lwp_bins))
        max_re=30. & dre=2.
        re_bins=0. & while max(re_bins) lt max_re do re_bins=[re_bins, max(re_bins)+dre] & histo_re=fltarr(n_elements(re_bins))
        max_tau=50. & dtau=2.5
        tau_bins=0. & while max(tau_bins) lt max_tau do tau_bins=[tau_bins, max(tau_bins)+dtau] & histo_tau=fltarr(n_elements(tau_bins))
        ; I think these are the excluded values.  should be 0 to 30 20220225
        max_solar_zenith=90. & dsolar_zenith=2.5
        solar_zenith_bins=30. & while max(solar_zenith_bins) lt max_solar_zenith do solar_zenith_bins=[solar_zenith_bins, max(solar_zenith_bins)+dsolar_zenith] & histo_szen=fltarr(n_elements(solar_zenith_bins))
        max_view_zenith=45. & dview_zenith=2.5
        view_zenith_bins=0. & while max(view_zenith_bins) lt max_view_zenith do view_zenith_bins=[view_zenith_bins, max(view_zenith_bins)+dview_zenith] & histo_vzen=fltarr(n_elements(view_zenith_bins))
        max_nd=300. & dnd=10
        nd_bins=0. & while max(nd_bins) lt max_nd do nd_bins=[nd_bins, max(nd_bins)+dnd] & histo_nd=fltarr(n_elements(nd_bins))
        max_temp=20.0 & dtemp=2.0
        temp_bins=-65.0 & while max(temp_bins) lt max_temp do temp_bins=[temp_bins, max(temp_bins)+dtemp] & histo_temp=fltarr(n_elements(temp_bins))
        max_phase=5 & dphase=1.0
        phase_bins=0 & while max(phase_bins) lt max_phase do phase_bins=[phase_bins, max(phase_bins)+dphase] & histo_phase=fltarr(n_elements(phase_bins))

        ;  Half bin width of the lat and lon grid
        ;dlon=2.5 & dlat=1.5  ;large grid
        dlon=1.0 & dlat=0.5  ;small grid
        
        ;  spaced lon array - turned this to a constant grid
        colon_vector_histo=(findgen((360.0/2*dlon))*2*dlon)+dlon

        ;  spaced lat array - turned this to a constant grid
        lat_vector_histo=((findgen(180.0/(2*dlat))*2*dlat)+dlat)-90.0        
        
        ;  Find the bounds of the granule and only loop through those 
        r=where(lat_1km ne -999.000,c)
        min_lat=floor(min(lat_1km[r])) & max_lat=ceil(max(lat_1km[r]))
        r=where(colon_1km ne -999.000,c)
        min_colon=floor(min(colon_1km[r])) & max_colon=ceil(max(colon_1km[r]))
        rlat=where(lat_vector_histo ge min_lat and lat_vector_histo le max_lat,clat)
        rcolon=where(colon_vector_histo ge min_colon and colon_vector_histo le max_colon,ccolon)
        ;print, min_lat,max_lat,min_colon,max_colon,'bounds of granule'
        ;  Step across the lat and lon 
        for jj=0,ccolon-1 do begin
          for kk=0,clat-1 do begin
            ship_latitude=lat_vector_histo[rlat[kk]]
            ship_longitude=colon_vector_histo[rcolon[jj]]
            ;print,ship_latitude,ship_longitude,'lat,lon,center box *********'
        
            ;  -999.000 lats and lons won't be captured here
            ;  Find the number of points in the 1KM box
            r1km=where(abs(lat_1km-ship_latitude) lt dlat and abs(colon_1km-ship_longitude) lt dlon and $
              good_data eq 1,count_1km) 
            ;  Find the number of points in the 5KM box
            r5km=where(abs(lat_5km-ship_latitude) lt dlat and abs(colon_5km-ship_longitude) lt dlon,count_5km)

            ;  Calculate these mean values and do a temperature check
            if count_5km gt 0 then begin
              solar_zenith_int=solar_zenith_angle[r5km]
              sensor_zenith_int=sensor_zenith_angle[r5km]
              top_temp_int=cloud_top_temperature[r5km]
              rcold=where(top_temp_int lt -20 and top_temp_int ne -9999,ccold)
              if float(ccold)/float(count_5km) gt 0.10 then cold_flag=1 else cold_flag=0
            endif else begin
              solar_zenith_int=1e6
              sensor_zenith_int=1e6
              cold_flag=1e6
            endelse
           
            ;  Now see if there are enough points in the grid box  
            if count_5km gt 100 and count_1km gt 10 and $
              cold_flag eq 0 and $
              mean(sensor_zenith_int) lt 30. and mean(solar_zenith_int) lt 60. then begin

              ;  Subset the high res modis  
              ;print,ship_latitude,ship_longitude,count_1km,'  1KM lat and colon in box must be greater than 10'
              lat_vector_int=lat_1km[r1km]
              colon_vector_int=colon_1km[r1km]
              re_vector_int=cloud_effective_radius[r1km]
              lwp_vector_int=cloud_water_path[r1km]
              tau_vector_int=cloud_optical_thickness[r1km]
              nd_vector_int=nd_array[r1km]
              cloud_phase_int=cloud_phase[r1km]
              ;print,max(colon_vector_int),' ',min(colon_vector_int),' using these lons 1km'
              ;print,max(lat_vector_int),' ',min(lat_vector_int),' using these lats 1km'
      
              ;  subset the low res modis  
              solar_zenith_int=solar_zenith_angle[r5km]
              sensor_zenith_int=sensor_zenith_angle[r5km]
              colon_low_int=colon_5km[r5km]
              lat_low_int=lat_5km[r5km]
              julian_day_int=julian_day[r5km]
              cloud_top_temp_int=cloud_top_temperature[r5km]
              ;print,max(colon_low_int),' ',min(colon_low_int),' using these lons 5km'
              ;print,max(lat_low_int),' ',min(lat_low_int),' using these lats 5km'
              ;print,mean(sensor_zenith_int),mean(solar_zenith_int),' sensorZ<30,solarZ<60 5km'     

              ;  Reinitialize the histograms to zero
              histo_lwp=make_array(/float,n_elements(lwp_bins),value=0)
              histo_tau=make_array(/float,n_elements(tau_bins),value=0)
              histo_re=make_array(/float,n_elements(re_bins),value=0)
              histo_nd=make_array(/float,n_elements(nd_bins),value=0)
              histo_szen=make_array(/float,n_elements(solar_zenith_bins),value=0)
              histo_vzen=make_array(/float,n_elements(view_zenith_bins),value=0)
              histo_temp=make_array(/float,n_elements(temp_bins),value=0)
              histo_phase=make_array(/float,n_elements(phase_bins),value=0)  
                          
              ;  Create the histograms
              for j=0,n_elements(lwp_bins)-1 do begin
                d=where(abs(lwp_vector_int-lwp_bins[j]) le dlwp/2., count)
                histo_lwp[j]=count
              endfor
              for j=0,n_elements(tau_bins)-1 do begin
                d=where(abs(tau_vector_int-tau_bins[j]) le dtau/2., count)
                histo_tau[j]=count
              endfor
              for j=0,n_elements(re_bins)-1 do begin
                d=where(abs(re_vector_int-re_bins[j]) le dre/2., count)
                histo_re[j]=count
              endfor
              for j=0,n_elements(nd_bins)-1 do begin
                d=where(abs(nd_vector_int-nd_bins[j]) le dnd/2., count)
                histo_nd[j]=count
              endfor
              for j=0,n_elements(solar_zenith_bins)-1 do begin
                d=where(abs(solar_zenith_int-solar_zenith_bins[j]) le dsolar_zenith/2., count)
                histo_szen[j]=count
              endfor
              for j=0,n_elements(view_zenith_bins)-1 do begin
                d=where(abs(sensor_zenith_int-view_zenith_bins[j]) le dview_zenith/2., count)
                histo_vzen[j]=count
              endfor
              for j=0,n_elements(temp_bins)-1 do begin
                d=where(abs(cloud_top_temp_int-temp_bins[j]) le dtemp/2., count)
                histo_temp[j]=count
              endfor
              for j=0,n_elements(phase_bins)-1 do begin
                d=where(abs(cloud_phase_int-phase_bins[j]) le dphase/2., count)
                histo_phase[j]=count
              endfor

              distfreq = Histogram(cloud_phase_int, MIN=Min(cloud_phase_int))

              ; Find the maximum of the frequency distribution.
              maxfreq = Max(distfreq)

              ; Find the mode.
              cloud_phase_mode = Where(distfreq EQ maxfreq, count) + Min(cloud_phase_int)

              ; Find the 10th percentile value
              r=where(cloud_top_temp_int ne -9999)
              cloud_top_temp_int=cloud_top_temp_int[r]
              sidx=sort(cloud_top_temp_int)
              sort_cloud_top_temp_int=cloud_top_temp_int[sidx]
              p10=round(.10*n_elements(sort_cloud_top_temp_int))
              tenth_per_temp=sort_cloud_top_temp_int[p10]
              ;print,p10,n_elements(sort_cloud_top_temp_int),sort_cloud_top_temp_int[p10]
              
              r=where(cloud_top_temp_int ne -9999,c)
              if c gt 0 then begin
                temp_median=median(cloud_top_temp_int[where(cloud_top_temp_int ne -9999.0000)])              
              endif else begin
                temp_median=-9999
              endelse
              
              
              ; write out the histogram file
              lat_string=strtrim(string(fix(mean(lat_vector_int)), format='(i3)'),2)
              ;lon_string=strtrim(string(fix(mean(lon_vector_int)), format='(i4)'),2)
              lon_string=strtrim(string(fix(mean(colon_vector_int)), format='(i4)'),2)
              histogram_file_string=output_file_string+'_lat_'+lat_string+'_lon_'+lon_string+'_histo.cdf
              print,histogram_file_string
              
              ;  Test to see if the file exists
              file_exists=file_test(out_dir+histogram_file_string)
              
              ;  If the file exists, append or rewrite variables
              if file_exists eq 1 then begin
                print,'file exists, append'
                cdfid=ncdf_open(out_dir+histogram_file_string, /write)
                julian_day_id=ncdf_varid(cdfid,'julian_day')
                center_lat_id=ncdf_varid(cdfid,'center_latitude')
                center_lon_id=ncdf_varid(cdfid,'center_longitude')
                count_5km_id=ncdf_varid(cdfid,'count_5km')
                count_1km_id=ncdf_varid(cdfid,'count_1km')
                mean_lat_id=ncdf_varid(cdfid,'mean_latitude')
                mean_lon_id=ncdf_varid(cdfid,'mean_longitude')
                mean_nd_id=ncdf_varid(cdfid,'mean_nd')
                mean_re_id=ncdf_varid(cdfid,'mean_re')
                mean_lwp_id=ncdf_varid(cdfid,'mean_lwp')
                mean_tau_id=ncdf_varid(cdfid,'mean_tau')
                mean_szen_id=ncdf_varid(cdfid,'mean_solar_zenith')
                mean_vzen_id=ncdf_varid(cdfid,'mean_view_zenith')
                mean_temp_id=ncdf_varid(cdfid,'mean_cloud_top_temp')
                median_temp_id=ncdf_varid(cdfid,'median_cloud_top_temp')
                tenth_temp_id=ncdf_varid(cdfid,'tenth_percentile_cloud_top_temp')
                mode_phase_id=ncdf_varid(cdfid,'mode_cloud_phase')
                nd_bins_id=ncdf_varid(cdfid,'nd_bins')
                nd_histo_id=ncdf_varid(cdfid,'nd_histo')
                re_bins_id=ncdf_varid(cdfid,'re_bins')
                re_histo_id=ncdf_varid(cdfid,'re_histo')
                lwp_bins_id=ncdf_varid(cdfid,'lwp_bins')
                lwp_histo_id=ncdf_varid(cdfid,'lwp_histo')
                tau_bins_id=ncdf_varid(cdfid,'tau_bins')
                tau_histo_id=ncdf_varid(cdfid,'tau_histo')
                szen_bins_id=ncdf_varid(cdfid,'solar_zenith_bins')
                szen_histo_id=ncdf_varid(cdfid,'solar_zenith_histo')
                vzen_bins_id=ncdf_varid(cdfid,'view_zenith_bins')
                vzen_histo_id=ncdf_varid(cdfid,'view_zenith_histo')
                temp_bins_id=ncdf_varid(cdfid,'cloud_top_temp_bins')
                temp_histo_id=ncdf_varid(cdfid,'cloud_top_temp_histo')
                phase_bins_id=ncdf_varid(cdfid,'cloud_phase_bins')
                phase_histo_id=ncdf_varid(cdfid,'cloud_phase_histo')
                cold_flag_id=ncdf_varid(cdfid,'cold_flag')
                if mean_szen_id eq -1 then begin
                  print,'defining sza,vza'
                  ncdf_control,cdfid,/redef
                  mean_szen_id=ncdf_vardef(cdfid, 'mean_solar_zenith', /float)
                  mean_vzen_id=ncdf_vardef(cdfid, 'mean_view_zenith', /float)
                  ncdf_control,cdfid,/endef
                endif
                if mean_tau_id eq -1 then begin
                  print,'defining tau variables'
                  ncdf_control,cdfid,/redef
                  tau_bins_did=ncdf_dimdef(cdfid,'tau',n_elements(tau_bins))
                  mean_tau_id=ncdf_vardef(cdfid, 'mean_tau', /float)
                  tau_bins_id=ncdf_vardef(cdfid, 'tau_bins', [tau_bins_did], /float)
                  tau_histo_id=ncdf_vardef(cdfid, 'tau_histo', [tau_bins_did], /float)
                  ncdf_control, cdfid, /endef
                endif
              endif else begin  
                print,'file does not exists, create'
                cdfid=ncdf_create(out_dir+histogram_file_string, /clobber)
                nd_bins_did=ncdf_dimdef(cdfid,'nd',n_elements(nd_bins))
                re_bins_did=ncdf_dimdef(cdfid,'re',n_elements(re_bins))
                lwp_bins_did=ncdf_dimdef(cdfid,'lwp',n_elements(lwp_bins))
                tau_bins_did=ncdf_dimdef(cdfid,'tau',n_elements(tau_bins))
                szen_bins_did=ncdf_dimdef(cdfid,'solar_zenith',n_elements(solar_zenith_bins))
                vzen_bins_did=ncdf_dimdef(cdfid,'view_zenith',n_elements(view_zenith_bins))
                temp_bins_did=ncdf_dimdef(cdfid,'cloud_top_temp',n_elements(temp_bins))
                phase_bins_did=ncdf_dimdef(cdfid,'cloud_phase',n_elements(phase_bins))  
                julian_day_id=ncdf_vardef(cdfid,'julian_day',/double)
                center_lat_id=ncdf_vardef(cdfid,'center_latitude',/float)
                center_lon_id=ncdf_vardef(cdfid,'center_longitude',/float)
                count_5km_id=ncdf_vardef(cdfid,'count_5km',/float)
                count_1km_id=ncdf_vardef(cdfid,'count_1km',/float)
                mean_lat_id=ncdf_vardef(cdfid,'mean_latitude',/float)
                mean_lon_id=ncdf_vardef(cdfid,'mean_longitude',/float)
                mean_nd_id=ncdf_vardef(cdfid,'mean_nd',/float)
                mean_re_id=ncdf_vardef(cdfid,'mean_re',/float)
                mean_lwp_id=ncdf_vardef(cdfid,'mean_lwp',/float)
                mean_tau_id=ncdf_vardef(cdfid,'mean_tau',/float)
                mean_szen_id=ncdf_vardef(cdfid,'mean_solar_zenith',/float)
                mean_vzen_id=ncdf_vardef(cdfid,'mean_view_zenith',/float)
                mean_temp_id=ncdf_vardef(cdfid,'mean_cloud_top_temp',/float)
                median_temp_id=ncdf_vardef(cdfid,'median_cloud_top_temp',/float)              
                tenth_temp_id=ncdf_vardef(cdfid,'tenth_percentile_cloud_top_temp',/float)  
                mode_phase_id=ncdf_vardef(cdfid,'mode_cloud_phase',/short)
                nd_bins_id=ncdf_vardef(cdfid,'nd_bins',[nd_bins_did],/float)
                nd_histo_id=ncdf_vardef(cdfid,'nd_histo',[nd_bins_did],/float)
                re_bins_id=ncdf_vardef(cdfid,'re_bins',[re_bins_did],/float)
                re_histo_id=ncdf_vardef(cdfid,'re_histo',[re_bins_did],/float)
                lwp_bins_id=ncdf_vardef(cdfid,'lwp_bins',[lwp_bins_did],/float)
                lwp_histo_id=ncdf_vardef(cdfid,'lwp_histo',[lwp_bins_did],/float)
                tau_bins_id=ncdf_vardef(cdfid,'tau_bins',[tau_bins_did],/float)
                tau_histo_id=ncdf_vardef(cdfid,'tau_histo',[tau_bins_did],/float)                
                szen_bins_id=ncdf_vardef(cdfid,'solar_zenith_bins',[szen_bins_did],/float)
                szen_histo_id=ncdf_vardef(cdfid,'solar_zenith_histo',[szen_bins_did],/float)
                vzen_bins_id=ncdf_vardef(cdfid,'view_zenith_bins',[vzen_bins_did],/float)
                vzen_histo_id=ncdf_vardef(cdfid,'view_zenith_histo',[vzen_bins_did],/float)
                temp_bins_id=ncdf_vardef(cdfid,'cloud_top_temp_bins',[temp_bins_did],/float)
                temp_histo_id=ncdf_vardef(cdfid,'cloud_top_temp_histo',[temp_bins_did],/float)
                phase_bins_id=ncdf_vardef(cdfid,'cloud_phase_bins',[phase_bins_did],/short)
                phase_histo_id=ncdf_vardef(cdfid,'cloud_phase_histo',[phase_bins_did],/short)
                cold_flag_id=ncdf_vardef(cdfid,'cold_flag',/short)
                ncdf_control, cdfid, /endef
              endelse
              
              ncdf_varput, cdfid, julian_day_id, mean(julian_day_int)
              ncdf_varput, cdfid, mean_lat_id, mean(lat_vector_int)
              ncdf_varput, cdfid, mean_lon_id, mean(colon_vector_int)
              ncdf_varput, cdfid, mean_nd_id, mean(nd_vector_int[where(nd_vector_int gt 0. and nd_vector_int lt 300. and re_vector_int gt 0. and lwp_vector_int lt 250.)])
              ncdf_varput, cdfid, mean_re_id, mean(re_vector_int[where(nd_vector_int gt 0. and nd_vector_int lt 300. and re_vector_int gt 0. and lwp_vector_int lt 250.)])
              ncdf_varput, cdfid, mean_lwp_id, mean(lwp_vector_int[where(nd_vector_int gt 0. and nd_vector_int lt 300. and re_vector_int gt 0. and lwp_vector_int lt 250.)])
              ncdf_varput, cdfid, mean_tau_id, mean(tau_vector_int[where(nd_vector_int gt 0. and nd_vector_int lt 300. and re_vector_int gt 0. and lwp_vector_int lt 250.)])
              ncdf_varput, cdfid, mean_szen_id,mean(solar_zenith_int[where(nd_vector_int gt 0. and nd_vector_int lt 300. and re_vector_int gt 0. and lwp_vector_int lt 250.)])
              ncdf_varput, cdfid, mean_vzen_id,mean(sensor_zenith_int[where(nd_vector_int gt 0. and nd_vector_int lt 300. and re_vector_int gt 0. and lwp_vector_int lt 250.)])
              ncdf_varput, cdfid, mean_temp_id, mean(cloud_top_temp_int[where(cloud_top_temp_int ne -9999.0000)])
              ncdf_varput, cdfid, median_temp_id,temp_median
              ncdf_varput, cdfid, tenth_temp_id,tenth_per_temp
              bmax=max(histo_phase,bidx)
              ncdf_varput, cdfid, mode_phase_id,phase_bins[bidx]

              ncdf_varput, cdfid, center_lat_id,ship_latitude
              ncdf_varput, cdfid, center_lon_id,ship_longitude
              ncdf_varput,cdfid,count_5km_id,count_5km
              ncdf_varput,cdfid,count_1km_id,count_1km
              ncdf_varput, cdfid, nd_bins_id, nd_bins
              ncdf_varput, cdfid, nd_histo_id, float(histo_nd)
              ncdf_varput, cdfid, re_bins_id, re_bins
              ncdf_varput, cdfid, re_histo_id, float(histo_re)
              ncdf_varput, cdfid, lwp_bins_id, lwp_bins
              ncdf_varput, cdfid, lwp_histo_id, float(histo_lwp)
              ncdf_varput, cdfid, tau_bins_id, tau_bins
              ncdf_varput, cdfid, tau_histo_id, float(histo_tau)
              ncdf_varput, cdfid, szen_bins_id, solar_zenith_bins
              ncdf_varput, cdfid, szen_histo_id, float(histo_szen)
              ncdf_varput, cdfid, vzen_bins_id, view_zenith_bins
              ncdf_varput, cdfid, vzen_histo_id, float(histo_vzen)

              ncdf_varput, cdfid, temp_bins_id, temp_bins
              ncdf_varput, cdfid, temp_histo_id, float(histo_temp)
              ncdf_varput, cdfid, phase_bins_id, phase_bins
              ncdf_varput, cdfid, phase_histo_id, float(histo_phase)
              
              ncdf_varput,cdfid,cold_flag_id,cold_flag

              ncdf_close, cdfid

            endif ; count_5km gt 100 and count_1km gt 10 and sensor_zenith lt 30. and solar_zenith
          endfor  ; for k=0,n_elements(lat_vector_histo)-1 do begin
        endfor  ;for j=0,n_elements(lon_vector_histo)-1 do begin

        ;  Plot the histograms
        files=file_search(out_dir+output_file_string+'*_histo.cdf',count=num_hist_files)
        if 1 eq 0 then begin
        files=file_search(out_dir+output_file_string+'*_histo.cdf',count=num_hist_files)
        for i=0,num_hist_files-1 do begin
          print,files[i]
          fid=ncdf_open(files[i])
          vid=ncdf_varid(fid,'nd_bins') & ncdf_varget,fid,vid,nd_bins
          vid=ncdf_varid(fid,'nd_histo') & ncdf_varget,fid,vid,nd_histo
          vid=ncdf_varid(fid,'re_bins') & ncdf_varget,fid,vid,re_bins
          vid=ncdf_varid(fid,'re_histo') & ncdf_varget,fid,vid,re_histo
          vid=ncdf_varid(fid,'lwp_bins') & ncdf_varget,fid,vid,lwp_bins    
          vid=ncdf_varid(fid,'lwp_histo') & ncdf_varget,fid,vid,lwp_histo 
          vid=ncdf_varid(fid,'tau_bins') & ncdf_varget,fid,vid,tau_bins
          vid=ncdf_varid(fid,'tau_histo') & ncdf_varget,fid,vid,tau_histo
          vid=ncdf_varid(fid,'cloud_top_temp_bins') & ncdf_varget,fid,vid,temp_bins
          vid=ncdf_varid(fid,'cloud_top_temp_histo') & ncdf_varget,fid,vid,temp_histo          
          vid=ncdf_varid(fid,'solar_zenith_bins') & ncdf_varget,fid,vid,solar_zenith_bins
          vid=ncdf_varid(fid,'solar_zenith_histo') & ncdf_varget,fid,vid,solar_zenith_histo
          vid=ncdf_varid(fid,'view_zenith_bins') & ncdf_varget,fid,vid,view_zenith_bins
          vid=ncdf_varid(fid,'view_zenith_histo') & ncdf_varget,fid,vid,view_zenith_histo 
          vid=ncdf_varid(fid,'cloud_phase_bins') & ncdf_varget,fid,vid,phase_bins
          vid=ncdf_varid(fid,'cloud_phase_histo') & ncdf_varget,fid,vid,phase_histo           
          vid=ncdf_varid(fid,'mean_latitude') & ncdf_varget,fid,vid,mean_latitude  
          vid=ncdf_varid(fid,'mean_longitude') & ncdf_varget,fid,vid,mean_longitude     
          vid=ncdf_varid(fid,'mean_nd') & ncdf_varget,fid,vid,mean_nd     
          vid=ncdf_varid(fid,'mean_re') & ncdf_varget,fid,vid,mean_re    
          vid=ncdf_varid(fid,'mean_lwp') & ncdf_varget,fid,vid,mean_lwp  
          vid=ncdf_varid(fid,'mean_tau') & ncdf_varget,fid,vid,mean_tau   
          vid=ncdf_varid(fid,'mean_cloud_top_temp') & ncdf_varget,fid,vid,mean_temp   
          vid=ncdf_varid(fid,'mean_solar_zenith') & ncdf_varget,fid,vid,mean_solar_zenith
          vid=ncdf_varid(fid,'mean_view_zenith') & ncdf_varget,fid,vid,mean_view_zenith  
          vid=ncdf_varid(fid,'mode_cloud_phase') & ncdf_varget,fid,vid,mode_phase   
          vid=ncdf_varid(fid,'median_cloud_top_temp') & ncdf_varget,fid,vid,median_temp
          vid=ncdf_varid(fid,'tenth_percentile_cloud_top_temp') & ncdf_varget,fid,vid,tenth_temp    
          ncdf_close,fid          
          pxdim=900
          pydim=800
          xl=0.08 & xr=0.95
          yb=0.07 & yt=0.95
          sx=0.10
          sy=0.07
          numplots_x=2
          numplots_y=4
          position_plots,xl,xr,yb,yt,sx,sy,numplots_x,numplots_y,pos
          dx=0.01 & dy=0.01
          pnum=0
          p0=plot([0,1],[0,1],position=pos[pnum,*],/buffer,dimensions=[pxdim,pydim],axis_style=4,/nodata)
          pnum=2
          p1=plot(lwp_bins,lwp_histo/total(lwp_histo),/hist,position=pos[pnum,*],/current,/xstyle,$
            xtitle='LWP (g/m2)',ytitle='Frequency',yrange=[0,1],font_size=12)
          t1=text(pos[pnum,2]-25.*dx,pos[pnum,3]-5.*dy,'mean_lwp='+strcompress(mean_lwp,/remove_all),font_size=14)
          pnum=0
          p1=plot(nd_bins,nd_histo/total(nd_histo),/hist,position=pos[pnum,*],/current,/xstyle,$
            xtitle='Nd (cm-3)',ytitle='Frequency',yrange=[0,1],font_size=12) 
          t1=text(pos[pnum,2]-25.*dx,pos[pnum,3]-5.*dy,'mean_nd='+strcompress(mean_nd,/remove_all),font_size=14)   
          pnum=1
          p1=plot(re_bins,re_histo/total(re_histo),/hist,position=pos[1,*],/current,/xstyle,$
            xtitle='Re (microns)',ytitle='Frequency',yrange=[0,1],font_size=12)
          t1=text(pos[pnum,2]-25.*dx,pos[pnum,3]-5.*dy,'mean_re='+strcompress(mean_re,/remove_all),font_size=14) 
          pnum=5
          p1=plot(solar_zenith_bins,solar_zenith_histo/total(solar_zenith_histo),/hist,$
            position=pos[pnum,*],/current,/xstyle,$
            xtitle='Solar Zenith',ytitle='Frequency',yrange=[0,1],font_size=12) 
          t1=text(pos[pnum,2]-25.*dx,pos[pnum,3]-5.*dy,'mean_sza='+strcompress(mean_solar_zenith,/remove_all),font_size=14) 
          pnum=4
          p1=plot(view_zenith_bins,view_zenith_histo/total(view_zenith_histo),/hist,$
            position=pos[pnum,*],/current,/xstyle,$
            xtitle='View Zenith',ytitle='Frequency',yrange=[0,1],font_size=12)  
          t1=text(pos[pnum,2]-25.*dx,pos[pnum,3]-5.*dy,'mean_vza='+strcompress(mean_view_zenith,/remove_all),font_size=14)   
          pnum=3
          p1=plot(tau_bins,tau_histo/total(tau_histo),/hist,position=pos[pnum,*],/current,/xstyle,$
            xtitle='Tau',ytitle='Frequency',yrange=[0,1],font_size=12)
          t1=text(pos[pnum,2]-25.*dx,pos[pnum,3]-5.*dy,'mean_tau='+strcompress(mean_tau,/remove_all),font_size=14)
          pnum=6
          p1=plot(temp_bins,temp_histo/total(temp_histo),/hist,position=pos[pnum,*],/current,/xstyle,$
            xtitle='Cloud Top Temp',ytitle='Frequency',yrange=[0,1],font_size=12)
          t1=text(pos[pnum,2]-25.*dx,pos[pnum,3]-4.*dy,'mean_temp='+strcompress(mean_temp,/remove_all),font_size=14)
          t1=text(pos[pnum,2]-25.*dx,pos[pnum,3]-6.*dy,'median_temp='+strcompress(median_temp,/remove_all),font_size=14)
          t1=text(pos[pnum,2]-25.*dx,pos[pnum,3]-8.*dy,'10%_temp='+strcompress(tenth_temp,/remove_all),font_size=14)
          pnum=7
          p1=plot(phase_bins,phase_histo/total(phase_histo),/hist,position=pos[pnum,*],/current,/xstyle,$
            xtitle='Cloud Phase',ytitle='Frequency',yrange=[0,1],font_size=12)
          t1=text(pos[pnum,2]-25.*dx,pos[pnum,3]-5.*dy,'mode_phase='+strcompress(mode_phase,/remove_all),font_size=14)

          
          t1=text(0.1,0.97,'Lat='+strcompress(mean_latitude,/remove_all),font_size=14)  
          t1=text(0.3,0.97,'Lon='+strcompress(mean_longitude,/remove_all),font_size=14)  
                    
          parts=strsplit(file_basename(files[i]),'cdf',/extract)  
          ;p0.save,out_dir+parts[0]+'png',height=pydim  
          p0.save,parts[0]+'png',height=pydim       
          
        endfor    ;  end of loop through histogram plot
        endif  ;end of do this plot
    
        ;**********************************************
        ;  Regrid and plot
        ;**********************************************

        ;  This does the regridding and plotting
        ;if 1 eq 1 and num_hist_files gt 0 then begin  ;only plots if a hist file is created ;&&&&&&
        if 1 eq 0 then begin ;plots all files   
        ;if 1 eq 1 then begin
        ;*********************************************
        ;  Create a regular lat-lon grid at 1km
        ;*********************************************

        ;radius_earth=6357.d  ;km
        ;res=1. ;km
        ;dlat_1km=res*(180./(!dpi*radius_earth))
        ;dlon_1km=res*((180./(radius_earth*cos(mean(abs(lat_1km))*!dpi/180.d))))  
        ;lat_vector2=min(lat_1km) & while max(lat_vector2) lt max(lat_1km) do lat_vector2=[lat_vector2,max(lat_vector2)+dlat_1km]
        ;colon_vector2=min(colon_1km) & while max(colon_vector2) lt max(colon_1km) do colon_vector2=[colon_vector2,max(colon_vector2)+dlon_1km]

        dlat_1km=0.01 & dlon_1km=0.1  ;what we use for high res plots
        ;dlat_1km=0.1 & dlon_1km=0.5
        r=where(lat_1km ne -999.000)
        lat_vector2=floor(min(lat_1km[r])) & while max(lat_vector2) lt max(lat_1km) do lat_vector2=[lat_vector2,max(lat_vector2)+dlat_1km]
        r=where(colon_1km ne -999.000)
        colon_vector2=floor(min(colon_1km[r])) & while max(colon_vector2) lt max(colon_1km) do colon_vector2=[colon_vector2,max(colon_vector2)+dlon_1km]

        lon_vector2=colon_vector2
        result=where(colon_vector2 gt 180,count)
        if count gt 0 then lon_vector2[result]=colon_vector2[result]-360.0
        
        plot_mod06_data,lat_5km,lon_5km,colon_5km,lat_1km,lon_1km,colon_1km,out_dir,$
          lat_vector2,lon_vector2,colon_vector2,lat_vector_histo,colon_vector_histo,$  
          solar_zenith_angle_1km,sensor_zenith_angle_1km,good_data,$
          cloud_top_temp_1km,cloud_top_temperature,cloud_optical_thickness,$
          cloud_effective_radius,cloud_water_path,cloud_phase,nd_array,cw,output_file_string,$
          plot_effective_radius,plot_nd_array,plot_liquid_water_path,plot_optical_depth,$  ;output regridded arrays
          plot_cloud_top_temp,plot_cloud_phase,plot_good_data,plot_sensor_zenith,plot_solar_zenith
    
        if 1 eq 1 then begin
        cdfid=ncdf_create(out_dir+output_file_string+'.cdf', /clobber)

        lat_did=ncdf_dimdef(cdfid, 'lat_did', n_elements(lat_vector2))
        lon_did=ncdf_dimdef(cdfid, 'lon_did', n_elements(lon_vector2))

        lat_vector_id=ncdf_vardef(cdfid, 'latitude_vector', [lat_did], /float)
        lon_vector_id=ncdf_vardef(cdfid, 'longitude_vector', [lon_did], /float)

        re_id=ncdf_vardef(cdfid, 're_interp_um', [lon_did, lat_did], /float)
        nd_id=ncdf_vardef(cdfid, 'nd_interp_per_cubic_cm', [lon_did, lat_did], /float)
        lwp_id=ncdf_vardef(cdfid, 'lwp_interp_g_per_sq_m', [lon_did, lat_did], /float)
        tau_id=ncdf_vardef(cdfid, 'tau_interp_unitless', [lon_did, lat_did], /float)
        ctt_id=ncdf_vardef(cdfid, 'cloud_top_temp_1km', [lon_did, lat_did], /float)
        cloud_phase_id=ncdf_vardef(cdfid,'cloud_phase_1km', [lon_did, lat_did], /float)
        good_data_id=ncdf_vardef(cdfid,'good_data_flag_1km', [lon_did, lat_did], /float)
        sensor_zenith_id=ncdf_vardef(cdfid,'sensor_zenith_1km', [lon_did, lat_did], /float)
        solar_zenith_id=ncdf_vardef(cdfid,'solar_zenith_1km', [lon_did, lat_did], /float)
  
        ncdf_control, cdfid, /endef
        ncdf_varput,cdfid,re_id,plot_effective_radius
        ncdf_varput,cdfid,nd_id,plot_nd_array
        ncdf_varput,cdfid,lwp_id,plot_liquid_water_path
        ncdf_varput,cdfid,tau_id,plot_optical_depth
        ncdf_varput,cdfid,lat_vector_id, lat_vector2
        ncdf_varput,cdfid,lon_vector_id, lon_vector2
        ncdf_varput,cdfid,ctt_id,plot_cloud_top_temp
        ncdf_varput,cdfid,cloud_phase_id,plot_cloud_phase
        ncdf_varput,cdfid,good_data_id,plot_good_data
        ncdf_varput,cdfid,sensor_zenith_id,plot_sensor_zenith
        ncdf_varput,cdfid,solar_zenith_id,plot_solar_zenith

        ncdf_close, cdfid
        endif  ;write regrid file
        endif  ;do plotting if there are histogram files
      endif else begin  ;found good data
        spawn,'rm '+mod06_file
        spawn,'rm '+mod03_file
        print,'no good data, files removed'
      endelse  ;remove files with no good data
    endif  ;found mod03
  endfor  ;end of loop through mod06 files
endfor  ;end of loop through doy
;stop
end

pro plot_mod06_data,lat_5km,lon_5km,colon_5km,lat_1km,lon_1km,colon_1km,out_dir,$
    lat_vector2,lon_vector2,colon_vector2,lat_vector_histo,colon_vector_histo,$  
    solar_zenith_angle_1km,sensor_zenith_angle_1km,good_data,$
    cloud_top_temp_1km,cloud_top_temperature,cloud_optical_thickness,$
    cloud_effective_radius,cloud_water_path,cloud_phase,nd_array,cw,output_file_string,$
    plot_effective_radius,plot_nd_array,plot_liquid_water_path,plot_optical_depth,$  ;output regridded arrays
    plot_cloud_top_temp,plot_cloud_phase,plot_good_data,plot_sensor_zenith,plot_solar_zenith

print,'start plotting'

;  Top is the last color to scale 256 colors, 0-255
top_color=252
;  Colortable  0-252  253=white
;mytable=colortable(39,ncolors=254)
mytable=colortable(5,ncolors=254)
;254=hot pink               ;gray=255
mytable=[mytable,transpose([238,18,137]),transpose([180,180,180])]
mycbtable=mytable[0:top_color,*]

;***********************
;  Set up the plot
;***********************

pxdim=1000
pydim=1000

;  Set up the positions
xl=0.06 & xr=0.90
yb=0.05 & yt=0.95
sx=0.13
sy=0.05
numplots_x=2
numplots_y=2;num_files/numplots_x
position_plots,xl,xr,yb,yt,sx,sy,numplots_x,numplots_y,pos
;  Colorbar position
cbpos=pos
cbpos[*,0]=pos[*,2]+0.07
cbpos[*,2]=cbpos[*,0]+0.007
cbpos[*,1]=cbpos[*,1]+0.02
cbpos[*,3]=cbpos[*,3]-0.02
dx=0.01
dy=0.01
;  Date format for plotting
dummy=label_date(date_format=['%Z/%M/%D %H:%I'])
pnum=0
p0=plot([0,1],[0,1],position=pos[pnum,*],/buffer,dimensions=[pxdim,pydim],axis_style=4,/nodata)
d=p0.convertcoord([pos[pnum,0],pos[pnum,2]],[pos[pnum,1],pos[pnum,3]],/normal,/to_device)
isx=(d[0,1,0]-d[0,0,0])
isy=(d[1,1,0]-d[1,0,0])

r=where(lat_1km ne -999.000)
ulat=max(lat_1km[r])  ;upper lat of box
llat=min(lat_1km[r])  ;lower lat of box
r=where(colon_1km ne -999.000)
llon=min(colon_1km[r])  ;left lon of box
rlon=max(colon_1km[r])  ;right lon of box

bulat=-50
bllat=-75
bllon=60
brlon=150
if bulat gt ulat then bulat=ulat  ;upper lat of box
if bllat lt llat then bllat=llat  ;lower lat of box
if bllon lt llon then bllon=llon  ;left lon of box
if brlon gt rlon then brlon=rlon  ;right lon of box

if (max(lon_1km) gt 179 and min(lon_1km) lt -179) or $
  (max(lat_1km) lt -60 ) or $
  (min(lat_1km) lt -75 ) then begin
  map_projection='Polar Stereographic'
  label_pos=1
endif else begin
  map_projection='Mercator'
  label_pos=0
endelse
print,map_projection
  
;if mean(sensor_zenith_int) lt 30. and mean(solar_zenith_int) lt 60. then begin

;  Calculate regular grid array of the plotting box
;delta=0.01
;ynum=fix((ulat-llat)/delta)
;grid_lat=findgen(ynum)*delta+llat
;xnum=fix((rlon-llon)/delta)
;grid_lon=findgen(xnum)*delta+llon
grid_lat=lat_vector2
grid_lon=colon_vector2

;**************************
;  *** Start new plot ***
;**************************
pnum=0
p0=plot([0,1],[0,1],position=pos[pnum,*],/buffer,dimensions=[pxdim,pydim],axis_style=4,/nodata)

;  CLOUD_WATER_PATH
;  The units are log g/m2
if 1 eq 1 then begin
  print,'cloud water path'
  pnum=0
  data_var=cloud_water_path
  data_lat=lat_1km
  data_lon=colon_1km
  r=where(data_var gt 0,c)
  if c gt 0 then data_var[r]=alog10(data_var[r])
  r=where(data_var ne -9999,c)
  if c gt 5 then begin  ;there has to be some data to grid
    dmax_wp=max(data_var[r])
    dmin_wp=min(data_var[r])
    good_lat=data_lat[r]
    good_lon=data_lon[r]
    good_var=data_var[r]
    grid_input,good_lon,good_lat,good_var,xyz,newdata_var,/degree,/sphere,duplicates='Avg',epsilon=0.01
    lon = !radeg * atan(xyz[1,*],xyz[0,*]) &   lat = !radeg * asin(xyz[2,*])
    ;  Triangulate the data
    ;qhull,good_lon,good_lat,triangles,/delaunay,sphere=s
    print,'qhull cloud water path'
    qhull,lon,lat,triangles,/delaunay,sphere=s
    print,'griddata cloud water path'
    ;triangulate,good_lon,good_lat,triangles,sphere=s
    ;grid_var=griddata(good_lon,good_lat,good_var,xout=grid_lon,yout=grid_lat,$
    grid_var=griddata(lon,lat,newdata_var,xout=grid_lon,yout=grid_lat,$
      /grid,/degrees,/sphere,triangles=triangles,$
      /kriging,min_points=16,sectors=8,empty_sectors=3,missing=-9999)
  ;;   Bytscal the data
  ;data_image=bytscl(grid_var,top=top_color,min=dmin_wp,max=dmax_wp)
  ;result=where(grid_var eq -9999,count)
  ;if count gt 0 then data_image[result]=255  ;gray
  ;p1=image(data_image,grid_lon,grid_lat,$  ;  I have called this one p1
  ;  limit=[llat,llon,ulat,rlon],$  ;map limit vector
  ;  /current,$  ;Put it in the current open plot window p0
  ;  grid_units='degrees',$
  ;  center_longitude=((rlon-llon)/2.0)+llon,$
  ;  /box_axes,$
  ;  label_position=label_pos,$
  ;  font_size=12,$  ;bigger font
  ;  linestyle=2,$  ;grid will be dashed line
  ;  rgb_table=mytable,$  ;use color table 5
  ;  map_projection=map_projection,$  ;map projection
  ;  position=pos[pnum,*])
  ;p2=mapcontinents(color='white',/hires,thick=2)  ;add the continent lines
  ;p3=plot([bllon,brlon,brlon,bllon,bllon],[bllat,bllat,bulat,bulat,bllat],/overplot,/data,color='green',thick=3)
  ;;  Put on a colorbar
  ;c0=colorbar(range=[dmin_wp,dmax_wp],$  ;the colorbar is to match the image labeled p1
  ;  orientation=1,position=reform(cbpos[pnum,*]),font_size=12,rgb_table=mycbtable,$
  ;  tickdir=1,border_on=1,title='LOG Cloud Water Path 1km')
  ;t1=text(pos[pnum,0],pos[pnum,3]+dy,'Cloud Water Path',font_size=12)
    plot_liquid_water_path=grid_var
  endif else begin
    plot_liquid_water_path=make_array(n_elements(grid_lon),n_elements(grid_lat),value=-9999)
  endelse
endif

;  CW 1km
if 1 eq 0 then begin
  print,'cw'
  data_var=cw
  data_lat=lat_1km
  data_lon=colon_1km
  r=where(data_var ne -9999,c)
  dmax_cw=max(data_var[r])
  dmin_cw=min(data_var[r])
  good_lat=data_lat[r]
  good_lon=data_lon[r]
  good_var=data_var[r]
  grid_input,good_lon,good_lat,good_var,xyz,newdata_var,/degree,/sphere,duplicates='Avg',epsilon=0.01
  lon = !radeg * atan(xyz[1,*],xyz[0,*]) &   lat = !radeg * asin(xyz[2,*])
  ;  Triangulate the data
  qhull,lon,lat,triangles,/delaunay,sphere=s
  grid_var=griddata(lon,lat,newdata_var,xout=grid_lon,yout=grid_lat,$
    /grid,/degrees,/sphere,triangles=triangles,$
    /kriging,min_points=16,sectors=8,empty_sectors=3,missing=-9999)
  ;   Bytscal the data
  data_image=bytscl(grid_var,top=top_color,min=dmin_cw,max=dmax_cw)
  result=where(grid_var eq -9999,count)
  if count gt 0 then data_image[result]=255  ;gray
  pnum=6
  p1=image(data_image,grid_lon,grid_lat,$  ;  I have called this one p1
    limit=[llat,llon,ulat,rlon],$  ;map limit vector
    /current,$  ;Put it in the current open plot window p0
    grid_units='degrees',$
    /box_axes,$
    label_position=label_pos,$
    font_size=12,$  ;bigger font
    linestyle=2,$  ;grid will be dashed line
    rgb_table=mytable,$  ;use color table 5
    map_projection=map_projection,$  ;map projection
    position=pos[pnum,*])
  p2=mapcontinents(color='white',/hires,thick=2)  ;add the continent lines
  ;  Put on a colorbar
  c0=colorbar(range=[dmin_cw,dmax_cw],$  ;the colorbar is to match the image labeled p1
    orientation=1,position=reform(cbpos[pnum,*]),font_size=12,rgb_table=mycbtable,$
    tickdir=1,border_on=1,title='CW 1km')
endif

;  CLoud optical thickness 1km
if 1 eq 1 then begin
  print,'optical thickness'
  pnum=1
  data_var=cloud_optical_thickness
  data_lat=lat_1km
  data_lon=colon_1km
  r=where(data_var ne -9999,c)
  if c gt 5 then begin
    dmax_tau=50;max(data_var[r])
    dmin_tau=0;min(data_var[r])
    good_lat=data_lat[r]
    good_lon=data_lon[r]
    good_var=data_var[r]
    grid_input,good_lon,good_lat,good_var,xyz,newdata_var,/degree,/sphere,duplicates='Avg',epsilon=0.01
    lon = !radeg * atan(xyz[1,*],xyz[0,*]) &   lat = !radeg * asin(xyz[2,*])
    ;  Triangulate the data
    print,'qhull opt thick'
    qhull,lon,lat,triangles,/delaunay,sphere=s
    print,'grid data opt thick'
    grid_var=griddata(lon,lat,newdata_var,xout=grid_lon,yout=grid_lat,$
      /grid,/degrees,/sphere,triangles=triangles,$
      /kriging,min_points=16,sectors=8,empty_sectors=3,missing=-9999)
    ;   Bytscal the data
    data_image=bytscl(grid_var,top=top_color,min=dmin_tau,max=dmax_tau)
    result=where(grid_var eq -9999,count)
    if count gt 0 then data_image[result]=255  ;gray
    p1=image(data_image,grid_lon,grid_lat,$  ;  I have called this one p1
      limit=[llat,llon,ulat,rlon],$  ;map limit vector
      /current,$  ;Put it in the current open plot window p0
      grid_units='degrees',$
      center_longitude=((rlon-llon)/2.0)+llon,$
      /box_axes,$
      label_position=label_pos,$
      font_size=12,$  ;bigger font
      linestyle=2,$  ;grid will be dashed line
      rgb_table=mytable,$  ;use color table 5
      map_projection=map_projection,$  ;map projection
      position=pos[pnum,*])
    p2=mapcontinents(color='white',/hires,thick=2)  ;add the continent lines
    ;  Put on a colorbar
    c0=colorbar(range=[dmin_tau,dmax_tau],$  ;the colorbar is to match the image labeled p1
      orientation=1,position=reform(cbpos[pnum,*]),font_size=12,rgb_table=mycbtable,$
      tickdir=1,border_on=1,title='Cloud Optical Thickness 1km')
    t1=text(pos[pnum,0],pos[pnum,3]+dy,'Cloud Optical Thickness 1km',font_size=12)
    plot_optical_depth=grid_var
  endif else begin
    plot_optical_depth=make_array(n_elements(grid_lon),n_elements(grid_lat),value=-9999)    
  endelse
endif

;  Solar Zenith Angle 1km
if 1 eq 1 then begin
  print,'solar zenith'
  pnum=2
  data_var=solar_zenith_angle_1km
  data_lat=lat_1km
  data_lon=colon_1km
  r=where(data_var gt -300.0,c)
  dmax_solz=max(data_var[r])
  dmin_solz=min(data_var[r])
  good_lat=data_lat[r]
  good_lon=data_lon[r]
  good_var=data_var[r]
  grid_input,good_lon,good_lat,good_var,xyz,newdata_var,/degree,/sphere,duplicates='Avg',epsilon=0.01
  lon = !radeg * atan(xyz[1,*],xyz[0,*]) &   lat = !radeg * asin(xyz[2,*])
  ;  Triangulate the data
  qhull,lon,lat,triangles,/delaunay,sphere=s
  grid_var=griddata(lon,lat,newdata_var,xout=grid_lon,yout=grid_lat,$
    /grid,/degrees,/sphere,triangles=triangles,$
    ; min_points fewer, set to missing
    ;/kriging,min_points=8,sectors=8,empty_sectors=3,missing=-9999) ;original
    ;/kriging,min_points=4,sectors=8,empty_sectors=6,missing=-9999)  ;better
    /kriging,min_points=16,sectors=8,empty_sectors=3,missing=-9999)  ;much better 12,8,3
  ;   Bytscal the data
  data_image=bytscl(grid_var,top=top_color,min=dmin_solz,max=dmax_solz)
  result=where(grid_var eq -9999 or grid_var eq -327.670,count)
  if count gt 0 then data_image[result]=255  ;gray
  p1=image(data_image,grid_lon,grid_lat,$  ;  I have called this one p1
    limit=[llat,llon,ulat,rlon],$  ;map limit vector
    /current,$  ;Put it in the current open plot window p0
    grid_units='degrees',$
    center_longitude=((rlon-llon)/2.0)+llon,$
    /box_axes,$
    label_position=label_pos,$
    font_size=12,$  ;bigger font
    linestyle=2,$  ;grid will be dashed line
    rgb_table=mytable,$  ;use color table 5
    map_projection=map_projection,$  ;map projection
    position=pos[pnum,*])
  p2=mapcontinents(color='white',/hires,thick=2)  ;add the continent lines
  ;  Put on a colorbar
  c0=colorbar(range=[dmin_solz,dmax_solz],$  ;the colorbar is to match the image labeled p1
    orientation=1,position=reform(cbpos[pnum,*]),font_size=12,rgb_table=mycbtable,$
    tickdir=1,border_on=1,title='Solar Zenith Angle 1km')
  t1=text(pos[pnum,0],pos[pnum,3]+dy,'Solar Zenith lt 60',font_size=12)
  plot_solar_zenith=grid_var
endif

;  Good Data 1km
if 1 eq 1 then begin
  print,'good data'
  pnum=3
  data_var=good_data
  data_lat=lat_1km
  data_lon=colon_1km
  r=where(data_var ne -9999,c)
  dmax_good=max(data_var[r])
  dmin_good=min(data_var[r])
  good_lat=data_lat[r]
  good_lon=data_lon[r]
  good_var=data_var[r]
  grid_input,good_lon,good_lat,good_var,xyz,newdata_var,/degree,/sphere,duplicates='First',epsilon=0.01
  lon = !radeg * atan(xyz[1,*],xyz[0,*]) &   lat = !radeg * asin(xyz[2,*])
  ;  Triangulate the data
  qhull,lon,lat,triangles,/delaunay,sphere=s
  ;grid_var=griddata(lon,lat,newdata_var,xout=grid_lon,yout=grid_lat,$
  ;  /grid,/degrees,/sphere,triangles=triangles,$
  ;  /kriging,min_points=16,sectors=8,empty_sectors=3,missing=-9999)
  grid_var=griddata(lon,lat,newdata_var,xout=grid_lon,yout=grid_lat,$
    /grid,/degrees,/sphere,triangles=triangles,$
    /nearest_neighbor,missing=-9999)
  r=where(plot_solar_zenith eq -9999,c)
  if c gt 0 then grid_var[r]=-9999
  ;   Bytscal the data
  good_data_ct=['deep sky blue','white','gold','red','orange','light gray']
  data_image=byte(grid_var)
  result=where(grid_var eq -9999,count)
  if count gt 0 then data_image[result]=5;gray
  result=where(grid_var eq 0,count)
  if count gt 0 then data_image[result]=0
  result=where(grid_var eq 1,count)
  if count gt 0 then data_image[result]=1
  result=where(grid_var eq 2,count)
  if count gt 0 then data_image[result]=2
  result=where(grid_var eq 3,count)
  if count gt 0 then data_image[result]=3
  result=where(grid_var eq 4,count)
  if count gt 0 then data_image[result]=4  
  p1=image(data_image,grid_lon,grid_lat,$  ;  I have called this one p1
    limit=[llat,llon,ulat,rlon],$  ;map limit vector
    /current,$  ;Put it in the current open plot window p0
    grid_units='degrees',$
    center_longitude=((rlon-llon)/2.0)+llon,$
    /box_axes,$
    label_position=label_pos,$
    font_size=12,$  ;bigger font
    linestyle=2,$  ;grid will be dashed line
    rgb_table=good_data_ct,$  ;use color table 5
    map_projection=map_projection,$  ;map projection
    position=pos[pnum,*])
  p2=mapcontinents(color='white',/hires,thick=2)  ;add the continent lines
  ;  Put on a colorbar
  ;c0=colorbar(range=[0,4],$  ;the colorbar is to match the image labeled p1
  ;  orientation=1,position=reform(cbpos[pnum,*]),font_size=12,rgb_table=mycbtable,$
  ;  tickdir=1,border_on=1,title='good data 1km')
  t1=text(pos[pnum,0]+0*dx,pos[pnum,3]+3*dy,'good data=white',font_size=10)
  t2=text(pos[pnum,0]+15*dx,pos[pnum,3]+3*dy,'clear sky=blue',font_size=10,color='deep sky blue')
  t2=text(pos[pnum,0]+15*dx,pos[pnum,3]+1.5*dy,'sensor zenith > 30=gold',font_size=10,color='gold')
  t2=text(pos[pnum,0]+0*dx,pos[pnum,3]+1.5*dy,'solar zenith > 60=red',font_size=10,color='red')
  t3=text(pos[pnum,0]+0*dx,pos[pnum,3]+0*dy,'sensor zenith > 30 & solar zenith > 60=orange',font_size=10,color='orange')
  ;t1=text(pos[pnum,0],pos[pnum,3]+dy,'Good data=solzen<60 senzen<30 re0-50 liq phase',font_size=12)
  plot_good_data=grid_var
endif

;  Sensor Zenith Angle 1km
if 1 eq 1 then begin
  print,'sensor zenith'
  pnum=0
  data_var=sensor_zenith_angle_1km
  data_lat=lat_1km
  data_lon=colon_1km
  r=where(data_var gt -300.0,c)
  dmax=max(data_var[r])
  dmin=min(data_var[r])
  good_lat=data_lat[r]
  good_lon=data_lon[r]
  good_var=data_var[r]
  grid_input,good_lon,good_lat,good_var,xyz,newdata_var,/degree,/sphere,duplicates='Avg',epsilon=0.01
  lon = !radeg * atan(xyz[1,*],xyz[0,*]) &   lat = !radeg * asin(xyz[2,*])
  ;  Triangulate the data
  qhull,lon,lat,triangles,/delaunay,sphere=s
  grid_var=griddata(lon,lat,newdata_var,xout=grid_lon,yout=grid_lat,$
    /grid,/degrees,/sphere,triangles=triangles,$
    /kriging,min_points=16,sectors=8,empty_sectors=3,missing=-9999)
  ;r=where(grid_var ge 0.5,c)
  ;if c gt 0 then grid_var[r]=1
  ;r=where(grid_var lt 0.5,c)
  ;if c gt 0 then grid_var[r]=0
  ;   Bytscal the data
  data_image=bytscl(grid_var,top=top_color,min=dmin,max=dmax)
  result=where(grid_var eq -9999 or grid_var eq -327.670,count)
  if count gt 0 then data_image[result]=255  ;gray
  p1=image(data_image,grid_lon,grid_lat,$  ;  I have called this one p1
    limit=[llat,llon,ulat,rlon],$  ;map limit vector
    /current,$  ;Put it in the current open plot window p0
    grid_units='degrees',$
    center_longitude=((rlon-llon)/2.0)+llon,$
    /box_axes,$
    label_position=label_pos,$
    font_size=12,$  ;bigger font
    linestyle=2,$  ;grid will be dashed line
    rgb_table=mytable,$  ;use color table 5
    map_projection=map_projection,$  ;map projection
    position=pos[pnum,*])
  p2=mapcontinents(color='white',/hires,thick=2)  ;add the continent lines
  ;  Put on a colorbar
  c0=colorbar(range=[dmin,dmax],$  ;the colorbar is to match the image labeled p1
    orientation=1,position=reform(cbpos[pnum,*]),font_size=12,rgb_table=mycbtable,$
    tickdir=1,border_on=1,title='Sensor Zenith 1km')
  t1=text(pos[pnum,0],pos[pnum,3]+dy,'Sensor Zenith lt 30',font_size=12)
  plot_sensor_zenith=grid_var
endif


p0.save,out_dir+output_file_string+'.lwp_tau.png',height=pydim



;****************************
;  Start next image
;****************************
pnum=0
p0=plot([0,1],[0,1],position=pos[pnum,*],/buffer,dimensions=[pxdim,pydim],axis_style=4,/nodata)

;  CLOUD_TOP_TEMP_1km 
print,'cloud top temp 1km'
pnum=0
data_var=cloud_top_temp_1km
data_var=data_var;[0:*:10,0:*:10]
data_lat=lat_1km;[0:*:10,0:*:10]
data_lon=colon_1km;[0:*:10,0:*:10]
r=where(data_var ne -9999,c)
dmax_temp=max(data_var[r])
dmin_temp=min(data_var[r])
good_lat=data_lat[r]
good_lon=data_lon[r]
good_var=data_var[r]
grid_input,good_lon,good_lat,good_var,xyz,newdata_var,/degrees,/sphere,duplicates='Avg',epsilon=0.01
lon = !radeg * atan(xyz[1,*],xyz[0,*]) &   lat = !radeg * asin(xyz[2,*])
;  Triangulate the data
;qhull,good_lon,good_lat,triangles,/delaunay,sphere=s
qhull,lon,lat,triangles,/delaunay,sphere=s
;grid_var=griddata(good_lon,good_lat,good_var,xout=grid_lon,yout=grid_lat,$
grid_var=griddata(lon,lat,newdata_var,xout=grid_lon,yout=grid_lat,$
  /grid,/degrees,/sphere,triangles=triangles,$
  /kriging,min_points=16,sectors=8,empty_sectors=3,missing=-9999)
;  Bytscal the data
data_image=bytscl(grid_var,top=top_color,min=dmin_temp,max=dmax_temp)
result=where(grid_var eq -9999,count)
if count gt 0 then data_image[result]=255  ;gray
p1=image(data_image,grid_lon,grid_lat,$  ;  I have called this one p1
  limit=[llat,llon,ulat,rlon],$  ;map limit vector
  /current,$  ;Put it in the current open plot window p0
  grid_units='degrees',$
  center_longitude=((rlon-llon)/2.0)+llon,$
  ;max_value=dmax_temp,min_value=dmin_temp,$
  /box_axes,$
  label_position=label_pos,$
  font_size=12,$  ;bigger font
  linestyle=2,$  ;grid will be dashed line
  rgb_table=mytable,$  ;use color table 5
  map_projection=map_projection,$  ;map projection
  position=pos[pnum,*])
p2=mapcontinents(color='white',/hires,thick=2)  ;add the continent lines
p3=plot([bllon,brlon,brlon,bllon,bllon],[bllat,bllat,bulat,bulat,bllat],/overplot,/data,color='green',thick=3)
;  Put on a colorbar
c0=colorbar(range=[dmin_temp,dmax_temp],$  ;the colorbar is to match the image labeled p1
  orientation=1,position=reform(cbpos[pnum,*]),font_size=12,rgb_table=mycbtable,$
  tickdir=1,border_on=1,title='Cloud Top Temp 1km')
t1=text(pos[pnum,0],pos[pnum,3]+dy,'Cloud Top Temp 1km',font_size=12)  
plot_cloud_top_temp=grid_var   

;  CLOUD_TOP_TEMPERATURE
if 1 eq 0 then begin
print,'cloud top temp 5km'
data_var=cloud_top_temperature
data_var=data_var;[0:*:2,0:*:2]
data_lat=lat_5km;[0:*:2,0:*:2]
data_lon=colon_5km;[0:*:2,0:*:2]
r=where(data_var ne -9999,c)
dmax_temp=max(data_var[r])
dmin_temp=min(data_var[r])
good_lat=data_lat[r]
good_lon=data_lon[r]
good_var=data_var[r]
;  Triangulate the data
qhull,good_lon,good_lat,triangles,/delaunay,sphere=s
grid_var=griddata(good_lon,good_lat,good_var,xout=grid_lon,yout=grid_lat,$
  /grid,/degrees,/sphere,triangles=triangles,$
  /kriging,min_points=16,sectors=8,empty_sectors=3,missing=-9999)
;  Bytscal the data
data_image=bytscl(grid_var,top=top_color,min=dmin_temp,max=dmax_temp)
result=where(grid_var eq -9999,count)
if count gt 0 then data_image[result]=255  ;gray
pnum=1
p1=image(data_image,grid_lon,grid_lat,$  ;  I have called this one p1
  limit=[llat,llon,ulat,rlon],$  ;map limit vector
  /current,$  ;Put it in the current open plot window p0
  grid_units='degrees',$
  center_longitude=((rlon-llon)/2.0)+llon,$
  /box_axes,$
  label_position=label_pos,$
  font_size=12,$  ;bigger font
  linestyle=2,$  ;grid will be dashed line
  rgb_table=mytable,$  ;use color table 5
  map_projection=map_projection,$  ;map projection
  position=pos[pnum,*])
p2=mapcontinents(color='white',/hires,thick=2)  ;add the continent lines
;  Put on a colorbar
c0=colorbar(range=[dmin_temp,dmax_temp],$  ;the colorbar is to match the image labeled p1
  orientation=1,position=reform(cbpos[pnum,*]),font_size=12,rgb_table=mycbtable,$
  tickdir=1,border_on=1,title='Cloud Top Temp 5km')
endif  
  
  ;  CLOUD_EFFECTIVE_RADIUS
print,'cloud effective radius'
if 1 eq 1 then begin
pnum=1
data_var=cloud_effective_radius
data_var=data_var
data_lat=lat_1km
data_lon=colon_1km
r=where(data_var ne -9999,c)
if c gt 5 then begin
dmax_re=30;max(data_var[r])
dmin_re=0;min(data_var[r])
good_lat=data_lat[r]
good_lon=data_lon[r]
good_var=data_var[r]
grid_input,good_lon,good_lat,good_var,xyz,newdata_var,/degree,/sphere,duplicates='Avg',epsilon=0.01
lon = !radeg * atan(xyz[1,*],xyz[0,*]) &   lat = !radeg * asin(xyz[2,*])
;  Triangulate the data
;qhull,good_lon,good_lat,triangles,/delaunay,sphere=s
qhull,lon,lat,triangles,/delaunay,sphere=s
;triangulate,good_lon,good_lat,triangles,sphere=s
;grid_var=griddata(good_lon,good_lat,good_var,xout=grid_lon,yout=grid_lat,$
grid_var=griddata(lon,lat,newdata_var,xout=grid_lon,yout=grid_lat,$
  /grid,/degrees,/sphere,triangles=triangles,$
  /kriging,min_points=16,sectors=8,empty_sectors=3,missing=-9999)
;   Bytscal the data
data_image=bytscl(grid_var,top=top_color,min=dmin_re,max=dmax_re)
result=where(grid_var eq -9999,count)
if count gt 0 then data_image[result]=255  ;gray
p1=image(data_image,grid_lon,grid_lat,$  ;  I have called this one p1
  limit=[llat,llon,ulat,rlon],$  ;map limit vector
  /current,$  ;Put it in the current open plot window p0
  grid_units='degrees',$
  center_longitude=((rlon-llon)/2.0)+llon,$
  /box_axes,$
  label_position=label_pos,$
  font_size=12,$  ;bigger font
  linestyle=2,$  ;grid will be dashed line
  rgb_table=mytable,$  ;use color table 5
  map_projection=map_projection,$  ;map projection
  position=pos[pnum,*])
p2=mapcontinents(color='white',/hires,thick=2)  ;add the continent lines
p3=plot([bllon,brlon,brlon,bllon,bllon],[bllat,bllat,bulat,bulat,bllat],/overplot,/data,color='green',thick=3)
;  Put on a colorbar
c0=colorbar(range=[dmin_re,dmax_re],$  ;the colorbar is to match the image labeled p1
  orientation=1,position=reform(cbpos[pnum,*]),font_size=12,rgb_table=mycbtable,$
  tickdir=1,border_on=1,title='Cloud Effective Radius 1km')
t1=text(pos[pnum,0],pos[pnum,3]+dy,'Cloud Effective Radius 1km',font_size=12)     
plot_effective_radius=grid_var  ;output regridded array
endif else begin
  plot_effective_radius=make_array(n_elements(grid_lon),n_elements(grid_lat),value=-9999)
endelse
endif

;  ND_ARRAY
print,'nd array'
if 1 eq 1 then begin
pnum=2
data_var=nd_array
;r=where(data_var gt 0,c)
;data_var[r]=alog10(data_var[r])
data_lat=lat_1km
data_lon=colon_1km
r=where(data_var ne -9999,c)
if c gt 5 then begin
dmax_nd=200;max(data_var[r])
dmin_nd=0;min(data_var[r])
good_lat=data_lat[r]
good_lon=data_lon[r]
good_var=data_var[r]
grid_input,good_lon,good_lat,good_var,xyz,newdata_var,/degree,/sphere,duplicates='Avg',epsilon=0.01
lon = !radeg * atan(xyz[1,*],xyz[0,*]) &   lat = !radeg * asin(xyz[2,*])
;  Triangulate the data
;qhull,good_lon,good_lat,triangles,/delaunay,sphere=s
qhull,lon,lat,triangles,/delaunay,sphere=s
;triangulate,good_lon,good_lat,triangles,sphere=s
;grid_var=griddata(good_lon,good_lat,good_var,xout=grid_lon,yout=grid_lat,$
grid_var=griddata(lon,lat,newdata_var,xout=grid_lon,yout=grid_lat,$
  /grid,/degrees,/sphere,triangles=triangles,$
  /kriging,min_points=16,sectors=8,empty_sectors=3,missing=-9999)
;   Bytscal the data
data_image=bytscl(grid_var,top=top_color,min=dmin_nd,max=dmax_nd)
result=where(grid_var eq -9999,count)
if count gt 0 then data_image[result]=255  ;gray
p1=image(data_image,grid_lon,grid_lat,$  ;  I have called this one p1
  limit=[llat,llon,ulat,rlon],$  ;map limit vector
  /current,$  ;Put it in the current open plot window p0
  grid_units='degrees',$
  center_longitude=((rlon-llon)/2.0)+llon,$
  /box_axes,$
  label_position=label_pos,$
  font_size=12,$  ;bigger font
  linestyle=2,$  ;grid will be dashed line
  rgb_table=mytable,$  ;use color table 5
  map_projection=map_projection,$  ;map projection
  position=pos[pnum,*])
p2=mapcontinents(color='white',/hires,thick=2)  ;add the continent lines
p3=plot([bllon,brlon,brlon,bllon,bllon],[bllat,bllat,bulat,bulat,bllat],/overplot,/data,color='green',thick=3)
;  Put on a colorbar
c0=colorbar(range=[dmin_nd,dmax_nd],$  ;the colorbar is to match the image labeled p1
  orientation=1,position=reform(cbpos[pnum,*]),font_size=12,rgb_table=mycbtable,$
  tickdir=1,border_on=1,title='Nd array 1km')
t1=text(pos[pnum,0],pos[pnum,3]+dy,'Nd Array 1km',font_size=12)     
plot_nd_array=grid_var
endif else begin
  plot_nd_array=make_array(n_elements(grid_lon),n_elements(grid_lat),value=-9999)
endelse

endif

;  CLOUD_PHASE 0=missing, 1=clear, 2=liquid, 3=ice, 4=undetermined
print,'cloud phase'
if 1 eq 1 then begin
pnum=3
data_var=cloud_phase
data_lat=lat_1km
data_lon=colon_1km
r=where(data_var ne -9999,c)
dmax_phase=max(data_var[r])
dmin_phase=min(data_var[r])
good_lat=data_lat[r]
good_lon=data_lon[r]
good_var=data_var[r]
grid_input,good_lon,good_lat,good_var,xyz,newdata_var,/degree,/sphere,duplicates='First',epsilon=0.01
lon = !radeg * atan(xyz[1,*],xyz[0,*]) &   lat = !radeg * asin(xyz[2,*])
;  Triangulate the data
;qhull,good_lon,good_lat,triangles,/delaunay,sphere=s
qhull,lon,lat,triangles,/delaunay,sphere=s
;triangulate,good_lon,good_lat,triangles,sphere=s
;grid_var=griddata(good_lon,good_lat,good_var,xout=grid_lon,yout=grid_lat,$
;grid_var=griddata(lon,lat,newdata_var,xout=grid_lon,yout=grid_lat,$
;  /grid,/degrees,/sphere,triangles=triangles,$
;  /kriging,min_points=15,sectors=8,empty_sectors=3,missing=-9999)
grid_var=griddata(lon,lat,newdata_var,xout=grid_lon,yout=grid_lat,$
  /grid,/degrees,/sphere,triangles=triangles,$
  /nearest_neighbor,missing=-9999)  
r=where(plot_solar_zenith eq -9999,c)
if c gt 0 then grid_var[r]=-9999
;   Bytscal the data
phase_table=['light gray','blue','green','gold','red']
data_image=byte(grid_var)
r=where(grid_var lt 0,c)
if c gt 0 then data_image[r]=0
p1=image(data_image,grid_lon,grid_lat,$  ;  I have called this one p1
  limit=[llat,llon,ulat,rlon],$  ;map limit vector
  /current,$  ;Put it in the current open plot window p0
  grid_units='degrees',$
  center_longitude=((rlon-llon)/2.0)+llon,$
  /box_axes,$
  label_position=label_pos,$
  font_size=12,$  ;bigger font
  linestyle=2,$  ;grid will be dashed line
  rgb_table=phase_table,$  ;use color table 5
  map_projection=map_projection,$  ;map projection
  position=pos[pnum,*])
p2=mapcontinents(color='white',/hires,thick=2)  ;add the continent lines
p3=plot([bllon,brlon,brlon,bllon,bllon],[bllat,bllat,bulat,bulat,bllat],/overplot,/data,color='black',thick=3)
;  Put on a colorbar
c0=colorbar(range=[dmin_phase,dmax_phase],$  ;the colorbar is to match the image labeled p1
  orientation=1,position=reform(cbpos[pnum,*]),font_size=12,rgb_table=phase_table,$
  tickdir=1,border_on=1,title='Cloud Phase 1km')
t1=text(pos[pnum,0],pos[pnum,3]+dy,'0=miss 1=clear 2=liq 3=ice 4=undet',font_size=14) 
plot_cloud_phase=grid_var
endif
p0.save,out_dir+output_file_string+'.temp_re_nd_phase.png',height=pydim


end
