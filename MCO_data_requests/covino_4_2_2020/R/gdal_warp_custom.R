gdal_warp = function(file_in, file_out, proj_in, proj_out, res){
  warp_cmd = paste0("gdalwarp -tr ", res, " ", res, " -s_srs ", proj_in, 
                    " -t_srs ", proj_out, " -r bilinear -of vrt -overwrite ", file_in, " ", file_in, ".vrt")
  
  translate_cmd = paste0("gdal_translate -co compress=LZW ", file_in, ".vrt ", file_out)
  
  system(warp_cmd)
  system(translate_cmd)
  invisible(file.remove(paste0(file_in, ".vrt")))
}
  
#gdal_warp(file_in = "/home/zhoylman/surface_area_bias/data/raster/percent_error_4k_global_mean_reducer.tif",
#          file_out = "/home/zhoylman/surface_area_bias/data/raster/percent_error_0p5deg_global_mean_reducer.tif",
#          proj_in = "EPSG:4326", proj_out = "EPSG:3857", res = 4000)

# gdal_warp(file_in = "/home/zhoylman/surface_area_bias/data/raster/percent_error_4k_global_mean_reducer.tif",
#          file_out = "/home/zhoylman/surface_area_bias/data/raster/percent_error_0p5deg_global_mean_reducer.tif",
#          proj_in = "EPSG:4326", proj_out = "EPSG:4326", res = 0.5)
