library(data.table)

fread.and.modify <- function(file.number, driver) {
  tmp <- fread(paste0("drivers/",driver,"/",file.number,".csv"), header=T, sep=",")
  tmp[, drive:=file.number]
  return(tmp)
}

drivers <- list.files("./drivers/")
for (i in 1:length(drivers)) {
  driver <- drivers[i]
  drives <- rbindlist(lapply(1:200, fread.and.modify, driver))
  write.table(drives, paste0(driver, ".csv"), quote=F, sep=",", row.names=F)
}
