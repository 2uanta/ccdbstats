# f = "username","family_name","given_name","ccri","rapi"
f = "cq_local_account.csv"
# f = "cq_users.csv"
users = read.csv(f, sep=",",header=T)
names(users) = sub("ccri", "u_ccri", names(users))
names(users) = sub("rapi", "u_rapi", names(users))
f ="cq_rap_users.csv"
rap_users = read.csv(f, sep=",",header=T)
names(rap_users) = sub("ccri", "r_ccri", names(rap_users))
names(rap_users) = sub("rapi", "r_rapi", names(rap_users))
rap_users = rap_users[rap_users$status == "activated",]

storage_resources = list("sb"="ddn_sb", "lb"="ddn_lb", "gs"="gss_gs")
# set first digit to 1 to run on Thuresday, 2 for Friday, etc...
lag = 1+7*0
# fs="gs"
for (fs in c("sb", "lb", "gs")) {
  time = as.numeric(as.POSIXct(paste(Sys.Date()-lag, "17:00:00")))
  day = format(Sys.Date()-lag, "%y%m%d")
  SA = paste0("sampled_at:", time)
  CO = "consortium:cq"
  stor_res = paste0("cq_mcgill_", storage_resources[[fs]])
  fn = paste0("storage-stats/mmrep_quota_",fs,".",
  format(Sys.Date()-lag,"%y%m%d"), "1700.log")
  usage_file = paste0("storage_usage/", stor_res, "-", day, ".ccsu")
  print(usage_file)
  SR = paste0("storage_resource:", stor_res)
  # capacity line
  exec = system(paste0("df -B ",  1024*1024*1024, " /", fs ) , intern = TRUE) 
  exec = strsplit(exec[length(exec)], "[ ]+")[[1]]
  capacity = as.numeric(exec[2])
  used = as.numeric(exec[3])
  CA = paste0("capacity_in_gib:", capacity)
  US = paste0("used_in_gib:", used)
  line = paste(CO, SR, CA, US, SA, sep="|")
  # print(line)
  write(line, file=usage_file)

  # read the whole file
  data= do.call(rbind,
    lapply(Sys.glob(fn), function(f) {
      # fs = gsub("storage-stats/mmrep_quota_(sb|lb|gs){1}\\.([0-9]{10})\\.log", "\\1", f)
      datetime = gsub("storage-stats/mmrep_quota_(sb|lb|gs){1}\\.([0-9]{10})\\.log", "\\2", f)
      a = read.csv(f, skip=2, sep="", stringsAsFactors=FALSE, header=F, nrows=-1)
      # keep 1st 4 columns
      a = a[,c(1:5)]
      a = a[complete.cases(a),]
      # add fs and datetime
      a$fs = fs
      a$datetime = as.POSIXct(datetime, format="%y%m%d%H%M")
      a
    })
  )
  names(data) = c('Name', 'fileset', 'type', 'GB', 'quota', 'fs', 'datetime')
  # names(data); dim(data)
  # Check if fileset is really a factor
  # levels(as.factor(data$fileset))
  # Check if type is really a factor
  #	levels(as.factor(data$type))
  # eliminate rows with zero GB used
  data = data[data$GB != 0,]
  data$CO = CO  
  data$SR = SR  
  data$ST = paste0("storage_in_gib:", data$GB)  
  data$QU = paste0("quota_in_gib:", data$quota)  
  data$SA = SA  

  data_home = data[grepl("(sb|lb|gs)_home_.+", data$Name),]
  data_project = data[grepl("(sb|lb|gs)_project_.+", data$Name),]
  data_others = data[!(grepl("(sb|lb|gs)_home_.+", data$Name) | grepl("(sb|lb|gs)_project_.+", data$Name)),]

  print(dim(data_home))
  print(dim(data_project))
  print(dim(data_others))

  if (nrow(data_home) > 0) {
    ###
    ### Process for home records
    ###

    data_home$fstype = gsub("(sb|lb|gs)?_(.+?)_(.+)", "\\2", data_home$Name)
    # levels(as.factor(data$fstype))
    data_home$username = ifelse(data_home$fstype == "home",  gsub("(sb|lb|gs)?_(home)_(.+)", "\\3", data_home$Name), NA)
    data_home$rapi = ifelse(data_home$fstype == "project",  gsub("(sb|lb|gs)?_(project)_(.+)", "\\3", data_home$Name), NA)
    # map username -> ccri
    library(plyr)
    data_home = join(data_home, users, by="username", type="left", match="first")
    data_home$ccri = ifelse(is.na(data_home$u_ccri), NA, as.character(data_home$u_ccri))
    data_home$rapi = ifelse(data_home$fstype == "home", as.character(data_home$u_rapi), data_home$rapi)

    data_home$USR = paste0("username:", data_home$username)  
    data_home$CC = paste0("ccri:", data_home$ccri)
    data_home$RA = paste0("rapi:", data_home$rapi)
    # eliminate rows with no rapi and no ccri
    data_home = data_home[!(grepl("rapi:NA", data_home$RA) & grepl("ccri:NA", data_home$CC)),]
    # combined rapi and ccri columns
    data_home$RACC = ifelse(data_home$CC == "ccri:NA", data_home$RA, paste(data_home$RA, data_home$CC, sep='|'))
    # create output table
    out = data.frame(data_home$CO, data_home$SR, data_home$RACC, data_home$USR, data_home$ST, data_home$QU, data_home$SA)
    if (nrow(out) > 0) write.table(out, file=usage_file, append=TRUE, col.names = F, row.names = F, sep='|', quote = F)  
    print(dim(out))
  }

  if (nrow(data_project) > 0) {
    ###
    ### Process for project records
    ###

    ## Expand Name
    part2 = ifelse(grepl("(sb|lb|gs)+_(.+)", data_project$Name), gsub("(sb|lb|gs)_(.+)", "\\2", data_project$Name),  gsub("(sb|lb|gs)_(.+)", "\\1", data_project$Name))
    data_project$id = ifelse(grepl("(.+?)_(.+)", part2), gsub("(.+?)_(.+)", "\\2", part2),  gsub("(.+?)_(.+)", "\\1", part2))
    data_project$fstype = ifelse(grepl("(.+?)_(.+)", part2), gsub("(.+?)_(.+)", "\\1", part2),  "other")

    # create rap column for project records
    data_project$rap = gsub("([a-z]{3}-[0-9]{3})(-\\S{2})?", "\\1", data_project$id)
    # map rap -> ccri
    data_project = join(data_project, rap_users, by="rap", type="left", match="first")
    data_project$rapi = ifelse(data_project$rap == data_project$id, as.character(data_project$r_rapi), as.character(data_project$id))
    # create rapi column
    data_project$rapi = ifelse(data_project$fstype == "scratch", "scratch", data_project$rapi)
    data_project$rapi = ifelse(grepl("atlas", data_project$id), "muz-612-aa" , data_project$rapi)
    data_project$rapi = ifelse(grepl("mugqic", data_project$id), "bws-221-aa" , data_project$rapi)
    data_project$rapi = ifelse(grepl("deap", data_project$id), "vyq-512-aa" , data_project$rapi)
    data_project$rapi = ifelse(grepl("genap", data_project$id), "bws-221-af" , data_project$rapi)
    data_project$rapi = ifelse(grepl("belle", data_project$id), "jfd-420-ab" , data_project$rapi)
    data_project = aggregate(cbind(GB, quota) ~ fs+fstype+datetime+rapi, data=data_project, sum)

    data_project$RA = paste0("rapi:", data_project$rapi)
    # eliminate rows with no rapi
    data_project = data_project[data_project$RA != "rapi:",]
    #
    data_project$CO = CO  
    data_project$SR = SR  
    data_project$ST = paste0("storage_in_gib:", data_project$GB)  
    data_project$QU = paste0("quota_in_gib:", data_project$quota)  
    data_project$SA = SA  
    #
    out = data.frame(data_project$CO, data_project$SR, data_project$RA, data_project$ST, data_project$QU, data_project$SA)
    if (nrow(out) > 0) write.table(out, file=usage_file, append=TRUE, col.names = F, row.names = F, sep='|', quote = F)  
    print(dim(out))
  }

  if (nrow(data_others) > 0) {
    ###
    ### Process for other records
    ###

    ## Expand Name
    part2 = ifelse(grepl("(sb|lb|gs)+_(.+)", data_others$Name), gsub("(sb|lb|gs)_(.+)", "\\2", data_others$Name),  gsub("(sb|lb|gs)_(.+)", "\\1", data_others$Name))
    data_others$id = ifelse(grepl("(.+?)_(.+)", part2), gsub("(.+?)_(.+)", "\\2", part2),  gsub("(.+?)_(.+)", "\\1", part2))
    data_others$fstype = ifelse(grepl("(.+?)_(.+)", part2), gsub("(.+?)_(.+)", "\\1", part2),  "other")
    # create rapi column
    data_others$rapi = ifelse(data_others$fstype == "scratch", "scratch", data_others$id)
    data_others$rapi = ifelse(grepl("atlas", data_others$id), "muz-612-aa" , data_others$rapi)
    data_others$rapi = ifelse(grepl("mugqic", data_others$id), "bws-221-aa" , data_others$rapi)
    data_others$rapi = ifelse(grepl("deap", data_others$id), "vyq-512-aa" , data_others$rapi)
    data_others$rapi = ifelse(grepl("genap", data_others$id), "bws-221-af" , data_others$rapi)
    data_others$rapi = ifelse(grepl("belle", data_others$id), "jfd-420-ab" , data_others$rapi)
    #data_others = join(data_others, rap_users, by="rap", type="left", match="first")
    #data_others$rapi = ifelse(is.na(data_others$rap), data_others$rapi, data_others$r_rapi)
    data_others = aggregate(cbind(GB, quota) ~ fs+fstype+datetime+rapi, data=data_others, sum)

    data_others$RA = paste0("rapi:", data_others$rapi)

    data_others$CO = CO  
    data_others$SR = SR  
    data_others$ST = paste0("storage_in_gib:", data_others$GB)  
    data_others$QU = paste0("quota_in_gib:", data_others$quota)  
    data_others$SA = SA  

    # create output table
    out = data.frame(data_others$CO, data_others$SR, data_others$RA, data_others$ST, data_others$QU, data_others$SA)
    if (nrow(out) > 0) write.table(out, file=usage_file, append=TRUE, col.names = F, row.names = F, sep='|', quote = F)  
    print(dim(out))
  }
}
