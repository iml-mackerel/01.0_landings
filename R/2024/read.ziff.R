read.ziff<- function (sp, path, year = NULL, language = "en") 
{
  owd<- getwd()

  setwd(rprojroot::find_rstudio_root_file())
    
    # load all raw data
    raw  <- list.files('data/ziff_meta' , pattern = 'csv', full.names = T)
    rawn <- sub('\\.csv$', '',list.files('data/ziff_meta' , pattern = 'csv'))
    
    rawcsv <- lapply(raw, read.csv)
    names(rawcsv) <- rawn
    list2env(rawcsv,envir=.GlobalEnv)
    
    # basic transformations
    ziff_meta_csv[] <- lapply(ziff_meta_csv, as.character)
    ziff_species <- ziff_species[,c(1:4)]
 
    
    ziff_gear <- read.csv("data/ziff_meta/ziff_gear.csv", encoding="latin1")
    ziff_species <- read.csv("data/ziff_meta/ziff_species.csv", encoding="latin1")[,-5]
    ziff_tonnage <- read.csv("data/ziff_meta/ziff_tonnage.csv")
    ziff_meta_csv <- read.csv("data/ziff_meta/ziff_meta_csv.csv", encoding="latin1")
    
    language <- match.arg(language, choices = c("en", "fr"))
    files <- list.files(pattern = "version_totale_", full.names = TRUE, 
                        ignore.case = TRUE, path = path)
    if (all(is.na(files))) 
        stop("No files with string \"Version_totale\" in directory.")
    if (!is.null(year)) {
        if (!is.numeric(year)) 
            stop("Argument \"year\" needs to be a numeric vector.")
        ys <- sapply(files, function(x) {
            y <- gsub(".*totale_(.+).txt", "\\1", x)
            y <- gsub("PR", "", y)
        })
       
    }
    ziff <- lapply(1:length(files), function(x) {
        print(files[x])
        z <- fread(file = files[x], sep = ";")
        z <- z[z$cod_esp %in% sp, ]
        return(z)
    })
    ziff <- rbindlist(ziff, fill = TRUE)
    ziff <- as.data.frame(ziff)
    #ziffbackup=ziff#dev purpose
    #ziff=ziffbackup
    
    ziff[ziff$opano %in% c("", "XXX"), "opano"] <- NA
    ziff[ziff$div %in% c("", "XXX"), "div"] <- NA
    ziff$opano <- toupper(ziff$opano)
    ziff$div <- toupper(ziff$div)
    ziff$date_cap <- ymd(ziff$date_cap)
    ziff$date_deb <- ymd(ziff$date_deb)
    ziff$annee <- with(ziff, ifelse(is.na(date_cap), year(date_deb), 
                                    year(date_cap)))
    if (!is.null(year)) 
        ziff <- ziff[ziff$annee %in% year, ]
    ziff$annee_gestion <- ifelse(month(ziff$date_deb) <= 4 & 
                                     day(ziff$date_deb) <= 15 & ziff$annee > 1999, ziff$annee - 
                                     1, ziff$annee)
    ziff$mois_cap <- month(ziff$date_cap)
    ziff$mois_deb <- month(ziff$date_deb)
    ziff$mois <- with(ziff, ifelse(is.na(mois_cap), mois_deb, 
                                   mois_cap))
    ziff$trim_cap <- quarter(ziff$date_cap)
    ziff$trim_deb <- quarter(ziff$date_deb)
    ziff$trim <- with(ziff, ifelse(is.na(trim_cap), trim_deb, 
                                   trim_cap))
    provs <- data.frame(fr = c("Inconnu", "N-É", "N-B", "IPE", 
                               "QC", "T-N"), en = c("Unknown", "NS", "NB", "PEI", "QC", 
                                                    "NL"))
    ziff$prov_att <- provs[, language][floor(ziff$port_att/10000) + 
                                           1]
    ziff$prov_att[is.na(ziff$prov_att)] <- provs[1, language]
    ziff$prov_deb <- provs[, language][floor(ziff$port_deb/10000) + 
                                           1]
    ziff$prov_deb[is.na(ziff$prov_deb)] <- provs[1, language]
    ziff[ziff$un_mes == "P" & !is.na(ziff$un_mes), "pd_deb"] <- ziff[ziff$un_mes == 
                                                                         "P" & !is.na(ziff$un_mes), "pd_deb"] * 0.453592
    levels(ziff$un_mes)[levels(ziff$un_mes) == "P"] <- "KfromP"
    ziff[ziff == 0] <- NA
    ziff[] <- lapply(ziff, function(x) if (is.factor(x)) 
        factor(x)
        else x)
    if (language == "en") 
        ziff_meta_csv[41:42, "fr"] <- c("Q_latitude", "Q_longitude")
        dfmatch<- left_join(ziff_meta_csv, data.frame(fr=colnames(ziff), val=1:ncol(ziff)))

        colnames(ziff)[dfmatch$val] <- dfmatch[,"en"] 
        colnames(ziff)[87:ncol(ziff)] <-  c( "year", "year.management", 
                            "catch.month", "land.month", "month", "catch.trim", 
                            "land.trim", "trim", "prov.home", "prov.land")
    temp <- ziff_species
    val <- ziff_meta_csv[ziff_meta_csv$fr == "cod_esp", language]
    names(temp) <- c(val, paste0(val, "_en"), paste0(val, "_fr"), 
                     paste0(val, "_lat"))
    ziff <- merge(ziff, temp, by = val, all.x = T)
    temp <- ziff_species
    val <- ziff_meta_csv[ziff_meta_csv$fr == "prespvis", language]
    names(temp) <- c(val, paste0(val, "_en"), paste0(val, "_fr"), 
                     paste0(val, "_lat"))
    ziff <- merge(ziff, temp, by = val, all.x = T)
    temp <- ziff_species
    val <- ziff_meta_csv[ziff_meta_csv$fr == "prespcap", language]
    names(temp) <- c(val, paste0(val, "_en"), paste0(val, "_fr"), 
                     paste0(val, "_lat"))
    ziff <- merge(ziff, temp, by = val, all.x = T)
    temp <- ziff_gear
    val <- ziff_meta_csv[ziff_meta_csv$fr == "engin", language]
    names(temp) <- c(val, paste0(val, "_fr"), paste0(val, "_en"), 
                     paste0(val, "_fixmob"), paste0(val, "_cat"))
    ziff <- merge(ziff, temp, by = val, all.x = T)
    temp <- ziff_tonnage
    val <- ziff_meta_csv[ziff_meta_csv$fr == "cl_ton", language]
    names(temp) <- c(val, paste0(val, "_desc"))
    ziff <- merge(ziff, temp, by = val, all.x = T)
    rm(temp)
    
    setwd(owd)
    return(ziff)
    
    
    
}
