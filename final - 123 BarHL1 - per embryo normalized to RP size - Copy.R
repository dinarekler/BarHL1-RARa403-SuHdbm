library("readxl")
library("gdata")


# specifying the path name
path <- "F:/from Public/chayak_lab_data/Dina/123 - dnRAR dnSuH BarHL1/123 - analysis.xlsx"

library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
        
        sheets <- readxl::excel_sheets(filename)
        x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X,
                                                            col_names = FALSE))
        if(!tibble) x <- lapply(x, as.data.frame)
        names(x) <- sheets
        x
}

BarHL1_data <- read_excel_allsheets("123 - analysis.xlsx")

######### Divide each dataframe by RP size #########

BarHL1_data[["1_123.1"]] <- BarHL1_data[["1_123.1"]]/50.32
BarHL1_data[["2_123.2"]] <- BarHL1_data[["2_123.2"]]/53.29
BarHL1_data[["3_123.3"]] <- BarHL1_data[["3_123.3"]]/40.57
BarHL1_data[["4_123.4"]] <- BarHL1_data[["4_123.4"]]/43.16
BarHL1_data[["1_123.5"]] <- BarHL1_data[["1_123.5"]]/41.70
BarHL1_data[["2_123.6"]] <- BarHL1_data[["2_123.6"]]/51.12
BarHL1_data[["3_123.7"]] <- BarHL1_data[["3_123.7"]]/40.87
BarHL1_data[["1_123.9"]] <- BarHL1_data[["1_123.9"]]/44.25
BarHL1_data[["2_123.10"]] <- BarHL1_data[["2_123.10"]]/53.91
BarHL1_data[["3_123.11"]] <- BarHL1_data[["3_123.11"]]/43.45
BarHL1_data[["4_123.12"]] <- BarHL1_data[["4_123.12"]]/41.19
BarHL1_data[["1_123.13"]] <- BarHL1_data[["1_123.13"]]/53.43
BarHL1_data[["2_123.14"]] <- BarHL1_data[["2_123.14"]]/37.60
BarHL1_data[["3_123.15"]] <- BarHL1_data[["3_123.15"]]/40.63
BarHL1_data[["4_123.16"]] <- BarHL1_data[["4_123.16"]]/44.77
BarHL1_data[["1_123.17"]] <- BarHL1_data[["1_123.17"]]/41.22
BarHL1_data[["2_123.18"]] <- BarHL1_data[["2_123.18"]]/40.33
BarHL1_data[["3_123.19"]] <- BarHL1_data[["3_123.19"]]/34.71
BarHL1_data[["4_123.20"]] <- BarHL1_data[["4_123.20"]]/41.92
BarHL1_data[["1_123.21"]] <- BarHL1_data[["1_123.21"]]/46.05
BarHL1_data[["2_123.22"]] <- BarHL1_data[["2_123.22"]]/49.35
BarHL1_data[["3_123.23"]] <- BarHL1_data[["3_123.23"]]/40.02
BarHL1_data[["4_123.24"]] <- BarHL1_data[["4_123.24"]]/46.71
BarHL1_data[["1_123.25"]] <- BarHL1_data[["1_123.25"]]/49.75
BarHL1_data[["2_123.26"]] <- BarHL1_data[["2_123.26"]]/45.43
BarHL1_data[["3_123.27"]] <- BarHL1_data[["3_123.27"]]/39.10
BarHL1_data[["1_123.29"]] <- BarHL1_data[["1_123.29"]]/34.61
BarHL1_data[["2_123.30"]] <- BarHL1_data[["2_123.30"]]/54.48
BarHL1_data[["3_123.31"]] <- BarHL1_data[["3_123.31"]]/39.58
BarHL1_data[["4_123.32"]] <- BarHL1_data[["4_123.32"]]/40.64
BarHL1_data[["1_123.33"]] <- BarHL1_data[["1_123.33"]]/43.91
BarHL1_data[["2_123.34"]] <- BarHL1_data[["2_123.34"]]/49.98
BarHL1_data[["3_123.35"]] <- BarHL1_data[["3_123.35"]]/49.98	
BarHL1_data[["4_123.36"]] <- BarHL1_data[["4_123.36"]]/46.69
BarHL1_data[["4_123.38"]] <- BarHL1_data[["4_123.38"]]/38.80
BarHL1_data[["2_123.39"]] <- BarHL1_data[["2_123.39"]]/34.75
BarHL1_data[["4_123.40"]] <- BarHL1_data[["4_123.40"]]/43.90

## Vectorize each embryo

c1_123.1 <- unlist(BarHL1_data[["1_123.1"]])
c2_123.2 <- unlist(BarHL1_data[["2_123.2"]])
c3_123.3 <- unlist(BarHL1_data[["3_123.3"]])
c4_123.4 <- unlist(BarHL1_data[["4_123.4"]])
c1_123.5 <- unlist(BarHL1_data[["1_123.5"]])
c2_123.6 <- unlist(BarHL1_data[["2_123.6"]])
c3_123.7 <- unlist(BarHL1_data[["3_123.7"]])
c1_123.9 <- unlist(BarHL1_data[["1_123.9"]])
c2_123.10 <- unlist(BarHL1_data[["2_123.10"]])
c3_123.11 <- unlist(BarHL1_data[["3_123.11"]])
c4_123.12 <- unlist(BarHL1_data[["4_123.12"]])
c1_123.13 <- unlist(BarHL1_data[["1_123.13"]])
c2_123.14 <- unlist(BarHL1_data[["2_123.14"]])
c3_123.15 <- unlist(BarHL1_data[["3_123.15"]])
c4_123.16 <- unlist(BarHL1_data[["4_123.16"]])
c1_123.17 <- unlist(BarHL1_data[["1_123.17"]])
c2_123.18 <- unlist(BarHL1_data[["2_123.18"]])
c3_123.19 <- unlist(BarHL1_data[["3_123.19"]])
c4_123.20 <- unlist(BarHL1_data[["4_123.20"]])
c1_123.21 <- unlist(BarHL1_data[["1_123.21"]])
c2_123.22 <- unlist(BarHL1_data[["2_123.22"]])
c3_123.23 <- unlist(BarHL1_data[["3_123.23"]])
c4_123.24 <- unlist(BarHL1_data[["4_123.24"]])
c1_123.25 <- unlist(BarHL1_data[["1_123.25"]])
c2_123.26 <- unlist(BarHL1_data[["2_123.26"]])
c3_123.27 <- unlist(BarHL1_data[["3_123.27"]])
c1_123.29 <- unlist(BarHL1_data[["1_123.29"]])
c2_123.30 <- unlist(BarHL1_data[["2_123.30"]])
c3_123.31 <- unlist(BarHL1_data[["3_123.31"]])
c4_123.32 <- unlist(BarHL1_data[["4_123.32"]])
c1_123.33 <- unlist(BarHL1_data[["1_123.33"]])
c2_123.34 <- unlist(BarHL1_data[["2_123.34"]])
c3_123.35 <- unlist(BarHL1_data[["3_123.35"]])	
c4_123.36 <- unlist(BarHL1_data[["4_123.36"]])
c4_123.38 <- unlist(BarHL1_data[["4_123.38"]])
c2_123.39 <- unlist(BarHL1_data[["2_123.39"]])
c4_123.40 <- unlist(BarHL1_data[["4_123.40"]])


# Create list of vectors per group

control_list <- list(c1_123.1,c1_123.5,c1_123.9,c1_123.13,c1_123.17,c1_123.21,
                     c1_123.25,c1_123.29,c1_123.33)
dnRAR_list <- list(c2_123.2,c2_123.6,c2_123.10,c2_123.14,c2_123.18,c2_123.22,
                   c2_123.26,c2_123.30,c2_123.34,c2_123.39)
control_SuH_list <- list(c3_123.7,c3_123.11,c3_123.15,c3_123.19,
                         c3_123.23,c3_123.27,c3_123.31,c3_123.35)
dnRAR_SuH_list <- list(c4_123.4,c4_123.12,c4_123.16,c4_123.20,c4_123.24,
                       c4_123.32,c4_123.36,c4_123.38,c4_123.40)

# Remove NAs in the lists

for (i in c(1:9)) {
        control_list[[i]] <- na.omit(control_list[[i]])
}
for (i in c(1:10)) {
        dnRAR_list[[i]] <- na.omit(dnRAR_list[[i]])
}
for (i in c(1:8)) {
        control_SuH_list[[i]] <- na.omit(control_SuH_list[[i]])
}
for (i in c(1:9)) {
        dnRAR_SuH_list[[i]] <- na.omit(dnRAR_SuH_list[[i]])
}

# Sort each embryo

for (i in c(1:9)) {
        control_list[[i]] <- sort(control_list[[i]])
}
for (i in c(1:10)) {
        dnRAR_list[[i]] <- sort(dnRAR_list[[i]])
}
for (i in c(1:8)) {
        control_SuH_list[[i]] <- sort(control_SuH_list[[i]])
}
for (i in c(1:9)) {
        dnRAR_SuH_list[[i]] <- sort(dnRAR_SuH_list[[i]])
}










tr_blue <- rgb(100,0,180, max = 255, alpha = 120)

hist(control_list[[2]], breaks = seq(0,20, by = 0.1), ylim = c(1,200), xlim = c(0,4),
     col = "mediumspringgreen", xlab = "distance from dorsal midline (um)",
     ylab = "BarHL1+ cells", main = "Spatial distribution of BarHL1+ cells",
     cex.axis = 1.2, cex.lab = 1.6)
par(new = TRUE)
hist(dnRAR, breaks = seq(0,5, by = 0.1), ylim = c(1,1000), xlim = c(0,4),
     col = tr_blue, xlab = "", ylab = "", main = "",
     cex.axis = 1.2)

par(new = FALSE)

