##PH List of Barangays and Location

### Load packages
pacman::p_load(tidyverse,
               dplyr,
               rvest,
               magrittr,
               stringr,
               xml2,
               urltools,
               tibble)

## Get PH provinces URL
url2 <- read_html("https://www.philatlas.com/barangays.html")
prov_name <- url2 %>%
  html_nodes("#brgysByProv li") %>%
  html_text()

prov_links <- url2 %>%
  html_nodes("#brgysByProv a") %>%
  html_attr("href")

prov_links2 <- paste0("https://www.philatlas.com/", prov_links)

prov_temp <- cbind(prov_name, prov_links2)
prov_temp %>%
  matrix(ncol=2, byrow = FALSE) %>%
  data.frame -> prov_df

prov_df <- prov_df %>%
  filter(row_number()<= n()-2) ## to remove link to NCR and HUC barangays

colnames(prov_df) <- c("province", "url")

#write.csv(prov_df, "PhilAtlas PH Provinces URLs.csv", row.names=FALSE)


## Get PH Barangays URL per Province
n <- 1

prov_brgys <- data.frame(matrix(ncol = 3))
colnames(prov_brgys) <- c("prov_df.province.n.", "X1", 
                          "X2")

while(n<=nrow(prov_df)) {
  prov_url <- read_html(prov_df$url[n])
  
  brgy_name <- prov_url %>%
    html_nodes("#brgyList li") %>%
    html_text()
  
  brgy_link <- prov_url %>%
    html_nodes("#brgyList a") %>%
    html_attr("href")
  
  brgy_link2 <- paste0("https://www.philatlas.com/", brgy_link)
  
  brgy_temp <- cbind(brgy_name, brgy_link2)
  brgy_temp %>%
    matrix(ncol=2, byrow=FALSE) %>%
    data.frame -> brgy_df
  
  prov_brgys <- rbind(prov_brgys,
                      data.frame(prov_df$province[n],
                                 brgy_df))
  
  n <- n+1
}

prov_brgys <- prov_brgys[-1,]

colnames(prov_brgys) <- c("province", "barangay", "url")

ph_prov_brgys <- prov_brgys %>%
  separate(barangay, into = c('barangay', 'lgu_add1', 'lgu_add2', 'lgu_add3'), sep = ",") %>%
  mutate_if(is.character, str_trim) 
#needed to separate column of lgu since some barangays have town or purok
#in their name

#write.csv(ph_prov_brgys, "PhilAtlas PH Provincial Barangays List URL.csv", row.names=FALSE)

##Scrape by provincial alphabetical order
# Save dataframe per letter
for (i in seq_along(LETTERS)) {
  a_prov_df <- ph_prov_brgys %>%
    filter(grepl(paste0("^", LETTERS[i]), province))
  assign(paste0(LETTERS[i], "_prov_", "df"), a_prov_df)
}

rm(a_prov_df)

# Start with A and so on
# Provinces starting with A
n <- 1

prov_A_brgy <- data.frame(matrix(ncol = 8))
colnames(prov_A_brgy) <- c("A_prov_df.province.n.",
                           "A_prov_df.barangay.n.",
                           "A_prov_df.lgu_add1.n.",
                           "A_prov_df.lgu_add2.n.",
                           "A_prov_df.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(A_prov_df)) {
  brgy_url_A <- read_html(A_prov_df$url[n])
  
  brgy_lat_A <- brgy_url_A %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_A <- brgy_url_A %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_A <- brgy_url_A %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_A <- cbind(brgy_lat_A, brgy_long_A, brgy_reg_A)
  brgy_loc_A %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfA
  
  prov_A_brgy <- rbind(prov_A_brgy,
                       data.frame(A_prov_df$province[n],
                                  A_prov_df$barangay[n],
                                  A_prov_df$lgu_add1[n],
                                  A_prov_df$lgu_add2[n],
                                  A_prov_df$lgu_add3[n],
                                  brgyloc_dfA))
  
  prov_A_brgy <- prov_A_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prov_A_brgy <- prov_A_brgy[-1,]

colnames(prov_A_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

write.csv(prov_A_brgy, "A Provinces Lat Long.csv", row.names=FALSE)

# Provinces starting with B
n <- 1

prov_B_brgy <- data.frame(matrix(ncol = 8))
colnames(prov_B_brgy) <- c("B_prov_df.province.n.",
                           "B_prov_df.barangay.n.",
                           "B_prov_df.lgu_add1.n.",
                           "B_prov_df.lgu_add2.n.",
                           "B_prov_df.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(B_prov_df)) {
  
  url_B <- B_prov_df$url[n]
  # to prevent VPN
  download.file(url_B, destfile = "prov_B.html", quiet=TRUE) 
  brgy_B <- read_html("prov_B.html")
  
  brgy_lat_B <- brgy_B %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_B <- brgy_B %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_B <- brgy_B %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_B <- cbind(brgy_lat_B, brgy_long_B, brgy_reg_B)
  brgy_loc_B %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfB
  
  prov_B_brgy <- rbind(prov_B_brgy,
                       data.frame(B_prov_df$province[n],
                                  B_prov_df$barangay[n],
                                  B_prov_df$lgu_add1[n],
                                  B_prov_df$lgu_add2[n],
                                  B_prov_df$lgu_add3[n],
                                  brgyloc_dfB))
  
  prov_B_brgy <- prov_B_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prov_B_brgy <- prov_B_brgy[-1,]

colnames(prov_B_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

write.csv(prov_B_brgy, "B Provinces Lat Long.csv", row.names=FALSE)

# Provinces starting with C
#n <- 1

#prov_C_brgy <- data.frame(matrix(ncol = 8))
#colnames(prov_C_brgy) <- c("C_prov_df.province.n.",
#                           "C_prov_df.barangay.n.",
#                           "C_prov_df.lgu_add1.n.",
#                           "C_prov_df.lgu_add2.n.",
#                           "C_prov_df.lgu_add3.n.",
#                           "X1", "X2", "X3")

#while(n<=nrow(C_prov_df)) {
  
#  url_C <- C_prov_df$url[n]
  # to prevent VPN
#  download.file(url_C, destfile = "prov_C.html", quiet=TRUE) 
#  brgy_C <- read_html("prov_C.html")
#  
#  brgy_lat_C <- brgy_C %>%
#    html_nodes("#latitude") %>%
#    html_text()
  
#  brgy_long_C <- brgy_C %>%
#    html_nodes("#longitude") %>%
#    html_text()
  
#  brgy_reg_C <- brgy_C %>%
#    html_nodes("tr:nth-child(3) .iboxVal") %>%
#    html_text()
  
#  brgy_loc_C <- cbind(brgy_lat_C, brgy_long_C, brgy_reg_C)
#  brgy_loc_C %>%
#    matrix(ncol=3, byrow=FALSE) %>%
#    data.frame -> brgyloc_dfC
  
#  prov_C_brgy <- rbind(prov_C_brgy,
#                       data.frame(C_prov_df$province[n],
#                                  C_prov_df$barangay[n],
#                                  C_prov_df$lgu_add1[n],
#                                  C_prov_df$lgu_add2[n],
#                                  C_prov_df$lgu_add3[n],
#                                  brgyloc_dfC))
  
#  prov_C_brgy <- prov_C_brgy %>%
#    mutate_if(is.character, str_trim)
  
#  n <- n+1
#}

#prov_C_brgy <- prov_C_brgy[-1,]

#colnames(prov_C_brgy) <- c("province", "barangay", "lgu_add1",
#                           "lgu_add2", "lgu_add3", "latitude", 
#                           "longitude", "region")

#prioritize Cavite, Camarines and Catanduanes
prio_C <- C_prov_df[C_prov_df$province %in% c('Cavite', 
                                              'Camarines Sur', 
                                              'Camarines Norte', 
                                              'Catanduanes'), ]
n <- 1
prio_C_brgy <- data.frame(matrix(ncol = 8))
colnames(prio_C_brgy) <- c("prio_C.province.n.",
                           "prio_C.barangay.n.",
                           "prio_C.lgu_add1.n.",
                           "prio_C.lgu_add2.n.",
                           "prio_C.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(prio_C)) {
  
  url_C_prio <- prio_C$url[n]
  # to prevent VPN
  download.file(url_C_prio, destfile = "prov_C_prio.html", quiet=TRUE) 
  brgy_C_prio <- read_html("prov_C_prio.html")
  
  brgy_lat_C_prio <- brgy_C_prio %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_C_prio <- brgy_C_prio %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_C_prio <- brgy_C_prio %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_C_prio <- cbind(brgy_lat_C_prio, 
                           brgy_long_C_prio, 
                           brgy_reg_C_prio)
  brgy_loc_C_prio %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfC_prio
  
  prio_C_brgy <- rbind(prio_C_brgy,
                       data.frame(prio_C$province[n],
                                  prio_C$barangay[n],
                                  prio_C$lgu_add1[n],
                                  prio_C$lgu_add2[n],
                                  prio_C$lgu_add3[n],
                                  brgyloc_dfC_prio))
  
  prio_C_brgy <- prio_C_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prio_C_brgy <- prio_C_brgy[-1,]

colnames(prio_C_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")


#Alternative code for C provinces (divided into 2 parts)
# Part 1
prov_C1_brgy <- data.frame()

for(i in 1:2725) { 
  url_C1 <- C_prov_df$url[i]
  download.file(url_C1, destfile = "prov_C1.html", quiet=TRUE) 
  brgy_C1 <- read_html("prov_C1.html")
  
  brgy_lat_C1 <- brgy_C1 %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_C1 <- brgy_C1 %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_C1 <- brgy_C1 %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_C1 <- cbind(brgy_lat_C1, 
                       brgy_long_C1, 
                       brgy_reg_C1)
  
  brgyloc_dfC1 <- brgy_loc_C1 %>%
    as.data.frame() %>%
    set_names(c("latitude", "longitude", "region"))
  
  prov_C1_brgy <- rbind(prov_C1_brgy,
                       data.frame(C_prov_df$province[i],
                                  C_prov_df$barangay[i],
                                  C_prov_df$lgu_add1[i],
                                  C_prov_df$lgu_add2[i],
                                  C_prov_df$lgu_add3[i],
                                  brgyloc_dfC1))
  
  prov_C1_brgy <- prov_C1_brgy %>%
    mutate_if(is.character, str_trim)
}

colnames(prov_C1_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

write.csv(prov_C1_brgy, "Part 1 - C Provinces Lat Long.csv", 
          row.names=FALSE)

#Part 2
prov_C2_brgy <- data.frame()

for(i in 2726:5449) { 
  url_C2 <- C_prov_df$url[i]
  download.file(url_C2, destfile = "prov_C2.html", quiet=TRUE) 
  brgy_C2 <- read_html("prov_C2.html")
  
  brgy_lat_C2 <- brgy_C2 %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_C2 <- brgy_C2 %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_C2 <- brgy_C2 %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_C2 <- cbind(brgy_lat_C2, 
                       brgy_long_C2, 
                       brgy_reg_C2)
  
  brgyloc_dfC2 <- brgy_loc_C2 %>%
    as.data.frame() %>%
    set_names(c("latitude", "longitude", "region"))
  
  prov_C2_brgy <- rbind(prov_C2_brgy,
                        data.frame(C_prov_df$province[i],
                                   C_prov_df$barangay[i],
                                   C_prov_df$lgu_add1[i],
                                   C_prov_df$lgu_add2[i],
                                   C_prov_df$lgu_add3[i],
                                   brgyloc_dfC2))
  
  prov_C2_brgy <- prov_C2_brgy %>%
    mutate_if(is.character, str_trim)
}

colnames(prov_C2_brgy) <- c("province", "barangay", "lgu_add1",
                            "lgu_add2", "lgu_add3", "latitude", 
                            "longitude", "region")

write.csv(prov_C2_brgy, "Part 2 - C Provinces Lat Long.csv",
          row.names=FALSE)

# Provinces starting with D
n <- 1

prov_D_brgy <- data.frame(matrix(ncol = 8))
colnames(prov_D_brgy) <- c("D_prov_df.province.n.",
                           "D_prov_df.barangay.n.",
                           "D_prov_df.lgu_add1.n.",
                           "D_prov_df.lgu_add2.n.",
                           "D_prov_df.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(D_prov_df)) {
  
  url_D <- D_prov_df$url[n]
  # to prevent VPN
  download.file(url_D, destfile = "prov_D.html", quiet=TRUE) 
  brgy_D <- read_html("prov_D.html")
  
  brgy_lat_D <- brgy_D %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_D <- brgy_D %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_D <- brgy_D %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_D <- cbind(brgy_lat_D, brgy_long_D, brgy_reg_D)
  brgy_loc_D %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfD
  
  prov_D_brgy <- rbind(prov_D_brgy,
                       data.frame(D_prov_df$province[n],
                                  D_prov_df$barangay[n],
                                  D_prov_df$lgu_add1[n],
                                  D_prov_df$lgu_add2[n],
                                  D_prov_df$lgu_add3[n],
                                  brgyloc_dfD))
  
  prov_D_brgy <- prov_D_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prov_D_brgy <- prov_D_brgy[-1,]

colnames(prov_D_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

write.csv(prov_D_brgy, "D Provinces Lat Long.csv", row.names=FALSE)

# Provinces starting with E
n <- 1

prov_E_brgy <- data.frame(matrix(ncol = 8))
colnames(prov_E_brgy) <- c("E_prov_df.province.n.",
                           "E_prov_df.barangay.n.",
                           "E_prov_df.lgu_add1.n.",
                           "E_prov_df.lgu_add2.n.",
                           "E_prov_df.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(E_prov_df)) {
  
  url_E <- E_prov_df$url[n]
  # to prevent VPN
  download.file(url_E, destfile = "prov_E.html", quiet=TRUE) 
  brgy_E <- read_html("prov_E.html")
  
  brgy_lat_E <- brgy_E %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_E <- brgy_E %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_E <- brgy_E %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_E <- cbind(brgy_lat_E, brgy_long_E, brgy_reg_E)
  brgy_loc_E %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfE
  
  prov_E_brgy <- rbind(prov_E_brgy,
                       data.frame(E_prov_df$province[n],
                                  E_prov_df$barangay[n],
                                  E_prov_df$lgu_add1[n],
                                  E_prov_df$lgu_add2[n],
                                  E_prov_df$lgu_add3[n],
                                  brgyloc_dfE))
  
  prov_E_brgy <- prov_E_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prov_E_brgy <- prov_E_brgy[-1,]

colnames(prov_E_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

write.csv(prov_E_brgy, "E Provinces Lat Long.csv", row.names=FALSE)

# Provinces starting with G
n <- 1

prov_G_brgy <- data.frame(matrix(ncol = 8))
colnames(prov_G_brgy) <- c("G_prov_df.province.n.",
                           "G_prov_df.barangay.n.",
                           "G_prov_df.lgu_add1.n.",
                           "G_prov_df.lgu_add2.n.",
                           "G_prov_df.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(G_prov_df)) {
  
  url_G <- G_prov_df$url[n]
  # to prevent VPN
  download.file(url_G, destfile = "prov_G.html", quiet=TRUE) 
  brgy_G <- read_html("prov_G.html")
  
  brgy_lat_G <- brgy_G %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_G <- brgy_G %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_G <- brgy_G %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_G <- cbind(brgy_lat_G, brgy_long_G, brgy_reg_G)
  brgy_loc_G %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfG
  
  prov_G_brgy <- rbind(prov_G_brgy,
                       data.frame(G_prov_df$province[n],
                                  G_prov_df$barangay[n],
                                  G_prov_df$lgu_add1[n],
                                  G_prov_df$lgu_add2[n],
                                  G_prov_df$lgu_add3[n],
                                  brgyloc_dfG))
  
  prov_G_brgy <- prov_G_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prov_G_brgy <- prov_G_brgy[-1,]

colnames(prov_G_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

write.csv(prov_G_brgy, "G Provinces Lat Long.csv", row.names=FALSE)

# Provinces starting with I
#n <- 1

#prov_I_brgy <- data.frame(matrix(ncol = 8))
#colnames(prov_I_brgy) <- c("I_prov_df.province.n.",
#                           "I_prov_df.barangay.n.",
#                           "I_prov_df.lgu_add1.n.",
#                           "I_prov_df.lgu_add2.n.",
#                           "I_prov_df.lgu_add3.n.",
#                           "X1", "X2", "X3")

#while(n<=nrow(I_prov_df)) {
#  url_I <- I_prov_df$url[n]
  # to prevent VPN
#  download.file(url_I, destfile = "prov_I.html", quiet=TRUE) 
#  brgy_I <- read_html("prov_I.html")
  
#  brgy_lat_I <- brgy_I %>%
#    html_nodes("#latitude") %>%
#    html_text()
  
#  brgy_long_I <- brgy_I %>%
#    html_nodes("#longitude") %>%
#    html_text()
  
#  brgy_reg_I <- brgy_I %>%
#    html_nodes("tr:nth-child(3) .iboxVal") %>%
#    html_text()
  
#  brgy_loc_I <- cbind(brgy_lat_I, brgy_long_I, brgy_reg_I)
#  brgy_loc_I %>%
#    matrix(ncol=3, byrow=FALSE) %>%
#    data.frame -> brgyloc_dfI
  
#  prov_I_brgy <- rbind(prov_I_brgy,
#                       data.frame(I_prov_df$province[n],
#                                  I_prov_df$barangay[n],
#                                  I_prov_df$lgu_add1[n],
#                                  I_prov_df$lgu_add2[n],
#                                  I_prov_df$lgu_add3[n],
#                                  brgyloc_dfI))
  
#  prov_I_brgy <- prov_I_brgy %>%
#    mutate_if(is.character, str_trim)
  
#  n <- n+1
#}

#prov_I_brgy <- prov_I_brgy[-1,]

#colnames(prov_I_brgy) <- c("province", "barangay", "lgu_add1",
#                           "lgu_add2", "lgu_add3", "latitude", 
#                           "longitude", "region")

# Alternative code for I (divided into 2 parts)
# Part 1
prov_I1_brgy <- data.frame()

for(i in 1:2140) { 
  url_I1 <- I_prov_df$url[i]
  download.file(url_I1, destfile = "prov_I1.html", quiet=TRUE) 
  brgy_I1 <- read_html("prov_I1.html")
  
  brgy_lat_I1 <- brgy_I1 %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_I1 <- brgy_I1 %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_I1 <- brgy_I1 %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_I1 <- cbind(brgy_lat_I1, 
                       brgy_long_I1, 
                       brgy_reg_I1)
  
  brgyloc_dfI1 <- brgy_loc_I1 %>%
    matrix(ncol=3, byrow=TRUE) %>%
    as.data.frame() %>%
    set_names(c("latitude", "longitude", "region"))
  
  prov_I1_brgy <- rbind(prov_I1_brgy,
                        data.frame(I_prov_df$province[i],
                                   I_prov_df$barangay[i],
                                   I_prov_df$lgu_add1[i],
                                   I_prov_df$lgu_add2[i],
                                   I_prov_df$lgu_add3[i],
                                   brgyloc_dfI1))
  
  prov_I1_brgy <- prov_I1_brgy %>%
    mutate_if(is.character, str_trim)
}

colnames(prov_I1_brgy) <- c("province", "barangay", "lgu_add1",
                            "lgu_add2", "lgu_add3", "latitude", 
                            "longitude", "region")

write.csv(prov_I1_brgy, "Part 1 - I Provinces Lat Long.csv",
          row.names = FALSE)

# Part 2
prov_I2_brgy <- data.frame()

for(i in 2141:4279) { 
  url_I2 <- I_prov_df$url[i]
  download.file(url_I2, destfile = "prov_I2.html", quiet=TRUE) 
  brgy_I2 <- read_html("prov_I2.html")
  
  brgy_lat_I2 <- brgy_I2 %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_I2 <- brgy_I2 %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_I2 <- brgy_I2 %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_I2 <- cbind(brgy_lat_I2, 
                       brgy_long_I2, 
                       brgy_reg_I2)
  
  brgyloc_dfI2 <- brgy_loc_I2 %>%
    matrix(ncol=3, byrow=TRUE) %>%
    as.data.frame() %>%
    set_names(c("latitude", "longitude", "region"))
  
  prov_I2_brgy <- rbind(prov_I2_brgy,
                        data.frame(I_prov_df$province[i],
                                   I_prov_df$barangay[i],
                                   I_prov_df$lgu_add1[i],
                                   I_prov_df$lgu_add2[i],
                                   I_prov_df$lgu_add3[i],
                                   brgyloc_dfI2))
  
  prov_I2_brgy <- prov_I2_brgy %>%
    mutate_if(is.character, str_trim)
}

colnames(prov_I2_brgy) <- c("province", "barangay", "lgu_add1",
                            "lgu_add2", "lgu_add3", "latitude", 
                            "longitude", "region")

write.csv(prov_I2_brgy, "Part 2 - I Provinces Lat Long.csv",
          row.names = FALSE)

# Provinces starting with K
n <- 1

prov_K_brgy <- data.frame(matrix(ncol = 8))
colnames(prov_K_brgy) <- c("K_prov_df.province.n.",
                           "K_prov_df.barangay.n.",
                           "K_prov_df.lgu_add1.n.",
                           "K_prov_df.lgu_add2.n.",
                           "K_prov_df.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(K_prov_df)) {
  url_K <- K_prov_df$url[n]
  # to prevent VPN
  download.file(url_K, destfile = "prov_K.html", quiet=TRUE) 
  brgy_K <- read_html("prov_K.html")
  
  brgy_lat_K <- brgy_K %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_K <- brgy_K %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_K <- brgy_K %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_K <- cbind(brgy_lat_K, brgy_long_K, brgy_reg_K)
  brgy_loc_K %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfK
  
  prov_K_brgy <- rbind(prov_K_brgy,
                       data.frame(K_prov_df$province[n],
                                  K_prov_df$barangay[n],
                                  K_prov_df$lgu_add1[n],
                                  K_prov_df$lgu_add2[n],
                                  K_prov_df$lgu_add3[n],
                                  brgyloc_dfK))
  
  prov_K_brgy <- prov_K_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prov_K_brgy <- prov_K_brgy[-1,]

colnames(prov_K_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

# Provinces starting with L
#n <- 1

#prov_L_brgy <- data.frame(matrix(ncol = 8))
#colnames(prov_L_brgy) <- c("L_prov_df.province.n.",
#                           "L_prov_df.barangay.n.",
#                           "L_prov_df.lgu_add1.n.",
#                           "L_prov_df.lgu_add2.n.",
#                           "L_prov_df.lgu_add3.n.",
#                           "X1", "X2", "X3")

#while(n<=nrow(L_prov_df)) {
#  url_L <- L_prov_df$url[n]
  # to prevent VPN
#  download.file(url_L, destfile = "prov_L.html", quiet=TRUE) 
#  brgy_L <- read_html("prov_L.html")
  
#  brgy_lat_L <- brgy_L %>%
#    html_nodes("#latitude") %>%
#    html_text()
  
#  brgy_long_L <- brgy_L %>%
#    html_nodes("#longitude") %>%
#    html_text()
  
#  brgy_reg_L <- brgy_L %>%
#    html_nodes("tr:nth-child(3) .iboxVal") %>%
#    html_text()
  
#  brgy_loc_L <- cbind(brgy_lat_L, brgy_long_L, brgy_reg_L)
#  brgy_loc_L %>%
#    matrix(ncol=3, byrow=FALSE) %>%
#    data.frame -> brgyloc_dfL
  
#  prov_L_brgy <- rbind(prov_L_brgy,
#                       data.frame(L_prov_df$province[n],
#                                  L_prov_df$barangay[n],
#                                  L_prov_df$lgu_add1[n],
#                                  L_prov_df$lgu_add2[n],
#                                  L_prov_df$lgu_add3[n],
#                                  brgyloc_dfL))
  
#  prov_L_brgy <- prov_L_brgy %>%
#    mutate_if(is.character, str_trim)
  
#  n <- n+1
#}

#prov_L_brgy <- prov_L_brgy[-1,]

#colnames(prov_L_brgy) <- c("province", "barangay", "lgu_add1",
#                           "lgu_add2", "lgu_add3", "latitude", 
#                           "longitude", "region")

#prioritize Laguna
prio_L <- L_prov_df[L_prov_df$province %in% c('Laguna'), ]

n <- 1
prio_L_brgy <- data.frame(matrix(ncol = 8))
colnames(prio_L_brgy) <- c("prio_L.province.n.",
                           "prio_L.barangay.n.",
                           "prio_L.lgu_add1.n.",
                           "prio_L.lgu_add2.n.",
                           "prio_L.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(prio_L)) {
  
  url_L_prio <- prio_L$url[n]
  # to prevent VPN
  download.file(url_L_prio, destfile = "prov_L_prio.html", quiet=TRUE) 
  brgy_L_prio <- read_html("prov_L_prio.html")
  
  brgy_lat_L_prio <- brgy_L_prio %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_L_prio <- brgy_L_prio %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_L_prio <- brgy_L_prio %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_L_prio <- cbind(brgy_lat_L_prio, 
                           brgy_long_L_prio, 
                           brgy_reg_L_prio)
  brgy_loc_L_prio %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfL_prio
  
  prio_L_brgy <- rbind(prio_L_brgy,
                       data.frame(prio_L$province[n],
                                  prio_L$barangay[n],
                                  prio_L$lgu_add1[n],
                                  prio_L$lgu_add2[n],
                                  prio_L$lgu_add3[n],
                                  brgyloc_dfL_prio))
  
  prio_L_brgy <- prio_L_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prio_L_brgy <- prio_L_brgy[-1,]

colnames(prio_L_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

#Alternative code for L provinces (divided into 2 parts)
# Part 1
prov_L1_brgy <- data.frame()

for(i in 1:2191) { 
  url_L1 <- L_prov_df$url[i]
  download.file(url_L1, destfile = "prov_L1.html", quiet=TRUE) 
  brgy_L1 <- read_html("prov_L1.html")
  
  brgy_lat_L1 <- brgy_L1 %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_L1 <- brgy_L1 %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_L1 <- brgy_L1 %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_L1 <- cbind(brgy_lat_L1, 
                       brgy_long_L1, 
                       brgy_reg_L1)
  
  brgyloc_dfL1 <- brgy_loc_L1 %>%
    as.data.frame() %>%
    set_names(c("latitude", "longitude", "region"))
  
  prov_L1_brgy <- rbind(prov_L1_brgy,
                        data.frame(L_prov_df$province[i],
                                   L_prov_df$barangay[i],
                                   L_prov_df$lgu_add1[i],
                                   L_prov_df$lgu_add2[i],
                                   L_prov_df$lgu_add3[i],
                                   brgyloc_dfL1))
  
  prov_L1_brgy <- prov_L1_brgy %>%
    mutate_if(is.character, str_trim)
}

colnames(prov_L1_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

write.csv(prov_L1_brgy, "Part 1 - L Provinces Lat Long.csv",
          row.names = FALSE)

#Part 2
prov_L2_brgy <- data.frame()

for(i in 2192:4381) { 
  url_L2 <- L_prov_df$url[i]
  download.file(url_L2, destfile = "prov_L2.html", quiet=TRUE) 
  brgy_L2 <- read_html("prov_L2.html")
  
  brgy_lat_L2 <- brgy_L2 %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_L2 <- brgy_L2 %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_L2 <- brgy_L2 %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_L2 <- cbind(brgy_lat_L2, 
                       brgy_long_L2, 
                       brgy_reg_L2)
  
  brgyloc_dfL2 <- brgy_loc_L2 %>%
    as.data.frame() %>%
    set_names(c("latitude", "longitude", "region"))
  
  prov_L2_brgy <- rbind(prov_L2_brgy,
                        data.frame(L_prov_df$province[i],
                                   L_prov_df$barangay[i],
                                   L_prov_df$lgu_add1[i],
                                   L_prov_df$lgu_add2[i],
                                   L_prov_df$lgu_add3[i],
                                   brgyloc_dfL2))
  
  prov_L2_brgy <- prov_L2_brgy %>%
    mutate_if(is.character, str_trim)
}

colnames(prov_L2_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

write.csv(prov_L2_brgy, "Part 2 - L Provinces Lat Long.csv",
          row.names=FALSE)

# Provinces starting with M
n <- 1

prov_M_brgy <- data.frame(matrix(ncol = 8))
colnames(prov_M_brgy) <- c("M_prov_df.province.n.",
                           "M_prov_df.barangay.n.",
                           "M_prov_df.lgu_add1.n.",
                           "M_prov_df.lgu_add2.n.",
                           "M_prov_df.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(M_prov_df)) {
  url_M <- M_prov_df$url[n]
  # to prevent VPN
  download.file(url_M, destfile = "prov_M.html", quiet=TRUE) 
  brgy_M <- read_html("prov_M.html")
  
  brgy_lat_M <- brgy_M %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_M <- brgy_M %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_M <- brgy_M %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_M <- cbind(brgy_lat_M, brgy_long_M, brgy_reg_M)
  brgy_loc_M %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfM
  
  prov_M_brgy <- rbind(prov_M_brgy,
                       data.frame(M_prov_df$province[n],
                                  M_prov_df$barangay[n],
                                  M_prov_df$lgu_add1[n],
                                  M_prov_df$lgu_add2[n],
                                  M_prov_df$lgu_add3[n],
                                  brgyloc_dfM))
  
  prov_M_brgy <- prov_M_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prov_M_brgy <- prov_M_brgy[-1,]

colnames(prov_M_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

write.csv(prov_M_brgy, "M Provinces Lat Long.csv", row.names=FALSE)

# Provinces starting with N
n <- 1

prov_N_brgy <- data.frame(matrix(ncol = 8))
colnames(prov_N_brgy) <- c("N_prov_df.province.n.",
                           "N_prov_df.barangay.n.",
                           "N_prov_df.lgu_add1.n.",
                           "N_prov_df.lgu_add2.n.",
                           "N_prov_df.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(N_prov_df)) {
  url_N <- N_prov_df$url[n]
  # to prevent VPN
  download.file(url_N, destfile = "prov_N.html", quiet=TRUE) 
  brgy_N <- read_html("prov_N.html")
  
  brgy_lat_N <- brgy_N %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_N <- brgy_N %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_N <- brgy_N %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_N <- cbind(brgy_lat_N, brgy_long_N, brgy_reg_N)
  brgy_loc_N %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfN
  
  prov_N_brgy <- rbind(prov_N_brgy,
                       data.frame(N_prov_df$province[n],
                                  N_prov_df$barangay[n],
                                  N_prov_df$lgu_add1[n],
                                  N_prov_df$lgu_add2[n],
                                  N_prov_df$lgu_add3[n],
                                  brgyloc_dfN))
  
  prov_N_brgy <- prov_N_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prov_N_brgy <- prov_N_brgy[-1,]

colnames(prov_N_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")


# Provinces starting with O
n <- 1

prov_O_brgy <- data.frame(matrix(ncol = 8))
colnames(prov_O_brgy) <- c("O_prov_df.province.n.",
                           "O_prov_df.barangay.n.",
                           "O_prov_df.lgu_add1.n.",
                           "O_prov_df.lgu_add2.n.",
                           "O_prov_df.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(O_prov_df)) {
  url_O <- O_prov_df$url[n]
  # to prevent VPN
  download.file(url_O, destfile = "prov_O.html", quiet=TRUE) 
  brgy_O <- read_html("prov_O.html")
  
  brgy_lat_O <- brgy_O %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_O <- brgy_O %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_O <- brgy_O %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_O <- cbind(brgy_lat_O, brgy_long_O, brgy_reg_O)
  brgy_loc_O %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfO
  
  prov_O_brgy <- rbind(prov_O_brgy,
                       data.frame(O_prov_df$province[n],
                                  O_prov_df$barangay[n],
                                  O_prov_df$lgu_add1[n],
                                  O_prov_df$lgu_add2[n],
                                  O_prov_df$lgu_add3[n],
                                  brgyloc_dfO))
  
  prov_O_brgy <- prov_O_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prov_O_brgy <- prov_O_brgy[-1,]

colnames(prov_O_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

write.csv(prov_O_brgy, "O Provinces Lat Long.csv", row.names=FALSE)

# Provinces starting with P
n <- 1

prov_P_brgy <- data.frame(matrix(ncol = 8))
colnames(prov_P_brgy) <- c("P_prov_df.province.n.",
                           "P_prov_df.barangay.n.",
                           "P_prov_df.lgu_add1.n.",
                           "P_prov_df.lgu_add2.n.",
                           "P_prov_df.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(P_prov_df)) {
  url_P <- P_prov_df$url[n]
  # to prevent VPN
  download.file(url_P, destfile = "prov_P.html", quiet=TRUE) 
  brgy_P <- read_html("prov_P.html")
  
  brgy_lat_P <- brgy_P %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_P <- brgy_P %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_P <- brgy_P %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_P <- cbind(brgy_lat_P, brgy_long_P, brgy_reg_P)
  brgy_loc_P %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfP
  
  prov_P_brgy <- rbind(prov_P_brgy,
                       data.frame(P_prov_df$province[n],
                                  P_prov_df$barangay[n],
                                  P_prov_df$lgu_add1[n],
                                  P_prov_df$lgu_add2[n],
                                  P_prov_df$lgu_add3[n],
                                  brgyloc_dfP))
  
  prov_P_brgy <- prov_P_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prov_P_brgy <- prov_P_brgy[-1,]

colnames(prov_P_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

write.csv(prov_P_brgy, "P Provinces Lat Long.csv", row.names=FALSE)


# Provinces starting with Q
n <- 1

prov_Q_brgy <- data.frame(matrix(ncol = 8))
colnames(prov_Q_brgy) <- c("Q_prov_df.province.n.",
                           "Q_prov_df.barangay.n.",
                           "Q_prov_df.lgu_add1.n.",
                           "Q_prov_df.lgu_add2.n.",
                           "Q_prov_df.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(Q_prov_df)) {
  url_Q <- Q_prov_df$url[n]
  # to prevent VPN
  download.file(url_Q, destfile = "prov_Q.html", quiet=TRUE) 
  brgy_Q <- read_html("prov_Q.html")
  
  brgy_lat_Q <- brgy_Q %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_Q <- brgy_Q %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_Q <- brgy_Q %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_Q <- cbind(brgy_lat_Q, brgy_long_Q, brgy_reg_Q)
  brgy_loc_Q %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfQ
  
  prov_Q_brgy <- rbind(prov_Q_brgy,
                       data.frame(Q_prov_df$province[n],
                                  Q_prov_df$barangay[n],
                                  Q_prov_df$lgu_add1[n],
                                  Q_prov_df$lgu_add2[n],
                                  Q_prov_df$lgu_add3[n],
                                  brgyloc_dfQ))
  
  prov_Q_brgy <- prov_Q_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prov_Q_brgy <- prov_Q_brgy[-1,]

colnames(prov_Q_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")


# Provinces starting with R
n <- 1

prov_R_brgy <- data.frame(matrix(ncol = 8))
colnames(prov_R_brgy) <- c("R_prov_df.province.n.",
                           "R_prov_df.barangay.n.",
                           "R_prov_df.lgu_add1.n.",
                           "R_prov_df.lgu_add2.n.",
                           "R_prov_df.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(R_prov_df)) {
  url_R <- R_prov_df$url[n]
  # to prevent VPN
  download.file(url_R, destfile = "prov_R.html", quiet=TRUE) 
  brgy_R <- read_html("prov_R.html")
  
  brgy_lat_R <- brgy_R %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_R <- brgy_R %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_R <- brgy_R %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_R <- cbind(brgy_lat_R, brgy_long_R, brgy_reg_R)
  brgy_loc_R %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfR
  
  prov_R_brgy <- rbind(prov_R_brgy,
                       data.frame(R_prov_df$province[n],
                                  R_prov_df$barangay[n],
                                  R_prov_df$lgu_add1[n],
                                  R_prov_df$lgu_add2[n],
                                  R_prov_df$lgu_add3[n],
                                  brgyloc_dfR))
  
  prov_R_brgy <- prov_R_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prov_R_brgy <- prov_R_brgy[-1,]

colnames(prov_R_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")


# Provinces starting with S
#n <- 1

#prov_S_brgy <- data.frame(matrix(ncol = 8))
#colnames(prov_S_brgy) <- c("S_prov_df.province.n.",
#                           "S_prov_df.barangay.n.",
#                           "S_prov_df.lgu_add1.n.",
#                           "S_prov_df.lgu_add2.n.",
#                           "S_prov_df.lgu_add3.n.",
#                           "X1", "X2", "X3")

#while(n<=nrow(S_prov_df)) {
#  url_S <- S_prov_df$url[n]
  # to prevent VPN
#  download.file(url_S, destfile = "prov_S.html", quiet=TRUE) 
#  brgy_S <- read_html("prov_S.html")
  
#  brgy_lat_S <- brgy_S %>%
#    html_nodes("#latitude") %>%
#    html_text()
  
#  brgy_long_S <- brgy_S %>%
#    html_nodes("#longitude") %>%
#    html_text()
  
#  brgy_reg_S <- brgy_S %>%
#    html_nodes("tr:nth-child(3) .iboxVal") %>%
#    html_text()
  
#  brgy_loc_S <- cbind(brgy_lat_S, brgy_long_S, brgy_reg_S)
#  brgy_loc_S %>%
#    matrix(ncol=3, byrow=FALSE) %>%
#    data.frame -> brgyloc_dfS
  
#  prov_S_brgy <- rbind(prov_S_brgy,
#                       data.frame(S_prov_df$province[n],
#                                  S_prov_df$barangay[n],
#                                  S_prov_df$lgu_add1[n],
#                                  S_prov_df$lgu_add2[n],
#                                  S_prov_df$lgu_add3[n],
#                                  brgyloc_dfS))
  
#  prov_S_brgy <- prov_S_brgy %>%
#    mutate_if(is.character, str_trim)
  
#  n <- n+1
#}

#prov_S_brgy <- prov_S_brgy[-1,]

#colnames(prov_S_brgy) <- c("province", "barangay", "lgu_add1",
#                           "lgu_add2", "lgu_add3", "latitude", 
#                           "longitude", "region")

#prioritize Sorsogon
prio_S <- S_prov_df[S_prov_df$province %in% c('Sorsogon'), ]

n <- 1
prio_S_brgy <- data.frame(matrix(ncol = 8))
colnames(prio_S_brgy) <- c("prio_S.province.n.",
                           "prio_S.barangay.n.",
                           "prio_S.lgu_add1.n.",
                           "prio_S.lgu_add2.n.",
                           "prio_S.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(prio_S)) {
  
  url_S_prio <- prio_S$url[n]
  # to prevent VPN
  download.file(url_S_prio, destfile = "prov_S_prio.html", quiet=TRUE) 
  brgy_S_prio <- read_html("prov_S_prio.html")
  
  brgy_lat_S_prio <- brgy_S_prio %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_S_prio <- brgy_S_prio %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_S_prio <- brgy_S_prio %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_S_prio <- cbind(brgy_lat_S_prio, 
                           brgy_long_S_prio, 
                           brgy_reg_S_prio)
  brgy_loc_S_prio %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfS_prio
  
  prio_S_brgy <- rbind(prio_S_brgy,
                       data.frame(prio_S$province[n],
                                  prio_S$barangay[n],
                                  prio_S$lgu_add1[n],
                                  prio_S$lgu_add2[n],
                                  prio_S$lgu_add3[n],
                                  brgyloc_dfS_prio))
  
  prio_S_brgy <- prio_S_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prio_S_brgy <- prio_S_brgy[-1,]

colnames(prio_S_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")


#Alternative code for S provinces (divided into 2 parts)
# Part 1
prov_S1_brgy <- data.frame()

for(i in 1:1885) { 
  url_S1 <- S_prov_df$url[i]
  download.file(url_S1, destfile = "prov_S1.html", quiet=TRUE) 
  brgy_S1 <- read_html("prov_S1.html")
  
  brgy_lat_S1 <- brgy_S1 %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_S1 <- brgy_S1 %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_S1 <- brgy_S1 %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_S1 <- cbind(brgy_lat_S1, 
                       brgy_long_S1, 
                       brgy_reg_S1)
  
  brgyloc_dfS1 <- brgy_loc_S1 %>%
    as.data.frame() %>%
    set_names(c("latitude", "longitude", "region"))
  
  prov_S1_brgy <- rbind(prov_S1_brgy,
                        data.frame(S_prov_df$province[i],
                                   S_prov_df$barangay[i],
                                   S_prov_df$lgu_add1[i],
                                   S_prov_df$lgu_add2[i],
                                   S_prov_df$lgu_add3[i],
                                   brgyloc_dfS1))
  
  prov_S1_brgy <- prov_S1_brgy %>%
    mutate_if(is.character, str_trim)
}

colnames(prov_S1_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

write.csv(prov_S1_brgy, "Part 1 - S Provinces Lat Long.csv",
          row.names = FALSE)

# Part 2
prov_S2_brgy <- data.frame()

for(i in 1886:3769) { 
  url_S2 <- S_prov_df$url[i]
  download.file(url_S2, destfile = "prov_S2.html", quiet=TRUE) 
  brgy_S2 <- read_html("prov_S2.html")
  
  brgy_lat_S2 <- brgy_S2 %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_S2 <- brgy_S2 %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_S2 <- brgy_S2 %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_S2 <- cbind(brgy_lat_S2, 
                       brgy_long_S2, 
                       brgy_reg_S2)
  
  brgyloc_dfS2 <- brgy_loc_S2 %>%
    as.data.frame() %>%
    set_names(c("latitude", "longitude", "region"))
  
  prov_S2_brgy <- rbind(prov_S2_brgy,
                        data.frame(S_prov_df$province[i],
                                   S_prov_df$barangay[i],
                                   S_prov_df$lgu_add1[i],
                                   S_prov_df$lgu_add2[i],
                                   S_prov_df$lgu_add3[i],
                                   brgyloc_dfS2))
  
  prov_S2_brgy <- prov_S2_brgy %>%
    mutate_if(is.character, str_trim)
}

colnames(prov_S2_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

write.csv(prov_S2_brgy, "Part 2 - S Provinces Lat Long.csv",
          row.names = FALSE)

# Provinces starting with T
n <- 1

prov_T_brgy <- data.frame(matrix(ncol = 8))
colnames(prov_T_brgy) <- c("T_prov_df.province.n.",
                           "T_prov_df.barangay.n.",
                           "T_prov_df.lgu_add1.n.",
                           "T_prov_df.lgu_add2.n.",
                           "T_prov_df.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(T_prov_df)) {
  url_T <- T_prov_df$url[n]
  # to prevent VPN
  download.file(url_T, destfile = "prov_T.html", quiet=TRUE) 
  brgy_T <- read_html("prov_T.html")
  
  brgy_lat_T <- brgy_T %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_T <- brgy_T %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_T <- brgy_T %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_T <- cbind(brgy_lat_T, brgy_long_T, brgy_reg_T)
  brgy_loc_T %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfT
  
  prov_T_brgy <- rbind(prov_T_brgy,
                       data.frame(T_prov_df$province[n],
                                  T_prov_df$barangay[n],
                                  T_prov_df$lgu_add1[n],
                                  T_prov_df$lgu_add2[n],
                                  T_prov_df$lgu_add3[n],
                                  brgyloc_dfT))
  
  prov_T_brgy <- prov_T_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prov_T_brgy <- prov_T_brgy[-1,]

colnames(prov_T_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")


# Provinces starting with Z
n <- 1

prov_Z_brgy <- data.frame(matrix(ncol = 8))
colnames(prov_Z_brgy) <- c("Z_prov_df.province.n.",
                           "Z_prov_df.barangay.n.",
                           "Z_prov_df.lgu_add1.n.",
                           "Z_prov_df.lgu_add2.n.",
                           "Z_prov_df.lgu_add3.n.",
                           "X1", "X2", "X3")

while(n<=nrow(Z_prov_df)) {
  url_Z <- Z_prov_df$url[n]
  # to prevent VPN
  download.file(url_Z, destfile = "prov_Z.html", quiet=TRUE) 
  brgy_Z <- read_html("prov_Z.html")
  
  brgy_lat_Z <- brgy_Z %>%
    html_nodes("#latitude") %>%
    html_text()
  
  brgy_long_Z <- brgy_Z %>%
    html_nodes("#longitude") %>%
    html_text()
  
  brgy_reg_Z <- brgy_Z %>%
    html_nodes("tr:nth-child(3) .iboxVal") %>%
    html_text()
  
  brgy_loc_Z <- cbind(brgy_lat_Z, brgy_long_Z, brgy_reg_Z)
  brgy_loc_Z %>%
    matrix(ncol=3, byrow=FALSE) %>%
    data.frame -> brgyloc_dfZ
  
  prov_Z_brgy <- rbind(prov_Z_brgy,
                       data.frame(Z_prov_df$province[n],
                                  Z_prov_df$barangay[n],
                                  Z_prov_df$lgu_add1[n],
                                  Z_prov_df$lgu_add2[n],
                                  Z_prov_df$lgu_add3[n],
                                  brgyloc_dfZ))
  
  prov_Z_brgy <- prov_Z_brgy %>%
    mutate_if(is.character, str_trim)
  
  n <- n+1
}

prov_Z_brgy <- prov_Z_brgy[-1,]

colnames(prov_Z_brgy) <- c("province", "barangay", "lgu_add1",
                           "lgu_add2", "lgu_add3", "latitude", 
                           "longitude", "region")

##### ---- consolidate ---- ####
#### Since other provinces were ran in different sessions ####

prov_A_brgy <- read.csv(file = "A Provinces Lat Long.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = ",",
                        fileEncoding = "latin1")

prov_B_brgy <- read.csv(file = "B Provinces Lat Long.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = ",",
                        fileEncoding = "latin1")

prov_D_brgy <- read.csv(file = "D Provinces Lat Long.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = ",",
                        fileEncoding = "latin1")

prov_E_brgy <- read.csv(file = "E Provinces Lat Long.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = ",",
                        fileEncoding = "latin1")

prov_G_brgy <- read.csv(file = "G Provinces Lat Long.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = ",",
                        fileEncoding = "latin1")

prov_K_brgy <- read.csv(file = "K Provinces Lat Long.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = ",",
                        fileEncoding = "latin1")

prov_M_brgy <- read.csv(file = "M Provinces Lat Long.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = ",",
                        fileEncoding = "latin1")

prov_N_brgy <- read.csv(file = "N Provinces Lat Long.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = ",",
                        fileEncoding = "latin1")

prov_O_brgy <- read.csv(file = "O Provinces Lat Long.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = ",",
                        fileEncoding = "latin1")

prov_P_brgy <- read.csv(file = "P Provinces Lat Long.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = ",",
                        fileEncoding = "latin1")

prov_Q_brgy <- read.csv(file = "Q Provinces Lat Long.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = ",",
                        fileEncoding = "latin1")

prov_R_brgy <- read.csv(file = "R Provinces Lat Long.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = ",",
                        fileEncoding = "latin1")

prov_T_brgy <- read.csv(file = "T Provinces Lat Long.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = ",",
                        fileEncoding = "latin1")

prov_Z_brgy <- read.csv(file = "Z Provinces Lat Long.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = ",",
                        fileEncoding = "latin1")

# Consolidate
conso_prov <- rbind(prov_A_brgy, prov_B_brgy, prov_C1_brgy,
                    prov_C2_brgy, prov_D_brgy, prov_E_brgy, 
                    prov_G_brgy, prov_I1_brgy, prov_I2_brgy,
                    prov_K_brgy, prov_L1_brgy, prov_L2_brgy,
                    prov_M_brgy, prov_N_brgy, prov_O_brgy, 
                    prov_P_brgy, prov_Q_brgy, prov_R_brgy, 
                    prov_S1_brgy, prov_S2_brgy, prov_T_brgy,
                    prov_Z_brgy)

write.csv(conso_prov, "Complete PhiAtlas PH Provincial Barangays Lat Long.csv", 
          row.names=FALSE)
